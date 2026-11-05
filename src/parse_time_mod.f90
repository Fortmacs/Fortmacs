! parse_time_mod.f90 -- parsing time strings.
! Ported from GNU Emacs 31.1 lisp/calendar/parse-time.el.
!
! Copyright (C) 1996, 2000-2026 Free Software Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> `parse_time_string' parses a time in a string and returns a decoded
!> time, just like `decode_time', where unspecified components in the
!> string are unallocated (except unspecified DST, which is
!> DECODED_TIME_DST_UNKNOWN).  `encode_partial_decoded_time' may be
!> applied on these values to obtain an internal time value
!> (`parse_iso8601_time_string' does).
!>
!> Upstream's parse-time-rules, a list of slots, tests and
!> extractor functions, is the code of `parse_time_string'.  What is not
!> ported yet is listed in docs/status/parse_time_mod.md.
module parse_time_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT, FM_OVERFLOW_ERROR
    use time_functions_mod, only: time_type, DECODED_TIME_DST_UNKNOWN, DECODED_TIME_DST_YES, DECODED_TIME_DST_NO
    use time_date_mod, only: partial_decoded_time_type, encode_partial_decoded_time
    use iso8601_mod, only: parse_iso8601
    implicit none(type, external)
    private

    public :: parse_time_string, parse_iso8601_time_string

    !> A name of a month or a weekday and its number, as
    !> `parse_time_months' and `parse_time_weekdays' hold them.
    type, public :: parse_time_name_type
        character(len=9) :: name = ""
        integer :: value = 0
    end type parse_time_name_type

    !> A time zone name, its offset in seconds east of Greenwich and
    !> whether it is a daylight saving time zone, as `parse_time_zoneinfo'
    !> holds them.
    type, public :: parse_time_zone_type
        character(len=3) :: name = ""
        integer :: offset = 0
        logical :: dst = .false.
    end type parse_time_zone_type

    !> The names of the months (lower case) that `parse_time_string'
    !> knows, and their numbers.  Port of parse-time.el parse-time-months.
    type(parse_time_name_type), public :: parse_time_months(23) = [ &
                                          parse_time_name_type("jan", 1), parse_time_name_type("feb", 2), &
                                          parse_time_name_type("mar", 3), parse_time_name_type("apr", 4), &
                                          parse_time_name_type("may", 5), parse_time_name_type("jun", 6), &
                                          parse_time_name_type("jul", 7), parse_time_name_type("aug", 8), &
                                          parse_time_name_type("sep", 9), parse_time_name_type("oct", 10), &
                                          parse_time_name_type("nov", 11), parse_time_name_type("dec", 12), &
                                          parse_time_name_type("january", 1), parse_time_name_type("february", 2), &
                                          parse_time_name_type("march", 3), parse_time_name_type("april", 4), &
                                          parse_time_name_type("june", 6), parse_time_name_type("july", 7), &
                                          parse_time_name_type("august", 8), parse_time_name_type("september", 9), &
                                          parse_time_name_type("october", 10), parse_time_name_type("november", 11), &
                                          parse_time_name_type("december", 12)]

    !> The names of the weekdays (lower case) that `parse_time_string'
    !> knows, and their numbers (0 for Sunday).  Port of parse-time.el
    !> parse-time-weekdays.
    type(parse_time_name_type), public :: parse_time_weekdays(14) = [ &
                                          parse_time_name_type("sun", 0), parse_time_name_type("mon", 1), &
                                          parse_time_name_type("tue", 2), parse_time_name_type("wed", 3), &
                                          parse_time_name_type("thu", 4), parse_time_name_type("fri", 5), &
                                          parse_time_name_type("sat", 6), parse_time_name_type("sunday", 0), &
                                          parse_time_name_type("monday", 1), parse_time_name_type("tuesday", 2), &
                                          parse_time_name_type("wednesday", 3), parse_time_name_type("thursday", 4), &
                                          parse_time_name_type("friday", 5), parse_time_name_type("saturday", 6)]

    !> The time zone names (lower case) that `parse_time_string' knows:
    !> zoneinfo, seconds off and whether it is daylight saving time.
    !> Port of parse-time.el parse-time-zoneinfo.
    type(parse_time_zone_type), public :: parse_time_zoneinfo(11) = [ &
                                          parse_time_zone_type("z", 0), parse_time_zone_type("ut", 0), &
                                          parse_time_zone_type("gmt", 0), &
                                          parse_time_zone_type("pst", -8*3600), parse_time_zone_type("pdt", -7*3600, .true.), &
                                          parse_time_zone_type("mst", -7*3600), parse_time_zone_type("mdt", -6*3600, .true.), &
                                          parse_time_zone_type("cst", -6*3600), parse_time_zone_type("cdt", -5*3600, .true.), &
                                          parse_time_zone_type("est", -5*3600), parse_time_zone_type("edt", -4*3600, .true.)]

    ! The slots of a decoded time as parse-time-rules numbers them.
    integer, parameter :: SECOND = 0, MINUTE = 1, HOUR = 2, DAY = 3, MONTH = 4, YEAR = 5, WEEKDAY = 6, DST = 7, ZONE = 8

    ! A token of the text: a run of lower-case letters, digits, signs and
    ! colons, or a number when it is all digits.
    type :: token_type
        character(len=:), allocatable :: text
        logical :: is_number = .false.
        integer(int64) :: number = 0
    end type token_type

contains

    !> Parse the time in STRING into a decoded time (second, minute, hour,
    !> day, month, year, weekday, DST and zone).  STRING should be an ISO
    !> 8601 time string, e.g., "2020-01-15T16:12:21-08:00", or something
    !> resembling an RFC 822 (or later) date-time, e.g., "Wed, 15 Jan 2020
    !> 16:12:21 -0800".  This function is somewhat liberal in what format it
    !> accepts, and will attempt to return a "likely" value even for
    !> somewhat malformed strings.  The values returned are identical to
    !> those of `decode_time', but any unknown values other than DST are
    !> unallocated, and an unknown DST value is DECODED_TIME_DST_UNKNOWN.
    !> Note that, unlike `decode_time', this function does not interpret the
    !> time string, and in particular the values of DST and ZONE do not
    !> affect the returned value of date and time, they only affect the last
    !> two components of the returned value.  This function simply parses
    !> the textual representation of date and time into separate numerical
    !> values, and doesn't care whether the time is local or UTC.
    !>
    !> KEEP_FRACTION is as for `parse_iso8601'.  A part of a time like
    !> "1a:2b:3c" that is not a number is an error in STATUS.  Port of
    !> parse-time.el:parse-time-string (FORM t is KEEP_FRACTION true).
    function parse_time_string(string, status, keep_fraction) result(time)
        character(len=*), intent(in) :: string
        type(status_type), intent(out), optional :: status
        logical, intent(in), optional :: keep_fraction
        type(partial_decoded_time_type) :: time
        type(status_type) :: st
        type(token_type), allocatable :: tokens(:)
        integer(int64) :: slots(0:8)
        logical :: known(0:8)
        integer :: k

        if (present(status)) status = status_type()
        time = parse_iso8601(string, st, keep_fraction)
        if (st%ok()) return

        if (st%code /= FM_WRONG_TYPE_ARGUMENT) then
            if (present(status)) status = st
            return
        end if

        st = status_type()
        known = .false.
        slots = 0
        known(DST) = .true.
        slots(DST) = DECODED_TIME_DST_UNKNOWN
        call tokenize(ascii_downcase(string), tokens, st)

        if (st%ok()) then
            do k = 1, size(tokens)
                call set_slots_by_rules(tokens(k), slots, known, st)
                if (.not. st%ok()) exit
            end do
        end if

        if (.not. st%ok()) then
            if (present(status)) status = st
            time = partial_decoded_time_type()
            return
        end if

        time = partial_decoded_time_type()
        if (known(SECOND)) time%second = time_type(seconds=slots(SECOND))
        if (known(MINUTE)) time%minute = int(slots(MINUTE))
        if (known(HOUR)) time%hour = int(slots(HOUR))
        if (known(DAY)) time%day = int(slots(DAY))
        if (known(MONTH)) time%month = int(slots(MONTH))
        if (known(YEAR)) time%year = slots(YEAR)
        if (known(WEEKDAY)) time%weekday = int(slots(WEEKDAY))
        time%dst = int(slots(DST))
        if (known(ZONE)) time%zone = int(slots(ZONE))
    end function parse_time_string

    !> Parse an ISO 8601 time string, such as "2020-01-15T16:12:21-08:00".
    !> Fall back on parsing something resembling an RFC 822 (or later)
    !> date-time.  This function is like `parse_time_string' except that it
    !> returns a time value when successful (an unknown component of the
    !> date or the time is an error in STATUS).  KEEP_FRACTION is as for
    !> `parse_iso8601'.  Port of parse-time.el:parse-iso8601-time-string.
    type(time_type) function parse_iso8601_time_string(date_string, status, keep_fraction) result(time)
        character(len=*), intent(in) :: date_string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep_fraction
        type(partial_decoded_time_type) :: decoded

        time = time_type()
        decoded = parse_time_string(date_string, status, keep_fraction)
        if (status%ok()) time = encode_partial_decoded_time(decoded, status)
    end function parse_iso8601_time_string

    ! The class of the character C in a token: "a" for a lower-case
    ! letter, "0" for a digit, "+" and "-" for the signs, "d" for a colon
    ! and " " for one that ends a token.  Port of
    ! parse-time.el:parse-time-string-chars.
    pure character function token_class(c)
        character, intent(in) :: c

        select case (c)
        case ("a":"z")
            token_class = "a"
        case ("0":"9")
            token_class = "0"
        case ("+", "-")
            token_class = c
        case (":")
            token_class = "d"
        case default
            token_class = " "
        end select
    end function token_class

    ! Tokenize STRING into substrings.  Each substring is a run of "valid"
    ! characters, i.e., lowercase letters, digits, plus or minus signs or
    ! colons; one of digits only is a number (a number beyond int64 is an
    ! error in STATUS).  Port of parse-time.el:parse-time-tokenize.
    subroutine tokenize(string, tokens, status)
        character(len=*), intent(in) :: string
        type(token_type), allocatable, intent(out) :: tokens(:)
        type(status_type), intent(inout) :: status
        type(token_type) :: token
        character :: c
        integer :: k, start, n, ios
        logical :: all_digits

        allocate (tokens(0))
        k = 1
        n = len(string)
        c = " "

        do while (k <= n)
            ! Skip invalid characters.
            do while (k <= n)
                c = token_class(string(k:k))
                if (c /= " ") exit
                k = k + 1
            end do

            start = k
            all_digits = c == "0"

            ! Scan valid characters.
            do
                k = k + 1
                if (k > n) exit
                c = token_class(string(k:k))
                if (c == " ") exit
                all_digits = all_digits .and. c == "0"
            end do

            if (k > n + 1) cycle
            token%text = string(start:k - 1)
            token%is_number = all_digits

            if (all_digits) then
                if (len(token%text) > 18) then
                    call set_error(status, FM_OVERFLOW_ERROR, token%text)
                    return
                end if

                read (token%text, *, iostat=ios) token%number

                if (ios /= 0) then
                    call set_error(status, FM_OVERFLOW_ERROR, token%text)
                    return
                end if
            end if

            tokens = [tokens, token]
        end do
    end subroutine tokenize

    ! Use the first of parse-time-rules that matches TOKEN to the slots:
    ! SLOTS holds the values and KNOWN which are set.  Port of the rules
    ! loop of parse-time.el:parse-time-string and parse-time.el
    ! parse-time-rules.
    subroutine set_slots_by_rules(token, slots, known, status)
        type(token_type), intent(in) :: token
        integer(int64), intent(inout) :: slots(0:8)
        logical, intent(inout) :: known(0:8)
        type(status_type), intent(inout) :: status
        integer :: k, l
        integer(int64) :: hours, minutes

        l = len(token%text)

        ! ((6) parse-time-weekdays)
        if (.not. known(WEEKDAY) .and. .not. token%is_number) then
            k = findloc(parse_time_weekdays%name == token%text, .true., dim=1)

            if (k > 0) then
                call set_slot(WEEKDAY, int(parse_time_weekdays(k)%value, int64))
                return
            end if
        end if

        ! ((3) (1 31))
        if (.not. known(DAY) .and. token%is_number) then
            if (token%number >= 1 .and. token%number <= 31) then
                call set_slot(DAY, token%number)
                return
            end if
        end if

        ! ((4) parse-time-months)
        if (.not. known(MONTH) .and. .not. token%is_number) then
            k = findloc(parse_time_months%name == token%text, .true., dim=1)

            if (k > 0) then
                call set_slot(MONTH, int(parse_time_months(k)%value, int64))
                return
            end if
        end if

        ! ((5) (100))
        if (.not. known(YEAR) .and. token%is_number) then
            if (token%number >= 100) then
                call set_slot(YEAR, token%number)
                return
            end if
        end if

        ! ((2 1 0) HH:MM:SS)
        if (.not. known(HOUR) .and. .not. token%is_number .and. l == 8) then
            if (token%text(3:3) == ":" .and. token%text(6:6) == ":") then
                call set_number(HOUR, 1, 2)
                if (status%ok()) call set_number(MINUTE, 4, 5)
                if (status%ok()) call set_number(SECOND, 7, 8)
                return
            end if
        end if

        ! ((8 7) parse-time-zoneinfo)
        if (.not. known(ZONE) .and. .not. token%is_number) then
            k = findloc(parse_time_zoneinfo%name == token%text, .true., dim=1)

            if (k > 0) then
                call set_slot(ZONE, int(parse_time_zoneinfo(k)%offset, int64))

                if (parse_time_zoneinfo(k)%dst) then
                    call set_slot(DST, int(DECODED_TIME_DST_YES, int64))
                else
                    call set_slot(DST, int(DECODED_TIME_DST_NO, int64))
                end if

                return
            end if
        end if

        ! ((8) +HHMM or -HHMM)
        if (.not. known(ZONE) .and. .not. token%is_number .and. l == 5) then
            if (token%text(1:1) == "+" .or. token%text(1:1) == "-") then
                minutes = parse_integer(token%text, 4, 5, status)
                if (.not. status%ok()) return
                hours = parse_integer(token%text, 2, 3, status)
                if (.not. status%ok()) return
                call set_slot(ZONE, 60*(minutes + 60*hours)*merge(-1, 1, token%text(1:1) == "-"))
                return
            end if
        end if

        ! ((5 4 3) YYYY-MM-DD)
        if (.not. known(YEAR) .and. .not. token%is_number .and. l == 10) then
            if (token%text(5:5) == "-" .and. token%text(8:8) == "-") then
                call set_number(YEAR, 1, 4)
                if (status%ok()) call set_number(MONTH, 6, 7)
                if (status%ok()) call set_number(DAY, 9, 10)
                return
            end if
        end if

        ! ((2 1 0) HH:MM)
        if (.not. known(HOUR) .and. .not. token%is_number .and. l == 5) then
            if (token%text(3:3) == ":") then
                call set_number(HOUR, 1, 2)
                if (status%ok()) call set_number(MINUTE, 4, 5)
                call set_slot(SECOND, 0_int64)
                return
            end if
        end if

        ! ((2 1 0) H:MM)
        if (.not. known(HOUR) .and. .not. token%is_number .and. l == 4) then
            if (token%text(2:2) == ":") then
                call set_number(HOUR, 1, 1)
                if (status%ok()) call set_number(MINUTE, 3, 4)
                call set_slot(SECOND, 0_int64)
                return
            end if
        end if

        ! ((2 1 0) H:MM:SS)
        if (.not. known(HOUR) .and. .not. token%is_number .and. l == 7) then
            if (token%text(2:2) == ":") then
                call set_number(HOUR, 1, 1)
                if (status%ok()) call set_number(MINUTE, 3, 4)
                if (status%ok()) call set_number(SECOND, 6, 7)
                return
            end if
        end if

        ! ((5) (50 110) +1900)
        if (.not. known(YEAR) .and. token%is_number) then
            if (token%number >= 50 .and. token%number <= 110) then
                call set_slot(YEAR, 1900 + token%number)
                return
            end if
        end if

        ! ((5) (0 49) +2000)
        if (.not. known(YEAR) .and. token%is_number) then
            if (token%number >= 0 .and. token%number <= 49) call set_slot(YEAR, 2000 + token%number)
        end if

    contains

        ! Set the slot N to VALUE.
        subroutine set_slot(n, value)
            integer, intent(in) :: n
            integer(int64), intent(in) :: value

            slots(n) = value
            known(n) = .true.
        end subroutine set_slot

        ! Set the slot N to the integer of the characters FIRST to LAST of
        ! the token (an error in STATUS if they are not one).
        subroutine set_number(n, first, last)
            integer, intent(in) :: n, first, last
            integer(int64) :: value

            value = parse_integer(token%text, first, last, status)
            if (status%ok()) call set_slot(n, value)
        end subroutine set_number
    end subroutine set_slots_by_rules

    ! The integer of the characters FIRST to LAST of TEXT: an optional
    ! sign and digits ("Not an integer string: `TEXT'" in STATUS if they
    ! are not).  Port of cl-extra.el:cl-parse-integer (no RADIX or
    ! JUNK-ALLOWED; tokens hold no whitespace).
    integer(int64) function parse_integer(text, first, last, status) result(number)
        character(len=*), intent(in) :: text
        integer, intent(in) :: first, last
        type(status_type), intent(inout) :: status
        integer :: k, start
        logical :: negative, valid

        number = 0
        negative = .false.
        start = first

        if (start <= last) then
            if (text(start:start) == "+" .or. text(start:start) == "-") then
                negative = text(start:start) == "-"
                start = start + 1
            end if
        end if

        ! At least one digit, and digits only.
        valid = start <= last
        if (valid) valid = verify(text(start:last), "0123456789") == 0

        if (.not. valid) then
            call set_error(status, FM_GENERIC_ERROR, "Not an integer string: `"//text//"'")
            return
        end if

        do k = start, last
            number = 10*number + (iachar(text(k:k)) - iachar("0"))
        end do

        if (negative) number = -number
    end function parse_integer

    ! STRING with ASCII upper-case letters in lower case (downcase; the
    ! other letters never make a token).  Not a port.
    pure function ascii_downcase(string) result(lower)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: lower
        integer :: k

        lower = string

        do k = 1, len(string)
            if (lge(string(k:k), "A") .and. lle(string(k:k), "Z")) lower(k:k) = achar(iachar(string(k:k)) + 32)
        end do
    end function ascii_downcase
end module parse_time_mod
