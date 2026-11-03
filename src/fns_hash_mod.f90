! fns_hash_mod.f90 -- md5, secure-hash and buffer-hash.
! Ported from GNU Emacs 31.1 src/fns.c (in part: the message digests).
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Part of fns_mod: the message digests of strings and buffers (`md5',
!> `secure_hash', `buffer_hash'), over the Gnulib digests md5_mod,
!> sha1_mod, sha256_mod, sha512_mod and sha3_mod.  A submodule can use
!> coding_mod and fileio_mod, which a module below them cannot.
submodule(fns_mod) fns_hash_mod
    use buffer_mod, only: buf_bytes, set_buffer_internal
    use marker_mod, only: char_to_byte
    use error_mod, only: FM_CODING_SYSTEM_ERROR, FM_FILE_ERROR
    use coding_mod, only: is_coding_system, coding_system_priority_list, coding_system_list_entry_type, &
                          encode_coding_text, encode_coding_string, coding_system_for_write, buffer_file_coding_system, &
                          find_operation_coding_system
    use fileio_mod, only: select_safe_coding_system_function, is_file_name_match
    use sysdep_mod, only: fortmacs_getrandom, fortmacs_strerror
    use md5_mod, only: compute_md5
    use sha1_mod, only: compute_sha1, sha1_context_type
    use sha256_mod, only: compute_sha224, compute_sha256
    use sha512_mod, only: compute_sha384, compute_sha512
    use sha3_mod, only: compute_sha3, SHA3_224_DIGEST_SIZE, SHA3_256_DIGEST_SIZE, SHA3_384_DIGEST_SIZE, &
                        SHA3_512_DIGEST_SIZE
    implicit none(type, external)

contains

    recursive module subroutine md5_string(string, hash, status, beginning, end, coding_system, noerror, multibyte)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        character(len=*), intent(in), optional :: coding_system
        logical, intent(in), optional :: noerror, multibyte
        character(len=:), allocatable :: data

        hash = ""
        call extract_data_from_string(string, data, status, beginning, end, coding_system, noerror, multibyte)
        if (.not. status%ok()) return
        call compute_secure_hash(SECURE_HASH_MD5, data, .false., hash, status)
    end subroutine md5_string

    recursive module subroutine md5_buffer(buffer, hash, status, beginning, end, coding_system, noerror)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        character(len=*), intent(in), optional :: coding_system
        logical, intent(in), optional :: noerror
        character(len=:), allocatable :: data

        hash = ""
        call extract_data_from_buffer(buffer, data, status, beginning, end, coding_system, noerror)
        if (.not. status%ok()) return
        call compute_secure_hash(SECURE_HASH_MD5, data, .false., hash, status)
    end subroutine md5_buffer

    recursive module subroutine secure_hash_string(algorithm, string, hash, status, beginning, end, binary, multibyte)
        integer, intent(in) :: algorithm
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        logical, intent(in), optional :: binary, multibyte
        character(len=:), allocatable :: data
        logical :: is_binary

        hash = ""
        is_binary = .false.
        if (present(binary)) is_binary = binary
        call extract_data_from_string(string, data, status, beginning, end, multibyte=multibyte)
        if (.not. status%ok()) return
        call compute_secure_hash(algorithm, data, is_binary, hash, status)
    end subroutine secure_hash_string

    recursive module subroutine secure_hash_buffer(algorithm, buffer, hash, status, beginning, end, binary)
        integer, intent(in) :: algorithm
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        logical, intent(in), optional :: binary
        character(len=:), allocatable :: data
        logical :: is_binary

        hash = ""
        is_binary = .false.
        if (present(binary)) is_binary = binary
        call extract_data_from_buffer(buffer, data, status, beginning, end)
        if (.not. status%ok()) return
        call compute_secure_hash(algorithm, data, is_binary, hash, status)
    end subroutine secure_hash_buffer

    pure module function secure_hash_algorithms() result(algorithms)
        integer :: algorithms(10)

        algorithms = [SECURE_HASH_MD5, SECURE_HASH_SHA1, SECURE_HASH_SHA224, SECURE_HASH_SHA256, SECURE_HASH_SHA384, &
                      SECURE_HASH_SHA512, SECURE_HASH_SHA3_224, SECURE_HASH_SHA3_256, SECURE_HASH_SHA3_384, &
                      SECURE_HASH_SHA3_512]
    end function secure_hash_algorithms

    module subroutine hash_random_bytes(algorithm, length, hash, status, binary)
        integer, intent(in) :: algorithm
        integer(int64), intent(in) :: length
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: binary
        character(len=:), allocatable :: data
        integer(int32) :: err
        logical :: is_binary

        hash = ""
        is_binary = .false.
        if (present(binary)) is_binary = binary

        if (length < 0) then
            call set_error(status, FM_GENERIC_ERROR, "Without a length, `hash_random_bytes' can't be used; see Info node "// &
                           "`(elisp)Format of GnuTLS Cryptography Inputs'")
            return
        end if

        allocate (character(len=length) :: data)
        err = fortmacs_getrandom(data)

        if (err /= 0) then
            call set_error(status, FM_FILE_ERROR, "Getting random data: "//fortmacs_strerror(err))
            return
        end if

        call compute_secure_hash(algorithm, data, is_binary, hash, status)
    end subroutine hash_random_bytes

    module subroutine buffer_hash(hash, status, buffer)
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        type(sha1_context_type) :: context
        character(len=:), allocatable :: digest
        integer(int64) :: b

        hash = ""
        b = current

        if (present(buffer)) then
            call resolve_buffer(buffer, b, status)

            if (.not. status%ok()) then
                call nsberror(status)
                return
            end if
        end if

        associate (t => buffers(b)%text)
            ! Process the first part of the buffer.
            call context%process_bytes(t%beginning(BEGINNING_BYTE:t%gpt_byte - 1))
            ! If the gap is before the end of the buffer, process the last half
            ! of the buffer.
            if (t%gpt_byte < t%z_byte) call context%process_bytes(t%beginning(t%gpt_byte + t%gap_size:t%z_byte + t%gap_size - 1))
        end associate

        call context%finish(digest)
        hash = digest_to_hex(digest)
    end subroutine buffer_hash

    !> The bytes of the multibyte text STRING (or of the unibyte STRING
    !> when MULTIBYTE is false) to hash, in DATA: encoded by CODING_SYSTEM,
    !> by default the most preferred coding system (or `raw-text' for a
    !> unibyte string), then the bytes from BEGINNING to END of the
    !> encoded text, as `substring' counts.  An invalid coding system is a
    !> `coding_system_error' error condition, or `raw-text' if NOERROR.
    !> Port of fns.c:extract_data_from_object (a string; with
    !> coding.c:preferred_coding_system).
    recursive subroutine extract_data_from_string(string, data, status, beginning, end, coding_system, noerror, multibyte)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: data
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        character(len=*), intent(in), optional :: coding_system
        logical, intent(in), optional :: noerror, multibyte
        type(coding_system_list_entry_type), allocatable :: priorities(:)
        character(len=:), allocatable :: coding, encoded, used
        integer(int64) :: from_byte, to_byte
        logical :: is_multibyte, ok, ignore_errors

        data = ""
        ignore_errors = .false.
        if (present(noerror)) ignore_errors = noerror
        is_multibyte = .true.
        if (present(multibyte)) is_multibyte = multibyte
        coding = ""
        if (present(coding_system)) coding = coding_system

        if (len(coding) == 0) then
            ! Decide the coding-system to encode the data with.
            if (is_multibyte) then
                ! Use default, we can't guess correct value.
                call coding_system_priority_list(priorities, .true.)
                if (size(priorities) > 0) coding = trim(priorities(1)%name)
            else
                coding = "raw-text"
            end if
        end if

        if (.not. is_coding_system(coding)) then
            ! Invalid coding system.
            if (.not. ignore_errors) then
                call set_error(status, FM_CODING_SYSTEM_ERROR, coding)
                return
            end if

            coding = "raw-text"
        end if

        if (is_multibyte) then
            call encode_coding_text(string, coding, encoded, used, status, multibyte=.true.)
            if (.not. status%ok()) return
        else
            encoded = string
        end if

        call validate_subarray(encoded, len(encoded, kind=int64), from_byte, to_byte, ok, status, beginning, end)
        if (.not. ok) return
        data = encoded(from_byte + 1:to_byte)
    end subroutine extract_data_from_string

    !> The text of BUFFER from BEGINNING to END (by default its accessible
    !> text, the two in either order) to hash, in DATA: encoded by
    !> CODING_SYSTEM, by default the one `write_region' would choose
    !> (`coding_system_for_write', else the buffer's own
    !> `buffer_file_coding_system', else `file_coding_systems''s, else
    !> the default `buffer_file_coding_system', confirmed by
    !> `select_safe_coding_system_function'; `raw-text' for a unibyte
    !> buffer).  A position outside the accessible text is an
    !> `args_out_of_range' error condition; a default coding system that
    !> is invalid a `coding_system_error' one, or `raw-text' if NOERROR.
    !> Encoding sets `last_coding_system_used'.  Port of
    !> fns.c:extract_data_from_object (a buffer).
    recursive subroutine extract_data_from_buffer(buffer, data, status, beginning, end, coding_system, noerror)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable, intent(out) :: data
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        character(len=*), intent(in), optional :: coding_system
        logical, intent(in), optional :: noerror
        character(len=:), allocatable :: coding, text, decoding, encoding, chosen
        character(len=:), allocatable :: beginning_text, end_text
        integer(int64) :: b, previous, s, e, temp
        logical :: force_raw_text, is_multibyte, ignore_errors

        data = ""
        ignore_errors = .false.
        if (present(noerror)) ignore_errors = noerror
        call resolve_buffer(buffer, b, status)

        if (.not. status%ok()) then
            call nsberror(status)
            return
        end if

        previous = current
        call set_buffer_internal(b)
        s = buffers(b)%begv
        if (present(beginning)) s = beginning
        e = buffers(b)%zv
        if (present(end)) e = end

        if (s > e) then
            temp = s
            s = e
            e = temp
        end if

        if (.not. (buffers(b)%begv <= s .and. e <= buffers(b)%zv)) then
            beginning_text = ".false."
            if (present(beginning)) beginning_text = number_to_string(beginning)
            end_text = ".false."
            if (present(end)) end_text = number_to_string(end)
            call set_error(status, FM_ARGS_OUT_OF_RANGE, beginning_text//", "//end_text)
            call set_buffer_internal(previous)
            return
        end if

        coding = ""
        if (present(coding_system)) coding = coding_system

        if (len(coding) == 0) then
            ! Decide the coding-system to encode the data with.
            ! See fileio.c:Fwrite-region.
            if (allocated(coding_system_for_write%value)) then
                coding = coding_system_for_write%value
            else
                force_raw_text = .false.

                if (buffer_file_coding_system%local_p() .and. allocated(buffer_file_coding_system%value)) then
                    coding = buffer_file_coding_system%value
                end if

                if (len(coding) == 0) then
                    if (.not. buffers(b)%multibyte) force_raw_text = .true.
                end if

                if (len(coding) == 0 .and. allocated(buffers(b)%filename)) then
                    ! Check file-coding-system-alist.
                    call find_operation_coding_system(buffers(b)%filename, is_file_name_match, decoding, encoding)
                    coding = encoding
                end if

                if (len(coding) == 0 .and. allocated(buffer_file_coding_system%value)) then
                    ! If we still have not decided a coding system, use the
                    ! default value of buffer-file-coding-system.
                    coding = buffer_file_coding_system%value
                end if

                if (.not. force_raw_text .and. associated(select_safe_coding_system_function)) then
                    ! Confirm that CODING can surely encode the current region.
                    call select_safe_coding_system_function(s, e, coding, .false., "", chosen, status)

                    if (.not. status%ok()) then
                        call set_buffer_internal(previous)
                        return
                    end if

                    coding = ""
                    if (allocated(chosen)) coding = chosen
                end if

                if (force_raw_text) coding = "raw-text"
            end if

            if (.not. is_coding_system(coding)) then
                ! Invalid coding system.
                if (.not. ignore_errors) then
                    call set_error(status, FM_CODING_SYSTEM_ERROR, coding)
                    call set_buffer_internal(previous)
                    return
                end if

                coding = "raw-text"
            end if
        end if

        text = buf_bytes(buffers(b), char_to_byte(s), char_to_byte(e))
        is_multibyte = buffers(b)%multibyte
        call set_buffer_internal(previous)

        if (is_multibyte) then
            call encode_coding_string(text, coding, data, status, multibyte=.true.)
        else
            data = text
        end if
    end subroutine extract_data_from_buffer

    !> The digest of DATA by ALGORITHM, as hexadecimal digits in HASH, or
    !> as its bytes if BINARY.  An unknown ALGORITHM is an error ("Invalid
    !> algorithm arg").  Port of fns.c:secure_hash.
    pure subroutine compute_secure_hash(algorithm, data, binary, hash, status)
        integer, intent(in) :: algorithm
        character(len=*), intent(in) :: data
        logical, intent(in) :: binary
        character(len=:), allocatable, intent(out) :: hash
        type(status_type), intent(inout) :: status
        character(len=:), allocatable :: digest

        hash = ""

        select case (algorithm)
        case (SECURE_HASH_MD5)
            digest = compute_md5(data)
        case (SECURE_HASH_SHA1)
            digest = compute_sha1(data)
        case (SECURE_HASH_SHA224)
            digest = compute_sha224(data)
        case (SECURE_HASH_SHA256)
            digest = compute_sha256(data)
        case (SECURE_HASH_SHA384)
            digest = compute_sha384(data)
        case (SECURE_HASH_SHA512)
            digest = compute_sha512(data)
        case (SECURE_HASH_SHA3_224)
            digest = compute_sha3(data, SHA3_224_DIGEST_SIZE)
        case (SECURE_HASH_SHA3_256)
            digest = compute_sha3(data, SHA3_256_DIGEST_SIZE)
        case (SECURE_HASH_SHA3_384)
            digest = compute_sha3(data, SHA3_384_DIGEST_SIZE)
        case (SECURE_HASH_SHA3_512)
            digest = compute_sha3(data, SHA3_512_DIGEST_SIZE)
        case default
            call set_error(status, FM_GENERIC_ERROR, "Invalid algorithm arg: "//number_to_string(int(algorithm, int64)))
            return
        end select

        if (binary) then
            hash = digest
        else
            hash = digest_to_hex(digest)
        end if
    end subroutine compute_secure_hash

    !> The lowercase hexadecimal digits of the bytes of DIGEST, two per
    !> byte.  Port of fns.c:hexbuf_digest (and fns.c:make_digest_string).
    pure function digest_to_hex(digest) result(hex)
        character(len=*), intent(in) :: digest
        character(len=2*len(digest)) :: hex
        character(len=16), parameter :: HEX_DIGITS = "0123456789abcdef"
        integer :: i, byte

        do i = 1, len(digest)
            byte = iachar(digest(i:i))
            hex(2*i - 1:2*i - 1) = HEX_DIGITS(ishft(byte, -4) + 1:ishft(byte, -4) + 1)
            hex(2*i:2*i) = HEX_DIGITS(iand(byte, 15) + 1:iand(byte, 15) + 1)
        end do
    end function digest_to_hex
end submodule fns_hash_mod
