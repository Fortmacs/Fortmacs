! mail_extract_mod.f90 -- extract full name and address from email header.
! Ported from GNU Emacs 31.1 lisp/mail/mail-extr.el.
!
! Copyright (C) 1991-2026 Free Software Foundation, Inc.
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

!> Extract the full name and the canonical address from an RFC 822 (or
!> later) mail address: `extract_mail_address_components' takes a
!> string (or the accessible part of a buffer) and gives each
!> recipient's full name, prettified for display, and canonical
!> address:
!>
!>     call extract_mail_address_components("""J. Random Hacker"" <foo@example.org>", parts, status)
!>     ! parts(1)%full_name is "J. Random Hacker", parts(1)%address "foo@example.org"
!>
!> The command `what_domain' shows the country (or the kind of
!> organization) a top-level domain stands for.  The work is done in two
!> buffers, " *extract address components*" and " *canonical address*",
!> as upstream does it.
!>
!> See docs/status/mail_extract_mod.md for what is deferred.
module mail_extract_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE, SCAN_ERROR_CODE
    use handle_mod, only: handle_type
    use data_mod, only: choice_setting_type
    use value_mod, only: value_type, make_list
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, set_buffer_if_live, get_or_make_buffer, erase_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_end_of_buffer, &
                                     is_beginning_of_buffer, buffer_string, delete_region, narrow_to_region, widen, &
                                     insert_buffer_substring, buffer_substring_no_properties, character_after, &
                                     following_character, preceding_character, save_excursion_begin, save_excursion_end, &
                                     excursion_type, save_restriction_begin, save_restriction_end, restriction_type, point_marker
    use insert_delete_mod, only: insert, insert_character, insert_before_markers
    use editing_commands_mod, only: forward_character, backward_character, delete_character
    use case_table_mod, only: downcase, upcase
    use casefiddle_mod, only: capitalize_word, upcase_word, capitalize_region, downcase_region, downcase_string
    use simple_mod, only: just_one_space
    use syntax_mod, only: make_syntax_table, modify_syntax_entry, set_syntax_table, syntax_table, character_syntax, &
                          skip_characters_forward, skip_characters_backward, forward_word, scan_expressions, scan_error_data
    use search_mod, only: search_forward, regular_expression_search_forward, looking_at, match_beginning, match_end, &
                          replace_match, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use text_properties_mod, only: set_text_properties
    use minibuffer_reading_tables_mod, only: completion_table_type, completion_list_table
    use minibuffer_reading_mod, only: completion_ignore_case
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use display_engine_mod, only: message
    implicit none(type, external)
    private

    public :: extract_mail_address_components, what_domain, initialize_mail_extract

    !> The full name and the canonical address of one recipient, as
    !> `extract_mail_address_components' gives them; each is unallocated
    !> when none was found.  Not a port.
    type, public :: mail_address_components_type
        character(len=:), allocatable :: full_name
        character(len=:), allocatable :: address
    end type mail_address_components_type

    !> The setting `mail_extr_disable_voodoo': .false. (names are always
    !> modified), .true. (never) or a regular expression (R6-39:
    !> `mail_extr_disable_voodoo%regexp = "\cj"'; unallocated when none is
    !> set).  Not a port.
    type, extends(choice_setting_type), public :: mail_extr_disable_voodoo_choice_setting_type
        character(len=:), allocatable :: regular_expression
    contains
        procedure :: clear_kinds => mail_extr_disable_voodoo_clear_kinds
        procedure :: other_value => mail_extr_disable_voodoo_other_value
        procedure :: kind_hint => mail_extr_disable_voodoo_kind_hint
    end type mail_extr_disable_voodoo_choice_setting_type

    !> Whether to try to guess middle initial from mail address.
    !> If true, then when we see an address like "John Smith <jqs@host.com>"
    !> we will assume that "John Q. Smith" is the fellow's name.  Port of
    !> mail-extr.el mail-extr-guess-middle-initial.
    logical, public :: mail_extr_guess_middle_initial = .false.

    !> Whether to ignore a name that is just a single word.
    !> If true, then when we see an address like "Idiot <dumb@stupid.com>"
    !> we will act as though we couldn't find a full name in the address.
    !> Port of mail-extr.el mail-extr-ignore-single-names.
    logical, public :: mail_extr_ignore_single_names = .false.

    !> Whether to ignore a name that is equal to the mailbox name.
    !> If true, then when the address is like "Single <single@address.com>"
    !> we will act as though we couldn't find a full name in the address.
    !> Port of mail-extr.el mail-extr-ignore-realname-equals-mailbox-name.
    logical, public :: mail_extr_ignore_realname_equals_mailbox_name = .true.

    !> Matches prefixes to the full name that identify a person's position.
    !> These are stripped from the full name because they do not contribute to
    !> uniquely identifying the person.  Port of mail-extr.el
    !> mail-extr-full-name-prefixes (`initialize_mail_extract' sets it).
    character(len=:), allocatable, public :: mail_extr_full_name_prefixes

    !> Whether the local mail transport agent looks at ! before @.  Port of
    !> mail-extr.el mail-extr-@-binds-tighter-than-!.
    logical, public :: mail_extract_at_sign_binds_tighter_than_exclamation_mark = .false.

    !> Whether to throw away information in UUCP addresses
    !> by translating things like "foo!bar!baz@host" into "baz@bar.UUCP".
    !> Port of mail-extr.el mail-extr-mangle-uucp.
    logical, public :: mail_extr_mangle_uucp = .false.

    !> If it is a regexp (`mail_extr_disable_voodoo%regexp'), names matching
    !> it will never be modified.
    !> If it is .true., modifying of names will never take place; if
    !> .false., it always takes place.  It affects how
    !> `extract_mail_address_components' works.  Port of mail-extr.el
    !> mail-extr-disable-voodoo.
    type(mail_extr_disable_voodoo_choice_setting_type), public :: mail_extr_disable_voodoo = &
                                                       mail_extr_disable_voodoo_choice_setting_type(name="mail_extr_disable_voodoo")

    character(len=*), parameter :: TAB = achar(9), LF = achar(10), CR = achar(13)
    ! U+00A0 NO-BREAK SPACE, which upstream counts as whitespace.
    character(len=*), parameter :: NO_BREAK_SPACE = char(194)//char(160)
    integer(int32), parameter :: NO_BREAK_SPACE_CODE = 160
    character(len=*), parameter :: WHITESPACE = " "//TAB//LF//CR//NO_BREAK_SPACE

    ! Any character that can occur in a name, not counting characters that
    ! separate parts of a multipart name (hyphen and period).
    character(len=*), parameter :: ALL_LETTERS_BUT_SEPARATORS = "][[:alnum:]{|}'~`"
    ! Any character that can occur in a name, including the separator.
    character(len=*), parameter :: ALL_LETTERS = ALL_LETTERS_BUT_SEPARATORS//"-"
    ! Any character that can start a name.
    character(len=*), parameter :: FIRST_LETTERS = "[:alpha:]"
    ! Any character that can end a name.
    character(len=*), parameter :: LAST_LETTERS = "[:alpha:]`'."
    character(len=*), parameter :: LEADING_GARBAGE = "\W+"
    ! Matches periods used instead of spaces.  Must not match the period
    ! following an initial.
    character(len=*), parameter :: BAD_DOT_PATTERN = "\(["//ALL_LETTERS//"]["//LAST_LETTERS//"]\)\.+\(["// &
                                   FIRST_LETTERS//"]\)"
    ! Matches the occurrence of a generational name suffix, and the last
    ! character of the preceding name.
    character(len=*), parameter :: FULL_NAME_SUFFIX_PATTERN = "\(,? ?\([JjSs][Rr]\.?\|V?I+V?\)\)\([^"//ALL_LETTERS// &
                                   "]\([^"//ALL_LETTERS//"]\|\'\)\|\'\)"
    character(len=*), parameter :: ROMAN_NUMERAL_PATTERN = "V?I+V?\b"
    ! Matches a trailing uppercase (with other characters possible) acronym.
    character(len=*), parameter :: WEIRD_ACRONYM_PATTERN = "\([A-Z]+[-_/]\|[A-Z][A-Z][A-Z]?\b\)"
    ! Matches a trailing alternative address.
    character(len=*), parameter :: ALTERNATIVE_ADDRESS_PATTERN = "\(aka *\)?[a-zA-Z.]+[!@][a-zA-Z.]"
    ! Matches a variety of trailing comments not including comma-delimited
    ! comments.
    character(len=*), parameter :: TRAILING_COMMENT_START_PATTERN = " [-{]\|--\|[+@#></;]"
    ! Matches a name (not an initial).
    character(len=*), parameter :: NAME_PATTERN = "\b["//FIRST_LETTERS//"]["//ALL_LETTERS//"]*["//LAST_LETTERS//"]"
    character(len=*), parameter :: INITIAL_PATTERN = "\b["//FIRST_LETTERS//"]\([. ]\|\b\)"
    ! Matches telephone extensions.
    character(len=*), parameter :: TELEPHONE_EXTENSION_PATTERN = &
                                   "\(\([Ee]xt\|[Tt]ph\|[Tt]el\|[Xx]\)\.?\)? *\+?[0-9][- 0-9]+"
    ! Matches ham radio call signs.
    character(len=*), parameter :: HAM_CALL_SIGN_PATTERN = "\b\(DX[0-9]+\|[AKNW][A-Z]?[0-9][A-Z][A-Z]?[A-Z]?\)"
    ! Matches normal single-part name.
    character(len=*), parameter :: NORMAL_NAME_PATTERN = "\b["//FIRST_LETTERS//"]["//ALL_LETTERS_BUT_SEPARATORS// &
                                   "]+["//LAST_LETTERS//"]"
    ! Matches normal two names with missing middle initial.
    character(len=*), parameter :: TWO_NAME_PATTERN = "\`\("//NORMAL_NAME_PATTERN//"\|"//INITIAL_PATTERN//"\) +\("// &
                                   NAME_PATTERN//"\)\(,\|\'\)"
    character(len=*), parameter :: LISTSERV_LIST_NAME_PATTERN = "Multiple recipients of list \([-A-Z]+\)"
    character(len=*), parameter :: STUPID_VMS_DATE_STAMP_PATTERN = &
                                   "[0-9][0-9]-[JFMASOND][aepuco][nbrylgptvc]-[0-9][0-9][0-9][0-9] [0-9]+ *"
    ! HZ: GB (PRC Chinese character encoding) in ASCII embedding protocol.
    character(len=*), parameter :: HZ_EMBEDDED_GB_ENCODED_CHINESE_PATTERN = "~{\([^~].\|~[^}]\)+~}"
    ! The leading optional lowercase letters are for a bastardized version of
    ! the encoding, as is the optional nature of the final slash.
    character(len=*), parameter :: X400_ENCODED_ADDRESS_PATTERN = "[a-z]?[a-z]?\(/[A-Za-z]+\(\.[A-Za-z]+\)?=[^/]+\)+/?\'"
    ! S stands for Surname (family name), G for Given name, PN for
    ! Personal Name (the combination of the G and S fields).
    character(len=*), parameter :: X400_ENCODED_ADDRESS_SURNAME_PATTERN = "/[Ss]=\([^/]+\)\(/\|\'\)"
    character(len=*), parameter :: X400_ENCODED_ADDRESS_GIVEN_NAME_PATTERN = "/[Gg]=\([^/]+\)\(/\|\'\)"
    character(len=*), parameter :: X400_ENCODED_ADDRESS_FULL_NAME_PATTERN = "/[Pp][Nn]=\([^/]+\)\(/\|\'\)"

    ! The message of the scan error that `move_over_expression' takes as
    ! no expression to move over.
    character(len=*), parameter :: UNBALANCED_PARENTHESES = "Unbalanced parentheses"

    ! A top-level domain: its CODE and the NAME of the country or kind of
    ! organization it stands for.
    type :: top_level_domain_type
        character(len=14) :: code
        character(len=60) :: name
    end type top_level_domain_type

    ! Table of top-level domain names (upstream's
    ! mail-extr-all-top-level-domains, its names formatted).  Keep in mind
    ! that the country abbreviations follow ISO-3166.  Latest change:
    ! 2007/11/15.  In two parts, as a statement has at most 255
    ! continuation lines.
    type(top_level_domain_type), parameter :: TOP_LEVEL_DOMAINS_1(*) = [ &
                                              top_level_domain_type(code="ac", name="Ascension Island"), &
                                              top_level_domain_type(code="ad", name="Andorra"), &
                                              top_level_domain_type(code="ae", name="United Arab Emirates"), &
                                              top_level_domain_type(code="af", name="Afghanistan"), &
                                              top_level_domain_type(code="ag", name="Antigua and Barbuda"), &
                                              top_level_domain_type(code="ai", name="Anguilla"), &
                                              top_level_domain_type(code="al", name="Albania"), &
                                              top_level_domain_type(code="am", name="Armenia"), &
                                              top_level_domain_type(code="an", name="Netherlands Antilles"), &
                                              top_level_domain_type(code="ao", name="Angola"), &
                                              top_level_domain_type(code="aq", name="Antarctica"), &
                                              top_level_domain_type(code="ar", name="Argentine Republic"), &
                                              top_level_domain_type(code="as", name="American Samoa"), &
                                              top_level_domain_type(code="at", name="The Republic of Austria"), &
                                              top_level_domain_type(code="au", name="Australia"), &
                                              top_level_domain_type(code="aw", name="Aruba"), &
                                              top_level_domain_type(code="ax", name="Aland Islands"), &
                                              top_level_domain_type(code="az", name="Azerbaijan"), &
                                              top_level_domain_type(code="ba", name="Bosnia-Herzegovina"), &
                                              top_level_domain_type(code="bb", name="Barbados"), &
                                              top_level_domain_type(code="bd", name="Bangladesh"), &
                                              top_level_domain_type(code="be", name="The Kingdom of Belgium"), &
                                              top_level_domain_type(code="bf", name="Burkina Faso"), &
                                              top_level_domain_type(code="bg", name="Bulgaria"), &
                                              top_level_domain_type(code="bh", name="Bahrain"), &
                                              top_level_domain_type(code="bi", name="Burundi"), &
                                              top_level_domain_type(code="bj", name="Benin"), &
                                              top_level_domain_type(code="bl", name="Saint Barthelemy"), &
                                              top_level_domain_type(code="bm", name="Bermuda"), &
                                              top_level_domain_type(code="bn", name="Brunei Darussalam"), &
                                              top_level_domain_type(code="bo", name="Republic of Bolivia"), &
                                              top_level_domain_type(code="br", name="The Federative Republic of Brazil"), &
                                              top_level_domain_type(code="bs", name="Bahamas"), &
                                              top_level_domain_type(code="bt", name="Bhutan"), &
                                              top_level_domain_type(code="bv", name="Bouvet Island"), &
                                              top_level_domain_type(code="bw", name="Botswana"), &
                                              top_level_domain_type(code="by", name="Belarus"), &
                                              top_level_domain_type(code="bz", name="Belize"), &
                                              top_level_domain_type(code="ca", name="Canada"), &
                                              top_level_domain_type(code="cc", name="Cocos (Keeling) Islands"), &
                                              top_level_domain_type(code="cd", name="The Democratic Republic of the Congo"), &
                                              top_level_domain_type(code="cf", name="Central African Republic"), &
                                              top_level_domain_type(code="cg", name="Congo"), &
                                              top_level_domain_type(code="ch", name="The Swiss Confederation"), &
                                              top_level_domain_type(code="ci", name="Ivory Coast"), &
                                              top_level_domain_type(code="ck", name="Cook Islands"), &
                                              top_level_domain_type(code="cl", name="The Republic of Chile"), &
                                              top_level_domain_type(code="cm", name="Cameroon"), &
                                              top_level_domain_type(code="cn", name="The People's Republic of China"), &
                                              top_level_domain_type(code="co", name="Colombia"), &
                                              top_level_domain_type(code="cr", name="The Republic of Costa Rica"), &
                                              top_level_domain_type(code="cu", name="Cuba"), &
                                              top_level_domain_type(code="cv", name="Cape Verde"), &
                                              top_level_domain_type(code="cx", name="Christmas Island"), &
                                              top_level_domain_type(code="cy", name="Cyprus"), &
                                              top_level_domain_type(code="cz", name="Czech Republic"), &
                                              top_level_domain_type(code="de", name="Germany"), &
                                              top_level_domain_type(code="dj", name="Djibouti"), &
                                              top_level_domain_type(code="dk", name="Denmark"), &
                                              top_level_domain_type(code="dm", name="Dominica"), &
                                              top_level_domain_type(code="do", name="The Dominican Republic"), &
                                              top_level_domain_type(code="dz", name="Algeria"), &
                                              top_level_domain_type(code="ec", name="The Republic of Ecuador"), &
                                              top_level_domain_type(code="ee", name="Estonia"), &
                                              top_level_domain_type(code="eg", name="The Arab Republic of Egypt"), &
                                              top_level_domain_type(code="eh", name="Western Sahara"), &
                                              top_level_domain_type(code="er", name="Eritrea"), &
                                              top_level_domain_type(code="es", name="The Kingdom of Spain"), &
                                              top_level_domain_type(code="et", name="Ethiopia"), &
                                              top_level_domain_type(code="eu", name="European Union"), &
                                              top_level_domain_type(code="fi", name="The Republic of Finland"), &
                                              top_level_domain_type(code="fj", name="Fiji"), &
                                              top_level_domain_type(code="fk", name="Falkland Islands (Malvinas)"), &
                                              top_level_domain_type(code="fm", name="Federated States of Micronesia"), &
                                              top_level_domain_type(code="fo", name="Faroe Islands"), &
                                              top_level_domain_type(code="fr", name="France"), &
                                              top_level_domain_type(code="ga", name="Gabon"), &
                                              top_level_domain_type(code="gb", name="United Kingdom"), &
                                              top_level_domain_type(code="gd", name="Grenada"), &
                                              top_level_domain_type(code="ge", name="Georgia"), &
                                              top_level_domain_type(code="gf", name="French Guiana"), &
                                              top_level_domain_type(code="gg", name="Guernsey"), &
                                              top_level_domain_type(code="gh", name="Ghana"), &
                                              top_level_domain_type(code="gi", name="Gibraltar"), &
                                              top_level_domain_type(code="gl", name="Greenland"), &
                                              top_level_domain_type(code="gm", name="Gambia"), &
                                              top_level_domain_type(code="gn", name="Guinea"), &
                                              top_level_domain_type(code="gp", name="Guadeloupe (Fr.)"), &
                                              top_level_domain_type(code="gq", name="Equatorial Guinea"), &
                                              top_level_domain_type(code="gr", name="The Hellenic Republic (Greece)"), &
                                            top_level_domain_type(code="gs", name="South Georgia and The South Sandwich Islands"), &
                                              top_level_domain_type(code="gt", name="Guatemala"), &
                                              top_level_domain_type(code="gu", name="Guam (U.S.)"), &
                                              top_level_domain_type(code="gw", name="Guinea-Bissau"), &
                                              top_level_domain_type(code="gy", name="Guyana"), &
                                              top_level_domain_type(code="hk", name="Hong Kong"), &
                                              top_level_domain_type(code="hm", name="Heard Island and McDonald Islands"), &
                                              top_level_domain_type(code="hn", name="Honduras"), &
                                              top_level_domain_type(code="hr", name="Croatia (Hrvatska)"), &
                                              top_level_domain_type(code="ht", name="Haiti"), &
                                              top_level_domain_type(code="hu", name="The Hungarian Republic"), &
                                              top_level_domain_type(code="id", name="Indonesia"), &
                                              top_level_domain_type(code="ie", name="Ireland"), &
                                              top_level_domain_type(code="il", name="The State of Israel"), &
                                              top_level_domain_type(code="im", name="The Isle of Man"), &
                                              top_level_domain_type(code="in", name="The Republic of India"), &
                                              top_level_domain_type(code="io", name="British Indian Ocean Territory"), &
                                              top_level_domain_type(code="iq", name="Iraq"), &
                                              top_level_domain_type(code="ir", name="Islamic Republic of Iran"), &
                                              top_level_domain_type(code="is", name="The Republic of Iceland"), &
                                              top_level_domain_type(code="it", name="The Italian Republic"), &
                                              top_level_domain_type(code="je", name="Jersey"), &
                                              top_level_domain_type(code="jm", name="Jamaica"), &
                                              top_level_domain_type(code="jo", name="Jordan"), &
                                              top_level_domain_type(code="jp", name="Japan"), &
                                              top_level_domain_type(code="ke", name="Kenya"), &
                                              top_level_domain_type(code="kg", name="Kyrgyzstan"), &
                                              top_level_domain_type(code="kh", name="Cambodia"), &
                                              top_level_domain_type(code="ki", name="Kiribati"), &
                                              top_level_domain_type(code="km", name="Comoros"), &
                                              top_level_domain_type(code="kn", name="Saint Kitts and Nevis"), &
                                              top_level_domain_type(code="kp", name="Democratic People's Republic of Korea"), &
                                              top_level_domain_type(code="kr", name="Republic of Korea"), &
                                              top_level_domain_type(code="kw", name="Kuwait"), &
                                              top_level_domain_type(code="ky", name="Cayman Islands"), &
                                              top_level_domain_type(code="kz", name="Kazakhstan"), &
                                              top_level_domain_type(code="la", name="Lao People's Democratic Republic"), &
                                              top_level_domain_type(code="lb", name="Lebanon"), &
                                              top_level_domain_type(code="lc", name="Saint Lucia"), &
                                              top_level_domain_type(code="li", name="Liechtenstein"), &
                                          top_level_domain_type(code="lk", name="The Democratic Socialist Republic of Sri Lanka"), &
                                              top_level_domain_type(code="lr", name="Liberia"), &
                                              top_level_domain_type(code="ls", name="Lesotho"), &
                                              top_level_domain_type(code="lt", name="Lithuania"), &
                                              top_level_domain_type(code="lu", name="Luxembourg"), &
                                              top_level_domain_type(code="lv", name="Latvia"), &
                                              top_level_domain_type(code="ly", name="Libyan Arab Jamahiriya"), &
                                              top_level_domain_type(code="ma", name="Morocco"), &
                                              top_level_domain_type(code="mc", name="Monaco"), &
                                              top_level_domain_type(code="md", name="The Republic of Moldova"), &
                                              top_level_domain_type(code="me", name="Montenegro"), &
                                              top_level_domain_type(code="mf", name="Saint Martin (French part)"), &
                                              top_level_domain_type(code="mg", name="Madagascar"), &
                                              top_level_domain_type(code="mh", name="Marshall Islands"), &
                                              top_level_domain_type(code="mk", name="The Former Yugoslav Republic of Macedonia"), &
                                              top_level_domain_type(code="ml", name="Mali"), &
                                              top_level_domain_type(code="mm", name="Myanmar"), &
                                              top_level_domain_type(code="mn", name="Mongolia"), &
                                              top_level_domain_type(code="mo", name="Macao"), &
                                              top_level_domain_type(code="mp", name="Northern Mariana Islands"), &
                                              top_level_domain_type(code="mq", name="Martinique"), &
                                              top_level_domain_type(code="mr", name="Mauritania"), &
                                              top_level_domain_type(code="ms", name="Montserrat"), &
                                              top_level_domain_type(code="mt", name="Malta"), &
                                              top_level_domain_type(code="mu", name="Mauritius"), &
                                              top_level_domain_type(code="mv", name="Maldives"), &
                                              top_level_domain_type(code="mw", name="Malawi"), &
                                              top_level_domain_type(code="mx", name="The United Mexican States"), &
                                              top_level_domain_type(code="my", name="Malaysia"), &
                                              top_level_domain_type(code="mz", name="Mozambique"), &
                                              top_level_domain_type(code="na", name="Namibia"), &
                                              top_level_domain_type(code="nc", name="New Caledonia (Fr.)"), &
                                              top_level_domain_type(code="ne", name="Niger"), &
                                              top_level_domain_type(code="nf", name="Norfolk Island"), &
                                              top_level_domain_type(code="ng", name="Nigeria"), &
                                              top_level_domain_type(code="ni", name="The Republic of Nicaragua"), &
                                              top_level_domain_type(code="nl", name="The Kingdom of the Netherlands"), &
                                              top_level_domain_type(code="no", name="The Kingdom of Norway"), &
                                              top_level_domain_type(code="np", name="Nepal"), &
                                              top_level_domain_type(code="nr", name="Nauru"), &
                                              top_level_domain_type(code="nu", name="Niue"), &
                                              top_level_domain_type(code="nz", name="New Zealand")]
    type(top_level_domain_type), parameter :: TOP_LEVEL_DOMAINS_2(*) = [ &
                                              top_level_domain_type(code="om", name="Oman"), &
                                              top_level_domain_type(code="pa", name="Panama"), &
                                              top_level_domain_type(code="pe", name="Peru"), &
                                              top_level_domain_type(code="pf", name="French Polynesia"), &
                                              top_level_domain_type(code="pg", name="Papua New Guinea"), &
                                              top_level_domain_type(code="ph", name="The Republic of the Philippines"), &
                                              top_level_domain_type(code="pk", name="Pakistan"), &
                                              top_level_domain_type(code="pl", name="Poland"), &
                                              top_level_domain_type(code="pm", name="Saint Pierre and Miquelon"), &
                                              top_level_domain_type(code="pn", name="Pitcairn"), &
                                              top_level_domain_type(code="pr", name="Puerto Rico (U.S.)"), &
                                              top_level_domain_type(code="ps", name="Palestinian Territory, Occupied"), &
                                              top_level_domain_type(code="pt", name="The Portuguese Republic"), &
                                              top_level_domain_type(code="pw", name="Palau"), &
                                              top_level_domain_type(code="py", name="Paraguay"), &
                                              top_level_domain_type(code="qa", name="Qatar"), &
                                              top_level_domain_type(code="re", name="Reunion (Fr.)"), &
                                              top_level_domain_type(code="ro", name="Romania"), &
                                              top_level_domain_type(code="rs", name="Serbia"), &
                                              top_level_domain_type(code="ru", name="Russian Federation"), &
                                              top_level_domain_type(code="rw", name="Rwanda"), &
                                              top_level_domain_type(code="sa", name="Saudi Arabia"), &
                                              top_level_domain_type(code="sb", name="Solomon Islands"), &
                                              top_level_domain_type(code="sc", name="Seychelles"), &
                                              top_level_domain_type(code="sd", name="Sudan"), &
                                              top_level_domain_type(code="se", name="The Kingdom of Sweden"), &
                                              top_level_domain_type(code="sg", name="The Republic of Singapore"), &
                                              top_level_domain_type(code="sh", name="Saint Helena"), &
                                              top_level_domain_type(code="si", name="Slovenia"), &
                                              top_level_domain_type(code="sj", name="Svalbard and Jan Mayen"), &
                                              top_level_domain_type(code="sk", name="The Slovak Republic"), &
                                              top_level_domain_type(code="sl", name="Sierra Leone"), &
                                              top_level_domain_type(code="sm", name="San Marino"), &
                                              top_level_domain_type(code="sn", name="Senegal"), &
                                              top_level_domain_type(code="so", name="Somalia"), &
                                              top_level_domain_type(code="sr", name="Suriname"), &
                                              top_level_domain_type(code="st", name="Sao Tome and Principe"), &
                                              top_level_domain_type(code="su", name="The Union of Soviet Socialist Republics"), &
                                              top_level_domain_type(code="sv", name="El Salvador"), &
                                              top_level_domain_type(code="sy", name="Syrian Arab Republic"), &
                                              top_level_domain_type(code="sz", name="Swaziland"), &
                                              top_level_domain_type(code="tc", name="Turks and Caicos Islands"), &
                                              top_level_domain_type(code="td", name="Chad"), &
                                              top_level_domain_type(code="tf", name="French Southern Territories"), &
                                              top_level_domain_type(code="tg", name="Togo"), &
                                              top_level_domain_type(code="th", name="The Kingdom of Thailand"), &
                                              top_level_domain_type(code="tj", name="Tajikistan"), &
                                              top_level_domain_type(code="tk", name="Tokelau"), &
                                              top_level_domain_type(code="tl", name="East Timor"), &
                                              top_level_domain_type(code="tm", name="Turkmenistan"), &
                                              top_level_domain_type(code="tn", name="Tunisia"), &
                                              top_level_domain_type(code="to", name="Tonga"), &
                                              top_level_domain_type(code="tp", name="East Timor"), &
                                              top_level_domain_type(code="tr", name="The Republic of Turkey"), &
                                              top_level_domain_type(code="tt", name="Trinidad and Tobago"), &
                                              top_level_domain_type(code="tv", name="Tuvalu"), &
                                              top_level_domain_type(code="tw", name="Taiwan, Province of China"), &
                                              top_level_domain_type(code="tz", name="United Republic of Tanzania"), &
                                              top_level_domain_type(code="ua", name="Ukraine"), &
                                              top_level_domain_type(code="ug", name="Uganda"), &
                                top_level_domain_type(code="uk", name="The United Kingdom of Great Britain and Northern Ireland"), &
                                              top_level_domain_type(code="um", name="United States Minor Outlying Islands"), &
                                              top_level_domain_type(code="us", name="The United States of America"), &
                                              top_level_domain_type(code="uy", name="The Eastern Republic of Uruguay"), &
                                              top_level_domain_type(code="uz", name="Uzbekistan"), &
                                              top_level_domain_type(code="va", name="Holy See (Vatican City State)"), &
                                              top_level_domain_type(code="vc", name="Saint Vincent and the Grenadines"), &
                                              top_level_domain_type(code="ve", name="The Republic of Venezuela"), &
                                              top_level_domain_type(code="vg", name="Virgin Islands, British"), &
                                              top_level_domain_type(code="vi", name="Virgin Islands, U.S."), &
                                              top_level_domain_type(code="vn", name="Vietnam"), &
                                              top_level_domain_type(code="vu", name="Vanuatu"), &
                                              top_level_domain_type(code="wf", name="Wallis and Futuna"), &
                                              top_level_domain_type(code="ws", name="Samoa"), &
                                              top_level_domain_type(code="ye", name="Yemen"), &
                                              top_level_domain_type(code="yt", name="Mayotte"), &
                                              top_level_domain_type(code="yu", name="Yugoslavia, AKA Serbia-Montenegro"), &
                                              top_level_domain_type(code="za", name="The Republic of South Africa"), &
                                              top_level_domain_type(code="zm", name="Zambia"), &
                                              top_level_domain_type(code="zw", name="Republic of Zimbabwe"), &
                                              top_level_domain_type(code="aero", name="Air Transport Industry"), &
                                              top_level_domain_type(code="biz", name="Businesses"), &
                                              top_level_domain_type(code="com", name="Commercial"), &
                                              top_level_domain_type(code="coop", name="Cooperative Associations"), &
                                              top_level_domain_type(code="info", name="Info"), &
                                              top_level_domain_type(code="jobs", name="Employment"), &
                                              top_level_domain_type(code="mobi", name="Mobile products"), &
                                              top_level_domain_type(code="museum", name="Museums"), &
                                              top_level_domain_type(code="name", name="Individuals"), &
                                              top_level_domain_type(code="net", name="Network"), &
                                              top_level_domain_type(code="org", name="Non-profit Organization"), &
                                              top_level_domain_type(code="pro", name="Credentialed professionals"), &
                                              top_level_domain_type(code="tel", name="Contact data"), &
                                              top_level_domain_type(code="travel", name="Travel industry"), &
                                              top_level_domain_type(code="gov", name="United States Government"), &
                                              top_level_domain_type(code="edu", name="Educational"), &
                                              top_level_domain_type(code="mil", name="United States Military"), &
                                              top_level_domain_type(code="int", name="International Treaties"), &
                                              top_level_domain_type(code="uucp", name="Unix to Unix CoPy"), &
                                          top_level_domain_type(code="arpa", name="Advanced Research Projects Agency (U.S. DoD)"), &
                                              top_level_domain_type(code="abudhabi", name="Abu Dhabi"), &
                                              top_level_domain_type(code="africa", name="Africa"), &
                                              top_level_domain_type(code="alsace", name="Alsace, France"), &
                                              top_level_domain_type(code="amsterdam", name="Amsterdam, The Netherlands"), &
                                              top_level_domain_type(code="arab", name="League of Arab States"), &
                                              top_level_domain_type(code="asia", name="Asia-Pacific region"), &
                                              top_level_domain_type(code="bar", name="Bar, Montenegro"), &
                                              top_level_domain_type(code="barcelona", name="Barcelona, Spain"), &
                                              top_level_domain_type(code="bayern", name="Bavaria, Germany"), &
                                              top_level_domain_type(code="bcn", name="Barcelona, Spain"), &
                                              top_level_domain_type(code="berlin", name="Berlin, Germany"), &
                                              top_level_domain_type(code="boston", name="Boston, Massachusetts"), &
                                              top_level_domain_type(code="brussels", name="Brussels, Belgium"), &
                                              top_level_domain_type(code="budapest", name="Budapest, Hungary"), &
                                              top_level_domain_type(code="bzh", name="Brittany, France"), &
                                              top_level_domain_type(code="capetown", name="Cape Town, South Africa"), &
                                              top_level_domain_type(code="cat", name="Catalonia, Spain"), &
                                              top_level_domain_type(code="cologne", name="Cologne, Germany"), &
                                              top_level_domain_type(code="corsica", name="Corsica, France"), &
                                              top_level_domain_type(code="cymru", name="Wales, United Kingdom"), &
                                              top_level_domain_type(code="doha", name="Doha"), &
                                              top_level_domain_type(code="dubai", name="Dubai"), &
                                              top_level_domain_type(code="durban", name="Durban, South Africa"), &
                                              top_level_domain_type(code="eus", name="Basque, Spain and France"), &
                                              top_level_domain_type(code="frl", name="Friesland, Netherlands"), &
                                              top_level_domain_type(code="gal", name="Galicia, Spain"), &
                                              top_level_domain_type(code="gent", name="Ghent, Belgium"), &
                                              top_level_domain_type(code="hamburg", name="Hamburg, Germany"), &
                                              top_level_domain_type(code="helsinki", name="Helsinki, Finland"), &
                                              top_level_domain_type(code="irish", name="Ireland"), &
                                              top_level_domain_type(code="ist", name="İstanbul, Turkey"), &
                                              top_level_domain_type(code="istanbul", name="İstanbul, Turkey"), &
                                              top_level_domain_type(code="joburg", name="Johannesburg, South Africa"), &
                                              top_level_domain_type(code="kiwi", name="New Zealanders"), &
                                              top_level_domain_type(code="koeln", name="Cologne, Germany"), &
                                              top_level_domain_type(code="krd", name="Kurdistan"), &
                                              top_level_domain_type(code="kyoto", name="Kyoto, Japan"), &
                                              top_level_domain_type(code="lat", name="Latin America"), &
                                              top_level_domain_type(code="london", name="London, United Kingdom"), &
                                              top_level_domain_type(code="madrid", name="Madrid, Spain"), &
                                              top_level_domain_type(code="melbourne", name="Melbourne, Australia"), &
                                              top_level_domain_type(code="miami", name="Miami, Florida"), &
                                              top_level_domain_type(code="nagoya", name="Nagoya, Japan"), &
                                              top_level_domain_type(code="nrw", name="North Rhine-Westphalia, Germany"), &
                                              top_level_domain_type(code="nyc", name="New York City, New York"), &
                                              top_level_domain_type(code="okinawa", name="Okinawa, Japan"), &
                                              top_level_domain_type(code="osaka", name="Osaka, Japan"), &
                                              top_level_domain_type(code="paris", name="Paris, France"), &
                                              top_level_domain_type(code="quebec", name="Québec, Canada"), &
                                              top_level_domain_type(code="rio", name="Rio de Janeiro, Brazil"), &
                                              top_level_domain_type(code="ruhr", name="Ruhr, Germany"), &
                                              top_level_domain_type(code="ryukyu", name="Ryukyu Islands, Japan"), &
                                              top_level_domain_type(code="saarland", name="Saarland, Germany"), &
                                              top_level_domain_type(code="scot", name="Scotland, United Kingdom"), &
                                              top_level_domain_type(code="stockholm", name="Stockholm, Sweden"), &
                                              top_level_domain_type(code="swiss", name="Switzerland"), &
                                              top_level_domain_type(code="sydney", name="Sydney, Australia"), &
                                              top_level_domain_type(code="taipei", name="Taipei, Taiwan"), &
                                              top_level_domain_type(code="tatar", name="Tatars"), &
                                              top_level_domain_type(code="tirol", name="Tyrol, Austria"), &
                                              top_level_domain_type(code="tokyo", name="Tokyo, Japan"), &
                                              top_level_domain_type(code="vegas", name="Las Vegas, Nevada"), &
                                              top_level_domain_type(code="wales", name="Wales, United Kingdom"), &
                                              top_level_domain_type(code="wien", name="Vienna, Austria"), &
                                              top_level_domain_type(code="yokohama", name="Yokohama, Japan"), &
                                              top_level_domain_type(code="zuerich", name="Zurich, Switzerland"), &
                                              top_level_domain_type(code="xn--1qqw23a", name="Foshan, China"), &
                                              top_level_domain_type(code="xn--xhq521b", name="Guangdong, China"), &
                                              top_level_domain_type(code="xn--80adxhks", name="Moscow, Russia"), &
                                              top_level_domain_type(code="xn--p1acf", name="Russia"), &
                                              top_level_domain_type(code="xn--mgbca7dzdo", name="Abu Dhabi"), &
                                              top_level_domain_type(code="xn--ngbrx", name="Arab")]
    type(top_level_domain_type), parameter :: TOP_LEVEL_DOMAINS(*) = [TOP_LEVEL_DOMAINS_1, TOP_LEVEL_DOMAINS_2]

    ! Syntax tables used for quick parsing (upstream's
    ! mail-extr-address-syntax-table and the others).
    type(handle_type) :: address_syntax_table, address_comment_syntax_table, address_domain_literal_syntax_table, &
                         address_text_comment_syntax_table, address_text_syntax_table

    !> Extract full name and canonical address from ADDRESS.
    !> ADDRESS should be in RFC 822 (or later) format.
    !> COMPONENTS gets one element, the full name (`%full_name') and the
    !> canonical address (`%address') of the first recipient; none when
    !> ADDRESS holds no address.  If no name can be extracted, the full
    !> name is unallocated.  Also see `mail_extr_ignore_single_names' and
    !> `mail_extr_ignore_realname_equals_mailbox_name'.
    !>
    !> If the optional argument ALL is true, then ADDRESS can contain zero
    !> or more recipients, separated by commas, and COMPONENTS gets one
    !> element for each recipient.  If ALL is false, then if ADDRESS
    !> contains more than one recipients, all but the first is ignored.
    !>
    !> ADDRESS may be a string or a buffer.  If it is a buffer, the visible
    !> (narrowed) portion of the buffer will be interpreted as the address.
    !>
    !> This procedure is primarily meant for when you're displaying the
    !> result to the user: Many prettifications are applied to the
    !> result returned.  Port of mail-extr.el:mail-extract-address-components
    !> (mail-header-parse-address, which it points to for other uses, is
    !> not ported).
    interface extract_mail_address_components
        module procedure extract_from_string, extract_from_buffer
    end interface extract_mail_address_components

contains

    !> Set the default of `mail_extr_full_name_prefixes' and make the
    !> syntax tables; register `what_domain'.  Not a port.
    subroutine initialize_mail_extract()
        type(status_type) :: st

        mail_extr_full_name_prefixes = "\(Prof\|D[Rr]\|Mrs?\|Rev\|Rabbi\|SysOp\|LCDR\)\.?[ "//TAB//LF//"]"
        mail_extr_disable_voodoo%regular_expression = "\cj"
        call make_syntax_tables(st)
        commands%what_domain%execute => execute_what_domain
    end subroutine initialize_mail_extract

    ! Make the five syntax tables.  Port of the syntax table setup in
    ! mail-extr.el.
    subroutine make_syntax_tables(status)
        type(status_type), intent(out) :: status

        call make_syntax_table(address_syntax_table, status)
        if (.not. status%ok()) return
        call make_syntax_table(address_comment_syntax_table, status)
        if (.not. status%ok()) return
        call make_syntax_table(address_domain_literal_syntax_table, status)
        if (.not. status%ok()) return
        call make_syntax_table(address_text_comment_syntax_table, status)
        if (.not. status%ok()) return
        call make_syntax_table(address_text_syntax_table, status)
        if (.not. status%ok()) return
        ! mail-extr-address-syntax-table
        call modify(address_syntax_table, 0, 31, "w")
        call modify(address_syntax_table, 32, 32, " ")
        call modify(address_syntax_table, iachar("!"), iachar("~"), "w")
        call modify(address_syntax_table, 127, 127, "w")
        call modify(address_syntax_table, 9, 9, " ")
        call modify(address_syntax_table, 13, 13, " ")
        call modify(address_syntax_table, 10, 10, " ")
        call modify_each(address_syntax_table, "()<>@,;:", ".")
        call modify(address_syntax_table, iachar("\"), iachar("\"), "\")
        call modify(address_syntax_table, iachar(""""), iachar(""""), """")
        ! % and ! aren't RFC 822 (or later) characters, but it is
        ! convenient to pretend.
        call modify_each(address_syntax_table, ".[]%!", ".")
        ! mail-extr-address-comment-syntax-table
        call modify(address_comment_syntax_table, 0, 255, "w")
        call modify_each(address_comment_syntax_table, " "//TAB//CR//LF, " ")
        call modify(address_comment_syntax_table, NO_BREAK_SPACE_CODE, NO_BREAK_SPACE_CODE, " ")
        call modify(address_comment_syntax_table, iachar("("), iachar("("), "()")
        call modify(address_comment_syntax_table, iachar(")"), iachar(")"), ")(")
        call modify(address_comment_syntax_table, iachar("\"), iachar("\"), "\")
        ! mail-extr-address-domain-literal-syntax-table
        call modify(address_domain_literal_syntax_table, 0, 255, "w")
        call modify_each(address_domain_literal_syntax_table, " "//TAB//CR//LF, " ")
        call modify(address_domain_literal_syntax_table, NO_BREAK_SPACE_CODE, NO_BREAK_SPACE_CODE, " ")
        call modify(address_domain_literal_syntax_table, iachar("["), iachar("["), "(]")
        call modify(address_domain_literal_syntax_table, iachar("]"), iachar("]"), ")[")
        call modify(address_domain_literal_syntax_table, iachar("\"), iachar("\"), "\")
        ! mail-extr-address-text-comment-syntax-table
        call modify(address_text_comment_syntax_table, 0, 255, "w")
        call modify_each(address_text_comment_syntax_table, " "//TAB//CR//LF, " ")
        call modify(address_text_comment_syntax_table, NO_BREAK_SPACE_CODE, NO_BREAK_SPACE_CODE, " ")
        call modify(address_text_comment_syntax_table, iachar("("), iachar("("), "()")
        call modify(address_text_comment_syntax_table, iachar(")"), iachar(")"), ")(")
        call modify(address_text_comment_syntax_table, iachar("["), iachar("["), "(]")
        call modify(address_text_comment_syntax_table, iachar("]"), iachar("]"), ")[")
        call modify(address_text_comment_syntax_table, iachar("{"), iachar("{"), "(}")
        call modify(address_text_comment_syntax_table, iachar("}"), iachar("}"), "){")
        call modify(address_text_comment_syntax_table, iachar("\"), iachar("\"), "\")
        call modify(address_text_comment_syntax_table, iachar(""""), iachar(""""), """")
        ! mail-extr-address-text-syntax-table
        call modify(address_text_syntax_table, 0, 127, ".")
        call modify(address_text_syntax_table, 128, 255, "w")
        call modify_each(address_text_syntax_table, " "//TAB//CR//LF, " ")
        call modify(address_text_syntax_table, iachar("A"), iachar("Z"), "w")
        call modify(address_text_syntax_table, iachar("a"), iachar("z"), "w")
        call modify_each(address_text_syntax_table, "-}{|'~", "w")
        call modify(address_text_syntax_table, iachar("0"), iachar("9"), "w")

    contains

        ! Give characters FROM to TO the syntax ENTRY in TABLE.
        subroutine modify(table, from, to, entry)
            type(handle_type), intent(in) :: table
            integer, intent(in) :: from, to
            character(len=*), intent(in) :: entry

            if (.not. status%ok()) return
            call modify_syntax_entry(from, to, entry, status, table)
        end subroutine modify

        ! Give each character of CHARACTERS the syntax ENTRY in TABLE.
        subroutine modify_each(table, characters, entry)
            type(handle_type), intent(in) :: table
            character(len=*), intent(in) :: characters, entry
            integer :: k

            do k = 1, len(characters)
                call modify(table, iachar(characters(k:k)), iachar(characters(k:k)), entry)
            end do
        end subroutine modify_each
    end subroutine make_syntax_tables

    ! ------------------------------------------------------------------
    ! Utility procedures
    ! ------------------------------------------------------------------

    ! Skip whitespace forward (the Latin-1 no-break space included).
    ! Port of mail-extr.el:mail-extr-skip-whitespace-forward.
    subroutine skip_whitespace_forward(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved

        moved = skip_characters_forward(WHITESPACE, status)
    end subroutine skip_whitespace_forward

    ! Skip whitespace backward.  Port of
    ! mail-extr.el:mail-extr-skip-whitespace-backward.
    subroutine skip_whitespace_backward(status)
        type(status_type), intent(out) :: status
        integer(int64) :: moved

        moved = skip_characters_backward(WHITESPACE, status)
    end subroutine skip_whitespace_backward

    ! Undo \ quoting between BEGINNING and END.  Port of
    ! mail-extr.el:mail-extr-undo-backslash-quoting.
    subroutine undo_backslash_quoting(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        type(restriction_type) :: restriction
        integer(int64) :: found

        call save_excursion_begin(excursion)
        call save_restriction_begin(restriction)
        call narrow_to_region(beginning, end, status)

        if (status%ok()) then
            call goto_character(point_minimum())

            do
                call search_forward("\", found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. found == 0) exit
                call delete_character(-1_int64, status)
                if (.not. status%ok()) exit

                if (.not. is_end_of_buffer()) then
                    call forward_character(status, 1_int64)
                    if (.not. status%ok()) exit
                end if
            end do
        end if

        call save_restriction_end(restriction)
        call save_excursion_end(excursion)
    end subroutine undo_backslash_quoting

    ! Replace the character at POSITION with a space.  Port of
    ! mail-extr.el:mail-extr-nuke-char-at.
    subroutine blank_character_at(position, status)
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion

        call save_excursion_begin(excursion)
        call goto_character(position)
        call delete_character(1_int64, status)
        if (status%ok()) call insert(" ", status)
        call save_excursion_end(excursion)
    end subroutine blank_character_at

    ! Delete the elements of POSITIONS outside BEGINNING..END; with
    ! BLANK, also replace the character at each of them with a space.
    ! Port of mail-extr.el:mail-extr-nuke-outside-range.
    subroutine remove_outside_range(positions, beginning, end, blank, status)
        integer(int64), allocatable, intent(inout) :: positions(:)
        integer(int64), intent(in) :: beginning, end
        logical, intent(in) :: blank
        type(status_type), intent(out) :: status
        logical, allocatable :: outside(:)
        integer :: k

        status = status_type()
        outside = positions > end .or. positions < beginning

        if (blank) then
            do k = 1, size(positions)
                if (outside(k)) then
                    call blank_character_at(positions(k), status)
                    if (.not. status%ok()) return
                end if
            end do
        end if

        positions = pack(positions,.not. outside)
    end subroutine remove_outside_range

    ! Safely skip over COUNT balanced expressions, if there is one: MOVED
    ! is false when the parentheses are unbalanced; any other scan error
    ! is an error condition.  Port of mail-extr.el:mail-extr-safe-move-sexp.
    subroutine move_over_expression(count, moved, status)
        integer(int64), intent(in) :: count
        logical, intent(out) :: moved
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: message_text
        integer(int64) :: position, beginning, end

        moved = .false.
        position = scan_expressions(point(), count, status)

        if (.not. status%ok()) then
            if (status%code /= SCAN_ERROR_CODE) return
            call scan_error_data(status, message_text, beginning, end)
            if (message_text == UNBALANCED_PARENTHESES) status = status_type()
            return
        end if

        if (position /= 0) call goto_character(position)
        moved = .true.
    end subroutine move_over_expression

    ! The character after POSITION in the current buffer; -1 when there
    ! is none (upstream's nil).  Not a port.
    integer(int32) function character_at(position)
        integer(int64), intent(in) :: position
        logical :: found

        call character_after(character_at, found, position)
        if (.not. found) character_at = -1
    end function character_at

    ! Whether the top-level domain CODE is in the table (an exact,
    ! case-sensitive match, as upstream's table lookup is).
    ! Not a port.
    logical function is_top_level_domain(code)
        character(len=*), intent(in) :: code

        is_top_level_domain = .false.
        if (len(code) == 0 .or. len(code) > len(TOP_LEVEL_DOMAINS(1)%code)) return
        is_top_level_domain = any(TOP_LEVEL_DOMAINS%code == code)
    end function is_top_level_domain

    ! ------------------------------------------------------------------
    ! The main procedure to grind addresses
    ! ------------------------------------------------------------------

    ! `extract_mail_address_components' of the string ADDRESS.
    ! Port of mail-extr.el:mail-extract-address-components (a string
    ! ADDRESS).
    subroutine extract_from_string(address, components, status, all)
        character(len=*), intent(in) :: address
        type(mail_address_components_type), allocatable, intent(out) :: components(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: all
        type(handle_type) :: none

        call extract_addresses(address, none, .false., components, status, all)
    end subroutine extract_from_string

    ! `extract_mail_address_components' of the accessible part of the
    ! buffer ADDRESS.  Port of mail-extr.el:mail-extract-address-components
    ! (a buffer ADDRESS).
    subroutine extract_from_buffer(address, components, status, all)
        type(handle_type), intent(in) :: address
        type(mail_address_components_type), allocatable, intent(out) :: components(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: all

        call extract_addresses("", address, .true., components, status, all)
    end subroutine extract_from_buffer

    ! The work of `extract_mail_address_components': TEXT, or with
    ! FROM_BUFFER the accessible part of SOURCE, is the address.  The
    ! current buffer is current again afterwards.  Port of
    ! mail-extr.el:mail-extract-address-components.
    subroutine extract_addresses(text, source, from_buffer, components, status, all)
        character(len=*), intent(in) :: text
        type(handle_type), intent(in) :: source
        logical, intent(in) :: from_buffer
        type(mail_address_components_type), allocatable, intent(out) :: components(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: all
        type(handle_type) :: old, canonicalization_buffer, extraction_buffer
        logical :: every

        every = .false.
        if (present(all)) every = all
        allocate (components(0))
        old = current_buffer()
        call get_or_make_buffer(" *canonical address*", canonicalization_buffer, status)
        if (.not. status%ok()) return
        call get_or_make_buffer(" *extract address components*", extraction_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(extraction_buffer, status)
        if (.not. status%ok()) return
        call extract(status)
        ! As with-current-buffer restores it: only a live buffer.
        call set_buffer_if_live(old)

    contains

        ! Fill the extraction buffer and extract each address.
        subroutine extract(status)
            type(status_type), intent(out) :: status
            integer(int64) :: found, moved
            logical :: changed

            ! buffer-disable-undo.
            buffers(current)%undo_enabled = .false.
            if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)
            call set_syntax_table(address_syntax_table, status)
            if (.not. status%ok()) return
            call widen()
            call erase_buffer(status)
            if (.not. status%ok()) return
            call case_fold_search%set_local(.false.)
            ! Insert extra space at beginning to allow later replacement with <
            ! without having to move markers.
            call insert(" ", status)
            if (.not. status%ok()) return

            ! Insert the address itself.
            if (from_buffer) then
                call insert_buffer_substring(source, status)
            else
                call insert(text, status)
            end if

            if (.not. status%ok()) return
            call set_text_properties(point_minimum(), point_maximum(), value_type(), changed, status)
            if (.not. status%ok()) return
            call set_buffer(canonicalization_buffer, status)
            if (.not. status%ok()) return
            buffers(current)%undo_enabled = .false.
            if (allocated(buffers(current)%undo_entries)) deallocate (buffers(current)%undo_entries)
            call case_fold_search%set_local(.false.)
            call set_buffer(extraction_buffer, status)
            if (.not. status%ok()) return
            ! Unfold multiple lines.
            call goto_character(point_minimum())

            do
                call regular_expression_search_forward("\([^\]\(\\\\\)*\)"//LF//"[ "//TAB//"]", found, status, &
                                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found == 0) exit
                call replace_match("\1 ", status, fixedcase=.true.)
                if (.not. status%ok()) return
            end do

            ! Loop over addresses until we have as many as we want.
            do
                if (.not. every .and. size(components) > 0) exit
                call goto_character(point_minimum())
                moved = skip_characters_forward(" "//TAB, status)
                if (.not. status%ok()) return
                if (is_end_of_buffer()) exit
                call extract_one_address(canonicalization_buffer, extraction_buffer, every, components, status)
                if (.not. status%ok()) return
            end do
        end subroutine extract
    end subroutine extract_addresses

    ! Extract the first address of the extraction buffer (the current
    ! buffer) and append its full name and canonical address to
    ! COMPONENTS; with EVERY, delete it from the buffer.  Port of the
    ! body of the loop over addresses in
    ! mail-extr.el:mail-extract-address-components.
    subroutine extract_one_address(canonicalization_buffer, extraction_buffer, every, components, status)
        type(handle_type), intent(in) :: canonicalization_buffer, extraction_buffer
        logical, intent(in) :: every
        type(mail_address_components_type), allocatable, intent(inout) :: components(:)
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: opening_angles(:), closing_angles(:), at_signs(:), colons(:), commas(:), &
                                       exclamation_marks(:), percent_signs(:), semicolons(:), saved_at_signs(:)
        type(handle_type), allocatable :: percent_markers(:)
        type(handle_type) :: end_of_address, closing_marker, route_marker, marker
        type(mail_address_components_type) :: value
        type(excursion_type) :: excursion
        type(status_type) :: st
        integer(int32) :: code, next, first_initial, last_initial, middle_initial
        integer(int64) :: opening_angle, closing_angle, at_sign, group_colon, group_semicolon, route_colon, first_real, &
                          last_real, phrase_beginning, phrase_end, comment_beginning, comment_end, quote_beginning, &
                          quote_end, atom_beginning, atom_end, mailbox_beginning, mailbox_end, temporary, position, found, moved, &
                          buffer_length, i
        logical :: has_end_of_address, split, blank, moved_over, dot_ends_name, disable_initial_guessing, names_match
        integer :: k

        allocate (opening_angles(0), closing_angles(0), at_signs(0), colons(0), commas(0), exclamation_marks(0), &
                  percent_signs(0), semicolons(0), saved_at_signs(0))
        has_end_of_address = .false.
        group_colon = 0
        group_semicolon = 0
        route_colon = 0
        first_real = 0
        last_real = 0
        phrase_beginning = 0
        phrase_end = 0
        comment_beginning = 0
        comment_end = 0
        quote_beginning = 0
        quote_end = 0
        disable_initial_guessing = .false.
        call set_syntax_table(address_syntax_table, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())

        ! Insert extra space at beginning to allow later replacement with <
        ! without having to move markers.
        if (following_character() /= iachar(" ")) then
            call insert(" ", status)
            if (.not. status%ok()) return
        end if

        ! First pass grabs useful information about address.
        do
            call skip_whitespace_forward(status)
            if (.not. status%ok()) return
            if (is_end_of_buffer()) exit
            code = character_at(point())
            if (first_real == 0 .and. code /= iachar("(")) first_real = point()
            split = .false.

            ! Commas delimit addresses when outside < > pairs.
            if (code == iachar(",")) then
                ! Handle ROUTE-ADDR address that is missing its <.
                if (size(opening_angles) == 0 .and. character_at(point() + 1) /= iachar("@")) then
                    split = .true.
                else if (size(closing_angles) > 0) then
                    if (size(opening_angles) == 0) then
                        call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer or type(marker_type), got .false.")
                        return
                    end if

                    ! Handle weird munged addresses.
                    split = opening_angles(size(opening_angles)) < closing_angles(1)
                end if
            end if

            if (code == iachar("(")) then
                ! A comment.
                call set_syntax_table(address_comment_syntax_table, status)
                if (.not. status%ok()) return

                ! Only record the first non-empty comment's position.
                if (comment_beginning == 0) then
                    call save_excursion_begin(excursion)
                    call forward_character(status, 1_int64)
                    if (status%ok()) call skip_whitespace_forward(status)
                    next = character_at(point())
                    call save_excursion_end(excursion)
                    if (.not. status%ok()) return
                    if (next /= iachar(")")) comment_beginning = point()
                end if

                call move_or_step(status)
                if (.not. status%ok()) return
                call set_syntax_table(address_syntax_table, status)
                if (.not. status%ok()) return
                if (comment_beginning /= 0 .and. comment_end == 0) comment_end = point()
            else if (code == iachar("""")) then
                ! Quoted text: only record the first non-empty quote's position.
                if (quote_beginning == 0) then
                    call save_excursion_begin(excursion)
                    call forward_character(status, 1_int64)
                    if (status%ok()) call skip_whitespace_forward(status)
                    next = character_at(point())
                    call save_excursion_end(excursion)
                    if (.not. status%ok()) return
                    if (next /= iachar("""")) quote_beginning = point()
                end if

                call move_or_step(status)
                if (.not. status%ok()) return
                if (quote_beginning /= 0 .and. quote_end == 0) quote_end = point()
            else if (code == iachar("[")) then
                ! Domain literals.
                call set_syntax_table(address_domain_literal_syntax_table, status)
                if (.not. status%ok()) return
                call move_or_step(status)
                if (.not. status%ok()) return
                call set_syntax_table(address_syntax_table, status)
                if (.not. status%ok()) return
            else if (split) then
                ! The argument contains more than one address.
                ! Temporarily hide everything after this one.
                call copy_marker_at(point() + 1, end_of_address, .true., status)
                if (.not. status%ok()) return
                has_end_of_address = .true.
                call narrow_to_region(point_minimum(), point() + 1, status)
                if (.not. status%ok()) return
                call delete_character(1_int64, status)
                if (.not. status%ok()) return
                code = iachar("(")
            else if (is_one_of(code, "<>@:,!%;")) then
                ! Record the position of various interesting chars, determine
                ! validity later.
                select case (achar(code))
                case ("<")
                    opening_angles = [point(), opening_angles]
                case (">")
                    closing_angles = [point(), closing_angles]
                case ("@")
                    at_signs = [point(), at_signs]
                case (":")
                    colons = [point(), colons]
                case (",")
                    commas = [point(), commas]
                case ("!")
                    exclamation_marks = [point(), exclamation_marks]
                case ("%")
                    percent_signs = [point(), percent_signs]
                case (";")
                    semicolons = [point(), semicolons]
                case default
                    ! `is_one_of' admits only the characters above.
                end select

                call forward_character(status, 1_int64)
                if (.not. status%ok()) return
            else if (code == iachar(".")) then
                call forward_character(status, 1_int64)
                if (.not. status%ok()) return
            else if (is_one_of(code, ")]\")) then
                ! A comment terminator or domain literal terminator is invalid
                ! here, and \ is allowed only within quoted strings, domain
                ! literals and comments.
                call blank_character_at(point(), status)
                if (.not. status%ok()) return
                call forward_character(status, 1_int64)
                if (.not. status%ok()) return
            else
                ! Move over a word, recognizing non-ASCII characters except
                ! Latin-1 nbsp as words.
                do
                    moved = skip_characters_forward("^"//achar(0)//"-"//achar(127)//NO_BREAK_SPACE, status)
                    if (.not. status%ok()) return
                    if (is_end_of_buffer()) exit
                    if (character_syntax(character_at(point())) /= "w") exit
                    call forward_word(1_int64)
                    if (is_end_of_buffer()) exit
                    next = character_at(point())
                    if (next <= 127 .or. next == NO_BREAK_SPACE_CODE) exit
                end do
            end if

            ! At the end of first address of a multiple address header, no
            ! last real position.
            if (code /= iachar("(") .and. .not. (code == iachar(",") .and. is_end_of_buffer())) last_real = point()
        end do

        ! Use only the leftmost <, if any.  Replace all others with spaces.
        do k = 1, size(opening_angles) - 1
            call blank_character_at(opening_angles(k), status)
            if (.not. status%ok()) return
        end do

        if (size(opening_angles) > 1) opening_angles = opening_angles(size(opening_angles):)

        ! Use only the rightmost >, if any.  Replace all others with spaces.
        do k = 2, size(closing_angles)
            call blank_character_at(closing_angles(k), status)
            if (.not. status%ok()) return
        end do

        if (size(closing_angles) > 1) closing_angles = closing_angles(1:1)

        ! If multiple @s and a :, but no < and >, insert around buffer.
        ! Example: @foo.bar.dom,@xxx.yyy.zzz:mailbox@aaa.bbb.ccc
        ! This commonly happens on the UUCP "From " line.
        if (size(at_signs) > 1 .and. size(colons) == 1 .and. size(semicolons) == 0 .and. size(opening_angles) == 0) then
            call goto_character(point_minimum())
            call delete_character(1_int64, status)
            if (.not. status%ok()) return
            opening_angles = [point()]
            call insert("<", status)
            if (.not. status%ok()) return
        end if

        ! If < but no >, insert > in rightmost possible position.
        if (size(opening_angles) > 0 .and. size(closing_angles) == 0) then
            call goto_character(point_maximum())
            closing_angles = [point()]
            call insert(">", status)
            if (.not. status%ok()) return
        end if

        ! If > but no <, replace > with space.
        if (size(closing_angles) > 0 .and. size(opening_angles) == 0) then
            call blank_character_at(closing_angles(1), status)
            if (.not. status%ok()) return
            deallocate (closing_angles)
            allocate (closing_angles(0))
        end if

        opening_angle = 0
        if (size(opening_angles) > 0) opening_angle = opening_angles(1)
        closing_angle = 0
        if (size(closing_angles) > 0) closing_angle = closing_angles(1)

        ! Trim other punctuation lists of items outside < > pair to handle
        ! MTAs that stick punctuation marks outside of a < > pair.
        if (opening_angle /= 0) then
            call remove_outside_range(at_signs, opening_angle, closing_angle, .false., status)
            if (.not. status%ok()) return
            call remove_outside_range(exclamation_marks, opening_angle, closing_angle, .false., status)
            if (.not. status%ok()) return
            call remove_outside_range(percent_signs, opening_angle, closing_angle, .false., status)
            if (.not. status%ok()) return
        end if

        ! Check for : that indicates GROUP list and for : part of
        ! ROUTE-ADDR spec.  Can't possibly be more than two :.  Blank any
        ! extra.
        do k = 1, size(colons)
            temporary = colons(k)

            if (opening_angle /= 0 .and. closing_angle /= 0 .and. temporary > opening_angle .and. temporary < closing_angle) then
                blank = route_colon /= 0 .or. size(at_signs) < 2
                if (.not. blank) blank = temporary > at_signs(1) .or. temporary < at_signs(2)

                if (blank) then
                    call blank_character_at(temporary, status)
                    if (.not. status%ok()) return
                else
                    route_colon = temporary
                end if
            else if (opening_angle == 0 .or. temporary < opening_angle) then
                group_colon = temporary
            end if
        end do

        ! Blank any ; that is in or to the left of a < > pair or to the left
        ! of a GROUP starting :.  Also, there may only be one ;.
        do k = 1, size(semicolons)
            temporary = semicolons(k)

            if (opening_angle /= 0 .and. closing_angle /= 0 .and. temporary > opening_angle .and. temporary < closing_angle) then
                call blank_character_at(temporary, status)
                if (.not. status%ok()) return
            else if ((group_colon == 0 .or. temporary > group_colon) .and. group_semicolon == 0) then
                group_semicolon = temporary
            end if
        end do

        ! Blank unmatched GROUP syntax characters.
        if (group_colon /= 0 .and. group_semicolon == 0) then
            call blank_character_at(group_colon, status)
            if (.not. status%ok()) return
            group_colon = 0
        end if

        if (group_semicolon /= 0 .and. group_colon == 0) then
            call blank_character_at(group_semicolon, status)
            if (.not. status%ok()) return
            group_semicolon = 0
        end if

        ! Handle junk like ";@host.company.dom" that sendmail adds.
        if (group_semicolon /= 0) then
            call remove_outside_range(exclamation_marks, group_colon, group_semicolon, .false., status)
            if (.not. status%ok()) return
            call remove_outside_range(at_signs, group_colon, group_semicolon, .false., status)
            if (.not. status%ok()) return
            call remove_outside_range(percent_signs, group_colon, group_semicolon, .false., status)
            if (.not. status%ok()) return
            call remove_outside_range(commas, group_colon, group_semicolon, .false., status)
            if (.not. status%ok()) return
            if (last_real /= 0 .and. last_real > group_semicolon + 1) last_real = group_semicolon + 1

            if (comment_end /= 0 .and. comment_end > group_semicolon) then
                comment_end = 0
                comment_beginning = 0
            end if

            if (quote_end /= 0 .and. quote_end > group_semicolon) then
                quote_end = 0
                quote_beginning = 0
            end if
        end if

        ! Any commas must be between < and : of ROUTE-ADDR.  Blank all of
        ! the commas.
        call remove_outside_range(commas, 1_int64, 1_int64, .true., status)
        if (.not. status%ok()) return

        ! Locate PHRASE part of ROUTE-ADDR.
        if (opening_angle /= 0) then
            call goto_character(opening_angle)
            call skip_whitespace_backward(status)
            if (.not. status%ok()) return
            phrase_end = point()
            call goto_character(point_minimum())
            call skip_whitespace_forward(status)
            if (.not. status%ok()) return

            if (point() < phrase_end) then
                phrase_beginning = point()
            else
                phrase_end = 0
            end if
        end if

        ! Handle ROUTE-ADDRS with real ROUTEs.
        ! If there are multiple @s, then we assume ROUTE-ADDR syntax, and
        ! any % or ! must be semantically meaningless.
        if (route_colon /= 0) then
            deallocate (exclamation_marks, percent_signs)
            allocate (exclamation_marks(0), percent_signs(0), percent_markers(0))
            call copy_marker_at(closing_angle, closing_marker, .false., status)
            if (.not. status%ok()) return
            call copy_marker_at(route_colon, route_marker, .false., status)
            if (.not. status%ok()) return
            call goto_character(closing_angle)
            call insert_before_markers("X", status)
            if (.not. status%ok()) return
            call goto_character(at_signs(1))

            do k = 2, size(at_signs)
                call delete_character(1_int64, status)
                if (.not. status%ok()) return
                call point_marker(marker)
                percent_markers = [marker, percent_markers]
                call insert("%", status)
                if (.not. status%ok()) return
                call marker_position(closing_marker, position, status)
                if (.not. status%ok()) return
                call goto_character(position - 1)
                call save_excursion_begin(excursion)
                call marker_position(route_marker, position, status)
                if (status%ok()) call insert_buffer_substring(extraction_buffer, status, at_signs(k), position)
                if (status%ok()) call marker_position(route_marker, position, status)
                if (status%ok()) call delete_region(at_signs(k), position, status)
                call save_excursion_end(excursion)
                if (.not. status%ok()) return
                if (k == size(at_signs)) saved_at_signs = [point()]
            end do

            at_signs = saved_at_signs
            call marker_position(closing_marker, position, status)
            if (.not. status%ok()) return
            call goto_character(position)
            call delete_character(-1_int64, status)
            if (.not. status%ok()) return
            call marker_position(route_marker, position, status)
            if (.not. status%ok()) return
            call blank_character_at(position, status)
            if (.not. status%ok()) return
            call destroy_marker(route_marker, status)
            if (.not. status%ok()) return
            route_colon = 0
            call marker_position(closing_marker, closing_angle, status)
            if (.not. status%ok()) return
            call destroy_marker(closing_marker, status)
            if (.not. status%ok()) return
            deallocate (percent_signs)
            allocate (percent_signs(size(percent_markers)))

            do k = 1, size(percent_markers)
                call marker_position(percent_markers(k), percent_signs(k), status)
                if (.not. status%ok()) return
                call destroy_marker(percent_markers(k), status)
                if (.not. status%ok()) return
            end do
        end if

        at_sign = 0
        if (size(at_signs) > 0) at_sign = at_signs(1)
        call canonicalize(status)
        if (.not. status%ok()) return
        ! Done canonicalizing address.  We are now back in extraction-buffer.
        call set_buffer(extraction_buffer, status)
        if (.not. status%ok()) return

        ! Decide what part of the address to search to find the full name.
        if (phrase_beginning /= 0 .and. quote_beginning == phrase_beginning .and. quote_end <= phrase_end) then
            ! Example: "First M. Last" <fml@foo.bar.dom>
            call narrow_to_region(quote_beginning + 1, quote_end - 1, status)
            if (.not. status%ok()) return
            call undo_backslash_quoting(point_minimum(), point_maximum(), status)
            if (.not. status%ok()) return
        else if (phrase_beginning /= 0) then
            ! Example: First Last <fml@foo.bar.dom>
            call narrow_to_region(phrase_beginning, phrase_end, status)
            if (.not. status%ok()) return
        else if (comment_beginning /= 0) then
            ! Example: fml@foo.bar.dom (First M. Last)
            call narrow_to_region(comment_beginning + 1, comment_end - 1, status)
            if (.not. status%ok()) return
            call undo_backslash_quoting(point_minimum(), point_maximum(), status)
            if (.not. status%ok()) return
            call goto_character(point_minimum())
        else
            ! Otherwise we try to get the name from the mailbox portion
            ! of the address.  Example: First_M_Last@foo.bar.dom
            call name_from_mailbox(status)
            if (.not. status%ok()) return
        end if

        call set_syntax_table(address_text_syntax_table, status)
        if (.not. status%ok()) return
        call clean_up_name(mailbox_beginning, mailbox_end, canonicalization_buffer, comment_beginning, comment_end, &
                           disable_initial_guessing, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())
        ! If name is "First Last" and userid is "F?L", then assume
        ! the middle initial is the second letter in the userid.
        call goto_character(point_minimum())

        if (mail_extr_guess_middle_initial .and. .not. disable_initial_guessing .and. mailbox_end - mailbox_beginning == 3) then
            call goto_character(point_minimum())

            if (looking_at(TWO_NAME_PATTERN, status)) then
                first_initial = character_at(match_beginning(0, st))
                last_initial = character_at(match_beginning(3, st))
                middle_initial = -1
                call set_buffer(canonicalization_buffer, status)
                if (.not. status%ok()) return
                ! Comparing ignoring case (char-equal with case-fold-search).
                if (downcase(first_initial) == downcase(character_at(mailbox_beginning)) .and. &
                    downcase(last_initial) == downcase(character_at(mailbox_end - 1))) middle_initial = character_at( &
                                                                                                    mailbox_beginning + 1)
                call set_buffer(extraction_buffer, status)
                if (.not. status%ok()) return

                if (middle_initial >= 0) then
                    if (character_syntax(middle_initial) == "w") then
                        call goto_character(match_beginning(3, st))
                        call insert_character(upcase(middle_initial), status)
                        if (status%ok()) call insert(". ", status)
                        if (.not. status%ok()) return
                    end if
                end if
            end if

            if (.not. status%ok()) return
        end if

        ! Drop name if it is the same as mailbox name.
        buffer_length = point_maximum() - point_minimum()

        if (buffer_length > 0 .and. buffer_length == mailbox_end - mailbox_beginning) then
            call goto_character(point_maximum())
            call insert_buffer_substring(canonicalization_buffer, status, mailbox_beginning, mailbox_end)
            if (.not. status%ok()) return
            names_match = .true.
            i = 0

            do while (names_match .and. i < buffer_length)
                if (downcase(character_at(i + point_minimum())) /= downcase(character_at(i + buffer_length + point_minimum()))) then
                    names_match = .false.
                end if

                i = i + 1
            end do

            call delete_region(point_minimum() + buffer_length, point_maximum(), status)
            if (.not. status%ok()) return

            if (names_match .and. mail_extr_ignore_realname_equals_mailbox_name) then
                call narrow_to_region(point(), point(), status)
                if (.not. status%ok()) return
            end if
        end if

        ! Drop name if it's just one word.
        call goto_character(point_minimum())

        if (mail_extr_ignore_single_names) then
            call regular_expression_search_forward("[- ]", found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return

            if (found == 0) then
                call narrow_to_region(point(), point(), status)
                if (.not. status%ok()) return
            end if
        end if

        ! Record the result.
        if (point_minimum() /= point_maximum()) value%full_name = buffer_string()
        call set_buffer(canonicalization_buffer, status)
        if (.not. status%ok()) return
        if (point_minimum() /= point_maximum()) value%address = buffer_string()
        call set_buffer(extraction_buffer, status)
        if (.not. status%ok()) return
        components = [components, value]

        ! Unless one address is all we wanted, delete this one from
        ! extraction-buffer and get ready to extract the next address.
        if (every) then
            if (has_end_of_address) then
                call marker_position(end_of_address, position, status)
                if (.not. status%ok()) return
                call narrow_to_region(1_int64, position, status)
                if (.not. status%ok()) return
            else
                call widen()
            end if

            call delete_region(point_minimum(), point_maximum(), status)
            if (.not. status%ok()) return
            call widen()
        end if

        if (has_end_of_address) call destroy_marker(end_of_address, status)

    contains

        ! Move over one balanced expression, else forward one character.
        subroutine move_or_step(status)
            type(status_type), intent(out) :: status
            logical :: moved_over

            call move_over_expression(1_int64, moved_over, status)
            if (.not. status%ok()) return
            if (.not. moved_over) call forward_character(status, 1_int64)
        end subroutine move_or_step

        ! Turn the first % into an @ when there is no @.
        subroutine percent_to_at_sign(status)
            type(status_type), intent(out) :: status

            status = status_type()
            if (size(percent_signs) == 0 .or. at_sign /= 0) return
            call goto_character(percent_signs(1))
            call delete_character(1_int64, status)
            if (.not. status%ok()) return
            at_sign = point()
            call insert("@", status)
            if (.not. status%ok()) return
            percent_signs = percent_signs(2:)
        end subroutine percent_to_at_sign

        ! Make the canonical address in the canonicalization buffer, and
        ! set MAILBOX_BEGINNING and MAILBOX_END.
        subroutine canonicalize(status)
            type(status_type), intent(out) :: status
            type(restriction_type) :: restriction
            type(handle_type) :: at_marker
            type(handle_type), allocatable :: saved_percent_markers(:)
            integer(int64) :: insert_point, saved_exclamation_mark, start, domain_position
            character(len=:), allocatable :: domain
            logical :: at_is_marker, dot_before
            integer :: k, n

            call set_buffer(canonicalization_buffer, status)
            if (.not. status%ok()) return
            call widen()
            call erase_buffer(status)
            if (.not. status%ok()) return
            call insert_buffer_substring(extraction_buffer, status)
            if (.not. status%ok()) return

            if (opening_angle /= 0) then
                call goto_character(opening_angle + 1)
                call skip_whitespace_forward(status)
                if (.not. status%ok()) return
                call narrow_to_region(point(), closing_angle, status)
            else if (first_real /= 0 .and. last_real /= 0) then
                call narrow_to_region(first_real, last_real, status)
            else
                call narrow_to_region(point_maximum(), point_maximum(), status)
            end if

            if (.not. status%ok()) return

            if (at_sign /= 0 .and. size(percent_signs) > 0) then
                call remove_outside_range(percent_signs, point_minimum(), at_sign, .true., status)
                if (.not. status%ok()) return
            end if

            if (size(percent_signs) > 0 .and. size(exclamation_marks) > 0) then
                call remove_outside_range(exclamation_marks, point_minimum(), percent_signs(1), .true., status)
                if (.not. status%ok()) return
            end if

            if (at_sign /= 0 .and. size(exclamation_marks) > 0 .and. size(percent_signs) == 0) then
                call remove_outside_range(exclamation_marks, point_minimum(), at_sign, .true., status)
                if (.not. status%ok()) return
            end if

            call percent_to_at_sign(status)
            if (.not. status%ok()) return

            if (mail_extr_mangle_uucp .and. size(exclamation_marks) > 0) then
                at_is_marker = .false.
                allocate (saved_percent_markers(0))
                call save_restriction_begin(restriction)

                if (at_sign /= 0 .and. mail_extract_at_sign_binds_tighter_than_exclamation_mark) then
                    call goto_character(at_sign)
                    percent_signs = [point(), percent_signs]
                    at_sign = 0
                    call delete_character(1_int64, status)
                    if (status%ok()) call insert("%", status)
                    insert_point = point_maximum()
                else if (mail_extract_at_sign_binds_tighter_than_exclamation_mark) then
                    insert_point = point_maximum()
                else if (size(percent_signs) > 0) then
                    insert_point = percent_signs(size(percent_signs))
                    deallocate (saved_percent_markers)
                    allocate (saved_percent_markers(size(percent_signs)))

                    do k = 1, size(percent_signs)
                        if (status%ok()) call copy_marker_at(percent_signs(k), saved_percent_markers(k), .false., status)
                    end do

                    deallocate (percent_signs)
                    allocate (percent_signs(0))

                    if (at_sign /= 0 .and. status%ok()) then
                        call copy_marker_at(at_sign, at_marker, .false., status)
                        at_is_marker = .true.
                    end if
                else if (at_sign /= 0) then
                    insert_point = at_sign
                    call copy_marker_at(at_sign, at_marker, .false., status)
                    at_is_marker = .true.
                else
                    insert_point = point_maximum()
                end if

                if (status%ok()) call narrow_to_region(point_minimum(), insert_point, status)
                saved_exclamation_mark = exclamation_marks(1)
                n = size(exclamation_marks)

                do k = 1, n
                    if (.not. status%ok()) exit
                    call goto_character(point_maximum())

                    if (at_sign == 0 .and. k == n) then
                        at_sign = point()
                        call insert_before_markers("@ ", status)
                    else
                        percent_signs = [point(), percent_signs]
                        call insert_before_markers("% ", status)
                    end if

                    if (status%ok()) call backward_character(status, 1_int64)
                    start = point_minimum()
                    if (k < n) start = exclamation_marks(k + 1) + 1
                    if (status%ok()) call insert_buffer_substring(canonicalization_buffer, status, start, exclamation_marks(k))
                    if (status%ok()) call delete_character(1_int64, status)
                    if (.not. status%ok()) exit
                    call save_excursion_begin(excursion)
                    call move_over_expression(-1_int64, moved_over, status)
                    if (status%ok()) call skip_whitespace_backward(status)
                    dot_before = preceding_character() == iachar(".")
                    call save_excursion_end(excursion)
                    if (.not. status%ok()) exit

                    if (.not. dot_before) then
                        call save_excursion_begin(excursion)
                        call skip_whitespace_backward(status)
                        dot_before = preceding_character() == iachar(".")
                        call save_excursion_end(excursion)
                        if (.not. status%ok()) exit

                        if (dot_before) then
                            call insert_before_markers("uucp", status)
                        else
                            call insert_before_markers(".uucp", status)
                        end if
                    end if
                end do

                call save_restriction_end(restriction)
                if (.not. status%ok()) return

                if (size(saved_percent_markers) > 0) then
                    call append_marker_positions(saved_percent_markers, status)
                    if (.not. status%ok()) return
                end if

                if (at_is_marker) then
                    call marker_position(at_marker, at_sign, status)
                    if (.not. status%ok()) return
                    call destroy_marker(at_marker, status)
                    if (.not. status%ok()) return
                end if

                call narrow_to_region(saved_exclamation_mark + 1, point_maximum(), status)
                if (.not. status%ok()) return
            end if

            call percent_to_at_sign(status)
            if (.not. status%ok()) return
            percent_signs = percent_signs(size(percent_signs):1:-1)

            ! A % followed by a top-level domain becomes the @ (the
            ! address is truncated there).
            do k = 1, size(percent_signs)
                if (k < size(percent_signs)) then
                    call goto_character(percent_signs(k + 1))
                else
                    call goto_character(at_sign)
                end if

                call skip_whitespace_backward(status)
                if (.not. status%ok()) return
                call save_excursion_begin(excursion)
                call move_over_expression(-1_int64, moved_over, status)
                domain_position = point()
                if (status%ok()) call skip_whitespace_backward(status)
                dot_before = preceding_character() == iachar(".")
                call save_excursion_end(excursion)
                if (.not. status%ok()) return

                if (dot_before) then
                    call buffer_substring_no_properties(domain_position, point(), domain, status)
                    if (.not. status%ok()) return

                    if (is_top_level_domain(domain)) then
                        call narrow_to_region(point_minimum(), point(), status)
                        if (.not. status%ok()) return
                        call goto_character(percent_signs(k))
                        call delete_character(1_int64, status)
                        if (.not. status%ok()) return
                        at_sign = point()
                        percent_signs = percent_signs(1:k)
                        percent_signs = pack(percent_signs, percent_signs /= at_sign)
                        call insert("@", status)
                        if (.not. status%ok()) return
                        exit
                    end if
                end if
            end do

            mailbox_beginning = point_minimum()

            if (size(percent_signs) > 0) then
                mailbox_end = percent_signs(1)
            else if (at_sign /= 0) then
                mailbox_end = at_sign
            else
                mailbox_end = point_maximum()
            end if

            ! Make the domain-name part lowercase since it's case
            ! insensitive anyway.
            if (at_sign /= 0) call downcase_region(at_sign + 1, point_maximum(), status)
        end subroutine canonicalize

        ! Put the positions of MARKERS (destroying them) before the
        ! elements of PERCENT_SIGNS.
        subroutine append_marker_positions(markers, status)
            type(handle_type), intent(in) :: markers(:)
            type(status_type), intent(out) :: status
            integer(int64), allocatable :: positions(:)
            integer :: k

            status = status_type()
            allocate (positions(size(markers)))

            do k = 1, size(markers)
                call marker_position(markers(k), positions(k), status)
                if (.not. status%ok()) return
                call destroy_marker(markers(k), status)
                if (.not. status%ok()) return
            end do

            percent_signs = [positions, percent_signs]
        end subroutine append_marker_positions

        ! Put the mailbox part of the canonical address at the end of the
        ! extraction buffer, narrowed to it, and make a name of it.
        subroutine name_from_mailbox(status)
            type(status_type), intent(out) :: status
            type(restriction_type) :: restriction
            type(status_type) :: st
            integer(int64) :: found

            call goto_character(point_maximum())
            call narrow_to_region(point(), point(), status)
            if (.not. status%ok()) return
            call insert_buffer_substring(canonicalization_buffer, status, mailbox_beginning, mailbox_end)
            if (.not. status%ok()) return
            call goto_character(point_minimum())
            ! Example: First_Last.XXX@foo.bar.dom
            call regular_expression_search_forward("[_0-9]", found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            dot_ends_name = found /= 0
            call goto_character(point_minimum())

            if (.not. mail_extr_mangle_uucp) then
                call modify_syntax_entry(iachar("!"), iachar("!"), "w", status, syntax_table())
                if (.not. status%ok()) return
            end if

            do
                call skip_whitespace_forward(status)
                if (.not. status%ok()) exit
                if (is_end_of_buffer()) exit
                code = character_at(point())

                if (code == iachar("""")) then
                    quote_beginning = point()
                    call move_or_step(status)
                    if (.not. status%ok()) exit
                    ! Take into account deletions.
                    quote_end = point() - 2
                    call save_excursion_begin(excursion)
                    call backward_character(status, 1_int64)
                    if (status%ok()) call delete_character(1_int64, status)

                    if (status%ok()) then
                        call goto_character(quote_beginning)
                        if (.not. is_end_of_buffer()) call delete_character(1_int64, status)
                    end if

                    call save_excursion_end(excursion)
                    if (.not. status%ok()) exit
                    call undo_backslash_quoting(quote_beginning, quote_end, status)
                    if (.not. status%ok()) exit
                    if (character_at(point()) /= iachar(" ")) call insert(" ", status)
                    if (.not. status%ok()) exit
                    dot_ends_name = .true.
                else if (code == iachar(".")) then
                    next = character_at(point() + 1)

                    if (next == iachar("_") .or. next == iachar("=")) then
                        call forward_character(status, 1_int64)
                        if (status%ok()) call delete_character(1_int64, status)
                        if (status%ok()) call insert(" ", status)
                    else if (dot_ends_name) then
                        call narrow_to_region(point_minimum(), point(), status)
                    else
                        call delete_character(1_int64, status)
                        if (status%ok()) call insert(" ", status)
                    end if

                    if (.not. status%ok()) exit
                else if (character_syntax(code) == "." .or. character_syntax(code) == "\") then
                    call delete_character(1_int64, status)
                    if (status%ok()) call insert(" ", status)
                    if (.not. status%ok()) exit
                else
                    atom_beginning = point()
                    call forward_word(1_int64)
                    atom_end = point()
                    call goto_character(atom_beginning)
                    call save_restriction_begin(restriction)
                    call narrow_to_region(atom_beginning, atom_end, status)
                    if (status%ok()) call name_from_atom(extraction_buffer, status)
                    call save_restriction_end(restriction)
                    if (.not. status%ok()) exit
                end if
            end do

            ! Undo the dirty deed.
            if (.not. mail_extr_mangle_uucp) then
                call modify_syntax_entry(iachar("!"), iachar("!"), ".", st, syntax_table())
                if (status%ok()) status = st
            end if
        end subroutine name_from_mailbox
    end subroutine extract_one_address

    ! Make a name of the atom the buffer (EXTRACTION_BUFFER) is narrowed
    ! to.  Port of the atom case of the loop over the mailbox in
    ! mail-extr.el:mail-extract-address-components.
    subroutine name_from_atom(extraction_buffer, status)
        type(handle_type), intent(in) :: extraction_buffer
        type(status_type), intent(out) :: status
        character(len=*), parameter :: FIELD_PATTERNS(3) = [character(len=32) :: &
                                                            X400_ENCODED_ADDRESS_GIVEN_NAME_PATTERN, &
                                                            X400_ENCODED_ADDRESS_SURNAME_PATTERN, &
                                                            X400_ENCODED_ADDRESS_FULL_NAME_PATTERN]
        type(excursion_type) :: excursion
        integer(int64) :: beginning, end, found
        integer :: k

        if (looking_at(X400_ENCODED_ADDRESS_PATTERN, status)) then
            ! Handle X.400 addresses encoded in RFC 822 or later: copy
            ! the contents of the individual fields that might hold
            ! name data to the beginning.
            do k = 1, size(FIELD_PATTERNS)
                call save_excursion_begin(excursion)
                call regular_expression_search_forward(trim(FIELD_PATTERNS(k)), found, status, noerror=SEARCH_NOERROR)
                call save_excursion_end(excursion)
                if (.not. status%ok()) return

                if (found /= 0) then
                    beginning = match_beginning(1, status)
                    if (status%ok()) end = match_end(1, status)
                    if (status%ok()) call insert_buffer_substring(extraction_buffer, status, beginning, end)
                    if (status%ok()) call insert(" ", status)
                    if (.not. status%ok()) return
                end if
            end do

            ! Discard the rest, since it contains stuff like
            ! routing information, not part of a name.
            call skip_whitespace_backward(status)
            if (.not. status%ok()) return
            call delete_region(point(), point_maximum(), status)
            if (.not. status%ok()) return

            ! Handle periods used for spacing.
            do
                call regular_expression_search_forward(BAD_DOT_PATTERN, found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok() .or. found == 0) return
                call replace_match("\1 \2", status, fixedcase=.true.)
                if (.not. status%ok()) return
            end do
        end if

        if (.not. status%ok()) return
        ! Handle normal addresses: _ and = used for spacing.
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("\([^_=]+\)[_=]", found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit
            call replace_match("\1 ", status, fixedcase=.true.)
            if (.not. status%ok()) return
        end do

        call goto_character(point_maximum())
    end subroutine name_from_atom

    ! Whether character C is one of the ASCII CHARACTERS.  Not a port.
    logical function is_one_of(c, characters)
        integer(int32), intent(in) :: c
        character(len=*), intent(in) :: characters

        is_one_of = .false.
        if (c < 0 .or. c > 127) return
        is_one_of = index(characters, achar(c)) > 0
    end function is_one_of

    ! Make a full name of the text the extraction buffer is narrowed to:
    ! drop comments, titles, trailing comments and the like, put a last
    ! name given first at the end, and compress whitespace, unless
    ! `mail_extr_disable_voodoo' says not to.  MAILBOX_BEGINNING and
    ! MAILBOX_END are the mailbox in CANONICALIZATION_BUFFER (for a name
    ! with &); COMMENT_BEGINNING and COMMENT_END are upstream's
    ! mailextr-comment_start and mailextr-cend; DISABLE_INITIAL_GUESSING is set
    ! when the name was made from the mailbox.  Port of
    ! mail-extr.el:mail-extr-voodoo.
    subroutine clean_up_name(mailbox_beginning, mailbox_end, canonicalization_buffer, comment_beginning, comment_end, &
                             disable_initial_guessing, status)
        integer(int64), intent(in) :: mailbox_beginning, mailbox_end
        type(handle_type), intent(in) :: canonicalization_buffer
        integer(int64), intent(inout) :: comment_beginning, comment_end
        logical, intent(inout) :: disable_initial_guessing
        type(status_type), intent(out) :: status
        type(excursion_type) :: excursion
        integer(int64) :: found
        logical :: saved_fold

        status = status_type()

        if (allocated(mail_extr_disable_voodoo%regular_expression)) then
            call goto_character(point_minimum())
            call regular_expression_search_forward(mail_extr_disable_voodoo%regular_expression, found, status, &
                                                   noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. found /= 0) return
        else if (mail_extr_disable_voodoo%value) then
            return
        end if

        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        call save_excursion_begin(excursion)
        call clean_up_words(mailbox_beginning, mailbox_end, canonicalization_buffer, comment_beginning, comment_end, &
                            disable_initial_guessing, status)
        call save_excursion_end(excursion)
        case_fold_search%value = saved_fold
    end subroutine clean_up_name

    ! The work of `clean_up_name' (the arguments are the same).  Port of
    ! mail-extr.el:mail-extr-voodoo.
    subroutine clean_up_words(mailbox_beginning, mailbox_end, canonicalization_buffer, comment_beginning, comment_end, &
                              disable_initial_guessing, status)
        integer(int64), intent(in) :: mailbox_beginning, mailbox_end
        type(handle_type), intent(in) :: canonicalization_buffer
        integer(int64), intent(inout) :: comment_beginning, comment_end
        logical, intent(inout) :: disable_initial_guessing
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: comment_start, moved, suffix, last_word_beginning, this_word_beginning, name_beginning, name_end, &
                          found, repeat, position
        integer(int32) :: next_code, initial
        integer :: word_count
        logical :: mixed_case, lower_case, last_name_comma, begin_again, drop_this_word_if_trailing, &
                   drop_last_word_if_trailing, word_found, name_done, moved_over, matched, saved

        word_count = 0
        mixed_case = .false.
        lower_case = .false.
        suffix = 0
        last_name_comma = .false.
        begin_again = .false.
        drop_this_word_if_trailing = .false.
        drop_last_word_if_trailing = .false.
        word_found = .false.
        name_done = .false.
        last_word_beginning = 0
        this_word_beginning = 0
        call set_syntax_table(address_text_syntax_table, status)
        if (.not. status%ok()) return
        ! Get rid of comments.
        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            moved = skip_characters_forward("^({[""'`", status)
            if (.not. status%ok()) return
            comment_start = point()
            call set_syntax_table(address_text_comment_syntax_table, status)
            if (.not. status%ok()) return
            next_code = following_character()

            if (next_code == iachar("'") .or. next_code == iachar("`")) then
                repeat = 1
                if (next_code == iachar("'")) repeat = 2
                call search_forward("'", found, status, noerror=SEARCH_MOVE, count=repeat)
            else
                call move_over_expression(1_int64, moved_over, status)
                if (status%ok() .and. .not. moved_over) call goto_character(point_maximum())
            end if

            if (.not. status%ok()) return
            call set_syntax_table(address_text_syntax_table, status)
            if (.not. status%ok()) return

            if (character_at(comment_start) == iachar("(")) then
                ! Delete the comment itself.
                call delete_region(comment_start, point(), status)
                if (.not. status%ok()) return
                ! Canonicalize whitespace where the comment was.
                moved = skip_characters_backward(" "//TAB, status)
                if (.not. status%ok()) return

                if (looking_at("\([ "//TAB//"]+$\|[ "//TAB//"]+,\)", status)) then
                    call replace_match("", status)
                else if (status%ok()) then
                    comment_start = point()
                    moved = skip_characters_forward(" "//TAB, status)

                    if (status%ok()) then
                        if (is_beginning_of_buffer()) then
                            call delete_region(point(), comment_start, status)
                        else
                            call just_one_space(1_int64, status)
                        end if
                    end if
                end if

                if (.not. status%ok()) return
            end if
        end do

        call search_forward(" ", found, status, noerror=SEARCH_NOERROR)
        if (.not. status%ok()) return

        if (found == 0) then
            call goto_character(point_minimum())
            call search_forward("_", found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return

            if (found /= 0) then
                ! Handle the use of underlines as spaces.
                ! Example: fml@foo.bar.dom (First_M._Last)
                call goto_character(point_minimum())

                do
                    call search_forward("_", found, status, noerror=SEARCH_NOERROR)
                    if (.not. status%ok()) return
                    if (found == 0) exit
                    call replace_match(" ", status, fixedcase=.true.)
                    if (.not. status%ok()) return
                end do
            else
                call search_forward(".", found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return

                if (found /= 0) then
                    ! Fix . used as space.
                    ! Example: danj1@cb.att.com (daniel.jacobson)
                    call goto_character(point_minimum())

                    do
                        call regular_expression_search_forward(BAD_DOT_PATTERN, found, status, noerror=SEARCH_NOERROR)
                        if (.not. status%ok()) return
                        if (found == 0) exit
                        call replace_match("\1 \2", status, fixedcase=.true.)
                        if (.not. status%ok()) return
                    end do
                end if
            end if
        end if

        ! Loop over the words (and other junk) in the name.
        call goto_character(point_minimum())

        do while (.not. name_done)
            if (word_found) then
                ! Last time through this loop we skipped over a word.
                last_word_beginning = this_word_beginning
                drop_last_word_if_trailing = drop_this_word_if_trailing
                word_found = .false.
            end if

            if (begin_again) then
                ! Last time through the loop we found something that
                ! indicates we should pretend we are beginning again from
                ! the start.
                word_count = 0
                last_word_beginning = 0
                drop_last_word_if_trailing = .false.
                mixed_case = .false.
                lower_case = .false.
                begin_again = .false.
            end if

            ! Initialize for this iteration of the loop.
            call skip_whitespace_forward(status)
            if (.not. status%ok()) return

            if (word_count == 0) then
                call narrow_to_region(point(), point_maximum(), status)
                if (.not. status%ok()) return
            end if

            this_word_beginning = point()
            drop_this_word_if_trailing = .false.
            next_code = following_character()
            ! Decide what to do based on what we are looking at.
            matched = .false.

            if (word_count == 0) then
                matched = looking_at(mail_extr_full_name_prefixes, status)
                if (.not. status%ok()) return
            end if

            if (matched) then
                ! Delete title.
                call goto_character(match_end(0, st))
                call narrow_to_region(point(), point_maximum(), status)
            else if (word_count >= 2 .and. is_looking_at(FULL_NAME_SUFFIX_PATTERN)) then
                ! Stop after name suffix.
                call add_suffix(status)
                word_found = .true.
                name_done = .true.
            else if (is_looking_at("MKA \(.+\)")) then
                ! Handle SCA names: "Mundanely Known As".
                call goto_character(match_beginning(1, st))
                call narrow_to_region(point(), point_maximum(), status)
                begin_again = .true.
            else if (next_code == iachar(",") .and. word_count == 1) then
                ! Check for initial last name followed by comma.
                call forward_character(status, 1_int64)
                last_name_comma = .true.
                if (status%ok() .and. following_character() /= iachar(" ")) call insert(" ", status)
            else if (is_one_of(next_code, "({[""'`")) then
                ! Delete parenthesized/quoted comment/nickname.
                call delete_comment(status)
            else if (is_looking_at(STUPID_VMS_DATE_STAMP_PATTERN)) then
                ! Handle VMS date stamps.
                call replace_match("", status, fixedcase=.true.)
            else if (is_looking_at(HZ_EMBEDDED_GB_ENCODED_CHINESE_PATTERN)) then
                ! Handle Chinese characters.
                call goto_character(match_end(0, st))
                word_found = .true.
            else if (word_count == 0 .and. is_looking_at(LEADING_GARBAGE)) then
                ! Skip initial garbage characters.
                call goto_character(match_end(0, st))
                call narrow_to_region(point(), point_maximum(), status)
            else if (is_stopping_point()) then
                name_done = .true.
            else if (is_looking_at(HAM_CALL_SIGN_PATTERN)) then
                ! Delete ham radio call signs.
                call delete_region(match_beginning(0, st), match_end(0, st), status)
            else if (is_looking_at(INITIAL_PATTERN)) then
                ! Fixup initials.
                if (next_code /= upcase(next_code)) lower_case = .true.
                call forward_character(status, 1_int64)

                if (status%ok()) then
                    if (following_character() == iachar(".")) then
                        call forward_character(status, 1_int64)
                    else
                        call insert(".", status)
                    end if
                end if

                if (status%ok() .and. following_character() /= iachar(" ")) call insert(" ", status)
                word_found = .true.
            else if (word_count == 0 .and. is_looking_at(LISTSERV_LIST_NAME_PATTERN)) then
                ! Handle BITNET LISTSERV list names.
                call narrow_to_region(match_beginning(1, st), match_end(1, st), status)
                word_found = .true.
                name_done = .true.
            else if (word_count > 0 .and. preceding_character() == iachar(" ") .and. next_code == iachar("&") .and. &
                     point() + 1 == point_maximum()) then
                ! Handle & substitution, when & is last and is not first.
                call delete_character(1_int64, status)
                position = point()
                if (status%ok()) call insert_buffer_substring(canonicalization_buffer, status, mailbox_beginning, mailbox_end)
                if (status%ok()) call capitalize_region(position, point(), status)
                disable_initial_guessing = .true.
                word_found = .true.
            else if (word_count > 0 .and. next_code == iachar("&")) then
                ! Handle & between names, as in "Bob & Susie".
                name_beginning = point()
                name_end = name_beginning + 1
                word_found = .true.
                call goto_character(name_end)
            else if (is_looking_at(NAME_PATTERN)) then
                ! Regular name words.
                name_beginning = point()
                name_end = match_end(0, st)

                ! Certain words will be dropped if they are at the end:
                ! trailing 4-or-more letter lowercase words preceded by
                ! mixed case or uppercase words, and a trailing word which
                ! is terminated with a period.
                if (word_count >= 2 .and. .not. lower_case) then
                    if (is_looking_at("[[:lower:]]\{4,\}[ "//TAB//"]*\'") .or. character_at(name_end - 1) == iachar(".")) then
                        drop_this_word_if_trailing = .true.
                    end if
                end if

                ! Set the flags that indicate whether we have seen a
                ! lowercase word, a mixed case word, and an uppercase word.
                call regular_expression_search_forward("[[:lower:]]", found, status, bound=name_end, noerror=SEARCH_NOERROR)

                if (status%ok() .and. found /= 0) then
                    call goto_character(name_beginning)
                    call regular_expression_search_forward("[[:upper:]]", found, status, bound=name_end, noerror=SEARCH_NOERROR)

                    if (found /= 0) then
                        mixed_case = .true.
                    else
                        lower_case = .true.
                    end if
                end if

                call goto_character(name_end)
                word_found = .true.
            else if (is_looking_at("[0-9]+\>")) then
                ! Allow a number as a word, if it doesn't mean anything else.
                name_beginning = point()
                name_end = match_end(0, st)
                call goto_character(name_end)
                word_found = .true.
            else
                name_done = .true.
            end if

            if (.not. status%ok()) return
            ! Count any word that we skipped over.
            if (word_found) word_count = word_count + 1
        end do

        ! If the last thing in the name is 2 or more periods, or one or more
        ! other sentence terminators (but not a single period) then keep them
        ! and the preceding word.  This is for the benefit of whole sentences
        ! in the name field: it's better behavior than dropping the last word
        ! of the sentence...
        if (suffix == 0) then
            if (is_looking_at("\(\.+\|[?!;:.][?!;:.]+\|[?!;:][?!;:.]*\)\'")) then
                suffix = point_maximum()
                call goto_character(suffix)
            end if
        end if

        if (.not. status%ok()) return
        ! Drop everything after point and certain trailing words.
        position = point()
        if (drop_last_word_if_trailing .and. last_word_beginning /= 0) position = last_word_beginning
        call narrow_to_region(point_minimum(), position, status)
        if (.not. status%ok()) return

        ! We simply refuse to believe that any last name is PARC or ADOC.
        ! If it looks like that is the last name, that there is no meaningful
        ! here at all.
        if (suffix == 0) then
            call goto_character(point_minimum())
            saved = case_fold_search%value
            case_fold_search%value = .true.
            matched = looking_at("[-A-Za-z_]+[. ]\(PARC\|ADOC\)\'", status)
            case_fold_search%value = saved
            if (.not. status%ok()) return

            if (matched) then
                call erase_buffer(status)
                if (.not. status%ok()) return
            end if
        end if

        ! If last name first put it at end (but before suffix).
        if (last_name_comma) then
            call goto_character(point_minimum())
            call search_forward(",", found, status)
            if (.not. status%ok()) return
            name_end = point() - 1

            if (suffix /= 0) then
                call goto_character(suffix)
            else
                call goto_character(point_maximum())
            end if

            if (preceding_character() /= iachar(" ")) call insert(" ", status)
            if (status%ok()) call insert_buffer_substring(current_buffer(), status, point_minimum(), name_end)
            if (.not. status%ok()) return
            call goto_character(name_end)
            moved = skip_characters_forward(TAB//" ,", status)
            if (.not. status%ok()) return
            call narrow_to_region(point(), point_maximum(), status)
            if (.not. status%ok()) return
        end if

        ! Compress whitespace.
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("[ "//TAB//LF//"]+", found, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit

            if (is_end_of_buffer()) then
                call replace_match("", status, fixedcase=.true.)
            else
                call replace_match(" ", status, fixedcase=.true.)
            end if

            if (.not. status%ok()) return
        end do

    contains

        ! Whether the text after point matches REGULAR_EXPRESSION, a valid regular
        ! expression of this module.
        logical function is_looking_at(regular_expression)
            character(len=*), intent(in) :: regular_expression
            type(status_type) :: st

            is_looking_at = looking_at(regular_expression, st)
            if (.not. st%ok()) is_looking_at = .false.
        end function is_looking_at

        ! Whether point is at one of the places where the name stops:
        ! before ALL CAPS acronyms preceded by mixed-case words (XT-DEM),
        ! a trailing alternative address, a trailing comment not
        ! introduced by comma, or a telephone number.
        logical function is_stopping_point()
            is_stopping_point = .false.

            if (word_count >= 2 .and. mixed_case) then
                if (is_looking_at(WEIRD_ACRONYM_PATTERN)) is_stopping_point = .not. is_looking_at(ROMAN_NUMERAL_PATTERN)
            end if

            if (.not. is_stopping_point) is_stopping_point = is_looking_at(ALTERNATIVE_ADDRESS_PATTERN)
            if (.not. is_stopping_point) is_stopping_point = is_looking_at(TRAILING_COMMENT_START_PATTERN)
            if (.not. is_stopping_point .and. word_count >= 1) is_stopping_point = is_looking_at(TELEPHONE_EXTENSION_PATTERN)
        end function is_stopping_point

        ! Put a comma and a space before the generational suffix at
        ! point, and capitalize or upcase it.
        subroutine add_suffix(status)
            type(status_type), intent(out) :: status

            call skip_whitespace_backward(status)
            if (.not. status%ok()) return
            suffix = point()

            if (following_character() == iachar(",")) then
                call forward_character(status, 1_int64)
            else
                call insert(",", status)
            end if

            if (.not. status%ok()) return

            ! Enforce at least one space after comma.
            if (following_character() /= iachar(" ")) then
                call insert(" ", status)
                if (.not. status%ok()) return
            end if

            call skip_whitespace_forward(status)
            if (.not. status%ok()) return

            if (is_one_of(following_character(), "jJsS")) then
                call capitalize_word(1_int64, status)
                if (.not. status%ok()) return

                if (following_character() == iachar(".")) then
                    call forward_character(status, 1_int64)
                else
                    call insert(".", status)
                end if
            else
                call upcase_word(1_int64, status)
            end if
        end subroutine add_suffix

        ! Delete the parenthesized or quoted comment or nickname at
        ! point, or make the whole name of a quoted one; a quoted
        ! initial stays.
        subroutine delete_comment(status)
            type(status_type), intent(out) :: status
            logical :: at_end

            comment_beginning = point()
            call set_syntax_table(address_text_comment_syntax_table, status)
            if (.not. status%ok()) return

            if (next_code == iachar("'") .or. next_code == iachar("`")) then
                repeat = 1
                if (next_code == iachar("'")) repeat = 2
                call search_forward("'", found, status, noerror=SEARCH_NOERROR, count=repeat)
                if (status%ok() .and. found == 0) call delete_character(1_int64, status)
            else
                call move_over_expression(1_int64, moved_over, status)
                if (status%ok() .and. .not. moved_over) call goto_character(point_maximum())
            end if

            if (.not. status%ok()) return
            call set_syntax_table(address_text_syntax_table, status)
            if (.not. status%ok()) return
            comment_end = point()
            at_end = is_looking_at(" *\'")

            if (word_count == 0 .and. at_end .and. comment_end - comment_beginning >= 2) then
                ! Handle case of entire name being quoted.
                call narrow_to_region(comment_beginning + 1, comment_end - 1, status)
                if (.not. status%ok()) return
                call goto_character(point_minimum())
            else
                initial = -1

                ! Handle case of quoted initial.
                if (comment_end - comment_beginning == 3 .and. .not. at_end) then
                    initial = character_at(comment_beginning + 1)
                else if (comment_end - comment_beginning == 4 .and. .not. at_end) then
                    if (character_at(comment_beginning + 2) == iachar(".")) initial = character_at(comment_beginning + 1)
                end if

                call delete_region(comment_beginning, comment_end, status)
                if (.not. status%ok()) return

                if (initial >= 0) then
                    call insert_character(initial, status)
                    if (status%ok()) call insert(". ", status)
                end if
            end if
        end subroutine delete_comment
    end subroutine clean_up_words

    ! ------------------------------------------------------------------
    ! Domains
    ! ------------------------------------------------------------------

    !> Convert mail domain DOMAIN to the country it corresponds to.
    !> TEXT, if present, gets the message shown ("CU: Cuba").  Port of
    !> mail-extr.el:what-domain.
    subroutine what_domain(domain, status, text)
        character(len=*), intent(in) :: domain
        type(status_type), intent(out) :: status
        character(len=:), allocatable, intent(out), optional :: text
        character(len=:), allocatable :: code, shown
        integer :: k

        status = status_type()
        code = downcase_string(domain)
        k = 0
        if (len(code) > 0 .and. len(code) <= len(TOP_LEVEL_DOMAINS(1)%code)) k = findloc(TOP_LEVEL_DOMAINS%code, code, 1)

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No such domain")
            return
        end if

        shown = upcase_ascii(trim(TOP_LEVEL_DOMAINS(k)%code))//": "//trim(TOP_LEVEL_DOMAINS(k)%name)
        call message(shown)
        if (present(text)) text = shown

    contains

        ! STRING with its ASCII letters upcased (a domain code is ASCII).
        pure function upcase_ascii(string) result(out)
            character(len=*), intent(in) :: string
            character(len=len(string)) :: out
            integer :: j

            out = string

            do j = 1, len(out)
                if (out(j:j) >= "a" .and. out(j:j) <= "z") out(j:j) = achar(iachar(out(j:j)) - 32)
            end do
        end function upcase_ascii
    end subroutine what_domain

    ! Read a domain with completion, ignoring case, and show its country.
    ! Port of the argument reading of mail-extr.el:what-domain.
    subroutine execute_what_domain(status)
        type(status_type), intent(out) :: status
        type(value_type) :: names(size(TOP_LEVEL_DOMAINS))
        type(completion_table_type) :: table
        character(len=:), allocatable :: domain
        logical :: saved_ignore_case
        integer :: k

        do k = 1, size(TOP_LEVEL_DOMAINS)
            names(k) = value_type(text=trim(TOP_LEVEL_DOMAINS(k)%code))
        end do

        table = completion_list_table(make_list(names))
        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read("Domain: ", table, domain, status, require_match=REQUIRE_MATCH_YES)
        completion_ignore_case%value = saved_ignore_case
        if (.not. status%ok()) return
        call what_domain(domain, status)
    end subroutine execute_what_domain

    ! ------------------------------------------------------------------
    ! The setting mail_extr_disable_voodoo
    ! ------------------------------------------------------------------

    !> Forget the regexp of `mail_extr_disable_voodoo', as any assignment
    !> to it does.  Not a port.
    subroutine mail_extr_disable_voodoo_clear_kinds(self)
        class(mail_extr_disable_voodoo_choice_setting_type), intent(inout) :: self

        if (allocated(self%regular_expression)) deallocate (self%regular_expression)
    end subroutine mail_extr_disable_voodoo_clear_kinds

    !> The regexp of `mail_extr_disable_voodoo' as help shows it
    !> (`%regexp = "..."'); "" when none is set.  Not a port.
    function mail_extr_disable_voodoo_other_value(self) result(text)
        class(mail_extr_disable_voodoo_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (allocated(self%regular_expression)) text = "%regexp = """//self%regular_expression//""""
    end function mail_extr_disable_voodoo_other_value

    !> The assignment that sets the regexp `mail_extr_disable_voodoo'
    !> takes, for the error of a rejected value.  Not a port.
    function mail_extr_disable_voodoo_kind_hint(self, number) result(text)
        class(mail_extr_disable_voodoo_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a regular expression is `"//trim(self%name)//"%regexp = ""...""'"
        if (len(number) < 0) text = number
    end function mail_extr_disable_voodoo_kind_hint
end module mail_extract_mod
