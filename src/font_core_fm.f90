! font_core_fm.f90 -- Font Lock mode and Global Font Lock mode.
! Ported from GNU Emacs 31.1 lisp/font-core.el.
!
! Copyright (C) 1992-2026 Free Software Foundation, Inc.
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

!> Font Lock mode, the buffer-local minor mode that fontifies text, and
!> Global Font Lock mode, which turns it on in every buffer whose major
!> mode supports it.  A major mode supports Font Lock by giving the
!> buffer its `font_lock_defaults': the keywords to highlight (by
!> decoration level), whether to skip syntactic fontification, whether
!> keywords ignore case, and changes to the syntax table:
!>
!>     call font_lock_defaults%set_local(my_keywords)
!>
!> The work of turning Font Lock on and off in a buffer is done by
!> `font_lock_function' (`font_lock_default_function', which hands it to
!> font_lock_fm's `font_lock_mode_internal' when the buffer has
!> defaults).
!>
!> The keyword types are here because `font_lock_defaults' holds them; the
!> fontification is in font_lock_fm.
!>
!> Deferred: the OTHER-VARS of `font_lock_defaults' (variables a major mode
!> sets for Font Lock), keywords given as a function or a variable to
!> evaluate, `define-globalized-minor-mode' in general (Global Font Lock
!> mode is written out here as the macro expands it).
module font_core_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, list, list_elements, make_list, vtag, V_ARRAY
    use data_fm, only: symbol_value, setq_local, set, set_default, boundp, append_locals_clearer, value_eq
    use buffer_fm, only: buffers, current, buffer_list, resolve_buffer, set_buffer_internal, is_buffer_modified, &
                         restore_buffer_modified, current_buffer
    use handle_fm, only: handle_type
    use eval_fm, only: hook_type, change_major_mode_hook
    use subr_fm, only: after_change_major_mode_hook, after_hook_procedure
    use derived_fm, only: is_derived_mode
    use easy_mmode_fm, only: minor_mode_type
    use modes_fm, only: minor_modes
    use commands_fm, only: commands
    use textprop_fm, only: remove_list_of_text_properties
    use editfns_fm, only: widen, narrow_to_region, point_min, point_max
    use syntax_fm, only: parse_state_type
    implicit none(type, external)
    private

    public :: font_lock_keywords_function_procedure, font_lock_syntactic_face_procedure
    public :: syms_of_font_core, font_lock_mode, global_font_lock_mode, turn_on_font_lock, &
              turn_on_font_lock_if_desired, font_lock_change_mode, font_lock_defontify, font_lock_default_function, &
              font_lock_keywords_level, font_lock_mode_enabled, font_lock_face_procedure, font_lock_matcher_procedure, &
              font_lock_form_procedure, font_lock_function_procedure, major_mode_of_current, font_lock_properties_procedure

    !> Overrides of a highlight (OVERRIDE in `font_lock_keywords'): keep
    !> an existing face (FONT_LOCK_OVERRIDE_NO), override it (FONT_LOCK_OVERRIDE_YES), keep it and add only where
    !> there is none (`keep'), merge before it (`prepend') or after it
    !> (`append').
    integer, parameter, public :: FONT_LOCK_OVERRIDE_NO = 0, FONT_LOCK_OVERRIDE_YES = 1, FONT_LOCK_OVERRIDE_KEEP = 2, &
                                  FONT_LOCK_OVERRIDE_PREPEND = 3, FONT_LOCK_OVERRIDE_APPEND = 4

    abstract interface
        !> A keyword's MATCHER given as a function: search forward from
        !> point for something to highlight, not beyond LIMIT; on success
        !> set the match data and move point past the match (and FOUND).
        !> Not a port.
        subroutine font_lock_matcher_procedure(limit, found, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: limit
            logical, intent(out) :: found
            type(status_type), intent(out) :: status
        end subroutine font_lock_matcher_procedure

        !> A highlight's FACE given as a function of the match: FACE gets
        !> the name of the face to give it, or "" for none.  (A subroutine:
        !> gfortran mishandles procedure pointer components whose function
        !> result is allocatable.)  Not a port.
        subroutine font_lock_face_procedure(face)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: face
        end subroutine font_lock_face_procedure

        !> An anchored highlighter's PRE-MATCH-FORM or POST-MATCH-FORM:
        !> run before or after its matcher's search; LIMIT, if positive, is
        !> where the search ends (PRE-MATCH-FORM's value).  Not a port.
        subroutine font_lock_form_procedure(limit)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(out) :: limit
        end subroutine font_lock_form_procedure

        !> `font_lock_function': turn Font Lock on or off (MODE) in the
        !> current buffer.  Not a port.
        subroutine font_lock_function_procedure(mode, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(in) :: mode
            type(status_type), intent(out) :: status
        end subroutine font_lock_function_procedure

        !> The text properties of a FACENAME of the form (face FACE PROP
        !> VAL ...) that depend on the match: give the text from START to
        !> END its properties (the match data is that of the match).
        !> Not a port.
        subroutine font_lock_properties_procedure(start, end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
        end subroutine font_lock_properties_procedure
    end interface

    !> A MATCH-HIGHLIGHT of `font_lock_keywords': the face to give
    !> subexpression SUBEXP of the match (`face', or the face
    !> `face_function' names; neither for no face), how it combines with
    !> existing fontification (OVERRIDE, a FONT_LOCK_OVERRIDE_*), and
    !> whether a SUBEXP that did not match is fine (LAXMATCH) rather than an
    !> error.
    !> The face may come with text properties to give the text as well
    !> (a FACENAME of the form (face FACE PROP VAL ...)): the names in
    !> `true_properties' are set to true, those in `nil_properties' to
    !> .false., `display' is the `display' property, and
    !> `properties_function' gives the ones that depend on the match.
    type, public :: font_lock_match_highlight_type
        integer :: subexp = 0
        character(len=:), allocatable :: face
        procedure(font_lock_face_procedure), pointer, nopass :: face_function => null()
        integer :: override = FONT_LOCK_OVERRIDE_NO
        logical :: laxmatch = .false.
        character(len=64), allocatable :: true_properties(:)
        character(len=64), allocatable :: nil_properties(:)
        character(len=:), allocatable :: display
        procedure(font_lock_properties_procedure), pointer, nopass :: properties_function => null()
    end type font_lock_match_highlight_type

    !> A HIGHLIGHT of a keyword: a MATCH-HIGHLIGHT (`match'), or with
    !> `anchored' a MATCH-ANCHORED (ANCHORED-MATCHER PRE-MATCH-FORM
    !> POST-MATCH-FORM SUBEXP-HIGHLIGHTERS...): a regexp (`regexp') or
    !> function (`matcher') searched for after each match of the keyword's
    !> MATCHER, from there to the end of the line or to where
    !> `pre_match_form' says, each match highlighted as `highlights' say.
    type, public :: font_lock_highlight_type
        logical :: anchored = .false.
        type(font_lock_match_highlight_type) :: match
        character(len=:), allocatable :: regexp
        procedure(font_lock_matcher_procedure), pointer, nopass :: matcher => null()
        procedure(font_lock_form_procedure), pointer, nopass :: pre_match_form => null()
        procedure(font_lock_form_procedure), pointer, nopass :: post_match_form => null()
        type(font_lock_match_highlight_type), allocatable :: highlights(:)
    end type font_lock_highlight_type

    !> One element of `font_lock_keywords': a MATCHER (a regexp, or a
    !> function) and the HIGHLIGHTs to apply to each match, in order.
    type, public :: font_lock_keyword_type
        character(len=:), allocatable :: regexp
        procedure(font_lock_matcher_procedure), pointer, nopass :: matcher => null()
        type(font_lock_highlight_type), allocatable :: highlights(:)
    end type font_lock_keyword_type

    abstract interface
        !> A function giving the keywords of a decoration level (a
        !> function in KEYWORDS of `font_lock_defaults', which
        !> font-lock-eval-keywords calls when the keywords are set up).
        !> Not a port.
        subroutine font_lock_keywords_function_procedure(keywords)
            import :: font_lock_keyword_type
            implicit none(type, external)
            type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        end subroutine font_lock_keywords_function_procedure

        !> `font_lock_syntactic_face_function': FACE gets the name of the
        !> face to give the string or comment whose parse state (at its
        !> start) is STATE ("" for none).  Not a port.
        subroutine font_lock_syntactic_face_procedure(state, face)
            import :: parse_state_type
            implicit none(type, external)
            type(parse_state_type), intent(in) :: state
            character(len=:), allocatable, intent(out) :: face
        end subroutine font_lock_syntactic_face_procedure
    end interface

    !> The keywords of one decoration level (one keyword list), or the
    !> function computing them when Font Lock sets up (`function').
    type, public :: font_lock_keywords_type
        type(font_lock_keyword_type), allocatable :: keywords(:)
        procedure(font_lock_keywords_function_procedure), pointer, nopass :: function => null()
    end type font_lock_keywords_type

    !> One entry of SYNTAX-ALIST in `font_lock_defaults': the syntax
    !> descriptor (as `modify_syntax_entry' takes it) that the characters
    !> CHARS have for fontification.
    type, public :: font_lock_syntax_entry_type
        character(len=:), allocatable :: chars
        character(len=:), allocatable :: syntax
    end type font_lock_syntax_entry_type

    !> What a major mode tells Font Lock (the value of `font_lock_defaults'
    !> in a buffer):
    !>
    !> KEYWORDS (`levels') are the keyword lists, the first for the least
    !> decoration and the last for the most; `font_lock_maximum_decoration'
    !> chooses one.
    !>
    !> If KEYWORDS-ONLY is true, syntactic fontification (strings and
    !> comments) is not performed.
    !>
    !> If CASE-FOLD is true, the case of the keywords is ignored when
    !> fontifying.
    !>
    !> SYNTAX-ALIST gives characters a syntax for fontification only, each
    !> entry as for `modify_syntax_entry'.
    type, public :: font_lock_defaults_type
        type(font_lock_keywords_type), allocatable :: levels(:)
        logical :: keywords_only = .false.
        logical :: case_fold = .false.
        type(font_lock_syntax_entry_type), allocatable :: syntax_modifications(:)
        !> OTHER-VARS: the buffer's `font_lock_syntactic_face_function'
        !> (null leaves it alone).
        procedure(font_lock_syntactic_face_procedure), pointer, nopass :: syntactic_face_function => null()
    end type font_lock_defaults_type

    !> One buffer's `font_lock_defaults' (and whether it has one).
    type :: defaults_entry_type
        integer(int64) :: serial = 0
        logical :: set = .false.
        type(font_lock_defaults_type) :: value
    end type defaults_entry_type

    !> Defaults for Font Lock mode specified by the major mode.
    !> Defaults should be of the form:
    !>
    !>  (KEYWORDS [KEYWORDS-ONLY [CASE-FOLD [SYNTAX-ALIST ...]]])
    !>
    !> KEYWORDS may be a name (a variable or function whose value is the
    !> keywords to use for fontification) or a list of names (specifying
    !> different levels of fontification).
    !>
    !> If KEYWORDS-ONLY is true, syntactic fontification (strings and
    !> comments) is not performed.
    !>
    !> If CASE-FOLD is true, the case of the keywords is ignored when
    !> fontifying.
    !>
    !> If SYNTAX-ALIST is true, it should be an array of entries of two values, of the
    !> form CHAR-OR-STRING and STRING used to set the local Font Lock
    !> syntax table, for keyword and syntactic fontification (see
    !> `modify_syntax_entry').
    !>
    !> These item elements are used by Font Lock mode to set the variables
    !> `font_lock_keywords', `font_lock_keywords_only',
    !> `font_lock_keywords_case_fold_search' and `font_lock_syntax_table'.
    !>
    !> In Fortran the value is a `font_lock_defaults_type', given to the
    !> current buffer with `font_lock_defaults%set_local'.  Port of
    !> font-core.el:font-lock-defaults.
    type, public :: font_lock_defaults_setting_type
        character(len=48) :: name = "font_lock_defaults"
        type(defaults_entry_type), allocatable :: entries(:)
    contains
        !> Give the current buffer these defaults (set_local): its
        !> KEYWORDS (or the keyword lists of its decoration LEVELS),
        !> KEYWORDS_ONLY, CASE_FOLD and SYNTAX_MODIFICATIONS.
        procedure :: set_local => defaults_set_local
        !> Whether the current buffer has defaults (font-lock-defaults is
        !> set).
        procedure :: present_p => has_defaults
        !> The current buffer's defaults (call only when present_p).
        procedure :: current => current_defaults
        !> Forget the current buffer's defaults.
        procedure :: clear_local => defaults_clear_local
    end type font_lock_defaults_setting_type

    !> The current buffer's Font Lock defaults (see the type).  Port of
    !> font-core.el:font-lock-defaults.
    type(font_lock_defaults_setting_type), public :: font_lock_defaults

    !> A function which is called when `font_lock_mode' is toggled.
    !> It will be passed one argument, which is the current value of
    !> `font_lock_mode'.  Port of font-core.el:font-lock-function.
    procedure(font_lock_function_procedure), pointer, public :: font_lock_function => null()

    !> `font_lock_mode_internal' of font_lock_fm (installed by it; font-lock.el
    !> is loaded when Font Lock first needs it).
    procedure(font_lock_function_procedure), pointer, public :: font_lock_mode_internal_hook => null()

    !> Whether the current buffer is ready for fontification
    !> (font-lock-specified-p of font_lock_fm, installed by it).
    procedure(specified_procedure), pointer, public :: font_lock_specified_test => null()

    !> The first fontification after Font Lock mode is turned on (its
    !> :after-hook, font-lock-initial-fontify of font_lock_fm, installed by
    !> it).
    procedure(after_hook_procedure), pointer, public :: font_lock_initial_fontify_hook => null()

    abstract interface
        ! Not a port.
        logical function specified_procedure(mode)
            implicit none(type, external)
            logical, intent(in) :: mode
        end function specified_procedure
    end interface

    !> Modes for which Font Lock mode is automagically turned on.
    !> Global Font Lock mode is controlled by the command
    !> `global_font_lock_mode'.  If `all' (the default), all modes that
    !> support Font Lock mode have it automatically turned on; otherwise
    !> only the major modes in `modes' (or with `negate', all but those).
    !> For example, modes = [character(len=64) :: "c_mode", "c++_mode"]
    !> means that Font Lock mode is turned on for buffers in C and C++
    !> modes only.  Port of font-core.el:font-lock-global-modes.
    type, public :: font_lock_global_modes_type
        logical :: all = .true.
        logical :: negate = .false.
        character(len=64), allocatable :: modes(:)
    end type font_lock_global_modes_type

    !> Modes for which Font Lock mode is automagically turned on (see the
    !> type).  Port of font-core.el:font-lock-global-modes.
    type(font_lock_global_modes_type), public :: font_lock_global_modes

    !> Hook run after entering or leaving `font_lock_mode'.  Port of
    !> font-core.el:font-lock-mode-hook.
    type(hook_type), target, public :: font_lock_mode_hook = hook_type(name="font_lock_mode_hook")

    !> Hook run after entering or leaving `global_font_lock_mode'.
    !> Port of font-core.el:global-font-lock-mode-hook.
    type(hook_type), target, public :: global_font_lock_mode_hook = hook_type(name="global_font_lock_mode_hook")

    !> The major mode Global Font Lock mode last turned Font Lock on for
    !> (the buffer-local `font_lock_mode_major_mode' of
    !> define-globalized-minor-mode), and whether the user set Font Lock
    !> mode explicitly (`font_lock_mode__set_explicitly'), by buffer.
    type :: global_entry_type
        integer(int64) :: serial = 0
        logical :: set_explicitly = .false.
    end type global_entry_type

    type(global_entry_type), allocatable :: global_entries(:)

    !> `font_lock_mode__suppress_set_explicitly'.
    logical :: suppress_set_explicitly = .false.

contains

    !> Define Font Lock mode and Global Font Lock mode (as loading
    !> font-core.el does).  Not a port.
    subroutine syms_of_font_core()
        type(status_type) :: st
        logical :: bound

        call minor_modes%font_lock_mode%define("font_lock_mode", body=font_lock_mode_body, hook=font_lock_mode_hook, &
                                               after_hook=font_lock_mode_after_hook)
        commands%font_lock_mode%execute => font_lock_mode
        call minor_modes%global_font_lock_mode%define("global_font_lock_mode", global=.true., &
                                                      body=global_font_lock_mode_body, hook=global_font_lock_mode_hook)
        commands%global_font_lock_mode%execute => global_font_lock_mode
        commands%turn_on_font_lock%execute => turn_on_font_lock
        commands%turn_on_font_lock%is_command = .false.
        commands%turn_on_font_lock_if_desired%execute => turn_on_font_lock_if_desired
        commands%turn_on_font_lock_if_desired%is_command = .false.
        commands%font_lock_change_mode%execute => font_lock_change_mode
        commands%font_lock_change_mode%is_command = .false.
        commands%font_lock_defontify%execute => font_lock_defontify
        commands%font_lock_defontify%is_command = .false.
        commands%global_font_lock_mode_enable_in_buffer%execute => global_font_lock_mode_enable_in_buffer
        commands%global_font_lock_mode_enable_in_buffer%is_command = .false.
        commands%font_lock_mode_set_explicitly%execute => font_lock_mode_set_explicitly
        commands%font_lock_mode_set_explicitly%is_command = .false.
        if (.not. associated(font_lock_function)) font_lock_function => font_lock_default_function
        ! textprop.c's char-property-alias-alist (nil by default).
        call boundp(value_type(name="char_property_aliases"), bound, st)
        if (.not. bound) call set_default(value_type(name="char_property_aliases"), value_type(), st)
        call append_locals_clearer(clear_font_core_locals)
        ! (add-hook 'font-lock-mode-hook #'font-lock-mode-set-explicitly)
        call font_lock_mode_hook%append(font_lock_mode_set_explicitly)
    end subroutine syms_of_font_core

    ! ------------------------------------------------------------------
    ! font-lock-defaults
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry, made if MAKE (0 if none).
    !> Not a port.
    integer function defaults_index(self, make) result(k)
        class(font_lock_defaults_setting_type), intent(inout) :: self
        logical, intent(in) :: make
        type(defaults_entry_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(current)%serial) then
                k = i
                return
            end if
        end do

        if (.not. make) return

        ! Reuse the entry of a dead buffer.
        do i = 1, size(self%entries)
            if (.not. self%entries(i)%set) then
                self%entries(i)%serial = buffers(current)%serial
                k = i
                return
            end if
        end do

        n = size(self%entries)
        allocate (grown(n + 1))
        grown(1:n) = self%entries
        grown(n + 1)%serial = buffers(current)%serial
        call move_alloc(grown, self%entries)
        k = n + 1
    end function defaults_index

    ! Not a port.
    subroutine defaults_set_local(self, keywords, levels, keywords_only, case_fold, syntax_modifications, syntactic_face_function)
        class(font_lock_defaults_setting_type), intent(inout) :: self
        !> The keywords, for a mode with one decoration level.
        type(font_lock_keyword_type), intent(in), optional :: keywords(:)
        !> The keyword lists of the decoration levels, least first.
        type(font_lock_keywords_type), intent(in), optional :: levels(:)
        logical, intent(in), optional :: keywords_only, case_fold
        type(font_lock_syntax_entry_type), intent(in), optional :: syntax_modifications(:)
        !> OTHER-VARS: the buffer's `font_lock_syntactic_face_function'.
        procedure(font_lock_syntactic_face_procedure), optional :: syntactic_face_function
        integer :: k

        k = defaults_index(self, .true.)
        if (k == 0) return
        self%entries(k)%set = .true.
        self%entries(k)%value = font_lock_defaults_type()

        if (present(levels)) then
            allocate (self%entries(k)%value%levels(size(levels)))
            self%entries(k)%value%levels(:) = levels(:)
        else
            allocate (self%entries(k)%value%levels(1))

            if (present(keywords)) then
                allocate (self%entries(k)%value%levels(1)%keywords(size(keywords)))
                self%entries(k)%value%levels(1)%keywords(:) = keywords(:)
            else
                allocate (self%entries(k)%value%levels(1)%keywords(0))
            end if
        end if

        if (present(keywords_only)) self%entries(k)%value%keywords_only = keywords_only
        if (present(case_fold)) self%entries(k)%value%case_fold = case_fold

        if (present(syntax_modifications)) then
            self%entries(k)%value%syntax_modifications = syntax_modifications
        else
            allocate (self%entries(k)%value%syntax_modifications(0))
        end if

        if (present(syntactic_face_function)) self%entries(k)%value%syntactic_face_function => syntactic_face_function
    end subroutine defaults_set_local

    ! Not a port.
    logical function has_defaults(self) result(p)
        class(font_lock_defaults_setting_type), intent(inout) :: self
        integer :: k

        k = defaults_index(self, .false.)
        p = .false.
        if (k > 0) p = self%entries(k)%set
    end function has_defaults

    ! Not a port.
    function current_defaults(self) result(v)
        class(font_lock_defaults_setting_type), intent(inout) :: self
        type(font_lock_defaults_type) :: v
        integer :: k

        k = defaults_index(self, .false.)
        if (k == 0) return
        if (self%entries(k)%set) v = self%entries(k)%value
    end function current_defaults

    ! Not a port.
    subroutine defaults_clear_local(self)
        class(font_lock_defaults_setting_type), intent(inout) :: self
        integer :: k

        k = defaults_index(self, .false.)
        if (k == 0) return
        self%entries(k)%set = .false.
        self%entries(k)%serial = 0
        self%entries(k)%value = font_lock_defaults_type()
    end subroutine defaults_clear_local

    !> A keyword list as one decoration level of `font_lock_defaults'.
    !> Not a port.
    function font_lock_keywords_level(keywords) result(level)
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        type(font_lock_keywords_type) :: level

        allocate (level%keywords(size(keywords)))
        level%keywords(:) = keywords(:)
    end function font_lock_keywords_level

    !> kill-all-local-variables: the buffer-local variables of font-core.el
    !> kept here.  Not a port.
    subroutine clear_font_core_locals()
        integer :: k

        call font_lock_defaults%clear_local()
        k = global_index(.false.)
        if (k > 0) global_entries(k)%set_explicitly = .false.
    end subroutine clear_font_core_locals

    ! ------------------------------------------------------------------
    ! Font Lock mode
    ! ------------------------------------------------------------------

    !> Whether Font Lock mode is on in the current buffer (the variable
    !> `font_lock_mode').  Not a port.
    logical function font_lock_mode_enabled() result(on)
        on = minor_modes%font_lock_mode%enabled()
    end function font_lock_mode_enabled

    !> Toggle syntax highlighting in this buffer (Font Lock mode).
    !>
    !> When Font Lock mode is enabled, text is fontified as you type it:
    !>
    !>  - Comments are displayed in `font_lock_comment_face';
    !>  - Strings are displayed in `font_lock_string_face';
    !>  - Certain other expressions are displayed in other faces
    !>    according to the value of the variable `font_lock_keywords'.
    !>
    !> To customize the faces (colors, fonts, etc.) used by Font Lock for
    !> fontifying different parts of buffer text, use \[customize-face].
    !>
    !> You can enable Font Lock mode in any major mode automatically by
    !> turning on in the major mode's hook.  For example, put in your
    !> init.f90:
    !>
    !>  call c_mode_hook%append(turn_on_font_lock)
    !>
    !> Alternatively, you can use Global Font Lock mode to automagically
    !> turn on Font Lock mode in buffers whose major mode supports it
    !> and whose major mode is one of `font_lock_global_modes'.
    !>
    !> Where major modes support different levels of fontification, you
    !> can use the variable `font_lock_maximum_decoration' to specify
    !> which level you generally prefer.  When you turn Font Lock mode
    !> on/off the buffer is fontified/defontified.
    !>
    !> To add your own highlighting for some major mode, and modify the
    !> highlighting selected automatically via the variable
    !> `font_lock_maximum_decoration', you can use
    !> `font_lock_add_keywords'.
    !>
    !> To fontify a buffer, without turning on Font Lock mode and
    !> regardless of buffer size, you can use \[font_lock_fontify_buffer].
    !>
    !> You can set your own default settings for some mode, by setting a
    !> buffer local value for `font_lock_defaults', via its mode hook.
    !>
    !> The above is the default behavior of `font_lock_mode'; you may
    !> specify your own function which is called when `font_lock_mode'
    !> is toggled via `font_lock_function'.  Port of
    !> font-core.el:font-lock-mode.
    subroutine font_lock_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%font_lock_mode%command(status)
    end subroutine font_lock_mode

    !> The body of Font Lock mode, with its new state ON.
    !> Port of font-core.el:font-lock-mode in part.
    subroutine font_lock_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        logical :: mode

        status = status_type()
        mode = on

        ! Don't turn on Font Lock mode if the buffer is invisible (the
        ! name starts with a space).
        if (current > 0) then
            if (len(buffers(current)%name) > 0) then
                if (buffers(current)%name(1:1) == " ") then
                    call minor_modes%font_lock_mode%set_state(.false.)
                    mode = .false.
                end if
            end if
        end if

        call font_lock_function(mode, status)
        if (.not. status%ok()) return

        ! Arrange to unfontify this buffer if we change major mode later.
        if (mode) then
            call change_major_mode_hook%append(font_lock_change_mode, local=.true.)
        else
            call change_major_mode_hook%remove(font_lock_change_mode, local=.true.)
        end if
    end subroutine font_lock_mode_body

    !> Font Lock mode's :after-hook (font-lock-initial-fontify).
    !> Not a port.
    subroutine font_lock_mode_after_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (associated(font_lock_initial_fontify_hook)) call font_lock_initial_fontify_hook(status)
    end subroutine font_lock_mode_after_hook

    !> Get rid of fontification for the old major mode (run when the
    !> major mode changes).  Port of font-core.el:font-lock-change-mode.
    subroutine font_lock_change_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%font_lock_mode%disable(status)
    end subroutine font_lock_change_mode

    !> Clear out all `font_lock_face' properties in current buffer.
    !> A major mode that uses `font_lock_face' properties might want to put
    !> this function onto `change_major_mode_hook'.  Port of
    !> font-core.el:font-lock-defontify.
    subroutine font_lock_defontify(status)
        type(status_type), intent(out) :: status
        integer(int64) :: begv, zv
        logical :: modp, changed
        type(value_type) :: inhibit

        status = status_type()
        if (current == 0) return
        call is_buffer_modified(current_buffer(), modp, status)
        if (.not. status%ok()) return
        call symbol_value(value_type(name="inhibit_read_only"), inhibit, status)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), status)
        begv = buffers(current)%begv
        zv = buffers(current)%zv
        call widen()
        call remove_list_of_text_properties(point_min(), point_max(), list(value_type(name="font_lock_face")), changed, status)
        call narrow_to_region(begv, zv, status)
        call set(value_type(name="inhibit_read_only"), inhibit, status)
        call restore_buffer_modified(modp)
    end subroutine font_lock_defontify

    !> The default `font_lock_function': with MODE, make `font_lock_face'
    !> an alias of the `face' property (in `char_property_aliases'),
    !> without, remove the alias; then, if the buffer is ready for
    !> fontification, turn the fontification itself on or off.  Port of
    !> font-core.el:font-lock-default-function.
    subroutine font_lock_default_function(mode, status)
        logical, intent(in) :: mode
        type(status_type), intent(out) :: status
        type(value_type) :: table, elt, face, flf
        type(value_type), allocatable :: elts(:), members(:), kept(:)
        integer :: k, j, n, found
        logical :: has

        status = status_type()
        face = value_type(name="face")
        flf = value_type(name="font_lock_face")
        call symbol_value(value_type(name="char_property_aliases"), table, status)
        status = status_type()
        allocate (elts(0))
        if (vtag(table) == V_ARRAY) call list_elements(table, elts)
        found = 0

        do k = 1, size(elts)
            if (vtag(elts(k)) /= V_ARRAY) cycle
            call list_elements(elts(k), members)

            if (value_eq(members(1), face)) then
                found = k
                exit
            end if
        end do

        if (mode) then
            ! Add `font_lock_face' as an alias for the `face' property.
            if (found > 0) then
                call list_elements(elts(found), members)
                has = .false.

                do j = 2, size(members)
                    if (value_eq(members(j), value_type(name="font_lock_face"))) has = .true.
                end do

                if (.not. has) then
                    elt = append_symbol(members, value_type(name="font_lock_face"))
                    elts(found) = elt
                end if

                table = make_list(elts)
            else
                elt = list(face, value_type(name="font_lock_face"))
                elts = [elt, elts]
                table = make_list(elts)
            end if
        else if (found > 0) then
            ! Remove `font_lock_face' as an alias for the `face' property.
            call list_elements(elts(found), members)
            allocate (kept(size(members)))
            n = 0

            do j = 1, size(members)
                if (j > 1) then
                    if (value_eq(members(j), flf)) cycle
                end if

                n = n + 1
                kept(n) = members(j)
            end do

            if (n <= 1) then
                ! (The element goes.)
                deallocate (kept)
                allocate (kept(size(elts) - 1))

                do j = 1, size(elts)
                    if (j < found) kept(j) = elts(j)
                    if (j > found) kept(j - 1) = elts(j)
                end do

                call move_alloc(kept, elts)
            else
                elts(found) = make_list(kept(1:n))
            end if

            table = make_list(elts)
        end if

        call setq_local("char_property_aliases", table, status)
        if (.not. status%ok()) return

        ! Only do hard work if the mode has specified stuff in
        ! `font_lock_defaults'.
        if (associated(font_lock_specified_test) .and. associated(font_lock_mode_internal_hook)) then
            if (font_lock_specified_test(mode)) call font_lock_mode_internal_hook(mode, status)
        end if

    contains

        function append_symbol(items, s) result(v)
            type(value_type), intent(in) :: items(:), s
            type(value_type) :: v
            type(value_type), allocatable :: all(:)

            all = [items, s]
            v = make_list(all)
        end function append_symbol
    end subroutine font_lock_default_function

    !> The current buffer's major mode (`major_mode').  Not a port.
    function major_mode_of_current() result(mode)
        character(len=:), allocatable :: mode

        mode = "fundamental_mode"
        if (current == 0) return
        if (allocated(buffers(current)%major_mode)) mode = buffers(current)%major_mode
    end function major_mode_of_current

    !> Turn on Font Lock mode (only if the terminal can display it).
    !> Port of font-core.el:turn-on-font-lock.
    subroutine turn_on_font_lock(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. minor_modes%font_lock_mode%enabled()) call minor_modes%font_lock_mode%enable(status)
    end subroutine turn_on_font_lock

    !> Turn on Font Lock mode if the major mode is one of
    !> `font_lock_global_modes'.  Port of
    !> font-core.el:turn-on-font-lock-if-desired.
    subroutine turn_on_font_lock_if_desired(status)
        type(status_type), intent(out) :: status
        logical :: desired, member
        integer :: k

        status = status_type()
        if (current == 0) return

        if (font_lock_global_modes%all) then
            desired = .true.
        else
            member = .false.

            if (allocated(font_lock_global_modes%modes)) then
                do k = 1, size(font_lock_global_modes%modes)
                    if (trim(font_lock_global_modes%modes(k)) == major_mode_of_current()) member = .true.
                end do
            end if

            desired = member .neqv. font_lock_global_modes%negate
        end if

        if (desired) call turn_on_font_lock(status)
    end subroutine turn_on_font_lock_if_desired

    ! ------------------------------------------------------------------
    ! Global Font Lock mode (define-globalized-minor-mode)
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry, made if MAKE (0 if none).
    !> Not a port.
    integer function global_index(make) result(k)
        logical, intent(in) :: make
        type(global_entry_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (current == 0) return
        if (.not. allocated(global_entries)) allocate (global_entries(0))

        do i = 1, size(global_entries)
            if (global_entries(i)%serial == buffers(current)%serial) then
                k = i
                return
            end if
        end do

        if (.not. make) return
        n = size(global_entries)
        allocate (grown(n + 1))
        grown(1:n) = global_entries
        grown(n + 1)%serial = buffers(current)%serial
        call move_alloc(grown, global_entries)
        k = n + 1
    end function global_index

    !> Toggle Font Lock mode in many buffers.
    !> Specifically, Font Lock mode is enabled in all buffers where
    !> `turn_on_font_lock_if_desired' would do it.
    !>
    !> With prefix ARG, enable Global Font Lock mode if ARG is positive;
    !> otherwise, disable it.
    !>
    !> See `font_lock_mode' for more information on Font Lock mode.
    !> Port of font-core.el:global-font-lock-mode.
    subroutine global_font_lock_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%global_font_lock_mode%command(status)
    end subroutine global_font_lock_mode

    !> The body of Global Font Lock mode with its new state ON: set up the
    !> hook to handle future mode changes and new buffers, then go through
    !> the existing buffers.
    !> Port of font-core.el:global-font-lock-mode in part.
    subroutine global_font_lock_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:)
        integer(int64) :: b, saved
        integer :: k

        status = status_type()

        if (on) then
            call after_change_major_mode_hook%append(global_font_lock_mode_enable_in_buffer)
        else
            call after_change_major_mode_hook%remove(global_font_lock_mode_enable_in_buffer)
        end if

        all = buffer_list()
        saved = current

        do k = 1, size(all)
            call resolve_buffer(all(k), b, status)

            if (.not. status%ok()) then
                status = status_type()
                cycle
            end if

            call set_buffer_internal(b)

            if (on) then
                call turn_on_font_lock_if_desired(status)
            else if (minor_modes%font_lock_mode%enabled()) then
                call minor_modes%font_lock_mode%disable(status)
            end if

            if (.not. status%ok()) exit
        end do

        if (saved > 0) call set_buffer_internal(saved)
    end subroutine global_font_lock_mode_body

    !> Turn on Font Lock mode in the current buffer unless the user set it
    !> explicitly there.  Port of the global-font-lock-mode-enable-in-buffer
    !> define-globalized-minor-mode defines.
    subroutine global_font_lock_mode_enable_in_buffer(status)
        type(status_type), intent(out) :: status
        integer :: k
        logical :: saved

        status = status_type()
        k = global_index(.false.)

        if (k > 0) then
            if (global_entries(k)%set_explicitly) return
        end if

        ! We are not part of the major mode hook so we don't set it
        ! explicitly.
        saved = suppress_set_explicitly
        suppress_set_explicitly = .true.
        call turn_on_font_lock_if_desired(status)
        suppress_set_explicitly = saved
    end subroutine global_font_lock_mode_enable_in_buffer

    !> Record that Font Lock mode was set explicitly in the current buffer
    !> (it runs from `font_lock_mode_hook').  Port of the
    !> font-lock-mode-set-explicitly define-globalized-minor-mode defines.
    subroutine font_lock_mode_set_explicitly(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        if (suppress_set_explicitly) return
        k = global_index(.true.)
        if (k > 0) global_entries(k)%set_explicitly = .true.
    end subroutine font_lock_mode_set_explicitly
end module font_core_fm
