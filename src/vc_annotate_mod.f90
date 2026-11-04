! vc_annotate_mod.f90 -- VC Annotate support.
! Ported from GNU Emacs 31.1 lisp/vc/vc-annotate.el.
!
! Copyright (C) 1997-1998, 2000-2026 Free Software Foundation, Inc.
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

!> `vc_annotate' (C-x v g): the edit history of a file, each line shown
!> with the revision, author and date that last changed it, colored by
!> age.  VC-Annotate mode moves between revisions (n, p, j, a, w), shows
!> the log (l), diff (d, D) or contents (f) of the revision at a line,
!> goes to the line in the file (RET) and hides the annotations (v).
!>
!> The options `vc_annotate_background_mode', `vc_annotate_color_map'
!> and `vc_annotate_very_old_color' get their defaults, which depend on
!> the frame, when `vc_annotate' is first used (when upstream loads
!> vc-annotate.el), unless the configuration set them.  What is not
!> ported yet is listed in PORTING.md's vc_annotate_mod row.
module vc_annotate_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type
    use data_mod, only: clear_all_local_variables, append_locals_clearer, value_eq
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use modes_mod, only: major_modes, minor_modes
    use derived_mod, only: is_derived_mode
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, get_buffer, is_buffer_live, &
                          get_file_buffer, rename_buffer, erase_buffer, buffer_serial_live
    use editfns_mod, only: point, goto_char, point_min, point_max, is_eob, widen, narrow_to_region, is_buffer_narrowed
    use cmds_mod, only: forward_line
    use simple_mod, only: line_number_at_pos
    use subr_mod, only: add_to_invisibility_spec, remove_from_invisibility_spec
    use textprop_mod, only: put_text_property
    use callint_mod, only: current_prefix_arg, prefix_numeric_value, prefix_arg_type, PREFIX_NONE
    use minibuf_mod, only: read_string, minibuf_level
    use xdisp_mod, only: message
    use keymap_mod, only: keymap_set
    use fileio_mod, only: file_exists
    use files_mod, only: file_relative_name, find_file_noselect
    use faces_mod, only: named_face_type, face_named, frame_class, frame_background, frame_colors, FACE_CLASS_COLOR, &
                         FACE_BACKGROUND_DARK
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type
    use timefns_mod, only: time_type, current_time, real
    use tty_colors_mod, only: tty_color_type, tty_colors
    use window_mod, only: pop_to_buffer, switch_to_buffer_other_window, recenter
    use vc_hooks_mod, only: vc_backends, vc_backend, vc_backend_name, vc_working_revision, vc_short_revision, &
                            vc_state, vc_execute_root, vc_not_supported, vc_use_short_revision, vc_file_name_change_type, &
                            vc_prefix_map, VC_UP_TO_DATE, VC_REVISION_GRANULARITY_FILE
    use string_type_mod, only: string_type
    use vc_dispatcher_mod, only: vc_action_type, vc_exec_after, vc_parent_buffer, vc_sentinel_movepoint
    use vc_mod, only: vc_fileset_type, vc_buffer_revision, set_vc_buffer_revision, vc_buffer_overriding_fileset, &
                      set_vc_buffer_overriding_fileset, vc_ensure_vc_buffer, vc_read_revision, vc_find_revision, &
                      vc_diff_internal, vc_print_log_internal, vc_allow_async_diff
    use vc_git_mod, only: vc_git_print_log_follow
    use log_view_mod, only: log_view_vc_backend, log_view_vc_fileset, log_view_vc_annotate
    use vc_hooks_mod, only: vc_log_limit_type, VC_LIMIT_NUMBER
    implicit none(type, external)
    private

    public :: init_vc_annotate, vc_annotate, vc_annotate_mode, toggle_vc_annotate_annotation_visibility, &
              display_vc_annotate_default, vc_annotate_oldest_in_map, display_vc_annotate_autoscale, &
              display_vc_annotate_select, previous_vc_annotate_revision, next_vc_annotate_revision, &
              vc_annotate_working_revision, extract_vc_annotate_revision_at_line, vc_annotate_revision_at_line, &
              find_vc_annotate_revision_at_line, vc_annotate_revision_previous_to_line, &
              show_vc_annotate_log_revision_at_line, show_vc_annotate_diff_revision_at_line, &
              show_vc_annotate_changeset_diff_revision_at_line, warp_vc_annotate_revision, &
              convert_vc_annotate_time, vc_annotate_difference, display_vc_annotate, vc_annotate_lines, &
              goto_vc_annotate_line, vc_annotate_file_name_change, vc_annotate_current_color_map, &
              set_vc_annotate_defaults

    ! ------------------------------------------------------------------
    ! Options
    ! ------------------------------------------------------------------

    !> Values of `vc_annotate_display_mode': by the range of
    !> `vc_annotate_color_map' (VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP), scaled
    !> to the oldest annotation (VC_ANNOTATE_DISPLAY_MODE_SCALE), scaled
    !> from the oldest to the newest one (VC_ANNOTATE_DISPLAY_MODE_FULLSCALE),
    !> or over a number of days (VC_ANNOTATE_DISPLAY_MODE_DAYS).
    integer, parameter, public :: VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP = 0, VC_ANNOTATE_DISPLAY_MODE_SCALE = 1, &
                                  VC_ANNOTATE_DISPLAY_MODE_FULLSCALE = 2, VC_ANNOTATE_DISPLAY_MODE_DAYS = 3

    !> A display mode of \[vc_annotate]: its `kind' (a
    !> VC_ANNOTATE_DISPLAY_MODE_* value) and, for
    !> VC_ANNOTATE_DISPLAY_MODE_DAYS, the fractional number of `days'.
    type, public :: vc_annotate_display_mode_type
        integer :: kind = VC_ANNOTATE_DISPLAY_MODE_FULLSCALE
        real(real64) :: days = 0
    end type vc_annotate_display_mode_type

    !> Which mode to color the output of \[vc_annotate] with by default.
    !> Port of vc-annotate.el vc-annotate-display-mode.
    type(vc_annotate_display_mode_type), public :: vc_annotate_display_mode

    !> True means `vc_annotate_color_map' is applied to the background.
    !>
    !> When true, the color range from `vc_annotate_color_map' is applied
    !> to the background, while the foreground remains default.
    !>
    !> When false, the color range from `vc_annotate_color_map' is applied
    !> to the foreground, and the color from the option `vc_annotate_background'
    !> is applied to the background.  Port of vc-annotate.el
    !> vc-annotate-background-mode (unallocated until its default is set).
    logical, allocatable, public :: vc_annotate_background_mode

    !> An entry of `vc_annotate_color_map': the color of lines of an age
    !> up to `age' days.
    type, public :: vc_annotate_color_type
        real(real64) :: age = 0
        character(len=32) :: color = ""
    end type vc_annotate_color_type

    !> Array of age versus color, for \[vc_annotate].
    !> Ages are given in units of fractional days.  Default is eighteen
    !> steps using a twenty day increment, from red to blue.  For TTY
    !> displays with 8 or fewer colors, the default is red to blue with
    !> all other colors between (excluding black and white).  Port of
    !> vc-annotate.el vc-annotate-color-map.
    type(vc_annotate_color_type), allocatable, public :: vc_annotate_color_map(:)

    !> Color for lines older than the current color range in \[vc_annotate].
    !> Port of vc-annotate.el vc-annotate-very-old-color.
    character(len=:), allocatable, public :: vc_annotate_very_old_color

    !> Background color for \[vc_annotate].
    !> Default color is used if unallocated.  Port of vc-annotate.el
    !> vc-annotate-background.
    character(len=:), allocatable, public :: vc_annotate_background

    !> Menu elements for the mode-specific menu of VC-Annotate mode.
    !> Array of factors, used to expand/compress the time scale.  See `vc_annotate'.
    !> Port of vc-annotate.el vc-annotate-menu-elements.
    real(real64), allocatable, public :: vc_annotate_menu_elements(:)

    !> If true, \[vc_annotate] will use short revisions in its buffer name.
    !> Port of vc-annotate.el vc-annotate-use-short-revision.
    logical, public :: vc_annotate_use_short_revision = .true.

    !> Global variable.  The ratio `display_vc_annotate_select' scales
    !> `vc_annotate_color_map' with in VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP
    !> (1.0 when upstream has no ratio).  Port of
    !> vc-annotate.el vc-annotate-ratio.
    real(real64), public :: vc_annotate_ratio = 1.0_real64

    !> Hook run after entering VC-Annotate mode.
    type(hook_type), target, public :: vc_annotate_mode_hook = hook_type(name="vc_annotate_mode_hook")

    !> Local keymap used for VC-Annotate mode.  Port of vc-annotate.el
    !> vc-annotate-mode-map.
    type(handle_type), public :: vc_annotate_mode_map

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! A VC-Annotate buffer's own values, by serial number: vc-annotate-backend,
    ! vc-annotate-parent-display-mode, vc-annotate-color-map (when made
    ! buffer-local by `display_vc_annotate') and vc-annotate-offset.
    type :: local_type
        integer(int64) :: serial = 0
        integer :: backend = 0
        type(vc_annotate_display_mode_type) :: parent_display_mode
        logical :: has_color_map = .false.
        type(vc_annotate_color_type), allocatable :: color_map(:)
        logical :: has_offset = .false.
        real(real64) :: offset = 0
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! The text properties and the invisibility spec element of the
    ! annotations (text property names are run-time data of the text
    ! property interface).
    character(len=*), parameter :: FACE_PROPERTY = "face", INVISIBLE_PROPERTY = "invisible", &
                                   ANNOTATION = "vc_annotate_annotation"

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals' (0 if none).
    ! Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, dim=1)
    end function local_index

    ! Find or make the current buffer's entry in `locals', reusing the
    ! entry of a closed buffer; K is its index (0 without a current
    ! buffer).  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, entry]
        locals(k) = local_type(serial=buffers(current)%serial)
    end subroutine make_local_entry

    ! Forget the current buffer's values (kill-all-local-variables).
    ! Not a port.
    subroutine clear_annotate_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k) = local_type()
    end subroutine clear_annotate_locals

    ! The current buffer's `vc_annotate_backend' (0 for none).  Not a port.
    integer function annotate_backend() result(b)
        integer :: k

        b = 0
        k = local_index()
        if (k > 0) b = locals(k)%backend
    end function annotate_backend

    ! The file of the current buffer's `vc_buffer_overriding_fileset'
    ! ((caadr vc-buffer-overriding-fileset)), "" for none.  Not a port.
    function annotated_file() result(file)
        character(len=:), allocatable :: file
        type(vc_fileset_type) :: fs
        logical :: has

        file = ""
        call vc_buffer_overriding_fileset(fs, has)
        if (.not. has .or. .not. allocated(fs%files)) return
        if (size(fs%files) > 0) file = fs%files(1)%text
    end function annotated_file

    !> The current buffer's `vc_annotate_color_map': its own (set by
    !> `display_vc_annotate'), else the global one.  Not a port.
    subroutine vc_annotate_current_color_map(map)
        type(vc_annotate_color_type), allocatable, intent(out) :: map(:)
        integer :: k

        call set_vc_annotate_defaults()
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_color_map) then
                map = locals(k)%color_map
                return
            end if
        end if

        map = vc_annotate_color_map
    end subroutine vc_annotate_current_color_map

    ! (kill-local-variable 'vc-annotate-color-map).  Not a port.
    subroutine clear_local_color_map()
        integer :: k

        k = local_index()
        if (k == 0) return
        locals(k)%has_color_map = .false.
        if (allocated(locals(k)%color_map)) deallocate (locals(k)%color_map)
    end subroutine clear_local_color_map

    ! (equal major-mode 'vc-annotate-mode).  Not a port.
    logical function is_annotate_buffer()
        is_annotate_buffer = .false.
        if (current == 0) return
        if (.not. allocated(buffers(current)%major_mode)) return
        is_annotate_buffer = buffers(current)%major_mode == trim(major_modes%vc_annotate_mode%name)
    end function is_annotate_buffer

    ! ------------------------------------------------------------------
    ! The defaults of the options
    ! ------------------------------------------------------------------

    !> Give `vc_annotate_background_mode', `vc_annotate_color_map' and
    !> `vc_annotate_very_old_color' their defaults for the frame, unless
    !> they have values.  Port of the defcustom default values of
    !> vc-annotate.el vc-annotate-background-mode, vc-annotate-color-map
    !> and vc-annotate-very-old-color.
    subroutine set_vc_annotate_defaults()
        logical :: few_colors

        ! (and (tty-display-color-p) (<= (display-color-cells) 8))
        few_colors = frame_class == FACE_CLASS_COLOR .and. frame_colors <= 8

        if (.not. allocated(vc_annotate_background_mode)) then
            vc_annotate_background_mode = .not. (frame_background == FACE_BACKGROUND_DARK .or. few_colors)
        end if

        if (.not. allocated(vc_annotate_color_map)) then
            if (few_colors) then
                call set_tty_color_map()
            else if (vc_annotate_background_mode) then
                ! Normal colormap for background colors with dark foreground:
                ! hue stepped from 0-240deg, value=1., saturation=0.20
                call set_color_map([character(len=7) :: "#FFCCCC", "#FFD8CC", "#FFE4CC", "#FFF0CC", "#FFFCCC", &
                                    "#F6FFCC", "#EAFFCC", "#DEFFCC", "#D2FFCC", "#CCFFD2", "#CCFFDE", "#CCFFEA", &
                                    "#CCFFF6", "#CCFCFF", "#CCF0FF", "#CCE4FF", "#CCD8FF", "#CCCCFF"])
            else
                ! Normal colormap for foreground colors on dark background:
                ! hue stepped from 0-240deg, value=1., saturation=0.75
                call set_color_map([character(len=7) :: "#FF3F3F", "#FF6C3F", "#FF993F", "#FFC63F", "#FFF33F", &
                                    "#DDFF3F", "#B0FF3F", "#83FF3F", "#56FF3F", "#3FFF56", "#3FFF83", "#3FFFB0", &
                                    "#3FFFDD", "#3FF3FF", "#3FC6FF", "#3F99FF", "#3F6CFF", "#3F3FFF"])
            end if
        end if

        if (.not. allocated(vc_annotate_very_old_color)) then
            if (vc_annotate_background_mode) then
                vc_annotate_very_old_color = "#CCCCFF"
            else
                vc_annotate_very_old_color = "#3F3FFF"
            end if
        end if

        if (.not. allocated(vc_annotate_menu_elements)) vc_annotate_menu_elements = [2.0_real64, 0.5_real64, &
                                                                                     0.1_real64, 0.01_real64]
    end subroutine set_vc_annotate_defaults

    ! `vc_annotate_color_map' with COLORS at ages 20, 40, ... 360.
    ! Not a port.
    subroutine set_color_map(colors)
        character(len=*), intent(in) :: colors(:)
        integer :: k

        allocate (vc_annotate_color_map(size(colors)))

        do k = 1, size(colors)
            vc_annotate_color_map(k)%age = 20.0_real64*k
            vc_annotate_color_map(k)%color = colors(k)
        end do
    end subroutine set_color_map

    ! The custom sorted TTY colormap: the terminal's colors but white and
    ! black, red first, then yellow, cyan, green, magenta, the others in
    ! alphabetical order and blue last, at ages from 20 to 360 days.
    ! Not a port.
    subroutine set_tty_color_map()
        type(tty_color_type), allocatable :: colors(:)
        character(len=32), allocatable :: names(:)
        character(len=32) :: name
        real(real64) :: date, delta
        integer :: k, j, n

        colors = tty_colors()
        allocate (names(size(colors)))
        n = 0

        do k = 1, size(colors)
            if (colors(k)%name == "white" .or. colors(k)%name == "black") cycle
            ! Insertion sort by `color_rank'.
            name = colors(k)%name
            j = n

            do while (j > 0)
                if (.not. is_color_before(name, names(j))) exit
                names(j + 1) = names(j)
                j = j - 1
            end do

            names(j + 1) = name
            n = n + 1
        end do

        allocate (vc_annotate_color_map(n))
        date = 20.0_real64
        delta = (360.0_real64 - date)/real(max(n - 1, 1), real64)

        do k = 1, n
            vc_annotate_color_map(k)%age = date
            vc_annotate_color_map(k)%color = names(k)
            date = date + delta
        end do

    contains

        ! The order of the custom TTY colormap's sort.
        logical function is_color_before(a, b)
            character(len=*), intent(in) :: a, b

            if (color_rank(a) /= color_rank(b)) then
                is_color_before = color_rank(a) < color_rank(b)
            else
                is_color_before = llt(trim(a), trim(b))
            end if
        end function is_color_before

        integer function color_rank(c)
            character(len=*), intent(in) :: c

            select case (trim(c))
            case ("red")
                color_rank = 1
            case ("yellow")
                color_rank = 2
            case ("cyan")
                color_rank = 3
            case ("green")
                color_rank = 4
            case ("magenta")
                color_rank = 5
            case ("blue")
                color_rank = 7
            case default
                color_rank = 6
            end select
        end function color_rank
    end subroutine set_tty_color_map

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for output buffers of the `vc_annotate' command.
    !>
    !> You can use the mode-specific menu to alter the time-span of the used
    !> colors.  See variable `vc_annotate_menu_elements' for customizing the
    !> menu items.  Port of vc-annotate.el:vc-annotate-mode.
    recursive subroutine vc_annotate_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved

        if (.not. associated(major_modes%vc_annotate_mode%execute)) call init_vc_annotate()
        call major_modes%vc_annotate_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! Frob buffer-invisibility-spec so that if it is originally a naked t,
        ! it will become a list, to avoid initial annotations being invisible.
        call add_to_invisibility_spec("foo")
        call remove_from_invisibility_spec("foo")
        ! The fontification is done by vc-annotate-lines instead of font-lock.
        allocate (keywords(1))
        keywords(1)%matcher => vc_annotate_lines
        allocate (keywords(1)%highlights(0))
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call major_modes%vc_annotate_mode%end(saved, status)
    end subroutine vc_annotate_mode

    !> Toggle whether or not the annotation is visible.  Port of
    !> vc-annotate.el:vc-annotate-toggle-annotation-visibility.
    recursive subroutine toggle_vc_annotate_annotation_visibility(status)
        type(status_type), intent(out) :: status
        logical :: visible
        integer :: k

        status = status_type()
        if (current == 0) return
        visible = .true.

        associate (b => buffers(current))
            if (.not. b%invisibility_spec_t .and. allocated(b%invisibility_spec)) then
                do k = 1, size(b%invisibility_spec)
                    if (b%invisibility_spec(k)%cons) cycle
                    if (value_eq(b%invisibility_spec(k)%atom, value_type(name=ANNOTATION))) visible = .false.
                end do
            end if
        end associate

        if (visible) then
            call add_to_invisibility_spec(ANNOTATION)
        else
            call remove_from_invisibility_spec(ANNOTATION)
        end if
    end subroutine toggle_vc_annotate_annotation_visibility

    ! ------------------------------------------------------------------
    ! Display modes
    ! ------------------------------------------------------------------

    !> Display the output of \[vc_annotate] using the default color range.
    !> The color range is given by `vc_annotate_color_map', scaled by RATIO.
    !> The current time is used as the offset.  Port of
    !> vc-annotate.el:vc-annotate-display-default.
    recursive subroutine display_vc_annotate_default(ratio, status)
        real(real64), intent(in) :: ratio
        type(status_type), intent(out) :: status

        call message("Redisplaying annotation...")
        call display_vc_annotate(ratio, status)
        if (.not. status%ok()) return
        call message("Redisplaying annotation...done")
    end subroutine display_vc_annotate_default

    !> Return the oldest time in the COLOR_MAP.  Port of
    !> vc-annotate.el:vc-annotate-oldest-in-map.
    real(real64) function vc_annotate_oldest_in_map(color_map) result(oldest)
        type(vc_annotate_color_type), intent(in) :: color_map(:)

        ! Since entries should be sorted, we can just use the last one.
        oldest = 0
        if (size(color_map) > 0) oldest = color_map(size(color_map))%age
    end function vc_annotate_oldest_in_map

    ! Call the backend's `annotate_time' on the line at point (DAYS, FOUND
    ! false if none) and make its annotation invisible with the property
    ! `vc_annotate_annotation'.  Port of
    ! vc-annotate.el:vc-annotate-get-time-set-line-props.
    recursive subroutine get_time_set_line_props(days, found)
        real(real64), intent(out) :: days
        logical, intent(out) :: found
        integer(int64) :: bol
        type(status_type) :: st
        logical :: read_only
        integer :: b

        days = 0
        found = .false.
        bol = point()
        b = annotate_backend()
        if (b == 0) return
        if (.not. associated(vc_backends(b)%annotate_time)) return
        call vc_backends(b)%annotate_time(days, found)
        ! (let ((inhibit-read-only t)) ...)
        read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call put_text_property(bol, point(), value_type(name=INVISIBLE_PROPERTY), value_type(name=ANNOTATION), st)
        buffers(current)%read_only = read_only
    end subroutine get_time_set_line_props

    !> Highlight the output of \[vc_annotate] using an autoscaled color map.
    !> Autoscaling means that the map is scaled from the current time to the
    !> oldest annotation in the buffer, or, with prefix argument FULL, to
    !> cover the range from the oldest annotation to the newest.  Port of
    !> vc-annotate.el:vc-annotate-display-autoscale.
    recursive subroutine display_vc_annotate_autoscale(status, full)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: full
        type(vc_annotate_color_type), allocatable :: map(:)
        real(real64) :: newest, oldest, now, date
        integer(int64) :: opoint, shortage
        logical :: is_full, found

        is_full = .false.
        if (present(full)) is_full = full
        newest = 0
        ! Any CVS users at the founding of Rome?
        oldest = 999999
        now = convert_vc_annotate_time()
        call message("Redisplaying annotation...")
        ! Run through this file and find the oldest and newest dates annotated.
        opoint = point()
        call goto_char(point_min())

        do while (.not. is_eob())
            call get_time_set_line_props(date, found)

            if (found) then
                if (date > newest) newest = date
                if (date < oldest) oldest = date
            end if

            shortage = forward_line(1_int64)
        end do

        call goto_char(opoint)
        call vc_annotate_current_color_map(map)

        if (is_full) then
            call display_vc_annotate((newest - oldest)/vc_annotate_oldest_in_map(map), status, newest)
        else
            call display_vc_annotate((now - oldest)/vc_annotate_oldest_in_map(map), status)
        end if

        if (.not. status%ok()) return

        if (is_full) then
            call message("Redisplaying annotation...done (Spanned from "//days_string(now - oldest)//" to " &
                         //days_string(now - newest)//" days old)")
        else
            call message("Redisplaying annotation...done (Spanned to "//days_string(now - oldest)//" days old)")
        end if
    end subroutine display_vc_annotate_autoscale

    ! DAYS formatted with %.1f.  Not a port.
    function days_string(days) result(s)
        real(real64), intent(in) :: days
        character(len=:), allocatable :: s
        character(len=40) :: buffer

        write (buffer, "(f0.1)") days
        s = trim(buffer)
        if (s(1:1) == ".") s = "0"//s

        if (len(s) > 1) then
            if (s(1:2) == "-.") s = "-0"//s(2:)
        end if
    end function days_string

    !> Highlight the output of \[vc_annotate].
    !> By default, the current buffer is highlighted, unless overridden by
    !> BUFFER.  `vc_annotate_display_mode' specifies the highlighting mode to
    !> use; you may override this using the second optional arg MODE.
    !> Port of vc-annotate.el:vc-annotate-display-select.
    recursive subroutine display_vc_annotate_select(status, buffer, mode)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        type(vc_annotate_display_mode_type), intent(in), optional :: mode
        type(vc_annotate_color_type), allocatable :: map(:)
        type(handle_type) :: target
        type(vc_action_type) :: action
        type(status_type) :: st

        if (present(mode)) vc_annotate_display_mode = mode
        target = current_buffer()
        if (present(buffer)) target = buffer
        call pop_to_buffer(target, status)
        if (.not. status%ok()) return
        call set_buffer(target, st)

        select case (vc_annotate_display_mode%kind)
        case (VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP)
            ! The ratio is global, thus relative to the global color-map.
            call clear_local_color_map()
            call display_vc_annotate_default(vc_annotate_ratio, status)
        case (VC_ANNOTATE_DISPLAY_MODE_SCALE, VC_ANNOTATE_DISPLAY_MODE_FULLSCALE)
            ! One of the auto-scaling modes
            action%execute => autoscale_action
            action%buffer = target
            action%flag = vc_annotate_display_mode%kind == VC_ANNOTATE_DISPLAY_MODE_FULLSCALE
            call vc_exec_after(action, status)
        case (VC_ANNOTATE_DISPLAY_MODE_DAYS)
            ! A fixed number of days lookback
            call vc_annotate_current_color_map(map)
            call display_vc_annotate_default(vc_annotate_display_mode%days/vc_annotate_oldest_in_map(map), status)
        case default
            call set_error(status, FM_GENERIC_ERROR, "No such display mode")
        end select
    end subroutine display_vc_annotate_select

    ! (vc-run-delayed (vc-annotate-display-autoscale FULL)) of
    ! `display_vc_annotate_select'.  Port of the delayed part of
    ! vc-annotate.el:vc-annotate-display-select.
    recursive subroutine autoscale_action(action, status)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(status_type) :: st

        status = status_type()
        if (.not. is_buffer_live(action%buffer)) return
        call set_buffer(action%buffer, st)
        call display_vc_annotate_autoscale(status, action%flag)
    end subroutine autoscale_action

    ! ------------------------------------------------------------------
    ! vc-annotate
    ! ------------------------------------------------------------------

    !> Display the edit history of the current FILE using colors.
    !>
    !> This command creates a buffer that shows, for each line of the current
    !> file, when it was last edited and by whom.  Additionally, colors are
    !> used to show the age of each line--blue means oldest, red means
    !> youngest, and intermediate colors indicate intermediate ages.  By
    !> default, the time scale stretches back one year into the past;
    !> everything that is older than that is shown in blue.
    !>
    !> With a prefix argument, this command asks two questions in the
    !> minibuffer.  First, you may enter a revision number REV; then the buffer
    !> displays and annotates that revision instead of the working revision
    !> (type RET in the minibuffer to leave that default unchanged).  Then,
    !> you are prompted for the time span in days which the color range
    !> should cover.  For example, a time span of 20 days means that changes
    !> over the past 20 days are shown in red to blue, according to their
    !> age, and everything that is older than that is shown in blue.
    !>
    !> If MOVE_POINT_TO is given, move the point to that line.
    !>
    !> If BACKEND is given, use that VC backend.
    !>
    !> Customization variables:
    !>
    !> `vc_annotate_menu_elements' customizes the menu elements of the
    !> mode-specific menu.  `vc_annotate_color_map' and
    !> `vc_annotate_very_old_color' define the mapping of time to colors.
    !> `vc_annotate_background' specifies the background color.
    !> `vc_annotate_background_mode' specifies whether the color map
    !> should be applied to the background or to the foreground.  Port of
    !> vc-annotate.el:vc-annotate (DISPLAY_MODE absent is
    !> VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP, as upstream without one).
    recursive subroutine vc_annotate(file, rev, status, display_mode, buffer, move_point_to, backend)
        character(len=*), intent(in) :: file, rev
        type(status_type), intent(out) :: status
        type(vc_annotate_display_mode_type), intent(in), optional :: display_mode
        type(handle_type), intent(in), optional :: buffer
        integer(int64), intent(in), optional :: move_point_to
        integer, intent(in), optional :: backend
        type(vc_annotate_display_mode_type) :: mode
        type(vc_fileset_type) :: fs
        type(handle_type) :: file_buffer, old, out
        type(vc_action_type) :: action
        type(status_type) :: st
        character(len=:), allocatable :: temp_buffer_name, root, name, dir
        integer(int64) :: current_line
        logical :: has, found
        integer :: b, k

        status = status_type()
        if (present(display_mode)) mode = display_mode
        if (.not. present(display_mode)) mode = vc_annotate_display_mode_type(kind=VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP)
        ! Not sure why.  --Stef
        vc_annotate_display_mode = mode
        call set_vc_annotate_defaults()
        b = 0
        if (present(backend)) b = backend

        if (b == 0) then
            call vc_buffer_overriding_fileset(fs, has)
            if (has) b = fs%backend
        end if

        if (b == 0) b = vc_backend(file)

        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "File "//file//" is not under version control")
            return
        end if

        file_buffer = get_file_buffer(file)

        if (file_buffer%index /= 0) then
            name = buffers(file_buffer%index)%name
        else
            ! Try to avoid ambiguity.
            call vc_execute_root(b, default_directory(), root, found, status)
            if (.not. status%ok()) return
            name = file_relative_name(file, root)
        end if

        temp_buffer_name = "*Annotate "//name//" (rev "//rev//")*"
        ! If BUFFER is specified, we presume the caller maintains current line,
        ! so we don't need to do it here.  This implementation may give
        ! strange results occasionally in the case of REV != WORKFILE-REV.
        current_line = 0

        if (present(move_point_to)) then
            current_line = move_point_to
        else if (.not. present(buffer)) then
            current_line = line_number_at_pos(absolute=.true.)
        end if

        call message("Annotating...")
        old = current_buffer()

        ! If BUFFER is specified it tells in which buffer we should put the
        ! annotations.  This is used when switching annotations to another
        ! revision, so we should update the buffer's name.
        if (present(buffer)) then
            call set_buffer(buffer, st)
            call rename_buffer(temp_buffer_name, .true., name, status)
            ! In case it had to be uniquified.
            if (status%ok()) temp_buffer_name = buffers(current)%name
            call set_buffer(old, st)
            if (.not. status%ok()) return
        end if

        ! (with-output-to-temp-buffer temp-buffer-name ...): the buffer is
        ! emptied, with no local variables.
        dir = default_directory()
        call get_or_make_buffer(temp_buffer_name, out, status)
        if (.not. status%ok()) return
        call set_buffer(out, st)
        call clear_all_local_variables()
        buffers(current)%directory = dir
        buffers(current)%read_only = .false.
        call erase_buffer(st)
        call set_buffer(old, st)

        if (.not. associated(vc_backends(b)%annotate_command)) then
            call vc_not_supported(status, "annotate-command", b)
            return
        end if

        call vc_backends(b)%annotate_command(file, out, rev, status)
        if (.not. status%ok()) return
        ! we must setup the mode first, and then set our local
        ! variables before the show-function is called at the exit of
        ! with-output-to-temp-buffer
        call set_buffer(out, st)

        if (.not. is_annotate_buffer()) then
            call vc_annotate_mode(status)
            if (.not. status%ok()) return
        end if

        call make_local_entry(k)
        locals(k)%backend = b
        locals(k)%parent_display_mode = mode
        fs = vc_fileset_type(backend=b)
        allocate (fs%files(1))
        fs%files(1)%text = file
        call set_vc_buffer_overriding_fileset(fs)
        call set_vc_buffer_revision(rev)
        ! (internal-temp-output-buffer-show buffer): unmodified, at its
        ! beginning, then `display_vc_annotate_select' as
        ! `temp_buffer_show_function'.
        buffers(current)%text%save_modiff = buffers(current)%text%modiff
        call widen()
        call goto_char(point_min())
        call display_vc_annotate_select(status, out)
        if (.not. status%ok()) return
        call set_buffer(out, st)
        action%execute => annotate_finish
        action%buffer = out
        action%n = int(current_line)
        call vc_exec_after(action, status)
    end subroutine vc_annotate

    ! The current buffer's `default_directory'.  Not a port.
    function default_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
    end function default_directory

    ! (vc-run-delayed ...) of `vc_annotate': go to the line ACTION%N (none
    ! when 0) and say that annotating is done.  Port of the delayed part
    ! of vc-annotate.el:vc-annotate.
    recursive subroutine annotate_finish(action, status)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: shortage

        status = status_type()
        if (.not. is_buffer_live(action%buffer)) return
        call set_buffer(action%buffer, st)

        ! Ideally, we'd rather not move point if the user has already
        ! moved it elsewhere, but really point here is not the position
        ! of the user's cursor :-(
        if (action%n > 0) then
            call goto_char(point_min())
            shortage = forward_line(int(action%n - 1, int64))
            vc_sentinel_movepoint = point()
        end if

        if (minibuf_level == 0) call message("Annotating... done")
    end subroutine annotate_finish

    ! ------------------------------------------------------------------
    ! Moving between revisions
    ! ------------------------------------------------------------------

    !> Visit the annotation of the revision previous to this one.
    !>
    !> With a numeric prefix argument, annotate the revision that many
    !> revisions previous.  Port of vc-annotate.el:vc-annotate-prev-revision.
    recursive subroutine previous_vc_annotate_revision(prefix, status)
        integer(int64), intent(in) :: prefix
        type(status_type), intent(out) :: status

        call warp_vc_annotate_revision(status, count=-prefix)
    end subroutine previous_vc_annotate_revision

    !> Visit the annotation of the revision after this one.
    !>
    !> With a numeric prefix argument, annotate the revision that many
    !> revisions after.  Port of vc-annotate.el:vc-annotate-next-revision.
    recursive subroutine next_vc_annotate_revision(prefix, status)
        integer(int64), intent(in) :: prefix
        type(status_type), intent(out) :: status

        call warp_vc_annotate_revision(status, count=prefix)
    end subroutine next_vc_annotate_revision

    !> Visit the annotation of the working revision of this file.  Port of
    !> vc-annotate.el:vc-annotate-working-revision.
    recursive subroutine vc_annotate_working_revision(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: warp_rev
        logical :: found

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call message("Cannot be invoked outside of a vc annotate buffer")
        else
            call vc_working_revision(annotated_file(), warp_rev, found)

            if (warp_rev == vc_buffer_revision()) then
                call message("Already at revision "//warp_rev)
            else
                call warp_vc_annotate_revision(status, revision=warp_rev)
            end if
        end if
    end subroutine vc_annotate_working_revision

    !> Extract the revision number of the current line: REVISION and the
    !> FILE it had (FOUND false if the line has none).  Port of
    !> vc-annotate.el:vc-annotate-extract-revision-at-line.
    recursive subroutine extract_vc_annotate_revision_at_line(revision, file, found)
        character(len=:), allocatable, intent(out) :: revision, file
        logical, intent(out) :: found
        integer :: b

        ! This function must be invoked from a buffer in vc-annotate-mode
        revision = ""
        file = ""
        found = .false.
        b = annotate_backend()
        if (b == 0) return
        if (.not. associated(vc_backends(b)%annotate_extract_revision_at_line)) return
        call vc_backends(b)%annotate_extract_revision_at_line(revision, file, found)
        if (found .and. len(file) == 0) file = annotated_file()
    end subroutine extract_vc_annotate_revision_at_line

    !> Visit the annotation of the revision identified in the current line.
    !> Port of vc-annotate.el:vc-annotate-revision-at-line.
    recursive subroutine vc_annotate_revision_at_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: revision, file
        logical :: found

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call message("Cannot be invoked outside of a vc annotate buffer")
            return
        end if

        call extract_vc_annotate_revision_at_line(revision, file, found)

        if (.not. found) then
            call message("Cannot extract revision number from the current line")
        else if (revision == vc_buffer_revision() .and. file == annotated_file()) then
            call message("Already at revision "//revision//" of "//file)
        else
            call warp_vc_annotate_revision(status, revision=revision, file=file)
        end if
    end subroutine vc_annotate_revision_at_line

    !> Visit the revision identified in the current line.  Port of
    !> vc-annotate.el:vc-annotate-find-revision-at-line.
    recursive subroutine find_vc_annotate_revision_at_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: revision, file
        type(handle_type) :: buffer
        logical :: found

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call message("Cannot be invoked outside of a vc annotate buffer")
            return
        end if

        call extract_vc_annotate_revision_at_line(revision, file, found)

        if (.not. found) then
            call message("Cannot extract revision number from the current line")
        else
            call vc_find_revision(file, revision, buffer, status, annotate_backend())
            if (.not. status%ok()) return
            call switch_to_buffer_other_window(buffer, status)
        end if
    end subroutine find_vc_annotate_revision_at_line

    ! The backend's `previous_revision' of REV for FILE ("" for none),
    ! with `vc_use_short_revision' bound to `vc_annotate_use_short_revision'
    ! (FOUND false if there is none).  Not a port.
    recursive subroutine previous_revision(b, file, rev, prev, found, status)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file, rev
        character(len=:), allocatable, intent(out) :: prev
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        logical :: saved

        status = status_type()
        prev = ""
        found = .false.

        if (.not. associated(vc_backends(b)%previous_revision)) then
            call vc_not_supported(status, "previous-revision", b)
            return
        end if

        saved = vc_use_short_revision
        vc_use_short_revision = vc_annotate_use_short_revision
        call vc_backends(b)%previous_revision(file, rev, prev, found)
        vc_use_short_revision = saved
    end subroutine previous_revision

    !> Visit the annotation of the revision before the revision at line.
    !> Port of vc-annotate.el:vc-annotate-revision-previous-to-line.
    recursive subroutine vc_annotate_revision_previous_to_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: revision, file_name, prev_rev
        logical :: found

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call message("Cannot be invoked outside of a vc annotate buffer")
            return
        end if

        call extract_vc_annotate_revision_at_line(revision, file_name, found)

        if (.not. found) then
            call message("Cannot extract revision number from the current line")
            return
        end if

        call previous_revision(annotate_backend(), file_name, revision, prev_rev, found, status)
        if (.not. status%ok()) return

        if (.not. found) then
            call message("No previous revisions")
        else
            call warp_vc_annotate_revision(status, revision=prev_rev, file=file_name)
        end if
    end subroutine vc_annotate_revision_previous_to_line

    !> Visit the log of the revision at line.
    !> If the VC backend supports it, only show the log entry for the revision.
    !> If a *vc-change-log* buffer exists and already shows a log for
    !> the file in question, search for the log entry required and move point.
    !> Port of vc-annotate.el:vc-annotate-show-log-revision-at-line.
    recursive subroutine show_vc_annotate_log_revision_at_line(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: fileset(:), files(:)
        type(vc_log_limit_type) :: limit
        character(len=:), allocatable :: revision, file
        type(handle_type) :: log_buffer, old
        type(status_type) :: st
        integer(int64) :: pos
        logical :: found, has, saved_follow
        integer :: b

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call message("Cannot be invoked outside of a vc annotate buffer")
            return
        end if

        call extract_vc_annotate_revision_at_line(revision, file, found)

        if (.not. found) then
            call message("Cannot extract revision number from the current line")
            return
        end if

        b = annotate_backend()
        log_buffer = get_buffer("*vc-change-log*")
        ! No need to follow renames: we specify the historical file name.
        saved_follow = vc_git_print_log_follow
        vc_git_print_log_follow = .false.
        found = .false.
        pos = 0

        if (is_buffer_live(log_buffer)) then
            ! Look for a log buffer that already displays the correct file.
            old = current_buffer()
            call set_buffer(log_buffer, st)
            call log_view_vc_fileset(fileset, has)

            if (b == log_view_vc_backend() .and. has) then
                if (size(fileset) == 1) then
                    ! Check if the entry we require can be found.
                    if (fileset(1)%text == file .and. associated(vc_backends(b)%show_log_entry)) then
                        call vc_backends(b)%show_log_entry(revision, found)
                        pos = point()
                    end if
                end if
            end if

            call set_buffer(old, st)
        end if

        if (found) then
            call pop_to_buffer(log_buffer, status)
            if (status%ok()) call goto_char(pos)
        else
            ! Ask the backend to display a single log entry.
            allocate (files(1))
            files(1)%text = file
            limit%kind = VC_LIMIT_NUMBER
            limit%n = 1
            call vc_print_log_internal(b, files, revision, status, .true., limit)
        end if

        vc_git_print_log_follow = saved_follow
    end subroutine show_vc_annotate_log_revision_at_line

    ! The diff of the revision at line with its previous revision, of the
    ! file (FILEDIFF) or of the whole changeset.  Port of
    ! vc-annotate.el:vc-annotate-show-diff-revision-at-line-internal.
    recursive subroutine show_diff_revision_at_line_internal(filediff, status)
        logical, intent(in) :: filediff
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: revision, file_name, prev_rev
        logical :: found, changes

        status = status_type()

        if (.not. is_derived_mode(major_modes%vc_annotate_mode)) then
            call message("Cannot be invoked outside of a vc annotate buffer")
            return
        end if

        call extract_vc_annotate_revision_at_line(revision, file_name, found)

        if (.not. found) then
            call message("Cannot extract revision number from the current line")
            return
        end if

        if (filediff) then
            call previous_revision(annotate_backend(), file_name, revision, prev_rev, found, status)
        else
            call previous_revision(annotate_backend(), "", revision, prev_rev, found, status)
        end if

        if (.not. status%ok()) return
        ! The value passed here should follow what
        ! `vc-deduce-fileset' returns.
        fs%backend = annotate_backend()

        if (filediff) then
            allocate (fs%files(1))
            fs%files(1)%text = file_name
        else
            allocate (fs%files(0))
        end if

        if (found) then
            call vc_diff_internal(vc_allow_async_diff, fs, changes, status, prev_rev, revision)
        else
            call vc_diff_internal(vc_allow_async_diff, fs, changes, status, rev2=revision)
        end if
    end subroutine show_diff_revision_at_line_internal

    !> Visit the diff of the revision at line from its previous revision.
    !> Port of vc-annotate.el:vc-annotate-show-diff-revision-at-line.
    recursive subroutine show_vc_annotate_diff_revision_at_line(status)
        type(status_type), intent(out) :: status

        call show_diff_revision_at_line_internal(.true., status)
    end subroutine show_vc_annotate_diff_revision_at_line

    !> Show the diffs of revision at current line relative to previous revision.
    !> This is done for all files in changeset.  Port of
    !> vc-annotate.el:vc-annotate-show-changeset-diff-revision-at-line.
    recursive subroutine show_vc_annotate_changeset_diff_revision_at_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved_dir, root
        logical :: found
        integer :: b

        status = status_type()
        b = annotate_backend()

        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a VC-Annotate buffer")
            return
        end if

        if (vc_backends(b)%revision_granularity == VC_REVISION_GRANULARITY_FILE) then
            call set_error(status, FM_GENERIC_ERROR, "The "//vc_backend_name(b)//" backend does not support changeset diffs")
            return
        end if

        ! Make sure `diff-goto-source' will be able to find all files.
        saved_dir = default_directory()
        call vc_execute_root(b, saved_dir, root, found, status)
        if (.not. status%ok()) return
        if (found) buffers(current)%directory = root
        call show_diff_revision_at_line_internal(.false., status)
        if (current /= 0) buffers(current)%directory = saved_dir
    end subroutine show_vc_annotate_changeset_diff_revision_at_line

    !> Annotate the revision described by COUNT or REVISION (of FILE, if
    !> given).
    !>
    !> If COUNT is a positive integer, warp that many revisions forward,
    !> if possible, otherwise echo a warning message.  If COUNT is a
    !> negative integer, warp that many revisions backward, if possible,
    !> otherwise echo a warning message.  If REVISION is given, then it
    !> describes a revision number, so warp to that revision.  Port of
    !> vc-annotate.el:vc-annotate-warp-revision (REVSPEC as COUNT or
    !> REVISION).
    recursive subroutine warp_vc_annotate_revision(status, count, revision, file)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: count
        character(len=*), intent(in), optional :: revision, file
        character(len=:), allocatable :: newrev, the_file, next
        type(handle_type) :: buffer
        integer(int64) :: oldline, lastline, revspec, shortage
        type(vc_annotate_display_mode_type) :: parent_mode
        character(len=21) :: count_text
        logical :: found, saved, has_rev
        integer :: b, k

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call message("Cannot be invoked outside of a vc annotate buffer")
            return
        end if

        buffer = current_buffer()
        parent_mode = vc_annotate_display_mode_type(kind=VC_ANNOTATE_DISPLAY_MODE_COLOR_MAP)
        oldline = line_number_at_pos()
        b = annotate_backend()
        the_file = annotated_file()
        if (present(file)) the_file = file
        has_rev = .false.

        if (present(revision)) then
            newrev = revision
            has_rev = .true.
        else if (.not. present(count)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid argument to vc-annotate-warp-revision")
            return
        else if (count > 0) then
            revspec = count
            newrev = vc_buffer_revision()
            has_rev = .true.

            if (.not. associated(vc_backends(b)%next_revision)) then
                call vc_not_supported(status, "next-revision", b)
                return
            end if

            saved = vc_use_short_revision
            vc_use_short_revision = vc_annotate_use_short_revision

            do while (revspec > 0 .and. has_rev)
                call vc_backends(b)%next_revision(the_file, newrev, next, found)
                has_rev = found
                newrev = next
                if (has_rev) the_file = vc_annotate_file_name_change(the_file, newrev, b)
                revspec = revspec - 1
            end do

            vc_use_short_revision = saved

            if (.not. has_rev) then
                write (count_text, "(i0)") count
                call message("Cannot increment "//trim(count_text)//" revisions from revision " &
                             //vc_buffer_revision())
            end if
        else if (count < 0) then
            revspec = count
            newrev = vc_buffer_revision()
            has_rev = .true.
            saved = vc_use_short_revision
            vc_use_short_revision = vc_annotate_use_short_revision

            do while (revspec < 0 .and. has_rev)
                the_file = vc_annotate_file_name_change(the_file, newrev, b, backward=.true.)
                call previous_revision(b, the_file, newrev, next, found, status)
                if (.not. status%ok()) exit
                has_rev = found
                newrev = next
                revspec = revspec + 1
            end do

            vc_use_short_revision = saved
            if (.not. status%ok()) return

            if (.not. has_rev) then
                write (count_text, "(i0)") - count
                call message("Cannot decrement "//trim(count_text)//" revisions from revision " &
                             //vc_buffer_revision())
            end if
        else
            call set_error(status, FM_GENERIC_ERROR, "Invalid argument to vc-annotate-warp-revision")
            return
        end if

        if (has_rev) then
            ! Pass the current line so that vc-annotate will
            ! place the point in the line.
            call goto_char(point_max())
            shortage = forward_line(-1_int64)
            lastline = line_number_at_pos()
            k = local_index()
            if (k > 0) parent_mode = locals(k)%parent_display_mode
            call vc_annotate(the_file, newrev, status, parent_mode, buffer, min(oldline, lastline), b)
        end if
    end subroutine warp_vc_annotate_revision

    !> Return the name of FILE at revision REV using backend B.
    !> If the file name has changed in the given revision, return the new name;
    !> otherwise, return FILE unchanged.
    !> If BACKWARD, return the name for FILE before REV.  Port of
    !> vc-annotate.el:vc-annotate--file-name-change.
    recursive function vc_annotate_file_name_change(file, rev, b, backward) result(name)
        character(len=*), intent(in) :: file, rev
        integer, intent(in) :: b
        logical, intent(in), optional :: backward
        character(len=:), allocatable :: name
        type(vc_file_name_change_type), allocatable :: changes(:)
        logical :: back
        integer :: k

        name = file
        if (.not. associated(vc_backends(b)%file_name_changes)) return
        back = .false.
        if (present(backward)) back = backward
        call vc_backends(b)%file_name_changes(rev, changes)

        do k = 1, size(changes)
            if (back) then
                ! (car (rassoc file ...))
                if (.not. allocated(changes(k)%new)) cycle
                if (changes(k)%new /= file) cycle
                if (allocated(changes(k)%old)) name = changes(k)%old
                return
            else
                ! (cdr (assoc file ...))
                if (.not. allocated(changes(k)%old)) cycle
                if (changes(k)%old /= file) cycle
                if (allocated(changes(k)%new)) name = changes(k)%new
                return
            end if
        end do
    end function vc_annotate_file_name_change

    ! ------------------------------------------------------------------
    ! Coloring the lines
    ! ------------------------------------------------------------------

    !> Convert the time value TIME (the current time if absent) to a
    !> floating-point number of days.  Port of
    !> vc-annotate.el:vc-annotate-convert-time.
    real(real64) function convert_vc_annotate_time(time) result(days)
        type(time_type), intent(in), optional :: time

        if (present(time)) then
            days = real(time)/86400.0_real64
        else
            days = real(current_time())/86400.0_real64
        end if
    end function convert_vc_annotate_time

    !> Return the time span in days to the next annotation (FOUND false if
    !> the line has none).
    !> This calls the backend function annotate_time, and returns the
    !> difference in days between the time returned and the current time,
    !> or OFFSET if present.  Port of vc-annotate.el:vc-annotate-difference
    !> (with vc-annotate.el:vc-default-annotate-current-time: no backend
    !> has its own `annotate_current_time').
    recursive subroutine vc_annotate_difference(difference, found, offset)
        real(real64), intent(out) :: difference
        logical, intent(out) :: found
        real(real64), intent(in), optional :: offset
        real(real64) :: next_time

        difference = 0
        call get_time_set_line_props(next_time, found)
        if (.not. found) return

        if (present(offset)) then
            difference = offset - next_time
        else
            difference = convert_vc_annotate_time() - next_time
        end if
    end subroutine vc_annotate_difference

    !> Highlight `vc_annotate' output in the current buffer.
    !> RATIO is the expansion that should be applied to `vc_annotate_color_map'.
    !> The annotations are relative to the current time, unless overridden by
    !> OFFSET.  Port of vc-annotate.el:vc-annotate-display.
    recursive subroutine display_vc_annotate(ratio, status, offset)
        real(real64), intent(in) :: ratio
        type(status_type), intent(out) :: status
        real(real64), intent(in), optional :: offset
        type(vc_annotate_color_type), allocatable :: map(:)
        integer :: k

        call make_local_entry(k)

        if (ratio /= 1.0_real64) then
            call vc_annotate_current_color_map(map)
            map%age = map%age*ratio
            locals(k)%color_map = map
            locals(k)%has_color_map = .true.
        end if

        locals(k)%has_offset = present(offset)
        if (present(offset)) locals(k)%offset = offset
        call minor_modes%font_lock_mode%enable(status)
    end subroutine display_vc_annotate

    !> The font-lock matcher of VC-Annotate mode: color the lines from point
    !> to LIMIT by their age; FOUND is always false (there were no matches
    !> for font-lock).  Port of vc-annotate.el:vc-annotate-lines.
    recursive subroutine vc_annotate_lines(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(vc_annotate_color_type), allocatable :: map(:)
        type(named_face_type), pointer :: face
        character(len=:), allocatable :: color
        real(real64) :: difference
        integer(int64) :: start, end, shortage
        logical :: has_difference, read_only
        integer :: k, j

        status = status_type()
        found = .false.
        if (.not. is_annotate_buffer()) return
        call vc_annotate_current_color_map(map)
        k = local_index()

        do while (point() < limit)
            if (k > 0) then
                if (locals(k)%has_offset) then
                    call vc_annotate_difference(difference, has_difference, locals(k)%offset)
                else
                    call vc_annotate_difference(difference, has_difference)
                end if
            else
                call vc_annotate_difference(difference, has_difference)
            end if

            start = point()
            shortage = forward_line(1_int64)
            end = point()

            if (has_difference) then
                j = findloc(map%age >= difference, .true., dim=1)

                if (j > 0) then
                    color = trim(map(j)%color)
                else
                    color = vc_annotate_very_old_color
                end if

                ! Make the face if not done.
                face => vc_annotate_face(color)
                read_only = buffers(current)%read_only
                buffers(current)%read_only = .false.
                call put_text_property(start, end, value_type(name=FACE_PROPERTY), value_type(name=trim(face%name)), &
                                       status)
                buffers(current)%read_only = read_only
                if (.not. status%ok()) return
            end if
        end do

        ! Pretend to font-lock there were no matches.
        found = .false.
    end subroutine vc_annotate_lines

    ! The face vc_annotate_face_COLOR (without a leading `#'), made if
    ! not done: COLOR as background with `vc_annotate_background_mode', else
    ! as foreground with `vc_annotate_background' as background.  Port of
    ! the faces of vc-annotate.el:vc-annotate-lines.
    function vc_annotate_face(color) result(face)
        character(len=*), intent(in) :: color
        type(named_face_type), pointer :: face
        character(len=:), allocatable :: name

        ! substring from index 1 to remove any leading `#' in the name
        name = "vc_annotate_face_"//color

        if (len(color) > 0) then
            if (color(1:1) == "#") name = "vc_annotate_face_"//color(2:)
        end if

        face => face_named(name)
        if (associated(face)) return
        ! A face made at run time lives as long as Fortmacs (make-face).
        allocate (face)
        call face%define(name)
        face%extend = .true.

        if (vc_annotate_background_mode) then
            face%background = color
        else
            face%foreground = color
            if (allocated(vc_annotate_background)) face%background = vc_annotate_background
        end if

        call face%recalc()
    end function vc_annotate_face

    ! ------------------------------------------------------------------
    ! Going to the line
    ! ------------------------------------------------------------------

    !> Go to the line corresponding to the current VC Annotate line.  Port of
    !> vc-annotate.el:vc-annotate-goto-line.
    recursive subroutine goto_vc_annotate_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rev, file, working
        type(handle_type) :: parent, target
        integer(int64) :: line, begv, zv, shortage
        logical :: narrowed, found

        status = status_type()

        if (.not. is_annotate_buffer()) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a VC-Annotate buffer")
            return
        end if

        line = line_number_at_pos(absolute=.true.)
        rev = vc_buffer_revision()
        file = annotated_file()
        parent = vc_parent_buffer()

        if (is_buffer_live(parent)) then
            target = parent
        else if (file_exists(file)) then
            call find_file_noselect(file, target, status)
            if (.not. status%ok()) return
        else
            call set_error(status, FM_GENERIC_ERROR, "File not found: "//file)
            return
        end if

        call pop_to_buffer(target, status)
        if (.not. status%ok()) return
        call set_buffer(target, status)
        if (.not. status%ok()) return
        ! (save-restriction (widen) ...)
        narrowed = is_buffer_narrowed()
        begv = point_min()
        zv = point_max()
        call widen()
        call goto_char(point_min())
        shortage = forward_line(line - 1)
        call recenter(prefix_arg_type(), .false., status)
        if (narrowed) call narrow_to_region(begv, zv, status)
        if (.not. status%ok()) return

        ! Issue a warning if the lines might be incorrect.
        if (buffers(current)%text%save_modiff < buffers(current)%text%modiff) then
            call message("Buffer modified; annotated line numbers may be incorrect")
        else if (vc_state(buffer_file_name()) /= VC_UP_TO_DATE) then
            call message("File is not up-to-date; annotated line numbers may be incorrect")
        else
            call vc_working_revision(buffer_file_name(), working, found)
            if (working /= rev) call message("Annotations were for revision "//rev//"; line numbers may be incorrect")
        end if
    end subroutine goto_vc_annotate_line

    ! The current buffer's visited file ("" for none).  Not a port.
    function buffer_file_name() result(file)
        character(len=:), allocatable :: file

        file = ""
        if (current == 0) return
        if (allocated(buffers(current)%filename)) file = buffers(current)%filename
    end function buffer_file_name

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! C-x v g: read the arguments of `vc_annotate' (the file of the
    ! overriding fileset or the visited file, the revision and span with
    ! a prefix argument).  Port of the interactive form of
    ! vc-annotate.el:vc-annotate.
    recursive subroutine cmd_vc_annotate(status)
        type(status_type), intent(out) :: status
        type(vc_annotate_display_mode_type) :: mode
        type(vc_fileset_type) :: fs
        type(string_type) :: files(1)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: name, def, rev, prompt, answer
        logical :: has, found
        integer :: ios

        old = current_buffer()
        call vc_buffer_overriding_fileset(fs, has)
        if (has) has = size(fs%files) == 1

        if (has) then
            name = fs%files(1)%text
        else
            call vc_ensure_vc_buffer(status)

            if (.not. status%ok()) then
                if (is_buffer_live(old)) call set_buffer(old, st)
                return
            end if

            name = buffer_file_name()
        end if

        def = vc_buffer_revision()

        if (len(def) == 0) then
            if (vc_annotate_use_short_revision) then
                call vc_short_revision(name, def, found)
            else
                call vc_working_revision(name, def, found)
            end if
        end if

        mode = vc_annotate_display_mode

        if (current_prefix_arg%kind == PREFIX_NONE) then
            rev = def
        else
            prompt = "Annotate from revision: "
            if (len(def) > 0) prompt = "Annotate from revision (default "//def//"): "
            files(1)%text = name
            call vc_read_revision(prompt, rev, status, files, default=def)

            if (status%ok()) then
                call read_string("Annotate span days (default 20): ", answer, status, default="20")
            end if

            if (.not. status%ok()) then
                if (is_buffer_live(old)) call set_buffer(old, st)
                return
            end if

            ! (float (string-to-number ...)): 0 when not a number.
            mode = vc_annotate_display_mode_type(kind=VC_ANNOTATE_DISPLAY_MODE_DAYS)
            read (answer, *, iostat=ios) mode%days
            if (ios /= 0) mode%days = 0
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
        call vc_annotate(name, rev, status, mode)
    end subroutine cmd_vc_annotate

    ! (interactive (progn (kill-local-variable 'vc-annotate-color-map) '(1.0))).
    recursive subroutine cmd_display_vc_annotate_default(status)
        type(status_type), intent(out) :: status

        call clear_local_color_map()
        call display_vc_annotate_default(1.0_real64, status)
    end subroutine cmd_display_vc_annotate_default

    ! (interactive "P").
    recursive subroutine cmd_display_vc_annotate_autoscale(status)
        type(status_type), intent(out) :: status

        call display_vc_annotate_autoscale(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_display_vc_annotate_autoscale

    recursive subroutine cmd_display_vc_annotate_select(status)
        type(status_type), intent(out) :: status

        call display_vc_annotate_select(status)
    end subroutine cmd_display_vc_annotate_select

    ! (interactive "p").
    recursive subroutine cmd_previous_vc_annotate_revision(status)
        type(status_type), intent(out) :: status

        call previous_vc_annotate_revision(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_previous_vc_annotate_revision

    ! (interactive "p").
    recursive subroutine cmd_next_vc_annotate_revision(status)
        type(status_type), intent(out) :: status

        call next_vc_annotate_revision(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_next_vc_annotate_revision

    ! `log_view_vc_annotate' of log-view.el's `log_view_annotate_version':
    ! annotate revision REV of FILE with BACKEND.  Not a port.
    recursive subroutine annotate_from_log_view(file, rev, backend, status)
        character(len=*), intent(in) :: file, rev
        integer, intent(in) :: backend
        type(status_type), intent(out) :: status

        call vc_annotate(file, rev, status, backend=backend)
    end subroutine annotate_from_log_view

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define VC-Annotate mode and its keymap, register the commands and
    !> bind C-x v g.  Not a port.
    subroutine init_vc_annotate()
        type(status_type) :: st

        if (associated(major_modes%vc_annotate_mode%execute)) return
        if (.not. allocated(locals)) allocate (locals(0))
        call major_modes%vc_annotate_mode%define("vc_annotate_mode", "Annotate", vc_annotate_mode, &
                                                 vc_annotate_mode_hook, parent=major_modes%special_mode)
        vc_annotate_mode_map = major_modes%vc_annotate_mode%map
        commands%vc_annotate%execute => cmd_vc_annotate
        commands%vc_annotate_mode%execute => vc_annotate_mode
        commands%toggle_vc_annotate_annotation_visibility%execute => toggle_vc_annotate_annotation_visibility
        commands%display_vc_annotate_default%execute => cmd_display_vc_annotate_default
        commands%display_vc_annotate_autoscale%execute => cmd_display_vc_annotate_autoscale
        commands%display_vc_annotate_select%execute => cmd_display_vc_annotate_select
        commands%previous_vc_annotate_revision%execute => cmd_previous_vc_annotate_revision
        commands%next_vc_annotate_revision%execute => cmd_next_vc_annotate_revision
        commands%vc_annotate_working_revision%execute => vc_annotate_working_revision
        commands%vc_annotate_revision_at_line%execute => vc_annotate_revision_at_line
        commands%find_vc_annotate_revision_at_line%execute => find_vc_annotate_revision_at_line
        commands%vc_annotate_revision_previous_to_line%execute => vc_annotate_revision_previous_to_line
        commands%show_vc_annotate_log_revision_at_line%execute => show_vc_annotate_log_revision_at_line
        commands%show_vc_annotate_diff_revision_at_line%execute => show_vc_annotate_diff_revision_at_line
        commands%show_vc_annotate_changeset_diff_revision_at_line%execute => &
            show_vc_annotate_changeset_diff_revision_at_line
        commands%goto_vc_annotate_line%execute => goto_vc_annotate_line
        call keymap_set(vc_annotate_mode_map, "a", "vc_annotate_revision_previous_to_line", st)
        call keymap_set(vc_annotate_mode_map, "d", "show_vc_annotate_diff_revision_at_line", st)
        call keymap_set(vc_annotate_mode_map, "=", "show_vc_annotate_diff_revision_at_line", st)
        call keymap_set(vc_annotate_mode_map, "D", "show_vc_annotate_changeset_diff_revision_at_line", st)
        call keymap_set(vc_annotate_mode_map, "f", "find_vc_annotate_revision_at_line", st)
        call keymap_set(vc_annotate_mode_map, "j", "vc_annotate_revision_at_line", st)
        call keymap_set(vc_annotate_mode_map, "l", "show_vc_annotate_log_revision_at_line", st)
        call keymap_set(vc_annotate_mode_map, "n", "next_vc_annotate_revision", st)
        call keymap_set(vc_annotate_mode_map, "p", "previous_vc_annotate_revision", st)
        call keymap_set(vc_annotate_mode_map, "w", "vc_annotate_working_revision", st)
        call keymap_set(vc_annotate_mode_map, "v", "toggle_vc_annotate_annotation_visibility", st)
        call keymap_set(vc_annotate_mode_map, "RET", "goto_vc_annotate_line", st)
        ! vc-hooks.el's vc-prefix-map: "g" #'vc-annotate.
        call keymap_set(vc_prefix_map, "g", "vc_annotate", st)
        log_view_vc_annotate => annotate_from_log_view
        call append_locals_clearer(clear_annotate_locals)
    end subroutine init_vc_annotate
end module vc_annotate_mod
