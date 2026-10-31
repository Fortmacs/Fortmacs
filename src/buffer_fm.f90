! buffer_fm.f90 -- buffer objects, the buffer registry and the current buffer.
! Ported from GNU Emacs 31.1 src/buffer.c and src/buffer.h.
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

!> Buffers.
!>
!> Each buffer's text is a gap buffer (`buffer_text_type', after
!> buffer.h:struct buffer_text) held in one allocatable string.  Positions
!> are 1-based as upstream (BEG = BEG_BYTE = 1).  Because BEG_BYTE is 1,
!> the string index of byte position `n' is simply
!> `n + (n >= gpt_byte ? gap_size : 0)' (buffer.h:BUF_BYTE_ADDRESS).
!>
!> Buffers live in a registry and are referred to by `handle_type'.
!> Lower layers (marker_fm, insdel_fm) work on the registry slot directly:
!> `buffers(i)' with `i' from `resolve_buffer', and `cur' is the slot of
!> the current buffer.  Everything else should go through the procedures.
!>
!> `kill_buffer' and `erase_buffer' need marker_fm and insdel_fm, which
!> themselves use this module, so they are implemented in the submodule
!> buffer_ops_fm (buffer_ops_fm.f90).
!>
!> Deferred: indirect buffers, buffer-local variables, overlays,
!> file visiting and locking, and hooks.
module buffer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use lisp_fm, only: vtag, V_NULL
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, &
                        FM_BUFFER_READ_ONLY, FM_INVALID_HANDLE
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use lisp_fm, only: value_type, make_symbol_ref
    use lread_fm, only: intern
    use data_fm, only: symbol_value, integer_setting, logical_setting, function_setting
    use intervals_fm, only: intervals_type, find_interval, textget
    use character_fm, only: bytes_by_char_head, raw_prev_char_len, &
                            string_char_and_length
    implicit none(type, external)
    private

    !> buffer.h: BEG, BEG_BYTE.
    integer(int64), parameter, public :: BEG = 1, BEG_BYTE = 1
    !> buffer.h: extra gap to allocate when growing, and the minimum gap.
    integer(int64), parameter, public :: GAP_BYTES_DFL = 2000, GAP_BYTES_MIN = 20
    !> buffer.h:BUF_BYTES_MAX on a 64-bit host (MOST_POSITIVE_FIXNUM - 1).
    integer(int64), parameter, public :: BUF_BYTES_MAX = 2_int64**61 - 2

    !> Port of buffer.h:struct buffer_text.  The anchor NUL bytes upstream
    !> keeps at GPT and Z (for C string searches) are not kept.
    type, public :: buffer_text_type
        !> Text and gap; len(beg) = (z_byte - BEG_BYTE) + gap_size.
        character(len=:), allocatable :: beg
        integer(int64) :: gpt = BEG, z = BEG
        integer(int64) :: gpt_byte = BEG_BYTE, z_byte = BEG_BYTE
        integer(int64) :: gap_size = 0
        integer(int64) :: modiff = 1, chars_modiff = 1, save_modiff = 1
        integer(int64) :: overlay_modiff = 1, compact = 1
        integer(int64) :: beg_unchanged = 0, end_unchanged = 0
        integer(int64) :: unchanged_modified = 1, overlay_unchanged_modified = 1
        logical :: inhibit_shrinking = .false.
        logical :: redisplay = .false.
        !> BUF_MARKERS: first marker slot of this text's marker chain (0 if
        !> none); the chain itself lives in marker_fm.
        integer(int64) :: markers = 0
        !> The text's intervals (text properties); none if `intervals%n' is 0.
        type(intervals_type) :: intervals
    end type buffer_text_type

    !> Port of the parts of buffer.h:struct buffer used so far.
    !> Kinds of `buffer_undo_list' entries (undo.c).
    integer, parameter, public :: UE_BOUNDARY = 0, UE_INSERTION = 1, UE_DELETION = 2, &
                                  UE_POINT = 3, UE_FIRST_CHANGE = 4

    !> One entry of `buffer_undo_list': a boundary; an insertion
    !> (BEG . END); a deletion (TEXT . POSITION), POSITION negative when
    !> point was at the end of the text; point (POSITION, in `beg'); or the
    !> first change since the buffer was unmodified, (t . TIME), with the
    !> visited file's modtime (`beg' seconds, `end' nanoseconds, `known').
    type, public :: undo_entry_type
        integer :: kind = UE_BOUNDARY
        integer(int64) :: beg = 0, end = 0
        logical :: known = .false.
        character(len=:), allocatable :: text
    end type undo_entry_type

    !> An element of a buffer's invisibility spec: `ATOM', or
    !> ATOM with ELLIPSIS (`with_ellipsis') .true. or .false..
    type, public :: invisibility_entry_type
        type(value_type) :: atom
        logical :: cons = .false., ellipsis = .false.
    end type invisibility_entry_type

    !> A part of a header line: TEXT in FACE (empty for the header line's
    !> own face), starting at column ALIGN_TO of the window's text area if
    !> ALIGN_TO is not negative (as a `(space :align-to ALIGN_TO)' display
    !> spec before it would).
    type, public :: header_line_segment_type
        character(len=:), allocatable :: text
        character(len=:), allocatable :: face
        integer :: align_to = -1
    end type header_line_segment_type

    !> A header line: its segments in order.  A buffer shows no header line
    !> while SEGMENTS is unallocated; `add' starts one.
    !> (The %-constructs, `:eval' and the other mode-line-format elements
    !> are Deferred, as for the mode line.)
    type, public :: header_line_format_type
        type(header_line_segment_type), allocatable :: segments(:)
    contains
        !> Append a segment: TEXT, in FACE if present, at column ALIGN_TO
        !> if present.
        procedure :: add => header_line_add
        !> No header line.
        procedure :: clear => header_line_clear
    end type header_line_format_type

    !> A buffer: its text, positions, mark and undo list, file, local
    !> variables and modes.  Port of buffer.h struct buffer (in part).
    type, public :: buffer_type
        character(len=:), allocatable :: name, last_name
        type(buffer_text_type) :: text
        integer(int64) :: pt = BEG, pt_byte = BEG_BYTE
        integer(int64) :: begv = BEG, begv_byte = BEG_BYTE
        integer(int64) :: zv = BEG, zv_byte = BEG_BYTE
        !> BVAR enable_multibyte_characters.
        logical :: multibyte = .true.
        !> BVAR bidi_display_reordering (`bidi_display_reordering'): true
        !> means reorder bidirectional text for display in the visual order.
        logical :: bidi_display_reordering = .true.
        !> BVAR bidi_paragraph_direction (`bidi_paragraph_direction'): 0
        !> to determine each paragraph's direction dynamically, 1
        !> (`left-to-right') or 2 (`right-to-left') to force it (bidi_fm
        !> L2R and R2L).
        integer :: bidi_paragraph_direction = 0
        !> BVAR read_only.
        logical :: read_only = .false.
        !> Upstream sets undo_list to t (disabled) for names starting with SPC.
        logical :: undo_enabled = .true.
        !> BVAR undo_list (`buffer_undo_list'): entries 1 to `undo_count',
        !> the newest last.  A list position k (entries 1 to k, the head
        !> being entry k) stands for a tail of upstream's list.
        type(undo_entry_type), allocatable :: undo_list(:)
        integer(int64) :: undo_count = 0
        !> simple.el undo-equiv-table, keyed by list position (0 to
        !> undo_count): where an undo record leads (simple_fm).
        integer(int64), allocatable :: undo_equiv(:)
        logical :: inhibit_buffer_hooks = .false.
        !> BVAR keymap: the local keymap, or the null handle (keymap_fm).
        type(handle_type) :: keymap
        !> BVAR syntax_table: the syntax table, or the null handle for the
        !> standard one (syntax_fm).
        type(handle_type) :: syntax_table
        !> BVAR downcase_table, upcase_table, case_canon_table and
        !> case_eqv_table: the case table and its companions (char-tables),
        !> or null handles for the standard ones (casetab_fm).
        type(handle_type) :: downcase_table, upcase_table, case_canon_table, case_eqv_table
        !> BVAR category_table: the category table, or the null handle for
        !> the standard one (category_fm).
        type(handle_type) :: category_table
        !> BVAR filename (`buffer_file_name'): the visited file's expanded
        !> name; unallocated when the buffer visits no file.
        character(len=:), allocatable :: filename
        !> BVAR directory (`default_directory'), ending in a slash;
        !> unallocated when unset.
        character(len=:), allocatable :: directory
        !> `buffer_backed_up': a backup was made since the file was visited.
        logical :: backed_up = .false.
        !> `read_only_mode__state' (simple.el): the `buffer_read_only'
        !> value last set by `read_only_mode' (or View mode), restored when
        !> reverting; only when `read_only_mode_state_set' (bound locally).
        logical :: read_only_mode_state = .false., read_only_mode_state_set = .false.
        !> `backup_inhibited' (files.el): true means don't make a backup,
        !> regardless of the other parameters.  Permanent-local; VC sets it
        !> for files under version control.
        logical :: backup_inhibited = .false.
        !> `vc_mode' (vc-hooks.el): the version control status shown in the
        !> mode line (" Git-main"); unallocated when the file is not under
        !> version control.  Permanent-local.
        character(len=:), allocatable :: vc_mode
        !> modtime and modtime_size: the visited file's modification time
        !> and size when last read or written.  `modtime_known' is false for
        !> UNKNOWN_MODTIME_NSECS; a nonexistent file has modtime_nsec -1
        !> (NONEXISTENT_MODTIME_NSECS).
        logical :: modtime_known = .false.
        integer(int64) :: modtime_sec = 0, modtime_size = -1
        integer(int32) :: modtime_nsec = 0
        !> BVAR mark: the mark marker (marker_fm), made on first use; the null
        !> handle until then.
        type(handle_type) :: mark
        !> BVAR mark_active (`mark_active'): True means the mark and region
        !> are currently active in this buffer.
        logical :: mark_active = .false.
        !> simple.el `mark_ring' (buffer-local): former marks, newest first.
        type(handle_type), allocatable :: mark_ring(:)
        !> Unique per buffer ever created, so caches keyed by slot (which is
        !> reused after kill-buffer) can tell buffers apart.
        integer(int64) :: serial = 0
        !> The start position of the last window to stop showing the
        !> buffer (buffer.h last_window_start).
        integer(int64) :: last_window_start = 1
        !> BVAR last_selected_window: the window that last showed the
        !> buffer while selected, or the null handle.
        type(handle_type) :: last_selected_window
        !> BVAR display_count (`buffer_display_count'): a number incremented
        !> each time this buffer is displayed in a window.
        integer(int64) :: display_count = 0
        !> BVAR display_time (`buffer_display_time'): the time when the
        !> buffer was last displayed in a window, in seconds and nanoseconds
        !> since the epoch (unset while DISPLAY_TIME_SET is
        !> false).
        logical :: display_time_set = .false.
        integer(int64) :: display_time_sec = 0
        integer(int32) :: display_time_nsec = 0
        !> buffer.h:local_var_alist (BVAR): the variables local to this buffer (name
        !> ids) and, while the buffer is not current, their values (data_fm
        !> swaps them in and out).
        integer(int64), allocatable :: local_ids(:)
        type(value_type), allocatable :: local_values(:)
        logical, allocatable :: local_bound(:)
        integer :: nlocal = 0
        !> BVAR major_mode (`major_mode'): the major mode's command name.
        character(len=:), allocatable :: major_mode
        !> BVAR header_line_format (`header_line_format'): the header line,
        !> shown at the top of each window that displays the buffer (see
        !> `header_line_format_type').
        type(header_line_format_type) :: header_line_format
        !> `mode_line_buffer_identification': the buffer's own identification
        !> in the mode line, as segments in which "%b" stands for the buffer
        !> name and "%%" for "%" (the ALIGN_TO of a segment is ignored).
        !> While SEGMENTS is unallocated the mode line shows the default,
        !> the name in `mode_line_buffer_id' padded to 12 columns ("%12b").
        type(header_line_format_type) :: mode_line_buffer_identification
        !> BVAR mode_name (`mode_name'): the major mode's name for the mode
        !> line.
        character(len=:), allocatable :: mode_name
        !> BVAR invisibility_spec (`buffer_invisibility_spec'): true for
        !> t, else the list `invisibility_spec'.
        logical :: invisibility_spec_t = .true.
        type(invisibility_entry_type), allocatable :: invisibility_spec(:)
    end type buffer_type

    !> True if Transient Mark mode is enabled.
    !> True also enables highlighting of the region whenever the mark is
    !> active.  The region is highlighted with the `region' face.  Port of
    !> buffer.c transient-mark-mode (on by default, as simple.el makes it).
    type(logical_setting), public :: transient_mark_mode = logical_setting(name="transient_mark_mode", value=.true.)
    !> Whether to deactivate the mark after an editing command.
    !> The command loop sets this to false before each command,
    !> and tests the value when the command returns.
    !> If an editing command sets this true, deactivate the mark after
    !> the command returns.
    !>
    !> Buffer modifications store true in this variable.  Port of keyboard.c
    !> Vdeactivate_mark, the variable `deactivate_mark' (here because
    !> insdel_fm sets it; renamed: the function is simple_fm's
    !> `deactivate_mark').
    logical, public :: deactivate_mark_variable = .false.

    !> The value of `fill_column' that is upstream's unset fill column
    !> (obsolete upstream): no fill column; `current_fill_column' then gives the
    !> largest fixnum.  (A provisional name, for the owner to confirm.)
    !> Not a port.
    integer(int64), parameter, public :: FILL_COLUMN_NONE = -huge(0_int64)

    !> Column beyond which automatic line-wrapping should happen.
    !> It is used by filling commands, such as `fill_region' and `fill_paragraph',
    !> and by `auto_fill_mode', which see.
    !> See also `current_fill_column'.
    !> Interactively, you can set the buffer local value using \[set_fill_column].
    !> Port of buffer.c fill-column (one global value; buffer-local values
    !> come with major and minor modes).
    type(integer_setting), public :: fill_column = integer_setting(name="fill_column", value=70)

    abstract interface
        !> Swap the buffer-local variables when buffer slot `new' becomes
        !> current after slot `old' (0 for none).  Not a port.
        subroutine swap_locals_proc(old, new)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: old, new
        end subroutine swap_locals_proc

        !> An `auto_fill_function'.  Not a port.
        subroutine auto_fill_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine auto_fill_proc
    end interface

    !> Function called (if not null) to perform auto-fill.
    !> It is called after self-inserting any character specified in
    !> the `auto_fill_chars' table.
    !> NOTE: This variable is not a hook;
    !> its value may not be several functions.  Port of buffer.c
    !> auto-fill-function (null when unset; one global value).
    type(function_setting), public :: auto_fill_function = function_setting(name="auto_fill_function")

    !> Swaps the buffer-local variables on a change of current buffer; set
    !> by data_fm (null for none).
    procedure(swap_locals_proc), pointer, public :: swap_buffer_locals_hook => null()

    !> Column for the default `indent_line_function' to indent to.
    !> Linefeed indents to this column in Fundamental mode.  Port of
    !> buffer.c left-margin (one global value).
    type(integer_setting), public :: left_margin = integer_setting(name="left_margin", value=0)

    !> Registry storage; slot `i' is valid while its handle is live.
    type(buffer_type), allocatable, public :: buffers(:)
    !> Slot of the current buffer, 0 before any buffer exists.
    integer(int64), protected, public :: cur = 0

    type(slot_table_type) :: table
    !> Live buffers, least recently selected first (upstream Vbuffer_alist,
    !> most recent first): `record_buffer'ed ones go last, new and buried
    !> ones first.
    type(handle_type), allocatable :: buffer_order(:)
    integer(int64) :: buffer_order_count = 0

    !> The selected frame's buffer lists (frame.h buffer_list and
    !> buried_buffer_list), most recent first: the buffers selected in
    !> the frame, and those buried there.  Fortmacs has one frame.
    type(handle_type), allocatable :: frame_buffers(:), frame_buried(:)
    integer(int64) :: last_serial = 0

    abstract interface
        !> Something to do with buffer `h'.  Not a port.
        recursive subroutine buffer_proc(h)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: h
        end subroutine buffer_proc
    end interface

    !> Called by kill_buffer before it kills a buffer, to have the windows
    !> showing it show another (window.el replace-buffer-in-windows, set by
    !> window_fm; buffer_fm is below the windows).
    procedure(buffer_proc), pointer, public :: replace_buffer_in_windows_hook => null()

    !> Called by kill_buffer to kill the processes of the buffer being
    !> killed (process.c:kill_buffer_processes, set by process_fm).
    procedure(buffer_proc), pointer, public :: kill_buffer_processes_hook => null()

    !> Called by kill_buffer, with the buffer to kill current, to run
    !> `kill_buffer_hook' (set by files_fm, which defines the hook as
    !> files.el does).
    procedure(buffer_proc), pointer, public :: run_kill_buffer_hook => null()

    interface
        !> Kill buffer `h': unchain its markers, free its text, and if it was
        !> current make another buffer current.  Port of the core of
        !> buffer.c:Fkill_buffer.  (In buffer_ops_fm.)
        recursive module subroutine kill_buffer(h, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            type(status_type), intent(out) :: status
        end subroutine kill_buffer

        !> Widen and delete all text of the current buffer.
        !> Port of buffer.c:Ferase_buffer.  (In buffer_ops_fm.)
        module subroutine erase_buffer(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine erase_buffer

        !> Run `buffer_list_update_hook' unless buffer slot I inhibits
        !> buffer hooks.  Port of buffer.c:run_buffer_list_update_hook.
        !> (In buffer_ops_fm.)
        recursive module subroutine run_buffer_list_update_hook(i, status)
            implicit none(type, external)
            integer(int64), intent(in) :: i
            type(status_type), intent(out) :: status
        end subroutine run_buffer_list_update_hook

        !> Return the buffer with buffer-local VARIABLE `equal' to VALUE.
        !> If there is no such live buffer, return the null handle.
        !> See also `find_buffer_visiting'.
        !> Port of buffer.c:Ffind_buffer (the null handle for none).  (In
        !> buffer_ops_fm.)
        module function find_buffer(variable, value) result(h)
            implicit none(type, external)
            type(value_type), intent(in) :: variable, value
            type(handle_type) :: h
        end function find_buffer

        !> Define the commands of buffer.c.  Port of buffer.c:syms_of_buffer
        !> (its commands; its variables are typed settings).  (In
        !> buffer_ops_fm.)
        module subroutine syms_of_buffer()
            implicit none(type, external)
        end subroutine syms_of_buffer
    end interface

    public :: resolve_buffer, is_buffer_live, current_buffer, buffer_handle, auto_fill_proc, swap_locals_proc, &
              set_buffer_internal
    public :: get_buffer, get_buffer_create, generate_new_buffer_name, get_file_buffer
    public :: buffer_name, buffer_last_name, rename_buffer, buffer_list, record_buffer, frame_buffer_list, &
              frame_seen_buffers, bury_buffer_internal, frames_discard_buffer
    public :: set_buffer, kill_buffer, erase_buffer
    public :: is_buffer_modified, restore_buffer_modified, &
              buffer_modified_tick, buffer_chars_modified_tick
    public :: barf_if_buffer_read_only, set_buffer_read_only, inhibit_read_only
    public :: modiff_incr, buf_compute_unchanged
    public :: buf_byte_index, buf_fetch_byte, buf_fetch_char, &
              buf_next_char_len, buf_prev_char_len, buf_bytes
    public :: temp_set_point_both
    public :: nsberror, buffer_enable_undo, set_buffer_modified, force_mode_line_update, buffer_file_name, &
              set_buffer_if_live, run_buffer_list_update_hook, find_buffer, syms_of_buffer
    public :: buffer_serial_live

contains

    !> Append the segment TEXT, in FACE if present, at column ALIGN_TO if
    !> present, to header line SELF.  Not a port.
    subroutine header_line_add(self, text, face, align_to)
        class(header_line_format_type), intent(inout) :: self
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: face
        integer, intent(in), optional :: align_to
        type(header_line_segment_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(self%segments)) allocate (self%segments(0))
        n = size(self%segments)
        allocate (grown(n + 1))

        do k = 1, n
            call move_alloc(self%segments(k)%text, grown(k)%text)
            call move_alloc(self%segments(k)%face, grown(k)%face)
            grown(k)%align_to = self%segments(k)%align_to
        end do

        grown(n + 1)%text = text
        grown(n + 1)%face = ""
        if (present(face)) grown(n + 1)%face = face
        if (present(align_to)) grown(n + 1)%align_to = align_to
        call move_alloc(grown, self%segments)
    end subroutine header_line_add

    !> Clear header line SELF: no header line.  Not a port.
    subroutine header_line_clear(self)
        class(header_line_format_type), intent(inout) :: self

        if (allocated(self%segments)) deallocate (self%segments)
    end subroutine header_line_clear

    ! ------------------------------------------------------------------
    ! Registry
    ! ------------------------------------------------------------------

    !> Slot index of buffer `h', or invalid-handle if it has been killed.
    !> Not a port.
    subroutine resolve_buffer(h, i, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status

        call table%resolve(h, i, status)
    end subroutine resolve_buffer

    !> Port of buffer.c:Fbuffer_live_p.
    logical function is_buffer_live(h)
        type(handle_type), intent(in) :: h

        is_buffer_live = table%is_live(h)
    end function is_buffer_live

    !> Whether a live buffer has serial number SERIAL (for Fortran objects
    !> keeping per-buffer values by serial number).  Not a port.
    logical function buffer_serial_live(serial) result(live)
        integer(int64), intent(in) :: serial
        integer(int64) :: k

        live = .false.
        if (.not. allocated(buffers)) return

        do k = 1, size(buffers, kind=int64)
            if (buffers(k)%serial /= serial) cycle
            live = is_buffer_live(buffer_handle(k))
            return
        end do
    end function buffer_serial_live

    !> Return the current buffer as a buffer handle.  Port of
    !> buffer.c:Fcurrent_buffer (null before any buffer exists).
    function current_buffer() result(h)
        type(handle_type) :: h
        integer(int64) :: k

        h = null_handle()

        do k = 1, buffer_order_count
            if (buffer_order(k)%index == cur) then
                h = buffer_order(k)
                return
            end if
        end do
    end function current_buffer

    !> Handle of the live buffer in slot `i', or the null handle.
    !> Not a port.
    function buffer_handle(i) result(h)
        integer(int64), intent(in) :: i
        type(handle_type) :: h

        h = table%handle_of(i)
    end function buffer_handle

    !> Live buffer named `name', or the null handle.
    !> Port of buffer.c:Fget_buffer (string argument; the names compared
    !> as buffer.c:assoc_ignore_text_properties does).
    function get_buffer(name) result(h)
        character(len=*), intent(in) :: name
        type(handle_type) :: h
        integer(int64) :: k

        h = null_handle()

        do k = 1, buffer_order_count
            if (buffers(buffer_order(k)%index)%name == name .and. &
                len(buffers(buffer_order(k)%index)%name) == len(name)) then
                h = buffer_order(k)
                return
            end if
        end do
    end function get_buffer

    !> True if no live buffer is named `name'.  Not a port.
    logical function name_free(name)
        character(len=*), intent(in) :: name
        type(handle_type) :: h

        h = get_buffer(name)
        name_free = h%index == 0
    end function name_free

    !> Live buffer named `name', created (empty, multibyte) if needed.
    !> The first buffer created becomes current.
    !> Port of buffer.c:Fget_buffer_create (with buffer.c:reset_buffer and
    !> buffer.c:alloc_buffer_text, in part: the fields Fortmacs keeps).
    recursive subroutine get_buffer_create(name, h, status, inhibit_buffer_hooks)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_buffer_hooks
        integer(int64) :: i
        integer :: astat
        type(buffer_type), allocatable :: grown(:)

        h = get_buffer(name)
        if (h%index /= 0) return

        if (len(name) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Empty string for buffer name is not allowed")
            return
        end if

        call table%acquire(h)
        i = h%index

        if (.not. allocated(buffers)) then
            allocate (buffers(table%capacity()))
        else if (size(buffers, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))
            grown(1:size(buffers)) = buffers
            call move_alloc(grown, buffers)
        end if

        ! A fresh slot value resets every field to its default.
        buffers(i) = buffer_type()
        buffers(i)%text%gap_size = 20
        allocate (character(len=buffers(i)%text%gap_size) :: buffers(i)%text%beg, stat=astat)

        if (astat /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Memory exhausted")
            return
        end if

        last_serial = last_serial + 1
        buffers(i)%serial = last_serial
        buffers(i)%name = name
        buffers(i)%last_name = name
        buffers(i)%undo_enabled = name(1:1) /= " "

        ! reset_buffer: a new buffer starts in the current one's directory.
        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) buffers(i)%directory = buffers(cur)%directory
        end if

        if (present(inhibit_buffer_hooks)) buffers(i)%inhibit_buffer_hooks = inhibit_buffer_hooks
        ! Put this in `buffer_order', the live buffers (at its end, the least
        ! recently selected one).
        call buffer_order_append(h)
        buffer_order(2:buffer_order_count) = buffer_order(1:buffer_order_count - 1)
        buffer_order(1) = h
        if (cur == 0) call set_buffer_internal(i)
        call run_buffer_list_update_hook(i, status)
    end subroutine get_buffer_create

    !> The live buffer visiting `filename', or the null handle.  The caller
    !> passes the expanded name (fileio_fm's `expand_file_name', which
    !> depends on this module).  Port of buffer.c:Fget_file_buffer (no file
    !> name handlers).
    function get_file_buffer(filename) result(h)
        character(len=*), intent(in) :: filename
        type(handle_type) :: h
        integer(int64) :: k, i
        type(status_type) :: status

        h = handle_type()

        do k = 1, buffer_order_count
            call resolve_buffer(buffer_order(k), i, status)
            if (.not. status%ok()) cycle
            if (.not. allocated(buffers(i)%filename)) cycle

            if (buffers(i)%filename == filename .and. &
                len(buffers(i)%filename) == len(filename)) then
                h = buffer_order(k)
                return
            end if
        end do
    end function get_file_buffer

    !> A name, based on `name', that no live buffer has.  `ignore' is a name
    !> that may be returned even if taken.
    !> Port of buffer.c:Fgenerate_new_buffer_name.
    function generate_new_buffer_name(name, ignore) result(newname)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: ignore
        character(len=:), allocatable :: newname
        character(len=:), allocatable :: genbase
        character(len=24) :: num
        integer(int64) :: count
        real :: r
        logical :: hidden

        if (present(ignore)) then
            if (same(name, ignore)) then
                newname = name
                return
            end if
        end if

        if (name_free(name)) then
            newname = name
            return
        end if

        hidden = .false.
        if (len(name) > 0) hidden = name(1:1) == " "

        if (.not. hidden) then
            genbase = name
        else
            ! Hidden buffers get a random suffix first, to find a free name fast.
            call random_number(r)
            write (num, "(i0)") int(r*1000000)
            genbase = name//"-"//trim(num)

            if (name_free(genbase)) then
                newname = genbase
                return
            end if
        end if

        count = 2

        do
            write (num, "(i0)") count
            newname = genbase//"<"//trim(num)//">"

            if (present(ignore)) then
                if (same(newname, ignore)) return
            end if

            if (name_free(newname)) return
            count = count + 1
        end do

    contains

        pure logical function same(a, b)
            character(len=*), intent(in) :: a, b

            same = len(a) == len(b) .and. a == b
        end function same
    end function generate_new_buffer_name

    !> Return the name of BUFFER (handle `h'), as a string.  Port of
    !> buffer.c:Fbuffer_name (a killed buffer is an error, where upstream
    !> returns no name).
    subroutine buffer_name(h, name, status)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        name = buffers(i)%name
    end subroutine buffer_name

    !> Port of buffer.c:Fbuffer_last_name.
    subroutine buffer_last_name(h, name, status)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        name = buffers(i)%last_name
    end subroutine buffer_last_name

    !> Rename the current buffer to `newname'; with `unique', pick a free
    !> variant instead of failing.  `actual' receives the name given.
    !> Port of buffer.c:Frename_buffer.
    recursive subroutine rename_buffer(newname, unique, actual, status)
        character(len=*), intent(in) :: newname
        logical, intent(in) :: unique
        character(len=:), allocatable, intent(out) :: actual
        type(status_type), intent(out) :: status
        type(handle_type) :: tem
        character(len=:), allocatable :: oldname

        if (cur == 0) then
            call set_error(status, FM_INVALID_HANDLE)
            return
        end if

        if (len(newname) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Empty string is invalid as a buffer name")
            return
        end if

        oldname = buffers(cur)%name
        actual = newname
        tem = get_buffer(newname)

        if (tem%index /= 0) then
            if (.not. unique .and. tem%index == cur) then
                actual = oldname
                return
            end if

            if (unique) then
                actual = generate_new_buffer_name(newname, oldname)
            else
                call set_error(status, FM_GENERIC_ERROR, format_message("Buffer name `")//newname//format_message("' is in use"))
                return
            end if
        end if

        buffers(cur)%last_name = oldname
        buffers(cur)%name = actual
        call run_buffer_list_update_hook(cur, status)
    end subroutine rename_buffer

    !> Return a list of all live buffers, most recently selected first.
    !> Port of buffer.c:Fbuffer_list (FRAME absent).
    function buffer_list() result(list)
        type(handle_type), allocatable :: list(:)

        if (buffer_order_count == 0) then
            allocate (list(0))
        else
            list = buffer_order(buffer_order_count:1:-1)
        end if
    end function buffer_list

    !> Return the buffer list in the proper order for the selected frame:
    !> the buffers shown in the frame come first, followed by the rest of
    !> the buffers, the ones buried there last.  Port of
    !> buffer.c:Fbuffer_list (FRAME a frame).
    function frame_buffer_list() result(list)
        type(handle_type), allocatable :: list(:)
        type(handle_type), allocatable :: general(:)
        integer :: n, k

        general = buffer_list()
        allocate (list(size(general)))
        n = 0

        if (allocated(frame_buffers)) then
            do k = 1, size(frame_buffers)
                n = n + 1
                list(n) = frame_buffers(k)
            end do
        end if

        do k = 1, size(general)
            if (in_list(frame_buffers, general(k))) cycle
            if (in_list(frame_buried, general(k))) cycle
            n = n + 1
            list(n) = general(k)
        end do

        if (allocated(frame_buried)) then
            do k = size(frame_buried), 1, -1
                n = n + 1
                list(n) = frame_buried(k)
            end do
        end if
    end function frame_buffer_list

    !> The buffers selected in the frame, most recent first (frame.h
    !> buffer_list).  Not a port.
    function frame_seen_buffers() result(list)
        type(handle_type), allocatable :: list(:)

        if (allocated(frame_buffers)) then
            list = frame_buffers
        else
            allocate (list(0))
        end if
    end function frame_seen_buffers

    !> Whether `h' is in `list'.  Not a port.
    logical function in_list(list, h)
        type(handle_type), allocatable, intent(in) :: list(:)
        type(handle_type), intent(in) :: h
        integer :: k

        in_list = .false.
        if (.not. allocated(list)) return

        do k = 1, size(list)
            if (list(k)%index == h%index .and. list(k)%generation == h%generation) then
                in_list = .true.
                return
            end if
        end do
    end function in_list

    !> `list' without `h'.  Not a port.
    subroutine list_delq(list, h)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: h
        type(handle_type), allocatable :: kept(:)
        integer :: k, n

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)

        do k = 1, n
            if (list(k)%index == h%index .and. list(k)%generation == h%generation) then
                allocate (kept(n - 1))
                kept(1:k - 1) = list(1:k - 1)
                kept(k:n - 1) = list(k + 1:n)
                call move_alloc(kept, list)
                return
            end if
        end do
    end subroutine list_delq

    !> Move H to the front of LIST (removing it elsewhere).  Not a port.
    subroutine list_push(list, h)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: h
        type(handle_type), allocatable :: grown(:)

        call list_delq(list, h)
        allocate (grown(size(list) + 1))
        grown(1) = h
        grown(2:) = list
        call move_alloc(grown, list)
    end subroutine list_push

    !> Move BUFFER to the end of the buffer list.  Port of
    !> buffer.c:Fbury_buffer_internal.
    recursive subroutine bury_buffer_internal(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: k
        type(status_type) :: status

        do k = 1, buffer_order_count
            if (buffer_order(k)%index == h%index .and. buffer_order(k)%generation == h%generation) then
                buffer_order(2:k) = buffer_order(1:k - 1)
                buffer_order(1) = h
                exit
            end if
        end do

        ! Update buffer lists of selected frame.
        call list_delq(frame_buffers, h)
        call list_push(frame_buried, h)
        call run_buffer_list_update_hook(h%index, status)
    end subroutine bury_buffer_internal

    !> Drop the killed buffer `h' from the frame's buffer lists.  Port of
    !> frame.c:frames_discard_buffer.
    subroutine frames_discard_buffer(h)
        type(handle_type), intent(in) :: h

        call list_delq(frame_buffers, h)
        call list_delq(frame_buried, h)
    end subroutine frames_discard_buffer

    !> Make `h' the current buffer.  Port of buffer.c:Fset_buffer (also
    !> buffer.c:restore_buffer, the same as an unwind function).
    subroutine set_buffer(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_buffer(h, i, status)

        if (.not. status%ok()) then
            status%detail = "Selecting deleted buffer"
            return
        end if

        call set_buffer_internal(i)
    end subroutine set_buffer

    !> Make buffer slot `i' (0 for none) current, swapping the buffer-local
    !> variables of the old and new current buffers.  Port of
    !> buffer.c:set_buffer_internal_2 and buffer.c:set_buffer_internal_1
    !> (no markers of indirect buffers to record or fetch).
    subroutine set_buffer_internal(i)
        integer(int64), intent(in) :: i

        if (i == cur) return
        if (associated(swap_buffer_locals_hook)) call swap_buffer_locals_hook(cur, i)
        cur = i
    end subroutine set_buffer_internal

    !> Move buffer `h' to the most recently selected end of the buffer
    !> list.  Port of buffer.c:record_buffer.
    recursive subroutine record_buffer(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: k, j
        type(status_type) :: status

        do k = 1, buffer_order_count
            if (buffer_order(k)%index == h%index .and. buffer_order(k)%generation == h%generation) then
                do j = k, buffer_order_count - 1
                    buffer_order(j) = buffer_order(j + 1)
                end do

                buffer_order(buffer_order_count) = h
                exit
            end if
        end do

        ! Update buffer list of selected frame.
        call list_push(frame_buffers, h)
        call list_delq(frame_buried, h)
        call run_buffer_list_update_hook(h%index, status)
    end subroutine record_buffer

    ! Not a port.
    subroutine buffer_order_append(h)
        type(handle_type), intent(in) :: h
        type(handle_type), allocatable :: grown(:)

        if (.not. allocated(buffer_order)) allocate (buffer_order(16))

        if (buffer_order_count == size(buffer_order, kind=int64)) then
            allocate (grown(2*size(buffer_order)))
            grown(1:buffer_order_count) = buffer_order(1:buffer_order_count)
            call move_alloc(grown, buffer_order)
        end if

        buffer_order_count = buffer_order_count + 1
        buffer_order(buffer_order_count) = h
    end subroutine buffer_order_append

    ! ------------------------------------------------------------------
    ! Modification state
    ! ------------------------------------------------------------------

    !> Advance modification counter `a' logarithmically in `len'; `old'
    !> receives its previous value.  Port of lisp.h:modiff_incr.
    pure subroutine modiff_incr(a, len, old)
        integer(int64), intent(inout) :: a
        integer(int64), intent(in) :: len
        integer(int64), intent(out), optional :: old

        if (present(old)) old = a

        if (len == 0) then
            a = a + 1
        else
            ! elogb (len) + 1, i.e. the bit length of len.
            a = a + (bit_size(len) - leadz(len))
        end if
    end subroutine modiff_incr

    !> Port of buffer.h:BUF_COMPUTE_UNCHANGED.
    pure subroutine buf_compute_unchanged(b, start, end)
        type(buffer_type), intent(inout) :: b
        integer(int64), intent(in) :: start, end

        associate (t => b%text)
            if (t%unchanged_modified == t%modiff .and. &
                t%overlay_unchanged_modified == t%overlay_modiff) then
                t%beg_unchanged = start - BEG
                t%end_unchanged = t%z - end
            else
                if (t%z - end < t%end_unchanged) t%end_unchanged = t%z - end
                if (start - BEG < t%beg_unchanged) t%beg_unchanged = start - BEG
            end if
        end associate
    end subroutine buf_compute_unchanged

    !> Port of buffer.c:Fbuffer_modified_p (the `autosaved' state is not
    !> tracked yet).
    subroutine is_buffer_modified(h, modified, status)
        type(handle_type), intent(in) :: h
        logical, intent(out) :: modified
        type(status_type), intent(out) :: status
        integer(int64) :: i

        modified = .false.
        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        modified = buffers(i)%text%save_modiff < buffers(i)%text%modiff
    end subroutine is_buffer_modified

    !> Mark the current buffer modified or unmodified.
    !> Port of buffer.c:Frestore_buffer_modified_p (also the behavior of
    !> Fset_buffer_modified_p, minus mode-line updates and file locks).
    subroutine restore_buffer_modified(flag)
        logical, intent(in) :: flag

        if (cur == 0) return

        associate (t => buffers(cur)%text)
            if (.not. flag) then
                t%save_modiff = t%modiff
            else if (t%save_modiff >= t%modiff) then
                call modiff_incr(t%modiff, 1_int64, t%save_modiff)
            end if
        end associate
    end subroutine restore_buffer_modified

    !> Port of buffer.c:Fbuffer_modified_tick.
    subroutine buffer_modified_tick(h, tick, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: tick
        type(status_type), intent(out) :: status
        integer(int64) :: i

        tick = 0
        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        tick = buffers(i)%text%modiff
    end subroutine buffer_modified_tick

    !> Port of buffer.c:Fbuffer_chars_modified_tick.
    subroutine buffer_chars_modified_tick(h, tick, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: tick
        type(status_type), intent(out) :: status
        integer(int64) :: i

        tick = 0
        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        tick = buffers(i)%text%chars_modiff
    end subroutine buffer_chars_modified_tick

    !> Value of the variable `inhibit_read_only' (.false. when it has no value).
    !> Port of buffer.c:Vinhibit_read_only (global value only).
    function inhibit_read_only() result(v)
        type(value_type) :: v
        type(status_type) :: st

        call symbol_value(make_symbol_ref(intern("inhibit_read_only")), v, st)
    end function inhibit_read_only

    !> Fail with buffer-read-only if the current buffer is read-only,
    !> unless `inhibit_read_only' is .true. or the text at `position'
    !> (default: point) has an `inhibit_read_only' text property that is not .false..
    !> Port of buffer.c:Fbarf_if_buffer_read_only.
    subroutine barf_if_buffer_read_only(status, position)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: position
        integer(int64) :: pos, k
        type(value_type) :: props

        if (cur == 0) return
        if (.not. buffers(cur)%read_only) return
        if (vtag(inhibit_read_only()) /= V_NULL) return
        pos = buffers(cur)%pt
        if (present(position)) pos = position

        associate (b => buffers(cur), iv => buffers(cur)%text%intervals)
            ! Fget_text_property (POSITION, Qinhibit_read_only, Qnil)
            if (iv%n > 0 .and. b%begv <= pos .and. pos < b%zv) then
                k = find_interval(iv, pos)
                props = iv%runs(k)%plist
                if (vtag(textget(props, make_symbol_ref(intern("inhibit_read_only")))) /= V_NULL) return
            end if
        end associate

        call set_error(status, FM_BUFFER_READ_ONLY, buffers(cur)%name)
    end subroutine barf_if_buffer_read_only

    !> Set BVAR read_only of buffer `h' (the `buffer_read_only' variable).
    !> Not a port.
    subroutine set_buffer_read_only(h, flag, status)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: flag
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        buffers(i)%read_only = flag
    end subroutine set_buffer_read_only

    ! ------------------------------------------------------------------
    ! Text access (buffer.h macros)
    ! ------------------------------------------------------------------

    !> String index of byte position `bytepos'.
    !> Port of buffer.h:BUF_BYTE_ADDRESS.
    pure integer(int64) function buf_byte_index(b, bytepos)
        type(buffer_type), intent(in) :: b
        integer(int64), intent(in) :: bytepos

        buf_byte_index = bytepos
        if (bytepos >= b%text%gpt_byte) buf_byte_index = bytepos + b%text%gap_size
    end function buf_byte_index

    !> Byte (0..255) at `bytepos'.  Port of buffer.h:BUF_FETCH_BYTE.
    pure integer(int32) function buf_fetch_byte(b, bytepos)
        type(buffer_type), intent(in) :: b
        integer(int64), intent(in) :: bytepos
        integer(int64) :: k

        k = buf_byte_index(b, bytepos)
        buf_fetch_byte = ichar(b%text%beg(k:k), kind=int32)
    end function buf_fetch_byte

    !> Character at `bytepos'.  Port of buffer.h:BUF_FETCH_CHAR.
    pure integer(int32) function buf_fetch_char(b, bytepos)
        type(buffer_type), intent(in) :: b
        integer(int64), intent(in) :: bytepos
        integer(int32) :: n

        if (b%multibyte) then
            call string_char_and_length(b%text%beg, buf_byte_index(b, bytepos), buf_fetch_char, n)
        else
            buf_fetch_char = buf_fetch_byte(b, bytepos)
        end if
    end function buf_fetch_char

    !> Byte length of the character at `bytepos'.
    !> Port of buffer.h:buf_next_char_len.
    pure integer(int32) function buf_next_char_len(b, bytepos)
        type(buffer_type), intent(in) :: b
        integer(int64), intent(in) :: bytepos

        buf_next_char_len = bytes_by_char_head(buf_fetch_byte(b, bytepos))
    end function buf_next_char_len

    !> Byte length of the character before `bytepos'.
    !> Port of buffer.h:buf_prev_char_len.
    pure integer(int32) function buf_prev_char_len(b, bytepos)
        type(buffer_type), intent(in) :: b
        integer(int64), intent(in) :: bytepos
        integer(int64) :: k

        k = bytepos
        if (bytepos > b%text%gpt_byte) k = bytepos + b%text%gap_size
        buf_prev_char_len = raw_prev_char_len(b%text%beg, k)
    end function buf_prev_char_len

    !> The bytes from `from_byte' up to (not including) `to_byte', joined
    !> across the gap.  Not a port; the byte-copy core of
    !> editfns.c:make_buffer_string_both.
    pure function buf_bytes(b, from_byte, to_byte) result(s)
        type(buffer_type), intent(in) :: b
        integer(int64), intent(in) :: from_byte, to_byte
        character(len=:), allocatable :: s
        integer(int64) :: g, n

        associate (t => b%text)
            g = t%gpt_byte
            n = max(0_int64, to_byte - from_byte)
            allocate (character(len=n) :: s)
            if (n == 0) return

            if (to_byte <= g) then
                s = t%beg(from_byte:to_byte - 1)
            else if (from_byte >= g) then
                s = t%beg(from_byte + t%gap_size:to_byte - 1 + t%gap_size)
            else
                s = t%beg(from_byte:g - 1)//t%beg(g + t%gap_size:to_byte - 1 + t%gap_size)
            end if
        end associate
    end function buf_bytes

    !> Set point of `b' without any text-property processing.
    !> Port of intervals.c:temp_set_point_both.
    pure subroutine temp_set_point_both(b, charpos, bytepos)
        type(buffer_type), intent(inout) :: b
        integer(int64), intent(in) :: charpos, bytepos

        b%pt = charpos
        b%pt_byte = bytepos
    end subroutine temp_set_point_both

    ! ------------------------------------------------------------------
    ! More of buffer.c
    ! ------------------------------------------------------------------

    !> Signal the error for a buffer argument that names no live buffer:
    !> "No such buffer NAME" if NAME is present, else "Invalid buffer
    !> argument".  Port of buffer.c:nsberror.
    subroutine nsberror(status, name)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name

        if (present(name)) then
            call set_error(status, FM_GENERIC_ERROR, "No such buffer "//name)
        else
            call set_error(status, FM_GENERIC_ERROR, "Invalid buffer argument")
        end if
    end subroutine nsberror

    !> Start keeping undo information for buffer BUFFER.
    !> An absent argument means do this for the current buffer.
    !> Port of buffer.c:Fbuffer_enable_undo (BUFFER a buffer; a buffer
    !> name is looked up with `get_buffer' by the command).
    subroutine buffer_enable_undo(status, buffer)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: i

        i = cur

        if (present(buffer)) then
            call resolve_buffer(buffer, i, status)

            if (.not. status%ok()) then
                call nsberror(status)
                return
            end if
        end if

        if (i == 0) return

        if (.not. buffers(i)%undo_enabled) then
            buffers(i)%undo_enabled = .true.
            buffers(i)%undo_count = 0
        end if
    end subroutine buffer_enable_undo

    !> Mark current buffer as modified or unmodified according to FLAG.
    !> A true FLAG means mark the buffer modified.
    !> In addition, this function unconditionally forces redisplay of the
    !> mode lines of the windows that display the current buffer, and also
    !> locks or unlocks the file visited by the buffer, depending on whether
    !> the function's argument is true, but only if both `buffer_file_name'
    !> and `buffer_file_truename' are true.
    !> Port of buffer.c:Fset_buffer_modified_p (file locking is deferred,
    !> as in `restore_buffer_modified').
    subroutine set_buffer_modified(flag)
        logical, intent(in) :: flag

        call restore_buffer_modified(flag)
        call force_mode_line_update(.false.)
    end subroutine set_buffer_modified

    !> Force redisplay of the current buffer's mode line and header line.
    !> With optional true ALL, force redisplay of all mode lines, tab lines and
    !> header lines.  This function also forces recomputation of the
    !> menu bar menus and the frame title.
    !> Port of buffer.c:Fforce_mode_line_update.  Redisplay (xdisp_fm)
    !> recomputes every mode line and header line each time, so there is
    !> no update_mode_lines flag to set: the next redisplay shows them.
    subroutine force_mode_line_update(all)
        logical, intent(in) :: all

        if (all) continue
    end subroutine force_mode_line_update

    !> Return absolute name of file BUFFER is visiting, or FOUND .false. if none.
    !> An absent argument means use the current buffer.
    !> Port of buffer.c:Fbuffer_file_name (FOUND .false. for none).
    subroutine buffer_file_name(filename, found, status, buffer)
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: i

        found = .false.
        filename = ""
        i = cur

        if (present(buffer)) then
            call resolve_buffer(buffer, i, status)
            if (.not. status%ok()) return
        end if

        if (i == 0) return
        if (.not. allocated(buffers(i)%filename)) return
        filename = buffers(i)%filename
        found = .true.
    end subroutine buffer_file_name

    !> Make buffer H current if it is live; do nothing otherwise.
    !> Port of buffer.c:set_buffer_if_live.
    subroutine set_buffer_if_live(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: i
        type(status_type) :: status

        call resolve_buffer(h, i, status)
        if (status%ok()) call set_buffer_internal(i)
    end subroutine set_buffer_if_live
end module buffer_fm
