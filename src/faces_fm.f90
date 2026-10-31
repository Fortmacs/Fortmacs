! faces_fm.f90 -- Lisp faces: their attributes and specs.
! Ported from GNU Emacs 31.1 lisp/faces.el and the face attribute part of
! src/xfaces.c.
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

!> Faces.  A face is a `named_face_type' object named after the Emacs
!> face (`region', `mode_line', `font_lock_comment_face'); the
!> configuration reaches every face through `faces' (face_table_fm,
!> generated).  Its components are the face's attributes as the
!> configuration sets them (upstream's set-face-attribute on all
!> frames): an attribute left alone is unspecified, and the face's spec
!> (its defface) decides it.  For example:
!>
!>     faces%region%background = "dark slate gray"
!>     faces%bold%weight = "extra-bold"
!>     faces%mode_line%inverse_video = .true.
!>     faces%link%underline = .false.
!>     faces%error%underline = face_underline(color="red", style=FACE_STYLE_WAVE)
!>     faces%mode_line%box = face_box(line_width=-1, style=FACE_BOX_RELEASED_BUTTON)
!>     faces%help_key_binding%inherit = "fixed_pitch"
!>
!> `face_spec_recalc' works out the attributes a face has on the frame
!> (its `lface'): the spec alternative that matches the display, then
!> the attributes set in the configuration.  xfaces_fm merges them and
!> realizes faces for the terminal.
!>
!> Deferred: themes and Customize, frame-local `set_face_attribute',
!> X resources, face aliases, `face_override_spec', stipples, fonts and
!> fontsets, and
!> the `cursor' face's face-no-inherit property (no face may inherit
!> from it; xfaces.c).
module faces_fm
    use, intrinsic :: iso_fortran_env, only: real64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use tty_colors_fm, only: tty_color_values
    implicit none(type, external)
    private

    public :: face_flag_type, face_line_type, face_box_type, face_height_type, face_inherit_type
    public :: face_attributes_type, named_face_type, face_display_type
    public :: face_attrs, face_underline, face_box, display_when, face_named, face_count, face_at, face_spec_recalc_all
    public :: set_frame_display, faces_equal, face_weight_numeric, face_slant_numeric
    public :: init_faces
    public :: color_values, is_color_dark, readable_foreground_color

    !> The relative luminance below which a color is considered "dark".
    !> A "dark" color in this sense provides better contrast with white
    !> than with black; see `is_color_dark'.  This value was determined
    !> experimentally.  Port of faces.el color-luminance-dark-limit.
    real(real64), parameter, public :: COLOR_LUMINANCE_DARK_LIMIT = 0.325_real64

    !> States of a face attribute: unspecified, .false., .true., a color,
    !> typed attributes, an array (of face names), a value (number).
    integer, parameter, public :: FACE_UNSPECIFIED = 0, FACE_OFF = 1, FACE_ON = 2, FACE_COLOR = 3, &
                                  FACE_PLIST = 4, FACE_LIST = 5, FACE_VALUE = 6

    !> Underline styles (`style' of `face_underline').
    integer, parameter, public :: FACE_STYLE_LINE = 1, FACE_STYLE_DOUBLE_LINE = 2, FACE_STYLE_WAVE = 3, &
                                  FACE_STYLE_DOTS = 4, FACE_STYLE_DASHES = 5

    !> Box styles (`style' of `face_box'): 3D buttons, and a
    !> flat button (a plain box in the background color).
    integer, parameter, public :: FACE_BOX_RELEASED_BUTTON = 1, FACE_BOX_PRESSED_BUTTON = 2, FACE_BOX_FLAT_BUTTON = 3

    !> Display classes of a frame (the `display_type' frame parameter).
    integer, parameter, public :: FACE_CLASS_COLOR = 1, FACE_CLASS_GRAYSCALE = 2, FACE_CLASS_MONO = 3
    !> Background modes of a frame (the `background_mode' frame parameter).
    integer, parameter, public :: FACE_BACKGROUND_LIGHT = 1, FACE_BACKGROUND_DARK = 2

    !> An attribute that is unspecified, .false. or .true. (`:inverse_video',
    !> `:extend').  Assign a logical to set it.
    type :: face_flag_type
        integer :: state = FACE_UNSPECIFIED
    contains
        procedure, private :: logical_to_flag
        generic :: assignment(=) => logical_to_flag
        !> Make the attribute unspecified again.
        procedure :: unspecify => flag_unspecify
    end type face_flag_type

    !> `:underline', `:overline' or `:strike_through': unspecified, .false., .true.,
    !> a color, or (for underlines) a color and a style.  Assign a
    !> logical, a color name, or for an underline the value of
    !> `face_underline(color=..., style=..., position=...)'.  `color' empty with `style' set
    !> means the foreground color (`:color foreground-color').
    type :: face_line_type
        integer :: state = FACE_UNSPECIFIED
        character(len=:), allocatable :: color
        integer :: style = FACE_STYLE_LINE
        !> `:position' of an underline (ignored on a terminal).
        logical :: position = .false.
    contains
        procedure, private :: logical_to_line, color_to_line
        generic :: assignment(=) => logical_to_line, color_to_line
        procedure :: unspecify => line_unspecify
    end type face_line_type

    !> `:box': unspecified, .false., or a box (a terminal draws none).  Assign
    !> a logical, a color name, or the value of `face_box(...)'.
    type :: face_box_type
        integer :: state = FACE_UNSPECIFIED
        integer :: line_width = 1, line_height = 1
        character(len=:), allocatable :: color
        character(len=:), allocatable :: style
    contains
        procedure, private :: logical_to_box, color_to_box
        generic :: assignment(=) => logical_to_box, color_to_box
        procedure :: unspecify => box_unspecify
    end type face_box_type

    !> `:height': unspecified, an absolute height in tenths of a point
    !> (assign an integer) or a factor of the underlying height (assign a
    !> real).
    type :: face_height_type
        integer :: state = FACE_UNSPECIFIED
        integer :: absolute = 0
        real :: factor = 1.0
    contains
        procedure, private :: integer_to_height, real_to_height
        generic :: assignment(=) => integer_to_height, real_to_height
        procedure :: unspecify => height_unspecify
    end type face_height_type

    !> `:inherit': unspecified, none, or the faces inherited from (their
    !> names, the first taking precedence).  Assign a name or an array of
    !> names.
    type :: face_inherit_type
        integer :: state = FACE_UNSPECIFIED
        character(len=64), allocatable :: names(:)
    contains
        procedure, private :: name_to_inherit, names_to_inherit
        generic :: assignment(=) => name_to_inherit, names_to_inherit
        procedure :: unspecify => inherit_unspecify
    end type face_inherit_type

    !> Face attributes (xfaces.c's Lisp face attribute vector, the parts
    !> a terminal needs and those defface specs use).  A character
    !> attribute is unspecified while unallocated; weight, slant and
    !> width are names ("bold", "italic", "condensed").
    type :: face_attributes_type
        character(len=:), allocatable :: family, foundry
        character(len=:), allocatable :: width
        type(face_height_type) :: height
        character(len=:), allocatable :: weight, slant
        type(face_line_type) :: underline, overline, strike_through
        type(face_box_type) :: box
        type(face_flag_type) :: inverse_video
        character(len=:), allocatable :: foreground, distant_foreground, background
        type(face_inherit_type) :: inherit
        type(face_flag_type) :: extend
    end type face_attributes_type

    !> When a spec alternative applies (a defface DISPLAY): `always' for
    !> t; otherwise every condition given must hold.  `classes' and
    !> `backgrounds' are FACE_CLASS_* and FACE_BACKGROUND_* lists,
    !> `types' window system types ("tty", "graphic", "x", "pc", ...),
    !> `min_colors' 0 for no condition.  `is_default' marks a `default'
    !> entry, whose attributes underlie the alternative chosen.
    type :: face_display_type
        logical :: always = .false.
        logical :: is_default = .false.
        integer, allocatable :: classes(:), backgrounds(:)
        character(len=16), allocatable :: types(:)
        integer :: min_colors = 0
        !> A `supports' condition: the attributes `supports_attrs' must be
        !> supported by the display (display-supports-face-attributes-p).
        logical :: supports = .false.
    end type face_display_type

    !> One alternative of a face spec; `supports_attrs' the attributes of a
    !> `supports' condition.
    type :: face_spec_entry_type
        type(face_display_type) :: display
        type(face_attributes_type) :: supports_attrs
        type(face_attributes_type) :: attrs
    end type face_spec_entry_type

    !> Whether the display supports face attributes ATTRS (the terminal's
    !> part of display-supports-face-attributes-p, installed by xfaces_fm).
    abstract interface
        ! Not a port.
        logical function supports_function(attrs)
            import :: face_attributes_type
            implicit none(type, external)
            type(face_attributes_type), intent(in) :: attrs
        end function supports_function
    end interface

    !> The function testing a `supports' condition; null: never supported.
    procedure(supports_function), pointer, public :: display_supports_face_attributes_p => null()

    !> A face.  The components inherited from face_attributes_type are the
    !> attributes the configuration gives the face; `lface' is what it has
    !> on the frame.  Port of what faces.el:defface (with
    !> custom-declare-face and face-spec-set) defines.
    type, extends(face_attributes_type) :: named_face_type
        !> The face's name, as `C-h v' and face references show it.
        character(len=64) :: name = ""
        !> The defface spec.
        type(face_spec_entry_type), allocatable :: spec(:)
        !> The attributes on the frame.
        type(face_attributes_type) :: lface
        !> The face's documentation (`face_documentation').
        character(len=:), allocatable :: doc
    contains
        !> Make the face known by NAME with documentation DOC.
        procedure :: define => face_define
        !> Add an alternative to the face's spec.
        procedure :: spec_entry => face_spec_entry
        !> Recompute the face's attributes on the frame.
        procedure :: recalc => face_spec_recalc
    end type named_face_type

    !> A pointer to a face, for the registry.
    type :: face_ref_type
        type(named_face_type), pointer :: p => null()
    end type face_ref_type

    !> The defined faces, in order of definition (face-list).
    type(face_ref_type), allocatable :: registry(:)

    !> The frame's display (frame parameters `display_type' and
    !> `background_mode', display-color-cells, window-system type).
    integer :: frame_class = FACE_CLASS_MONO
    integer :: frame_background = FACE_BACKGROUND_DARK
    integer :: frame_colors = 0
    character(len=16) :: frame_type = "tty"

    !> Basic default face.  Port of faces.el:default.
    type(named_face_type), target, public :: default
    !> Basic bold face.  Port of faces.el:bold.
    type(named_face_type), target, public :: bold
    !> Basic italic face.  Port of faces.el:italic.
    type(named_face_type), target, public :: italic
    !> Basic bold-italic face.  Port of faces.el:bold-italic.
    type(named_face_type), target, public :: bold_italic
    !> Basic underlined face.  Port of faces.el:underline.
    type(named_face_type), target, public :: underline
    !> The basic fixed-pitch face.  Port of faces.el:fixed-pitch.
    type(named_face_type), target, public :: fixed_pitch
    !> The basic fixed-pitch face with serifs.  Port of
    !> faces.el:fixed-pitch-serif.
    type(named_face_type), target, public :: fixed_pitch_serif
    !> The basic variable-pitch face.  Port of faces.el:variable-pitch.
    type(named_face_type), target, public :: variable_pitch
    !> The proportional face used for longer texts.  Port of
    !> faces.el:variable-pitch-text.
    type(named_face_type), target, public :: variable_pitch_text
    !> Basic face for shadowed text.  Port of faces.el:shadow.
    type(named_face_type), target, public :: shadow
    !> Basic face for unvisited links.  Port of faces.el:link.
    type(named_face_type), target, public :: link
    !> Basic face for visited links.  Port of faces.el:link-visited.
    type(named_face_type), target, public :: link_visited
    !> Basic face for highlighting.  Port of faces.el:highlight.
    type(named_face_type), target, public :: highlight
    !> Basic face for highlighting the region.  Port of faces.el:region.
    type(named_face_type), target, public :: region
    !> Basic face for displaying the secondary selection.  Port of
    !> faces.el:secondary-selection.
    type(named_face_type), target, public :: secondary_selection
    !> Basic face for highlighting trailing whitespace.  Port of
    !> faces.el:trailing-whitespace.
    type(named_face_type), target, public :: trailing_whitespace
    !> Face for displaying line numbers.  This face is used when
    !> `display_line_numbers' is true.
    !>
    !> If you customize the font of this face, make sure it is a
    !> monospaced font, otherwise line numbers will not line up, and text
    !> lines might move horizontally as you move through the buffer.  Port
    !> of faces.el:line-number.
    type(named_face_type), target, public :: line_number
    !> Face for displaying the current line number.  This face is used
    !> when `display_line_numbers' is true.
    !>
    !> If you customize the font of this face, make sure it is a
    !> monospaced font, otherwise line numbers will not line up, and text
    !> lines might move horizontally as you move through the buffer.
    !> Similarly, making this face's font different from that of the
    !> `line_number' face could produce such unwanted effects.  Port of
    !> faces.el:line-number-current-line.
    type(named_face_type), target, public :: line_number_current_line
    !> Face for highlighting "major ticks" (as in a ruler).  When
    !> `display_line_numbers_major_tick' is positive, highlight the line
    !> numbers of lines which are a multiple of its value.  This face is
    !> used when `display_line_numbers' is true.
    !>
    !> If you customize the font of this face, make sure it is a
    !> monospaced font, otherwise line numbers will not line up, and text
    !> lines might move horizontally as you move through the buffer.
    !> Similarly, making this face's font different from that of the
    !> `line_number' face could produce such unwanted effects.  Port of
    !> faces.el:line-number-major-tick.
    type(named_face_type), target, public :: line_number_major_tick
    !> Face for highlighting "minor ticks" (as in a ruler).  When
    !> `display_line_numbers_minor_tick' is positive, highlight the line
    !> numbers of lines which are a multiple of its value.  This face is
    !> used when `display_line_numbers' is true.
    !>
    !> If you customize the font of this face, make sure it is a
    !> monospaced font, otherwise line numbers will not line up, and text
    !> lines might move horizontally as you move through the buffer.
    !> Similarly, making this face's font different from that of the
    !> `line_number' face could produce such unwanted effects.  Port of
    !> faces.el:line-number-minor-tick.
    type(named_face_type), target, public :: line_number_minor_tick
    !> Face for displaying fill column indicator.  This face is used when
    !> `display_fill_column_indicator_mode' is true.  Port of
    !> faces.el:fill-column-indicator.
    type(named_face_type), target, public :: fill_column_indicator
    !> Face for characters displayed as sequences using `^' or `\'.  Port
    !> of faces.el:escape-glyph.
    type(named_face_type), target, public :: escape_glyph
    !> Face for highlighting homoglyphs.  Port of faces.el:homoglyph.
    type(named_face_type), target, public :: homoglyph
    !> Face for displaying nobreak space.  Port of faces.el:nobreak-space.
    type(named_face_type), target, public :: nobreak_space
    !> Face for displaying nobreak hyphens.  Port of
    !> faces.el:nobreak-hyphen.
    type(named_face_type), target, public :: nobreak_hyphen
    !> Face for the mode lines as well as header lines.  See
    !> `mode_line_active' and `mode_line_inactive' for the faces used on
    !> mode lines.  Port of faces.el:mode-line.
    type(named_face_type), target, public :: mode_line
    !> Face for the selected mode line.  This inherits from the
    !> `mode_line' face.  Port of faces.el:mode-line-active.
    type(named_face_type), target, public :: mode_line_active
    !> Basic mode line face for non-selected windows.  Port of
    !> faces.el:mode-line-inactive.
    type(named_face_type), target, public :: mode_line_inactive
    !> Basic mode line face for highlighting.  Port of
    !> faces.el:mode-line-highlight.
    type(named_face_type), target, public :: mode_line_highlight
    !> Face used to emphasize certain mode line features.  Use the
    !> `mode_line_emphasis' text property to apply it.  Port of
    !> faces.el:mode-line-emphasis.
    type(named_face_type), target, public :: mode_line_emphasis
    !> Face used for buffer identification parts of the mode line.  Port
    !> of faces.el:mode-line-buffer-id.
    type(named_face_type), target, public :: mode_line_buffer_id
    !> Basic header-line face.  Port of faces.el:header-line.
    type(named_face_type), target, public :: header_line
    !> Basic header line face for highlighting.  Port of
    !> faces.el:header-line-highlight.
    type(named_face_type), target, public :: header_line_highlight
    !> Face for the selected header line.  This inherits from the
    !> `header_line' face.  Port of faces.el:header-line-active.
    type(named_face_type), target, public :: header_line_active
    !> Basic header line face for non-selected windows.  Port of
    !> faces.el:header-line-inactive.
    type(named_face_type), target, public :: header_line_inactive
    !> Face used for vertical window dividers on ttys.  Port of
    !> faces.el:vertical-border.
    type(named_face_type), target, public :: vertical_border
    !> Basic face for window dividers.  When a divider is less than 3
    !> pixels wide, it is drawn solidly with the foreground of this face.
    !> For larger dividers this face is used for the inner part while the
    !> first pixel line/column is drawn with the
    !> `window_divider_first_pixel' face and the last pixel line/column
    !> with the `window_divider_last_pixel' face.  Port of
    !> faces.el:window-divider.
    type(named_face_type), target, public :: window_divider
    !> Basic face for first pixel line/column of window dividers.  When a
    !> divider is at least 3 pixels wide, its first pixel line/column is
    !> drawn with the foreground of this face.  If you do not want to
    !> accentuate the first pixel line/column, set this to the same as
    !> `window_divider' face.  Port of faces.el:window-divider-first-pixel.
    type(named_face_type), target, public :: window_divider_first_pixel
    !> Basic face for last pixel line/column of window dividers.  When a
    !> divider is at least 3 pixels wide, its last pixel line/column is
    !> drawn with the foreground of this face.  If you do not want to
    !> accentuate the last pixel line/column, set this to the same as
    !> `window_divider' face.  Port of faces.el:window-divider-last-pixel.
    type(named_face_type), target, public :: window_divider_last_pixel
    !> Basic face for the internal border.  For the internal border of
    !> child frames see `child_frame_border'.  Port of
    !> faces.el:internal-border.
    type(named_face_type), target, public :: internal_border
    !> Basic face for the internal border of child frames.  For the
    !> internal border of non-child frames see `internal_border'.  Port
    !> of faces.el:child-frame-border.
    type(named_face_type), target, public :: child_frame_border
    !> Face for minibuffer prompts.  By default, Emacs automatically adds
    !> this face to the value of `minibuffer_prompt_properties', which is
    !> a list of text properties used to display the prompt text.  Port
    !> of faces.el:minibuffer-prompt.
    type(named_face_type), target, public :: minibuffer_prompt
    !> Basic face for window margins (both left and right).  This face is
    !> used to customize the appearance of the margin areas.  Port of
    !> faces.el:margin.
    type(named_face_type), target, public :: margin
    !> Basic face for the fringes to the left and right of windows under
    !> X.  Port of faces.el:fringe.
    type(named_face_type), target, public :: fringe
    !> Basic face for the scroll bar colors under X.  Port of
    !> faces.el:scroll-bar.
    type(named_face_type), target, public :: scroll_bar
    !> Basic face for the frame border under X.  Port of faces.el:border.
    type(named_face_type), target, public :: border
    !> Basic face for the cursor color under X.  Currently, only the
    !> `background' attribute is meaningful; all other attributes are
    !> ignored.  The cursor foreground color is taken from the background
    !> color of the underlying text.
    !>
    !> Note: Other faces cannot inherit from the cursor face.  Port of
    !> faces.el:cursor.
    type(named_face_type), target, public :: cursor
    !> Basic face for the mouse color under X.  Port of faces.el:mouse.
    type(named_face_type), target, public :: mouse
    !> Basic tool-bar face.  Port of faces.el:tool-bar.
    type(named_face_type), target, public :: tool_bar
    !> Tab bar face.  Port of faces.el:tab-bar.
    type(named_face_type), target, public :: tab_bar
    !> Basic tab line face.  See `tab_line_active' and
    !> `tab_line_inactive' for the faces used on tab lines.  Port of
    !> faces.el:tab-line.
    type(named_face_type), target, public :: tab_line
    !> Face for the selected tab line.  This inherits from the `tab_line'
    !> face.  Port of faces.el:tab-line-active.
    type(named_face_type), target, public :: tab_line_active
    !> Basic tab line face for non-selected windows.  Port of
    !> faces.el:tab-line-inactive.
    type(named_face_type), target, public :: tab_line_inactive
    !> Basic face for the font and colors of the menu bar and popup
    !> menus.  Port of faces.el:menu.
    type(named_face_type), target, public :: menu
    !> Face to highlight argument names in *Help* buffers.  Port of
    !> faces.el:help-argument-name.
    type(named_face_type), target, public :: help_argument_name
    !> Face for keybindings in *Help* buffers.  This face is added by
    !> `substitute_command_keys', which see.  Port of
    !> faces.el:help-key-binding.
    type(named_face_type), target, public :: help_key_binding
    !> Face for displaying non-graphic characters (e.g. U+202A (LRE)).  It
    !> is used for characters of no fonts too.  Port of
    !> faces.el:glyphless-char.
    type(named_face_type), target, public :: glyphless_char
    !> Basic face used to highlight errors and to denote failure.  Port
    !> of faces.el:error.
    type(named_face_type), target, public :: error
    !> Basic face used to highlight warnings.  Port of faces.el:warning.
    type(named_face_type), target, public :: warning
    !> Basic face used to indicate successful operation.  Port of
    !> faces.el:success.
    type(named_face_type), target, public :: success
    !> Face for the name in `read_multiple_choice' output.  Port of
    !> faces.el:read-multiple-choice-face.
    type(named_face_type), target, public :: read_multiple_choice_face
    !> Face for displaying enabled items in TTY menus.  Port of
    !> faces.el:tty-menu-enabled-face.
    type(named_face_type), target, public :: tty_menu_enabled_face
    !> Face for displaying disabled items in TTY menus.  Port of
    !> faces.el:tty-menu-disabled-face.
    type(named_face_type), target, public :: tty_menu_disabled_face
    !> Face for displaying the currently selected item in TTY menus.
    !> Port of faces.el:tty-menu-selected-face.
    type(named_face_type), target, public :: tty_menu_selected_face
    !> Face used for a matching paren.  Port of faces.el:show-paren-match.
    type(named_face_type), target, public :: show_paren_match
    !> Face used for a matching paren when highlighting the whole
    !> expression.  This face is used by `show_paren_mode'.  Port of
    !> faces.el:show-paren-match-expression.
    type(named_face_type), target, public :: show_paren_match_expression
    !> Face used for a mismatching paren.  Port of
    !> faces.el:show-paren-mismatch.
    type(named_face_type), target, public :: show_paren_mismatch

contains

    ! ------------------------------------------------------------------
    ! Attribute types
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine logical_to_flag(self, value)
        class(face_flag_type), intent(inout) :: self
        logical, intent(in) :: value

        if (value) then
            self%state = FACE_ON
        else
            self%state = FACE_OFF
        end if
    end subroutine logical_to_flag

    ! Not a port.
    subroutine flag_unspecify(self)
        class(face_flag_type), intent(inout) :: self

        self%state = FACE_UNSPECIFIED
    end subroutine flag_unspecify

    ! Not a port.
    subroutine logical_to_line(self, value)
        class(face_line_type), intent(inout) :: self
        logical, intent(in) :: value

        if (allocated(self%color)) deallocate (self%color)
        self%style = FACE_STYLE_LINE
        self%position = .false.

        if (value) then
            self%state = FACE_ON
        else
            self%state = FACE_OFF
        end if
    end subroutine logical_to_line

    ! Not a port.
    subroutine color_to_line(self, color)
        class(face_line_type), intent(inout) :: self
        character(len=*), intent(in) :: color

        self%state = FACE_COLOR
        self%color = color
        self%style = FACE_STYLE_LINE
        self%position = .false.
    end subroutine color_to_line

    ! Not a port.
    subroutine line_unspecify(self)
        class(face_line_type), intent(inout) :: self

        self%state = FACE_UNSPECIFIED
        if (allocated(self%color)) deallocate (self%color)
    end subroutine line_unspecify

    ! Not a port.
    subroutine logical_to_box(self, value)
        class(face_box_type), intent(inout) :: self
        logical, intent(in) :: value

        if (allocated(self%color)) deallocate (self%color)
        if (allocated(self%style)) deallocate (self%style)
        self%line_width = 1
        self%line_height = 1

        if (value) then
            self%state = FACE_VALUE
        else
            self%state = FACE_OFF
        end if
    end subroutine logical_to_box

    ! Not a port.
    subroutine color_to_box(self, color)
        class(face_box_type), intent(inout) :: self
        character(len=*), intent(in) :: color

        if (allocated(self%style)) deallocate (self%style)
        self%state = FACE_COLOR
        self%color = color
        self%line_width = 1
        self%line_height = 1
    end subroutine color_to_box

    ! Not a port.
    subroutine box_unspecify(self)
        class(face_box_type), intent(inout) :: self

        self%state = FACE_UNSPECIFIED
    end subroutine box_unspecify

    ! Not a port.
    subroutine integer_to_height(self, value)
        class(face_height_type), intent(inout) :: self
        integer, intent(in) :: value

        self%state = FACE_VALUE
        self%absolute = value
    end subroutine integer_to_height

    ! Not a port.
    subroutine real_to_height(self, value)
        class(face_height_type), intent(inout) :: self
        real, intent(in) :: value

        self%state = FACE_PLIST
        self%factor = value
    end subroutine real_to_height

    ! Not a port.
    subroutine height_unspecify(self)
        class(face_height_type), intent(inout) :: self

        self%state = FACE_UNSPECIFIED
    end subroutine height_unspecify

    ! Not a port.
    subroutine name_to_inherit(self, name)
        class(face_inherit_type), intent(inout) :: self
        character(len=*), intent(in) :: name

        if (allocated(self%names)) deallocate (self%names)

        if (len_trim(name) == 0) then
            self%state = FACE_OFF
            return
        end if

        allocate (self%names(1))
        self%names(1) = name
        self%state = FACE_LIST
    end subroutine name_to_inherit

    ! Not a port.
    subroutine names_to_inherit(self, names)
        class(face_inherit_type), intent(inout) :: self
        character(len=*), intent(in) :: names(:)
        integer :: k

        if (allocated(self%names)) deallocate (self%names)

        if (size(names) == 0) then
            self%state = FACE_OFF
            return
        end if

        allocate (self%names(size(names)))

        do k = 1, size(names)
            self%names(k) = names(k)
        end do

        self%state = FACE_LIST
    end subroutine names_to_inherit

    ! Not a port.
    subroutine inherit_unspecify(self)
        class(face_inherit_type), intent(inout) :: self

        self%state = FACE_UNSPECIFIED
        if (allocated(self%names)) deallocate (self%names)
    end subroutine inherit_unspecify

    !> Face attributes with the given ones specified, for specs: the
    !> attributes of a defface alternative.  INHERIT names one face
    !> inherited from, INHERITS a list of them.  Not a port.
    function face_attrs(family, foundry, foreground, background, distant_foreground, weight, slant, width, height, &
                        relative_height, underline, underline_color, underline_style, overline, strike_through, &
                        box, box_color, inverse_video, inherit, inherits, extend) result(a)
        character(len=*), intent(in), optional :: family, foundry, foreground, background, distant_foreground, &
                                                  weight, slant, width, underline_color, box_color, inherit
        character(len=*), intent(in), optional :: inherits(:)
        integer, intent(in), optional :: height, underline_style
        real, intent(in), optional :: relative_height
        logical, intent(in), optional :: underline, overline, strike_through, box, inverse_video, extend
        type(face_attributes_type) :: a

        if (present(family)) a%family = family
        if (present(foundry)) a%foundry = foundry
        if (present(foreground)) a%foreground = foreground
        if (present(background)) a%background = background
        if (present(distant_foreground)) a%distant_foreground = distant_foreground
        if (present(weight)) a%weight = weight
        if (present(slant)) a%slant = slant
        if (present(width)) a%width = width
        if (present(height)) a%height = height
        if (present(relative_height)) a%height = relative_height
        if (present(underline)) a%underline = underline
        if (present(underline_color)) a%underline = underline_color

        if (present(underline_style)) then
            if (a%underline%state /= FACE_COLOR) then
                a%underline%state = FACE_PLIST
            else
                a%underline%state = FACE_PLIST
            end if

            a%underline%style = underline_style
        end if

        if (present(overline)) a%overline = overline
        if (present(strike_through)) a%strike_through = strike_through
        if (present(box)) a%box = box

        if (present(box_color)) then
            a%box%state = FACE_PLIST
            a%box%color = box_color
        end if

        if (present(inverse_video)) a%inverse_video = inverse_video
        if (present(inherit)) a%inherit = inherit
        if (present(inherits)) a%inherit = inherits
        if (present(extend)) a%extend = extend
    end function face_attrs

    !> An underline given as upstream's
    !> attributes `:color', `:style' and `:position', as `color', `style' and `position', for
    !> `faces%NAME%underline = face_underline(...)'.  An absent COLOR is
    !> the face's foreground color (`foreground-color'); STYLE is one of
    !> FACE_STYLE_LINE (the default), FACE_STYLE_DOUBLE_LINE,
    !> FACE_STYLE_WAVE, FACE_STYLE_DOTS and FACE_STYLE_DASHES; POSITION
    !> true draws the line at the descent of the text instead of the
    !> baseline (a number of pixels above the descent is Deferred with
    !> the GUI).  On a terminal, the styles other than a line are drawn
    !> where the terminal supports them (the Smulx or Su capability) and
    !> as a line otherwise.  The property-list form of xfaces.c's
    !> `:underline' attribute.  Not a port.
    function face_underline(color, style, position) result(line)
        character(len=*), intent(in), optional :: color
        integer, intent(in), optional :: style
        logical, intent(in), optional :: position
        type(face_line_type) :: line

        line%state = FACE_PLIST

        if (present(color)) then
            line%color = color
        else
            line%color = ""
        end if

        if (present(style)) line%style = style
        if (present(position)) line%position = position
    end function face_underline

    !> A box given as upstream's attributes `:line-width' WIDTH, `:color'
    !> COLOR and `:style' STYLE, for `faces%NAME%box = face_box(...)'.
    !> LINE_WIDTH N is upstream's single number: vertical lines of width
    !> |N| and horizontal lines of width N; VERTICAL_WIDTH and
    !> HORIZONTAL_WIDTH give upstream's two values, VWIDTH and HWIDTH, and default
    !> to 1.  A negative width draws the line over the text instead of
    !> beside it.  An absent COLOR is the face's background color for 3D
    !> boxes and FACE_BOX_FLAT_BUTTON and its foreground color otherwise;
    !> STYLE is FACE_BOX_RELEASED_BUTTON, FACE_BOX_PRESSED_BUTTON (3D
    !> buttons) or FACE_BOX_FLAT_BUTTON, and absent for a plain box.  A
    !> terminal draws no box.  The property-list form of xfaces.c's `:box'
    !> attribute.  Not a port.
    function face_box(line_width, vertical_width, horizontal_width, color, style) result(box)
        integer, intent(in), optional :: line_width, vertical_width, horizontal_width
        character(len=*), intent(in), optional :: color
        integer, intent(in), optional :: style
        type(face_box_type) :: box

        box%state = FACE_PLIST

        if (present(line_width)) then
            box%line_width = abs(line_width)
            box%line_height = line_width
        end if

        if (present(vertical_width)) box%line_width = vertical_width
        if (present(horizontal_width)) box%line_height = horizontal_width
        if (present(color)) box%color = color

        if (present(style)) then
            select case (style)
            case (FACE_BOX_RELEASED_BUTTON)
                box%style = "released-button"
            case (FACE_BOX_PRESSED_BUTTON)
                box%style = "pressed-button"
            case (FACE_BOX_FLAT_BUTTON)
                box%style = "flat-button"
            case default
                continue
            end select
        end if
    end function face_box

    !> A display condition for a spec alternative (see
    !> `face_display_type'); no argument at all means t.  A `supports'
    !> condition is given to `spec_entry'.  Not a port.
    function display_when(classes, min_colors, backgrounds, types, is_default) result(d)
        integer, intent(in), optional :: classes(:), backgrounds(:)
        integer, intent(in), optional :: min_colors
        character(len=*), intent(in), optional :: types(:)
        logical, intent(in), optional :: is_default
        type(face_display_type) :: d
        integer :: k

        d%always = .not. (present(classes) .or. present(min_colors) .or. present(backgrounds) .or. present(types))
        if (present(classes)) d%classes = classes
        if (present(backgrounds)) d%backgrounds = backgrounds
        if (present(min_colors)) d%min_colors = min_colors

        if (present(types)) then
            allocate (d%types(size(types)))

            do k = 1, size(types)
                d%types(k) = types(k)
            end do
        end if

        if (present(is_default)) d%is_default = is_default
    end function display_when

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Make face SELF known by NAME, with documentation DOC and no spec
    !> yet (`face_spec_entry' adds alternatives).  Port of
    !> faces.el:make-face / custom-declare-face (internal-make-lisp-face).
    subroutine face_define(self, name, doc)
        class(named_face_type), intent(inout), target :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: doc
        type(face_ref_type), allocatable :: grown(:)
        integer :: n

        self%name = name
        if (present(doc)) self%doc = doc
        if (allocated(self%spec)) deallocate (self%spec)
        allocate (self%spec(0))
        if (associated(face_named(name))) return
        if (.not. allocated(registry)) allocate (registry(0))
        n = size(registry)
        allocate (grown(n + 1))
        grown(1:n) = registry

        select type (self)
        type is (named_face_type)
            grown(n + 1)%p => self
        end select

        call move_alloc(grown, registry)
    end subroutine face_define

    !> Add the alternative (DISPLAY . ATTRS) at the end of the face's
    !> spec.  Not a port.
    subroutine face_spec_entry(self, display, attrs, supports)
        class(named_face_type), intent(inout) :: self
        type(face_display_type), intent(in) :: display
        type(face_attributes_type), intent(in) :: attrs
        !> The attributes of a `supports' condition in DISPLAY.
        type(face_attributes_type), intent(in), optional :: supports
        type(face_spec_entry_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(self%spec)) allocate (self%spec(0))
        n = size(self%spec)
        allocate (grown(n + 1))
        grown(1:n) = self%spec
        grown(n + 1)%display = display

        if (present(supports)) then
            grown(n + 1)%display%supports = .true.
            grown(n + 1)%display%always = .false.
            grown(n + 1)%supports_attrs = supports
        end if

        grown(n + 1)%attrs = attrs
        call move_alloc(grown, self%spec)
    end subroutine face_spec_entry

    !> The face named NAME, or null.  Port of
    !> xfaces.c:Finternal_lisp_face_p (the face itself).
    function face_named(name) result(p)
        character(len=*), intent(in) :: name
        type(named_face_type), pointer :: p
        integer :: k

        p => null()
        if (.not. allocated(registry)) return

        do k = 1, size(registry)
            if (trim(registry(k)%p%name) == name) then
                p => registry(k)%p
                return
            end if
        end do
    end function face_named

    !> The number of faces defined.  Not a port.
    integer function face_count()
        face_count = 0
        if (allocated(registry)) face_count = size(registry)
    end function face_count

    !> The K-th face defined (face-list order).  Not a port.
    function face_at(k) result(p)
        integer, intent(in) :: k
        type(named_face_type), pointer :: p

        p => registry(k)%p
    end function face_at

    !> Tell the face specs about the frame: its display class
    !> (FACE_CLASS_*), background mode (FACE_BACKGROUND_*), number of
    !> colors and window system type ("tty").  Port of the frame
    !> parameters frame.el:frame-set-background-mode sets.
    subroutine set_frame_display(class, background, colors, type)
        integer, intent(in) :: class, background, colors
        character(len=*), intent(in) :: type

        frame_class = class
        frame_background = background
        frame_colors = colors
        frame_type = type
    end subroutine set_frame_display

    !> True if DISPLAY matches the frame.  Port of
    !> faces.el:face-spec-set-match-display.
    logical function match_display(display, supports_attrs) result(match)
        type(face_display_type), intent(in) :: display
        type(face_attributes_type), intent(in) :: supports_attrs
        integer :: k

        match = .true.
        if (display%always) return

        if (allocated(display%types)) then
            match = .false.

            do k = 1, size(display%types)
                if (trim(display%types(k)) == trim(frame_type)) match = .true.
                ! (type graphic) matches window systems only.
            end do

            if (.not. match) return
        end if

        if (display%min_colors > 0) then
            match = frame_colors >= display%min_colors
            if (.not. match) return
        end if

        if (allocated(display%classes)) then
            match = any(display%classes == frame_class)
            if (.not. match) return
        end if

        if (allocated(display%backgrounds)) then
            match = any(display%backgrounds == frame_background)
            if (.not. match) return
        end if

        if (display%supports) then
            match = .false.

            if (associated(display_supports_face_attributes_p)) then
                match = display_supports_face_attributes_p(supports_attrs)
            end if
        end if
    end function match_display

    !> Merge the specified attributes of FROM into TO, which they
    !> replace (as face-spec-set-2 / set-face-attribute do).  Not a port.
    subroutine set_attributes(to, from)
        type(face_attributes_type), intent(inout) :: to
        type(face_attributes_type), intent(in) :: from

        if (allocated(from%family)) to%family = from%family
        if (allocated(from%foundry)) to%foundry = from%foundry
        if (allocated(from%width)) to%width = from%width
        if (from%height%state /= FACE_UNSPECIFIED) to%height = from%height
        if (allocated(from%weight)) to%weight = from%weight
        if (allocated(from%slant)) to%slant = from%slant
        if (from%underline%state /= FACE_UNSPECIFIED) to%underline = from%underline
        if (from%overline%state /= FACE_UNSPECIFIED) to%overline = from%overline
        if (from%strike_through%state /= FACE_UNSPECIFIED) to%strike_through = from%strike_through
        if (from%box%state /= FACE_UNSPECIFIED) to%box = from%box
        if (from%inverse_video%state /= FACE_UNSPECIFIED) to%inverse_video = from%inverse_video
        if (allocated(from%foreground)) to%foreground = from%foreground
        if (allocated(from%distant_foreground)) to%distant_foreground = from%distant_foreground
        if (allocated(from%background)) to%background = from%background
        if (from%inherit%state /= FACE_UNSPECIFIED) to%inherit = from%inherit
        if (from%extend%state /= FACE_UNSPECIFIED) to%extend = from%extend
    end subroutine set_attributes

    !> Reset the face attributes of SELF on the frame according to its
    !> spec: all unspecified, then the spec alternative chosen for the
    !> frame (with the `default' alternative's attributes underneath),
    !> then the attributes the configuration set (as
    !> face-set-after-frame-default applies the global ones).  Port of
    !> faces.el:face-spec-recalc (with face-spec-choose; no theme specs
    !> or X resources).
    subroutine face_spec_recalc(self)
        class(named_face_type), intent(inout) :: self
        integer :: k
        logical :: found

        self%lface = face_attributes_type()
        found = .false.

        if (allocated(self%spec)) then
            ! If the condition is `default', that sets the default for
            ! following conditions.
            do k = 1, size(self%spec)
                if (self%spec(k)%display%is_default) call set_attributes(self%lface, self%spec(k)%attrs)
            end do

            do k = 1, size(self%spec)
                if (self%spec(k)%display%is_default) cycle

                if (match_display(self%spec(k)%display, self%spec(k)%supports_attrs)) then
                    call set_attributes(self%lface, self%spec(k)%attrs)
                    found = .true.
                    exit
                end if
            end do
        end if

        call set_attributes(self%lface, self%face_attributes_type)
    end subroutine face_spec_recalc

    !> Recompute the attributes of every face on the frame (as
    !> frame-set-background-mode does for a new frame).  Not a port.
    subroutine face_spec_recalc_all()
        integer :: k

        if (.not. allocated(registry)) return

        do k = 1, size(registry)
            call registry(k)%p%recalc()
        end do
    end subroutine face_spec_recalc_all

    !> Whether face attributes A and B are the same.  Port of
    !> xfaces.c:lface_equal_p.
    logical function faces_equal(a, b) result(eq)
        type(face_attributes_type), intent(in) :: a, b

        eq = same_string(a%family, b%family) .and. same_string(a%foundry, b%foundry) .and. &
             same_string(a%width, b%width) .and. same_string(a%weight, b%weight) .and. &
             same_string(a%slant, b%slant) .and. same_string(a%foreground, b%foreground) .and. &
             same_string(a%distant_foreground, b%distant_foreground) .and. &
             same_string(a%background, b%background)
        if (.not. eq) return
        eq = a%height%state == b%height%state .and. a%height%absolute == b%height%absolute .and. &
             a%height%factor == b%height%factor
        if (.not. eq) return
        eq = same_line(a%underline, b%underline) .and. same_line(a%overline, b%overline) .and. &
             same_line(a%strike_through, b%strike_through)
        if (.not. eq) return
        eq = a%box%state == b%box%state .and. a%box%line_width == b%box%line_width .and. &
             a%box%line_height == b%box%line_height .and. same_string(a%box%color, b%box%color) .and. &
             same_string(a%box%style, b%box%style)
        if (.not. eq) return
        eq = a%inverse_video%state == b%inverse_video%state .and. a%extend%state == b%extend%state .and. &
             a%inherit%state == b%inherit%state
        if (.not. eq) return

        if (a%inherit%state == FACE_LIST) then
            eq = size(a%inherit%names) == size(b%inherit%names)
            if (eq) eq = all(a%inherit%names == b%inherit%names)
        end if

    contains

        logical function same_string(x, y)
            character(len=:), allocatable, intent(in) :: x, y

            same_string = allocated(x) .eqv. allocated(y)
            if (same_string .and. allocated(x)) same_string = x == y .and. len(x) == len(y)
        end function same_string

        logical function same_line(x, y)
            type(face_line_type), intent(in) :: x, y

            same_line = x%state == y%state .and. x%style == y%style .and. (x%position .eqv. y%position)
            if (same_line) same_line = same_string(x%color, y%color)
        end function same_line
    end function faces_equal

    !> The numeric value of weight NAME ("bold": 200), -1 if unknown.
    !> Port of font.c's weight_table (FONT_WEIGHT_NAME_NUMERIC).
    integer function face_weight_numeric(name) result(n)
        character(len=*), intent(in) :: name

        select case (name)
        case ("thin")
            n = 0
        case ("ultra-light", "ultralight", "extra-light", "extralight")
            n = 40
        case ("light")
            n = 50
        case ("semi-light", "semilight", "demilight")
            n = 55
        case ("regular", "normal", "unspecified", "book")
            n = 80
        case ("medium")
            n = 100
        case ("semi-bold", "semibold", "demibold", "demi-bold", "demi")
            n = 180
        case ("bold")
            n = 200
        case ("extra-bold", "extrabold", "ultra-bold", "ultrabold")
            n = 205
        case ("black", "heavy")
            n = 210
        case ("ultra-heavy", "ultraheavy")
            n = 250
        case default
            n = -1
        end select
    end function face_weight_numeric

    !> The numeric value of slant NAME ("italic": 200), -1 if unknown.
    !> Port of font.c's slant_table (FONT_SLANT_NAME_NUMERIC).
    integer function face_slant_numeric(name) result(n)
        character(len=*), intent(in) :: name

        select case (name)
        case ("reverse-oblique", "ro")
            n = 0
        case ("reverse-italic", "ri")
            n = 10
        case ("normal", "r", "unspecified")
            n = 100
        case ("italic", "i", "ot")
            n = 200
        case ("oblique", "o")
            n = 210
        case default
            n = -1
        end select
    end function face_slant_numeric

    ! ------------------------------------------------------------------
    ! The faces of faces.el
    ! ------------------------------------------------------------------

    !> Define the faces of faces.el with their specs.  Not a port.
    subroutine init_faces()
        integer, parameter :: C = FACE_CLASS_COLOR, G = FACE_CLASS_GRAYSCALE, M = FACE_CLASS_MONO
        integer, parameter :: LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK
        type(face_attributes_type) :: attrs

        call default%define("default", "Basic default face.")
        call default%spec_entry(display_when(), face_attrs())
        call bold%define("bold", "Basic bold face.")
        call bold%spec_entry(display_when(), face_attrs(weight="bold"))
        call italic%define("italic", "Basic italic face.")
        call italic%spec_entry(display_when(), face_attrs(slant="italic"), supports=face_attrs(slant="italic"))
        call italic%spec_entry(display_when(), face_attrs(underline=.true., slant="italic"), &
                               supports=face_attrs(underline=.true.))
        call italic%spec_entry(display_when(), face_attrs(slant="italic"))
        call bold_italic%define("bold_italic", "Basic bold-italic face.")
        call bold_italic%spec_entry(display_when(), face_attrs(weight="bold", slant="italic"))
        call underline%define("underline", "Basic underlined face.")
        call underline%spec_entry(display_when(), face_attrs(underline=.true.), supports=face_attrs(underline=.true.))
        call underline%spec_entry(display_when(), face_attrs(weight="bold"), &
                                  supports=face_attrs(weight="bold", underline=.true.))
        call underline%spec_entry(display_when(), face_attrs(underline=.true.))
        call fixed_pitch%define("fixed_pitch", "The basic fixed-pitch face.")
        call fixed_pitch%spec_entry(display_when(), face_attrs(family="Monospace"))
        call fixed_pitch_serif%define("fixed_pitch_serif", "The basic fixed-pitch face with serifs.")
        call fixed_pitch_serif%spec_entry(display_when(), face_attrs(family="Monospace Serif"))
        call variable_pitch%define("variable_pitch", "The basic variable-pitch face.")
        call variable_pitch%spec_entry(display_when(types=[character(len=3) :: "w32"]), &
                                       face_attrs(family="Arial", foundry="outline"))
        call variable_pitch%spec_entry(display_when(), face_attrs(family="Sans Serif"))
        call variable_pitch_text%define("variable_pitch_text", "The proportional face used for longer texts.")
        call variable_pitch_text%spec_entry(display_when(), face_attrs(inherit="variable_pitch", &
                                                                       relative_height=1.1))
        call shadow%define("shadow", "Basic face for shadowed text.")
        call shadow%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[LIGHT]), &
                               face_attrs(foreground="grey50"))
        call shadow%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[DARK]), &
                               face_attrs(foreground="grey70"))
        call shadow%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[LIGHT]), &
                               face_attrs(foreground="green"))
        call shadow%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[DARK]), &
                               face_attrs(foreground="yellow"))
        call link%define("link", "Basic face for unvisited links.")
        call link%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                             face_attrs(foreground="RoyalBlue3", underline=.true.))
        call link%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), &
                             face_attrs(foreground="blue", underline=.true.))
        call link%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                             face_attrs(foreground="cyan1", underline=.true.))
        call link%spec_entry(display_when(classes=[C], backgrounds=[DARK]), &
                             face_attrs(foreground="cyan", underline=.true.))
        call link%spec_entry(display_when(), face_attrs(inherit="underline"))
        call link_visited%define("link_visited", "Basic face for visited links.")
        call link_visited%spec_entry(display_when(is_default=.true.), face_attrs(inherit="link"))
        call link_visited%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), &
                                     face_attrs(foreground="magenta4"))
        call link_visited%spec_entry(display_when(classes=[C], backgrounds=[DARK]), &
                                     face_attrs(foreground="violet"))
        call highlight%define("highlight", "Basic face for highlighting.")
        call highlight%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                  face_attrs(background="darkseagreen2"))
        call highlight%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                  face_attrs(background="darkolivegreen"))
        call highlight%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), &
                                  face_attrs(background="darkseagreen2"))
        call highlight%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), &
                                  face_attrs(background="darkolivegreen"))
        call highlight%spec_entry(display_when(classes=[C], min_colors=8), &
                                  face_attrs(background="green", foreground="black"))
        call highlight%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call region%define("region", "Basic face for highlighting the region.")
        call region%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                               face_attrs(background="blue3", extend=.true.))
        call region%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                               face_attrs(background="lightgoldenrod2", extend=.true.))
        call region%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), &
                               face_attrs(background="blue3", extend=.true.))
        call region%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), &
                               face_attrs(background="lightgoldenrod2", extend=.true.))
        call region%spec_entry(display_when(classes=[C], min_colors=8), &
                               face_attrs(background="blue", foreground="white", extend=.true.))
        call region%spec_entry(display_when(types=[character(len=3) :: "tty"], classes=[M]), &
                               face_attrs(inverse_video=.true.))
        call region%spec_entry(display_when(), face_attrs(background="gray", extend=.true.))
        call secondary_selection%define("secondary_selection", "Basic face for displaying the secondary selection.")
        call secondary_selection%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[LIGHT]), &
                                            face_attrs(background="yellow1", extend=.true.))
        call secondary_selection%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[DARK]), &
                                            face_attrs(background="SkyBlue4", extend=.true.))
        call secondary_selection%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), &
                                            face_attrs(background="yellow", extend=.true.))
        call secondary_selection%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), &
                                            face_attrs(background="SkyBlue4", extend=.true.))
        call secondary_selection%spec_entry(display_when(classes=[C], min_colors=8), &
                                            face_attrs(background="cyan", foreground="black", extend=.true.))
        call secondary_selection%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call trailing_whitespace%define("trailing_whitespace", "Basic face for highlighting trailing whitespace.")
        call trailing_whitespace%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attrs(background="red1"))
        call trailing_whitespace%spec_entry(display_when(classes=[C], backgrounds=[DARK]), face_attrs(background="red1"))
        call trailing_whitespace%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call line_number%define("line_number", "Face for displaying line numbers.")
        call line_number%spec_entry(display_when(), &
                                    face_attrs(inherits=[character(len=7) :: "shadow", "default"]))
        call line_number_current_line%define("line_number_current_line", &
                                             "Face for displaying the current line number.")
        call line_number_current_line%spec_entry(display_when(), face_attrs(inherit="line_number"))
        call line_number_major_tick%define("line_number_major_tick", &
                                           'Face for highlighting "major ticks" (as in a ruler).')
        call line_number_major_tick%spec_entry(display_when(classes=[C, G], backgrounds=[LIGHT]), &
                                               face_attrs(background="grey85", weight="bold"))
        call line_number_major_tick%spec_entry(display_when(classes=[C, G], backgrounds=[DARK]), &
                                               face_attrs(background="grey75", weight="bold"))
        call line_number_major_tick%spec_entry(display_when(), face_attrs(inherit="line_number"))
        call line_number_minor_tick%define("line_number_minor_tick", &
                                           'Face for highlighting "minor ticks" (as in a ruler).')
        call line_number_minor_tick%spec_entry(display_when(classes=[C, G], backgrounds=[LIGHT]), &
                                               face_attrs(background="grey95", weight="bold"))
        call line_number_minor_tick%spec_entry(display_when(classes=[C, G], backgrounds=[DARK]), &
                                               face_attrs(background="grey55", weight="bold"))
        call line_number_minor_tick%spec_entry(display_when(), face_attrs(inherit="line_number"))
        call fill_column_indicator%define("fill_column_indicator", "Face for displaying fill column indicator.")
        ! (:stipple nil is not kept: stipples are deferred.)
        call fill_column_indicator%spec_entry(display_when(), &
                                              face_attrs(inherit="shadow", weight="normal", slant="normal", &
                                                         underline=.false., overline=.false., &
                                                         strike_through=.false., box=.false., &
                                                         inverse_video=.false.))
        call escape_glyph%define("escape_glyph", "Face for characters displayed as sequences using `^' or `\'.")
        call escape_glyph%spec_entry(display_when(backgrounds=[DARK]), face_attrs(foreground="cyan"))
        call escape_glyph%spec_entry(display_when(types=[character(len=3) :: "pc"]), face_attrs(foreground="magenta"))
        call escape_glyph%spec_entry(display_when(), face_attrs(foreground="brown"))
        call homoglyph%define("homoglyph", "Face for lookalike characters.")
        call homoglyph%spec_entry(display_when(backgrounds=[DARK]), face_attrs(foreground="cyan"))
        call homoglyph%spec_entry(display_when(types=[character(len=3) :: "pc"]), face_attrs(foreground="magenta"))
        call homoglyph%spec_entry(display_when(), face_attrs(foreground="brown"))
        call nobreak_space%define("nobreak_space", "Face for displaying nobreak space.")
        call nobreak_space%spec_entry(display_when(classes=[C], min_colors=88), &
                                      face_attrs(inherit="escape_glyph", underline=.true.))
        call nobreak_space%spec_entry(display_when(classes=[C], min_colors=8), &
                                      face_attrs(background="magenta"))
        call nobreak_space%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call nobreak_hyphen%define("nobreak_hyphen", "Face for displaying nobreak hyphens.")
        call nobreak_hyphen%spec_entry(display_when(backgrounds=[DARK]), face_attrs(foreground="cyan"))
        call nobreak_hyphen%spec_entry(display_when(types=[character(len=3) :: "pc"]), &
                                       face_attrs(foreground="magenta"))
        call nobreak_hyphen%spec_entry(display_when(), face_attrs(foreground="brown"))
        call mode_line%define("mode_line", "Face for the mode lines as well as header lines.")
        call mode_line%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[LIGHT]), &
                                  mode_line_box(background="grey75", foreground="black"))
        call mode_line%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[DARK]), &
                                  mode_line_box(background="grey20", foreground="white"))
        call mode_line%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call mode_line_active%define("mode_line_active", "Face for the selected mode line.")
        call mode_line_active%spec_entry(display_when(), face_attrs(inherit="mode_line"))
        call mode_line_inactive%define("mode_line_inactive", "Basic mode line face for non-selected windows.")
        call mode_line_inactive%spec_entry(display_when(is_default=.true.), face_attrs(inherit="mode_line"))
        call mode_line_inactive%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[LIGHT]), &
                                           inactive_box(weight="light", box_color="grey75", foreground="grey20", &
                                                        background="grey90"))
        call mode_line_inactive%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[DARK]), &
                                           inactive_box(weight="light", box_color="grey40", foreground="grey80", &
                                                        background="grey30"))
        call mode_line_highlight%define("mode_line_highlight", "Basic mode line face for highlighting.")
        call mode_line_highlight%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[LIGHT]), &
                                            highlight_box("grey40"), supports=face_attrs(box=.true.))
        call mode_line_highlight%spec_entry(display_when(classes=[C, G], min_colors=88, backgrounds=[DARK]), &
                                            highlight_box("grey80"), supports=face_attrs(box=.true.))
        call mode_line_highlight%spec_entry(display_when(), face_attrs(inherit="highlight"))
        call mode_line_emphasis%define("mode_line_emphasis", "Face used to emphasize certain mode line features.")
        call mode_line_emphasis%spec_entry(display_when(), face_attrs(weight="bold"))
        call mode_line_buffer_id%define("mode_line_buffer_id", &
                                        "Face used for buffer identification parts of the mode line.")
        call mode_line_buffer_id%spec_entry(display_when(), face_attrs(weight="bold"))
        call header_line%define("header_line", "Basic header-line face.")
        call header_line%spec_entry(display_when(is_default=.true.), face_attrs(inherit="mode_line"))
        call header_line%spec_entry(display_when(types=[character(len=3) :: "tty"]), &
                                    face_attrs(inverse_video=.false., underline=.true.))
        call header_line%spec_entry(display_when(classes=[C, G], backgrounds=[LIGHT]), &
                                    face_attrs(background="grey90", foreground="grey20", box=.false.))
        call header_line%spec_entry(display_when(classes=[C, G], backgrounds=[DARK]), &
                                    face_attrs(background="grey20", foreground="grey90", box=.false.))
        call header_line%spec_entry(display_when(classes=[M], backgrounds=[LIGHT]), &
                                    face_attrs(background="white", foreground="black", inverse_video=.false., &
                                               box=.false., underline=.true.))
        call header_line%spec_entry(display_when(classes=[M], backgrounds=[DARK]), &
                                    face_attrs(background="black", foreground="white", inverse_video=.false., &
                                               box=.false., underline=.true.))
        call header_line_highlight%define("header_line_highlight", "Basic header line face for highlighting.")
        call header_line_highlight%spec_entry(display_when(), face_attrs(inherit="mode_line_highlight"))
        call header_line_active%define("header_line_active", "Face for the selected header line.")
        call header_line_active%spec_entry(display_when(), face_attrs(inherit="header_line"))
        call header_line_inactive%define("header_line_inactive", "Basic header line face for non-selected windows.")
        call header_line_inactive%spec_entry(display_when(), face_attrs(inherit="header_line"))
        call vertical_border%define("vertical_border", "Face used for vertical window dividers on ttys.")
        call vertical_border%spec_entry(display_when(types=[character(len=3) :: "tty"]), &
                                        face_attrs(inherit="mode_line_inactive"))
        call window_divider%define("window_divider", "Basic face for window dividers.")
        call window_divider%spec_entry(display_when(), face_attrs(foreground="gray60"))
        call window_divider_first_pixel%define("window_divider_first_pixel", &
                                               "Basic face for first pixel line/column of window dividers.")
        call window_divider_first_pixel%spec_entry(display_when(), face_attrs(foreground="gray80"))
        call window_divider_last_pixel%define("window_divider_last_pixel", &
                                              "Basic face for last pixel line/column of window dividers.")
        call window_divider_last_pixel%spec_entry(display_when(), face_attrs(foreground="gray40"))
        call internal_border%define("internal_border", "Basic face for the internal border.")
        call internal_border%spec_entry(display_when(), face_attrs())
        call child_frame_border%define("child_frame_border", "Basic face for the internal border of child frames.")
        call child_frame_border%spec_entry(display_when(), face_attrs())
        call minibuffer_prompt%define("minibuffer_prompt", "Face for minibuffer prompts.")
        call minibuffer_prompt%spec_entry(display_when(backgrounds=[DARK]), face_attrs(foreground="cyan"))
        call minibuffer_prompt%spec_entry(display_when(types=[character(len=3) :: "pc"]), &
                                          face_attrs(foreground="magenta"))
        call minibuffer_prompt%spec_entry(display_when(), face_attrs(foreground="medium blue"))
        call margin%define("margin", "Basic face for window margins (both left and right).")
        call margin%spec_entry(display_when(), face_attrs(inherit="default"))
        call fringe%define("fringe", "Basic face for the fringes to the left and right of windows under X.")
        call fringe%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), face_attrs(background="grey95"))
        call fringe%spec_entry(display_when(classes=[C], backgrounds=[DARK]), face_attrs(background="grey10"))
        call fringe%spec_entry(display_when(), face_attrs(background="gray"))
        call scroll_bar%define("scroll_bar", "Basic face for the scroll bar colors under X.")
        call scroll_bar%spec_entry(display_when(), face_attrs())
        call border%define("border", "Basic face for the frame border under X.")
        call border%spec_entry(display_when(), face_attrs())
        call cursor%define("cursor", "Basic face for the cursor color under X.")
        call cursor%spec_entry(display_when(backgrounds=[LIGHT]), face_attrs(background="black"))
        call cursor%spec_entry(display_when(backgrounds=[DARK]), face_attrs(background="white"))
        call mouse%define("mouse", "Basic face for the mouse color under X.")
        call mouse%spec_entry(display_when(), face_attrs())
        call tool_bar%define("tool_bar", "Basic tool-bar face.")
        attrs = face_attrs(foreground="black")
        attrs%box%state = FACE_PLIST
        attrs%box%line_width = 1
        attrs%box%line_height = 1
        attrs%box%style = "released-button"
        call tool_bar%spec_entry(display_when(is_default=.true.), attrs)
        call tool_bar%spec_entry(display_when(types=[character(len=5) :: "haiku"]), &
                                 face_attrs(foreground="B_MENU_ITEM_TEXT_COLOR", background="B_MENU_BACKGROUND_COLOR"))
        call tool_bar%spec_entry(display_when(types=[character(len=7) :: "x", "w32", "ns", "pgtk", "android"], &
                                              classes=[C]), face_attrs(background="grey75"))
        call tool_bar%spec_entry(display_when(types=[character(len=1) :: "x"], classes=[M]), &
                                 face_attrs(background="grey"))
        call tab_bar%define("tab_bar", "Tab bar face.")
        call tab_bar%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                face_attrs(inherit="variable_pitch", background="grey85", foreground="black"))
        call tab_bar%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                face_attrs(inherit="variable_pitch", background="grey20", foreground="white"))
        call tab_bar%spec_entry(display_when(classes=[M]), face_attrs(background="grey"))
        call tab_bar%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call tab_line%define("tab_line", "Basic tab line face.")
        call tab_line%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                 face_attrs(inherit="variable_pitch", relative_height=0.9, background="grey85", &
                                            foreground="black"))
        call tab_line%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                 face_attrs(inherit="variable_pitch", relative_height=0.9, background="grey20", &
                                            foreground="white"))
        call tab_line%spec_entry(display_when(classes=[M]), face_attrs(background="grey"))
        call tab_line%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call tab_line_active%define("tab_line_active", "Face for the selected tab line.")
        call tab_line_active%spec_entry(display_when(), face_attrs(inherit="tab_line"))
        call tab_line_inactive%define("tab_line_inactive", "Basic tab line face for non-selected windows.")
        call tab_line_inactive%spec_entry(display_when(), face_attrs(inherit="tab_line"))
        call menu%define("menu", "Basic face for the font and colors of the menu bar and popup menus.")
        call menu%spec_entry(display_when(types=[character(len=3) :: "tty"]), face_attrs(inverse_video=.true.))
        call menu%spec_entry(display_when(types=[character(len=9) :: "x-toolkit"]), face_attrs())
        call menu%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call help_argument_name%define("help_argument_name", "Face to highlight argument names in *Help* buffers.")
        call help_argument_name%spec_entry(display_when(), face_attrs(inherit="italic"))
        call help_key_binding%define("help_key_binding", "Face for keybindings in *Help* buffers.")
        call help_key_binding%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                                         key_box(background="grey96", foreground="DarkBlue", box_color="grey80"))
        call help_key_binding%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                         key_box(background="grey19", foreground="LightBlue", box_color="grey35"))
        call help_key_binding%spec_entry(display_when(classes=[C, G], backgrounds=[LIGHT]), &
                                         face_attrs(background="grey90", inherit="fixed_pitch"))
        call help_key_binding%spec_entry(display_when(classes=[C, G], backgrounds=[DARK]), &
                                         face_attrs(background="grey25", inherit="fixed_pitch"))
        call help_key_binding%spec_entry(display_when(), face_attrs(background="grey90", inherit="fixed_pitch"))
        call glyphless_char%define("glyphless_char", &
                                   "Face for displaying non-graphic characters (e.g. U+202A (LRE)).")
        call glyphless_char%spec_entry(display_when(types=[character(len=3) :: "tty"]), &
                                       face_attrs(inherit="escape_glyph", underline=.true.))
        call glyphless_char%spec_entry(display_when(types=[character(len=3) :: "pc"]), &
                                       face_attrs(inherit="escape_glyph"))
        call glyphless_char%spec_entry(display_when(), face_attrs(relative_height=0.6))
        call error%define("error", "Basic face used to highlight errors and to denote failure.")
        call error%spec_entry(display_when(is_default=.true.), face_attrs(weight="bold"))
        call error%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                              face_attrs(foreground="Red1"))
        call error%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                              face_attrs(foreground="Pink"))
        call error%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), &
                              face_attrs(foreground="Red1"))
        call error%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), &
                              face_attrs(foreground="Pink"))
        call error%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="red"))
        call error%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call warning%define("warning", "Basic face used to highlight warnings.")
        call warning%spec_entry(display_when(is_default=.true.), face_attrs(weight="bold"))
        call warning%spec_entry(display_when(classes=[C], min_colors=16), face_attrs(foreground="DarkOrange"))
        call warning%spec_entry(display_when(classes=[C]), face_attrs(foreground="yellow"))
        call success%define("success", "Basic face used to indicate successful operation.")
        call success%spec_entry(display_when(is_default=.true.), face_attrs(weight="bold"))
        call success%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), &
                                face_attrs(foreground="ForestGreen"))
        call success%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), &
                                face_attrs(foreground="Green1"))
        call success%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), &
                                face_attrs(foreground="Green"))
        call success%spec_entry(display_when(classes=[C]), face_attrs(foreground="green"))
        call read_multiple_choice_face%define("read_multiple_choice_face", &
                                              "Face for the symbol name in `read_multiple_choice' output.")
        call read_multiple_choice_face%spec_entry(display_when(), &
                                                  face_attrs(inherits=[character(len=16) :: "help_key_binding", &
                                                                       "underline"], weight="bold"))
        call tty_menu_enabled_face%define("tty_menu_enabled_face", "Face for displaying enabled items in TTY menus.")
        call tty_menu_enabled_face%spec_entry(display_when(classes=[C]), &
                                              face_attrs(foreground="yellow", background="blue", weight="bold"))
        call tty_menu_enabled_face%spec_entry(display_when(), face_attrs(weight="bold"))
        call tty_menu_disabled_face%define("tty_menu_disabled_face", "Face for displaying disabled items in TTY menus.")
        call tty_menu_disabled_face%spec_entry(display_when(classes=[C], min_colors=16), &
                                               face_attrs(foreground="lightgray", background="blue"))
        call tty_menu_disabled_face%spec_entry(display_when(classes=[C]), &
                                               face_attrs(foreground="white", background="blue"))
        call tty_menu_disabled_face%spec_entry(display_when(), face_attrs(inherit="shadow"))
        call tty_menu_selected_face%define("tty_menu_selected_face", &
                                           "Face for displaying the currently selected item in TTY menus.")
        call tty_menu_selected_face%spec_entry(display_when(classes=[C]), face_attrs(background="red"))
        call tty_menu_selected_face%spec_entry(display_when(), face_attrs(inverse_video=.true.))
        call show_paren_match%define("show_paren_match", "Face used for a matching paren.")
        call show_paren_match%spec_entry(display_when(classes=[C], backgrounds=[LIGHT]), &
                                         face_attrs(background="turquoise"))
        call show_paren_match%spec_entry(display_when(classes=[C], backgrounds=[DARK]), &
                                         face_attrs(background="steelblue3"))
        call show_paren_match%spec_entry(display_when(backgrounds=[DARK], min_colors=4), &
                                         face_attrs(background="grey50"))
        call show_paren_match%spec_entry(display_when(backgrounds=[LIGHT], min_colors=4), &
                                         face_attrs(background="gray"))
        call show_paren_match%spec_entry(display_when(), face_attrs(inherit="underline"))
        call show_paren_match_expression%define("show_paren_match_expression", &
                                                "Face used for a matching paren when highlighting the whole expression.")
        call show_paren_match_expression%spec_entry(display_when(), face_attrs(inherit="show_paren_match"))
        call show_paren_mismatch%define("show_paren_mismatch", "Face used for a mismatching paren.")
        call show_paren_mismatch%spec_entry(display_when(classes=[C]), &
                                            face_attrs(foreground="white", background="purple"))
        call show_paren_mismatch%spec_entry(display_when(), face_attrs(inverse_video=.true.))

    contains

        !> The attributes of mode-line's graphic alternatives: a released
        !> button box of width -1.
        function mode_line_box(background, foreground) result(a)
            character(len=*), intent(in) :: background, foreground
            type(face_attributes_type) :: a

            a = face_attrs(background=background, foreground=foreground)
            a%box%state = FACE_PLIST
            a%box%line_width = -1
            a%box%line_height = -1
            a%box%style = "released-button"
        end function mode_line_box

        !> The attributes of mode-line-highlight's box alternatives.
        function highlight_box(color) result(a)
            character(len=*), intent(in) :: color
            type(face_attributes_type) :: a

            a%box%state = FACE_PLIST
            a%box%line_width = 2
            a%box%line_height = 2
            a%box%color = color
            a%box%style = "released-button"
        end function highlight_box

        !> The attributes of mode-line-inactive's alternatives.
        function inactive_box(weight, box_color, foreground, background) result(a)
            character(len=*), intent(in) :: weight, box_color, foreground, background
            type(face_attributes_type) :: a

            a = face_attrs(weight=weight, foreground=foreground, background=background)
            a%box%state = FACE_PLIST
            a%box%line_width = -1
            a%box%line_height = -1
            a%box%color = box_color
        end function inactive_box

        !> The attributes of help-key-binding's color alternatives.
        function key_box(background, foreground, box_color) result(a)
            character(len=*), intent(in) :: background, foreground, box_color
            type(face_attributes_type) :: a

            a = face_attrs(background=background, foreground=foreground, inherit="fixed_pitch")
            a%box%state = FACE_PLIST
            a%box%line_width = -1
            a%box%line_height = -1
            a%box%color = box_color
        end function key_box
    end subroutine init_faces

    ! ------------------------------------------------------------------
    ! Colors
    ! ------------------------------------------------------------------

    !> Return a description of the color named COLOR on the selected frame.
    !> The value is R, G and B, integers that range from 0 to 65535; white
    !> is 65535 65535 65535.  If COLOR is not directly supported by the
    !> display, the values are those of the supported color that is its
    !> best approximation.  `found' is false when the color is not defined
    !> (or is "unspecified-fg" or "unspecified-bg").  Port of
    !> faces.el:color-values (a character terminal's `tty_color_values').
    subroutine color_values(color, found, r, g, b)
        character(len=*), intent(in) :: color
        logical, intent(out) :: found
        integer, intent(out) :: r, g, b

        r = 0
        g = 0
        b = 0
        found = .false.
        if (color == "unspecified-fg" .or. color == "unspecified-bg") return
        call tty_color_values(color, found, r, g, b)
    end subroutine color_values

    !> Whether RGB is more readable against white than black.
    !> RGB is a 3-element array (R G B), each component in the range [0,1].
    !> This predicate can be used both for determining a suitable (black or
    !> white) contrast color with RGB as background and as foreground.
    !> Port of faces.el:color-dark-p.
    logical function is_color_dark(rgb, status)
        real(real64), intent(in) :: rgb(3)
        type(status_type), intent(out) :: status
        real(real64) :: r, g, b, y

        is_color_dark = .false.

        if (.not. (minval(rgb) >= 0 .and. maxval(rgb) <= 1)) then
            call set_error(status, FM_GENERIC_ERROR, "RGB components not in [0,1]")
            return
        end if

        ! Compute the relative luminance after gamma-correcting (assuming
        ! sRGB), and compare to a cut-off value determined experimentally.
        ! Use the power 2.2 as an approximation to sRGB gamma; it should be
        ! good enough for the purpose of this function.
        r = rgb(1)**2.2_real64
        g = rgb(2)**2.2_real64
        b = rgb(3)**2.2_real64
        y = r*0.2126_real64 + g*0.7152_real64 + b*0.0722_real64
        is_color_dark = y < COLOR_LUMINANCE_DARK_LIMIT
    end function is_color_dark

    !> Return a readable foreground color for background COLOR.
    !> The returned value is a string representing black or white, depending
    !> on which one provides better contrast with COLOR.  Port of
    !> faces.el:readable-foreground-color.
    function readable_foreground_color(color) result(fg)
        character(len=*), intent(in) :: color
        character(len=:), allocatable :: fg
        type(status_type) :: st
        logical :: found
        integer :: r, g, b

        ! We use #ffffff instead of "white", because the latter is
        ! sometimes less than white.  That way, we get the best contrast
        ! possible.
        call color_values(color, found, r, g, b)
        ! ((color-dark-p nil) is an error; it is taken as black.)
        fg = "black"
        if (.not. found) return
        if (is_color_dark([r, g, b]/65535.0_real64, st)) fg = "#ffffff"
    end function readable_foreground_color
end module faces_fm
