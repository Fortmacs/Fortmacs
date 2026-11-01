! A fixture for tools/test-fortran-names.sh (not compiled).
module lisp_cmds_fm
    use syntax_fm, only: scan_lists, scan_sexps, parse_sexp_ignore_comments
    implicit none (type, external)
    private
    public :: forward_sexp, backward_up_list, mark_defun, auto_mode_alist

    integer, parameter :: DABBREV_OFF = 3
    character(len=40), allocatable :: compilation_error_regexp_alist(:)

contains

    !> Move forward across one balanced expression (sexp).
    !> With ARG, do it that many times; a sexp is a defun's part.
    !> See `forward_sexp_function' and an ewoc_create of handlers.
    !> Port of lisp.el:forward-sexp (upstream's `forward-sexp' takes
    !> INTERACTIVE), and syntax.c:scan_lists; Fscan_lists is its DEFUN.
    subroutine forward_sexp(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: halfsexp

        call init_compilation_error_regexp_alist_alist()
        call scan_lists(arg, halfsexp, status)
        if (.not. status%ok()) call set_error(status, "No next sexp")
        call temp_buffer("(defun foo ()")
        call smie_down_list()
        ! A line the rename lengthens past 132 columns (reported): forward_sexp forward_sexp forward_sexp forward_sexp.
    end subroutine forward_sexp
end module lisp_cmds_fm
