! A fixture for tools/test-fortran-names.sh (not compiled).
module expression_motion_mod
    use syntax_mod, only: scan_groups, scan_expressions, parse_expression_ignore_comments
    implicit none (type, external)
    private
    public :: forward_expression, backward_up_group, mark_subprogram, auto_modes

    integer, parameter :: DABBREV_NO = 3
    character(len=40), allocatable :: compilation_error_regexp_names(:)

contains

    !> Move forward across one balanced expression (expression).
    !> With ARG, do it that many times; an expression is a subprogram's part.
    !> See `forward_expression_function' and a make_ewoc of handlers.
    !> Port of lisp.el:forward-sexp (upstream's `forward-sexp' takes
    !> INTERACTIVE), and syntax.c:scan_lists; Fscan_lists is its DEFUN.
    subroutine forward_expression(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: halfexpression

        call init_compilation_error_regexps()
        call scan_groups(arg, halfexpression, status)
        if (.not. status%ok()) call set_error(status, "No next expression")
        call temp_buffer("(defun foo ()")
        call smie_down_group()
        ! A line the rename lengthens past 132 columns (reported): forward_expression forward_expression forward_expression forward_expression.
    end subroutine forward_expression
end module expression_motion_mod
