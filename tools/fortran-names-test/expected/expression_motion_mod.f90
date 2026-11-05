! A fixture for tools/test-fortran-names.sh (not compiled).
module expression_motion_mod
    use syntax_mod, only: scan_groups, scan_expressions, parse_expression_ignore_comments
    implicit none (type, external)
    private
    public :: forward_expression, backward_up_group, mark_subprogram, auto_modes

    integer, parameter :: DYNAMIC_ABBREVIATION_NO = 3
    character(len=40), allocatable :: compilation_error_regular_expression_names(:)

contains

    !> Move forward across one balanced expression (expression).
    !> With ARGUMENT, do it that many times; an expression is a subprogram's part.
    !> See `forward_expression_function' and a make_fortmacs_widget_for_object_collections of handlers.
    !> Port of lisp.el:forward-sexp (upstream's `forward-sexp' takes
    !> INTERACTIVE), and syntax.c:scan_lists; Fscan_lists is its DEFUN.
    subroutine forward_expression(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        integer :: halfexpression

        call initialize_compilation_error_regular_expressions()
        call scan_groups(argument, halfexpression, status)
        if (.not. status%ok()) call set_error(status, "No next expression")
        call temporary_buffer("(defun foo ()")
        call simple_minded_indentation_engine_down_group()
        ! A line the rename lengthens past 132 columns (reported): forward_expression forward_expression forward_expression forward_expression.
    end subroutine forward_expression
end module expression_motion_mod
