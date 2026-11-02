! An interface's suffix is spelled out (R2-Q10), and so is a process argument
! and comint-check-proc's name.
module iface_mod
    implicit none (type, external)

    abstract interface
        subroutine filter_proc(proc, text)
            integer, intent(in) :: proc
            character(len=*), intent(in) :: text
        end subroutine filter_proc
    end interface

    procedure(filter_proc), pointer :: filter => null()

contains

    subroutine check(proc)
        integer, intent(in) :: proc

        call comint_check_proc(proc)
        ! A chained entry reaches its final name in one run.
        call kill_backward_up_list()
    end subroutine check
end module iface_mod
