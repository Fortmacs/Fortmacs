! An interface's suffix is spelled out (R2-Q10), and so is a process argument
! and comint-check-proc's name.
module iface_mod
    implicit none (type, external)

    abstract interface
        subroutine filter_procedure(process, text)
            integer, intent(in) :: process
            character(len=*), intent(in) :: text
        end subroutine filter_procedure
    end interface

    procedure(filter_procedure), pointer :: filter => null()

contains

    subroutine check(process)
        integer, intent(in) :: process

        call comint_check_process(process)
        ! A chained entry reaches its final name in one run.
        call cut_backward_up_group()
    end subroutine check
end module iface_mod
