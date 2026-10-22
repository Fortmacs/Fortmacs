! init.f90 -- an example Fortmacs configuration.
!
! Copy it to ~/.config/fortmacs/init.f90, change it, then run
! `fortmacs --recompile`.  The module must be named `user_config` and
! provide `configure`; everything else is up to you.  See the `fortmacs`
! module (src/fortmacs.f90) for what you can use.
module user_config
    ! The whole API, so that settings and functions need no list.
    ! allow(use-all)
    use fortmacs
    implicit none(type, external)
    private

    public :: configure

contains

    !> Runs once at startup.
    subroutine configure(status)
        type(status_type), intent(out) :: status
        tab_width = 4
        make_backup_files = .false.
        call defcommand("insert-date-stamp", insert_date_stamp)
        call keymap_global_set("C-c d", "insert-date-stamp", status)
    end subroutine configure

    !> Insert today's date, as YYYYMMDD.
    subroutine insert_date_stamp(status)
        type(status_type), intent(out) :: status
        character(len=8) :: date
        call date_and_time(date=date)
        call insert(date, status)
    end subroutine insert_date_stamp
end module user_config
