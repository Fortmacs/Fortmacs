! version_control_branches_mod.f90 -- the tag, branch, pull and push commands of VC.
! Ported from GNU Emacs 31.1 lisp/vc/vc.el.
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

!> Making tags and branches (C-x v s, C-x v b c), retrieving them
!> (C-x v r, C-x v b s), pulling (C-x v +) and pushing (C-x v P), and the
!> incoming revision the incoming and outgoing logs start from.
!>
!> Deferred: see docs/status/version_control_mod.md.
submodule(version_control_mod) version_control_branches_mod
    use execute_interactively_mod, only: current_prefix_argument, PREFIX_NONE
    use minibuffer_mod, only: read_directory_name, REQUIRE_MATCH_YES
    use files_mod, only: abbreviate_file_name
    use version_control_hooks_mod, only: VC_REVISION_GRANULARITY_REPOSITORY, vc_file_get_incoming_revision, &
                                         vc_file_set_incoming_revision, vc_file_clear_incoming_revisions
    implicit none(type, external)

contains

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function buffer_directory

    ! ------------------------------------------------------------------
    ! Tags and branches
    ! ------------------------------------------------------------------

    !> Whether the backend responsible for `default_directory' keeps
    !> revisions for the whole repository, so that the commands need not
    !> ask for a directory.  Not a port.
    logical function is_repository_granular(status) result(granular)
        type(status_type), intent(out) :: status
        integer :: b

        granular = .false.
        b = vc_responsible_backend(buffer_directory(), status)
        if (.not. status%ok()) return
        granular = vc_backends(b)%revision_granularity == VC_REVISION_GRANULARITY_REPOSITORY
    end function is_repository_granular

    module procedure vc_create_tag
    character(len=:), allocatable :: kind, tag_directory
    integer :: b

    kind = "tag"
    if (is_branch) kind = "branch"
    call message("Making "//kind//"... ")
    tag_directory = directory
    if (is_file_directory(tag_directory)) tag_directory = file_name_as_directory(tag_directory)
    b = vc_responsible_backend(tag_directory, status)
    if (.not. status%ok()) return

    if (.not. associated(vc_backends(b)%create_tag)) then
        call vc_not_supported(status, "create-tag", b)
        return
    end if

    call vc_backends(b)%create_tag(tag_directory, name, is_branch, status)
    if (.not. status%ok()) return
    call vc_resynch_buffer(tag_directory, tag_directory, status, .true., VC_NOQUERY_YES, .true.)
    if (.not. status%ok()) return
    call message("Making "//kind//"... done")
    end procedure vc_create_tag

    module procedure vc_create_branch
    call vc_create_tag(directory, name, .true., status)
    end procedure vc_create_branch

    module procedure vc_retrieve_tag
    character(len=:), allocatable :: message_text, kind
    logical :: update, branching
    integer :: b

    branching = .false.
    if (present(is_branch)) branching = is_branch

    if (branching .and. len(name) == 0) then
        call set_error(status, USER_ERROR_CODE, "Branch name required")
        return
    end if

    b = vc_responsible_backend(directory, status)
    if (.not. status%ok()) return
    update = .false.

    if (vc_backends(b)%update_on_retrieve_tag) then
        call read_yes_or_no("Update any affected buffers? ", update, status)
        if (.not. status%ok()) return
    end if

    kind = "tag"
    if (branching) kind = "branch"

    if (len(name) == 0) then
        message_text = "Updating "//abbreviate_file_name(directory)//"... "
    else
        message_text = "Retrieving "//kind//" "//name//" into "//abbreviate_file_name(directory)//"... "
    end if

    call message(message_text)

    if (.not. associated(vc_backends(b)%retrieve_tag)) then
        call vc_not_supported(status, "retrieve-tag", b)
        return
    end if

    call vc_backends(b)%retrieve_tag(directory, name, update, status)
    if (.not. status%ok()) return
    call vc_resynch_buffer(directory, directory, status, .true., VC_NOQUERY_YES, .true.)
    if (.not. status%ok()) return
    call vc_retrieve_tag_hook%execute(status)
    if (.not. status%ok()) return
    call message(message_text//"done")
    end procedure vc_retrieve_tag

    module procedure vc_switch_branch
    call vc_retrieve_tag(directory, name, status, .true.)
    end procedure vc_switch_branch

    !> The directory of the tag and branch commands: `default_directory'
    !> for a repository-granular backend, else read with PROMPT
    !> (`default_directory' as the default when DEFAULT).  ROOT asks for the
    !> repository's root instead of `default_directory'.  Port of the
    !> argument reading of vc.el:vc-create-tag and vc.el:vc-retrieve-tag.
    recursive subroutine read_tag_directory(directory, status, root, default)
        character(len=:), allocatable, intent(out) :: directory
        type(status_type), intent(out) :: status
        logical, intent(in) :: root, default
        character(len=:), allocatable :: expanded
        logical :: found

        directory = ""

        if (is_repository_granular(status)) then
            ! For VC's that do not work at file level, it's pointless
            ! to ask for a directory, branches are created at repository level.
            if (root) then
                call vc_root_directory(directory, found, status)
                if (.not. status%ok()) return
                call expand_file_name(directory, expanded, status)
                if (.not. status%ok()) return
                directory = expanded
            else
                directory = buffer_directory()
            end if
        else if (status%ok()) then
            if (default) then
                call read_directory_name("Directory: ", directory, status, buffer_directory(), buffer_directory(), &
                                         REQUIRE_MATCH_YES)
            else
                call read_directory_name("Directory: ", directory, status, buffer_directory(), mustmatch=REQUIRE_MATCH_YES)
            end if
        end if
    end subroutine read_tag_directory

    module procedure execute_vc_create_tag
    character(len=:), allocatable :: directory, name
    logical :: branching

    call read_tag_directory(directory, status, .false., .true.)
    if (.not. status%ok()) return
    branching = current_prefix_argument%kind /= PREFIX_NONE

    if (branching) then
        call read_string("New branch name: ", name, status, history="vc_revision_history")
    else
        call read_string("New tag name: ", name, status, history="vc_revision_history")
    end if

    if (.not. status%ok()) return
    call vc_create_tag(directory, name, branching, status)
    end procedure execute_vc_create_tag

    module procedure execute_vc_create_branch
    character(len=:), allocatable :: directory, name

    call read_tag_directory(directory, status, .false., .true.)
    if (.not. status%ok()) return
    call read_string("New branch name: ", name, status, history="vc_revision_history")
    if (.not. status%ok()) return
    call vc_create_branch(directory, name, status)
    end procedure execute_vc_create_branch

    module procedure execute_vc_retrieve_tag
    type(string_type), allocatable :: files(:)
    character(len=:), allocatable :: directory, name
    logical :: branching
    integer :: b

    ! XXX: Either we call expand-file-name here, or use
    ! file-in-directory-p inside vc-resynch-buffers-in-directory.
    call read_tag_directory(directory, status, .true., .false.)
    if (.not. status%ok()) return
    branching = current_prefix_argument%kind /= PREFIX_NONE
    call append_string(files, directory)
    b = vc_responsible_backend(directory, status)
    if (.not. status%ok()) return

    if (branching) then
        call vc_read_revision("Switch to branch: ", name, status, files, b)
    else
        call vc_read_revision("Tag name to retrieve (default latest revisions): ", name, status, files, b)
    end if

    if (.not. status%ok()) return
    call vc_retrieve_tag(directory, name, status, branching)
    end procedure execute_vc_retrieve_tag

    module procedure execute_vc_switch_branch
    type(string_type), allocatable :: files(:)
    character(len=:), allocatable :: directory, name
    integer :: b

    call read_tag_directory(directory, status, .true., .false.)
    if (.not. status%ok()) return
    call append_string(files, directory)
    b = vc_responsible_backend(directory, status)
    if (.not. status%ok()) return
    call vc_read_revision("Switch to branch: ", name, status, files, b)
    if (.not. status%ok()) return
    call vc_switch_branch(directory, name, status)
    end procedure execute_vc_switch_branch

    ! ------------------------------------------------------------------
    ! Pulling and pushing
    ! ------------------------------------------------------------------

    !> Forget the incoming revisions of BACKEND's repository, after a pull
    !> or push.  Port of the vc--repo-setprop call of vc.el:vc-pull and
    !> vc.el:vc-push.
    subroutine clear_incoming_revisions(backend)
        integer, intent(in) :: backend
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found

        call vc_root_directory(root, found, st, backend)
        if (found) call vc_file_clear_incoming_revisions(root)
    end subroutine clear_incoming_revisions

    module procedure vc_pull
    type(vc_fileset_type) :: fs, retry
    type(status_type) :: deduced, st
    type(handle_type) :: process
    integer :: b

    status = status_type()
    call vc_deduce_fileset(fs, deduced, .true.)

    if (deduced%ok()) then
        b = fs%backend
    else
        ! If `vc-deduce-fileset' failed then try again with
        ! ALLOW-UNREGISTERED true, but in this case we require a
        ! `pull' function because that doesn't need a list of files.
        call vc_deduce_fileset(retry, st, .true., .true.)
        b = 0
        if (st%ok()) b = retry%backend

        if (b == 0) then
            status = deduced
            return
        end if

        if (.not. associated(vc_backends(b)%pull)) then
            status = deduced
            return
        end if
    end if

    ! If a pull operation is defined, use it.
    if (associated(vc_backends(b)%pull)) then
        call vc_backends(b)%pull(prompt, status, process)
        if (.not. status%ok()) return
        ! FIXME: Ideally we would only clear out the stored value for the
        ! REMOTE-LOCATION from which we are pulling.
        call clear_incoming_revisions(b)
    else
        call set_error(status, GENERIC_ERROR_CODE, "VC update is unsupported for `"//vc_backend_name(b)//"'")
    end if
    end procedure vc_pull

    module procedure vc_push
    type(vc_fileset_type) :: fs
    type(handle_type) :: process

    call vc_deduce_fileset(fs, status, .true., .true.)
    if (.not. status%ok()) return

    if (associated(vc_backends(fs%backend)%push)) then
        call vc_backends(fs%backend)%push(prompt, status, process)
        if (.not. status%ok()) return
        ! FIXME: Ideally we would only clear out the
        ! REMOTE-LOCATION to which we are pushing.
        call clear_incoming_revisions(fs%backend)
    else
        call set_error(status, USER_ERROR_CODE, "VC push is unsupported for `"//vc_backend_name(fs%backend)//"'")
    end if
    end procedure vc_push

    module procedure execute_vc_pull
    call vc_pull(current_prefix_argument%kind /= PREFIX_NONE, status)
    end procedure execute_vc_pull

    module procedure execute_vc_push
    call vc_push(current_prefix_argument%kind /= PREFIX_NONE, status)
    end procedure execute_vc_push

    ! ------------------------------------------------------------------
    ! The incoming revision
    ! ------------------------------------------------------------------

    module procedure vc_incoming_revision
    character(len=:), allocatable :: root
    logical :: refreshing, found, known

    ! Some backends don't support REFRESH and so always behave as though
    ! REFRESH is true.  This is not just for a lack of implementation
    ! in Fortmacs; for example, Mercurial repositories don't store any
    ! representation of the incoming revision between running commands.
    !
    ! Fetching the incoming revision is often slow, and in many cases the
    ! last known incoming revision will serve perfectly well.  For
    ! example, when finding revisions that are outgoing, the last known
    ! incoming revision is fine except for the rare case in which someone
    ! else cherry-picks the very same commits that you have outstanding,
    ! and pushes them.  Given this, we implement our own caching.
    !
    ! Do store none, before an error condition occurs, if there is no
    ! incoming revision, because that's also something that can be slow
    ! to determine and so should be remembered.
    refreshing = .false.
    if (present(refresh)) refreshing = refresh
    revision = ""
    call vc_root_directory(root, found, status, backend)
    if (.not. status%ok()) return
    known = .false.
    found = .false.
    if (.not. refreshing) call vc_file_get_incoming_revision(root, upstream_location, revision, known, found)

    if (.not. known) then
        if (.not. associated(vc_backends(backend)%incoming_revision)) then
            call vc_not_supported(status, "incoming-revision", backend)
            return
        end if

        call vc_backends(backend)%incoming_revision(upstream_location, refreshing, revision, found, status)
        if (.not. status%ok()) return

        if (found) then
            call vc_file_set_incoming_revision(root, upstream_location, revision)
        else
            call vc_file_set_incoming_revision(root, upstream_location)
        end if
    end if

    if (.not. found) call set_error(status, USER_ERROR_CODE, "No incoming revision -- local-only branch?")
    end procedure vc_incoming_revision
end submodule version_control_branches_mod
