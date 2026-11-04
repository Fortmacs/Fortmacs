! test_project_mod.f90 -- tests for project_mod.
! Ports GNU Emacs 31.1 test/lisp/progmodes/project-tests.el in part;
! project/quoted-directory, project-find-regexp and
! project-find-regexp-with-prefix wait on xref_mod, and the
! project-name-<vc> tests need directory-local variables.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> Ports of project-tests.el: project-ignores, project-ignores-bug-50240,
!> project-vc-recognizes-git, project-vc-extra-root-markers-supports-wildcards,
!> project-vc-supports-project-in-different-dir,
!> project-vc-supports-files-in-subdirectory,
!> project-vc-ignores-in-external-directory,
!> project-vc-nonexistent-directory-no-error, project-try-vc-uses-cache and
!> project-try-vc-invalidates-cache.  The tests run in the Fortmacs
!> repository (as upstream's run in Emacs's): its test directory takes the
!> place of test/lisp, test/project-resources (with the files etc and
!> foo) that of project-resources, and test/bookmark-resources (marked by
!> test.bmk) that of autoconf-resources.  Directory-local variables are
!> not ported, so the tests that set `project_vc_ignores' in
!> project-resources/.dir-locals.el set it globally.  The VC tests are
!> skipped outside of a Git checkout.  Own tests: a project from a
!> procedure of `project_find_functions', the known projects list, the
!> buffer conditions, `combine_project_directories' and
!> `subtract_project_directories', and the ignores of a VC-aware project.
module test_project_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use string_type_mod, only: string_type
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, close_buffer, get_or_make_buffer, &
                          generate_new_buffer_name
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string
    use search_mod, only: string_match
    use fileio_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, file_exists, is_file_directory, &
                          create_directory_internal, write_region, expand_file_name, directory_file_name, &
                          file_name_nondirectory
    use files_mod, only: delete_directory, executable_find, file_relative_name, is_file_equal
    use bindings_mod, only: init_bindings
    use value_mod, only: value_type, make_keymap_ref, command_type
    use keymap_mod, only: key_parse, lookup_key, current_global_map
    use callint_mod, only: command_of
    use commands_mod, only: commands
    use vc_hooks_mod, only: vc_handled_backends
    use vc_mod, only: vc_responsible_backend
    use vc_hooks_mod, only: vc_backends
    use project_mod, only: project_type, transient_project_type, vc_project_type, &
                           get_current_project, project_find_functions, try_vc_project, project_clear_cache, &
                           project_vc_extra_root_markers, project_vc_ignores, project_vc_backend_markers, &
                           project_vc_non_essential_cache_timeout, project_vc_backend_marker_type, &
                           project_list_file, remember_project, forget_project, known_project_roots, &
                           combine_project_directories, subtract_project_directories, project_vc_ignores_of, &
                           is_project_buffer_matching, project_close_buffer_conditions, project_buffers_to_close
    implicit none(type, external)
    private

    public :: collect_project_mod

    !> project-tests--trivial: a project with a root and its ignores.
    type, extends(project_type) :: trivial_project_type
        character(len=:), allocatable :: directory
        type(string_type), allocatable :: ignore_list(:)
    contains
        procedure :: root => trivial_root
        procedure :: ignores => trivial_ignores
    end type trivial_project_type

    !> The temporary directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-project/"

    !> The project the finder of the own test gives.
    type(transient_project_type) :: found_project

contains

    !> Collect all exported unit tests.
    subroutine collect_project_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("project-ignores", test_ignores), &
                    new_unittest("project-ignores-bug-50240", test_ignores_bug_50240), &
                    new_unittest("project-vc-recognizes-git", test_vc_recognizes_git), &
                    new_unittest("project-vc-extra-root-markers-supports-wildcards", test_extra_root_markers_wildcards), &
                    new_unittest("project-vc-supports-project-in-different-dir", test_project_in_different_dir), &
                    new_unittest("project-vc-supports-files-in-subdirectory", test_files_in_subdirectory), &
                    new_unittest("project-vc-ignores-in-external-directory", test_ignores_in_external_directory), &
                    new_unittest("project-vc-nonexistent-directory-no-error", test_nonexistent_directory), &
                    new_unittest("project-try-vc-uses-cache", test_uses_cache), &
                    new_unittest("project-try-vc-invalidates-cache", test_invalidates_cache), &
                    new_unittest("project-current-from-finder", test_current_from_finder), &
                    new_unittest("project-known-projects", test_known_projects), &
                    new_unittest("project-close-buffer-conditions", test_close_buffer_conditions), &
                    new_unittest("project-combine-directories", test_combine_directories), &
                    new_unittest("project-vc-ignores", test_vc_ignores), &
                    new_unittest("project-prefix-map", test_prefix_map) &
                    ]
    end subroutine collect_project_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! Not a port.
    function trivial_root(self) result(root)
        class(trivial_project_type), intent(in) :: self
        character(len=:), allocatable :: root

        root = self%directory
    end function trivial_root

    ! Not a port.
    function trivial_ignores(self, dir) result(ignores)
        class(trivial_project_type), intent(in) :: self
        character(len=*), intent(in) :: dir
        type(string_type), allocatable :: ignores(:)

        ignores = self%ignore_list
    end function trivial_ignores

    !> The finder of the own test: `found_project' for any directory.
    recursive subroutine find_test_project(dir, project, status)
        character(len=*), intent(in) :: dir
        class(project_type), allocatable, intent(out) :: project
        type(status_type), intent(out) :: status

        status = status_type()
        allocate (project, source=found_project)
    end subroutine find_test_project

    !> The working directory, as a directory name.
    function wd() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(fortmacs_wd)
    end function wd

    !> The temporary directory, made empty.
    function temp_dir() result(dir)
        character(len=:), allocatable :: dir
        type(status_type) :: st

        dir = wd()//subdir
        if (is_file_directory(dir)) call delete_directory(dir, st, .true.)
        call create_directory_internal(wd()//"build", st)
        call create_directory_internal(dir, st)
    end function temp_dir

    !> The project resources directory.
    function resource_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = wd()//"test/project-resources/"
    end function resource_directory

    !> Write TEXT to FILE.
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *project-test*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> A list of the strings given.
    function strings(a1, a2) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2
        type(string_type), allocatable :: list(:)

        if (present(a2)) then
            allocate (list(2))
            list(2)%text = a2
        else
            allocate (list(1))
        end if

        list(1)%text = a1
    end function strings

    !> The base names of FILES, joined with spaces.
    function base_names(files) result(s)
        type(string_type), intent(in) :: files(:)
        character(len=:), allocatable :: s
        integer :: k

        s = ""

        do k = 1, size(files)
            if (k > 1) s = s//" "
            s = s//file_name_nondirectory(files(k)%text)
        end do
    end function base_names

    !> Set up the editor, a buffer in the working directory, the settings'
    !> defaults and an empty cache.
    subroutine setup()
        type(handle_type) :: h
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call get_or_make_buffer(" *project-test-main*", h, st)
        call set_buffer(h, st)
        buffers(current)%directory = wd()
        call project_clear_cache()
        project_vc_extra_root_markers = [character(len=1) ::]
        project_vc_ignores = [character(len=1) ::]
        project_vc_non_essential_cache_timeout = 300
    end subroutine setup

    !> Whether the working directory is in a Git checkout (else the test is
    !> skipped); only Git is handled.
    logical function in_git(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path
        type(status_type) :: st
        integer :: b

        call setup()
        in_git = .false.
        call executable_find("git", path)

        if (len(path) == 0) then
            call skip_test(error, "no git")
            return
        end if

        b = vc_responsible_backend(wd(), st, no_error=.true.)

        if (b > 0) then
            in_git = vc_backends(b)%name == "Git"
        end if

        if (.not. in_git) call skip_test(error, "not in a Git checkout")
        vc_handled_backends = [character(len=8) :: "Git"]
    end function in_git

    !> Handle all backends again.
    subroutine teardown()
        vc_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        project_vc_extra_root_markers = [character(len=1) ::]
        project_vc_ignores = [character(len=1) ::]
        project_vc_non_essential_cache_timeout = 300
        call project_clear_cache()
    end subroutine teardown

    ! ------------------------------------------------------------------
    ! project-tests.el
    ! ------------------------------------------------------------------

    !> Check that `list_files' correctly ignores the files returned by
    !> `ignores' if the root directory is a directory name (Bug#48471).
    subroutine test_ignores(error)
        type(error_type), allocatable, intent(out) :: error
        type(trivial_project_type) :: project
        type(string_type), allocatable :: files(:)
        type(status_type) :: st
        character(len=:), allocatable :: dir, path

        call setup()
        call executable_find("find", path)

        if (len(path) == 0) then
            call skip_test(error, "no find")
            return
        end if

        dir = temp_dir()
        call write_file(dir//"some-file", "")
        call write_file(dir//"ignored-file", "")
        project%directory = file_name_as_directory(dir)
        project%ignore_list = strings("./ignored-file")
        call project%list_files(files, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(files), 1)
        if (allocated(error)) return
        call check(error, file_relative_name(files(1)%text, dir), "some-file")
    end subroutine test_ignores

    !> Check that `list_files' does not ignore all files.
    !> When `ignores' includes a name matching project dir.
    subroutine test_ignores_bug_50240(error)
        type(error_type), allocatable, intent(out) :: error
        type(trivial_project_type) :: project
        type(string_type), allocatable :: files(:)
        type(status_type) :: st
        character(len=:), allocatable :: dir, path, expected

        call setup()
        call executable_find("find", path)

        if (len(path) == 0) then
            call skip_test(error, "no find")
            return
        end if

        dir = temp_dir()
        call write_file(dir//"some-file", "")
        project%directory = file_name_as_directory(dir)
        project%ignore_list = strings(file_name_nondirectory(directory_file_name(dir)))
        call project%list_files(files, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, size(files), 1)
        if (allocated(error)) return
        call expand_file_name("some-file", expected, st, dir)
        call check(error, files(1)%text, expected)
    end subroutine test_ignores_bug_50240

    !> Check that Git repository is detected.
    subroutine test_vc_recognizes_git(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project
        type(status_type) :: st

        if (.not. in_git(error)) return
        call get_current_project(project, st, directory=wd()//"test/")
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("\`test/test_project_mod\.f90", &
                                           file_relative_name(wd()//"test/test_project_mod.f90", project%root()), st) > 0, &
                       project%root())
        end if

        call teardown()
    end subroutine test_vc_recognizes_git

    !> Check that one can add wildcard entries.
    subroutine test_extra_root_markers_wildcards(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project, again
        type(status_type) :: st

        if (.not. in_git(error)) return
        project_vc_extra_root_markers = [character(len=32) :: "test_project_mod.*"]
        call get_current_project(project, st, directory=resource_directory())
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            select type (project)
            type is (vc_project_type)
                call check(error, project%backend, "Git")
            class default
                call check(error, .false., "not a VC-aware project")
            end select
        end if

        if (.not. allocated(error)) then
            call check(error, string_match("/test/\'", project%root(), st) > 0, project%root())
        end if

        ! bug#73801
        if (.not. allocated(error)) then
            call get_current_project(again, st, directory=project%root())
            call check(error, allocated(again), "no project at the root")
            if (.not. allocated(error)) call check(error, again%root(), project%root())
        end if

        call teardown()
    end subroutine test_extra_root_markers_wildcards

    !> Check that it picks up the ignores for a project in another
    !> directory (upstream: from its directory-local variables; here the
    !> global `project_vc_ignores').
    subroutine test_project_in_different_dir(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project
        type(string_type), allocatable :: ignores(:), files(:)
        type(status_type) :: st
        integer :: k
        logical :: has_etc

        if (.not. in_git(error)) return
        project_vc_extra_root_markers = [character(len=32) :: "foo"]
        project_vc_ignores = [character(len=32) :: "etc"]
        call get_current_project(project, st, directory=resource_directory())
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("/test/project-resources/\'", project%root(), st) > 0, project%root())
        end if

        if (.not. allocated(error)) then
            ignores = project%ignores(resource_directory())
            has_etc = .false.

            do k = 1, size(ignores)
                if (ignores(k)%text == "etc") has_etc = .true.
            end do

            call check(error, has_etc, "etc is not ignored")
        end if

        if (.not. allocated(error)) then
            call project%list_files(files, st)
            call check(error, st%ok(), format_error(st))
            if (.not. allocated(error)) call check(error, base_names(files), "foo")
        end if

        call teardown()
    end subroutine test_project_in_different_dir

    !> Check that it lists only files from a repo's subdirectory.
    subroutine test_files_in_subdirectory(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project
        type(string_type), allocatable :: files(:)
        type(status_type) :: st

        if (.not. in_git(error)) return
        project_vc_extra_root_markers = [character(len=32) :: "test_project_mod.f90"]
        project_vc_ignores = [character(len=32) :: "etc"]
        call get_current_project(project, st, directory=resource_directory())
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("/test/\'", project%root(), st) > 0, project%root())
        end if

        if (.not. allocated(error)) then
            call project%list_files(files, st, strings(resource_directory()))
            call check(error, st%ok(), format_error(st))
            if (.not. allocated(error)) call check(error, base_names(files), "foo")
        end if

        call teardown()
    end subroutine test_files_in_subdirectory

    !> Check that it applies `project_vc_ignores' when DIR is external to
    !> the root.
    subroutine test_ignores_in_external_directory(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project
        type(string_type), allocatable :: files(:)
        type(project_vc_backend_marker_type), allocatable :: saved(:)
        type(status_type) :: st

        if (.not. in_git(error)) return
        ! Do not detect VC backend.
        saved = project_vc_backend_markers
        deallocate (project_vc_backend_markers)
        allocate (project_vc_backend_markers(0))
        project_vc_extra_root_markers = [character(len=32) :: "test.bmk"]
        project_vc_ignores = [character(len=32) :: "etc"]
        call get_current_project(project, st, directory=resource_directory()//"../bookmark-resources/")
        project_vc_backend_markers = saved
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("/test/bookmark-resources/\'", project%root(), st) > 0, project%root())
        end if

        if (.not. allocated(error)) then
            call project%list_files(files, st, strings(resource_directory()))
            call check(error, st%ok(), format_error(st))
            if (.not. allocated(error)) call check(error, base_names(files), "foo")
        end if

        call teardown()
    end subroutine test_ignores_in_external_directory

    !> Check that is doesn't error out when the current dir does not
    !> exist.
    subroutine test_nonexistent_directory(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project
        type(status_type) :: st

        if (.not. in_git(error)) return
        project_vc_extra_root_markers = [character(len=32) :: "foo"]
        call get_current_project(project, st, directory=resource_directory()//"foo-456/bar/")
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("/test/project-resources/\'", project%root(), st) > 0, project%root())
        end if

        call teardown()
    end subroutine test_nonexistent_directory

    !> Check that it reuses the value that's already cached.
    subroutine test_uses_cache(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project, cached
        type(status_type) :: st

        if (.not. in_git(error)) return
        project_vc_extra_root_markers = [character(len=32) :: "test_project_mod.*"]
        call get_current_project(project, st, directory=resource_directory())
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("/test/\'", project%root(), st) > 0, project%root())
        end if

        if (.not. allocated(error)) then
            project_vc_extra_root_markers = [character(len=1) ::]
            project_vc_non_essential_cache_timeout = 0.1
            call get_current_project(cached, st, directory=resource_directory())
            call check(error, allocated(cached), "no cached project")
            if (.not. allocated(error)) call check(error, cached%root(), project%root())
        end if

        call teardown()
    end subroutine test_uses_cache

    !> Check that it invalidates the cached value that's too old.
    subroutine test_invalidates_cache(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project, fresh
        type(status_type) :: st

        if (.not. in_git(error)) return
        project_vc_extra_root_markers = [character(len=32) :: "test_project_mod.*"]
        call get_current_project(project, st, directory=resource_directory())
        call check(error, allocated(project), "no project")

        if (.not. allocated(error)) then
            call check(error, string_match("/test/\'", project%root(), st) > 0, project%root())
        end if

        if (.not. allocated(error)) then
            project_vc_extra_root_markers = [character(len=1) ::]
            project_vc_non_essential_cache_timeout = 0
            call get_current_project(fresh, st, directory=resource_directory())
            call check(error, allocated(fresh), "no fresh project")
            if (.not. allocated(error)) call check(error, is_file_equal(fresh%root(), wd()), fresh%root())
        end if

        call teardown()
    end subroutine test_invalidates_cache

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> A procedure of `project_find_functions' gives the current project.
    subroutine test_current_from_finder(error)
        type(error_type), allocatable, intent(out) :: error
        class(project_type), allocatable :: project
        type(status_type) :: st

        call setup()
        found_project%directory = wd()//"test/"
        call project_find_functions%clear()
        call project_find_functions%append(find_test_project)
        call get_current_project(project, st)
        call project_find_functions%clear()
        call project_find_functions%append(try_vc_project)
        call check(error, allocated(project), "no project")
        if (allocated(error)) return
        call check(error, project%root(), wd()//"test/")
        if (allocated(error)) return
        call check(error, project%name(), "test")
    end subroutine test_current_from_finder

    !> `remember_project' and `forget_project' keep the known projects in
    !> `project_list_file', most recent first, in namelist syntax.
    subroutine test_known_projects(error)
        type(error_type), allocatable, intent(out) :: error
        type(transient_project_type) :: a, b
        type(string_type), allocatable :: roots(:)
        type(status_type) :: st
        character(len=:), allocatable :: dir, saved_file
        type(handle_type) :: old, h
        integer(int64) :: inserted
        character(len=:), allocatable :: text

        call setup()
        dir = temp_dir()
        saved_file = project_list_file
        project_list_file = dir//"projects"
        a%directory = dir//"a/"
        b%directory = dir//"b/"
        call create_directory_internal(a%directory, st)
        call create_directory_internal(b%directory, st)
        call remember_project(a, st)
        call remember_project(b, st)
        call remember_project(a, st)
        roots = known_project_roots()
        call check(error, size(roots) >= 2, "too few projects")
        if (allocated(error)) return
        call check(error, roots(1)%text, a%directory)
        if (allocated(error)) return
        call check(error, roots(2)%text, b%directory)
        if (allocated(error)) return
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *project-test*"), h, st)
        call set_buffer(h, st)
        call insert_file(project_list_file)
        text = buffer_string()
        call close_buffer(h, st)
        call set_buffer(old, st)
        call check(error, index(text, "&project") > 0 .and. index(text, "known_project_roots(1:") > 0, text)
        if (allocated(error)) return
        call check(error, index(text, '"'//a%directory//'", "'//b%directory//'"') > 0, text)
        if (allocated(error)) return
        call forget_project(a%directory, st)
        roots = known_project_roots()
        call check(error, roots(1)%text, b%directory)
        call forget_project(b%directory, st)
        project_list_file = saved_file

    contains

        subroutine insert_file(file)
            use fileio_mod, only: insert_file_contents
            character(len=*), intent(in) :: file

            call insert_file_contents(file, .false., inserted, st)
        end subroutine insert_file
    end subroutine test_known_projects

    !> The default `project_close_buffer_conditions': file buffers and
    !> Fundamental mode buffers but internal ones.
    subroutine test_close_buffer_conditions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: plain, internal, old
        type(status_type) :: st
        type(transient_project_type) :: project
        type(handle_type), allocatable :: bufs(:)
        integer :: k
        logical :: has_plain, has_internal

        call setup()
        old = current_buffer()
        call get_or_make_buffer("project-plain", plain, st)
        call get_or_make_buffer(" project-internal", internal, st)
        buffers(plain%index)%major_mode = "fundamental_mode"
        buffers(internal%index)%major_mode = "fundamental_mode"
        call check(error, is_project_buffer_matching(plain, project_close_buffer_conditions), "plain buffer")

        if (.not. allocated(error)) then
            call check(error,.not. is_project_buffer_matching(internal, project_close_buffer_conditions), &
                       "internal buffer")
        end if

        if (.not. allocated(error)) then
            project%directory = temp_dir()
            buffers(plain%index)%directory = project%directory
            buffers(internal%index)%directory = project%directory
            bufs = project_buffers_to_close(project)
            has_plain = .false.
            has_internal = .false.

            do k = 1, size(bufs)
                if (bufs(k)%index == plain%index) has_plain = .true.
                if (bufs(k)%index == internal%index) has_internal = .true.
            end do

            call check(error, has_plain .and. .not. has_internal, "buffers to close")
        end if

        call close_buffer(plain, st)
        call close_buffer(internal, st)
        call set_buffer(old, st)
    end subroutine test_close_buffer_conditions

    !> `combine_project_directories' sorts, drops missing directories and
    !> subdirectories; `subtract_project_directories' drops what is inside.
    subroutine test_combine_directories(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: dirs(:), combined(:), outside(:)
        type(status_type) :: st
        character(len=:), allocatable :: dir

        call setup()
        dir = temp_dir()
        call create_directory_internal(dir//"b", st)
        call create_directory_internal(dir//"b/c", st)
        call create_directory_internal(dir//"a", st)
        allocate (dirs(4))
        dirs(1)%text = dir//"b/c"
        dirs(2)%text = dir//"b"
        dirs(3)%text = dir//"missing"
        dirs(4)%text = dir//"a"
        combined = combine_project_directories(dirs)
        call check(error, size(combined), 2)
        if (allocated(error)) return
        call check(error, combined(1)%text, dir//"a/")
        if (allocated(error)) return
        call check(error, combined(2)%text, dir//"b/")
        if (allocated(error)) return
        outside = subtract_project_directories(combined, strings(dir//"b/"))
        call check(error, size(outside), 1)
        if (allocated(error)) return
        call check(error, outside(1)%text, dir//"a/")
    end subroutine test_combine_directories

    !> The ignores of a VC-aware project without a backend: the extra
    !> ignores, then the directories of `vc_directory_exclusions'.
    subroutine test_vc_ignores(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: ignores(:)

        call setup()
        ignores = project_vc_ignores_of(wd(), "", strings("*.o", "./build/"))
        call check(error, size(ignores) > 2, "too few ignores")
        if (allocated(error)) return
        call check(error, ignores(1)%text, "*.o")
        if (allocated(error)) return
        call check(error, ignores(2)%text, "./build/")
        if (allocated(error)) return
        call check(error, ignores(3)%text, "SCCS/")
    end subroutine test_vc_ignores

    !> C-x p is `project_prefix_map': C-x p f is `find_project_file', C-x p
    !> C-b `list_project_buffers', C-x p C-x s `save_some_project_buffers'.
    subroutine test_prefix_map(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call check(error, binding_is("C-x p f", commands%find_project_file), "C-x p f")
        if (allocated(error)) return
        call check(error, binding_is("C-x p C-b", commands%list_project_buffers), "C-x p C-b")
        if (allocated(error)) return
        call check(error, binding_is("C-x p C-x s", commands%save_some_project_buffers), "C-x p C-x s")
        if (allocated(error)) return
        call check(error, binding_is("C-x p p", commands%switch_project), "C-x p p")

    contains

        logical function binding_is(keys, command)
            character(len=*), intent(in) :: keys
            type(command_type), intent(in), target :: command
            type(value_type), allocatable :: events(:)
            type(value_type) :: binding
            type(status_type) :: st

            binding_is = .false.
            call key_parse(keys, events, st)
            if (.not. st%ok()) return
            call lookup_key(make_keymap_ref(current_global_map()), events, binding, st)
            if (.not. st%ok()) return
            binding_is = associated(command_of(binding), command)
        end function binding_is
    end subroutine test_prefix_map
end module test_project_mod
