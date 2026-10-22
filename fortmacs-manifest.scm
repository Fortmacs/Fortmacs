;;; fortmacs-manifest.scm -- Guix manifest: Fortmacs built from this
;;; checkout, with the tools to work on it.
;;;
;;; Copyright (C) 2026 The Fortmacs authors
;;;
;;; This file is part of Fortmacs.
;;;
;;; Fortmacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Fortmacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Usage, from the top of the checkout:
;;;
;;;   guix shell -m fortmacs-manifest.scm -- fortmacs FILE
;;;   guix shell -m fortmacs-manifest.scm      # a shell with fortmacs, fpm, gfortran
;;;   guix build -m fortmacs-manifest.scm      # just build
;;;
;;; Guix compiles everything: fpm from its single-file release (the same
;;; 0.13.0 that CI uses; not every Guix channel has it), then Fortmacs
;;; from the files Git tracks in this checkout, running its tests.  The
;;; build is offline, so the test framework (test-drive) comes in as a
;;; Guix origin rather than being fetched by fpm.

(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages))

(define %source-dir (dirname (current-filename)))

(define gfortran-toolchain (specification->package "gfortran-toolchain"))

(define fpm
  (package
    (name "fpm")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fortran-lang/fpm/releases/download/v"
                                  version "/fpm-" version ".F90"))
              (sha256
               (base32 "1llcpkzkznwppsld2ycvm0qflv60mhkvm208m9dj2pqly5ngy700"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;the single-file release has none
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (copy-file source "fpm.F90")))
          (delete 'configure)
          (replace 'build
            (lambda _
              (mkdir "mod")
              (invoke "gfortran" "-O1" "-fopenmp" "-J" "mod" "-o" "fpm" "fpm.F90")))
          (replace 'install
            (lambda _
              (install-file "fpm" (string-append #$output "/bin")))))))
    (native-inputs (list gfortran-toolchain))
    (home-page "https://fpm.fortran-lang.org")
    (synopsis "Fortran Package Manager")
    (description "fpm builds Fortran projects and fetches their dependencies.")
    (license license:expat)))

(define test-drive-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/fortran-lang/test-drive")
          (commit "v0.6.1")))
    (file-name (git-file-name "test-drive" "0.6.1"))
    (sha256
     (base32 "1x6ia0ld3w8qzk2jiip6nqq34qk2f2lrikrm9n7yb2kdfrbfd8qv"))))

(define fortmacs
  (package
    (name "fortmacs")
    (version "0.0.0-git")
    (source (local-file %source-dir "fortmacs-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'use-local-test-drive
            (lambda _
              ;; No network in the build: fpm takes test-drive from here.
              (copy-recursively #$test-drive-source "test-drive")
              (substitute* "fpm.toml"
                (("^test-drive\\.git = .*")
                 "test-drive.path = \"test-drive\"\n")
                (("^test-drive\\.tag = .*") ""))
              (setenv "HOME" (getcwd))))
          (replace 'build
            (lambda _
              (invoke "fpm" "build" "--profile" "release")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "fpm" "test" "--profile" "release"))))
          (replace 'install
            (lambda _
              ;; The binary, plus the library and module files that
              ;; `fortmacs --recompile` builds init.f90 against.
              (invoke "fpm" "install" "--profile" "release"
                      "--prefix" #$output))))))
    (native-inputs (list fpm))
    ;; `fortmacs --recompile` runs gfortran.
    (propagated-inputs (list gfortran-toolchain))
    (home-page "https://github.com/vermi-culous/Fortmacs")
    (synopsis "GNU Emacs 31.1 rewritten in Fortran")
    (description "Fortmacs is a rewrite of GNU Emacs 31.1 in modern Fortran,
configured in Fortran too.")
    (license license:gpl3+)))

(packages->manifest
 (list fortmacs
       fpm
       gfortran-toolchain
       (specification->package "git")
       (specification->package "nss-certs")))
