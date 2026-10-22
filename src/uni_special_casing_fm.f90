! uni_special_casing_fm.f90 -- title case and special casing.  GENERATED
! by tools/gen-unispecial.sh from admin/unidata/UnicodeData.txt and
! admin/unidata/SpecialCasing.txt of GNU Emacs 31.1, the sources of
! lisp/international/uni-titlecase.el and uni-special-*.el.  Do not edit.
!
! Copyright (C) 1991-2026 Unicode, Inc. (the data; see
! admin/unidata/copyright.html upstream)
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

!> The title case mappings that differ from the simple upper case
!> mapping (`title_keys` to `title_values`), and the unconditional
!> special casings that are not a single character: for each of lower
!> (1), title (2) and upper (3) case, the characters (`special_keys_N`),
!> the length of their mapping (`special_lengths_N`) and up to three
!> characters of it (`special_values_N`, three per key, 0 for unused).
!> Used by casefiddle_fm.
module uni_special_casing_fm
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none(type, external)
    private

    public :: title_count, title_keys, title_values
    public :: special_count_1, special_keys_1, special_lengths_1, special_values_1
    public :: special_count_2, special_keys_2, special_lengths_2, special_values_2
    public :: special_count_3, special_keys_3, special_lengths_3, special_values_3

    integer, parameter :: title_count = 58

    integer(int32), parameter :: title_keys(title_count) = [ &
                                 452, 453, 454, 455, 456, 457, 458, 459, 460, 497, &
                                 498, 499, 4304, 4305, 4306, 4307, 4308, 4309, 4310, 4311, &
                                 4312, 4313, 4314, 4315, 4316, 4317, 4318, 4319, 4320, 4321, &
                                 4322, 4323, 4324, 4325, 4326, 4327, 4328, 4329, 4330, 4331, &
                                 4332, 4333, 4334, 4335, 4336, 4337, 4338, 4339, 4340, 4341, &
                                 4342, 4343, 4344, 4345, 4346, 4349, 4350, 4351 &
                                 ]

    integer(int32), parameter :: title_values(title_count) = [ &
                                 453, 453, 453, 456, 456, 456, 459, 459, 459, 498, &
                                 498, 498, 4304, 4305, 4306, 4307, 4308, 4309, 4310, 4311, &
                                 4312, 4313, 4314, 4315, 4316, 4317, 4318, 4319, 4320, 4321, &
                                 4322, 4323, 4324, 4325, 4326, 4327, 4328, 4329, 4330, 4331, &
                                 4332, 4333, 4334, 4335, 4336, 4337, 4338, 4339, 4340, 4341, &
                                 4342, 4343, 4344, 4345, 4346, 4349, 4350, 4351 &
                                 ]

    integer, parameter :: special_count_1 = 1

    integer(int32), parameter :: special_keys_1(special_count_1) = [ &
                                 304 &
                                 ]

    integer(int32), parameter :: special_lengths_1(special_count_1) = [ &
                                 2 &
                                 ]

    integer(int32), parameter :: special_values_1(3*special_count_1) = [ &
                                 105, 775, 0 &
                                 ]

    integer, parameter :: special_count_2 = 48

    integer(int32), parameter :: special_keys_2(special_count_2) = [ &
                                 223, 329, 496, 912, 944, 1415, 7830, 7831, 7832, 7833, &
                                 7834, 8016, 8018, 8020, 8022, 8114, 8116, 8118, 8119, 8130, &
                                 8132, 8134, 8135, 8146, 8147, 8150, 8151, 8162, 8163, 8164, &
                                 8166, 8167, 8178, 8180, 8182, 8183, 64256, 64257, 64258, 64259, &
                                 64260, 64261, 64262, 64275, 64276, 64277, 64278, 64279 &
                                 ]

    integer(int32), parameter :: special_lengths_2(special_count_2) = [ &
                                 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, &
                                 2, 2, 3, 3, 3, 2, 2, 2, 3, 2, &
                                 2, 2, 3, 3, 3, 2, 3, 3, 3, 2, &
                                 2, 3, 2, 2, 2, 3, 2, 2, 2, 3, &
                                 3, 2, 2, 2, 2, 2, 2, 2 &
                                 ]

    integer(int32), parameter :: special_values_2(3*special_count_2) = [ &
                                 83, 115, 0, 700, 78, 0, 74, 780, 0, 921, &
                                 776, 769, 933, 776, 769, 1333, 1410, 0, 72, 817, &
                                 0, 84, 776, 0, 87, 778, 0, 89, 778, 0, &
                                 65, 702, 0, 933, 787, 0, 933, 787, 768, 933, &
                                 787, 769, 933, 787, 834, 8122, 837, 0, 902, 837, &
                                 0, 913, 834, 0, 913, 834, 837, 8138, 837, 0, &
                                 905, 837, 0, 919, 834, 0, 919, 834, 837, 921, &
                                 776, 768, 921, 776, 769, 921, 834, 0, 921, 776, &
                                 834, 933, 776, 768, 933, 776, 769, 929, 787, 0, &
                                 933, 834, 0, 933, 776, 834, 8186, 837, 0, 911, &
                                 837, 0, 937, 834, 0, 937, 834, 837, 70, 102, &
                                 0, 70, 105, 0, 70, 108, 0, 70, 102, 105, &
                                 70, 102, 108, 83, 116, 0, 83, 116, 0, 1348, &
                                 1398, 0, 1348, 1381, 0, 1348, 1387, 0, 1358, 1398, &
                                 0, 1348, 1389, 0 &
                                 ]

    integer, parameter :: special_count_3 = 102

    integer(int32), parameter :: special_keys_3(special_count_3) = [ &
                                 223, 329, 496, 912, 944, 1415, 7830, 7831, 7832, 7833, &
                                 7834, 8016, 8018, 8020, 8022, 8064, 8065, 8066, 8067, 8068, &
                                 8069, 8070, 8071, 8072, 8073, 8074, 8075, 8076, 8077, 8078, &
                                 8079, 8080, 8081, 8082, 8083, 8084, 8085, 8086, 8087, 8088, &
                                 8089, 8090, 8091, 8092, 8093, 8094, 8095, 8096, 8097, 8098, &
                                 8099, 8100, 8101, 8102, 8103, 8104, 8105, 8106, 8107, 8108, &
                                 8109, 8110, 8111, 8114, 8115, 8116, 8118, 8119, 8124, 8130, &
                                 8131, 8132, 8134, 8135, 8140, 8146, 8147, 8150, 8151, 8162, &
                                 8163, 8164, 8166, 8167, 8178, 8179, 8180, 8182, 8183, 8188, &
                                 64256, 64257, 64258, 64259, 64260, 64261, 64262, 64275, 64276, 64277, &
                                 64278, 64279 &
                                 ]

    integer(int32), parameter :: special_lengths_3(special_count_3) = [ &
                                 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, &
                                 2, 2, 3, 3, 3, 2, 2, 2, 2, 2, &
                                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                                 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, &
                                 2, 2, 2, 3, 2, 3, 3, 2, 3, 3, &
                                 3, 2, 2, 3, 2, 2, 2, 2, 3, 2, &
                                 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, &
                                 2, 2 &
                                 ]

    integer(int32), parameter :: special_values_3(3*special_count_3) = [ &
                                 83, 83, 0, 700, 78, 0, 74, 780, 0, 921, &
                                 776, 769, 933, 776, 769, 1333, 1362, 0, 72, 817, &
                                 0, 84, 776, 0, 87, 778, 0, 89, 778, 0, &
                                 65, 702, 0, 933, 787, 0, 933, 787, 768, 933, &
                                 787, 769, 933, 787, 834, 7944, 921, 0, 7945, 921, &
                                 0, 7946, 921, 0, 7947, 921, 0, 7948, 921, 0, &
                                 7949, 921, 0, 7950, 921, 0, 7951, 921, 0, 7944, &
                                 921, 0, 7945, 921, 0, 7946, 921, 0, 7947, 921, &
                                 0, 7948, 921, 0, 7949, 921, 0, 7950, 921, 0, &
                                 7951, 921, 0, 7976, 921, 0, 7977, 921, 0, 7978, &
                                 921, 0, 7979, 921, 0, 7980, 921, 0, 7981, 921, &
                                 0, 7982, 921, 0, 7983, 921, 0, 7976, 921, 0, &
                                 7977, 921, 0, 7978, 921, 0, 7979, 921, 0, 7980, &
                                 921, 0, 7981, 921, 0, 7982, 921, 0, 7983, 921, &
                                 0, 8040, 921, 0, 8041, 921, 0, 8042, 921, 0, &
                                 8043, 921, 0, 8044, 921, 0, 8045, 921, 0, 8046, &
                                 921, 0, 8047, 921, 0, 8040, 921, 0, 8041, 921, &
                                 0, 8042, 921, 0, 8043, 921, 0, 8044, 921, 0, &
                                 8045, 921, 0, 8046, 921, 0, 8047, 921, 0, 8122, &
                                 921, 0, 913, 921, 0, 902, 921, 0, 913, 834, &
                                 0, 913, 834, 921, 913, 921, 0, 8138, 921, 0, &
                                 919, 921, 0, 905, 921, 0, 919, 834, 0, 919, &
                                 834, 921, 919, 921, 0, 921, 776, 768, 921, 776, &
                                 769, 921, 834, 0, 921, 776, 834, 933, 776, 768, &
                                 933, 776, 769, 929, 787, 0, 933, 834, 0, 933, &
                                 776, 834, 8186, 921, 0, 937, 921, 0, 911, 921, &
                                 0, 937, 834, 0, 937, 834, 921, 937, 921, 0, &
                                 70, 70, 0, 70, 73, 0, 70, 76, 0, 70, &
                                 70, 73, 70, 70, 76, 83, 84, 0, 83, 84, &
                                 0, 1348, 1350, 0, 1348, 1333, 0, 1348, 1339, 0, &
                                 1358, 1350, 0, 1348, 1341, 0 &
                                 ]
end module uni_special_casing_fm
