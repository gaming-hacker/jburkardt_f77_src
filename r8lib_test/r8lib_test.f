      program main

c*********************************************************************72
c
cc r8lib_test() tests r8lib().
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 July 2021
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'r8lib_test():'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test r8lib().'

      call i4int_to_r8int_test ( )

      call perm_uniform_test ( )

      call r8_abs_test ( )
      call r8_acos_test ( )
      call r8_acosh_test ( )
      call r8_asinh_test ( )
      call r8_add_test ( )
      call r8_agm_test ( )
      call r8_atan_test ( )
      call r8_atanh_test ( )
      call r8_big_test ( )
      call r8_cas_test ( )
      call r8_ceiling_test ( )
      call r8_choose_test ( )
      call r8_cosd_test ( )
      call r8_cotd_test ( )
      call r8_cscd_test ( )
      call r8_cube_root_test ( )
      call r8_diff_test ( )
      call r8_digit_test ( )
      call r8_e_test ( )
      call r8_epsilon_test ( )
      call r8_epsilon_compute_test ( )
      call r8_factorial_test ( )
      call r8_factorial2_test ( )
      call r8_fall_test ( )
      call r8_fractional_test ( )
      call r8_gamma_test ( )
      call r8_gamma_log_test ( )
      call r8_huge_test ( )
      call r8_log_2_test ( )
      call r8_log_b_test ( )
      call r8_mant_test ( )
      call r8_max_test ( )
      call r8_min_test ( )
      call r8_mod_test ( )
      call r8_modp_test ( )
      call r8_mop_test ( )
      call r8_nint_test ( )
      call r8_normal_01_test ( )
      call r8_normal_ab_test ( )
      call r8_pi_test ( )
      call r8_power_test ( )
      call r8_power_fast_test ( )
      call r8_rise_test ( )
      call r8_round2_test ( )
      call r8_roundb_test ( )
      call r8_roundx_test ( )
      call r8_secd_test ( )
      call r8_sign_test ( )
      call r8_sign3_test ( )
      call r8_sind_test ( )
      call r8_swap_test ( )
      call r8_swap3_test ( )
      call r8_tand_test ( )
      call r8_to_i4_test ( )
      call r8_to_r8_discrete_test ( )
      call r8_uniform_01_test ( )
      call r8_uniform_ab_test ( )
      call r8_walsh_1d_test ( )
      call r8_wrap_test ( )

      call r82col_print_part_test ( )

      call r82poly2_type_test ( )

      call r82row_order_type_test ( )
      call r82row_part_quick_a_test ( )
      call r82row_print_part_test ( )
      call r82row_sort_heap_index_a_test ( )
      call r82row_sort_quick_a_test ( )

      call r83col_print_part_test ( )

      call r83row_print_part_test ( )

      call r8block_expand_linear_test ( )
      call r8block_print_test ( )

      call r8col_find_test ( )
      call r8col_insert_test ( )
      call r8col_part_quick_a_test ( )
      call r8col_sort_heap_a_test ( )
      call r8col_sort_heap_index_a_test ( )
      call r8col_sort_quick_a_test ( )
      call r8col_sorted_tol_undex_test ( )
      call r8col_sorted_tol_unique_test ( )
      call r8col_sorted_tol_unique_count_test ( )
      call r8col_max_test ( )
      call r8col_mean_test ( )
      call r8col_min_test ( )
      call r8col_sum_test ( )
      call r8col_permute_test ( )
      call r8col_sortr_a_test ( )
      call r8col_swap_test ( )
      call r8col_to_r8vec_test ( )
      call r8col_tol_undex_test ( )
      call r8col_undex_test ( )
      call r8col_unique_count_test ( )
      call r8col_variance_test ( )

      call r8r8vec_index_insert_unique_test ( )

      call r8r8r8vec_index_insert_unique_test ( )

      call r8int_to_i4int_test ( )

      call r8mat_cholesky_inverse_test ( )
      call r8mat_cholesky_solve_test ( )
      call r8mat_cholesky_solve_upper_test ( )
      call r8mat_det_2d_test ( )
      call r8mat_det_3d_test ( )
      call r8mat_det_4d_test ( )
      call r8mat_det_5d_test ( )
      call r8mat_expand_linear_test ( )
      call r8mat_expand_linear2_test ( )
      call r8mat_fs_test ( )
      call r8mat_fss_test ( )
      call r8mat_givens_post_test ( )
      call r8mat_givens_pre_test ( )
      call r8mat_hess_test ( )
      call r8mat_house_axh_test ( )
      call r8mat_house_form_test ( )
      call r8mat_house_post_test ( )
      call r8mat_house_pre_test ( )
      call r8mat_indicator_test ( )
      call r8mat_inverse_2d_test ( )
      call r8mat_inverse_3d_test ( )
      call r8mat_inverse_4d_test ( )
      call r8mat_jac_test ( )
      call r8mat_kronecker_test ( )
      call r8mat_l_inverse_test ( )
      call r8mat_l_print_test ( )
      call r8mat_l1_inverse_test ( )
      call r8mat_lu_test ( )
      call r8mat_max_test ( )
      call r8mat_max_index_test ( )
      call r8mat_maxcol_minrow_test ( )
      call r8mat_maxrow_mincol_test ( )
      call r8mat_min_test ( )
      call r8mat_min_index_test ( )
      call r8mat_mincol_maxrow_test ( )
      call r8mat_minrow_maxcol_test ( )
      call r8mat_mm_test ( )
      call r8mat_mv_test ( )
      call r8mat_nint_test ( )
      call r8mat_nonzeros_test ( )
      call r8mat_norm_fro_test ( )
      call r8mat_norm_l1_test ( )
      call r8mat_nullspace_test ( )
      call r8mat_nullspace_size_test ( )
      call r8mat_orth_uniform_test ( )
      call r8mat_plot_test ( )
      call r8mat_power_method_test ( )
      call r8mat_print_test ( )
      call r8mat_print_some_test ( )
      call r8mat_ref_test ( )
      call r8mat_rref_test ( )
      call r8mat_solve_test ( )
      call r8mat_solve_2d_test ( )
      call r8mat_solve_3d_test ( )
      call r8mat_solve2_test ( )
      call r8mat_sub_test ( )
      call r8mat_symm_jacobi_test ( )
      call r8mat_to_r8plu_test ( )
      call r8mat_trace_test ( )
      call r8mat_transpose_test ( )
      call r8mat_transpose_print_test ( )
      call r8mat_u_inverse_test ( )
      call r8mat_u1_inverse_test ( )
      call r8mat_uniform_ab_test ( )

      call r8plu_det_test ( )
      call r8plu_inverse_test ( )
      call r8plu_mul_test ( )
      call r8plu_sol_test ( )
      call r8plu_to_r8mat_test ( )

      call r8poly_degree_test ( )
      call r8poly_deriv_test ( )
      call r8poly_lagrange_coef_test ( )
      call r8poly_lagrange_0_test ( )
      call r8poly_lagrange_1_test ( )
      call r8poly_lagrange_2_test ( )
      call r8poly_lagrange_factor_test ( )
      call r8poly_lagrange_val_test ( )
      call r8poly_print_test ( )
      call r8poly_value_horner_test ( )
      call r8poly_values_horner_test ( )

      call r8poly2_ex_test ( )
      call r8poly2_ex2_test ( )
      call r8poly2_root_test ( )
      call r8poly2_val_test ( )
      call r8poly2_val2_test ( )

      call r8poly3_root_test ( )

      call r8poly4_root_test ( )

      call r8row_max_test ( )
      call r8row_mean_test ( )
      call r8row_min_test ( )
      call r8row_part_quick_a_test ( )
      call r8row_sort_heap_a_test ( )
      call r8row_sort_heap_index_a_test ( )
      call r8row_sort_quick_a_test ( )
      call r8row_sum_test ( )
      call r8row_swap_test ( )
      call r8row_to_r8vec_test ( )
      call r8row_variance_test ( )

      call r8slmat_print_test ( )

      call r8vec_amax_test ( )
      call r8vec_amin_test ( )
      call r8vec_bracket_test ( )
      call r8vec_bracket2_test ( )
      call r8vec_bracket3_test ( )
      call r8vec_bracket5_test ( )
      call r8vec_chebyspace_test ( )
      call r8vec_concatenate_test ( )
      call r8vec_convolution_test ( )
      call r8vec_convolution_circ_test ( )
      call r8vec_dif_test ( )
      call r8vec_direct_product_test ( )
      call r8vec_direct_product2_test ( )
      call r8vec_even_test ( )
      call r8vec_even2_test ( )
      call r8vec_even3_test ( )
      call r8vec_expand_linear_test ( )
      call r8vec_frac_test ( )
      call r8vec_heap_d_extract_test ( )
      call r8vec_heap_d_insert_test ( )
      call r8vec_heap_d_max_test ( )
      call r8vec_histogram_test ( )
      call r8vec_house_column_test ( )
      call r8vec_index_delete_all_test ( )
      call r8vec_index_delete_dupes_test ( )
      call r8vec_index_delete_one_test ( )
      call r8vec_index_insert_test ( )
      call r8vec_index_order_test ( )
      call r8vec_index_insert_unique_test ( )
      call r8vec_index_search_test ( )
      call r8vec_index_sorted_range_test ( )
      call r8vec_indexed_heap_d_test ( )
      call r8vec_indexed_heap_d_extract_test ( )
      call r8vec_indexed_heap_d_insert_test ( )
      call r8vec_indexed_heap_d_max_test ( )
      call r8vec_indicator0_test ( )
      call r8vec_legendre_test ( )
      call r8vec_linspace_test ( )
      call r8vec_max_test ( )
      call r8vec_max_index_test ( )
      call r8vec_mean_test ( )
      call r8vec_median_test ( )
      call r8vec_midspace_test ( )
      call r8vec_min_test ( )
      call r8vec_min_index_test ( )
      call r8vec_nint_test ( )
      call r8vec_norm_l0_test ( )
      call r8vec_norm_l1_test ( )
      call r8vec_norm_l2_test ( )
      call r8vec_norm_li_test ( )
      call r8vec_normal_01_test ( )
      call r8vec_normalize_l1_test ( )
      call r8vec_order_type_test ( )
      call r8vec_permute_test ( )
      call r8vec_polarize_test ( )
      call r8vec_print_test ( )
      call r8vec_reverse_test ( )
      call r8vec_rotate_test ( )
      call r8vec_search_binary_a_test ( )
      call r8vec_sort_bubble_a_test ( )
      call r8vec_sort_heap_a_test ( )
      call r8vec_sort_heap_d_test ( )
      call r8vec_sort_heap_index_a_test ( )
      call r8vec_sort_heap_index_d_test ( )
      call r8vec_sort_heap_mask_a_test ( )
      call r8vec_sort_insert_a_test ( )
      call r8vec_sort_insert_index_a_test ( )
      call r8vec_sort_quick_a_test ( )
      call r8vec_sorted_merge_a_test ( )
      call r8vec_sorted_nearest_test ( )
      call r8vec_sorted_range_test ( )
      call r8vec_sorted_split_test ( )
      call r8vec_sorted_undex_test ( )
      call r8vec_sorted_unique_test ( )
      call r8vec_sorted_unique_count_test ( )
      call r8vec_sorted_unique_hist_test ( )
      call r8vec_split_test ( )
      call r8vec_transpose_print_test ( )
      call r8vec_undex_test ( )
      call r8vec_uniform_01_test ( )
      call r8vec_uniform_ab_test ( )
      call r8vec_variance_test ( )

      call r8vec2_sort_a_test ( )
      call r8vec2_sort_d_test ( )
      call r8vec2_sort_heap_index_a_test ( )
      call r8vec2_sorted_unique_test ( )
      call r8vec2_sorted_unique_index_test ( )
      call r8vec2_sum_max_index_test ( )

      call roots_to_r8poly_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'r8lib_test()'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 0
      end
      subroutine i4int_to_r8int_test ( )

c*********************************************************************72
c
cc I4INT_TO_R8INT_TEST tests I4INT_TO_R8INT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ihi
      integer ilo
      integer ir
      double precision r
      double precision r2
      double precision r8_uniform_ab
      double precision rhi
      double precision rhi2
      double precision rlo
      double precision rlo2
      integer seed
      integer test
      integer test_num

      ilo = 1
      ihi = 11
      rlo = 100.0D+00
      rhi = 200.0D+00
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4INT_TO_R8INT_TEST'
      write ( *, '(a)' ) '  For data in an interval,'
      write ( *, '(a)' ) 
     &  '  I4INT_TO_R8INT converts an integer to a real;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  Integer interval: ', ilo, ihi
      write ( *, '(a,2g14.6)' ) '  Real interval: ', rlo, rhi
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       R             I(R)  R(I(R))'
      write ( *, '(a)' ) ' '

      seed = 123456789

      rlo2 = rlo - 15.0D+00
      rhi2 = rhi + 15.0D+00

      do test = 1, test_num
        r = r8_uniform_ab ( rlo2, rhi2, seed )
        call r8int_to_i4int ( rlo, rhi, r, ilo, ihi, ir )
        call i4int_to_r8int ( ilo, ihi, ir, rlo, rhi, r2 )
        write ( *, '(2x,g14.6,i8,g14.6)' ) r, ir, r2
      end do

      return
      end
      subroutine perm_uniform_test ( )

c*********************************************************************72
c
cc PERM_UNIFORM_TEST tests PERM_UNIFORM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer p(n)
      integer seed
      integer test

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM_UNIFORM_TEST'
      write ( *, '(a)' ) 
     &  '  PERM_UNIFORM randomly selects a permutation.'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, 5

        call perm_uniform ( n, seed, p )
        write ( *, '(2x,10i4)' ) p(1:n)

      end do

      return
      end
      subroutine r8_abs_test ( )

c*********************************************************************72
c
cc R8_ABS_TEST tests R8_ABS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8
      double precision r8_abs
      double precision r8_absolute
      double precision r8_uniform_ab
      double precision r8_hi
      double precision r8_lo
      integer seed
      integer test
      integer test_num

      r8_hi = 5.0D+00
      r8_lo = -3.0D+00
      seed = 123456789
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ABS_TEST'
      write ( *, '(a)' ) '  R8_ABS returns the absolute value of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         R8_ABS(X)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
        r8_absolute = r8_abs ( r8 )
        write ( *, '(2x,f10.6,2x,f10.6)' ) r8, r8_absolute
      end do

      return
      end
      subroutine r8_acos_test ( )

c*********************************************************************72
c
cc R8_ACOS_TEST tests R8_ACOS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c
      double precision r8_acos
      integer test

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ACOS_TEST'
      write ( *, '(a)' ) 
     &  '  R8_ACOS computes the arc-cosine of an angle.' 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       C            R8_ACOS(C)        ACOS(C)'
      write ( *, '(a)' ) ' '

      do test = -1, 13

        c = dble ( test - 6 ) / dble ( 6 )

        if ( -1.0D+00 .le. c .and. c .le. 1.0D+00 ) then
          write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      c, r8_acos ( c ), acos ( c )
        else
          write ( *, '(2x,g14.6,2x,g14.6)' ) 
     &      c, r8_acos ( c )
        end if

      end do

      return
      end
      subroutine r8_acosh_test ( )

c*********************************************************************72
c
cc R8_ACOSH_TEST tests R8_ACOSH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision r8_acosh
      integer test
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ACOSH_TEST'
      write ( *, '(a)' ) 
     &  '  R8_ACOSH computes the arc-hyperbolic-cosine of an angle.' 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            A=R8_ACOSH(X)    COSH(A)'
      write ( *, '(a)' ) ' '

      do test = 0, 8

        x = 1.0D+00 + dble ( test ) / 2.0D+00
        a = r8_acosh ( x )
        x2 = cosh ( a )

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2

      end do

      return
      end
      subroutine r8_add_test ( )

c*********************************************************************72
c
cc R8_ADD_TEST tests R8_ADD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r1
      double precision r2
      double precision r3
      double precision r4
      double precision r8_add
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      integer seed
      integer test

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ADD_TEST'
      write ( *, '(a)' ) '  R8_ADD adds two R8''s.' 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       R1             R2              R3              R4'
      write ( *, '(a)' ) 
     &  '                                      R1+R2     R8_ADD(R1,R2)'
      write ( *, '(a)' ) ' '

      r8_lo = - 500.0D+00
      r8_hi = + 500.0D+00
      seed = 123456789

      do test = 1, 5

        r1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
        r2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
        r3 = r1 + r2
        r4 = r8_add ( r1, r2 )

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    r1, r2, r3, r4

      end do

      return
      end
      subroutine r8_agm_test ( )

c*********************************************************************72
c
cc R8_AGM_TEST tests R8_AGM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx1
      double precision fx2
      integer n_data
      double precision r8_agm
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_AGM_TEST:'
      write ( *, '(a)' ) 
     &  '  R8_AGM computes the arithmetic geometric mean.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '             X              Y              AGM           AGM'
      write ( *, '(a)' ) 
     &  '                                           ' //
     &  'Exact         Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call agm_values ( n_data, x, y, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = r8_agm ( x, y )

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16,2x,g24.16)' ) 
     &    x, y, fx1, fx2

      go to 10

20    continue

      return
      end
      subroutine r8_asinh_test ( )

c*********************************************************************72
c
cc R8_ASINH_TEST tests R8_ASINH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 November 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      integer i
      double precision r8_asinh
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ASINH_TEST'
      write ( *, '(a)' ) '  R8_ASINH computes the inverse hyperbolic'
      write ( *, '(a)' ) '  sine of a given value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       X       R8_ASINH(X)     SINH(R8_ASINH(X))'
      write ( *, '(a)' ) ' '

      do i = 0, 10
        x = 1.0D+00 + dble ( i ) / 5.0D+00
        a = r8_asinh ( x )
        x2 = dsinh ( a )
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2
      end do

      return
      end
      subroutine r8_atan_test ( )

c*********************************************************************72
c
cc R8_ATAN_TEST tests R8_ATAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 8 )

      double precision r8_atan
      integer test
      double precision x
      double precision xtest(test_num)
      double precision y
      double precision ytest(test_num)

      save xtest
      save ytest

      data xtest /
     &   1.0D+00,  1.0D+00,  0.0D+00, -1.0D+00,
     &  -1.0D+00, -1.0D+00,  0.0D+00,  1.0D+00 /
      data ytest /
     &   0.0D+00,  1.0D+00,  1.0D+00,  1.0D+00,
     &   0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ATAN_TEST'
      write ( *, '(a)' )
     &  '  R8_ATAN computes the arc-tangent given Y and X;'
      write ( *, '(a)' )
     &  '  ATAN2 is the system version of this routine.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '       X             Y          ATAN2(Y,X)    R8_ATAN(Y,X)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        x = xtest(test)
        y = ytest(test)
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )
     &    x, y, atan2 ( y, x ), r8_atan ( y, x )
      end do

      return
      end
      subroutine r8_atanh_test ( )

c*********************************************************************72
c
cc R8_ATANH_TEST tests R8_ATANH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 November 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      integer i
      double precision r8_atanh
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ATANH_TEST'
      write ( *, '(a)' ) '  R8_ATANH computes the inverse hyperbolic'
      write ( *, '(a)' ) '  tangent of a given value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      X       R8_ATANH(X)     TANH(R8_ATANH(X))'
      write ( *, '(a)' ) ' '

      do i = -2, 9
        x = dble ( i ) / 10.0D+00
        a = r8_atanh ( x )
        x2 = dtanh ( a )
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, a, x2
      end do

      return
      end
      subroutine r8_big_test ( )

c*********************************************************************72
c
cc R8_BIG_TEST tests R8_BIG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_big

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_BIG_TEST'
      write ( *, '(a)' ) '  R8_BIG returns a "big" R8;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g24.16)' ) '    R8_BIG ( ) =      ',
     &  r8_big ( )

      return
      end
      subroutine r8_cas_test ( )

c*********************************************************************72
c
cc R8_CAS_TEST tests R8_CAS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_cas
      double precision r8_pi
      integer test_num
      parameter ( test_num = 12 )
      integer test
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_CAS_TEST'
      write ( *, '(a)' ) '  R8_CAS evaluates the casine of a number.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        X           R8_CAS ( X )'
      write ( *, '(a)' ) ' '
      do test = 0, test_num
        x = r8_pi ( ) * dble ( test ) / dble ( test_num )
        write ( *, '(2x,g14.6,2x,g14.6)' ) x, r8_cas ( x )
      end do

      return
      end
      subroutine r8_ceiling_test ( )

c*********************************************************************72
c
cc R8_CEILING_TEST tests R8_CEILING.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r8_ceiling
      double precision rval
      double precision rval2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_CEILING_TEST'
      write ( *, '(a)' ) '  R8_CEILING rounds a value up.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        X           R8_CEILING(X)'
      write ( *, '(a)' ) ' '

      do i = -6, 6
        rval = dble ( i ) / 5.0D+00
        rval2 = r8_ceiling ( rval )
        write ( *, '(2x,g14.6,2x,g14.6)' ) rval, rval2
      end do

      return
      end
      subroutine r8_choose_test ( )

c*********************************************************************72
c
cc R8_CHOOSE_TEST tests R8_CHOOSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cnk
      integer k
      integer n
      double precision r8_choose

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_CHOOSE_TEST'
      write ( *, '(a)' ) '  R8_CHOOSE evaluates C(N,K).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N         K       CNK'
 
      do n = 0, 5
        write ( *, '(a)' ) ' '
        do k = 0, n
          cnk = r8_choose ( n, k )
          write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) n, k, cnk
        end do
      end do
 
      return
      end
      subroutine r8_cosd_test ( )

c*********************************************************************72
c
cc R8_COSD_TEST tests R8_COSD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angle
      double precision r8_cosd
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_COSD_TEST'
      write ( *, '(a)' ) '  R8_COSD computes the cosine of an angle'
      write ( *, '(a)' ) '  given in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ANGLE    R8_COSD(ANGLE)'
      write ( *, '(a)' ) ' '
 
      do i = 0, 360, 10
        angle = dble ( i )
        write ( *, '(2x,f8.2,2x,g14.6)' )  angle, r8_cosd ( angle )
      end do
 
      return
      end
      subroutine r8_cotd_test ( )

c*********************************************************************72
c
cc R8_COTD_TEST tests R8_COTD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angle
      double precision r8_cotd
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_COTD_TEST'
      write ( *, '(a)' ) '  R8_COTD computes the cotangent of an angle'
      write ( *, '(a)' ) '  given in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ANGLE    R8_COTD(ANGLE)'
      write ( *, '(a)' ) ' '
 
      do i = 0, 360, 10
        angle = dble ( i )
        if ( mod ( i, 180 ) .eq. 0 ) then
          write ( *, '(2x,f8.2,2x,a)' )  angle, '  Undefined'
        else
          write ( *, '(2x,f8.2,2x,g14.6)' )  angle, r8_cotd ( angle )
        end if
      end do
 
      return
      end
      subroutine r8_cscd_test ( )

c*********************************************************************72
c
cc R8_CSCD_TEST tests R8_CSCD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angle
      double precision r8_cscd
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_CSCD_TEST'
      write ( *, '(a)' ) '  R8_CSCD computes the cosecant of an angle'
      write ( *, '(a)' ) '  given in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ANGLE    R8_CSCD(ANGLE)'
      write ( *, '(a)' ) ' '
 
      do i = 0, 360, 10
        angle = dble ( i )
        if ( mod ( i, 180 ) .eq. 0 ) then
          write ( *, '(2x,f8.2,2x,a)' )  angle, '  Undefined'
        else
          write ( *, '(2x,f8.2,2x,g14.6)' )  angle, r8_cscd ( angle )
        end if
      end do
 
      return
      end
      subroutine r8_cube_root_test ( )

c*********************************************************************72
c
cc R8_CUBE_ROOT_TEST tests R8_CUBE_ROOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      integer i
      double precision r8_cube_root
      double precision r8_uniform_ab
      integer seed
      double precision x1
      double precision y
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_CUBE_ROOT_TEST'
      write ( *, '(a)' ) 
     &  '  R8_CUBE_ROOT computes the cube root of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X               Y               Y^3'
      write ( *, '(a)' ) ' '

      a = -10.0D+00
      b = +10.0D+00
      seed = 123456789

      do i = 1, 10
        x1 = r8_uniform_ab ( a, b, seed )
        y = r8_cube_root ( x1 )
        x2 = y ** 3
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x1, y, x2
      end do

      return
      end
      subroutine r8_diff_test ( )

c*********************************************************************72
c
cc R8_DIFF_TEST tests R8_DIFF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 15 )

      integer ndig
      double precision r8_diff
      integer test
      double precision x
      double precision y_test(test_num)
      double precision y

      save y_test

      data y_test /
     &  0.0625D+00, 0.125D+00, 0.25D+00, 0.50D+00,  0.874D+00,
     &  0.876D+00,  0.90D+00,  0.95D+00, 0.99D+00,  1.0D+00,
     &  1.01D+00,   1.05D+00,  1.10D+00, 3.0D+00,  10.0D+00 /

      ndig = 3
      x = 1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_DIFF_TEST'
      write ( *, '(a)' ) '  R8_DIFF computes a difference X-Y to a'
      write ( *, '(a)' ) '  given number of binary places.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' )
     &  '  For this test, we use ', ndig, ' binary places.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X       Y       X-Y     R8_DIFF(X,Y)'
      write ( *, '(a)' ) ' '
      do test = 1, test_num
        y = y_test(test)
        write ( *, '(4f10.4)' ) x, y, x - y, r8_diff ( x, y, ndig )
      end do

      return
      end
      subroutine r8_digit_test ( )

c*********************************************************************72
c
cc R8_DIGIT_TEST tests R8_DIGIT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer maxdig
      parameter ( maxdig = 20 )

      integer i
      integer digit(-2:maxdig)
      integer idigit
      double precision r8_pi
      double precision x

      x = r8_pi ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_DIGIT_TEST'
      write ( *, '(a)' ) '  R8_DIGIT extracts decimal digits.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g24.16)' ) '  Here, we get digits of ', x
      write ( *, '(a)' ) ' '

      do idigit = -2, maxdig
        call r8_digit ( x, idigit, digit(idigit) )
      end do

      write ( *, '(2x,25i3)' ) ( i, i = -2, maxdig )
      write ( *, '(2x,25i3)' ) digit(-2:maxdig)

      return
      end
      subroutine r8_e_test ( )

c*********************************************************************72
c
cc R8_E_TEST tests R8_E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer n
      double precision r8_e
      double precision value1
      double precision value2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_E_TEST'
      write ( *, '(a)' ) '  R8_E returns the value of E.'
      write ( *, '(a)' ) '  Compare E to (1+1/n)^n'
      value1 = r8_e ( )
      write ( *, '(a,g24.16)' ) '  R8_E =      ', value1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        N     Estimate      Error'
      write ( *, '(a)' ) ' '

      n = 1
      do i = 0, 20
        value2 = ( dble ( n + 1 ) / dble ( n ) ) ** n
        write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) 
     &    n, value2, abs ( value1 - value2 )
        n = n * 2
      end do

      return
      end
      subroutine r8_epsilon_test ( )

c*********************************************************************72
c
cc R8_EPSILON_TEST tests R8_EPSILON.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_epsilon
      double precision r
      double precision s

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_EPSILON_TEST'
      write ( *, '(a)' ) 
     &  '  R8_EPSILON returns the R8 machine precision.'
      write ( *, '(a)' ) ' '

      r = r8_epsilon ( )
      write ( *, '(a,g24.16)' ) '  R = R8_EPSILON()         = ', r

      s = ( 1.0D+00 + r ) - 1.0D+00
      write ( *, '(a,g24.16)' ) '  ( 1 + R ) - 1            = ', s

      s = ( 1.0D+00 + ( r / 2.0D+00 ) ) - 1.0D+00
      write ( *, '(a,g24.16)' ) '  ( 1 + (R/2) ) - 1        = ', s

      return
      end
      subroutine r8_epsilon_compute_test ( )

c*********************************************************************72
c
cc R8_EPSILON_COMPUTE_TEST tests R8_EPSILON_COMPUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_epsilon_compute
      double precision r
      double precision s

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_EPSILON_COMPUTE_TEST'
      write ( *, '(a)' ) 
     &  '  R8_EPSILON_COMPUTE computes the R8 machine precision.'
      write ( *, '(a)' ) ' '

      r = r8_epsilon_compute ( )
      write ( *, '(a,g24.16)' ) '  R = R8_EPSILON_COMPUTE() = ', r

      s = ( 1.0D+00 + r ) - 1.0D+00
      write ( *, '(a,g24.16)' ) '  ( 1 + R ) - 1            = ', s

      s = ( 1.0D+00 + ( r / 2.0D+00 ) ) - 1.0D+00
      write ( *, '(a,g24.16)' ) '  ( 1 + (R/2) ) - 1        = ', s

      return
      end
      subroutine r8_factorial_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL_TEST tests R8_FACTORIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1
      double precision f2
      integer n
      integer n_data
      double precision r8_factorial

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL_TEST'
      write ( *, '(a)' ) 
     &  '  R8_FACTORIAL computes the factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    N                Exact' //
     &  '                  Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial_values ( n_data, n, f1 )

        if ( n_data == 0 ) then
          go to 20
        end if

        f2 = r8_factorial ( n );

        write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) n, f1, f2

      go to 10

20    continue
     
      return
      end
      subroutine r8_factorial2_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL2_TEST tests R8_FACTORIAL2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1
      double precision f2
      integer n
      integer n_data
      double precision r8_factorial2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL2_TEST'
      write ( *, '(a)' ) 
     &  '  R8_FACTORIAL2 computes the double factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    N                Exact' //
     &  '                  Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial2_values ( n_data, n, f1 )

        if ( n_data == 0 ) then
          go to 20
        end if

        f2 = r8_factorial2 ( n );

        write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) n, f1, f2

      go to 10

20    continue
     
      return
      end
      subroutine r8_fall_test ( )

c*********************************************************************72
c
cc R8_FALL_TEST tests R8_FALL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1
      double precision f2
      integer n
      integer n_data
      double precision r8_fall
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FALL_TEST'
      write ( *, '(a)' ) 
     &  '  R8_FALL computes the falling factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    X          N                Exact' //
     &  '                  Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_fall_values ( n_data, x, n, f1 )

        if ( n_data == 0 ) then
          go to 20
        end if

        f2 = r8_fall ( x, n );

        write ( *, '(2x,f8.4,2x,i4,2x,g24.16,2x,g24.16)' ) x, n, f1, f2

      go to 10

20    continue
     
      return
      end
      subroutine r8_fractional_test ( )

c*********************************************************************72
c
cc R8_FRACTIONAL_TEST tests R8_FRACTIONAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fractional
      double precision r8
      double precision r8_fractional
      double precision r8_uniform_ab
      double precision r8_hi
      double precision r8_lo
      integer seed
      integer test
      integer test_num

      r8_hi = 5.0D+00
      r8_lo = -3.0D+00
      seed = 123456789
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FRACTIONAL_TEST'
      write ( *, '(a)' )
     &  '  R8_FRACTIONAL returns the fractional part of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        X           R8_FRACTIONAL(X)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
        fractional = r8_fractional ( r8 )
        write ( *, '(2x,f10.6,2x,f10.6)' ) r8, fractional
      end do

      return
      end
      subroutine r8_gamma_test ( )

c*********************************************************************72
c
cc R8_GAMMA_TEST tests R8_GAMMA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx1
      double precision fx2
      integer n_data
      double precision r8_gamma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_GAMMA_TEST:'
      write ( *, '(a)' ) '  R8_GAMMA evaluates the Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X            GAMMA(X)     R8_GAMMA(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_values ( n_data, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = r8_gamma ( x )

        write ( *, '(2x,f14.6,2x,g24.16,2x,g24.16)' ) x, fx1, fx2

      go to 10

20    continue

      return
      end
      subroutine r8_gamma_log_test ( )

c*********************************************************************72
c
cc R8_GAMMA_LOG_TEST tests R8_GAMMA_LOG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx1
      double precision fx2
      integer n_data
      double precision r8_gamma_log
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_GAMMA_LOG_TEST:'
      write ( *, '(a)' ) '  R8_GAMMA_LOG evaluates '
      write ( *, '(a)' ) '  the logarithm of the Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X           Log(Gamma(X))  R8_GAMMA_LOG(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_log_values ( n_data, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = r8_gamma_log ( x )

        write ( *, '(2x,f24.16,2x,g24.16,2x,g24.16)' ) x, fx1, fx2

      go to 10

20    continue

      return
      end
      subroutine r8_huge_test ( )

c*********************************************************************72
c
cc R8_HUGE_TEST tests R8_HUGE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_huge

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_HUGE_TEST'
      write ( *, '(a)' ) '  R8_HUGE returns a "huge" R8;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g24.16)' ) '    R8_HUGE ( ) =      ',
     &  r8_huge ( )

      return
      end
      subroutine r8_log_2_test ( )

c*********************************************************************72
c
cc R8_LOG_2_TEST tests R8_LOG_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 18 )

      double precision r8_log_2
      integer test
      double precision x
      double precision x_test(test_num)

      save x_test

      data x_test /
     &  0.0D+00,  1.0D+00,  2.0D+00,   3.0D+00,  9.0D+00,
     &  10.0D+00, 11.0D+00, 99.0D+00, 101.0D+00, -1.0D+00,
     &  -2.0D+00, -3.0D+00, -9.0D+00,   0.5D+00,  0.33D+00,
     &   0.25D+00, 0.20D+00, 0.01D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_LOG_2_TEST'
      write ( *, '(a)' ) '  R8_LOG_2 computes the logarithm base 2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    X     R8_LOG_2'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        x = x_test(test)
        write ( *, '( 2g14.6 )' ) x, r8_log_2 ( x )
      end do

      return
      end
      subroutine r8_log_b_test ( )

c*********************************************************************72
c
cc R8_LOG_B_TEST tests R8_LOG_B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 10 )

      double precision b
      double precision b_test(test_num)
      double precision r8_log_b
      integer test
      double precision x

      save b_test

      data b_test /
     &  2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00,
     &  7.0D+00, 8.0D+00, 16.0D+00, 32.0D+00, 256.0D+00 /

      x = 16.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_LOG_B_TEST'
      write ( *, '(a)' ) '  R8_LOG_B computes the logarithm base B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X      B      R8_LOG_B'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        b = b_test(test)

        write ( *, '( 2x,3g14.6, i12 )' ) x, b, r8_log_b ( x, b )

      end do

      return
      end
      subroutine r8_mant_test ( )

c*********************************************************************72
c
cc R8_MANT_TEST tests R8_MANT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer is
      integer l
      double precision r
      double precision x

      x = -314.159D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_MANT_TEST'
      write ( *, '(a)' ) '  R8_MANT decomposes a value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Number to be decomposed:'
      write ( *, '(2x,g14.6)' ) x

      call r8_mant ( x, is, r, l )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a,g14.6,a,i8)' )
     &  '  R8_MANT: X = ', is, ' * ', r, ' * 2 ^ ', l

      return
      end
      subroutine r8_max_test ( )

c*********************************************************************72
c
cc R8_MAX_TEST tests R8_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer i
      double precision r8_hi
      double precision r8_lo
      double precision r8_max
      double precision r8_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_MAX_TEST'
      write ( *, '(a)' ) '  R8_MAX returns the maximum of two R8''s.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       A       B      C=R8_MAX(A,B)'
      write ( *, '(a)' ) ''

      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      do i = 1, 10
        a = r8_uniform_ab ( r8_lo, r8_hi, seed )
        b = r8_uniform_ab ( r8_lo, r8_hi, seed )
        c = r8_max ( a, b )
        write ( *, '(2x,f8.4,2x,f8.4,2x,f8.4)' ) a, b, c
      end do

      return
      end
      subroutine r8_min_test ( )

c*********************************************************************72
c
cc R8_MIN_TEST tests R8_MIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer i
      double precision r8_hi
      double precision r8_lo
      double precision r8_min
      double precision r8_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_MIN_TEST'
      write ( *, '(a)' ) '  R8_MIN returns the minimum of two R8''s.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       A       B      C=R8_MIN(A,B)'
      write ( *, '(a)' ) ''

      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      do i = 1, 10
        a = r8_uniform_ab ( r8_lo, r8_hi, seed )
        b = r8_uniform_ab ( r8_lo, r8_hi, seed )
        c = r8_min ( a, b )
        write ( *, '(2x,f8.4,2x,f8.4,2x,f8.4)' ) a, b, c
      end do

      return
      end
      subroutine r8_mod_test ( )

c*********************************************************************72
c
cc R8_MOD_TEST tests R8_MOD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_mod
      double precision r8_uniform_ab
      integer test
      integer test_num
      parameter ( test_num = 10 )
      integer seed
      double precision x
      double precision x_hi
      double precision x_lo
      double precision y
      double precision z1
      double precision z2

      x_hi = 10.0D+00
      x_lo = - 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_MOD_TEST'
      write ( *, '(a)' )
     &  '  R8_MOD returns the remainder after division.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         Y     MOD(X,Y)    R8_MOD(X,Y)'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, test_num

        x = r8_uniform_ab ( x_lo, x_hi, seed )
        y = r8_uniform_ab ( x_lo, x_hi, seed )

        z1 =    mod ( x, y )
        z2 = r8_mod ( x, y )

        write ( * , '(2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4)' ) x, y, z1, z2

      end do

      return
      end
      subroutine r8_modp_test ( )

c*********************************************************************72
c
cc R8_MODP_TEST tests R8_MODP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_modp
      double precision r8_uniform_ab
      integer test
      integer test_num
      integer seed
      double precision x
      double precision x_hi
      double precision x_lo
      double precision y
      double precision z1
      double precision z2

      test_num = 10
      x_hi = 10.0D+00
      x_lo = - 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_MODP_TEST'
      write ( *, '(a)' )
     &  '  R8_MODP returns the remainder after division.'
      write ( *, '(a)' )
     & '  Unlike the FORTRAN MOD, R8_MODP ( X, Y ) is positive.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X       Y      MOD(X,Y)  R8_MODP(X,Y)'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, test_num

        x = r8_uniform_ab ( x_lo, x_hi, seed )
        y = r8_uniform_ab ( x_lo, x_hi, seed )

        z1 =    mod  ( x, y )
        z2 = r8_modp ( x, y )

        write ( * , '(2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4)' ) x, y, z1, z2

      end do

      return
      end
      subroutine r8_mop_test ( )

c*********************************************************************72
c
cc R8_MOP_TEST tests R8_MOP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4
      integer i4_max
      integer i4_min
      integer i4_uniform_ab
      double precision r8
      double precision r8_mop
      integer seed
      integer test

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8_MOP_TEST'
      write ( *, '(a)' ) '  R8_MOP evaluates (-1.0)^I4 as an R8.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '    I4  R8_MOP(I4)'
      write ( *, '(a)' ) ''

      i4_min = -100
      i4_max = +100
      seed = 123456789

      do test = 1, 10
        i4 = i4_uniform_ab ( i4_min, i4_max, seed )
        r8 = r8_mop ( i4 )
        write ( *, '(2x,i4,2x,f4.1)' ) i4, r8
      end do

      return
      end
      subroutine r8_nint_test ( )

c*********************************************************************72
c
cc R8_NINT_TEST tests R8_NINT
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision c
      integer r8_nint
      double precision r8_uniform_ab
      integer seed
      integer test
      integer test_num
      double precision x

      seed = 123456789
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NINT_TEST'
      write ( *, '(a)' )
     &  '  R8_NINT produces the nearest integer to an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     X      R8_NINT(X)'
      write ( *, '(a)' ) ' '

      b = -10.0D+00
      c = +10.0D+00

      do test = 1, test_num
        x = r8_uniform_ab ( b, c, seed )
        write ( *, '(2x,f10.4,2x,i8)' ) x, r8_nint ( x )
      end do

      return;
      end
      subroutine r8_normal_01_test ( )

c*********************************************************************72
c
cc R8_NORMAL_01_TEST tests R8_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_normal_01
      integer seed
      integer test
      integer test_num
      double precision x

      seed = 123456789
      test_num = 20

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NORMAL_01_TEST'
      write ( *, '(a)' ) '  R8_NORMAL_01 generates normally distributed'
      write ( *, '(a)' ) '  random values.'
      write ( *, '(a,i12)' )
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        x = r8_normal_01 ( seed )
        write ( *, '(2x,g14.6)' ) x

      end do

      return
      end
      subroutine r8_normal_ab_test ( )

c*********************************************************************72
c
cc R8_NORMAL_AB_TEST tests R8_NORMAL_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_normal_ab
      integer seed
      integer test
      integer test_num
      double precision x
      double precision x_mean
      double precision x_std

      x_mean = 100.0D+00
      x_std = 10.0D+00
      seed = 123456789
      test_num = 20

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NORMAL_AB_TEST'
      write ( *, '(a)' ) '  R8_NORMAL_AB generates normally distributed'
      write ( *, '(a)' ) 
     &  '  values with given mean and standard deviation.'
      write ( *, '(a,i12)' )
     &  '  Using initial random number seed = ', seed
      write ( *, '(a,g14.6)' ) '  Mean = ', x_mean
      write ( *, '(a,g14.6)' ) '  Standard deviation = ', x_std
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        x = r8_normal_ab ( x_mean, x_std, seed )
        write ( *, '(2x,g14.6)' ) x

      end do

      return
      end
      subroutine r8_pi_test ( )

c*********************************************************************72
c
cc R8_PI_TEST tests R8_PI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision four
      double precision one
      double precision r8_pi
      double precision v1
      double precision v2

      four = real ( 4, kind = 8 )
      one = real ( 1, kind = 8 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_PI_TEST'
      write ( *, '(a)' ) '  R8_PI returns the value of PI.'
      write ( *, '(a)' ) ' '
      v1 = r8_pi ( )
      write ( *, '(a,g24.16)' ) '  R8_PI =     ', v1
      v2 = four * atan ( one )
      write ( *, '(a,g24.16)' ) '  4*atan(1) = ', v2

      return
      end
      subroutine r8_power_test ( )

c*********************************************************************72
c
cc R8_POWER_TEST tests R8_POWER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_power
      integer i
      integer p
      double precision r
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_POWER_TEST'
      write ( *, '(a)' ) '  R8_POWER computes R^P.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R          P       R^P'
      write ( *, '(a)' ) ' '

      do i = -5, 5

        r = 2.0D+00
        p = i
        value = r8_power ( r, p )
        write ( *, '(2x,g14.6,i5,g14.6,i5)' ) r, p, value

      end do

      return
      end
      subroutine r8_power_fast_test ( )

c*********************************************************************72
c
cc R8_POWER_FAST_TEST tests R8_POWER_FAST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer mults
      integer p
      double precision r
      double precision rp

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_POWER_FAST_TEST'
      write ( *, '(a)' ) '  R8_POWER_FAST computes R^P, economizing on'
      write ( *, '(a)' ) '  multiplications.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R          P       R^P        Mults'
      write ( *, '(a)' ) ' '

      do i = -10, 40

        r = 2.0D+00
        p = i
        call r8_power_fast ( r, p, rp, mults )
        write ( *, '(2x,g14.6,i5,g14.6,i5)' ) r, p, rp, mults

      end do

      return
      end
      subroutine r8_rise_test ( )

c*********************************************************************72
c
cc R8_RISE_TEST tests R8_RISE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1
      double precision f2
      integer n
      integer n_data
      double precision r8_rise
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_RISE_TEST'
      write ( *, '(a)' ) 
     &  '  R8_RISE computes the rising factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    X          N                Exact' //
     &  '                  Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_rise_values ( n_data, x, n, f1 )

        if ( n_data == 0 ) then
          go to 20
        end if

        f2 = r8_rise ( x, n );

        write ( *, '(2x,f8.4,2x,i4,2x,g24.16,2x,g24.16)' ) x, n, f1, f2

      go to 10

20    continue
     
      return
      end
      subroutine r8_round2_test ( )

c*********************************************************************72
c
cc R8_ROUND2_TEST tests R8_ROUND2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 September 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer nplace
      double precision r8_pi
      double precision x
      double precision xround

      x = r8_pi ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ROUND2_TEST'
      write ( *, '(a)' ) '  R8_ROUND2 rounds a number to a'
      write ( *, '(a)' ) '  specified number of base 2 digits.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test effect on PI:'
      write ( *, '(a,g24.16)' ) '  X = ', x
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    NPLACE  XROUND'
      write ( *, '(a)' ) ' '

      do i = 0, 20
        nplace = i
        call r8_round2 ( nplace, x, xround )
        write ( *, '(2x,i8,g24.16)' ) i, xround
      end do

      return
      end
      subroutine r8_roundb_test ( )

c*********************************************************************72
c
cc R8_ROUNDB_TEST tests R8_ROUNDB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer base
      integer i
      integer nplace
      double precision r8_pi
      double precision x
      double precision xround

      base = 3
      x = r8_pi ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ROUNDB_TEST'
      write ( *, '(a)' ) '  R8_ROUNDB rounds a number to a '
      write ( *, '(a)' ) '  specified number of base BASE digits.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Here, we will use BASE = ',base
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test effect on PI:'
      write ( *, '(a,g24.16)' ) '  X = ', x
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    NPLACE  XROUND'
      write ( *, '(a)' ) ' '

      do i = 0, 20
        nplace = i
        call r8_roundb ( base, nplace, x, xround )
        write ( *, '(2x,i8,g24.16)' ) i, xround
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Try with a negative base:'
      x = 121.0D+00
      base = -3
      nplace = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a,g24.16)' ) '  Input quantity is X = ', x
      write ( *, '(a,i8)' ) '  to be rounded in base ', base

      do nplace = 1, 5

        call r8_roundb ( base, nplace, x, xround )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a,g24.16)' ) '  Output value to ', nplace,
     &    ' places is ', xround

      end do

      return
      end
      subroutine r8_roundx_test ( )

c*********************************************************************72
c
cc R8_ROUNDX_TEST tests R8_ROUNDX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer nplace
      double precision r8_pi
      double precision r8_uniform_01
      integer seed
      double precision x
      double precision xround

      seed = 123456789
      x = r8_pi ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ROUNDX_TEST'
      write ( *, '(a)' ) '  R8_ROUNDX rounds a number to a '
      write ( *, '(a)' ) '  specified number of decimal digits.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test effect on PI:'
      write ( *, '(a,g24.16)' ) '  X = ', x
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    NPLACE  XROUND'
      write ( *, '(a)' ) ' '

      do i = 0, 10
        nplace = i
        call r8_roundx ( nplace, x, xround )
        write ( *, '(2x,i8,g24.16)' ) i, xround
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test effect on random values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    NPLACE  X     XROUND'
      write ( *, '(a)' ) ' '

      do i = 1, 5

        x = r8_uniform_01 ( seed )

        write ( *, '(a)' ) ' '

        do nplace = 0, 10, 2
          call r8_roundx ( nplace, x, xround )
          write ( *, '(2x,i8,2x,g24.16,2x,g24.16)' ) nplace, x, xround
        end do

      end do

      return
      end
      subroutine r8_secd_test ( )

c*********************************************************************72
c
cc R8_SECD_TEST tests R8_SECD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angle
      double precision r8_secd
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_SECD_TEST'
      write ( *, '(a)' ) '  R8_SECD computes the secant of an angle'
      write ( *, '(a)' ) '  given in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ANGLE    R8_SECD(ANGLE)'
      write ( *, '(a)' ) ' '
 
      do i = 0, 360, 10
        angle = dble ( i )
        if ( mod ( i + 90, 180 ) .eq. 0 ) then
          write ( *, '(2x,f8.2,2x,a)' )  angle, '  Undefined'
        else
          write ( *, '(2x,f8.2,2x,g14.6)' )  angle, r8_secd ( angle )
        end if
      end do
 
      return
      end
      subroutine r8_sign_test ( )

c*********************************************************************72
c
cc R8_SIGN_TEST tests R8_SIGN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 5 )

      double precision r8
      double precision r8_sign
      double precision r8_test(test_num)
      double precision s
      integer test

      save r8_test

      data r8_test /
     & -1.25D+00, -0.25D+00, 0.0D+00, +0.5D+00, +9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_SIGN_TEST'
      write ( *, '(a)' ) '  R8_SIGN returns the sign of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R8       R8_SIGN(R8)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        r8 = r8_test(test)
        s = r8_sign ( r8 )
        write ( *, '(2x,f8.4,2x,f8.0)' ) r8, s
      end do

      return
      end
      subroutine r8_sign3_test ( )

c*********************************************************************72
c
cc R8_SIGN3_TEST tests R8_SIGN3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 5 )

      double precision r8
      double precision r8_sign3
      double precision r8_test(test_num)
      double precision s
      integer test

      save r8_test

      data r8_test /
     & -1.25D+00, -0.25D+00, 0.0D+00, +0.5D+00, +9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_SIGN3_TEST'
      write ( *, '(a)' ) 
     &  '  R8_SIGN3 returns the three-way sign of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R8       R8_SIGN3(R8)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        r8 = r8_test(test)
        s = r8_sign3 ( r8 )
        write ( *, '(2x,f8.4,2x,f8.0)' ) r8, s
      end do

      return
      end
      subroutine r8_sind_test ( )

c*********************************************************************72
c
cc R8_SIND_TEST tests R8_SIND.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angle
      double precision r8_sind
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_SIND_TEST'
      write ( *, '(a)' ) '  R8_SIND computes the sine of an angle'
      write ( *, '(a)' ) '  given in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ANGLE    R8_SIND(ANGLE)'
      write ( *, '(a)' ) ' '
 
      do i = 0, 360, 10
        angle = dble ( i )
        write ( *, '(2x,f8.2,2x,g14.6)' )  angle, r8_sind ( angle )
      end do
 
      return
      end
      subroutine r8_swap_test ( )

c*********************************************************************72
c
cc R8_SWAP_TEST tests R8_SWAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_SWAP_TEST'
      write ( *, '(a)' ) '  R8_SWAP swaps two reals.'

      x = 1.0D+00
      y = 3.141592653589793D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Before swapping:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '    X = ', x
      write ( *, '(a,g14.6)' ) '    Y = ', y

      call r8_swap ( x, y )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After swapping:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '    X = ', x
      write ( *, '(a,g14.6)' ) '    Y = ', y

      return
      end
      subroutine r8_swap3_test ( )

c*********************************************************************72
c
cc R8_SWAP3_TEST tests R8_SWAP3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision x
      double precision y
      double precision z

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_SWAP3_TEST'
      write ( *, '(a)' ) '  R8_SWAP3 swaps three reals.'

      x = 1.0D+00
      y = 3.141592653589793D+00
      z = 1952.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X   Y   Z'
      write ( *, '(a)' ) ' '
      write ( *, '(a,3g14.6)' ) '  Before: ', x, y, z

      do i = 1, 3

        call r8_swap3 ( x, y, z )
        write ( *, '(a,i2,3g14.6)' ) '  Swap  ', i, x, y, z

      end do

      return
      end
      subroutine r8_tand_test ( )

c*********************************************************************72
c
cc R8_TAND_TEST tests R8_TAND.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angle
      double precision r8_tand
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_TAND_TEST'
      write ( *, '(a)' ) '  R8_TAND computes the tangent of an angle'
      write ( *, '(a)' ) '  given in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ANGLE    R8_TAND(ANGLE)'
      write ( *, '(a)' ) ' '
 
      do i = 0, 360, 10
        angle = dble ( i )
        if ( mod ( i + 90, 180 ) .eq. 0 ) then
          write ( *, '(2x,f8.2,2x,a)' )  angle, '  Undefined'
        else
          write ( *, '(2x,f8.2,2x,g14.6)' )  angle, r8_tand ( angle )
        end if
      end do
 
      return
      end
      subroutine r8_to_i4_test ( )

c*********************************************************************72
c
cc R8_TO_I4_TEST tests R8_TO_I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ix
      integer ixmax
      integer ixmin
      double precision x
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_TO_I4_TEST'
      write ( *, '(a)' )
     &  '  R8_TO_I4 finds an integer IX in [IXMIN,IXMAX]'
      write ( *, '(a)' ) '  corresponding to X in [XMIN,XMAX].'

      xmin = 2.5D+00
      x = 3.5D+00
      xmax = 5.5D+00

      ixmin = 10
      ixmax = 40

      call r8_to_i4 ( xmin, xmax, x, ixmin, ixmax, ix )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a,g14.6,a,g14.6)' )
     &  '  XMIN ',  xmin, '   X = ',  x, '  XMAX = ', xmax
      write ( *, '(a,i14,a,i14,a,i14)' )
     &  ' IXMIN ', ixmin, '  IX = ', ix, ' IXMAX = ', ixmax

      return
      end
      subroutine r8_to_r8_discrete_test ( )

c*********************************************************************72
c
cc R8_TO_R8_DISCRETE_TEST tests R8_TO_R8_DISCRETE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8_uniform_ab
      integer ndx
      double precision r
      double precision rd
      double precision rhi
      double precision rhi2
      double precision rlo
      double precision rlo2
      integer seed
      integer test
      integer test_num

      ndx = 19
      rlo = 1.0D+00
      rhi = 10.0D+00
      test_num = 15

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_TO_R8_DISCRETE_TEST'
      write ( *, '(a)' )
     &  '  R8_TO_R8_DISCRETE maps numbers to a discrete set'
      write ( *, '(a)' ) '  of equally spaced numbers in an interval.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of discrete values = ', ndx
      write ( *, '(a,2g14.6)' ) '  Real interval: ', rlo, rhi
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  R   RD'
      write ( *, '(a)' ) ' '

      seed = 123456789

      rlo2 = rlo - 2.0D+00
      rhi2 = rhi + 2.0D+00

      do test = 1, test_num
        r = r8_uniform_ab ( rlo2, rhi2, seed )
        call r8_to_r8_discrete ( r, rlo, rhi, ndx, rd )
        write ( *, '(2x,g14.6,g14.6)' ) r, rd
      end do

      return
      end
      subroutine r8_uniform_01_test ( )

c*********************************************************************72
c
cc R8_UNIFORM_01_TEST tests R8_UNIFORM_01
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r8_uniform_01
      integer seed
      integer seed_old
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
      write ( *, '(a)' ) 
     &  '  R8_UNIFORM_01 produces a sequence of random values.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  Using random seed ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SEED   R8_UNIFORM_01(SEED)'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        seed_old = seed
        x = r8_uniform_01 ( seed )
        write ( *, '(2x,i12,2x,g14.6)' ) seed, x
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Verify that the sequence can be restarted.'
      write ( *, '(a)' ) 
     &  '  Set the seed back to its original value, and see that'
      write ( *, '(a)' ) '  we generate the same sequence.'

      seed = 123456789
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SEED   R8_UNIFORM_01(SEED)'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        seed_old = seed
        x = r8_uniform_01 ( seed )
        write ( *, '(2x,i12,2x,g14.6)' ) seed, x
      end do

      return
      end
      subroutine r8_uniform_ab_test ( )

c*********************************************************************72
c
cc R8_UNIFORM_AB_TEST tests R8_UNIFORM_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      parameter ( b = 10.0D+00 )
      double precision c
      parameter ( c = 20.0D+00 )
      double precision r8_uniform_ab
      integer i
      double precision r
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_UNIFORM_AB_TEST'
      write ( *, '(a)' ) 
     &  '  R8_UNIFORM_AB returns random values in a given range:'
      write ( *, '(a)' ) '  [ A, B ]'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For this problem:'
      write ( *, '(a,g14.6)' ) '  B = ', b
      write ( *, '(a,g14.6)' ) '  C = ', c
      write ( *, '(a)' ) ' '

      seed = 123456789

      do i = 1, 10
        r = r8_uniform_ab ( b, c, seed )
        write ( *, '(2x,g14.6)' ) r
      end do

      return
      end
      subroutine r8_walsh_1d_test ( )

c*********************************************************************72
c
cc R8_WALSH_1D_TEST tests R8_WALSH_1D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r8_walsh_1d
      double precision w0
      double precision wm1
      double precision wm2
      double precision wm3
      double precision wp1
      double precision wp2
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_WALSH_1D_TEST'
      write ( *, '(a)' ) '  R8_WALSH_1D evaluates 1D Walsh functions:'
      write ( *, '(a)' ) ' '
      write ( *, * ) 'X  W(+2) W(+1) W(0) W(-1) W(-2) W(-3)'
      write ( *, '(a)' ) ' '

      do i = 0, 32

        x = real ( i, kind = 8 ) / 4.0D+00

        wp2 = r8_walsh_1d ( x,  2 )
        wp1 = r8_walsh_1d ( x,  1 )
        w0  = r8_walsh_1d ( x,  0 )
        wm1 = r8_walsh_1d ( x, -1 )
        wm2 = r8_walsh_1d ( x, -2 )
        wm3 = r8_walsh_1d ( x, -3 )

        write ( *, '(2x,f10.6,6f4.1)' ) x, wp2, wp1, w0, wm1, wm2, wm3

      end do

      return
      end
      subroutine r8_wrap_test ( )

c*****************************************************************************80
c
cc R8_WRAP_TEST tests R8_WRAP;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      parameter ( a = - 2.0D+00 )
      double precision b
      parameter ( b = 12.0D+00 )
      double precision r
      double precision r2
      double precision r8_uniform_ab
      double precision r8_wrap
      double precision rhi
      parameter ( rhi = 6.5D+00 )
      double precision rlo
      parameter ( rlo = 3.0D+00 )
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_WRAP_TEST'
      write ( *, '(a)' ) 
     &  '  R8_WRAP "wraps" an R8 to lie within an interval:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a,g14.6)' ) 
     &  '  Wrapping interval is ', rlo, ', ', rhi
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R      R8_WRAP ( R )'
      write ( *, '(a)' ) ' '
      seed = 123456789

      do test = 1, test_num

        r = r8_uniform_ab ( a, b, seed )
        r2 = r8_wrap ( r, rlo, rhi )
        write ( *, '(2x,g14.6,2x,g14.6)' ) r, r2

      end do

      return
      end
      subroutine r82col_print_part_test ( )

c*********************************************************************72
c
cc R82COL_PRINT_PART_TEST tests R82COL_PRINT_PART.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer max_print
      double precision v(n,2)

      save v
c
c  FORTRAN arrays are listed in row order.
c
      data v /
     & 11.0,  21.0, 31.0, 41.0,  51.0, 
     & 61.0,  71.0, 81.0, 91.0, 101.0, 
     & 12.0,  22.0, 32.0, 42.0,  52.0, 
     & 62.0,  72.0, 82.0, 92.0, 102.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R82COL_PRINT_PART_TEST'
      write ( *, '(a)' ) '  R82COL_PRINT_PART prints part of an R82COL.'

      max_print = 2
      call r82col_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 2' )

      max_print = 5
      call r82col_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 5' )

      max_print = 25
      call r82col_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 25' )

      return
      end
      subroutine r82poly2_type_test ( )

c*********************************************************************72
c
cc R82POLY2_TYPE_TEST tests R82POLY2_TYPE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 12 )

      double precision a
      double precision a_test(test_num)
      double precision b
      double precision b_test(test_num)
      double precision c
      double precision c_test(test_num)
      double precision d
      double precision d_test(test_num)
      double precision e
      double precision e_test(test_num)
      double precision f
      double precision f_test(test_num)
      integer test
      integer type

      save a_test
      save b_test
      save c_test
      save d_test
      save e_test
      save f_test

      data a_test /
     &  9.0D+00, 4.0D+00, 9.0D+00,  1.0D+00, 0.0D+00, 
     &  1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 0.0D+00, 
     &  0.0D+00, 0.0D+00 /
      data b_test /
     &  -4.0D+00,   1.0D+00,  16.0D+00,   1.0D+00, 0.0D+00,  
     &   2.0D+00, 1.0D+00,   1.0D+00,  1.0D+00,  0.0D+00, 
     &   0.0D+00, 0.0D+00 /
      data c_test /
     &   0.0D+00,  -4.0D+00,   0.0D+00,   0.0D+00, 1.0D+00,  
     &   0.0D+00, 0.0D+00,   0.0D+00,  0.0D+00,  0.0D+00, 
     &   0.0D+00, 0.0D+00 /
      data d_test /
     &  -36.0D+00,  3.0D+00,  36.0D+00,  -6.0D+00, 3.0D+00, 
     &  -2.0D+00, 0.0D+00,   0.0D+00,  0.0D+00,  2.0D+00, 
     &   0.0D+00, 0.0D+00 /
      data e_test /
     &  -24.0D+00, -4.0D+00, -32.0D+00, -10.0D+00, -1.0D+00, 
     &   16.0D+00, -6.0D+00, -6.0D+00, -2.0D+00, -1.0D+00, 
     &   0.0D+00, 0.0D+00 /
      data f_test /
     &  -36.0D+00,  1.0D+00, -92.0D+00, 115.0D+00, -3.0D+00, 
     &   33.0D+00, +8.0D+00, 10.0D+00,  +1.0D+00,  1.0D+00, 
     &    0.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R82POLY2_TYPE_TEST'
      write ( *, '(a)' ) '  R82POLY2_TYPE determines the type of a'
      write ( *, '(a)' ) '  second order equation in two variables.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        a = a_test(test)
        b = b_test(test)
        c = c_test(test)
        d = d_test(test)
        e = e_test(test)
        f = f_test(test)

        write ( *, '(a)' ) ' '

        call r82poly2_print ( a, b, c, d, e, f )

        call r82poly2_type ( a, b, c, d, e, f, type )

        write ( *, '(a,i8)' ) '  Type = ', type

        call r82poly2_type_print ( type )

      end do

      return
      end
      subroutine r82row_order_type_test ( )

c*********************************************************************72
c
cc R82ROW_ORDER_TYPE_TEST tests R82ROW_ORDER_TYPE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 November 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integern
      parameter ( n = 4 )
      integer test_num
      parameter ( test_num = 10 )

      integer i
      integer j
      integer order
      integer seed
      integer test
      character * ( 40 ) title
      double precision x(2,n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R82ROW_ORDER_TYPE_TEST'
      write ( *, '(a)' ) '  R82ROW_ORDER_TYPE classifies a R8VEC as'
      write ( *, '(a)' ) '  -1: no order'
      write ( *, '(a)' ) '   0: all equal;'
      write ( *, '(a)' ) '   1: ascending;'
      write ( *, '(a)' ) '   2: strictly ascending;'
      write ( *, '(a)' ) '   3: descending;'
      write ( *, '(a)' ) '   4: strictly descending.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        call r8mat_uniform_01 ( 2, n, seed, x )

        do j = 1, n
          do i = 1, 2
            x(i,j) = dble ( nint ( 3.0D+00 * x(i,j) ) )
          end do
        end do

        call r82row_order_type ( n, x, order )

        write ( title, '(a,i8)' ) '  Order type = ', order

        call r82row_print ( n, x, title )

      end do

      return
      end
      subroutine r82row_part_quick_a_test ( )

c*********************************************************************72
c
cc R82ROW_PART_QUICK_A_TEST tests R82ROW_PART_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 12 )

      double precision a(2,n)
      double precision b
      double precision c
      integer l
      integer r
      integer seed

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R82ROW_PART_QUICK_A_TEST'
      write ( *, '(a)' ) '  R82ROW_PART_QUICK_A reorders an R82ROW'
      write ( *, '(a)' ) '  as part of a quick sort.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      call r8mat_uniform_ab ( 2, n, b, c, seed, a )

      call r82row_print ( n, a, '  Before rearrangement:' )

      call r82row_part_quick_a ( n, a, l, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Rearranged array'
      write ( *, '(a,i8)' ) '  Left index =  ', l
      write ( *, '(a,i8)' ) '  Key index =   ', l+1
      write ( *, '(a,i8)' ) '  Right index = ', r
      write ( *, '(a)' ) ' '

      call r82row_print ( l,     a(1:2,1:l),   '  Left half:' )
      call r82row_print ( 1,     a(1:2,l+1),   '  Key:' )
      call r82row_print ( n-l-1, a(1:2,l+2:n), '  Right half:' )

      return
      end
      subroutine r82row_print_part_test ( )

c*********************************************************************72
c
cc R82ROW_PRINT_PART_TEST tests R82ROW_PRINT_PART.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer max_print
      double precision v(2,n)

      save v
c
c  FORTRAN arrays are listed in row order.
c
      data v /
     & 11.0,  21.0, 
     & 12.0,  22.0, 
     & 13.0,  23.0, 
     & 14.0,  24.0, 
     & 15.0,  25.0, 
     & 16.0,  26.0, 
     & 17.0,  27.0, 
     & 18.0,  28.0, 
     & 19.0,  29.0, 
     & 20.0,  30.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R82ROW_PRINT_PART_TEST'
      write ( *, '(a)' ) '  R82ROW_PRINT_PART prints part of an R82ROW'
      write ( *, '(a)' ) '  as a list of columns (that is, transposed.)'

      max_print = 2
      call r82row_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 2' )

      max_print = 5
      call r82row_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 5' )

      max_print = 25
      call r82row_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 25' )

      return
      end
      subroutine r82row_sort_heap_index_a_test ( )

c*********************************************************************72
c
cc R82ROW_SORT_HEAP_INDEX_A_TEST tests R82ROW_SORT_HEAP_INDEX_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 12 )

      double precision a(2,n)
      double precision b
      double precision c
      integer i
      integer indx(n)
      integer seed

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R82ROW_SORT_HEAP_INDEX_A_TEST'
      write ( *, '(a)' ) 
     &  '  R82ROW_SORT_HEAP_INDEX_A index sorts an R82ROW'
      write ( *, '(a)' ) '  using heapsort.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      call r8mat_uniform_ab ( 2, n, b, c, seed, a )
c
c  Give a few elements the same first component.
c
      a(1,3) = a(1,5)
      a(1,4) = a(1,12)
c
c  Give a few elements the same second component.
c
      a(2,6) = a(2,1)
      a(2,2) = a(2,9)
c
c  Make two entries equal.
c
      a(1:2,7) = a(1:2,11)

      call r82row_print ( n, a, '  Before rearrangement:' )

      call r82row_sort_heap_index_a ( n, a, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I Index A(Index)'
      write ( *, '(a)' ) ' '

      do i = 1, n
        write ( *, '(2x,i8,i8,g14.6,g14.6)' ) i, indx(i), a(1:2,indx(i))
      end do

      call r82row_permute ( n, indx, a )

      call r82row_print ( n, a, 
     &  '  After rearrangement by R82ROW_PERMUTE:' )

      return
      end
      subroutine r82row_sort_quick_a_test ( )

c*********************************************************************72
c
cc R82ROW_SORT_QUICK_A_TEST tests R82ROW_SORT_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 12 )

      double precision a(2,n)
      double precision b
      double precision c
      integer seed

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R82ROW_SORT_QUICK_A_TEST'
      write ( *, '(a)' ) '  R82ROW_SORT_QUICK_A sorts an R82ROW'
      write ( *, '(a)' ) '  using quick sort.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      call r8mat_uniform_ab ( 2, n, b, c, seed, a )
c
c  Give a few elements the same first component.
c
      a(1,3) = a(1,5)
      a(1,4) = a(1,12)
c
c  Give a few elements the same second component.
c
      a(2,6) = a(2,1)
      a(2,2) = a(2,9)
c
c  Make two entries equal.
c
      a(1:2,7) = a(1:2,11)

      call r82row_print ( n, a, '  Before rearrangement:' )

      call r82row_sort_quick_a ( n, a )

      call r82row_print ( n, a, '  Sorted array:' )

      return
      end
      subroutine r83col_print_part_test ( )

c*********************************************************************72
c
cc R83COL_PRINT_PART_TEST tests R83COL_PRINT_PART.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer max_print
      double precision v(n,3)

      save v
c
c  FORTRAN arrays are listed in row order.
c
      data v /
     & 11.0,  21.0, 31.0, 41.0,  51.0, 
     & 61.0,  71.0, 81.0, 91.0, 101.0, 
     & 12.0,  22.0, 32.0, 42.0,  52.0, 
     & 62.0,  72.0, 82.0, 92.0, 102.0,
     & 13.0,  23.0, 33.0, 43.0,  53.0, 
     & 63.0,  73.0, 83.0, 93.0, 103.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R83COL_PRINT_PART_TEST'
      write ( *, '(a)' ) '  R83COL_PRINT_PART prints part of an R83COL.'

      max_print = 2
      call r83col_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 2' )

      max_print = 5
      call r83col_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 5' )

      max_print = 25
      call r83col_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 25' )

      return
      end
      subroutine r83row_print_part_test ( )

c*********************************************************************72
c
cc R83ROW_PRINT_PART_TEST tests R83ROW_PRINT_PART.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer max_print
      double precision v(3,n)

      save v
c
c  FORTRAN arrays are listed in row order.
c
      data v /
     & 11.0,  21.0,  31.0, 
     & 12.0,  22.0,  32.0,
     & 13.0,  23.0,  33.0,
     & 14.0,  24.0,  34.0,
     & 15.0,  25.0,  35.0,
     & 16.0,  26.0,  36.0,
     & 17.0,  27.0,  37.0,
     & 18.0,  28.0,  38.0,
     & 19.0,  29.0,  39.0,
     & 20.0,  30.0,  40.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R83ROW_PRINT_PART_TEST'
      write ( *, '(a)' ) '  R83ROW_PRINT_PART prints part of an R83ROW'
      write ( *, '(a)' ) '  as a list of columns (that is, transposed.)'

      max_print = 2
      call r83row_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 2' )

      max_print = 5
      call r83row_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 5' )

      max_print = 25
      call r83row_print_part ( n, v, max_print, 
     &  '  Output with MAX_PRINT = 25' )

      return
      end
      subroutine r8block_expand_linear_test ( )

c*********************************************************************72
c
cc R8BLOCK_EXPAND_LINEAR_TEST tests R8BLOCK_EXPAND_LINEAR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l
      parameter ( l = 4 )
      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 2 )
      integer lfat
      parameter ( lfat = 1 )
      integer mfat
      parameter ( mfat = 2 )
      integer nfat
      parameter ( nfat = 1 )
      integer l2
      parameter ( l2 = ( l - 1 ) * ( lfat + 1 ) + 1 )
      integer m2
      parameter ( m2 = ( m - 1 ) * ( mfat + 1 ) + 1 )
      integer n2
      parameter ( n2 = ( n - 1 ) * ( nfat + 1 ) + 1 )

      double precision x(l,m,n)
      double precision xfat(l2,m2,n2)

      save x

      data x /
     &  1.0D+00,  2.0D+00,  3.0D+00,   4.0D+00,  1.0D+00, 
     &  4.0D+00,  9.0D+00, 16.0D+00,   1.0D+00,  8.0D+00, 
     & 27.0D+00, 64.0D+00,  2.0D+00,   4.0D+00,  6.0D+00, 
     &  8.0D+00,  2.0D+00,  8.0D+00,  18.0D+00, 32.0D+00, 
     &  2.0D+00, 16.0D+00, 54.0D+00, 128.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8BLOCK_EXPAND_LINEAR_TEST'
      write ( *, '(a)' ) 
     &  '  R8BLOCK_EXPAND_LINEAR linearly interpolates new data'
      write ( *, '(a)' ) '  between old values in a 3D block.'

      call r8block_print ( l, m, n, x, '  Original block:' )
 
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  LFAT = ', lfat
      write ( *, '(a,i8)' ) '  MFAT = ', mfat
      write ( *, '(a,i8)' ) '  NFAT = ', nfat

      call r8block_expand_linear ( l, m, n, x, lfat, mfat, nfat, xfat )

      call r8block_print ( l2, m2, n2, xfat, '  Fattened block:' )

      return
      end
      subroutine r8block_print_test ( )

c*****************************************************************************80
c
cc R8BLOCK_PRINT_TEST tests R8BLOCK_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l
      parameter ( l = 4 )
      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 2 )

      double precision x(l,m,n)

      save x

      data x /
     &  1.0D+00,  2.0D+00,  3.0D+00,   4.0D+00,  1.0D+00, 
     &  4.0D+00,  9.0D+00, 16.0D+00,   1.0D+00,  8.0D+00, 
     & 27.0D+00, 64.0D+00,  2.0D+00,   4.0D+00,  6.0D+00, 
     &  8.0D+00,  2.0D+00,  8.0D+00,  18.0D+00, 32.0D+00, 
     &  2.0D+00, 16.0D+00, 54.0D+00, 128.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8BLOCK_PRINT_TEST'
      write ( *, '(a)' ) '  R8BLOCK_PRINT prints an R8BLOCK.'

      call r8block_print ( l, m, n, x, '  The 3D array:' )

      return
      end
      subroutine r8col_find_test ( )

c*********************************************************************72
c
cc R8COL_FIND_TEST tests R8COL_FIND.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer m
      parameter ( m = 3 )

      double precision dtab(m,n)
      integer i
      integer icol
      integer j
      integer k
      double precision r8vec(m)

      k = 1
      do i = 1, m
        do j = 1, n

          dtab(i,j) = dble ( k )

          if ( j .eq. 3 ) then
            r8vec(i) = dble ( k )
          end if

          k = k + 1

        end do
      end do

      call r8col_find ( m, n, dtab, r8vec, icol )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_FIND_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) 
     &  '  R8COL_FIND seeks a column matching given data.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  R8COL_FIND returns ICOL = ', icol

      return
      end
      subroutine r8col_insert_test ( )

c*********************************************************************72
c
cc R8COL_INSERT_TEST tests R8COL_INSERT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n_max
      parameter ( n_max = 10 )

      double precision a(m,n_max)
      integer col
      double precision r8vec1(m)
      double precision r8vec2(m)
      integer n

      save a
      save r8vec1
      save r8vec2

      data a /
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00 /
      data r8vec1 / 3.0D+00, 7.0D+00, 11.0D+00 /
      data r8vec2 / 3.0D+00, 4.0D+00, 18.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_INSERT_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) 
     &  '  R8COL_SORT_HEAP_A does an ascending heap sort'
      write ( *, '(a)' ) '  R8COL_INSERT inserts new columns.'

      n = 4

      call r8mat_print ( m, n, a, '  The unsorted matrix:' )

      call r8col_sort_heap_a ( m, n, a )

      call r8mat_print ( m, n, a, '  The sorted matrix:' )

      call r8vec_print ( m, r8vec1, '  New column:' )

      call r8col_insert ( n_max, m, n, a, r8vec1, col )

      if ( col .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  The data was already in column ', abs ( col )
      else
        call r8mat_print ( m, n, a, '  The updated matrix:' )
      end if

      call r8vec_print ( m, r8vec2, '  New column:' )

      call r8col_insert ( n_max, m, n, a, r8vec2, col )

      if ( col .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  The data was already in column ', abs ( col )
      else
        call r8mat_print ( m, n, a, '  The updated matrix:' )
      end if

      return
      end
      subroutine r8col_part_quick_a_test ( )

c*********************************************************************72
c
cc R8COL_PART_QUICK_A_TEST tests R8COL_PART_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n
      parameter ( n = 8 )

      double precision a(m,n)
      integer l
      integer r

      save a

      data a /
     &   2.0D+00, 4.0D+00, 
     &   8.0D+00, 8.0D+00, 
     &   6.0D+00, 2.0D+00, 
     &   0.0D+00, 2.0D+00, 
     &  10.0D+00, 6.0D+00, 
     &  10.0D+00, 0.0D+00, 
     &   0.0D+00, 6.0D+00, 
     &   5.0D+00, 8.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_PART_QUICK_A_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) '  R8COL_PART_QUICK_A partitions the matrix.'

      call r8mat_print ( m, n, a, '  The matrix:' )

      l = 2
      r = 4
      call r8col_part_quick_a ( m, n, a, l, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  L = ', l
      write ( *, '(a,i4)' ) '  R = ', r

      call r8mat_print ( m, n, a, '  The partitioned matrix:' )

      return
      end
      subroutine r8col_sort_heap_a_test ( )

c*********************************************************************72
c
cc R8COL_SORT_HEAP_A_TEST tests R8COL_SORT_HEAP_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n_max
      parameter ( n_max = 10 )

      double precision a(m,n_max)
      double precision r8vec1(m)
      double precision r8vec2(m)
      integer n

      save a
      save r8vec1
      save r8vec2

      data a /
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00 /
      data r8vec1 / 3.0D+00, 7.0D+00, 11.0D+00 /
      data r8vec2 / 3.0D+00, 4.0D+00, 18.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORT_HEAP_A_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) 
     &  '  R8COL_SORT_HEAP_A does an ascending heap sort'
      write ( *, '(a)' ) '  R8COL_INSERT inserts new columns.'

      n = 4

      call r8mat_print ( m, n, a, '  The unsorted matrix:' )

      call r8col_sort_heap_a ( m, n, a )

      call r8mat_print ( m, n, a, '  The sorted matrix:' )

      return
      end
      subroutine r8col_sort_heap_index_a_test ( )

c*********************************************************************72
c
cc R8COL_SORT_HEAP_INDEX_A_TEST tests R8COL_SORT_HEAP_INDEX_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 15 )

      double precision a(m,n)
      integer indx(n)
      integer j
      integer j2

      save a

      data a /
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORT_HEAP_INDEX_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_SORT_HEAP_INDEX_A computes an index vector which'
      write ( *, '(a)' ) '  ascending sorts an R8COL.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The unsorted R8COL (transposed):' )

      call r8col_sort_heap_index_a ( m, n, a, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The implicitly sorted R8COL (transposed)'
      write ( *, '(a)' ) ' '

      do j = 1, n
        j2 = indx(j)
        write ( *, '(2x,i4,a,2x,f10.1,2x,f10.1,2x,f10.1)' ) 
     &    j2, ':', a(1:m,j2)
      end do

      return
      end
      subroutine r8col_sort_quick_a_test ( )

c*********************************************************************72
c
cc R8COL_SORT_QUICK_A_TEST tests R8COL_SORT_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 10 )

      double precision a(m,n)
      double precision b
      parameter ( b = 0.0D+00 )
      double precision c
      parameter ( c = 10.0D+00 )
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORT_QUICK_A_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) '  R8COL_SORT_QUICK_A does a quicksort.'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  The unsorted matrix:' )

      call r8col_sort_quick_a ( m, n, a )

      call r8mat_print ( m, n, a, '  The sorted matrix:' )

      return
      end
      subroutine r8col_sorted_tol_undex_test ( )

c*********************************************************************72
c
cc R8COL_SORTED_TOL_UNDEX_TEST tests R8COL_SORTED_TOL_UNDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 22 )

      double precision a(m,n)
      double precision au(m,n)
      integer i
      integer j
      integer n_unique
      double precision tol
      integer undx(n)
      integer xdnu(n)

      save a

      data a /
     &  1.9D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  2.0D+00,  0.1D+00, 10.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  1.9D+00,  8.0D+00, 10.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.1D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORTED_TOL_UNDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_SORTED_TOL_UNDEX produces index vectors which '
      write ( *, '(a)' ) 
     &  '  create a sorted list of the tolerably unique columns'
      write ( *, '(a)' ) 
     &  '  of a sorted R8COL,'
      write ( *, '(a)' ) 
     &  '  and a map from the original R8COL to the (implicit)'
      write ( *, '(a)' ) 
     &  '  R8COL of sorted tolerably unique elements.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The unsorted R8COL (transposed):' )

      call r8col_sort_heap_a ( m, n, a )

      call r8mat_transpose_print ( m, n, a, 
     &  '  The sorted R8COL (transposed):' )

      tol = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Tolerance for equality = ', tol

      call r8col_sorted_tol_unique_count ( m, n, a, tol, n_unique )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of tolerably unique columns is ', n_unique

      call r8col_sorted_tol_undex ( m, n, a, n_unique, tol, undx, xdnu )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  XDNU points to the representative for each item.'
      write ( *, '(a)' ) '  UNDX selects the representatives.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  XDNU  UNDX'
      write ( *, '(a)' ) ' '
      do i = 1, n_unique
        write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, xdnu(i), undx(i)
      end do
      do i = n_unique + 1, n
        write ( *, '(2x,i4,2x,i4)'       ) i, xdnu(i)
      end do

      do j = 1, n_unique
        au(1:m,j) = a(1:m,undx(j))
      end do

      call r8mat_transpose_print ( m, n_unique, au, 
     &  '  The tolerably unique R8COL (transposed):' )

      return
      end
      subroutine r8col_sorted_tol_unique_test ( )

c*********************************************************************72
c
cc R8COL_SORTED_TOL_UNIQUE_TEST tests R8COL_SORTED_TOL_UNIQUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 22 )

      double precisiona(m,n)
      double precision tol
      integer unique_num

      save a

      data a / 
     &  1.9D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  2.0D+00,  0.1D+00, 10.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  1.9D+00,  8.0D+00, 10.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.1D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORTED_TOL_UNIQUE_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_SORTED_TOL_UNIQUE finds tolerably unique columns'
      write ( *, '(a)' ) '  in a sorted R8COL.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The unsorted R8COL (transposed):' )

      call r8col_sort_heap_a ( m, n, a )

      call r8mat_transpose_print ( m, n, a, 
     &  '  The sorted R8COL (transposed):' )

      tol = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Using tolerance = ', tol

      call r8col_sorted_tol_unique ( m, n, a, tol, unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of tolerably unique columns is ', unique_num

      call r8mat_transpose_print ( m, unique_num, a, 
     &  '  The sorted tolerably unique R8COL (transposed):' )

      return
      end
      subroutine r8col_sorted_tol_unique_count_test ( )

c*********************************************************************72
c
cc R8COL_SORTED_TOL_UNIQUE_COUNT_TEST tests R8COL_SORTED_TOL_UNIQUE_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 22 )

      double precision a(m,n)
      double precision tol
      integer unique_num

      save a

      data a /
     &  1.9D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00,
     &  3.0D+00,  7.0D+00, 11.0D+00,
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  2.0D+00,  0.1D+00, 10.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  1.9D+00,  8.0D+00, 10.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.1D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00,
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORTED_TOL_UNIQUE_COUNT_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_SORTED_TOL_UNIQUE_COUNT counts tolerably '
      write ( *, '(a)' ) '  unique columns in a sorted R8COL.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The unsorted R8COL (transposed):' )

      call r8col_sort_heap_a ( m, n, a )

      call r8mat_transpose_print ( m, n, a, 
     &  '  The sorted R8COL (transposed):' )

      tol = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Using tolerance = ', tol

      call r8col_sorted_tol_unique_count ( m, n, a, tol, unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of tolerably unique columns is ', unique_num

      return
      end
      subroutine r8col_max_test ( )

c*********************************************************************72
c
cc R8COL_MAX_TEST tests R8COL_MAX;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision amax(n)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_MAX_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) '  R8COL_MAX computes maximums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array:' )

      call r8col_max ( m, n, a, amax )

      call r8col_max ( n, amax, '  Column maximums:' )

      return
      end
      subroutine r8col_mean_test ( )

c*********************************************************************72
c
cc R8COL_MEAN_TEST tests R8COL_MEAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision mean(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_MEAN_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) '  R8COL_MEAN computes means;'


      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array:' )

      call r8col_mean ( m, n, a, mean )

      call r8vec_print ( n, mean, '  Column means:' )

      return
      end
      subroutine r8col_min_test ( )

c*********************************************************************72
c
cc R8COL_MIN_TEST tests R8COL_MIN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision amin(n)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_MIN_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) '  R8COL_MIN computes minimums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array:' )

      call r8col_min ( m, n, a, amin )

      call r8vec_print ( n, amin, '  Column minimums: ' )

      return
      end
      subroutine r8col_sum_test ( )

c*********************************************************************72
c
cc R8COL_SUM_TEST tests R8COL_SUM;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision colsum(n)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SUM_TEST'
      write ( *, '(a)' ) '  For an R8COL;'
      write ( *, '(a)' ) '  R8COL_SUM computes sums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array:' )

      call r8col_sum ( m, n, a, colsum )

      call r8vec_print ( n, colsum, '  Column sums:' )

      return
      end
      subroutine r8col_permute_test ( )

c*********************************************************************72
c
cc R8COL_PERMUTE_TEST tests R8COL_PERMUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 5 )

      double precision a(m,n)

      integer perm(n)

      save a
      save perm

      data a /
     &  11.0D+00, 21.0D+00, 31.0D+00, 
     &  12.0D+00, 22.0D+00, 32.0D+00, 
     &  13.0D+00, 23.0D+00, 33.0D+00, 
     &  14.0D+00, 24.0D+00, 34.0D+00, 
     &  15.0D+00, 25.0D+00, 35.0D+00 /
      data perm / 2, 4, 5, 1, 3 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_PERMUTE_TEST'
      write ( *, '(a)' ) '  R8COL_PERMUTE permutes an R8COL in place.'

      call r8mat_print ( m, n, a, '  A (unpermuted)' )

      call i4vec_print ( n, perm, 
     &  '  The (column) permutation vector:' )

      call r8col_permute ( m, n, perm, a )

      call r8mat_print ( m, n, a, '  A (permuted)' )

      return
      end
      subroutine r8col_sortr_a_test ( )

c*********************************************************************72
c
cc R8COL_SORTR_A_TEST tests R8COL_SORTR_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 10 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      parameter ( b = 0.0D+00 )
      double precision c
      parameter ( c = 10.0D+00 )
      integer key
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SORTR_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_SORTR_A is given an array, and reorders'
      write ( *, '(a)' ) '  it so that a particular column is sorted.'
      write ( *, '(a)' ) ' '

      key = 2
      write ( *, '(a,i8)' ) '  Here, the special column is ', key

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Unsorted array:' )

      call r8col_sortr_a ( m, n, a, key )

      call r8mat_print ( m, n, a, '  Sorted array:' )

      return
      end
      subroutine r8col_swap_test ( )

c*********************************************************************72
c
cc R8COL_SWAP_TEST tests R8COL_SWAP;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m 
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer icol1
      integer icol2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_SWAP_TEST'
      write ( *, '(a)' ) '  R8COL_SWAP swaps two columns of an R8COL;'

      call r8mat_indicator ( m, n, a )

      call r8mat_print ( m, n, a, '  The array:' )

      icol1 = 1
      icol2 = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a,i8)' ) 
     &  '  Swap columns ', icol1, ' and ', icol2

      call r8col_swap ( m, n, a, icol1, icol2 )

      call r8mat_print ( m, n, a, '  The updated matrix:' )

      return
      end
      subroutine r8col_to_r8vec_test ( )

c*********************************************************************72
c
cc R8COL_TO_R8VEC_TEST tests R8COL_TO_R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      double precision x(m*n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_TO_R8VEC_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_TO_R8VEC converts an array of columns to a vector.'
      write ( *, '(a)' ) ' '

      do i = 1, m
        do j = 1, n
          a(i,j) = dble ( 10 * i + j )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array of columns:' )

      call r8col_to_r8vec ( m, n, a, x )

      call r8vec_print ( m*n, x, '  The resulting vector of columns:' )

      return
      end
      subroutine r8col_tol_undex_test ( )

c*********************************************************************72
c
cc R8COL_TOL_UNDEX_TEST tests R8COL_TOL_UNDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n 
      parameter ( n = 22 )

      double precision a(m,n)
      double precision au(m,n)
      integer i
      integer j
      integer n_unique
      double precision tol
      integer undx(n)
      integer xdnu(n)

      save a

      data a /
     &  1.9D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  2.0D+00,  0.1D+00, 10.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  1.9D+00,  8.0D+00, 10.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.1D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  2.0D+00,  0.0D+00, 10.1D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_TOL_UNDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_TOL_UNDEX produces index vectors which '
      write ( *, '(a)' ) 
     &  '  create a sorted list of the tolerably unique columns'
      write ( *, '(a)' ) '  of an R8COL,'
      write ( *, '(a)' ) 
     &  '  and a map from the original R8COL to the (implicit)'
      write ( *, '(a)' ) '  R8COL of sorted tolerably unique elements.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The unsorted R8COL (transposed):' )

      tol = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Tolerance for equality = ', tol

      call r8col_tol_unique_count ( m, n, a, tol, n_unique )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of tolerably unique columns is ', n_unique

      call r8col_tol_undex ( m, n, a, n_unique, tol, undx, xdnu )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  XDNU points to the representative for each item.'
      write ( *, '(a)' ) '  UNDX selects the representatives.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  XDNU  UNDX'
      write ( *, '(a)' ) ' '
      do i = 1, n_unique
        write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, xdnu(i), undx(i)
      end do
      do i = n_unique + 1, n
        write ( *, '(2x,i4,2x,i4)'       ) i, xdnu(i)
      end do

      do j = 1, n_unique
        au(1:m,j) = a(1:m,undx(j))
      end do

      call r8mat_transpose_print ( m, n_unique, au, 
     &  '  The tolerably unique R8COL (transposed):' )

      return
      end
      subroutine r8col_undex_test ( )

c*********************************************************************72
c
cc R8COL_UNDEX_TEST tests R8COL_UNDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 February 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 15 )

      double precision a(m,n)
      double precision au(m,n)
      integer i
      integer j
      integer n_unique
      integer undx(n)
      integer xdnu(n)

      save a

      data a /
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_UNDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_UNDEX produces index vectors which create a sorted'
      write ( *, '(a)' ) 
     &  '  list of the unique columns of an (unsorted) R8COL,'
      write ( *, '(a)' ) 
     &  '  and a map from the original R8COL to the (implicit)'
      write ( *, '(a)' ) '  R8COL of sorted unique elements.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The R8COL (transposed):' )

      call r8col_unique_count ( m, n, a, n_unique )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique columns is ', n_unique

      call r8col_undex ( m, n, a, n_unique, undx, xdnu )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  XDNU points to the representative for each item.'
      write ( *, '(a)' ) '  UNDX selects the representatives.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  XDNU  UNDX'
      write ( *, '(a)' ) ' '
      do i = 1, n_unique
        write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, xdnu(i), undx(i)
      end do
      do i = n_unique + 1, n
        write ( *, '(2x,i4,2x,i4)'       ) i, xdnu(i)
      end do

      do j = 1, n_unique
        do i = 1, m
          au(i,j) = a(i,undx(j))
        end do
      end do

      call r8mat_transpose_print ( m, n_unique, au, 
     &  '  The Unique R8COL (transposed):' )

      return
      end
      subroutine r8col_unique_count_test ( )

c*********************************************************************72
c
cc R8COL_UNIQUE_COUNT_TEST tests R8COL_UNIQUE_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 15 )

      double precision a(m,n)
      integer unique_num

      save a

      data a /
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  4.0D+00,  8.0D+00, 12.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  6.0D+00,  0.0D+00, 
     &  3.0D+00,  4.0D+00, 18.0D+00, 
     &  0.0D+00,  0.0D+00,  0.0D+00, 
     &  0.0D+00,  6.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  3.0D+00,  7.0D+00, 11.0D+00, 
     &  2.0D+00,  0.0D+00, 10.0D+00, 
     &  2.0D+00,  6.0D+00, 10.0D+00, 
     &  1.0D+00,  5.0D+00,  9.0D+00, 
     &  1.0D+00,  5.0D+00,  9.1D+00, 
     &  1.0D+00,  5.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_UNIQUE_COUNT_TEST'
      write ( *, '(a)' ) '  R8COL_UNIQUE_COUNT counts unique columns'
      write ( *, '(a)' ) '  in an unsorted R8COL.'

      call r8mat_transpose_print ( m, n, a, 
     &  '  The R8COL (transposed):' )

      call r8col_unique_count ( m, n, a, unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique columns is ', unique_num

      return
      end
      subroutine r8col_variance_test ( )

c*********************************************************************72
c
cc R8COL_VARIANCE_TEST tests R8COL_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision variance(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8COL_VARIANCE_TEST'
      write ( *, '(a)' ) 
     &  '  R8COL_VARIANCE computes variances of an R8COL;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array:' )

      call r8col_variance ( m, n, a, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Column       variance:'
      write ( *, '(a)' ) ' '

      do j = 1, n
        write ( *, '(2x,i8,2x,f10.4)' ) j, variance(j)
      end do

      return
      end
      subroutine r8r8vec_index_insert_unique_test ( )

c*********************************************************************72
c
cc R8R8VEC_INDEX_INSERT_UNIQUE_TEST tests R8R8VEC_INDEX_INSERT_UNIQUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )

      integer i
      integer ierror
      integer indx(n_max)
      integer ival
      integer n
      double precision r8_uniform_ab
      integer seed
      real x(n_max)
      double precision x_max
      double precision x_min
      double precision xval
      double precision y(n_max)
      double precision y_max
      double precision y_min
      double precision yval

      n = 0
      x_min = 1.0D+00
      x_max = 4.0D+00
      y_min = 1.0D+00
      y_max = 3.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8VEC_INDEX_INSERT_UNIQUE_TEST'
      write ( *, '(a)' ) 
     &  '  R8R8VEC_INDEX_INSERT_UNIQUE inserts unique values into'
      write ( *, '(a)' ) '  an index sorted array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) '  Generate ', n_max, ' random values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Index    XVAL    YVAL'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do i = 1, n_max

        xval = r8_uniform_ab ( x_min, x_max, seed )
        xval = dble ( nint ( xval ) )
        yval = r8_uniform_ab ( y_min, y_max, seed )
        yval = dble ( nint ( yval ) )

        call r8r8vec_index_insert_unique ( n_max, n, x, y, indx, 
     &    xval, yval, ival, ierror )

        write ( *, '(2x,i3,6x,f6.2,9x,f6.2)' ) ival, xval, yval

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vector of unique X Y values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  X(I)   Y(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,f6.2,9x,f6.2)' ) i, x(i), y(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X, Y sorted by index'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(INDX(I))  Y(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(indx(i)), y(indx(i))
      end do

      return
      end
      subroutine r8r8r8vec_index_insert_unique_test ( )

c*********************************************************************72
c
cc R8R8R8VEC_INDEX_INSERT_UNIQUE_TEST tests R8R8R8VEC_INDEX_INSERT_UNIQUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )

      integer i
      integer ierror
      integer indx(n_max)
      integer ival
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval
      double precision y(n_max)
      double precision yval
      double precision z(n_max)
      double precision zval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8R8VEC_INDEX_INSERT_UNIQUE_TEST'
      write ( *, '(a)' ) 
     &  '  R8R8R8VEC_INDEX_INSERT_UNIQUE inserts unique values'
      write ( *, '(a)' ) '  into an index sorted array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of random values to generate = ', n_max
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    XVAL    YVAL  ZVAL  Index'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do i = 1, n_max

        xval = r8_uniform_ab ( 1.0D+00, 4.0D+00, seed )
        xval = dble ( nint ( xval ) )
        yval = r8_uniform_ab ( 1.0D+00, 3.0D+00, seed )
        yval = dble ( nint ( yval ) )
        zval = r8_uniform_ab ( 1.0D+00, 4.0D+00, seed )
        zval = dble ( nint ( zval ) )

        call r8r8r8vec_index_insert_unique ( n_max, n, x, y, z, 
     &    indx, xval, yval, zval, ival, ierror )

        write ( *, '(2x,i3,6x,f6.2,9x,f6.2,9x,f6.2)' ) 
     &    ival, xval, yval, zval

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vector of unique X Y Z values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  X(I)   Y(I)    Z(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,f6.2,9x,f6.2,9x,f6.2)' ) 
     &    i, x(i), y(i), z(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X Y Z sorted by index:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(indx(i)), y(indx(i)), z(indx(i))
      end do

      return
      end
      subroutine r8int_to_i4int_test ( )

c*********************************************************************72
c
cc R8INT_TO_I4INT_TEST tests R8INT_TO_I4INT and I4INT_TO_R8INT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ihi
      integer ilo
      integer ir
      double precision r
      double precision r2
      double precision r8_uniform_ab
      double precision rhi
      double precision rhi2
      double precision rlo
      double precision rlo2
      integer seed
      integer test
      integer test_num

      ilo = 1
      ihi = 11
      rlo = 100.0D+00
      rhi = 200.0D+00
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8INT_TO_I4INT_TEST'
      write ( *, '(a)' ) '  For data in an interval,'
      write ( *, '(a)' ) 
     &  '  R8INT_TO_I4INT converts a real to an integer.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  Integer interval: ', ilo, ihi
      write ( *, '(a,2g14.6)' ) '  Real interval: ', rlo, rhi
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  R   I(R)  R(I(R))'
      write ( *, '(a)' ) ' '

      seed = 123456789

      rlo2 = rlo - 15.0D+00
      rhi2 = rhi + 15.0D+00

      do test = 1, test_num
        r = r8_uniform_ab ( rlo2, rhi2, seed )
        call r8int_to_i4int ( rlo, rhi, r, ilo, ihi, ir )
        call i4int_to_r8int ( ilo, ihi, ir, rlo, rhi, r2 )
        write ( *, '(2x,g14.6,i8,g14.6)' ) r, ir, r2
      end do

      return
      end
      subroutine r8mat_cholesky_inverse_test ( )

c*********************************************************************72
c
cc R8MAT_CHOLESKY_INVERSE tests the Cholesky routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2013
c
c  Author:
c
c    John Burkardt
c
c     implicit none
c
      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision a3(n,n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_CHOLESKY_INVERSE'
      write ( *, '(a)' ) '  For a positive definite symmetric matrix,'
      write ( *, '(a)' ) 
     &  '  R8MAT_CHOLESKY_INVERSE computes the inverse.'

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = -1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a, '  Matrix to be inverted:' )

      call r8mat_copy ( n, n, a, a2 )

      call r8mat_cholesky_inverse ( n, a2 )

      call r8mat_print ( n, n, a2, '  Inverse matrix:' )

      call r8mat_mm ( n, n, n, a2, a, a3 )
  
      call r8mat_print ( n, n, a3, '  Product inv(A) * A:' )

      return
      end
      subroutine r8mat_cholesky_solve_test ( )

c*********************************************************************72
c
cc R8MAT_CHOLESKY_SOLVE_TEST tests R8MAT_CHOLESKY_SOLVE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2013
c
c  Author:
c
c    John Burkardt
c
c     implicit none
c
      integer n
      parameter ( n = 5 )

      double precision a(n,n)

      double precision b(n)
      double precision d(n,n)
      integer i
      integer ierror
      integer j
      double precision l(n,n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_CHOLESKY_SOLVE_TEST'
      write ( *, '(a)' ) '  For a positive definite symmetric matrix,'
      write ( *, '(a)' ) '  R8MAT_CHOLESKY_SOLVE solves a linear system'
      write ( *, '(a)' ) '  using the lower Cholesky factorization.'

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = -1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a, '  Matrix to be factored:' )
c
c  Compute the Cholesky factor.
c
      call r8mat_cholesky_factor ( n, a, l, ierror )
      call r8mat_print ( n, n, l, '  Cholesky factor L:' )
      call r8mat_mmt ( n, n, n, l, l, d )
      call r8mat_print ( n, n, d, '  Product L * L'':' )
c
c  Solve a linear system.
c
      do i = 1, n - 1
        b(i) = 0.0D+00
      end do
      b(n) = dble ( n + 1 )
      call r8vec_print ( n, b, '  Right hand side:' )

      call r8mat_cholesky_solve ( n, l, b, x )

      call r8vec_print ( n, x, '  Computed solution:' )

      return
      end
      subroutine r8mat_cholesky_solve_upper_test ( )

c*********************************************************************72
c
cc R8MAT_CHOLESKY_SOLVE_UPPER_TEST tests R8MAT_CHOLESKY_SOLVE_UPPER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2013
c
c  Author:
c
c    John Burkardt
c
c     implicit none
c
      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision b(n)
      double precision d(n,n)
      integer i
      integer ierror
      integer j
      double precision r(n,n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_CHOLESKY_SOLVE_UPPER_TEST'
      write ( *, '(a)' ) '  For a positive definite symmetric matrix,'
      write ( *, '(a)' ) 
     &  '  R8MAT_CHOLESKY_SOLVE_UPPER solves a linear system'
      write ( *, '(a)' ) '  using the upper Cholesky factorization.'

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = -1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a, '  Matrix to be factored:' )
c
c  Compute the Cholesky factor.
c
      call r8mat_cholesky_factor_upper ( n, a, r, ierror )
      call r8mat_print ( n, n, r, '  Cholesky factor R:' )
      call r8mat_mtm ( n, n, n, r, r, d )
      call r8mat_print ( n, n, d, '  Product R'' * R:' )
c
c  Solve a linear system.
c
      do i = 1, n - 1
        b(i) = 0.0D+00
      end do
      b(n) = dble ( n + 1 )
      call r8vec_print ( n, b, '  Right hand side:' )

      call r8mat_cholesky_solve_upper ( n, r, b, x )

      call r8vec_print ( n, x, '  Computed solution:' )

      return
      end
      subroutine r8mat_det_2d_test ( )

c*********************************************************************72
c
cc R8MAT_DET_2D_TEST tests R8MAT_DET_2D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 2 )

      double precision a(n,n)
      double precision det
      integer i
      integer j
      double precision r8mat_det_2d
      double precision x(n)

      save x

      data x / 1.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_DET_2D_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_DET_2D: determinant of a 2 by 2 matrix;'

      call r8mat_vand2 ( n, x, a )

      det = r8mat_det_2d ( a )

      call r8mat_print ( n, n, a, '  Matrix:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  R8MAT_DET_2D computes determinant:', det
c
c  Special formula for the determinant of a Vandermonde matrix:
c
      det = 1.0D+00
      do i = 1, n
        do j = 1, i-1
          det = det * ( x(i) - x(j) )
        end do
      end do
      write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

      return
      end
      subroutine r8mat_det_3d_test ( )

c*********************************************************************72
c
cc R8MAT_DET_3D_TEST tests R8MAT_DET_3D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      double precision det
      integer i
      integer j
      double precision r8mat_det_3d
      double precision x(n)

      save x

      data x / 1.0D+00, 10.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_DET_3D_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_DET_3D: determinant of a 3 by 3 matrix;'

      call r8mat_vand2 ( n, x, a )
      det = r8mat_det_3d ( a )

      call r8mat_print ( n, n, a, '  Matrix:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  R8MAT_DET_3D computes determinant:', det
c
c  Special formula for the determinant of a Vandermonde matrix:
c
      det = 1.0D+00
      do i = 1, n
        do j = 1, i - 1
          det = det * ( x(i) - x(j) )
        end do
      end do
      write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

      return
      end
      subroutine r8mat_det_4d_test ( )

c*********************************************************************72
c
cc R8MAT_DET_4D_TEST tests R8MAT_DET_4D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision det
      integer i
      integer j
      double precision r8mat_det_4d
      double precision x(n)

      save x

      data x / 1.0D+00, 10.0D+00, 4.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_DET_4D_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_DET_4D determinant of a 4 by 4 matrix;'

      call r8mat_vand2 ( n, x, a )
      det = r8mat_det_4d ( a )

      call r8mat_print ( n, n, a, '  Matrix:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  R8MAT_DET_4D computes determinant:', det
c
c  Special formula for the determinant of a Vandermonde matrix:
c
      det = 1.0D+00
      do i = 1, n
        do j = 1, i - 1
          det = det * ( x(i) - x(j) )
        end do
      end do
      write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

      return
      end
      subroutine r8mat_det_5d_test ( )

c*********************************************************************72
c
cc R8MAT_DET_5D_TEST tests R8MAT_DET_5D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision det
      integer i
      integer j
      double precision r8mat_det_5d
      double precision x(n)

      save x

      data x /
     &  1.0D+00, 10.0D+00, 4.0D+00, 2.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_DET_5D_TEST'
      write ( *, '(a)' ) '  R8MAT_DET_5D: determinant of 5 by 5 matrix.'

      call r8mat_vand2 ( n, x, a )
      det = r8mat_det_5d ( a )

      call r8mat_print ( n, n, a, '  Matrix:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  R8MAT_DET_5D computes determinant: ', det
c
c  Special formula for the determinant of a Vandermonde matrix:
c
      det = 1.0D+00
      do i = 1, n
        do j = 1, i - 1
          det = det * ( x(i) - x(j) )
        end do
      end do
      write ( *, '(a,g14.6)' ) '  Exact determinant is ', det

      return
      end
      subroutine r8mat_expand_linear_test ( )

c*********************************************************************72
c
cc R8MAT_EXPAND_LINEAR_TEST tests R8MAT_EXPAND_LINEAR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 3 )
      integer mfat
      parameter ( mfat = 2 )
      integer nfat
      parameter ( nfat = 1 )

      integer m2
      parameter ( m2 = ( m - 1 ) * ( mfat + 1 ) + 1 )
      integer n2
      parameter ( n2 = ( n - 1 ) * ( nfat + 1 ) + 1 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision x(m,n)
      double precision xfat(m2,n2)

      save x

      data x /
     &  1.0D+00, 2.0D+00,  3.0D+00,  4.0D+00, 
     &  1.0D+00, 4.0D+00,  9.0D+00, 16.0D+00, 
     &  1.0D+00, 8.0D+00, 27.0D+00, 64.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_EXPAND_LINEAR_TEST'
      write ( *, '(a)' ) '  R8MAT_EXPAND_LINEAR linearly interpolates'
      write ( *, '(a)' ) '  new data between old values in a matrix.'

      call r8mat_print ( m, n, x, '  Original matrix:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  MFAT = ', mfat
      write ( *, '(a,i8)' ) '  NFAT = ', nfat

      call r8mat_expand_linear ( m, n, x, mfat, nfat, xfat )

      call r8mat_print ( m2, n2, xfat, '  Fattened matrix:' )

      return
      end
      subroutine r8mat_expand_linear2_test ( )

c*********************************************************************72
c
cc R8MAT_EXPAND_LINEAR2_TEST tests R8MAT_EXPAND_LINEAR2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer m2
      parameter ( m2 = 10 )
      integer n
      parameter ( n = 2 )
      integer n2
      parameter ( n2 = 5 )

      double precision a(m,n)
      double precision a2(m2,n2)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_EXPAND_LINEAR2_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_EXPAND_LINEAR2 fills in a large array by'
      write ( *, '(a)' ) '  interpolating data from a small array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Original matrix has dimensions:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,2i8)' ) m, n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Expanded matrix has dimensions:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,2i8)' ) m2, n2

      do i = 1, m
        do j = 1, n
          a(i,j) = 10.0D+00 * dble ( i ) + dble ( j )
        end do
      end do

      call r8mat_print ( m, n, a, '  The little matrix A:' )

      call r8mat_expand_linear2 ( m, n, a, m2, n2, a2 )

      call r8mat_print ( m2, n2, a2, '  Expanded array A2:' )

      return
      end
      subroutine r8mat_fs_test ( )

c*********************************************************************72
c
cc R8MAT_FS_TEST tests R8MAT_FS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n,n)
      double precision b(n)
      integer i
      integer info
      integer seed
      double precision x(n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_FS_TEST'
      write ( *, '(a)' ) '  For a matrix in general storage,'
      write ( *, '(a)' ) 
     &  '  R8MAT_FS factors and solves a linear system.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_01 ( n, n, seed, a )
c
c  Set the desired solutions.
c
      do i = 1, n
        x(i) = dble ( i )
      end do
      call r8mat_mv ( n, n, a, x, b )
c
c  Factor and solve the system.
c
      call r8mat_fs ( n, a, b, info )
      
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_FS_TEST - Fatal error!'
        write ( *, '(a)' ) '  R8MAT_FS reports the matrix is singular.'
        return
      end if

      call r8vec_print ( n, b, '  Solution:' )

      return
      end
      subroutine r8mat_fss_test ( )

c*********************************************************************72
c
cc R8MAT_FSS_TEST tests R8MAT_FSS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 November 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )
      integer nb
      parameter ( nb = 3 )

      double precision a(n,n)
      double precision b(n,nb)
      integer i
      integer info
      integer seed
      double precision x(n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_FSS_TEST'
      write ( *, '(a)' ) '  For a matrix in general storage,'
      write ( *, '(a)' ) 
     &  '  R8MAT_FSS factors and solves multiple linear system.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_01 ( n, n, seed, a )
c
c  Set the desired solutions.
c
      do i = 1, n
        x(i) = 1.0D+00
      end do
      call r8mat_mv ( n, n, a, x, b(1,1) )

      do i = 1, n
        x(i) = dble ( i )
      end do
      call r8mat_mv ( n, n, a, x, b(1,2) )

      do i = 1, n
        x(i) = dble ( mod ( i - 1, 3 ) + 1 )
      end do
      call r8mat_mv ( n, n, a, x, b(1,3) )
c
c  Factor and solve the system.
c
      call r8mat_fss ( n, a, nb, b, info )
      
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_FSS_TEST - Fatal error!'
        write ( *, '(a)' ) '  R8MAT_FSS reports the matrix is singular.'
        return
      end if

      call r8mat_print ( n, nb, b, '  Solutions:' )

      return
      end
      subroutine r8mat_givens_post_test ( )

c*********************************************************************72
c
cc R8MAT_GIVENS_POST_TEST tests R8MAT_GIVENS_POST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      double precision ag(n,n)
      integer col
      double precision g(n,n)
      integer i
      integer j
      integer row
   
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_GIVENS_POST_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_GIVENS_POST computes a Givens ' // 
     &  'postmultiplier rotation matrix.'

      do i = 1, n
        do j = 1, n
          a(i,j) = dble ( i ** ( j - 1 ) )
        end do
      end do

      call r8mat_print ( n, n, a, '  Matrix A:' )

      row = 3
      col = 2

      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  I, J=', row, col

      call r8mat_givens_post ( n, a, row, col, g )

      call r8mat_print ( n, n, g, '  G' )

      call r8mat_mm ( n, n, n, a, g, ag )

      call r8mat_print ( n, n, ag, '  A*G' )

      return
      end
      subroutine r8mat_givens_pre_test ( )

c*********************************************************************72
c
cc R8MAT_GIVENS_PRE_TEST tests R8MAT_GIVENS_PRE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c 
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      integer col
      double precision g(n,n)
      double precision ga(n,n)
      integer i
      integer j
      integer row

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_GIVENS_PRE_TEST'
      write ( *, '(a)' ) '  R8MAT_GIVENS_PRE computes a Givens ' //
     &  'premultiplier rotation matrix.'

      do i = 1, n
        do j = 1, n
          a(i,j) = dble ( i ** ( j - 1 ) )
        end do
      end do

      call r8mat_print ( n, n, a, '  Matrix A:' )

      row = 3
      col = 2

      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  I, J=', row, col

      call r8mat_givens_pre ( n, a, row, col, g )

      call r8mat_print ( n, n, g, '  G' )
 
      call r8mat_mm ( n, n, n, g, a, ga )

      call r8mat_print ( n, n, ga, '  G*A' )

      return
      end
      subroutine r8mat_hess_test ( )

c*********************************************************************72
c
cc R8MAT_HESS_TEST tests R8MAT_HESS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision h(n,n)
      external r8mat_hess_f
      double precision x(n)

      save x

      data x /
     &  1.0D+00, 2.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_HESS_TEST'
      write ( *, '(a)' ) '  R8MAT_HESS estimates the Hessian matrix'
      write ( *, '(a)' ) '  of a scalar function.'

      call r8mat_hess ( r8mat_hess_f, n, x, h )

      call r8mat_print ( n, n, h, '  Estimated jacobian:' )

      call r8mat_hess_exact ( n, x, h )

      call r8mat_print ( n, n, h, '  Exact jacobian:' )

      return
      end
      subroutine r8mat_hess_f ( n, x, f )

c*********************************************************************72
c
cc R8MAT_HESS_F is a sample nonlinear function for treatment by R8MAT_HESS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of parameters.
c
c    Input, double precision X(N), the parameter values.
c
c    Output, double precision F, the function value.
c
      implicit none

      integer n

      double precision f
      double precision x(n)

      f = x(1)**2 + x(1) * x(2) + x(2) * cos ( 10.0D+00 * x(3) )

      return
      end
      subroutine r8mat_hess_exact ( n, x, h )

c*********************************************************************72
c
cc R8MAT_HESS_EXACT is the exact Hessian of R8MAT_HESS_F.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of parameters.
c
c    Input, double precision X(N), the parameter values.
c
c    Output, double precision H(N,N), the Hessian values.
c
      implicit none

      integer n

      double precision h(n,n)
      double precision x(n)

      h(1,1) = 2.0D+00
      h(1,2) = 1.0D+00
      h(1,3) = 0.0D+00

      h(2,1) = 1.0D+00
      h(2,2) = 0.0D+00
      h(2,3) = - 10.0D+00 * sin ( 10.0D+00 * x(3) )

      h(3,1) = 0.0D+00
      h(3,2) = - 10.0D+00 * sin ( 10.0D+00 * x(3) )
      h(3,3) = - 100.0D+00 * x(2) * cos ( 10.0D+00 * x(3) )

      return
      end
      subroutine r8mat_house_axh_test ( )

c*********************************************************************72
c
cc R8MAT_HOUSE_AXH_TEST tests R8MAT_HOUSE_AXH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision a_col(n)
      double precision ah(n,n)
      double precision h(n,n)
      double precision ha(n,n)
      integer i
      integer k
      double precision r8_hi
      double precision r8_lo
      integer seed
      double precision v(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_HOUSE_AXH_TEST'
      write ( *, '(a)' ) '  R8MAT_HOUSE_AXH multiplies a matrix A'
      write ( *, '(a)' ) '  times a compact Householder matrix.'

      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789

      call r8mat_uniform_ab ( n, n, r8_lo, r8_hi, seed, a )

      call r8mat_print ( n, n, a, '  Matrix A:' )
c
c  Request V, the compact form of the Householder matrix H
c  such that H*A packs column 3 of A.
c
      k = 3
      do i = 1, n
        a_col(i) = a(i,k)
      end do

      call r8vec_house_column ( n, a_col, k, v )

      call r8vec_print ( n, v, 
     &  '  Compact vector V so H*A packs column 3:' )

      call r8mat_house_form ( n, v, h )

      call r8mat_print ( n, n, h, '  Householder matrix H:' )
c
c  Compute A*H.
c
      call r8mat_house_axh ( n, a, v, ah )

      call r8mat_print ( n, n, ah, '  Indirect product A*H:' )
c
c  Compare with a direct calculation of A*H.
c
      call r8mat_mm ( n, n, n, a, h, ah )

      call r8mat_print ( n, n, ah, '  Direct product A*H:' )
c
c  Compute H*A to demonstrate packing.
c
      call r8mat_mm ( n, n, n, h, a, ha )

      call r8mat_print ( n, n, ha, 
     &  '  Product H*A has packed column 3:' )

      return
      end
      subroutine r8mat_house_form_test ( )

c*********************************************************************72
c
cc R8MAT_HOUSE_FORM_TEST tests R8MAT_HOUSE_FORM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision h(n,n)
      double precision v(n)

      save v

      data v /
     &  0.0D+00, 0.0D+00, 1.0D+00, 2.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_HOUSE_FORM_TEST'
      write ( *, '(a)' ) '  R8MAT_HOUSE_FORM forms a Householder'
      write ( *, '(a)' ) '  matrix from its compact form.'

      call r8vec_print ( n, v, '  Compact vector form V:' )

      call r8mat_house_form ( n, v, h )

      call r8mat_print ( n, n, h, '  Householder matrix H:' )

      return
      end
      subroutine r8mat_house_post_test ( )

c*********************************************************************72
c
cc R8MAT_HOUSE_POST_TEST tests R8MAT_HOUSE_POST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision ah(n,n)
      double precision b
      double precision c
      double precision h(n,n)
      integer row
      integer col
      integer seed

      b = 0.0D+00
      c = 5.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_HOUSE_POST'
      write ( *, '(a)' ) '  R8MAT_HOUSE_POST computes a Householder'
      write ( *, '(a)' ) '  postmultiplier;'

      seed = 123456789

      call r8mat_uniform_ab ( n, n, b, c, seed, a )

      call r8mat_print ( n, n, a, '  Matrix A:' )

      row = 2
      col = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  I, J=', row, col

      call r8mat_house_post ( n, a, row, col, h )

      call r8mat_print ( n, n, h, '  Householder matrix H:' )

      call r8mat_mm ( n, n, n, a, h, ah )
 
      call r8mat_print ( n, n, ah, '  Product A*H:' )

      return
      end
      subroutine r8mat_house_pre_test ( )

c*********************************************************************72
c
cc R8MAT_HOUSE_PRE_TEST tests R8MAT_HOUSE_PRE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision b
      double precision c
      integer col
      double precision h(n,n)
      double precision ha(n,n)
      integer row
      integer seed

      b = 0.0D+00
      c = 5.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_HOUSE_PRE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_HOUSE_PRE computes a Householder premultiplier;'

      seed = 123456789

      call r8mat_uniform_ab ( n, n, b, c, seed, a )

      call r8mat_print ( n, n, a, '  Matrix A:' )

      row = 2
      col = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  I, J=', row, col

      call r8mat_house_pre ( n, a, row, col, h )

      call r8mat_print ( n, n, h, '  Householder matrix H:' )

      call r8mat_mm ( n, n, n, h, a, ha )

      call r8mat_print ( n, n, ha, '  Product H*A:' )

      return
      end
      subroutine r8mat_indicator_test ( )

c*********************************************************************72
c
cc R8MAT_INDICATOR_TEST tests R8MAT_INDICATOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_INDICATOR_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_INDICATOR returns an indicator matrix.'

      call r8mat_indicator ( m, n, a )

      call r8mat_print ( m, n, a, '  The indicator matrix:' )

      return
      end
      subroutine r8mat_inverse_2d_test ( )

c*********************************************************************72
c
cc R8MAT_INVERSE_2D_TEST tests R8MAT_INVERSE_2D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 2 )
c
c  Each ROW of this definion is a COLUMN of the matrix.
c
      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)
      double precision det

      save a

      data a /
     &  1.0D+00, 3.0D+00, 
     &  2.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_INVERSE_2D_TEST'
      write ( *, '(a)' ) '  R8MAT_INVERSE_2D inverts a 2 by 2 matrix.'

      call r8mat_print ( n, n, a, '  Matrix A to invert:' )
c
c  Compute the inverse matrix.
c
      call r8mat_inverse_2d ( a, b, det )

      if ( det .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The input matrix was singular, no inverse'
        write ( *, '(a)' ) '  could be computed.'
        return
      end if

      call r8mat_print ( n, n, b, '  Inverse matrix B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_inverse_3d_test ( )

c*********************************************************************72
c
cc R8MAT_INVERSE_3D_TEST tests R8MAT_INVERSE_3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )
c
c  Each ROW of this definion is a COLUMN of the matrix.
c
      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)
      double precision det

      save a

      data a /
     &  1.0D+00, 4.0D+00, 7.0D+00, 
     &  2.0D+00, 5.0D+00, 8.0D+00, 
     &  3.0D+00, 6.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_INVERSE_3D_TEST'
      write ( *, '(a)' ) '  R8MAT_INVERSE_3D inverts a 3 by 3 matrix.'

      call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )
c
c  Compute the inverse matrix.
c
      call r8mat_inverse_3d ( a, b, det )

      if ( det .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The input matrix was singular, no inverse'
        write ( *, '(a)' ) '  could be computed.'
        return
      end if

      call r8mat_print ( n, n, b, '  Inverse matrix B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_inverse_4d_test ( )

c*********************************************************************72
c
cc R8MAT_INVERSE_4D_TEST tests R8MAT_INVERSE_4D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)
      double precision det
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_INVERSE_4D_TEST'
      write ( *, '(a)' ) '  R8MAT_INVERSE_4D inverts a 4 x 4 matrix.'

      do i = 1, n
        do j = 1, n

          if ( i .le. j ) then
            a(i,j) = dble ( n + 1 - j )
          else if ( j .eq. i - 1 ) then
            a(i,j) = n - j
          else
            a(i,j) = 0.0D+00
          end if

        end do
      end do

      call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )

      call r8mat_inverse_4d ( a, b, det )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Determinant is ', det

      call r8mat_print ( n, n, b, '  Inverse B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_jac_test ( )

c*********************************************************************72
c
cc R8MAT_JAC_TEST tests R8MAT_JAC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision eps
      double precision fprime(m,n)
      external r8mat_jac_f
      double precision x(n)

      save x

      data x /
     & 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /

      eps = 0.00001D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_JAC_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_JAC estimates the M by N jacobian matrix'
      write ( *, '(a)' ) '  of a nonlinear function.'

      call r8mat_jac ( m, n, eps, r8mat_jac_f, x, fprime )

      call r8mat_print ( m, n, fprime, '  Estimated jacobian:' )

      call r8mat_jac_exact ( m, n, x, fprime )

      call r8mat_print (  m, n, fprime, '  Exact jacobian:' )

      return
      end
      subroutine r8mat_jac_f ( m, n, x, f )

c*********************************************************************72
c
cc R8MAT_JAC_F is a sample nonlinear function for treatment by R8MAT_JAC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of functions.
c
c    Input, integer N, the number of parameters.
c
c    Input, double precision X(N), the parameter values.
c
c    Output, double precision F(M), the function values.
c
      implicit none

      integer m
      integer n

      double precision f(m)
      double precision x(n)

      f(1) = sin ( x(1) * x(2) )
      f(2) = sqrt ( 1.0D+00 + x(1)**2 ) + x(3)
      f(3) = x(1) + 2.0D+00 * x(2) + 3.0D+00 * x(3) + 4.0D+00 * x(4)

      return
      end
      subroutine r8mat_jac_exact ( m, n, x, fprime )

c*********************************************************************72
c
cc R8MAT_JAC_EXAT is the exact jacobian of R8MAT_JAC_F.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of functions.
c
c    Input, integer N, the number of parameters.
c
c    Input, double precision X(N), the parameter values.
c
c    Output, double precision FPRIME(M,N), the jacobian values.
c
      implicit none

      integer m
      integer n

      double precision fprime(m,n)
      double precision x(n)

      fprime(1,1) = cos ( x(1) * x(2) ) * x(2)
      fprime(1,2) = cos ( x(1) * x(2) ) * x(1)
      fprime(1,3) = 0.0D+00
      fprime(1,4) = 0.0D+00

      fprime(2,1) = x(1) / sqrt ( 1.0D+00 + x(1)**2 )
      fprime(2,2) = 0.0D+00
      fprime(2,3) = 1.0D+00
      fprime(2,4) = 0.0D+00

      fprime(3,1) = 1.0D+00
      fprime(3,2) = 2.0D+00
      fprime(3,3) = 3.0D+00
      fprime(3,4) = 4.0D+00

      return
      end
      subroutine r8mat_kronecker_test ( )

c*********************************************************************72
c
cc R8MAT_KRONECKER_TEST tests R8MAT_KRONECKER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m1
      integer m2
      integer m
      integer n1
      integer n2
      integer n

      parameter ( m1 = 2 )
      parameter ( m2 = 3 )
      parameter ( m = m1 * m2 )
      parameter ( n1 = 3 )
      parameter ( n2 = 2 )
      parameter ( n = n1 * n2 )

      double precision a(m1,n1)
      double precision b(m2,n2)
      double precision c(m,n)

      save a
      save b

      data a /
     &  1.0D+00, 4.0D+00, 
     &  2.0D+00, 5.0D+00, 
     &  3.0D+00, 6.0D+00 /
      data b /
     &  7.0D+00,  9.0D+00, 11.0D+00, 
     &  8.0D+00, 10.0D+00, 12.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_KRONECKER_TEST'
      write ( *, '(a)' ) '  R8MAT_KRONECKER computes the Kronecker'
      write ( *, '(a)' ) '  product of two matrices.'

      call r8mat_print ( m1, n1, a, '  Factor matrix A:' )
      call r8mat_print ( m2, n2, b, '  Factor matrix B:' )

      call r8mat_kronecker ( m1, n1, a, m2, n2, b, c )

      call r8mat_print ( m, n, c, '  Kronecker product C = kron(A,B)' )

      return
      end
      subroutine r8mat_l_inverse_test ( )

c*********************************************************************72
c
cc R8MAT_L_INVERSE_TEST tests R8MAT_L_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)

      save a

      data a /
     &  1.0D+00, 2.0D+00, 4.0D+00,  7.0D+00, 
     &  0.0D+00, 3.0D+00, 5.0D+00,  8.0D+00, 
     &  0.0D+00, 0.0D+00, 6.0D+00,  9.0D+00, 
     &  0.0D+00, 0.0D+00, 0.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_L_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_L_INVERSE inverts a lower triangular matrix.'

      call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )

      call r8mat_l_inverse ( n, a, b )

      call r8mat_print ( n, n, b, '  Inverse matrix B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_l_print_test ( )

c*********************************************************************72
c
cc R8MAT_L_PRINT_TEST tests R8MAT_L_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a1(28)
      double precision a2(18)
      double precision a3(10)
      integer m1
      parameter ( m1 = 7 )
      integer m2
      parameter ( m2 = 7 )
      integer m3
      parameter ( m3 = 4 )
      integer n1
      parameter ( n1 = 7 )
      integer n2
      parameter ( n2 = 3 )
      integer n3
      parameter ( n3 = 7 )

      save a1
      save a2
      save a3

      data a1 /
     &  11.0D+00, 21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 
     &                                61.0D+00, 71.0D+00, 
     &            22.0D+00, 32.0D+00, 42.0D+00, 52.0D+00, 
     &                                62.0D+00, 72.0D+00, 
     &                      33.0D+00, 43.0D+00, 53.0D+00, 
     &                                63.0D+00, 73.0D+00, 
     &                                44.0D+00, 54.0D+00, 
     &                                64.0D+00, 74.0D+00, 
     &                                          55.0D+00, 
     &                                65.0D+00, 75.0D+00, 
     &                                66.0D+00, 76.0D+00, 
     &                                          77.0D+00 /
      data a2 /
     &  11.0D+00, 21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 
     &                                61.0D+00, 71.0D+00, 
     &            22.0D+00, 32.0D+00, 42.0D+00, 52.0D+00, 
     &                                62.0D+00, 72.0D+00, 
     &                      33.0D+00, 43.0D+00, 53.0D+00, 
     &                                63.0D+00, 73.0D+00 /
      data a3 /
     &  11.0D+00, 21.0D+00, 31.0D+00, 41.0D+00, 
     &            22.0D+00, 32.0D+00, 42.0D+00, 
     &                      33.0D+00, 43.0D+00, 
     &                                44.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_L_PRINT_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_L_PRINT prints a lower triangular matrix'
      write ( *, '(a)' ) 
     &  '  stored compactly.  Only the (possibly) nonzero '
      write ( *, '(a)' ) '  elements are printed.'

      call r8mat_l_print ( m1, n1, a1, '  A 7 by 7 matrix.' )

      call r8mat_l_print ( m2, n2, a2, '  A 7 by 3 matrix.' )

      call r8mat_l_print ( m3, n3, a3, '  A 4 by 7 matrix.' )

      return
      end
      subroutine r8mat_l1_inverse_test ( )

c*********************************************************************72
c
cc R8MAT_L1_INVERSE_TEST tests R8MAT_L1_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)

      save a

      data a /
     &  1.0D+00, 2.0D+00, 0.0D+00, 5.0D+00, 0.0D+00, 75.0D+00, 
     &  0.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 
     &  0.0D+00, 0.0D+00, 1.0D+00, 3.0D+00, 0.0D+00,  0.0D+00, 
     &  0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00, 0.0D+00,  6.0D+00, 
     &  0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00,  4.0D+00, 
     &  0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_L1_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_L1_INVERSE inverts a unit lower triangular matrix.'

      call r8mat_print ( n, n, a, '  Matrix A to be inverted:' )

      call r8mat_l1_inverse ( n, a, b )

      call r8mat_print ( n, n, b, '  Inverse matrix B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_lu_test ( )

c*********************************************************************72
c
cc R8MAT_LU_TEST tests R8MAT_LU.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 5 )

      double precision a(m,n)
      double precision l(m,m)
      double precision lu(m,n)
      double precision p(m,m)
      double precision plu(m,n)
      double precision u(m,n)
      double precision x(n)

      save x

      data x /
     &  1.0D+00, 10.0D+00, 4.0D+00, 2.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_LU_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_LU computes the LU factors of a matrix.'

      call r8mat_vand2 ( n, x, a )

      call r8mat_print ( m, n, a, '  Matrix to be factored:' )

      call r8mat_lu ( m, n, a, l, p, u )

      call r8mat_print ( m, m, p, '  P factor:' )

      call r8mat_print ( m, m, l, '  L factor:' )

      call r8mat_print ( m, n, u, '  U factor:' )

      call r8mat_mm ( m, m, n, l, u, lu )

      call r8mat_mtm ( m, m, n, p, lu, plu )

      call r8mat_print ( m, n, plu, '  P*L*U:' )

      return
      end
      subroutine r8mat_max_test ( )

c*********************************************************************72
c
cc R8MAT_MAX_TEST tests R8MAT_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      double precision r8mat_max
      integer seed
      double precision temp

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MAX_TEST'
      write ( *, '(a)' ) '  For a real matrix,'
      write ( *, '(a)' ) '  R8MAT_MAX computes the maximum value;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      temp = r8mat_max ( m, n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Maximum value = ', temp

      return
      end
      subroutine r8mat_max_index_test ( )

c*********************************************************************72
c
cc R8MAT_MAX_INDEX_TEST tests R8MAT_MAX_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m 
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      integer i
      integer j
      integer seed

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MAX_INDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_MAX_INDEX locates the maximum entry of an R8MAT;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      call r8mat_max_index ( m, n, a, i, j )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2i8)' ) '  Maximum I,J indices            ', i, j

      return
      end
      subroutine r8mat_maxcol_minrow_test ( )

c*********************************************************************72
c
cc R8MAT_MAXCOL_MINROW_TEST tests R8MAT_MAXCOL_MINROW.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      double precision r8mat_maxcol_minrow
      integer seed
      double precision temp

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MAXCOL_MINROW_TEST'
      write ( *, '(a)' ) '  R8MAT_MAXCOL_MINROW computes the maximum'
      write ( *, '(a)' ) '  over columns of the mininum over rows;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      temp = r8mat_maxcol_minrow ( m, n, a )

      write ( *, '(a,g14.6)' ) '  MAXCOL_MINROW = ', temp

      return
      end
      subroutine r8mat_maxrow_mincol_test ( )

c*********************************************************************72
c
cc R8MAT_MAXROW_MINCOL_TEST tests R8MAT_MAXROW_MINCOL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      double precision r8mat_maxrow_mincol
      integer seed
      double precision temp

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MAXROW_MINCOL_TEST'
      write ( *, '(a)' ) '  R8MAT_MAXROW_MINCOL computes the maximum'
      write ( *, '(a)' ) '  over rows of the mininum over columns;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      temp = r8mat_maxrow_mincol ( m, n, a )

      write ( *, '(a,g14.6)' ) '  MAXROW_MINCOL = ', temp

      return
      end
      subroutine r8mat_min_test ( )

c*********************************************************************72
c
cc R8MAT_MIN_TEST tests R8MAT_MIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      double precision r8mat_min
      integer seed
      double precision temp

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MIN_TEST'
      write ( *, '(a)' ) '  For a real matrix,'
      write ( *, '(a)' ) '  R8MAT_MIN computes the minimum value;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      temp = r8mat_min ( m, n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Minimum value = ', temp

      return
      end
      subroutine r8mat_min_index_test ( )

c*********************************************************************72
c
cc R8MAT_MIN_INDEX_TEST tests R8MAT_MIN_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m 
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      integer i
      integer j
      integer seed

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MIN_INDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_MIN_INDEX locates the minimum entry of an R8MAT;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      write ( *, '(a)' ) ' '
      call r8mat_min_index ( m, n, a, i, j )
      write ( *, '(a,2i8)' ) '  Minimum I,J indices            ', i, j

      return
      end
      subroutine r8mat_mincol_maxrow_test ( )

c*********************************************************************72
c
cc R8MAT_MINCOL_MAXROW_TEST tests R8MAT_MINCOL_MAXROW.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      double precision r8mat_mincol_maxrow
      integer seed
      double precision temp

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MINCOL_MAXROW_TEST'
      write ( *, '(a)' ) '  R8MAT_MINCOL_MAXROW computes the minimum'
      write ( *, '(a)' ) '  over columns of the maxinum over rows;'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      temp = r8mat_mincol_maxrow ( m, n, a )

      write ( *, '(a,g14.6)' ) '  MINCOL_MAXROW = ', temp

      return
      end
      subroutine r8mat_minrow_maxcol_test ( )

c*********************************************************************72
c
cc R8MAT_MINROW_MAXCOL_TEST tests R8MAT_MINROW_MAXCOL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      double precision r8mat_minrow_maxcol
      integer seed
      double precision temp

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MINROW_MAXCOL_TEST'
      write ( *, '(a)' ) '  R8MAT_MINROW_MAXCOL computes the minimum'
      write ( *, '(a)' ) '  over rows of the maxinum over columns;'
      write ( *, '(a)' ) ' '

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  Random array:' )

      temp = r8mat_minrow_maxcol ( m, n, a )

      write ( *, '(a,g14.6)' ) '  MINROW_MAXCOL = ', temp

      return
      end
      subroutine r8mat_mm_test ( )

c*********************************************************************72
c
cc R8MAT_MM_TEST tests R8MAT_MM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n1
      parameter ( n1 = 4 )
      integer n2
      parameter ( n2 = 3 )
      integer n3
      parameter ( n3 = n1 )

      double precision a(n1,n2)
      double precision b(n2,n3)
      double precision c(n1,n3)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MM_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_MM computes a matrix-matrix product C = A * B.'

      do i = 1, n1
        do j = 1, n2

          if ( j .eq. 1 ) then
            a(i,j) = 1.0D+00
          else if ( i .eq. 1 ) then
            a(i,j) = 0.0D+00
          else
            a(i,j) = a(i-1,j-1) + a(i-1,j)
          end if

        end do
      end do

      do j = 1, n3
        do i = 1, n2
          b(i,j) = a(j,i)
        end do
      end do

      call r8mat_mm ( n1, n2, n3, a, b, c )

      call r8mat_print ( n1, n2, a, '  A:' )
      call r8mat_print ( n2, n3, b, '  B:' )
      call r8mat_print ( n1, n3, c, '  C = A*B:' )

      return
      end
      subroutine r8mat_mv_test ( )

c*********************************************************************72
c
cc R8MAT_MV_TEST tests R8MAT_MV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 2 )

      double precision a(m,n)
      double precision b(m)
      double precision x(n)

      save a
      save x

      data a /
     &  1.0, 2.0, 3.0, 4.0, 
     &  1.0, 1.0, 1.0, 1.0 /
      data x / 1.0, 2.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_MV_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_MV computes a matrix-vector product b = A * x;'

      call r8mat_mv ( m, n, a, x, b )

      call r8mat_print ( m, n, a, '  A:' )
      call r8vec_print ( n, x, '  X:' )
      call r8vec_print ( m, b, '  B = A*X:' )

      return
      end
      subroutine r8mat_nint_test ( )

c*********************************************************************72
c
cc R8MAT_NINT_TEST tests R8MAT_NINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer seed
      double precision x1
      double precision x2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_NINT_TEST'
      write ( *, '(a)' ) '  R8MAT_NINT rounds an R8MAT.'

      x1 = -5.0D+00
      x2 = +5.0D+00
      seed = 123456789
      call r8mat_uniform_ab ( m, n, x1, x2, seed, a )
      call r8mat_print ( m, n, a, '  Matrix A:' )
      call r8mat_nint ( m, n, a )
      call r8mat_print ( m, n, a, '  Rounded matrix A:' )

      return
      end
      subroutine r8mat_nonzeros_test ( )

c*********************************************************************72
c
cc R8MAT_NONZEROS_TEST tests R8MAT_NONZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer c1
      integer c2
      integer i
      integer j
      integer r8mat_nonzeros

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_NONZEROS_TEST'
      write ( *, '(a)' ) '  R8MAT_NONZEROS counts nonzeros in an R8MAT.'

      c1 = 0
      do i = 1, m
        do j = 1, n
          if ( mod ( i, 2 ) == 0 .and. mod (  j, 2 ) == 0 ) then
            a(i,j) = 1.0D+00
            c1 = c1 + 1
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( m, n, a, '  Matrix A:' )

      c2 = r8mat_nonzeros ( m, n, a )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Expected nonzeros = ', c1
      write ( *, '(a,i4)' ) '  Computed nonzeros = ', c2

      return
      end
      subroutine r8mat_norm_fro_test ( )

c*********************************************************************72
c
cc R8MAT_NORM_FRO_TEST tests R8MAT_NORM_FRO.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision r8mat_norm_fro
      double precision t1
      double precision t2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NORM_FRO_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NORM_FRO computes the Frobenius norm of an R8MAT;'

      t1 = 0.0D+00
      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
          t1 = t1 + dble ( k * k )
        end do
      end do

      t1 = sqrt ( t1 )

      call r8mat_print ( m, n, a, '  A:' )

      t2 = r8mat_norm_fro ( m, n, a )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Expected norm = ', t1
      write ( *, '(a,g14.6)' ) '  Computed norm = ', t2

      return
      end
      subroutine r8mat_norm_l1_test ( )

c*********************************************************************72
c
cc R8MAT_NORM_L1_TEST tests R8MAT_NORM_L1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision r8mat_norm_l1
      integer seed
      double precision t
      double precision x1
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NORM_L1_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NORM_L1 computes the L1 norm of an R8MAT;'

      x1 = -5.0D+00
      x2 = +5.0D+00
      seed = 123456789
      call r8mat_uniform_ab ( m, n, x1, x2, seed, a )
      call r8mat_nint ( m, n, a )

      call r8mat_print ( m, n, a, '  A:' )

      t = r8mat_norm_l1 ( m, n, a )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Computed norm = ', t

      return
      end
      subroutine r8mat_nullspace_test ( )

c*********************************************************************72
c
cc R8MAT_NULLSPACE_TEST tests R8MAT_NULLSPACE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 7 )

      double precision a(m,n)
      double precision ax(m,n)
      double precision nullspace(n,n)
      integer nullspace_size

      save a

      data a /
     &  1.0, -2.0, 3.0, -1.0, 
     &  3.0, -6.0, 9.0, -3.0, 
     &  0.0,  0.0, 0.0,  0.0, 
     &  2.0, -2.0, 0.0,  1.0, 
     &  6.0, -8.0, 6.0,  0.0, 
     &  3.0,  3.0, 6.0,  9.0, 
     &  1.0,  1.0, 2.0,  3.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NULLSPACE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NULLSPACE computes the nullspace of a matrix.'

      call r8mat_print ( m, n, a, '  Input A:' )

      call r8mat_nullspace_size ( m, n, a, nullspace_size )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Nullspace size is ', nullspace_size
    
      call r8mat_nullspace ( m, n, a, nullspace_size, nullspace )

      call r8mat_print ( n, nullspace_size, nullspace, 
     &  '  Nullspace vectors:' )

      call r8mat_mm ( m, n, nullspace_size, a, nullspace, ax ) 

      call r8mat_print ( m, nullspace_size, ax, 
     &  '  Product A * Nullspace vectors:' )

      return
      end
      subroutine r8mat_nullspace_size_test ( )

c*********************************************************************72
c
cc R8MAT_NULLSPACE_SIZE_TEST tests R8MAT_NULLSPACE_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 7 )

      double precision a(m,n)

      integer nullspace_size

      save a

      data a /
     &  1.0, -2.0, 3.0, -1.0, 
     &  3.0, -6.0, 9.0, -3.0, 
     &  0.0,  0.0, 0.0,  0.0, 
     &  2.0, -2.0, 0.0,  1.0, 
     &  6.0, -8.0, 6.0,  0.0, 
     &  3.0,  3.0, 6.0,  9.0, 
     &  1.0,  1.0, 2.0,  3.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NULLSPACE_SIZE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NULLSPACE_SIZE sizes the nullspace of a matrix.'

      call r8mat_print ( m, n, a, '  Input A:' )

      call r8mat_nullspace_size ( m, n, a, nullspace_size )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Nullspace size is ', nullspace_size
    
      return
      end
      subroutine r8mat_orth_uniform_test ( )

c*********************************************************************72
c
cc R8MAT_ORTH_UNIFORM_TEST tests R8MAT_ORTH_UNIFORM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision ata(n,n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_ORTH_UNIFORM_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_ORTH_UNIFORM computes a random orthogonal matrix.'

      seed = 123456789

      call r8mat_orth_uniform ( n, seed, a )

      call r8mat_print ( n, n, a, '  Random orthogonal matrix A' )

      call r8mat_mtm ( n, n, n, a, a, ata )

      call r8mat_print ( n, n, ata, '  A''*A should be identity:' )

      return
      end
      subroutine r8mat_plot_test ( )

c*********************************************************************72
c
cc R8MAT_PLOT_TEST tests R8MAT_PLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 10 )
      integer n
      parameter ( n = 100 )

      double precision a(m,n)
      integer i
      integer im1
      integer ip1
      integer j

      do j = 1, n
        do i = 1, m
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, m

        a(i,i) = -2.0D+00

        if ( i + 1 .le. n ) then
          ip1 = i + 1
        else
          ip1 = 1
        end if

        a(i,ip1) = 1.0D+00

        if ( 1 .le. i - 1 ) then
          im1 = i - 1
        else
          im1 = n
        end if

        a(i,im1) = 1.0D+00

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_PLOT_TEST'
      write ( *, '(a)' ) '  R8MAT_PLOT prints a symbolic picture of a'
      write ( *, '(a)' ) '  matrix.  Typically, '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    "-" for negative, '
      write ( *, '(a)' ) '    " " for zero, and'
      write ( *, '(a)' ) '    "+" for positive entries'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  or'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    "X" for nonzero and, '
      write ( *, '(a)' ) '    " " for zero.'
      write ( *, '(a)' ) ' '

      call r8mat_plot ( m, n, a, '  A plot of the matrix:' )

      return
      end
      subroutine r8mat_power_method_test ( )

c*********************************************************************72
c
cc R8MAT_POWER_METHOD_TEST tests R8MAT_POWER_METHOD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision av(n)
      integer i
      integer j
      double precision r
      double precision v(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_POWER_METHOD_TEST'
      write ( *, '(a)' ) '  R8MAT_POWER_METHOD applies the power method'
      write ( *, '(a)' ) '  to a matrix.'

      do i = 1, n
        do j = 1, n
          if ( j .eq. i - 1 .or. j .eq. i + 1 ) then
            a(i,j) = -1.0D+00
          else if ( j == i ) then
            a(i,j) = 2.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      do j = 1, n
        v(j) = 0.0D+00
      end do

      call r8mat_power_method ( n, a, r, v )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Estimated eigenvalue = ', r

      call r8vec_print ( n, v, '  Estimated eigenvector V:' )

      call r8mat_mv ( n, n, a, v, av )
 
      call r8vec_print ( n, av, '  Value of A*V:' )

      return
      end
      subroutine r8mat_print_test ( )

c*********************************************************************72
c
cc R8MAT_PRINT_TEST tests R8MAT_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_PRINT_TEST'
      write ( *, '(a)' ) '  R8MAT_PRINT prints an R8MAT.'

      do j = 1, n
        do i = 1, m
          a(i,j) = dble ( i * 10 + j )
        end do
      end do

      call r8mat_print ( m, n, a, '  The R8MAT:' )

      return
      end
      subroutine r8mat_print_some_test ( )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME_TEST tests R8MAT_PRINT_SOME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_PRINT_SOME_TEST'
      write ( *, '(a)' ) '  R8MAT_PRINT_SOME prints some of an R8MAT.'

      do j = 1, n
        do i = 1, m
          a(i,j) = dble ( i * 10 + j )
        end do
      end do

      call r8mat_print_some ( m, n, a, 2, 1, 4, 2, 
     &  '  The R8MAT, rows 2:4, cols 1:2' )

      return
      end
      subroutine r8mat_ref_test ( )

c*********************************************************************72
c
cc R8MAT_REF_TEST tests R8MAT_REF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 7 )

      double precision a(m,n)

      save a

      data a /
     &  1.0, -2.0, 3.0, -1.0, 
     &  3.0, -6.0, 9.0, -3.0, 
     &  0.0,  0.0, 0.0,  0.0, 
     &  2.0, -2.0, 0.0,  1.0, 
     &  6.0, -8.0, 6.0,  0.0, 
     &  3.0,  3.0, 6.0,  9.0, 
     &  1.0,  1.0, 2.0,  3.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_REF_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_REF computes the row echelon form of a matrix.'

      call r8mat_print ( m, n, a, '  Input A:' )

      call r8mat_ref ( m, n, a )

      call r8mat_print ( m, n, a, '  REF form:' )

      return
      end
      subroutine r8mat_rref_test ( )

c*********************************************************************72
c
cc R8MAT_RREF_TEST tests R8MAT_RREF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 7 )

      double precision a(m,n)

      save a

      data a /
     &  1.0, -2.0, 3.0, -1.0, 
     &  3.0, -6.0, 9.0, -3.0, 
     &  0.0,  0.0, 0.0,  0.0, 
     &  2.0, -2.0, 0.0,  1.0, 
     &  6.0, -8.0, 6.0,  0.0, 
     &  3.0,  3.0, 6.0,  9.0, 
     &  1.0,  1.0, 2.0,  3.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_RREF_TEST'
      write ( *, '(a)' ) '  R8MAT_RREF computes the reduced '
      write ( *, '(a)' ) '  row echelon form of a matrix.'

      call r8mat_print ( m, n, a, '  Input A:' )

      call r8mat_rref ( m, n, a )

      call r8mat_print ( m, n, a, '  RREF form:' )

      return
      end
      subroutine r8mat_solve_test ( )

c*********************************************************************72
c
cc R8MAT_SOLVE_TEST tests R8MAT_SOLVE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )
      integer rhs_num
      parameter ( rhs_num = 2 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision a(n,n+rhs_num) 
      integer info

      save a

      data a /
     &     1.0D+00,  4.0D+00,  7.0D+00, 
     &     2.0D+00,  5.0D+00,  8.0D+00, 
     &     3.0D+00,  6.0D+00,  0.0D+00, 
     &    14.0D+00, 32.0D+00, 23.0D+00, 
     &     7.0D+00, 16.0D+00,  7.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_SOLVE_TEST'
      write ( *, '(a)' ) '  R8MAT_SOLVE solves linear systems.'
c
c  Print out the matrix to be inverted.
c
      call r8mat_print ( n, n+rhs_num, a, '  The linear system:' )
c
c  Solve the systems.
c
      call r8mat_solve ( n, rhs_num, a, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The input matrix was singular.'
        write ( *, '(a)' ) '  The solutions could not be computed.'
        write ( *, '(a)' ) ' '
        return
      end if

      call r8mat_print ( n, rhs_num, a(1:n,n+1:n+rhs_num), 
     &  '  The computed solutions' )

      return
      end
      subroutine r8mat_solve_2d_test ( )

c*********************************************************************72
c
cc R8MAT_SOLVE_2D_TEST tests R8MAT_SOLVE_2D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      integer n
      parameter ( n = 2 )

      double precision a(n,n)
      double precision b(n)
      double precision det
      integer i
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 5 )
      double precision x(n)
      double precision x2(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_SOLVE_2D_TEST'
      write ( *, '(a)' ) '  R8MAT_SOLVE_2D solves 2D linear systems.'

      seed = 123456789

      do test = 1, test_num

        call r8mat_uniform_01 ( n, n, seed, a )
        call r8vec_uniform_01 ( n, seed, x )
        call r8mat_mv ( n, n, a, x, b )

        call r8mat_solve_2d ( a, b, det, x2 )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Solution / Computed:'
        write ( *, '(a)' ) ' '

        do i = 1, n
          write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), x2(i)
        end do

      end do

      return
      end
      subroutine r8mat_solve_3d_test ( )

c*********************************************************************72
c
cc R8MAT_SOLVE_3D_TEST tests R8MAT_SOLVE_3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      double precision b(n)
      double precision det
      integer i
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 5 )
      double precision x(n)
      double precision x2(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_SOLVE_3D_TEST'
      write ( *, '(a)' ) '  R8MAT_SOLVE_3D solves 3D linear systems.'

      seed = 123456789

      do test = 1, test_num

        call r8mat_uniform_01 ( n, n, seed, a )
        call r8vec_uniform_01 ( n, seed, x )
        call r8mat_mv ( n, n, a, x, b )

        call r8mat_solve_3d ( a, b, det, x2 )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Solution / Computed:'
        write ( *, '(a)' ) ' '

        do i = 1, n
          write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), x2(i)
        end do

      end do

      return
      end
      subroutine r8mat_solve2_test ( )

c*********************************************************************72
c
cc R8MAT_SOLVE2_TEST tests R8MAT_SOLVE2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 4 )
      integer test_num
      parameter ( test_num = 4 )

      double precision a(n_max,n_max)
      double precision a1(2,2)
      double precision a2(3,3)
      double precision a3(4,4)
      double precision a4(3,3)
      double precision b(n_max)
      double precision b1(2)
      double precision b2(3)
      double precision b3(4)
      double precision b4(3)
      integer ierror
      integer n
      integer n_test(test_num)
      integer test
      double precision x(n_max)

      save a1
      save a2
      save a3
      save a4
      save b1
      save b2
      save b3
      save b4
      save n_test

      data a1 /
     &  1.0D+00, 3.0D+00, 
     &  2.0D+00, 4.0D+00 /
      data a2 /
     &  2.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 0.0D+00, 
     &  1.0D+00, 0.0D+00, 1.0D+00 /
      data a3 /
     &  1.0D+00, 2.0D+00, 1.0D+00, 3.0D+00, 
     &  0.0D+00, 1.0D+00, 2.0D+00, 1.0D+00, 
     &  0.0D+00, 0.0D+00, 3.0D+00, 2.0D+00, 
     &  1.0D+00, 3.0D+00, 0.0D+00, 1.0D+00 /
      data a4 /
     &  2.0D+00, 1.0D+00, 3.0D+00, 
     &  4.0D+00, 2.0D+00, 6.0D+00, 
     &  1.0D+00, 4.0D+00, 5.0D+00 /
      data b1 /
     &  5.0D+00, 11.0D+00 /
      data b2 /
     &  4.0D+00, 2.0D+00, 2.0D+00 /
      data b3 /
     &  5.0D+00, 11.0D+00, 16.0D+00, 15.0D+00 /
      data b4 /
     &  13.0D+00, 17.0D+00, 20.0D+00 /
      data n_test / 2, 3, 4, 3 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_SOLVE2_TEST'
      write ( *, '(a)' ) '  R8MAT_SOLVE2 is a linear solver.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        n = n_test ( test )
 
        if ( test .eq. 1 ) then
          call r8mat_copy ( n, n, a1, a )
          call r8vec_copy ( n, b1, b )
        else if ( test .eq. 2 ) then
           call r8mat_copy ( n, n, a2, a )
          call r8vec_copy ( n, b2, b )
        else if ( test .eq. 3 ) then
          call r8mat_copy ( n, n, a3, a )
          call r8vec_copy ( n, b3, b )
        else if ( test .eq. 4 ) then
          call r8mat_copy ( n, n, a4, a )
          call r8vec_copy ( n, b4, b )
        end if

        call r8vec_print ( n, b, '  Right hand side:' )
        call r8mat_solve2 ( n, a, b, x, ierror )

        write ( *, '(a)' ) ' '
        if ( ierror .eq. 0 ) then
          write ( *, '(a)' ) '  The system is nonsingular.'
        else if ( ierror .eq. 1 ) then
          write ( *, '(a)' ) '  The system is singular, but consistent.'
        else if ( ierror .eq. 2 ) then
          write ( *, '(a)' ) 
     &      '  The system is singular and inconsistent.'
        end if

        call r8vec_print ( n, x, '  Computed solution:' )

      end do

      return
      end
      subroutine r8mat_sub_test ( )

c*********************************************************************72
c
cc R8MAT_SUB_TEST tests R8MAT_SUB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision b(m,n)
      double precision c(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_SUB_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_SUB computes C = A - B for R8MAT''s.'

      call r8mat_indicator ( m, n, a )

      call r8mat_transpose ( m, n, a, b )
 
      call r8mat_sub ( m, n, a, b, c )

      call r8mat_print ( m, n, a, '  A:' )
      call r8mat_print ( m, n, b, '  B:' )
      call r8mat_print ( m, n, c, '  C = A-B:' )

      return
      end
      subroutine r8mat_symm_jacobi_test ( )

c*********************************************************************72
c
cc R8MAT_SYMM_JACOBI_TEST tests R8MAT_SYMM_JACOBI;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      integer i
      double precision q(n,n)
      integer seed
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_SYMM_JACOBI_TEST'
      write ( *, '(a)' ) '  For a symmetric R8MAT:'
      write ( *, '(a)' ) '  R8MAT_SYMM_JACOBI diagonalizes;'
c
c  Choose the eigenvalues.
c
      call r8vec_indicator1 ( n, x )
c
c  Choose the eigenvectors.
c
      seed = 123456789
      call r8mat_orth_uniform ( n, seed, q )
c
c  Compute A = Q*X*Q.
c 
      call r8mat_symm_eigen ( n, x, q, a )

      call r8mat_print ( n, n, a, '  Matrix to diagonalize:' )

      call r8mat_symm_jacobi ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Computed Eigenvalues:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,g14.6)' ) a(i,i)
      end do

      return
      end
      subroutine r8mat_to_r8plu_test ( )

c*********************************************************************72
c
cc R8MAT_TO_R8PLU_TEST tests R8MAT_TO_R8PLU;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision b
      double precision c
      integer info
      double precision lu(n,n)
      integer pivot(n)
      integer seed

      b = 0.0D+00
      c = 1.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_TO_R8PLU_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_TO_R8PLU determines the compressed PLU factors'
      write ( *, '(a)' ) '  of a R8MAT.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_ab ( n, n, b, c, seed, a )

      call r8mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      call r8mat_to_r8plu ( n, a, pivot, lu, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a)' ) 
     &  '  R8MAT_TO_R8PLU declares the matrix is singular!'
        write ( *, '(a,i8)' ) '  The value of INFO is ', info
      end if
c
c  Display the gory details.
c
      call i4vec_print ( n, pivot, '  The pivot vector P:' )

      call r8mat_print ( n, n, lu, '  The compressed LU factors:' )
c
c  Recover the matrix from the PLU factors.
c
      call r8plu_to_r8mat ( n, pivot, lu, a2 )

      call r8mat_print ( n, n, a2, '  The recovered matrix A2:' )

      return
      end
      subroutine r8mat_trace_test ( )

c*********************************************************************72
c
cc R8MAT_TRACE_TEST tests R8MAT_TRACE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      integer i
      integer j
      double precision r8mat_trace
      double precision trace

      do i = 1, n
        do j = 1, n

          if ( i .le. j ) then
            a(i,j) = dble ( n + 1 - j )
          else if ( j == i - 1 ) then
            a(i,j) = dble ( n - j )
          else
            a(i,j) = 0.0D+00
          end if

        end do
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_TRACE_TEST'
      write ( *, '(a)' ) '  R8MAT_TRACE computes the trace of a matrix'

      call r8mat_print ( n, n, a, '  Matrix:' )

      trace = r8mat_trace ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Trace is ', trace

      return
      end
      subroutine r8mat_transpose_test ( )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_TEST tests R8MAT_TRANSPOSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision at(n,m)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_TRANSPOSE_TEST'
      write ( *, '(a)' ) '  R8MAT_TRANSPOSE transposes an R8MAT.'

      call r8mat_indicator ( m, n, a )
      call r8mat_print ( m, n, a, '  Matrix A:' )

      call r8mat_transpose ( m, n, a, at )
      call r8mat_print ( n, m, at, '  Transposed matrix At:' )

      return
      end
      subroutine r8mat_transpose_print_test ( )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_TEST tests R8MAT_TRANSPOSE_PRINT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 7 )
      integer n
      parameter ( n = 12 )

      double precision a(m,n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_TRANSPOSE_PRINT_TEST'
      write ( *, '(a)' ) '  R8MAT_TRANSPOSE_PRINT prints a R8MAT,'
      write ( *, '(a)' ) '  transposed.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix row order M =    ', m
      write ( *, '(a,i8)' ) '  Matrix column order N = ', n
c
c  Set the matrix.
c
      do i = 1, m
        do j = 1, n
          a(i,j) = real ( i * 100 + j, kind = 8 )
        end do
      end do

      call r8mat_transpose_print ( m, n, a, 
     &  '  The transposed matrix A:' )

      return
      end
      subroutine r8mat_u_inverse_test ( )

c*********************************************************************72
c
cc R8MAT_U_INVERSE_TEST tests R8MAT_U_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)

      save a

      data a /
     &  1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 
     &  2.0D+00, 3.0D+00, 0.0D+00,  0.0D+00, 
     &  4.0D+00, 5.0D+00, 6.0D+00,  0.0D+00, 
     &  7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_U_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_U_INVERSE inverts an upper triangular matrix.'
   
      call r8mat_print ( n, n, a, '  Input matrix A' )

      call r8mat_u_inverse ( n, a, b )

      call r8mat_print ( n, n, b, '  Inverse matrix B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_u1_inverse_test ( )

c*********************************************************************72
c
cc R8MAT_U1_INVERSE_TEST tests R8MAT_U1_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)

      save a

      data a /
     &  1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 
     &  2.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 
     &  0.0D+00, 0.0D+00, 1.0D+00, 0.0D+00, 0.0D+00,  0.0D+00, 
     &  5.0D+00, 0.0D+00, 3.0D+00, 1.0D+00, 0.0D+00,  0.0D+00, 
     &  0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00,  0.0D+00, 
     & 75.0D+00, 0.0D+00, 0.0D+00, 6.0D+00, 4.0D+00,  1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_U1_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_U1_INVERSE inverts a unit upper triangular matrix.'

      call r8mat_print ( n, n, a, '  Input matrix A' )

      call r8mat_u1_inverse (  n, a, b )

      call r8mat_print ( n, n, b, '  Inverse matrix B:' )

      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  Product C = A * B:' )

      return
      end
      subroutine r8mat_uniform_ab_test ( )

c*********************************************************************72
c
cc R8MAT_UNIFORM_AB_TEST tests R8MAT_UNIFORM_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision b
      double precision c
      integer seed

      b = 2.0D+00
      c = 10.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_UNIFORM_AB_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_UNIFORM_AB sets a matrix to random values.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      call r8mat_uniform_ab ( m, n, b, c, seed, a )
c
c  Print out the matrix to be inverted.
c
      call r8mat_print ( m, n, a, '  The random matrix:' )

      return
      end
      subroutine r8plu_det_test ( )

c*********************************************************************72
c
cc R8PLU_DET_TEST tests R8PLU_DET;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision det
      integer info
      double precision lu(n,n)
      integer pivot(n)
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8PLU_DET_TEST'
      write ( *, '(a)' ) 
     &  '  R8PLU_DET determines the determinant of a matrix'
      write ( *, '(a)' ) '  from its compressed PLU factors.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_01 ( n, n, seed, a )

      call r8mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      call r8mat_to_r8plu ( n, a, pivot, lu, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Fatal error!'
        write ( *, '(a)' ) 
     &    '  R8MAT_TO_R8PLU declares the matrix is singular!'
        write ( *, '(a,i8)' ) '  The value of INFO is ', info
        return
      end if
c
c  Compute the determinant.
c
      call r8plu_det ( n, pivot, lu, det )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  The determinant = ', det

      return
      end
      subroutine r8plu_inverse_test ( )

c*********************************************************************72
c
cc R8PLU_INVERSE_TEST tests R8PLU_INVERSE;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)
      integer info
      double precision lu(n,n)
      integer pivot(n)
      integer :: seed = 123456789

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8PLU_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  R8PLU_INVERSE determines the inverse of a matrix'
      write ( *, '(a)' ) '  from its compressed PLU factors.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_01 ( n, n, seed, a )

      call r8mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      call r8mat_to_r8plu ( n, a, pivot, lu, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST087 - Fatal error!'
        write ( *, '(a)' ) 
     &    '  R8MAT_TO_R8PLU declares the matrix is singular!'
        write ( *, '(a,i8)' ) '  The value of INFO is ', info
        return
      end if
c
c  Compute the inverse.
c
      call r8plu_inverse ( n, pivot, lu, b )

      call r8mat_print ( n, n, b, '  The inverse matrix B:' )
c
c  Compute the product C = A * B.
c
      call r8mat_mm ( n, n, n, a, b, c )

      call r8mat_print ( n, n, c, '  The product matrix C = A * B:' )

      return
      end
      subroutine r8plu_mul_test ( )

c*********************************************************************72
c
cc R8PLU_MUL_TEST tests R8PLU_MUL;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision b(n)
      integer i
      integer info
      double precision lu(n,n)
      integer pivot(n)
      integer seed
      double precision x(n)

      seed = 123456789
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8PLU_MUL_TEST'
      write ( *, '(a)' ) '  R8PLU_MUL computes the product A*x=b'
      write ( *, '(a)' ) '  using the compressed PLU factors of A.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_01 ( n, n, seed, a )

      call r8mat_print ( n, n, a, '  The matrix A:' )
c
c  Set the right hand side B1.
c
      do i = 1, n
        x(i) = dble ( i )
      end do

      call r8mat_mv ( n, n, a, x, b )
 
      call r8vec_print ( n, b, 
     &  '  The right hand side B (computed from A):' )
c
c  Factor the matrix.
c
      call r8mat_to_r8plu ( n, a, pivot, lu, info )
c
c  Compute the matrix-vector product.
c
      call r8plu_mul ( n, pivot, lu, x, b )

      call r8vec_print ( n, b, 
     &  '  The right hand side B (computed from PLU):' )

      return
      end
      subroutine r8plu_sol_test ( )

c*********************************************************************72
c
cc R8PLU_SOL_TEST tests R8PLU_SOL;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision b(n)
      integer i
      integer info
      double precision lu(n,n)
      integer pivot(n)
      integer seed
      double precision x(n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8PLU_SOL_TEST'
      write ( *, '(a)' ) '  R8PLU_SOL solves a linear system A*x=b'
      write ( *, '(a)' ) '  using the compressed PLU factors of A.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_01 ( n, n, seed, a )

      call r8mat_print ( n, n, a, '  The matrix A:' )
c
c  Set the right hand side.
c
      do i = 1, n
        x(i) = real ( i, kind = 8 )
      end do
      call r8mat_mv ( n, n, a, x, b )
      do i = 1, n
        x(i) = 0.0D+00
      end do

      call r8vec_print ( n, b, '  The right hand side B:' )
c
c  Factor the matrix.
c
      call r8mat_to_r8plu ( n, a, pivot, lu, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Fatal error!'
        write ( *, '(a)' ) 
     &    '  R8MAT_TO_R8PLU declares the matrix is singular!'
        write ( *, '(a,i8)' ) '  The value of INFO is ', info
        return
      end if
c
c  Solve the system.
c
      call r8plu_sol ( n, pivot, lu, b, x )

      call r8vec_print ( n, x, '  The computed solution X:' )

      return
      end
      subroutine r8plu_to_r8mat_test ( )

c*********************************************************************72
c
cc R8PLU_TO_R8MAT_TEST tests R8PLU_TO_R8MAT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision b
      double precision c
      integer info
      double precision lu(n,n)
      integer pivot(n)
      integer seed

      b = 0.0D+00
      c = 1.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8PLU_TO_R8MAT_TEST'
      write ( *, '(a)' ) 
     &  '  R8PLU_TO_R8MAT determines the original matrix from'
      write ( *, '(a)' ) '  the compressed PLU factors.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      call r8mat_uniform_ab ( n, n, b, c, seed, a )

      call r8mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      call r8mat_to_r8plu ( n, a, pivot, lu, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a)' ) 
     &  '  R8MAT_TO_R8PLU declares the matrix is singular!'
        write ( *, '(a,i8)' ) '  The value of INFO is ', info
      end if
c
c  Display the gory details.
c
      call i4vec_print ( n, pivot, '  The pivot vector P:' )

      call r8mat_print ( n, n, lu, '  The compressed LU factors:' )
c
c  Recover the matrix from the PLU factors.
c
      call r8plu_to_r8mat ( n, pivot, lu, a2 )

      call r8mat_print ( n, n, a2, '  The recovered matrix A2:' )

      return
      end
      subroutine r8poly_degree_test ( )

c*********************************************************************72
c
cc R8POLY_DEGREE_TEST tests R8POLY_DEGREE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c1(0:3)
      double precision c2(0:3)
      double precision c3(0:3)
      double precision c4(0:3)
      double precision c5(0:3)
      integer d
      integer m
      integer r8poly_degree

      save c1
      save c2
      save c3
      save c4
      save c5

      data c1
     &  / 1.0, 2.0, 3.0, 4.0 /
      data c2
     &  / 1.0, 2.0, 3.0, 0.0 /
      data c3(0:3)
     &  / 1.0, 2.0, 0.0, 4.0 /
      data c4(0:3)
     &  / 1.0, 0.0, 0.0, 0.0 /
      data c5(0:3)
     &  / 0.0, 0.0, 0.0, 0.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_DEGREE_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY_DEGREE determines the degree of an R8POLY.'

      m = 3

      call r8poly_print ( m, c1, '  The R8POLY:' )
      d = r8poly_degree ( m, c1 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c2, '  The R8POLY:' )
      d = r8poly_degree ( m, c2 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c3, '  The R8POLY:' )
      d = r8poly_degree ( m, c3 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c4, '  The R8POLY:' )
      d = r8poly_degree ( m, c4 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c5, '  The R8POLY:' )
      d = r8poly_degree ( m, c5 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      return
      end
      subroutine r8poly_deriv_test ( )

c*********************************************************************72
c
cc R8POLY_DERIV_TEST tests R8POLY_DERIV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision c(0:n)
      double precision cp(0:n)
      integer d
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_DERIV_TEST'
      write ( *, '(a)' ) '  R8POLY_DERIV computes the coefficients of'
      write ( *, '(a)' ) '  the derivative of a polynomial.'

      call r8vec_indicator1 ( n, x )

      call roots_to_r8poly ( n, x, c )

      call r8poly_print ( n, c, '  The initial polynomial' )

      do d = 0, n
        call r8poly_deriv ( n, c, d, cp )
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  The derivative of order ', d
        write ( *, '(a)' ) ' '
        call r8poly_print ( n - d, cp, ' ' )
      end do

      return
      end
      subroutine r8poly_lagrange_coef_test ( )

c*********************************************************************72
c
cc R8POLY_LAGRANGE_COEF_TEST tests R8POLY_LAGRANGE_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer npol
      parameter ( npol = 5 )

      integer ipol
      double precision pcof(0:npol-1)
      double precision xpol(npol)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_LAGRANGE_COEF_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY_LAGRANGE_COEF returns the coefficients'
      write ( *, '(a)' ) '  for a Lagrange basis polynomial.'
      write ( *, '(a)' ) '  R8POLY_PRINT prints a polynomial.'

      call r8vec_indicator1 ( npol, xpol )

      call r8vec_print ( npol, xpol, '  Abscissas:' )

      do ipol = 1, npol

        call r8poly_lagrange_coef ( npol, ipol, xpol, pcof )

        call r8poly_print ( npol - 1, pcof, 
     &    '  The Lagrange basis polynomial:' )

      end do

      return
      end
      subroutine r8poly_lagrange_0_test ( )

c*********************************************************************72
c
cc R8POLY_LAGRANGE_0_TEST tests R8POLY_LAGRANGE_0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer npol
      parameter ( npol = 5 )

      integer ival
      integer nx
      double precision wval
      double precision xhi
      double precision xlo
      double precision xpol(npol)
      double precision xval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_LAGRANGE_0_TEST'
      write ( *, '(a)' ) '  R8POLY_LAGRANGE_0 evaluates the Lagrange'
      write ( *, '(a)' ) '  factor W(X) at a point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of data points is ', npol
c
c  Set the abscissas of the polynomials.
c
      xlo = 0.0D+00
      xhi = real ( npol - 1, kind = 8 )

      call r8vec_even ( npol, xlo, xhi, xpol )

      call r8vec_print ( npol, xpol, '  Abscissas:' )
c
c  Evaluate W(X), W'(X), W''.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      X          W(X)'
      write ( *, '(a)' ) ' '

      nx = 4 * npol - 1

      do ival = 1, nx

        call r8vec_even_select ( nx, xlo, xhi, ival, xval )

        call r8poly_lagrange_0 ( npol, xpol, xval, wval )

        write ( *, '(6g12.4)' ) xval, wval

      end do

      return
      end
      subroutine r8poly_lagrange_1_test ( )

c*********************************************************************72
c
cc R8POLY_LAGRANGE_1_TEST tests R8POLY_LAGRANGE_1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer npol
      parameter ( npol = 5 )

      double precision dwdx
      integer ival
      integer nx
      double precision xhi
      double precision xlo
      double precision xpol(npol)
      double precision xval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_LAGRANGE_1_TEST'
      write ( *, '(a)' ) '  R8POLY_LAGRANGE_1 evaluates the Lagrange'
      write ( *, '(a)' ) '  factor W''(X) at a point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of data points is ', npol
c
c  Set the abscissas of the polynomials.
c
      xlo = 0.0D+00
      xhi = real ( npol - 1, kind = 8 )

      call r8vec_even ( npol, xlo, xhi, xpol )

      call r8vec_print ( npol, xpol, '  Abscissas:' )
c
c  Evaluate W(X), W'(X), W''.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      X          W''(X)'
      write ( *, '(a)' ) ' '

      nx = 4 * npol - 1

      do ival = 1, nx

        call r8vec_even_select ( nx, xlo, xhi, ival, xval )

        call r8poly_lagrange_1 ( npol, xpol, xval, dwdx )

        write ( *, '(2g12.4)' ) xval, dwdx

      end do

      return
      end
      subroutine r8poly_lagrange_2_test ( )

c*********************************************************************72
c
cc R8POLY_LAGRANGE_2_TEST tests R8POLY_LAGRANGE_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer npol
      parameter ( npol = 5 )

      double precision dw2dx2
      integer ival
      integer nx
      double precision xhi
      double precision xlo
      double precision xpol(npol)
      double precision xval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_LAGRANGE_2_TEST'
      write ( *, '(a)' ) '  R8POLY_LAGRANGE_2 evaluates the Lagrange'
      write ( *, '(a)' ) '  factor W"(X) at a point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of data points is ', npol
c
c  Set the abscissas of the polynomials.
c
      xlo = 0.0D+00
      xhi = real ( npol - 1, kind = 8 )

      call r8vec_even ( npol, xlo, xhi, xpol )

      call r8vec_print ( npol, xpol, '  Abscissas:' )
c
c  Evaluate W(X), W'(X), W''.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      X          W"(X)'
      write ( *, '(a)' ) ' '

      nx = 4 * npol - 1

      do ival = 1, nx

        call r8vec_even_select ( nx, xlo, xhi, ival, xval )

        call r8poly_lagrange_2 ( npol, xpol, xval, dw2dx2 )

        write ( *, '(2g12.4)' ) xval, dw2dx2

      end do

      return
      end
      subroutine r8poly_lagrange_factor_test ( )

c*********************************************************************72
c
cc R8POLY_LAGRANGE_FACTOR_TEST tests R8POLY_LAGRANGE_FACTOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer npol
      parameter ( npol = 5 )

      double precision dwdx
      integer i
      double precision wval
      double precision xhi
      double precision xlo
      double precision xpol(npol)
      double precision xval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_LAGRANGE_FACTOR_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY_LAGRANGE_FACTOR evaluates the Lagrange'
      write ( *, '(a)' ) '  factor W(X) at a point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) 
     &  '  For this test, we use ', npol, ' functions.'
c
c  Set the abscissas of the polynomials.
c
      xlo = 0.0D+00
      xhi = dble ( npol - 1 )

      do i = 1, npol
        xpol(i) = ( dble ( npol - ( i - 1 ) ) * xlo 
     &            + dble (        ( i - 1 ) ) * xhi ) 
     &            / dble ( npol             )
      end do

      call r8vec_print ( npol, xpol, '  Abscissas:' )
c
c  Evaluate W(X) and W'(X).
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          W(X)          W''(X)'
      write ( *, '(a)' ) ' '

      do i = 0, 2 * npol - 1

        call r8vec_even_select ( 2 * npol - 1, xhi, xlo, i, xval )

        call r8poly_lagrange_factor ( npol, xpol, xval, wval, dwdx )

        write ( *, '(2x,f10.4,2x,f10.4,2x,f10.4)' ) xval, wval, dwdx

      end do

      return
      end
      subroutine r8poly_lagrange_val_test ( )

c*********************************************************************72
c
cc R8POLY_LAGRANGE_VAL_TEST tests R8POLY_LAGRANGE_VAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer npol
      parameter ( npol = 5 )

      double precision dpdx(npol)
      integer ipol
      integer ival
      integer nx
      double precision pval(npol)
      double precision xhi
      double precision xlo
      double precision xpol(npol)
      double precision xval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_LAGRANGE_VAL_TEST'
      write ( *, '(a)' ) '  R8POLY_LAGRANGE_VAL evaluates a Lagrange'
      write ( *, '(a)' ) '  interpolating polynomial at a point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of data points = ', npol
c
c  Set the abscissas of the polynomials.
c
      xlo = 0.0D+00
      xhi = dble ( npol - 1 )
      call r8vec_even ( npol, xlo, xhi, xpol )

      call r8vec_print ( npol, xpol, '  Abscissas:' )
c
c  Evaluate the polynomials.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Here are the values of the functions at '
      write ( *, '(a)' ) '  several points:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      X          L1          L2          L3      L4' // 
     &  '          L5'
      write ( *, '(a)' ) ' '

      nx = 2 * npol - 1

      do ival = 1, nx

        call r8vec_even_select ( nx, xlo, xhi, ival, xval )

        do ipol = 1, npol
          call r8poly_lagrange_val ( npol, ipol, xpol, xval, 
     &      pval(ipol), dpdx(ipol) )
        end do

        write ( *, '(6g12.4)' ) xval, ( pval(ipol), ipol = 1, npol )

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  And the derivatives:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      X          L''1         L''2         L''3' // 
     &  '     L''4         L''5'
      write ( *, '(a)' ) ' '

      nx = 2 * npol - 1

      do ival = 1, nx

        call r8vec_even_select ( nx, xlo, xhi, ival, xval )

        do ipol = 1, npol
          call r8poly_lagrange_val ( npol, ipol, xpol, xval, 
     &      pval(ipol), dpdx(ipol) )
        end do

        write ( *, '(6g12.4)' ) xval, ( dpdx(ipol), ipol = 1, npol )

      end do

      return
      end
      subroutine r8poly_print_test ( )

c*********************************************************************72
c
cc R8POLY_PRINT_TEST tests R8POLY_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )

      double precision c(0:m)

      save c

      data c /
     &  12.0D+00, -3.4D+00, 56.0D+00, 0.0D+00, 0.78D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_PRINT_TEST'
      write ( *, '(a)' ) '  R8POLY_PRINT prints an R8POLY.'

      call r8poly_print ( m, c, '  The R8POLY:' )

      return
      end
      subroutine r8poly_value_horner_test ( )

c*********************************************************************72
c
cc R8POLY_VALUE_HORNER_TEST tests R8POLY_VALUE_HORNER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 16 )

      double precision c(0:m)
      integer i
      double precision p
      double precision r8poly_value_horner
      double precision x(n)
      double precision x_hi
      double precision x_lo

      save c

      data c /
     &  24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_VALUE_HORNER_TEST'
      write ( *, '(a)' ) '  R8POLY_VALUE_HORNER evaluates a polynomial'
      write ( *, '(a)' ) '  at one point, using Horner''s method.'

      call r8poly_print ( m, c, '  The polynomial coefficients:' )

      x_lo = 0.0D+00
      x_hi = 5.0D+00
      call r8vec_linspace ( n, x_lo, x_hi, x )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   I    X    P(X)'
      write ( *, '(a)' ) ''

      do i = 1, n
        p = r8poly_value_horner ( m, c, x(i) )
        write ( *, '(2x,i2,2x,f8.4,2x,g14.6)' ) i, x(i), p
      end do

      return
      end
      subroutine r8poly_values_horner_test ( )

c*********************************************************************72
c
cc R8POLY_VALUE_HORNERS_TEST tests R8POLY_VALUE_HORNERS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 16 )

      double precision c(0:m)
      double precision p(n)
      double precision x(n)
      double precision x_hi
      double precision x_lo

      save c

      data c /
     &  24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_VALUES_HORNER_TEST'
      write ( *, '(a)' ) '  R8POLY_VALUES_HORNER evaluates a polynomial'
      write ( *, '(a)' ) '  at a point, using Horner''s method.'

      call r8poly_print ( m, c, '  The polynomial:' )

      x_lo = 0.0D+00
      x_hi = 5.0D+00
      call r8vec_linspace ( n, x_lo, x_hi, x )

      call r8poly_values_horner ( m, c, n, x, p )

      call r8vec2_print ( n, x, p, '  X, P(X)' )

      return
      end
      subroutine r8poly2_ex_test ( )

c*********************************************************************72
c
cc R8POLY2_EX_TEST tests R8POLY2_EX2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer ierror
      double precision x1
      double precision x2
      double precision x3
      double precision xmin
      double precision y1
      double precision y2
      double precision y3
      double precision ymin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY2_EX_TEST'
      write ( *, '(a)' ) '  R8POLY2_EX finds the extreme value'
      write ( *, '(a)' ) '  of a parabola determined by three points.'
      a =  2.0D+00
      b = -4.0D+00
      c = 10.0D+00

      x1 = 1.0D+00
      y1 = a * x1 ** 2 + b * x1 + c
      x2 = 2.0D+00
      y2 = a * x2 ** 2 + b * x2 + c
      x3 = 3.0D+00
      y3 = a * x3 ** 2 + b * x3 + c
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parabolic coefficients A, B, C ='
      write ( *, '(2x,3g14.6)' ) a, b, c
      write ( *, '(a)' ) ' '

      call r8r8_print ( x1, y1, '  Point 1' )
      call r8r8_print ( x2, y2, '  Point 2' )
      call r8r8_print ( x3, y3, '  Point 3' )

      a = 0.0D+00
      b = 0.0D+00
      c = 0.0D+00

      call r8poly2_ex ( x1, y1, x2, y2, x3, y3, xmin, ymin, ierror )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  R8POLY2_EX returns XMIN, YMIN = ', xmin, ymin

      return
      end
      subroutine r8poly2_ex2_test ( )

c*********************************************************************72
c
cc R8POLY2_EX2_TEST tests R8POLY2_EX2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer ierror
      double precision x1
      double precision x2
      double precision x3
      double precision xmin
      double precision y1
      double precision y2
      double precision y3
      double precision ymin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY2_EX2_TEST'
      write ( *, '(a)' ) '  R8POLY2_EX2 finds the extreme value'
      write ( *, '(a)' ) '  of a parabola determined by three points.'
      a =  2.0D+00
      b = -4.0D+00
      c = 10.0D+00

      x1 = 1.0D+00
      y1 = a * x1 ** 2 + b * x1 + c
      x2 = 2.0D+00
      y2 = a * x2 ** 2 + b * x2 + c
      x3 = 3.0D+00
      y3 = a * x3 ** 2 + b * x3 + c
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parabolic coefficients A, B, C ='
      write ( *, '(2x,3g14.6)' ) a, b, c
      write ( *, '(a)' ) ' '

      call r8r8_print ( x1, y1, '  Point 1' )
      call r8r8_print ( x2, y2, '  Point 2' )
      call r8r8_print ( x3, y3, '  Point 3' )

      a = 0.0D+00
      b = 0.0D+00
      c = 0.0D+00

      call r8poly2_ex2 ( x1, y1, x2, y2, x3, y3, xmin, ymin, a, b, c, 
     &  ierror )

      write ( *, '(a)' ) ' '
      write ( *, '(a,3g14.6)' ) 
     &  '  R8POLY2_EX2 returns XMIN, YMIN = ', xmin, ymin
      write ( *, '(a,3g14.6)' ) '  and A, B, C = ', a, b, c

      return
      end
      subroutine r8poly2_root_test ( )

c*********************************************************************72
c
cc R8POLY2_ROOT_TEST tests R8POLY2_ROOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 3 )

      double precision a
      double precision a_test(test_num)
      double precision b
      double precision b_test(test_num)
      double precision c
      double precision c_test(test_num)
      double complex r1
      double complex r2
      integer test

      save a_test
      save b_test
      save c_test

      data a_test /
     &  2.0D+00, 1.0D+00, 1.0D+00 /
      data b_test /
     &  -2.0D+00, -20.0D+00, -2.0D+00 /
      data c_test /
     & -24.0D+00, 100.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY2_ROOT_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY2_ROOT finds quadratic equation roots.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         A         B         C     R1         R2'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        a = a_test(test)
        b = b_test(test)
        c = c_test(test)

        call r8poly2_root ( a, b, c, r1, r2 )

        write ( *, '(2x,3f8.1,4g14.6)' ) a, b, c, r1, r2

      end do

      return
      end
      subroutine r8poly2_val_test ( )

c*********************************************************************72
c
cc R8POLY2_VAL_TEST tests R8POLY2_VAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision x
      double precision x1
      double precision x2
      double precision x3
      double precision y
      double precision y1
      double precision y2
      double precision y3
      double precision yp
      double precision ypp

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY2_VAL_TEST'
      write ( *, '(a)' ) '  R8POLY2_VAL evaluates a parabola given'
      write ( *, '(a)' ) '  3 data points.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Our parabola will be 2*x*x + 3 * x + 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Case 1: 3 distinct data points:'
      write ( *, '(a)' ) ' '

      x1 = -1.0D+00
      x2 = 1.0D+00
      x3 = 3.0D+00

      call r8poly2_val_f ( x1, y1, yp, ypp )
      call r8poly2_val_f ( x2, y2, yp, ypp )
      call r8poly2_val_f ( x3, y3, yp, ypp )

      write ( *, '(2x,2g14.6)' ) x1, y1
      write ( *, '(2x,2g14.6)' ) x2, y2
      write ( *, '(2x,2g14.6)' ) x3, y3

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sampled data:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X, Y, Y'', Y"'
      write ( *, '(a)' ) ' '
      do i = 0, 3
        x = dble ( i )
        call r8poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )
        write ( *, '(2x,4g14.6)' ) x, y, yp, ypp
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Case 2: X1=X2, X3 distinct:'
      write ( *, '(a)' ) ' '

      x1 = - 1.0D+00
      x2 = - 1.0D+00
      x3 = 3.0D+00

      call r8poly2_val_f ( x1, y1, y2, ypp )
      call r8poly2_val_f ( x3, y3, yp, ypp )
      write ( *, '(2x,2g14.6)' ) x1, y1
      write ( *, '(2x,2g14.6)' ) x2, y2
      write ( *, '(2x,2g14.6)' ) x3, y3

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sampled data:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X, Y, Y'', Y"'
      write ( *, '(a)' ) ' '
      do i = 0, 3
        x = dble ( i )
        call r8poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )
        write ( *, '(2x,4g14.6)' ) x, y, yp, ypp
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Case 3: X1=X2=X3:'
      write ( *, '(a)' ) ' '

      x1 = - 1.0D+00
      x2 = - 1.0D+00
      x3 = - 1.0D+00

      call r8poly2_val_f ( x1, y1, y2, y3 )

      write ( *, '(2x,2g14.6)' ) x1, y1
      write ( *, '(2x,2g14.6)' ) x2, y2
      write ( *, '(2x,2g14.6)' ) x3, y3

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sampled data:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X, Y, Y'', Y"'
      write ( *, '(a)' ) ' '
      do i = 0, 3
        x = dble ( i )
        call r8poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )
        write ( *, '(2x,4g14.6)' ) x, y, yp, ypp
      end do

      return
      end
      subroutine r8poly2_val_f ( x, y, yp, ypp )

c*********************************************************************72
c
cc R8POLY2_VAL_F evaluates a parabola for us.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision x
      double precision y
      double precision yp
      double precision ypp

      y = 2.0D+00 * x ** 2 + 3.0D+00 * x + 1.0D+00
      yp = 4.0D+00 * x + 3.0D+00
      ypp = 4.0D+00

      return
      end
      subroutine r8poly2_val2_test ( )

c*********************************************************************72
c
cc R8POLY2_VAL2_TEST tests R8POLY2_VAL2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndata
      parameter ( ndata = 5 )
      integer dim_num
      parameter ( dim_num = 2 )

      integer i
      integer left
      double precision xdata(ndata)
      double precision xval
      double precision ydata(dim_num,ndata)
      double precision yval(dim_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY2_VAL2_TEST'
      write ( *, '(a)' ) '  R8POLY2_VAL2 evaluates parabolas through'
      write ( *, '(a)' ) '  3 points in a table'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Our data tables will actually be parabolas:'
      write ( *, '(a)' ) '    A: 2*x^2 + 3 * x + 1.'
      write ( *, '(a)' ) '    B: 4*x^2 - 2 * x + 5.'
      write ( *, '(a)' ) ' '

      do i = 1, ndata
        xval = 2.0D+00 * dble ( i )
        xdata(i) = xval
        ydata(1,i) = 2.0D+00 * xval ** 2 + 3.0D+00 * xval + 1.0D+00
        ydata(2,i) = 4.0D+00 * xval ** 2 - 2.0D+00 * xval + 5.0D+00
        write ( *, '(2x,i8,3g14.6)' ) 
     &    i, xdata(i), ydata(1,i), ydata(2,i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Interpolated data:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  LEFT, X, Y1, Y2'
      write ( *, '(a)' ) ' '

      do i = 0, 4
        xval = dble ( 2 * i + 1 )
        left = max ( min ( i + 1, ndata - 2 ), 1 )
        call r8poly2_val2 ( dim_num, ndata, xdata, ydata, left, 
     &    xval, yval )
        write ( *, '(2x,i8,3g14.6)' ) left, xval, yval(1), yval(2)
      end do

      return
      end
      subroutine r8poly3_root_test ( )

c*********************************************************************72
c
cc R8POLY3_ROOT_TEST tests R8POLY3_ROOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 4 )

      double precision a
      double precision a_test(test_num)
      double precision b
      double precision b_test(test_num)
      double precision c
      double precision c_test(test_num)
      double precision d
      double precision d_test(test_num)
      double complex r1
      double complex r2
      double complex r3
      integer test

      save a_test
      save b_test
      save c_test
      save d_test

      data a_test /
     &  1.0D+00, 9.0D+00, 1.0D+00, 1.0D+00 /
      data b_test /
     &  -6.0D+00, -36.0D+00, -5.0D+00, -8.0D+00  /
      data c_test /
     &  11.0D+00, 54.0D+00, 8.0D+00, 25.0D+00  /
      data d_test /
     &  -6.0D+00, -27.0D+00, -4.0D+00, -26.0D+00  /
c
c  1: Three distinct real roots, 1, 2, 3.
c  2: One repeated real root, 1.5, 1.5, 1.5.
c  3: Two real roots, one repeated, 1, 2, 2.
c  4: One real root, a complex conjugate pair, 2, 3+2I, 3-2I.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY3_ROOT_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY3_ROOT finds roots of cubic equations.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        a = a_test(test)
        b = b_test(test)
        c = c_test(test)
        d = d_test(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Polynomial coefficients A, B, C, D:'
        write ( *, '(a)' ) ' '
        write ( *, '(2x,4g14.6)' ) a, b, c, d

        call r8poly3_root ( a, b, c, d, r1, r2, r3 )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Roots:'
        write ( *, '(a)' ) ' '
        write ( *, '(2x,2g14.6)' ) r1
        write ( *, '(2x,2g14.6)' ) r2
        write ( *, '(2x,2g14.6)' ) r3

      end do

      return
      end
      subroutine r8poly4_root_test ( )

c*********************************************************************72
c
cc R8POLY4_ROOT_TEST tests R8POLY4_ROOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 7 )

      double precision a
      double precision a_test(test_num)
      double precision b
      double precision b_test(test_num)
      double precision c
      double precision c_test(test_num)
      double precision d
      double precision d_test(test_num)
      double precision e
      double precision e_test(test_num)
      double complex r1
      double complex r2
      double complex r3
      double complex r4
      integer test

      save a_test
      save b_test
      save c_test
      save d_test
      save e_test

      data a_test /
     &  1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00 /
      data b_test /
     &  -10.0D+00, -5.0D+00, -22.0D+00, -16.0D+00, -20.0D+00, 
     &  2.0D+00, 0.0D+00 /
      data c_test /
     &  35.0D+00, 1.0D+00, 141.0D+00, 72.0D+00, 150.0D+00, 
     & 1.0D+00, 13.0D+00 /
      data d_test /
     &  -50.0D+00, 21.0D+00, -220.0D+00, -128.0D+00, -500.0D+00, 
     &  8.0D+00, 0.0D+00 /
      data e_test /
     &  24.0D+00, -18.0D+00, +100.0D+00, 80.0D+00, 625.0D+00, 
     &  -12.0D+00, 36.0D+00 /
c
c  1: Four distinct real roots, 1, 2, 3, 4.
c  2: Three distinct real roots, 1, -2, 3, 3
c  3: Two distinct real roots, 1, 1, 10, 10.
c  4: Two distinct real roots, 2, 2, 2, 10
c  5: One real root, 5, 5, 5, 5
c  6: Two distinct real roots, one complex conjugate pair.
c  7: Two distinct complex conjugate pairs.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY4_ROOT_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY4_ROOT finds roots of quartic equations.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        a = a_test(test)
        b = b_test(test)
        c = c_test(test)
        d = d_test(test)
        e = e_test(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  A =', a
        write ( *, '(a,g14.6)' ) '  B =', b
        write ( *, '(a,g14.6)' ) '  C =', c
        write ( *, '(a,g14.6)' ) '  D =', d
        write ( *, '(a,g14.6)' ) '  E =', e

        call r8poly4_root ( a, b, c, d, e, r1, r2, r3, r4 )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Roots:'
        write ( *, '(a)' ) ' '
        write ( *, '(2x,2g14.6)' ) r1
        write ( *, '(2x,2g14.6)' ) r2
        write ( *, '(2x,2g14.6)' ) r3
        write ( *, '(2x,2g14.6)' ) r4

      end do

      return
      end
      subroutine r8row_max_test ( )

c*********************************************************************72
c
cc R8ROW_MAX_TEST tests R8ROW_MAX;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision amax(m)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_MAX_TEST'
      write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
      write ( *, '(a)' ) '  R8ROW_MAX computes maximums;'
 
      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The original matrix:' )

      call r8row_max ( m, n, a, amax )

      call r8vec_print ( m, amax, '  Row maximums:' )

      return
      end
      subroutine r8row_mean_test ( )

c*********************************************************************72
c
cc R8ROW_MEAN_TEST tests R8ROW_MEAN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision mean(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_MEAN_TEST'
      write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
      write ( *, '(a)' ) '  R8ROW_MEAN computes means;'
 
      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The original matrix:' )

      call r8row_mean ( m, n, a, mean )

      call r8vec_print ( m, mean, '  Row means:' )

      return
      end
      subroutine r8row_min_test ( )

c*********************************************************************72
c
cc R8ROW_MIN_TEST tests R8ROW_MIN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      double precision amin(m)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_MIN_TEST'
      write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
      write ( *, '(a)' ) '  R8ROW_MIN computes minimums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The original matrix:' )

      call r8row_min ( m, n, a, amin )

      call r8vec_print ( m, amin, '  Row minimums:' )

      return
      end
      subroutine r8row_part_quick_a_test ( )

c*********************************************************************72
c
cc R8ROW_PART_QUICK_A_TEST tests R8ROW_PART_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 8 )
      integer n
      parameter ( n = 2 )

      double precision a(m,n)
      integer l
      integer r

      save a

      data a /
     &   2.0D+00, 8.0D+00, 6.0D+00, 0.0D+00, 10.0D+00, 
     &  10.0D+00, 0.0D+00, 5.0D+00, 
     &   4.0D+00, 8.0D+00, 2.0D+00, 2.0D+00,  6.0D+00, 
     &   0.0D+00, 6.0D+00, 8.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_PART_QUICK_A_TEST'
      write ( *, '(a)' ) '  For an R8ROW;'
      write ( *, '(a)' ) '  R8ROW_PART_QUICK_A partitions the matrix.'

      call r8mat_print ( m, n, a, '  The matrix:' )

      l = 2
      r = 4
      call r8row_part_quick_a ( m, n, a, l, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  L = ', l
      write ( *, '(a,i4)' ) '  R = ', r

      call r8mat_print ( m, n, a, '  The partitioned matrix:' )

      return
      end
      subroutine r8row_sort_heap_a_test ( )

c*********************************************************************72
c
cc R8ROW_SORT_HEAP_A_TEST tests R8ROW_SORT_HEAP_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)

      save a

      data a /
     &   2.0D+00,  4.0D+00, 1.0D+00,  3.0D+00, 
     &   6.0D+00,  8.0D+00, 5.0D+00,  7.0D+00, 
     &  10.0D+00, 12.0D+00, 9.0D+00, 11.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_SORT_HEAP_A_TEST'
      write ( *, '(a)' ) '  For an R8ROW;'
      write ( *, '(a)' ) 
     &  '  R8ROW_SORT_HEAP_A does an ascending heap sort'

      call r8mat_print ( m, n, a, '  The unsorted matrix:' )

      call r8row_sort_heap_a ( m, n, a )

      call r8mat_print ( m, n, a, '  The sorted matrix:' )

      return
      end
      subroutine r8row_sort_heap_index_a_test ( )

c*********************************************************************72
c
cc R8ROW_SORT_HEAP_INDEX_A_TEST tests R8ROW_SORT_HEAP_INDEX_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 15 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      integer i
      integer i2
      integer indx(m)

      save a

      data a /
     &  2.0D+00,  4.0D+00,  1.0D+00,  3.0D+00,  2.0D+00, 
     &  3.0D+00,  0.0D+00,  0.0D+00,  2.0D+00,  3.0D+00, 
     &  2.0D+00,  2.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, 
     &  6.0D+00,  8.0D+00,  5.0D+00,  7.0D+00,  6.0D+00, 
     &  4.0D+00,  0.0D+00,  6.0D+00,  6.0D+00,  7.0D+00, 
     &  0.0D+00,  6.0D+00,  5.0D+00,  5.0D+00,  5.1D+00, 
     & 10.0D+00, 12.0D+00,  9.0D+00, 11.0D+00,  0.0D+00, 
     & 18.0D+00,  0.0D+00, 10.0D+00, 10.0D+00, 11.0D+00, 
     & 10.0D+00, 10.0D+00,  9.0D+00,  9.1D+00,  9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_SORT_HEAP_INDEX_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8ROW_SORT_HEAP_INDEX_A computes an index vector which'
      write ( *, '(a)' ) '  ascending sorts an R8ROW.'

      call r8mat_transpose_print ( m, n, a, '  The unsorted R8ROW:' )

      call r8row_sort_heap_index_a ( m, n, a, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The implicitly sorted R8ROW'
      write ( *, '(a)' ) ' '

      do i = 1, m
        i2 = indx(i)
        write ( *, '(2x,i4,a,2x,f10.1,2x,f10.1,2x,f10.1)' ) 
     &    i2, ':', a(i2,1:n)
      end do

      return
      end
      subroutine r8row_sort_quick_a_test ( )

c*********************************************************************72
c
cc R8ROW_SORT_QUICK_A_TEST tests R8ROW_SORT_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 10 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision b
      double precision c
      integer seed

      b = 0.0D+00
      c = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_SORT_QUICK_A_TEST'
      write ( *, '(a)' ) '  For an R8ROW;'
      write ( *, '(a)' ) '  R8ROW_SORT_QUICK_A does a quicksort.'

      seed = 123456789

      call r8mat_uniform_ab ( m, n, b, c, seed, a )

      call r8mat_print ( m, n, a, '  The unsorted matrix:' )

      call r8row_sort_quick_a ( m, n, a )

      call r8mat_print ( m, n, a, '  The sorted matrix:' )

      return
      end
      subroutine r8row_sum_test ( )

c*********************************************************************72
c
cc R8ROW_SUM_TEST tests R8ROW_SUM;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision rowsum(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_SUM_TEST'
      write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
      write ( *, '(a)' ) '  R8ROW_SUM computes sums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The original matrix:' )

      call r8row_sum ( m, n, a, rowsum )

      call r8vec_print ( m, rowsum, '  The row sums:' )

      return
      end
      subroutine r8row_swap_test ( )

c*********************************************************************72
c
cc R8ROW_SWAP_TEST tests R8ROW_SWAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      integer row1
      integer row2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_SWAP_TEST'
      write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
      write ( *, '(a)' ) '  R8ROW_SWAP swaps two rows;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The original matrix:' )

      row1 = 1
      row2 = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,i3,a,i3)' ) '  Swap rows ', row1, ' and ', row2

      call r8row_swap ( m, n, a, row1, row2 )

      call r8mat_print ( m, n, a, '  The modified matrix:' )

      return
      end
      subroutine r8row_to_r8vec_test ( )

c*********************************************************************72
c
cc R8ROW_TO_R8VEC_TEST tests R8ROW_TO_R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      double precision x(m*n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_TO_R8VEC_TEST'
      write ( *, '(a)' ) 
     &  '  R8ROW_TO_R8VEC converts an array of rows into a vector.'

      do i = 1, m
        do j = 1, n
          a(i,j) = dble ( 10 * i + j )
        end do
      end do

      call r8mat_print ( m, n, a, '  The array of rows:' )

      call r8row_to_r8vec ( m, n, a, x )

      call r8vec_print ( m*n, x, '  The resulting vector of rows:' )

      return
      end
      subroutine r8row_variance_test ( )

c*********************************************************************72
c
cc R8ROW_VARIANCE_TEST tests R8ROW_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision variance(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8ROW_VARIANCE_TEST'
      write ( *, '(a)' ) '  For a R8ROW (a matrix regarded as rows):'
      write ( *, '(a)' ) '  R8ROW_VARIANCE computes variances;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
        end do
      end do

      call r8mat_print ( m, n, a, '  The original matrix:' )

      call r8row_variance ( m, n, a, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Row variances:'
      write ( *, '(a)' ) ' '
      do i = 1, m
        write ( *, '(2x,i3,3x,f10.4)' ) i, variance(i)
      end do

      return
      end
      subroutine r8slmat_print_test ( )

c*********************************************************************72
c
cc R8SLMAT_PRINT_TEST tests R8SLMAT_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 3 )

      double precision a1(21)
      double precision a2(15)
      double precision a3(6)
      integer m
      integer m_test(test_num)
      integer n
      integer n_test(test_num)
      integer test

      save a1
      save a2
      save a3
      save m_test
      save n_test

      data a1 /
     &  21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 61.0D+00, 71.0D+00, 
     &            32.0D+00, 42.0D+00, 52.0D+00, 62.0D+00, 72.0D+00, 
     &                      43.0D+00, 53.0D+00, 63.0D+00, 73.0D+00, 
     &                                54.0D+00, 64.0D+00, 74.0D+00, 
     &                                          65.0D+00, 75.0D+00, 
     &                                                    76.0D+00 /
      data a2 /
     &  21.0D+00, 31.0D+00, 41.0D+00, 51.0D+00, 61.0D+00, 71.0D+00, 
     &            32.0D+00, 42.0D+00, 52.0D+00, 62.0D+00, 72.0D+00, 
     &                      43.0D+00, 53.0D+00, 63.0D+00, 73.0D+00 /
      data a3 /
     &  21.0D+00, 31.0D+00, 41.0D+00, 
     &            32.0D+00, 42.0D+00, 
     &                      43.0D+00 /
      data m_test / 7, 7, 4 /
      data n_test / 7, 3, 7 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8SLMAT_PRINT_TEST'
      write ( *, '(a)' ) 
     &  '  R8SLMAT_PRINT prints a strictly lower triangular matrix'
      write ( *, '(a)' ) 
     &  '  stored compactly.  Only the (possibly) nonzero '
      write ( *, '(a)' ) '  elements are printed.'

      do test = 1, test_num

        m = m_test(test)
        n = n_test(test)
 
        if ( test .eq. 1 ) then
          call r8slmat_print ( m, n, a1, '  R8SLMAT' )
        else if ( test == 2 ) then
          call r8slmat_print ( m, n, a2, '  R8SLMAT' )
        else if ( test == 3 ) then
          call r8slmat_print ( m, n, a3, '  R8SLMAT' )
        end if

      end do

      return
      end
      subroutine r8vec_amax_test ( )

c*********************************************************************72
c
cc R8VEC_AMAX_TEST tests R8VEC_AMAX;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_amax
      integer seed
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_AMAX_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_AMAX: maximum magnitude entry;'

      r8_lo = - 5.0D+00
      r8_hi = + 5.0D+00
      seed = 123456789

      write ( *, * ) 'what the hell?'
  
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      value = r8vec_amax ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Maximum absolute:         ', value

      return
      end
      subroutine r8vec_amin_test ( )

c*********************************************************************72
c
cc R8VEC_AMIN_TEST tests R8VEC_AMIN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision aval
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_amin
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_AMIN_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_AMIN:      minimum magnitude entry.'

      r8_lo = - 5.0D+00
      r8_hi = + 5.0D+00
      seed = 123456789
  
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      aval = r8vec_amin ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Minimum absolute:         ', aval

      return
      end
      subroutine r8vec_bracket_test ( )

c*********************************************************************72
c
cc R8VEC_BRACKET_TEST tests R8VEC_BRACKET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )
      integer test_num
      parameter ( test_num = 6 )

      integer left
      integer right
      integer test
      double precision x(n)
      double precision xtest(test_num)
      double precision xval

      save xtest

      data xtest /
     &  -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_BRACKET_TEST'
      write ( *, '(a)' ) '  R8VEC_BRACKET finds a pair of entries in a'
      write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

      call r8vec_indicator1 ( n, x )
      x(6) = x(5)

      call r8vec_print ( n, x, '  Sorted array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    LEFT             RIGHT'
      write ( *, '(a)' ) '  X(LEFT)   XVAL   X(RIGHT)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        xval = xtest(test)

        call r8vec_bracket ( n, x, xval, left, right )

        write ( *, '(i14,14x,i14)' ) left, right

        if ( 1 .le. left .and. 1 .le. right ) then
          write ( *, '(2x,3g14.6)' ) x(left), xval, x(right)
        else if ( left .lt. 1 .and. 1 .le. right ) then
          write ( *, '(2x,14x,2g14.6)' )          xval, x(right)
        else if ( 1 .le. left .and. right .lt. 1 ) then
          write ( *, '(2x,2g14.6)' ) x(left), xval
        else if ( left .lt. 1 .and. right .lt. 1 ) then
          write ( *, '(2x,14x,g14.6)' )          xval
        end if

      end do

      return
      end
      subroutine r8vec_bracket2_test ( )

c*********************************************************************72
c
cc R8VEC_BRACKET2_TEST tests R8VEC_BRACKET2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )
      integer test_num
      parameter ( test_num = 6 )

      integer left
      integer right
      integer start
      integer test
      double precision x(n)
      double precision xtest(test_num)

      double precision xval

      save xtest

      data xtest /
     &  -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_BRACKET2_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_BRACKET2 finds a pair of entries in a'
      write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

      call r8vec_indicator1 ( n, x )
      x(6) = x(5)

      call r8vec_print ( n, x, '  Sorted array:' )

      left = 0

      do test = 1, test_num

        xval = xtest(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Search for XVAL = ', xval

        if ( 0 .lt. left ) then
          start = left
        else
          start = int ( ( n + 1 ) / 2 )
        end if

        write ( *, '(a,i8)' ) '  Start = ', start

        call r8vec_bracket2 ( n, x, xval, start, left, right )

        write ( *, '(a,i8)' ) '  Left = ', left
        write ( *, '(a,i8)' ) '  Right = ', right

        if ( 1 .le. left ) then
          write ( *, '(a,g14.6)' ) '  X(LEFT)=', x(left)
        end if

        if ( 1 .le. right ) then
          write ( *, '(a,g14.6)' ) '  X(RIGHT) = ', x(right)
        end if

      end do

      return
      end
      subroutine r8vec_bracket3_test ( )

c*********************************************************************72
c
cc R8VEC_BRACKET3_TEST tests R8VEC_BRACKET3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )
      integer test_num
      parameter ( test_num = 6 )

      integer left
      integer test
      double precision x(n)
      double precision x_test(test_num)
      double precision xval

      save x_test

      data x_test /
     & -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_BRACKET3_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_BRACKET3 finds a pair of entries in a'
      write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

      call r8vec_indicator1 ( n, x )
      x(6) = x(5)

      call r8vec_print ( n, x, '  Sorted array:' )

      left = int ( ( n + 1 ) / 2 )

      do test = 1, test_num

        xval = x_test(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Search for XVAL = ', xval

        write ( *, '(a,i8)' ) 
     &    '  Starting guess for interval is = ', left

        call r8vec_bracket3 ( n, x, xval, left )

        write ( *, '(a)' ) '  Nearest interval:'
        write ( *, '(a,i8,a,g14.6)' ) '    X[', left,' ]= ', x(left)
        write ( *, '(a,i8,a,g14.6)' ) 
     &  '    X[', left+1, ' ]= ', x(left+1)

      end do

      return
      end
      subroutine r8vec_bracket5_test ( )

c*********************************************************************72
c
cc R8VEC_BRACKET5_TEST tests R8VEC_BRACKET5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )
      integer test_num
      parameter ( test_num = 6 )

      integer left
      integer r8vec_bracket5
      integer right
      integer test
      double precision x(n)
      double precision xtest(test_num)
      double precision xval

      save xtest

      data xtest /
     &  -10.0D+00, 1.0D+00, 4.5D+00, 5.0D+00, 10.0D+00, 12.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_BRACKET5_TEST'
      write ( *, '(a)' ) '  R8VEC_BRACKET5 finds a pair of entries in a'
      write ( *, '(a)' ) '  sorted R8VEC which bracket a value.'

      call r8vec_indicator1 ( n, x )
      x(6) = x(5)

      call r8vec_print ( n, x, '  Sorted array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        LEFT                   RIGHT'
      write ( *, '(a)' ) '      X(LEFT)       XVAL     X(RIGHT)'
      write ( *, '(a)' ) ' ' 

      do test = 1, test_num

        xval = xtest(test)

        left = r8vec_bracket5 ( n, x, xval )

        if ( left .eq. -1 ) then
          write ( *, '(2x,i10)' ) left
          write ( *, '(2x,10x,2x,f10.4,2x,a)' ) xval, '(Not bracketed!)'
        else
          right = left + 1
          write ( *, '(2x,i10,2x,10x,2x,i10)' ) left, right
          write ( *, '(2x,f10.4,2x,f10.4,2x,f10.4)' ) 
     &      x(left), xval, x(right)
        end if

      end do

      return
      end
      subroutine r8vec_chebyspace_test ( )

c*********************************************************************72
c
cc R8VEC_CHEBYSPACE_TEST tests R8VEC_CHEBYSPACE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer n
      double precision r(n_max)
      double precision r1
      double precision r2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_CHEBYSPACE_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_CHEBYSPACE computes N Chebyshev points in [R1,R2].'

      r1 = -1.0D+00
      r2 = +1.0D+00
      n = 5

      call r8vec_chebyspace ( n, r1, r2, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4,a,g14.6,a,g14.6)' ) 
     &  '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

      call r8vec_print ( n, r, '  Chebyshev points:' )

      r1 =   0.0D+00
      r2 = +10.0D+00
      n = 7

      call r8vec_chebyspace ( n, r1, r2, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4,a,g14.6,a,g14.6)' ) 
     &  '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

      call r8vec_print ( n, r, '  Chebyshev points:' )

      return
      end
      subroutine r8vec_concatenate_test ( )

c*********************************************************************72
c
cc R8VEC_CONCATENATE_TEST tests R8VEC_CONCATENATE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n1
      parameter ( n1 = 5 )
      integer n2
      parameter ( n2 = 3 )
      integer n3
      parameter ( n3 = n1 + n2 )

      double precision a1(n1)
      double precision a2(n2)
      double precision a3(n3)

      save a1
      save a2

      data a1 /
     &  91.1, 31.2, 71.3, 51.4, 31.5 /
      data a2 /
     &  42.6, 22.7, 12.8 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_CONCATENATE_TEST'
      write ( *, '(a)' ) '  R8VEC_CONCATENATE concatenates two R8VECs'

      call r8vec_print ( n1, a1, '  Array 1:' )
      call r8vec_print ( n2, a2, '  Array 2:' )
      call r8vec_concatenate ( n1, a1, n2, a2, a3 )
      call r8vec_print ( n3, a3, '  Array 3 = Array 1 + Array 2:' )

      return
      end
      subroutine r8vec_convolution_test ( )

c*********************************************************************72
c
cc R8VEC_CONVOLUTION_TEST tests R8VEC_CONVOLUTION
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 May 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n

      parameter ( m = 4 )
      parameter ( n = 3 )

      double precision x(m)
      double precision y(n)
      double precision z(m+n-1)
      double precision z_correct(m+n-1)

      save x
      save y
      save z_correct

      data x / 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /
      data y / -1.0D+00, 5.0D+00, 3.0D+00 /
      data z_correct  / -1.0D+00, 3.0D+00, 10.0D+00, 17.0D+00, 
     &  29.0D+00, 12.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_CONVOLUTION_TEST'
      write ( *, '(a)' ) '  R8VEC_CONVOLUTION computes the convolution'
      write ( *, '(a)' ) '  of two vectors.'

      call r8vec_print ( m, x, '  The factor X:' )
      call r8vec_print ( n, y, '  The factor Y:' )

      call r8vec_convolution ( m, x, n, y, z )

      call r8vec_print ( m + n - 1, z, 
     &  '  The convolution z = x star y:' )

      call r8vec_print ( m + n - 1, z_correct, '  Correct answer:' )

      return
      end
      subroutine r8vec_convolution_circ_test ( )

c*********************************************************************72
c
cc R8VEC_CONVOLUTION_CIRC_TEST tests R8VEC_CONVOLUTION_CIRC
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 August 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision x(n)
      double precision y(n)
      double precision z(n)
      double precision z_correct(n)

      save x
      save y
      save z_correct

      data x / 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /
      data y / 1.0D+00, 2.0D+00, 4.0D+00, 8.0D+00 /
      data z_correct / 37.0D+00, 44.0D+00, 43.0D+00, 26.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_CONVOLUTION_CIRC_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_CONVOLUTION_CIRC computes the circular convolution'
      write ( *, '(a)' ) '  of two vectors.'

      call r8vec_print ( n, x, '  The factor X:' )
      call r8vec_print ( n, y, '  The factor Y:' )

      call r8vec_convolution_circ ( n, x, y, z )

      call r8vec_print ( n, z, 
     &  '  The circular convolution z = x CC y:' )

      call r8vec_print ( n, z_correct, '  Correct answer:' )

      return
      end
      subroutine r8vec_dif_test ( )

c*********************************************************************72
c
cc R8VEC_DIF_TEST tests R8VEC_DIF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision cof(0:n)
      double precision fdif
      double precision h
      integer i
      double precision r8vec_dif_f
      double precision x
      double precision xi

      h = 0.01D+00
      x = 1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_DIF_TEST'
      write ( *, '(a)' ) '  R8VEC_DIF estimates derivatives.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Estimate the derivative of order N = ', n
      write ( *, '(a,g14.6)' ) '  Using H = ', h
      write ( *, '(a,g14.6)' ) '  at argument X = ', x
c
c  Get the coefficients.
c
      call r8vec_dif ( n, h, cof )
    
      call r8vec_print ( n + 1, cof, '  The difference coefficients:' )

      fdif = 0.0D+00
      do i = 0, n
        xi = x + dble ( 2 * i - n ) * h
        fdif = fdif + cof(i) * r8vec_dif_f ( xi )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Estimate is FDIF = ', fdif

      return
      end
      function r8vec_dif_f ( x )

c*********************************************************************72
c
cc R8VEC_DIF_F evaluates the function used in R8VEC_DIF_TEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision r8vec_dif_f
      double precision x

      r8vec_dif_f = exp ( x )

      return
      end
      subroutine r8vec_direct_product_test ( )

c*********************************************************************72
c
cc R8VEC_DIRECT_PRODUCT_TEST tests R8VEC_DIRECT_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer factor_num
      parameter ( factor_num = 3 )
      integer factor_order_max
      parameter ( factor_order_max = 4 )
      integer point_num
      parameter ( point_num = 24 )

      integer factor_index
      integer factor_order
      double precision factor_value(factor_order_max)
      integer j
      double precision x(factor_num,point_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_DIRECT_PRODUCT_TEST'
      write ( *, '(a)' ) '  R8VEC_DIRECT_PRODUCT forms the entries of a'
      write ( *, '(a)' ) 
     &  '  direct product of a given number of R8VEC factors.'

      x(1:factor_num,1:point_num) = 0.0D+00

      do factor_index = 1, factor_num

        if ( factor_index .eq. 1 ) then
          factor_order = 4
          factor_value(1) = 1.0D+00
          factor_value(2) = 2.0D+00
          factor_value(3) = 3.0D+00
          factor_value(4) = 4.0D+00
        else if ( factor_index .eq. 2 ) then
          factor_order = 3
          factor_value(1) = 50.0D+00
          factor_value(2) = 60.0D+00
          factor_value(3) = 70.0D+00
        else if ( factor_index .eq. 3 ) then
          factor_order = 2
          factor_value(1) = 800.0D+00
          factor_value(2) = 900.0D+00
        end if

        call r8vec_direct_product ( factor_index, factor_order,
     &    factor_value, factor_num, point_num, x )

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     J         X(1)      X(2)      X(3)'
      write ( *, '(a)' ) ' '

      do j = 1, point_num
        write ( *, '(2x,i4,4x,f8.1,2x,f8.1,2x,f8.1)' ) 
     &    j, x(1:factor_num,j)
      end do

      return
      end
      subroutine r8vec_direct_product2_test ( )

c*********************************************************************72
c
cc R8VEC_DIRECT_PRODUCT2_TEST tests R8VEC_DIRECT_PRODUCT2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer factor_num
      parameter ( factor_num = 3 )
      integer factor_order_max
      parameter ( factor_order_max = 4 )
      integer point_num
      parameter ( point_num = 24 )

      integer factor_index
      integer factor_order
      double precision factor_value(factor_order_max)
      integer j
      double precision w(point_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_DIRECT_PRODUCT2_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_DIRECT_PRODUCT2 forms the entries of a'
      write ( *, '(a)' ) 
     &  '  direct product of a given number of R8VEC factors.'

      do j = 1, point_num
        w(j) = 1.0D+00
      end do

      do factor_index = 1, factor_num

        if ( factor_index .eq. 1 ) then
          factor_order = 4
          factor_value(1) = 2.0D+00
          factor_value(2) = 3.0D+00
          factor_value(3) = 5.0D+00
          factor_value(4) = 7.0D+00
        else if ( factor_index .eq. 2 ) then
          factor_order = 3
          factor_value(1) = 11.0D+00
          factor_value(2) = 13.0D+00
          factor_value(3) = 17.0D+00
        else if ( factor_index .eq. 3 ) then
          factor_order = 2
          factor_value(1) = 19.0D+00
          factor_value(2) = 21.0D+00
        end if

        call r8vec_direct_product2 ( factor_index, factor_order, 
     &    factor_value, factor_num, point_num, w )

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     J         W(J)'
      write ( *, '(a)' ) ' '

      do j = 1, point_num
        write ( *, '(2x,i4,4x,f8.1)' ) j, w(j)
      end do

      return
      end
      subroutine r8vec_even_test ( )

c*********************************************************************72
c
cc R8VEC_EVEN_TEST tests R8VEC_EVEN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision x(n)
      double precision xhi
      double precision xlo

      xlo = 0.0D+00
      xhi = 99.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN_TEST'
      write ( *, '(a)' ) '  R8VEC_EVEN computes N evenly spaced values'
      write ( *, '(a)' ) '  between XLO and XHI.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  XLO = ', xlo
      write ( *, '(a,g14.6)' ) '  XHI = ', xhi
      write ( *, '(a,i8)' ) '  while N = ', n

      call r8vec_even ( n, xlo, xhi, x )

      call r8vec_print ( n, x, '  Resulting array:' )

      return
      end
      subroutine r8vec_even2_test ( )

c*********************************************************************72
c
cc R8VEC_EVEN2_TEST tests R8VEC_EVEN2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nold
      parameter ( nold = 5 )
      integer maxval
      parameter ( maxval = 20 )

      integer i
      integer istar
      integer jstar
      integer nfill(nold-1)
      integer nval
      double precision xold(nold)
      double precision xval(maxval)

      save nfill
      save xold

      data nfill / 4, 3, 5, 0 /
      data xold /
     &  0.0D+00, 1.0D+00, 5.0D+00, 2.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN2_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_EVEN2 interpolates a specified number of '
      write ( *, '(a)' ) '  points pairs of values in a vector.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Input data:'
      write ( *, '(a)' ) ' '
      do i = 1, nold
        write ( *, '(2x,g14.6)' ) xold(i)
        if ( i .lt. nold ) then
          write ( *, '(2x,a,i10,a)' ) '(', nfill(i), ')'
        end if
      end do

      call r8vec_even2 ( maxval, nfill, nold, xold, nval, xval )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Resulting vector:'
      write ( *, '(a)' ) ' '

      istar = 1
      jstar = 1
      do i = 1, nval

        if ( i .eq. istar ) then

          write ( *, '(2x,a1,g14.6)' ) '*', xval(i)

          if ( jstar .lt. nold ) then
            istar = istar + nfill(jstar) + 1
            jstar = jstar + 1
          end if

        else

          write ( *, '(2x,g14.6)' ) xval(i)

        end if

      end do

      return
      end
      subroutine r8vec_even3_test ( )

c*********************************************************************72
c
cc R8VEC_EVEN3_TEST tests R8VEC_EVEN3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nold
      parameter ( nold = 4 )
      integer nval
      parameter ( nval = 12 )

      double precision xold(nold)
      double precision xval(nval)

      save xold

      data xold /
     &  0.0D+00, 5.1D+00, 7.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN3_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_EVEN3 tries to evenly interpolate new data'
      write ( *, '(a)' ) '  between old values.'

      call r8vec_print ( nold, xold, '  Original vector:' )

      call r8vec_even3 ( nold, nval, xold, xval )

      call r8vec_print ( nval, xval, '  New vector:' )

      return
      end
      subroutine r8vec_expand_linear_test ( )

c*********************************************************************72
c
cc R8VEC_EXPAND_LINEAR_TEST tests R8VEC_EXPAND_LINEAR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )
      integer fat
      parameter ( fat = 3 )
      integer nfat
      parameter ( nfat = ( n - 1 ) * ( fat + 1 ) + 1 )

      double precision x(n)
      double precision xfat(nfat)

      save x

      data x /
     &  16.0D+00, 4.0D+00, 0.0D+00, 4.0D+00, 16.0D+00, 36.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EXPAND_LINEAR_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_EXPAND_LINEAR linearly interpolates new data'
      write ( *, '(a)' ) '  between old values.'

      call r8vec_print ( n, x, '  Original vector:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Expansion factor is ', fat

      call r8vec_expand_linear ( n, x, fat, xfat )

      call r8vec_print ( nfat, xfat, '  Fattened vector:' )

      return
      end
      subroutine r8vec_frac_test ( )

c*********************************************************************72
c
cc R8VEC_FRAC_TEST tests R8VEC_FRAC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision afrac
      integer k
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_FRAC_TEST'
      write ( *, '(a)' ) '  R8VEC_FRAC: K-th smallest R8VEC entry;'

      seed = 123456789

      call r8vec_uniform_01 ( n, seed, a )

      call r8vec_print ( n, a, '  Array to search:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Fractile  Value '
      write ( *, '(a)' ) ' '

      do k = 1, n, n / 2

        call r8vec_frac ( n, a, k, afrac )

        write ( *, '(2x,i8,2x,g14.6)' ) k, afrac

      end do

      return
      end
      subroutine r8vec_heap_d_extract_test ( )

c*********************************************************************72
c
cc R8VEC_HEAP_D_EXTRACT_TEST tests R8VEC_HEAP_D_EXTRACT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a(n_max)
      double precision b
      double precision c
      integer i
      double precision r8_uniform_ab
      integer n
      integer seed
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_HEAP_D_EXTRACT_TEST'
      write ( *, '(a)' ) '  For a heap descending sorted R8VEC,'
      write ( *, '(a)' ) 
     &  '  R8VEC_HEAP_D_EXTRACT extracts the maximum value;'

      n = 0

      seed = 123456789

      do i = 1, n_max

        b = 0.0D+00
        c = 10.0D+00

        value = r8_uniform_ab ( b, c, seed )

        call r8vec_heap_d_insert ( n, a, value )

        write ( *, '(a)' ) ' '
        write ( *, '(a,f10.4)' ) '  Inserting value          ', value

        call r8vec_heap_d_max ( n, a, value )

        write ( *, '(a,f10.4)' ) '  Current maximum value is ', value

      end do

      call r8vec_print ( n, a, '  Current heap as a vector:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now extract the maximum several times.'
      write ( *, '(a)' ) ' '

      do i = 1, 5
        call r8vec_heap_d_extract ( n, a, value )
        write ( *, '(a,f10.4)' ) 
     &  '  Extracting maximum element = ', value
      end do

      call r8vec_print ( n, a, '  Current heap as a vector:' )

      return
      end
      subroutine r8vec_heap_d_insert_test ( )

c*********************************************************************72
c
cc R8VEC_HEAP_D_INSERT_TEST tests R8VEC_HEAP_D_INSERT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a(n_max)
      double precision b
      double precision c
      integer i
      double precision r8_uniform_ab
      integer n
      integer seed
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_HEAP_D_INSERT_TEST'
      write ( *, '(a)' ) '  For a heap descending sorted R8VEC,'
      write ( *, '(a)' ) 
     &  '  R8VEC_HEAP_D_INSERT inserts a value into the heap.'

      n = 0

      seed = 123456789

      do i = 1, n_max

        b = 0.0D+00
        c = 10.0D+00

        value = r8_uniform_ab ( b, c, seed )

        call r8vec_heap_d_insert ( n, a, value )

        write ( *, '(a)' ) ' '
        write ( *, '(a,f10.4)' ) '  Inserting value          ', value

        call r8vec_heap_d_max ( n, a, value )

        write ( *, '(a,f10.4)' ) '  Current maximum value is ', value

      end do

      call r8vec_print ( n, a, '  Current heap as a vector:' )

      return
      end
      subroutine r8vec_heap_d_max_test ( )

c*********************************************************************72
c
cc R8VEC_HEAP_D_MAX_TEST tests R8VEC_HEAP_D_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a(n_max)
      double precision b
      double precision c
      integer i
      double precision r8_uniform_ab
      integer n
      integer seed
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_HEAP_D_MAX_TEST'
      write ( *, '(a)' ) '  For a heap descending sorted R8VEC,'
      write ( *, '(a)' ) 
     &  '  R8VEC_HEAP_D_MAX reports the maximum value.'

      n = 0

      seed = 123456789

      do i = 1, n_max

        b = 0.0D+00
        c = 10.0D+00

        value = r8_uniform_ab ( b, c, seed )

        call r8vec_heap_d_insert ( n, a, value )

        write ( *, '(a)' ) ' '
        write ( *, '(a,f10.4)' ) '  Inserting value          ', value

        call r8vec_heap_d_max ( n, a, value )

        write ( *, '(a,f10.4)' ) '  Current maximum value is ', value

      end do

      call r8vec_print ( n, a, '  Current heap as a vector:' )

      return
      end
      subroutine r8vec_histogram_test ( )

c*********************************************************************72
c
cc R8VEC_HISTOGRAM_TEST tests R8VEC_HISTOGRAM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer histo_num
      parameter ( histo_num = 20 )
      integer n
      parameter ( n = 1000 )

      double precision a(n)
      double precision a_hi
      double precision a_lo
      double precision bin_hi
      double precision bin_lo
      integer histo_gram(0:histo_num+1)
      integer i
      integer seed
      integer test
      integer test_num

      seed = 123456789
      test_num = 2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_HISTOGRAM_TEST'
      write ( *, '(a)' ) '  R8VEC_HISTOGRAM histograms a real vector.'

      do test = 1, test_num

        if ( test .eq. 1 ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Uniform data:'

          a_lo =  0.0D+00
          a_hi = +1.0D+00
          call r8vec_uniform_01 ( n, seed, a )

        else if ( test .eq. 2 ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Normal data:'
          a_lo = -3.0D+00
          a_hi = +3.0D+00
          call r8vec_normal_01 ( n, seed, a )

        end if

        call r8vec_histogram ( n, a, a_lo, a_hi, histo_num, histo_gram )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Histogram of data:'
        write ( *, '(a)' ) ' '

        do i = 0, histo_num+1

          if ( i .eq. 0 ) then

            write ( *, '(2x,10x,2x,f10.4,2x,i8)' ) a_lo, histo_gram(i)

          else if ( i .le. histo_num ) then

            bin_lo = ( dble ( histo_num - i + 1 ) * a_lo   
     &               + dble (             i - 1 ) * a_hi ) 
     &               / dble ( histo_num         )

            bin_hi = ( dble ( histo_num - i ) * a_lo   
     &               + dble (             i ) * a_hi ) 
     &               / dble ( histo_num     )

            write ( *, '(2x,f10.4,2x,f10.4,2x,i8)' ) 
     &        bin_lo, bin_hi, histo_gram(i)

          else if ( i .eq. histo_num+1 ) then

            write ( *, '(2x,f10.4,2x,10x,2x,i8)' ) a_hi, histo_gram(i)

          end if

        end do

      end do

      return
      end
      subroutine r8vec_house_column_test ( )

c*********************************************************************72
c
cc R8VEC_HOUSE_COLUMN_TEST tests R8VEC_HOUSE_COLUMN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision a_col(n)
      double precision h(n,n)
      double precision ha(n,n)
      integer i
      integer k
      double precision r8_hi
      double precision r8_lo
      integer seed
      double precision v(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_HOUSE_COLUMN_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_HOUSE_COLUMN returns the compact form of'
      write ( *, '(a)' ) '  a Householder matrix that "packs" a column'
      write ( *, '(a)' ) '  of a matrix.'
c
c  Get a random matrix.
c
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789

      call r8mat_uniform_ab ( n, n, r8_lo, r8_hi, seed, a )

      call r8mat_print ( n, n, a, '  Matrix A:' )

      do k = 1, n - 1

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Working on column K = ', k

        do i = 1, n
          a_col(i) = a(i,k)
        end do

        call r8vec_house_column ( n, a_col, k, v )

        call r8mat_house_form ( n, v, h )

        call r8mat_print ( n, n, h, '  Householder matrix H:' )

        call r8mat_mm ( n, n, n, h, a, ha )

        call r8mat_print ( n, n, ha, '  Product H*A:' )
c
c  If we set A := HA, then we can successively convert A to upper
c  triangular form.
c
        call r8mat_copy ( n, n, ha, a )

      end do

      return
      end
      subroutine r8vec_index_delete_all_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_DELETE_ALL_TEST tests R8VEC_INDEX_DELETE_ALL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      integer i
      integer indx(n_max)
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_DELETE_ALL_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDEX_DELETE_ALL deletes all copies of a'
      write ( *, '(a)' ) '  particular value.'

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      seed = 123456789

      do i = 1, 20
        xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert ( n, x, indx, xval )
      end do

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Call R8VEC_INDEX_DELETE_ALL to delete all values of 7:'

      xval = 7.0D+00
      call r8vec_index_delete_all ( n, x, indx, xval )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      return
      end
      subroutine r8vec_index_delete_dupes_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_DELETE_DUPES_TEST tests R8VEC_INDEX_DELETE_DUPES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      integer i
      integer indx(n_max)
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_DELETE_DUPES_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDEX_DELETE_DUPES deletes duplicates.'

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      seed = 123456789

      do i = 1, 20
        xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert ( n, x, indx, xval )
      end do

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Call R8VEC_INDEX_DELETE_DUPES to delete duplicates:'

      call r8vec_index_delete_dupes ( n, x, indx, n, x, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of unique entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2)' ) i, indx(i), x(i)
      end do

      return
      end
      subroutine r8vec_index_delete_one_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_DELETE_ONE_TEST tests R8VEC_INDEX_DELETE_ONE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      integer i
      integer indx(n_max)
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_DELETE_ONE_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDEX_DELETE_ONE deletes one copies of a'
      write ( *, '(a)' ) '  particular value.'

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      seed = 123456789

      do i = 1, 20
        xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert ( n, x, indx, xval )
      end do

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Call R8VEC_INDEX_DELETE_ONE to delete one value of 8:'

      xval = 8.0D+00
      call r8vec_index_delete_one ( n, x, indx, xval, n, x, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      return
      end
      subroutine r8vec_index_insert_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_INSERT_TEST tests R8VEC_INDEX_INSERT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      integer i
      integer indx(n_max)
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_INSERT_TEST'
      write ( *, '(a)' ) '  R8VEC_INDEX_INSERT inserts values into an'
      write ( *, '(a)' ) '  index sorted array.'

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      seed = 123456789

      do i = 1, 20
        xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert ( n, x, indx, xval )
      end do

      xval = 7.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      xval = 8.0D+00
      call r8vec_index_insert ( n, x, indx, xval )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      return
      end
      subroutine r8vec_index_order_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_ORDER_TEST tests R8VEC_INDEX_ORDER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      integer i
      integer indx(n_max)
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_ORDER_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDEX_ORDER sorts an index sorted array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Generate some random values:'
      write ( *, '(a)' ) ' '
      seed = 123456789

      do i = 1, N_MAX
        xval = r8_uniform_ab ( 0.0D+00, 20.0D+00, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert_unique ( n, x, indx, xval )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of unique entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &  i, indx(i), x(i), x(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Now call R8VEC_INDEX_ORDER to carry out the sorting:'

      call r8vec_index_order ( n, x, indx )

      call r8vec_print ( n, x, '  X:' )

      return
      end
      subroutine r8vec_index_insert_unique_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_INSERT_UNIQUE_TEST tests R8VEC_INDEX_INSERT_UNIQUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision b
      double precision c
      integer i
      integer indx(n_max)
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_INSERT_UNIQUE_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDEX_INSERT_UNIQUE inserts unique values '
      write ( *, '(a)' ) '  into an index sorted array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Generate some random values:'
      write ( *, '(a)' ) ' '

      b = 0.0D+00
      c = dble ( n_max )
      seed = 123456789

      do i = 1, n_max
        xval = r8_uniform_ab ( b, c, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert_unique ( n, x, indx, xval )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      return
      end
      subroutine r8vec_index_search_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_SEARCH_TEST tests R8VEC_INDEX_SEARCH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision b
      double precision c
      integer equal
      integer i
      integer indx(n_max)
      integer less
      integer more
      integer n
      double precision r8_uniform_ab
      integer seed
      double precision x(n_max)
      double precision xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_SEARCH_TEST'
      write ( *, '(a)' ) '  R8VEC_INDEX_SEARCH searches for an entry '
      write ( *, '(a)' ) '  with a given value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Generate some random values:'
      write ( *, '(a)' ) ' '

      b = 0.0D+00
      c = dble ( n_max )
      seed = 123456789

      do i = 1, n_max
        xval = r8_uniform_ab ( b, c, seed )
        xval = dble ( nint ( xval ) )
        write ( *, '(4x,f6.2)' ) xval
        call r8vec_index_insert_unique ( n, x, indx, xval )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,f6.2,9x,f6.2)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Results of search for given XVAL:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  XVAL  Less Equal More'
      write ( *, '(a)' ) ' '

      do i = 0, n_max
        xval = dble ( i )
        call r8vec_index_search ( n, x, indx, xval, less, equal, more )
        write ( *, '(2x,f6.2,3x,i3,3x,i3,3x,i3)' ) 
     &    xval, less, equal, more
      end do

      return
      end
      subroutine r8vec_index_sorted_range_test ( )

c*********************************************************************72
c
cc R8VEC_INDEX_SORTED_RANGE_TEST tests R8VEC_INDEX_SORTED_RANGE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer i
      integer i_hi
      integer i_lo
      integer indx(n)
      double precision r(n)
      double precision r_lo
      double precision r_hi
      double precision r8_uniform_01
      integer seed
      double precision t
      integer test

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEX_SORTED_RANGE_TEST'
      write ( *, '(a)' )
     &  '  R8VEC_INDEX_SORTED_RANGE seeks the range I_LO:I_HI'
      write ( *, '(a)' ) '  of entries of sorted indexed R so that'
      write ( *, '(a)' )
     &  '  R_LO <= R(INDX(I)) <= R_HI for I_LO <= I <= I_HI.'

      seed = 123456789

      do test = 1, 5

        call r8vec_uniform_01 ( n, seed, r )

        call r8vec_print ( n, r, '  Array' )

        call r8vec_sort_heap_index_a ( n, r, indx )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '     I  INDX    R(INDX(I))'
        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i4,2x,i4,2x,g14.6)' ) i, indx(i), r(indx(i))
        end do

        r_lo = r8_uniform_01 ( seed )
        r_hi = r8_uniform_01 ( seed )

        if ( r_hi .lt. r_lo ) then
          t = r_lo
          r_lo = r_hi
          r_hi = t
        end if

        call r8vec_index_sorted_range ( n, r, indx, r_lo, r_hi, i_lo,
     &    i_hi )

        write ( *, '(a)' ) ' '
        if ( i_hi .lt. i_lo ) then
          write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_LO', r_lo
          write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_HI', r_hi
          write ( *, '(a)' ) '  Empty range in R.'
        else

          write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_LO', r_lo
          do i = i_lo, i_hi
            write ( *, '(2x,i4,2x,i4,2x,g14.6)' ) i, indx(i), r(indx(i))
          end do
          write ( *, '(2x,a4,2x,6x,g14.6)' ) 'R_HI', r_hi
        end if

      end do

      return
      end
      subroutine r8vec_indexed_heap_d_test ( )

c*********************************************************************72
c
cc R8VEC_INDEXED_HEAP_D_TEST tests R8VEC_INDEXED_HEAP_D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 20 )
      integer n
      parameter ( n = 10 )

      double precision a(m)
      integer i
      integer indx(n)

      save a
      save indx

      data a /
     &  101.0D+00, 102.0D+00, 103.0D+00, 104.0D+00, 105.0D+00,
     &  106.0D+00, 107.0D+00, 108.0D+00, 109.0D+00, 110.0D+00,
     &  111.0D+00, 112.0D+00, 113.0D+00, 114.0D+00, 115.0D+00,
     &  116.0D+00, 117.0D+00, 118.0D+00, 119.0D+00, 120.0D+00 /
      data indx /
     &  1, 11, 17, 5, 7, 13, 15, 3, 19, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_TEST'
      write ( *, '(a)' ) '  R8VEC_INDEXED_HEAP_D creates a descending'
      write ( *, '(a)' ) '  heap from an indexed R8VEC.'
c
c  Print before.
c
      call r8vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Create the heap.
c
      call r8vec_indexed_heap_d ( n, a, indx )
c
c  Print afterwards.  Only INDX should change.
c
      call r8vec_print ( m, a,
     &  '  The data vector (should NOT change):' )
      call i4vec_print ( n, indx, '  The index vector (may change):' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) is now a heap:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do

      return
      end
      subroutine r8vec_indexed_heap_d_extract_test ( )

c*********************************************************************72
c
cc R8VEC_INDEXED_HEAP_D_EXTRACT_TEST tests R8VEC_INDEXED_HEAP_D_EXTRACT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 20 )
      integer n_max
      parameter ( n_max = 20 )

      double precision a(m)
      integer i
      integer indx(n_max)
      integer indx_extract
      integer indx_insert
      integer indx_max
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_EXTRACT_TEST'
      write ( *, '(a)' ) '  For an indexed R8VEC,'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDEXED_HEAP_D_EXTRACT extracts the maximum value;'
c
c  Set the data array.  To keep things easy, we will use the indicator vector.
c
      call r8vec_indicator1 ( m, a )
c
c  The index array will initially be a random subset of the numbers 1 to M,
c  in random order.
c
      n = 5
      indx(1)  =  9
      indx(2)  =  2
      indx(3)  =  8
      indx(4)  = 14
      indx(5)  =  5
      indx(6)  =  7
      indx(7)  = 15
      indx(8)  =  1
      indx(9)  = 19
      indx(10) = 20
      indx(11) =  3

      call r8vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Create the descending heap.
c
      call r8vec_indexed_heap_d ( n, a, indx )

      call i4vec_print ( n, indx, '  The index vector after heaping:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after heaping:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Insert five entries, and monitor the maximum.
c
      do i = 1, 5

        indx_insert = indx(n+1)

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Inserting value ', a(indx_insert)

        call r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

        call r8vec_indexed_heap_d_max ( n, a, indx, indx_max )

        write ( *, '(a,g14.6)' ) '  Current maximum is ', a(indx_max)

      end do
      call r8vec_print ( m, a, '  The data vector after insertions:' )
      call i4vec_print ( n, indx,
     &  '  The index vector after insertions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after insertions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Extract the first 5 largest elements.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now extract the maximum several times.'
      write ( *, '(a)' ) ' '

      do i = 1, 5
        call r8vec_indexed_heap_d_extract ( n, a, indx, indx_extract )
        write ( *, '(a,i8,a,g14.6)' ) '  Extracting maximum element A(',
     &    indx_extract,') = ', a(indx_extract)
      end do

      call r8vec_print ( m, a, '  The data vector after extractions:' )
      call i4vec_print ( n, indx,
     & '  The index vector after extractions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after extractions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do

      return
      end
      subroutine r8vec_indexed_heap_d_insert_test ( )

c*********************************************************************72
c
cc R8VEC_INDEXED_HEAP_D_INSERT_TEST tests R8VEC_INDEXED_HEAP_D_INSERT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 20 )
      integer n_max
      parameter ( n_max = 20 )

      double precision a(m)
      integer i
      integer indx(n_max)
      integer indx_insert
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_INSERT_TEST'
      write ( *, '(a)' ) '  For an indexed R8VEC,'
      write ( *, '(a)' )
     &  '  R8VEC_INDEXED_HEAP_D_INSERT inserts a value into the heap.'
c
c  Set the data array.  To keep things easy, we will use the indicator vector.
c
      call r8vec_indicator1 ( m, a )
c
c  The index array will initially be a random subset of the numbers 1 to M,
c  in random order.
c
      n = 5
      indx(1)  =  9
      indx(2)  =  2
      indx(3)  =  8
      indx(4)  = 14
      indx(5)  =  5
      indx(6)  =  7
      indx(7)  = 15
      indx(8)  =  1
      indx(9)  = 19
      indx(10) = 20
      indx(11) =  3

      call r8vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Create the descending heap.
c
      call r8vec_indexed_heap_d ( n, a, indx )

      call i4vec_print ( n, indx, '  The index vector after heaping:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after heaping:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Insert five entries, and monitor the maximum.
c
      do i = 1, 5

        indx_insert = indx(n+1)

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Inserting value ', a(indx_insert)

        call r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

      end do
      call r8vec_print ( m, a, '  The data vector after insertions:' )
      call i4vec_print ( n, indx,
     &  '  The index vector after insertions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after insertions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do

      return
      end
      subroutine r8vec_indexed_heap_d_max_test ( )

c*********************************************************************72
c
cc R8VEC_INDEXED_HEAP_D_MAX_TEST tests R8VEC_INDEXED_HEAP_D_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 20 )
      integer n_max
      parameter ( n_max = 20 )

      double precision a(m)
      integer i
      integer indx(n_max)
      integer indx_insert
      integer indx_max
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_MAX_TEST'
      write ( *, '(a)' ) '  For an indexed R8VEC,'
      write ( *, '(a)' )
     &  '  R8VEC_INDEXED_HEAP_D_MAX reports the maximum value.'
c
c  Set the data array.  To keep things easy, we will use the indicator vector.
c
      call r8vec_indicator1 ( m, a )
c
c  The index array will initially be a random subset of the numbers 1 to M,
c  in random order.
c
      n = 5
      indx(1)  =  9
      indx(2)  =  2
      indx(3)  =  8
      indx(4)  = 14
      indx(5)  =  5
      indx(6)  =  7
      indx(7)  = 15
      indx(8)  =  1
      indx(9)  = 19
      indx(10) = 20
      indx(11) =  3

      call r8vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Create the descending heap.
c
      call r8vec_indexed_heap_d ( n, a, indx )

      call i4vec_print ( n, indx, '  The index vector after heaping:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after heaping:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do
c
c  Insert five entries, and monitor the maximum.
c
      do i = 1, 5

        indx_insert = indx(n+1)

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Inserting value ', a(indx_insert)

        call r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

        call r8vec_indexed_heap_d_max ( n, a, indx, indx_max )

        write ( *, '(a,g14.6)' ) '  Current maximum is ', a(indx_max)

      end do
      call r8vec_print ( m, a, '  The data vector after insertions:' )
      call i4vec_print ( n, indx,
     &  '  The index vector after insertions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after insertions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6)' ) i, a(indx(i))
      end do

      return
      end
      subroutine r8vec_indicator0_test ( )

c*********************************************************************72
c
cc R8VEC_INDICATOR0_TEST tests R8VEC_INDICATOR0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_INDICATOR0_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_INDICATOR0 returns an indicator vector.'

      call r8vec_indicator0 ( n, a )

      call r8vec_print ( n, a, '  The indicator0 vector:' )

      return
      end
      subroutine r8vec_legendre_test ( )

c*********************************************************************72
c
cc R8VEC_LEGENDRE_TEST tests R8VEC_LEGENDRE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 7 )

      integer n
      double precision r(n_max)
      double precision r1
      double precision r2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_LEGENDRE_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_LEGENDRE computes N Legendre points in [R1,R2].'

      r1 = -1.0D+00
      r2 = +1.0D+00
      n = 5

      call r8vec_legendre ( n, r1, r2, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4,a,g14.6,a,g14.6)' ) 
     &  '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

      call r8vec_print ( n, r, '  Legendre points:' )

      r1 =   0.0D+00
      r2 = +10.0D+00
      n = 7

      call r8vec_legendre ( n, r1, r2, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4,a,g14.6,a,g14.6)' ) 
     &  '  N = ', n, '  R1 = ', r1, '  R2 = ', r2

      call r8vec_print ( n, r, '  Legendre points:' )

      return
      end
      subroutine r8vec_linspace_test ( )

c*********************************************************************72
c
cc R8VEC_LINSPACE_TEST tests R8VEC_LINSPACE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_LINSPACE_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) 
     &  '  R8VEC_LINSPACE: evenly spaced points between A and B;'

      a = 10.0D+00
      b = 20.0D+00

      call r8vec_linspace ( n, a, b, x )
      call r8vec_print ( n, x, '  r8vec_linspace ( 5, 10, 20 )' )

      return
      end
      subroutine r8vec_max_test ( )

c*********************************************************************72
c
cc R8VEC_MAX_TEST tests R8VEC_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision aval
      double precision b
      double precision c
      double precision r8vec_max
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MAX_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_MAX:       maximum entry;'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      aval = r8vec_max ( n, a )
      write ( *, '(a,g14.6)' ) '  Maximum:                 ', aval

      return
      end
      subroutine r8vec_max_index_test ( )

c*********************************************************************72
c
cc R8VEC_MAX_INDEX_TEST tests R8VEC_MAX_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MAX_INDEX_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_MAX_INDEX: index of maximum entry;'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call r8vec_max_index ( n, a, ival )
      write ( *, '(a,i8)' ) '  Maximum index:           ', ival

      return
      end
      subroutine r8vec_mean_test ( )

c*********************************************************************72
c
cc R8VEC_MEAN_TEST tests R8VEC_MEAN and R8VEC_MEDIAN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      double precision mean
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MEAN_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_MEAN:      mean value;'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call r8vec_mean ( n, a, mean )
      write ( *, '(a,g14.6)' ) '  Mean:                     ', mean

      return
      end
      subroutine r8vec_median_test ( )

c*********************************************************************72
c
cc R8VEC_MEDIAN_TEST tests R8VEC_MEDIAN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      double precision median
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MEDIAN_TEST'
      write ( *, '(a)' ) '  R8VEC_MEDIAN: median value of R8VEC;'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call r8vec_median ( n, a, median )
      write ( *, '(a,g14.6)' ) '  Median:                   ', median

      return
      end
      subroutine r8vec_midspace_test ( )

c*********************************************************************72
c
cc R8VEC_MIDSPACE_TEST tests R8VEC_MIDSPACE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MIDSPACE_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) 
     &  '  R8VEC_MIDSPACE: evenly spaced midpoints between A and B'

      a = 10.0D+00
      b = 20.0D+00

      call r8vec_midspace ( n, a, b, x )
      call r8vec_print ( n, x, '  r8vec_midspace ( 5, 10, 20 )' )

      return
      end
      subroutine r8vec_min_test ( )

c*********************************************************************72
c
cc R8VEC_MIN_TEST tests R8VEC_MIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision aval
      double precision b
      double precision c
      double precision r8vec_min
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MIN_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_MIN:       minimum entry.'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      aval = r8vec_min ( n, a )
      write ( *, '(a,g14.6)' ) '  Minimum:                 ', aval

      return
      end
      subroutine r8vec_min_index_test ( )

c*********************************************************************72
c
cc R8VEC_MIN_INDEX_TEST tests R8VEC_MIN_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_MIN_INDEX_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_MIN_INDEX: index of minimum entry;'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call r8vec_min_index ( n, a, ival )
      write ( *, '(a,i8)' ) '  Minimum index:           ', ival

      return
      end
      subroutine r8vec_nint_test ( )

c*********************************************************************72
c
cc R8VEC_NINT_TEST tests R8VEC_NINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n)
      integer seed
      double precision x1
      double precision x2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8VEC_NINT_TEST'
      write ( *, '(a)' ) '  R8VEC_NINT rounds an R8VEC.'

      x1 = -5.0D+00
      x2 = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, x1, x2, seed, a )
      call r8vec_print ( n, a, '  Vector A:' )
      call r8vec_nint ( n, a )
      call r8vec_print ( n, a, '  Rounded vector A:' )

      return
      end
      subroutine r8vec_norm_l0_test ( )

c*********************************************************************72
c
cc R8VEC_NORM_L0_TEST tests R8VEC_NORM_L0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision a_lo
      double precision a_hi
      double precision r8vec_norm_l0
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORM_L0_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORM_L0 computes the L0 norm of an R8VEC.'

      a_lo = - 2.0D+00
      a_hi = + 2.0D+00

      seed = 123456789

      call r8vec_uniform_ab ( n, a_lo, a_hi, seed, a )
      call r8vec_nint ( n, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      write ( *, '(a,g14.6)' ) '  L0 norm:                 ', 
     &  r8vec_norm_l0 ( n, a )

      return
      end
      subroutine r8vec_norm_l1_test ( )

c*********************************************************************72
c
cc R8VEC_NORM_L1_TEST tests R8VEC_NORM_L1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      double precision r8vec_norm_l1
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORM_L1_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORM_L1 computes the L1 norm of an R8VEC'

      b = - real ( n, kind = 8 )
      c = real ( n, kind = 8 )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      write ( *, '(a,g14.6)' ) '  L1 norm:                 ', 
     &  r8vec_norm_l1 ( n, a )

      return
      end
      subroutine r8vec_norm_l2_test ( )

c*********************************************************************72
c
cc R8VEC_NORM_L2_TEST tests R8VEC_NORM_L2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      double precision r8vec_norm_l2
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORM_L2_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORM_L2 computes the L2 norm of an R8VEC'

      b = - real ( n, kind = 8 )
      c = real ( n, kind = 8 )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      write ( *, '(a,g14.6)' ) '  L2 norm:                 ', 
     &  r8vec_norm_l2 ( n, a )

      return
      end
      subroutine r8vec_norm_li_test ( )

c*********************************************************************72
c
cc R8VEC_NORM_LI_TEST tests R8VEC_NORM_LI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      double precision r8vec_norm_li
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORM_LI_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORM_LI computes the Loo norm of an R8VEC'

      b = - real ( n, kind = 8 )
      c = real ( n, kind = 8 )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      write ( *, '(a,g14.6)' ) '  L-Infinity norm:         ', 
     &  r8vec_norm_li ( n, a )

      return
      end
      subroutine r8vec_normal_01_test ( )

c*********************************************************************72
c
cc R8VEC_NORMAL_01_TEST tests R8VEC_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 1000 )

      integer i
      integer n
      double precision r8vec_max
      double precision r8vec_min
      integer seed
      double precision x(n_max)
      double precision x_max
      double precision x_mean
      double precision x_min
      double precision x_var

      seed = 123456789


      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_01_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORMAL_01 computes a vector of normally'
      write ( *, '(a)' ) '  distributed random numbers.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed
c
c  Test 1:
c  Simply call 5 times for 1 value, and print.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test #1: Call 5 times, 1 value each time.'
      write ( *, '(a)' ) ' '

      n = 1
      do i = 1, 5
        call r8vec_normal_01 ( n, seed, x )
        write ( *, '(2x,i8,g14.6)' ) i, x(1)
      end do
c
c  Test 2:
c  Restore the random number seed, and repeat.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test #2: Restore the random number seed.'
      write ( *, '(a)' ) '  Call 5 times, 1 value each time.'
      write ( *, '(a)' ) '  The results should be identical.'
      write ( *, '(a)' ) ' '

      seed = 123456789

      n = 1
      do i = 1, 5
        call r8vec_normal_01 ( n, seed, x )
        write ( *, '(2x,i8,g14.6)' ) i, x(1)
      end do
c
c  Test 3:
c  Restore the random number seed, compute all 5 values at once.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test #3: Restore the random number seed.'
      write ( *, '(a)' ) '  Call 1 time for 5 values.'
      write ( *, '(a)' ) '  The results should be identical.'
      write ( *, '(a)' ) ' '

      seed = 123456789

      n = 5
      call r8vec_normal_01 ( n, seed, x )

      do i = 1, n
        write ( *, '(2x,i8,g14.6)' ) i, x(i)
      end do
c
c  Test 4:
c  Restore the random number seed, compute all 5 values at once.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test #4: Restore the random number seed.'
      write ( *, '(a)' ) '  Call for 2, 1, and 2 values.'
      write ( *, '(a)' ) '  The results should be identical.'
      write ( *, '(a)' ) ' '

      seed = 123456789

      n = 2
      call r8vec_normal_01 ( n, seed, x )

      do i = 1, n
        write ( *, '(2x,i8,g14.6)' ) i, x(i)
      end do

      n = 1
      call r8vec_normal_01 ( n, seed, x )

      do i = 1, n
        write ( *, '(2x,i8,g14.6)' ) i, x(i)
      end do

      n = 2
      call r8vec_normal_01 ( n, seed, x )

      do i = 1, n
        write ( *, '(2x,i8,g14.6)' ) i, x(i)
      end do
c
c  Test 5:
c  Determine the minimum, maximum, mean and variance.
c
      n = n_max
      call r8vec_normal_01 ( n, seed, x )
      x_min = r8vec_min ( n, x )
      x_max = r8vec_max ( n, x )
      call r8vec_mean ( n, x, x_mean )
      call r8vec_variance ( n, x, x_var )
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test #5:'
      write ( *, '(a,i12)' ) '  Number of samples was ', n
      write ( *, '(a,g14.6)' ) '  Minimum value was ', x_min
      write ( *, '(a,g14.6)' ) '  Maximum value was ', x_max
      write ( *, '(a,g14.6)' ) '  Average value was ', x_mean
      write ( *, '(a,g14.6)' ) '  Variance was      ', x_var
      write ( *, '(a,g14.6)' ) '  Expected average  ', 0.0D+00
      write ( *, '(a,g14.6)' ) '  Expected variance ', 1.0D+00

      return
      end
      subroutine r8vec_normalize_l1_test ( )

c*********************************************************************72
c
cc R8VEC_NORMALIZE_L1_TEST tests R8VEC_NORMALIZE_L1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMALIZE_L1_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) '  R8VEC_NORMALIZE_L1:  make unit sum;'

      b = - dble ( n )
      c = dble ( n )

      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Input vector:' )

      call r8vec_normalize_l1 ( n, a )

      call r8vec_print ( n, a, '  After calling R8VEC_NORMALIZE_L1:' )

      return
      end
      subroutine r8vec_order_type_test ( )

c*********************************************************************72
c
cc R8VEC_ORDER_TYPE_TEST tests R8VEC_ORDER_TYPE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer test_num
      parameter ( test_num = 6 )

      integer j
      integer order
      integer test
      double precision x(n)
      double precision x_test(n,test_num)

      save x_test

      data x_test /
     &  1.0D+00, 3.0D+00, 2.0D+00, 4.0D+00, 
     &  2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 
     &  1.0D+00, 2.0D+00, 2.0D+00, 4.0D+00, 
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 
     &  4.0D+00, 4.0D+00, 3.0D+00, 1.0D+00, 
     &  9.0D+00, 7.0D+00, 3.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_ORDER_TYPE_TEST'
      write ( *, '(a)' ) '  R8VEC_ORDER_TYPE classifies a R8VEC as'
      write ( *, '(a)' ) '  -1: no order'
      write ( *, '(a)' ) '   0: all equal;'
      write ( *, '(a)' ) '   1: ascending;'
      write ( *, '(a)' ) '   2: strictly ascending;'
      write ( *, '(a)' ) '   3: descending;'
      write ( *, '(a)' ) '   4: strictly descending.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        do j = 1, n
          x(j) = x_test(j,test)
        end do

        call r8vec_order_type ( n, x, order )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  The following vector has order type ', order
        write ( *, '(a)' ) ' '
        do j = 1, n
          write ( *, '(i8,g14.6)' ) j, x(j)
        end do

      end do

      return
      end
      subroutine r8vec_permute_test ( )

c*********************************************************************72
c
cc R8VEC_PERMUTE_TEST tests R8VEC_PERMUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer p(n)
      double precision x(n)

      save p
      save x

      data p / 2, 4, 5, 1, 3 /
      data x /
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_PERMUTE_TEST'
      write ( *, '(a)' ) '  R8VEC_PERMUTE permutes a R8VEC in place.'

      call r8vec_print ( n, x, '  Original array X[]:' )

      call i4vec_print ( n, p, '  Permutation vector P[]:' )

      call r8vec_permute ( n, p, x )

      call r8vec_print ( n, x, '  Permuted array X[P[]]:' )

      return
      end
      subroutine r8vec_polarize_test ( )

c*********************************************************************72
c
cc R8VEC_POLARIZE_TEST tests R8VEC_POLARIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n)
      double precision a2(n)
      double precision a_normal(n)
      double precision a_parallel(n)
      double precision ap_norm
      integer i
      double precision p(n)
      double precision p_norm
      double precision pan
      double precision pap
      double precision r8vec_dot_product
      double precision r8vec_norm

      save a
      save p

      data a /
     &  1.0D+00, 2.0D+00,  3.0D+00 /
      data p /
     &  3.0D+00, 1.0D+00, -2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_POLARIZE_TEST'
      write ( *, '(a)' ) '  R8VEC_POLARIZE decomposes a vector into'
      write ( *, '(a)' ) 
     &  '  components parallel and normal to a direction.'

      call r8vec_print ( n, a, '  Original vector:' )

      call r8vec_print ( n, p, '  Direction vector:' )

      call r8vec_polarize ( n, a, p, a_normal, a_parallel )

      call r8vec_print ( n, a_normal, '  Normal component:' )

      call r8vec_print ( n, a_parallel, '  Parallel component:' )

      pan = r8vec_dot_product ( n, p, a_normal )

      p_norm = r8vec_norm ( n, p )
      ap_norm = r8vec_norm ( n, a_parallel )

      pap = r8vec_dot_product ( n, p, a_parallel ) 
     &  / ( p_norm * ap_norm )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Dot product of P and A_normal (should be 0) ', pan
      write ( *, '(a,g14.6)' ) 
     &  '  Cosine of angle between P and A_parallel ' //
     &  ' (should be 1 or -1) ', pap

      do i = 1, n
        a2(i) = a_normal(i) + a_parallel(i)
      end do

      call r8vec_print ( n, a2, 
     &  '  Sum of components (should equal A):' )

      return
      end
      subroutine r8vec_print_test ( )

c*********************************************************************72
c
cc R8VEC_PRINT_TEST tests R8VEC_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n)

      save a

      data a /
     &  123.456D+00,
     &  0.000005D+00,
     &  -1.0D+06,
     &  3.14159265D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
      write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

      call r8vec_print ( n, a, '  The vector:' )

      return
      end
      subroutine r8vec_reverse_test ( )

c***********************************************************************72
c
cc R8VEC_REVERSE_TEST tests R8VEC_REVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_REVERSE_TEST'
      write ( *, '(a)' ) '  R8VEC_REVERSE reverses a R8VEC.'

      call r8vec_indicator1 ( n, a )

      call r8vec_print ( n, a, '  Original array:' )

      call r8vec_reverse ( n, a )

      call r8vec_print ( n, a, '  Reversed array:' )

      return
      end
      subroutine r8vec_rotate_test ( )

c*********************************************************************72
c
cc R8VEC_ROTATE_TEST tests R8VEC_ROTATE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n)

      integer m

      save a

      data a /
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /

      m = 2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_ROTATE_TEST'
      write ( *, '(a)' ) '  R8VEC_ROTATE rotates a R8VEC in place.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) 
     &  '  Rotate entries ', m, ' places to the right.'

      call r8vec_print ( n, a, '  Original array:' )

      call r8vec_rotate ( n, a, m )

      call r8vec_print ( n, a, '  Rotated array:' )

      return
      end
      subroutine r8vec_search_binary_a_test ( )

c*********************************************************************72
c
cc R8VEC_SEARCH_BINARY_A_TEST tests R8VEC_SEARCH_BINARY_A;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      integer indx
      double precision search_val
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SEARCH_BINARY_A_TEST'
      write ( *, '(a)' ) '  For ascending order:'
      write ( *, '(a)' ) 
     &  '  R8VEC_SEARCH_BINARY_A searches a sorted array;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      call r8vec_uniform_01 ( n, seed, a )

      search_val = a(1)

      call r8vec_sort_heap_a ( n, a )

      call r8vec_print ( n, a, '  Sorted vector A:' )
c
c  Now search the sorted array for a given value.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Search the array for the value ', search_val

      call r8vec_search_binary_a ( n, a, search_val, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SEARCH RESULT:'
      write ( *, '(a)' ) ' '

      if ( 0 .lt. indx ) then
        write ( *, '(a,i8)' ) '    The value occurs in index ', indx
      else
        write ( *, '(a)' ) '    The value does not occur in the array.'
      end if

      return
      end
      subroutine r8vec_sort_bubble_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_BUBBLE_A_TEST tests R8VEC_SORT_BUBBLE_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_BUBBLE_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORT_BUBBLE_A ascending sorts a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Original array:' )

      call r8vec_sort_bubble_a ( n, a )

      call r8vec_print_some ( n, a, 1, 10, 
     &  '  Ascending sorted array:' )

      return
      end
      subroutine r8vec_sort_heap_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_HEAP_A_TEST tests R8VEC_SORT_HEAP_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_HEAP_A_TEST'
      write ( *, '(a)' ) '  R8VEC_SORT_HEAP_A ascending sorts a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Original array:' )

      call r8vec_sort_heap_a ( n, a )

      call r8vec_print_some ( n, a, 1, 10, '  Ascending sorted array:' )

      return
      end
      subroutine r8vec_sort_heap_d_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_HEAP_D_TEST tests R8VEC_SORT_HEAP_D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_HEAP_D_TEST'
      write ( *, '(a)' ) '  R8VEC_SORT_HEAP_D descending sorts a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Original array:' )

      call r8vec_sort_heap_d ( n, a )

      call r8vec_print_some ( n, a, 1, 10, 
     &  '  Descending sorted array:' )

      return
      end
      subroutine r8vec_sort_heap_index_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_HEAP_INDEX_A_TEST tests R8VEC_SORT_HEAP_INDEX_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer indx(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_HEAP_INDEX_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORT_HEAP_INDEX_A creates an ascending'
      write ( *, '(a)' ) '  sort index for a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Unsorted array:' )

      call r8vec_sort_heap_index_a ( n, a, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After indexed ascending sort:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), A(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i8,g14.6)' ) i, indx(i), a(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now use the index array to carry out the'
      write ( *, '(a)' ) '  permutation implicitly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  INDX(I), A(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(i8,g14.6)' ) indx(i), a(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Call R8VEC_PERMUTE to carry out the permutation'
      write ( *, '(a)' ) '  explicitly.'

      call r8vec_permute ( n, indx, a )

      call r8vec_print ( n, a, '  I, A(I)' )

      return
      end
      subroutine r8vec_sort_heap_index_d_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_HEAP_INDEX_D_TEST tests R8VEC_SORT_HEAP_INDEX_D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer indx(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_HEAP_INDEX_D_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORT_HEAP_INDEX_D creates a descending'
      write ( *, '(a)' ) '  sort index for a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Unsorted array:' )

      call r8vec_sort_heap_index_d ( n, a, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After indexed descending sort:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), A(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i8,g14.6)' ) i, indx(i), a(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now use the index array to carry out the'
      write ( *, '(a)' ) '  permutation implicitly.'
      write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  INDX(I), ARRAY(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(i8,g14.6)' ) indx(i), a(indx(i))
      end do

      return
      end
      subroutine r8vec_sort_heap_mask_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_HEAP_MASK_A_TEST tests R8VEC_SORT_HEAP_MASK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mask_num
      parameter ( mask_num = 10 )
      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer indx(mask_num)
      integer mask(mask_num)

      integer seed

      save mask

      data mask /
     &  2, 4, 7, 8, 9, 12, 13, 16, 18, 19 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_HEAP_MASK_A_TEST'
      write ( *, '(a)' ) '  R8VEC_SORT_HEAP_MASK_A creates an ascending'
      write ( *, '(a)' ) '  sort index for a masked R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Unsorted array:' )

      call i4vec_print ( mask_num, mask, '  The mask array:' )

      call r8vec_mask_print ( n, a, mask_num, mask, 
     &  '  The masked unsorted array:' )

      call r8vec_sort_heap_mask_a ( n, a, mask_num, mask, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After masked indexed ascending sort:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), MASK(INDX(I)), A(MASK(INDX(I)))'
      write ( *, '(a)' ) ' '
      do i = 1, mask_num
        write ( *, '(3i8,g14.6)' ) 
     &    i, indx(i), mask(indx(i)), a(mask(indx(i)))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Call I4VEC_PERMUTE to carry out the index permutation'
      write ( *, '(a)' ) '  explicitly on the MASK vector.'

      call i4vec_permute ( mask_num, indx, mask )
c
c  Essentially, INDX becomes the identity vector now.
c
      call i4vec_indicator1 ( mask_num, indx )

      call i4vec_print ( mask_num, mask, '  The reordered mask array:' )

      call r8vec_mask_print ( n, a, mask_num, mask, 
     &  '  The reordered masked sorted array:' )

      return
      end
      subroutine r8vec_sort_insert_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_INSERT_A_TEST tests R8VEC_SORT_INSERT_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_INSERT_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORT_INSERT_A ascending sorts a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Unsorted array:' )

      call r8vec_sort_insert_a ( n, a )

      call r8vec_print_some ( n, a, 1, 10, '  Sorted array:' )

      return
      end
      subroutine r8vec_sort_insert_index_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_INSERT_INDEX_A_TEST tests R8VEC_SORT_INSERT_INDEX_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer indx(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_INSERT_INDEX_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORT_INSERT_INDEX_A creates an ascending'
      write ( *, '(a)' ) '  sort index for a R8VEC.'

      b = 0.0D+00
      c = 3.0D+00 * dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print_some ( n, a, 1, 10, '  Unsorted array:' )

      call r8vec_sort_insert_index_a ( n, a, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After indexed ascending sort:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), A(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i8,g14.6)' ) i, indx(i), a(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now use the index array to carry out the'
      write ( *, '(a)' ) '  permutation implicitly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(i8, i8,g14.6)' ) i, indx(i), a(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Call R8VEC_PERMUTE to carry out the '
      write ( *, '(a)' ) '  permutation explicitly.'

      call r8vec_permute ( n, indx, a )

      call r8vec_print_some ( n, a, 1, 10, '  Permuted data' )

      return
      end
      subroutine r8vec_sort_quick_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORT_QUICK_A_TEST tests R8VEC_SORT_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORT_QUICK_A_TEST'
      write ( *, '(a)' ) '  R8VEC_SORT_QUICK_A sorts a R8VEC'
      write ( *, '(a)' ) '  using quick sort.'

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      call r8vec_print ( n, a, '  Unsorted array:' )

      call r8vec_sort_quick_a ( n, a )

      call r8vec_print ( n, a, '  Sorted array:' )

      return
      end
      subroutine r8vec_sorted_merge_a_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_MERGE_A_TEST tests R8VEC_SORTED_MERGE_A;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer na
      parameter ( na = 10 )
      integer nb
      parameter ( nb = 10 )

      double precision a(na)
      double precision b(nb)
      double precision c(na+nb)
      integer nc
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_MERGE_A_TEST'
      write ( *, '(a)' ) '  For ascending order:'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_MERGE_A merges two sorted R8VEC''s;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      call r8vec_uniform_01 ( na, seed, a )
      call r8vec_uniform_01 ( nb, seed, b )

      call r8vec_sort_heap_a ( na, a )

      call r8vec_sort_heap_a ( nb, b )

      call r8vec_print ( na, a, '  Sorted vector A:' )

      call r8vec_print ( nb, b, '  Sorted vector B:' )

      call r8vec_sorted_merge_a ( na, a, nb, b, nc, c )

      call r8vec_print ( nc, c, '  Merged vector C:' )

      return
      end
      subroutine r8vec_sorted_nearest_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_NEAREST_TEST tests R8VEC_SORTED_NEAREST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision b
      double precision c
      integer i
      integer j
      double precision r8_uniform_ab
      integer r8vec_sorted_nearest
      integer seed
      double precision x(n)
      double precision xval

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_NEAREST_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_NEAREST finds the nearest entry'
      write ( *, '(a)' ) '  in a sorted R8VEC.'

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789
    
      call r8vec_uniform_ab ( n, b, c, seed, x )
      call r8vec_sort_heap_a ( n, x )

      call r8vec_print ( n, x, '  Sorted array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     Test        Nearest'
      write ( *, '(a)' ) '     Value    Index   Value'
      write ( *, '(a)' ) ' '
      do i = 1, 10

        xval = r8_uniform_ab ( b, c, seed )

        j = r8vec_sorted_nearest ( n, x, xval )

        write ( *, '(2x,f8.4,4x,i8,2x,f8.4)' ) xval, j, x(j)

      end do

      return
      end
      subroutine r8vec_sorted_range_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_RANGE_TEST tests R8VEC_SORTED_RANGE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 September 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer i
      integer i_hi
      integer i_lo
      double precision r(n)
      double precision r_lo
      double precision r_hi
      double precision r8_uniform_01
      integer seed
      double precision t
      integer test

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_RANGE_TEST'
      write ( *, '(a)' )
     &  '  R8VEC_SORTED_RANGE seeks the range of indices'
      write ( *, '(a)' ) '  in a sorted vector R so that'
      write ( *, '(a)' ) '  R_LO <= R(I_LO:I_HI) <= R_HI.'

      seed = 123456789

      do test = 1, 5

        call r8vec_uniform_01 ( n, seed, r )

        call r8vec_sort_heap_a ( n, r )

        call r8vec_print ( n, r, '  Sorted array R:' )

        r_lo = r8_uniform_01 ( seed )
        r_hi = r8_uniform_01 ( seed )

        if ( r_hi .lt. r_lo ) then
          t = r_lo
          r_lo = r_hi
          r_hi = t
        end if

        call r8vec_sorted_range ( n, r, r_lo, r_hi, i_lo, i_hi )

        write ( *, '(a)' ) ' '
        if ( i_hi .lt. i_lo ) then
          write ( *, '(2x,a4,2x,g14.6)' ) 'R_LO', r_lo
          write ( *, '(2x,a4,2x,g14.6)' ) 'R_HI', r_hi
          write ( *, '(2x,a)' ) '  Empty range in R.'
        else

          write ( *, '(2x,a4,2x,g14.6)' ) 'R_LO', r_lo
          do i = i_lo, i_hi
            write ( *, '(2x,i4,2x,g14.6)' ) i, r(i)
          end do
          write ( *, '(2x,a4,2x,g14.6)' ) 'R_HI', r_hi
        end if

      end do

      return
      end
      subroutine r8vec_sorted_split_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_SPLIT_TEST tests R8VEC_SORTED_SPLIT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 25 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer i_gt
      integer i_lt
      integer seed
      double precision split

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_SPLIT_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_SPLIT splits a sorted vector into'
      write ( *, '(a)' ) '  entries less than and greater than a'
      write ( *, '(a)' ) '  splitting value.'

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      do i = 1, n
        a(i) = dble ( nint ( a(i) ) ) / 2.0D+00
      end do

      call r8vec_sort_heap_a ( n, a )

      split = 0.5D+00 * ( a(1) + a(n) )

      call r8vec_print ( n, a, '  The sorted array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Splitting value is ', split

      call r8vec_sorted_split ( n, a, split, i_lt, i_gt )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Lower index I_LT = ', i_lt
      write ( *, '(a,i8)' ) '  Upper index I_GT = ', i_gt

      return
      end
      subroutine r8vec_sorted_undex_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_UNDEX_TEST tests R8VEC_SORTED_UNDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer x_num
      parameter ( x_num = 9 )

      integer i
      double precision r8_epsilon
      double precision tol
      integer undx(x_num)
      integer x_unique_num
      double precision x_val(x_num)
      integer xdnu(x_num)
      double precision xu_val(x_num)

      save x_val

      data x_val /
     &  11.0, 11.0, 11.0, 22.0, 22.0, 33.0, 33.0, 55.0, 55.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_UNDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_UNDEX produces index vectors which ' // 
     &  'create a sorted'
      write ( *, '(a)' ) 
     &  '  list of the unique elements of a sorted R8VEC,'
      write ( *, '(a)' ) 
     &  '  and a map from the original vector to the (implicit)'
      write ( *, '(a)' ) '  vector of sorted unique elements.'

      call r8vec_print ( x_num, x_val, '  The vector X:' )

      tol = r8_epsilon ( )
      call r8vec_sorted_unique_count ( x_num, x_val, tol, x_unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Tolerance for equality is ', tol
      write ( *, '(a,i8)' ) 
     &  '  Number of unique entries in X is ', x_unique_num

      call r8vec_sorted_undex ( x_num, x_val, x_unique_num, tol, 
     &  undx, xdnu )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  UNDX can be used to list the unique elements of X'
      write ( *, '(a)' ) '  in sorted order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
      write ( *, '(a)' ) ' '
      do i = 1, x_unique_num
        write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), x_val(undx(i))
      end do

      do i = 1, x_unique_num
        xu_val(i) = x_val(undx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
      write ( *, '(a)' ) 
     &  '  containing only the unique elements, in sorted order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  UNDX     XU(I)'
      write ( *, '(a)' ) ' '
      do i = 1, x_unique_num
        write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), xu_val(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '  XDNU can be used to match each element of X with one of the'
      write ( *, '(a)' )'  unique elements'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )'     I  XDNU    X(I)       XU(XDNU(I))'
      write ( *, '(a)' ) ' '

      do i = 1, x_num
        write ( *, '(2x,i4,2x,i4,2x,f8.1,2x,f12.1)' ) 
     &    i, xdnu(i), x_val(i), xu_val(xdnu(i))
      end do

      return
      end
      subroutine r8vec_sorted_unique_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_UNIQUE_TEST tests R8VEC_SORTED_UNIQUE;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 30 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer seed
      double precision tol
      parameter ( tol = 0.25D+00 )
      integer unique_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_UNIQUE_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_UNIQUE finds the unique entries in'
      write ( *, '(a)' ) '  a sorted R8VEC;'

      b = 0.0D+00
      c = dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      do i = 1, n
        a(i) = dble ( int ( a(i) ) )
      end do

      call r8vec_print_some ( n, a, 1, 10, '  Unsorted array:' )

      call r8vec_sort_heap_a ( n, a )

      call r8vec_sorted_unique ( n, a, tol, unique_num )

      call r8vec_print ( unique_num, a, '  Unique entries' )

      return
      end
      subroutine r8vec_sorted_unique_count_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_UNIQUE_COUNT_TEST tests R8VEC_SORTED_UNIQUE_COUNT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 30 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer unique_num
      integer seed
      double precision tol

      tol = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_UNIQUE_COUNT_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_UNIQUE_COUNT counts the unique entries'
      write ( *, '(a)' ) '  of a sorted R8VEC;'

      b = 0.0D+00
      c = dble ( n )
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      do i = 1, n
        a(i) = dble ( int ( a(i) ) )
      end do

      call r8vec_sorted_unique_count ( n, a, tol, unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Using a tolerance of ', tol
      write ( *, '(a,i8,a)' ) 
     &  '  R8VEC_SORTED_UNIQUE_COUNT counts ', unique_num, 
     &  ' unique entries in A.'

      return
      end
      subroutine r8vec_sorted_unique_hist_test ( )

c*********************************************************************72
c
cc R8VEC_SORTED_UNIQUE_HIST_TEST tests R8VEC_SORTED_UNIQUE_HIST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer unique_max
      parameter ( unique_max = 30 )
      integer n
      parameter ( n = 30 )

      double precision a(n)
      integer acount(unique_max)
      double precision auniq(unique_max)
      double precision b
      double precision c
      integer i
      integer unique_num
      integer seed
      double precision tol

      tol = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SORTED_UNIQUE_HIST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SORTED_UNIQUE_HIST makes a historgram of'
      write ( *, '(a)' ) '  the unique entries in a real vector.'

      b = 0.0D+00
      c = dble ( n )
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  Using random number seed ', seed

      call r8vec_uniform_ab ( n, b, c, seed, a )

      do i = 1, n
        a(i) = dble ( int ( a(i) ) ) + 0.5D+00
      end do

      call r8vec_print ( n, a, '  Unsorted array:' )

      call r8vec_sort_bubble_a ( n, a )

      call r8vec_print ( n, a, '  Ascending sorted array:' )

      call r8vec_sorted_unique_hist ( n, a, tol, unique_max,
     &  unique_num, auniq, acount )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) 
     &  '  R8VEC_SORTED_UNIQUE_HIST counts ' , unique_num, 
     &  ' unique entries.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Value  Multiplicity'
      write ( *, '(a)' ) ' '
      do i = 1, unique_num
        write ( *, '(2x,i8,2x,g14.6,2x,i8)' ) i, auniq(i), acount(i)
      end do

      return
      end
      subroutine r8vec_split_test ( )

c*********************************************************************72
c
cc R8VEC_SPLIT_TEST tests R8VEC_SPLIT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 25 )

      double precision a(n)
      double precision b
      double precision c
      integer i
      integer isplit
      integer seed
      double precision split

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_SPLIT_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_SPLIT splits a sorted vector into'
      write ( *, '(a)' ) '  entries less than and greater than a'
      write ( *, '(a)' ) '  splitting value.'

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a )

      do i = 1, n
        a(i) = dble ( nint ( a(i) ) ) / 2.0D+00
      end do

      call r8vec_print ( n, a, '  The array:' )

      split = 0.5D+00 * ( a(1) + a(n) )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Splitting value is ', split

      call r8vec_split ( n, a, split, isplit )

      call r8vec_print ( n, a, '  The split array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Array entries <= SPLIT up to index ', isplit

      return
      end
      subroutine r8vec_transpose_print_test ( )

c*********************************************************************72
c
cc R8VEC_TRANSPOSE_PRINT_TEST tests R8VEC_TRANSPOSE_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 12 )

      integer seed
      double precision x(n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_TRANSPOSE_PRINT_TEST'
      write ( *, '(a)' )
     &  '  R8VEC_TRANSPOSE_PRINT prints an R8VEC "tranposed",'
      write ( *, '(a)' )
     &  '  that is, placing multiple entries on a line.'

      call r8vec_uniform_01 ( n, seed, x )

      call r8vec_transpose_print ( n, x, '  The vector X:' )

      return
      end
      subroutine r8vec_undex_test ( )

c*********************************************************************72
c
cc R8VEC_UNDEX_TEST tests R8VEC_UNDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer x_num
      parameter ( x_num = 9 )

      integer i
      double precision r8_epsilon
      double precision tol
      integer undx(x_num)
      integer x_unique_num
      double precision x_val(x_num)
      integer xdnu(x_num)
      double precision xu_val(x_num)

      save x_val

      data x_val /
     &  33.0, 55.0, 11.0, 11.0, 55.0, 33.0, 22.0, 22.0, 11.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_UNDEX_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_UNDEX produces index vectors which create a sorted'
      write ( *, '(a)' ) 
     &  '  list of the unique elements of an (unsorted) R8VEC,'
      write ( *, '(a)' ) 
     &  '  and a map from the original vector to the (implicit)'
      write ( *, '(a)' ) '  vector of sorted unique elements.'

      call r8vec_print ( x_num, x_val, '  The vector X:' )

      tol = r8_epsilon ( )
      call r8vec_unique_count ( x_num, x_val, tol, x_unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Tolerance for equality is ', tol
      write ( *, '(a,i8)' ) 
     &  '  Number of unique entries in X is ', x_unique_num

      call r8vec_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  UNDX can be used to list the unique elements of X'
      write ( *, '(a)' ) '  in sorted order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
      write ( *, '(a)' ) ' '
      do i = 1, x_unique_num
        write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), x_val(undx(i))
      end do

      do i = 1, x_unique_num
        xu_val(i) = x_val(undx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
      write ( *, '(a)' ) 
     &  '  containing only the unique elements, in sorted order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  UNDX     XU(I)'
      write ( *, '(a)' ) ' '
      do i = 1, x_unique_num
        write ( *, '(2x,i4,2x,i4,2x,f8.1)' ) i, undx(i), xu_val(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '  XDNU can be used to match each element of X with one of the'
      write ( *, '(a)' )'  unique elements'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )'     I  XDNU    X(I)       XU(XDNU(I))'
      write ( *, '(a)' ) ' '

      do i = 1, x_num
        write ( *, '(2x,i4,2x,i4,2x,f8.1,2x,f12.1)' ) 
     &    i, xdnu(i), x_val(i), xu_val(xdnu(i))
      end do

      return
      end
      subroutine r8vec_uniform_01_test ( )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision r(n)
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
      write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
      write ( *, '(a)' ) '  with entries in [0,1].'

      seed = 123456789

      do test = 1, test_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i12)' ) '  Input SEED = ', seed

        call r8vec_uniform_01 ( n, seed, r )

        call r8vec_print_some ( n, r, 1, 10, '  Random vector:' )

      end do

      return
      end
      subroutine r8vec_uniform_ab_test ( )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB_TEST tests R8VEC_UNIFORM_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a
      double precision b
      double precision r(n)
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 3 )

      a = 10.0D+00
      b = 20.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_UNIFORM_AB_TEST'
      write ( *, '(a)' ) '  R8VEC_UNIFORM_AB returns a random R8VEC '
      write ( *, '(a)' ) '  with entries in a given range [ A, B ]'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For this problem:'
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, '(a,g14.6)' ) '  B = ', b

      seed = 123456789

      do test = 1, test_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i12)' ) '  Input SEED = ', seed

        call r8vec_uniform_ab ( n, a, b, seed, r )

        call r8vec_print ( n, r, '  Random vector:' )

      end do

      return
      end
      subroutine r8vec_variance_test ( )

c*********************************************************************72
c
cc R8VEC_VARIANCE_TEST tests R8VEC_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n)
      double precision r8_hi
      double precision r8_lo
      integer seed
      double precision variance

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_VARIANCE_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_VARIANCE computes the variance of an R8VEC.'

      r8_lo = - 5.0D+00
      r8_hi = + 5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, a )

      call r8vec_print ( n, a, '  Vector:' )

      call r8vec_variance ( n, a, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Variance: ', variance

      return
      end
      subroutine r8vec2_sort_a_test ( )

c*********************************************************************72
c
cc R8VEC2_SORT_A_TEST tests R8VEC2_SORT_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a1(n)
      double precision a2(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC2_SORT_A_TEST'
      write ( *, '(a)' ) '  For a pair of R8VEC''s:'
      write ( *, '(a)' ) '  R8VEC2_SORT_A ascending sorts;'

      b = 1.0D+00
      c = 3.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b,  c, seed, a1 )

      b = 5.0D+00
      c = 10.0D+00

      call r8vec_uniform_ab ( n, b, c, seed, a2 )

      a1(3) = a1(1)
      a2(3) = a2(1)

      a1(6) = a1(2)
      a2(6) = a2(2)

      a1(9) = a1(1)
      a2(9) = a2(1)

      call r8vec2_print ( n, a1, a2, '  The pair of arrays:' )

      call r8vec2_sort_a ( n, a1, a2 )

      call r8vec2_print ( n, a1, a2, '  Arrays after ascending sort:' )

      return
      end
      subroutine r8vec2_sort_d_test ( )

c*********************************************************************72
c
cc R8VEC2_SORT_D_TEST tests R8VEC2_SORT_D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a1(n)
      double precision a2(n)
      double precision b
      double precision c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC2_SORT_D_TEST'
      write ( *, '(a)' ) '  For a pair of R8VEC''s:'
      write ( *, '(a)' ) '  R8VEC2_SORT_D descending sorts;'

      b = 1.0D+00
      c = 3.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b,  c, seed, a1 )

      b = 5.0D+00
      c = 10.0D+00

      call r8vec_uniform_ab ( n, b, c, seed, a2 )

      a1(3) = a1(1)
      a2(3) = a2(1)

      a1(6) = a1(2)
      a2(6) = a2(2)

      a1(9) = a1(1)
      a2(9) = a2(1)

      call r8vec2_print ( n, a1, a2, '  The pair of arrays:' )

      call r8vec2_sort_d ( n, a1, a2 )

      call r8vec2_print ( n, a1, a2, '  Arrays after descending sort:' )

      return
      end
      subroutine r8vec2_sort_heap_index_a_test ( )

c*********************************************************************72
c
cc R8VEC2_SORT_HEAP_INDEX_A_TEST tests R8VEC2_SORT_HEAP_INDEX_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer i
      integer i4_uniform_ab
      integer indx(n)
      integer seed
      double precision x(n)
      double precision y(n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC2_SORT_HEAP_INDEX_A_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC2_SORT_HEAP_INDEX_A creates a sort index'
      write ( *, '(a)' ) '  for an (X,Y) array.'

      do i = 1, n

        x(i) = dble ( i4_uniform_ab ( 0, n, seed ) ) / dble ( n )
        y(i) = dble ( i4_uniform_ab ( 0, n, seed ) ) / dble ( n )

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The unsorted array:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, X(I), Y(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,6x,2g14.6)' ) i, x(i), y(i)
      end do

      call r8vec2_sort_heap_index_a ( n, x, y, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After sorting:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), X(I), Y(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i8,2g14.6)' ) i, indx(i), x(i), y(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now use the index array to carry out the'
      write ( *, '(a)' ) '  permutation implicitly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), X(INDX(I)), Y(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i8,2g14.6)' ) i, indx(i), x(indx(i)), y(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  R8VEC_PERMUTE carries out the permutation.'

      call r8vec_permute ( n, indx, x )
      call r8vec_permute ( n, indx, y )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, X(I), Y(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,6x,2g14.6)' ) i, x(i), y(i)
      end do

      return
      end
      subroutine r8vec2_sorted_unique_test ( )

c*********************************************************************72
c
cc R8VEC2_SORTED_UNIQUE_TEST tests R8VEC2_SORTED_UNIQUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a1(n)
      double precision a2(n)
      double precision b
      double precision c
      integer unique_num
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC2_SORTED_UNIQUE_TEST'
      write ( *, '(a)' ) '  For a pair of R8VEC''s:'
      write ( *, '(a)' ) '  R8VEC2_SORTED_UNIQUE counts unique entries.'

      b = 1.0D+00
      c = 3.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a1 )

      b = 5.0D+00
      c = 10.0D+00

      call r8vec_uniform_ab ( n, b, c, seed, a2 )

      a1(3) = a1(1)
      a2(3) = a2(1)

      a1(6) = a1(2)
      a2(6) = a2(2)

      a1(9) = a1(1)
      a2(9) = a2(1)

      call r8vec2_print ( n, a1, a2, '  The pair of arrays:' )

      call r8vec2_sort_a ( n, a1, a2 )

      call r8vec2_print ( n, a1, a2, '  Arrays after ascending sort:' )

      call r8vec2_sorted_unique ( n, a1, a2, unique_num )

      call r8vec2_print ( unique_num, a1, a2, '  UNIQed array:' )

      return
      end
      subroutine r8vec2_sorted_unique_index_test ( )

c*********************************************************************72
c
cc R8VEC2_SORTED_UNIQUE_INDEX_TEST tests R8VEC2_SORTED_UNIQUE_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 February 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a1(n)
      double precision a2(n)
      double precision b
      double precision c
      integer indx(n)
      integer unique_num
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC2_SORTED_UNIQUE_INDEX_TEST'
      write ( *, '(a)' ) '  For a pair of R8VEC''s:'
      write ( *, '(a)' ) 
     &  '  R8VEC2_SORTED_UNIQUE_INDEX indexes unique entries.'

      b = 1.0D+00
      c = 3.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a1 )

      b = 5.0D+00
      c = 10.0D+00

      call r8vec_uniform_ab ( n, b, c, seed, a2 )

      a1(3) = a1(1)
      a2(3) = a2(1)

      a1(6) = a1(2)
      a2(6) = a2(2)

      a1(9) = a1(1)
      a2(9) = a2(1)

      call r8vec2_sort_a ( n, a1, a2 )

      call r8vec2_print ( n, a1, a2, '  Sorted arrays:' )

      call r8vec2_sorted_unique_index ( n, a1, a2, unique_num, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The number of unique elements is ', unique_num

      call i4vec_print ( unique_num, indx, 
     &  '  Index of Unique Elements:' )

      call r8vec_index_order ( unique_num, a1, indx )
      call r8vec_index_order ( unique_num, a2, indx )

      call r8vec2_print ( unique_num, a1, a2, 
     &  '  After Indexed Nonunique Deletion.' )

      return
      end
      subroutine r8vec2_sum_max_index_test ( )

c*********************************************************************72
c
cc R8VEC2_SUM_MAX_INDEX_TEST tests R8VEC2_SUM_MAX_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a1(n)
      double precision a2(n)
      double precision b
      double precision c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC2_SUM_MAX_INDEX_TEST'
      write ( *, '(a)' ) '  For a pair of R8VEC''s:'
      write ( *, '(a)' ) '  R8VEC2_SUM_MAX_INDEX: index of the sum'
      write ( *, '(a)' ) '  vector with maximum value.'

      b = 0.0D+00
      c = 10.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, b, c, seed, a1 )

      b = 0.0D+00
      c = 5.0D+00

      call r8vec_uniform_ab ( n, b, c, seed, a2 )

      call r8vec2_print ( n, a1, a2, '  The pair of vectors:' )

      call r8vec2_sum_max_index ( n, a1, a2, ival )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Index of maximum in A+B: ', ival

      return
      end
      subroutine roots_to_r8poly_test ( )

c*********************************************************************72
c
cc ROOTS_TO_R8POLY_TEST tests ROOTS_TO_R8POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision c(0:n)
      double precision x(n)

      save x

      data x /
     &  1.0D+00, -4.0D+00, 3.0D+00, 0.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ROOTS_TO_R8POLY:'
      write ( *, '(a)' ) 
     &  '  Given N real roots, construct the coefficient'
      write ( *, '(a)' ) '  vector of the corresponding polynomial.'

      call r8vec_print ( n, x, '  N real roots:' )

      call roots_to_r8poly ( n, x, c )

      call r8poly_print ( n, c, '  Corresponding polynomial:' )

      return
      end

