      program main

c*********************************************************************72
c
cc MAIN is the main program for I4LIB_PRB.
c
c  Discussion:
c
c    I4LIB_PRB tests the I4LIB library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 March 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4LIB_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the I4LIB library.'

      call i4_abs_test ( )
      call i4_bit_hi1_test ( )
      call i4_bit_lo0_test ( )
      call i4_bit_lo1_test ( )
      call i4_bit_reverse_test ( )
      call i4_ceiling_test ( )
      call i4_characteristic_test ( )
      call i4_choose_test ( )
      call i4_choose_check_test ( )
      call i4_div_rounded_test ( )
      call i4_division_test ( )
      call i4_divp_test ( )
      call i4_factorial_test ( )
      call i4_factorial2_test ( )
      call i4_fall_test ( )
      call i4_floor_test ( )
      call i4_gcd_test ( )
      call i4_gcdb_test ( )
      call i4_huge_test ( )
      call i4_huge_normalizer_test ( )
      call i4_is_even_test ( )
      call i4_is_odd_test ( )
      call i4_is_power_of_2_test ( )
      call i4_is_prime_test ( )
      call i4_lcm_test ( )
      call i4_log_10_test ( )
      call i4_log_2_test ( )
      call i4_log_i4_test ( )
      call i4_log_r8_test ( )
      call i4_mant_test ( )
      call i4_max_test ( )
      call i4_min_test ( )
      call i4_mod_inv_test ( )
      call i4_moddiv_test ( )
      call i4_modp_test ( )
      call i4_mop_test ( )
      call i4_power_test ( )
      call i4_rise_test ( )
      call i4_sign_test ( )
      call i4_sign3_test ( )
      call i4_swap_test ( )
      call i4_to_halton_test ( )
      call i4_to_isbn_test ( )
      call i4_to_pascal_test ( )
      call i4_to_pascal_degree_test ( )
      call i4_to_triangle_lower_test ( )
      call i4_uniform_ab_test ( )
      call i4_walsh_1d_test ( )
      call i4_width_test ( )
      call i4_wrap_test ( )
      call i4_xor_test ( )

      call i4block_print_test ( )

      call i4col_find_item_test ( )
      call i4col_find_pair_swap_test ( )
      call i4col_sort_a_test ( )
      call i4col_sort_d_test ( )
      call i4col_sort2_a_test ( )
      call i4col_sorted_singleton_count_test ( )
      call i4col_sorted_unique_count_test ( )

      call i4mat_elim_test ( )
      call i4mat_indicator_test ( )
      call i4mat_l1_inverse_test ( )
      call i4mat_max_test ( )
      call i4mat_max_index_test ( )
      call i4mat_min_test ( )
      call i4mat_min_index_test ( )
      call i4mat_perm_uniform_test ( )
      call i4mat_red_test ( )
      call i4mat_u1_inverse_test ( )

      call i4row_max_test ( )
      call i4row_mean_test ( )
      call i4row_min_test ( )
      call i4row_sort_a_test ( )
      call i4row_sort_d_test ( )
      call i4row_sort2_d_test ( )
      call i4row_sum_test ( )
      call i4row_swap_test ( )
      call i4row_variance_test ( )

      call i4vec_add_test ( )
      call i4vec_amax_test ( )
      call i4vec_amax_index_test ( )
      call i4vec_amin_test ( )
      call i4vec_amin_index_test ( )
      call i4vec_aminz_index_test ( )
      call i4vec_aminz_index_test ( )
      call i4vec_ascend_sub_test ( )
      call i4vec_ascends_test ( )
      call i4vec_bracket_test ( )
      call i4vec_concatenate_test ( )
      call i4vec_cum_test ( )
      call i4vec_cum0_test ( )
      call i4vec_decrement_test ( )
      call i4vec_descends_test ( )
      call i4vec_direct_product_test ( )
      call i4vec_direct_product2_test ( )
      call i4vec_frac_test ( )
      call i4vec_heap_a_test ( )
      call i4vec_heap_d_test ( )
      call i4vec_histogram_test ( )
      call i4vec_increment_test ( )
      call i4vec_index_test ( )
      call i4vec_index_insert_unique_test ( )
      call i4vec_index_order_test ( )
      call i4vec_indexed_heap_d_test ( )
      call i4vec_indexed_heap_d_extract_test ( )
      call i4vec_indexed_heap_d_insert_test ( )
      call i4vec_indexed_heap_d_max_test ( )
      call i4vec_indicator0_test ( )
      call i4vec_insert_test ( )
      call i4vec_max_test ( )
      call i4vec_max_index_test ( )
      call i4vec_max_index_last_test ( )
      call i4vec_mean_test ( )
      call i4vec_median_test ( )
      call i4vec_merge_a_test ( )
      call i4vec_min_test ( )
      call i4vec_min_index_test ( )
      call i4vec_nonzero_count_test ( )
      call i4vec_nonzero_first_test ( )
      call i4vec_order_type_test ( )
      call i4vec_pairwise_prime_test ( )
      call i4vec_part_test ( )
      call i4vec_part_quick_a_test ( )
      call i4vec_permute_test ( )
      call i4vec_print_test ( )
      call i4vec_reverse_test ( )
      call i4vec_run_count_test ( )
      call i4vec_search_binary_a_test ( )
      call i4vec_sort_bubble_a_test ( )
      call i4vec_sort_heap_a_test ( )
      call i4vec_sort_heap_d_test ( )
      call i4vec_sort_heap_index_a_test ( )
      call i4vec_sort_heap_index_d_test ( )
      call i4vec_sort_insert_a_test ( )
      call i4vec_sort_quick_a_test ( )
      call i4vec_sort_shell_a_test ( )
      call i4vec_sorted_unique_hist_test ( )
      call i4vec_sorted_unique_test ( )
      call i4vec_sum_test ( )
      call i4vec_transpose_print_test ( )
      call i4vec_undex_test ( )
      call i4vec_uniform_ab_test ( )
      call i4vec_unique_index_test ( )
      call i4vec_value_index_test ( )
      call i4vec_variance_test ( )

      call i4vec2_sort_a_test ( )
      call i4vec2_sort_d_test ( )
      call i4vec2_sorted_unique_test ( )

      call pascal_to_i4_test ( )

      call perm_uniform_test ( )
      call prime_test ( )

      call triangle_lower_to_i4_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4LIB_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine i4_abs_test ( )

c*********************************************************************72
c
cc I4_ABS_TEST tests I4_ABS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer b
      integer i
      integer i4_abs
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_ABS_TEST'
      write ( *, '(a)' ) '  I4_ABS returns the absolute value of an I4.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       A       B=I4_ABS(A)'
      write ( *, '(a)' ) ''

      i4_lo = -100
      i4_hi = +100
      seed = 123456789
      do i = 1, 10
        a = i4_uniform_ab ( i4_lo, i4_hi, seed )
        b = i4_abs ( a )
        write ( *, '(2x,i8,2x,i8)' ) a, b
      end do

      return
      end
      subroutine i4_bit_hi1_test ( )

c*********************************************************************72
c
cc I4_BIT_HI1_TEST tests I4_BIT_HI1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_uniform_ab
      integer i4_bit_hi1
      integer j
      integer seed
      integer test
      integer test_num

      seed = 123456789
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BIT_HI1_TEST'
      write ( *, '(a)' )
     &  '  I4_BIT_HI1 returns the location of the high 1 bit.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       I  I4_BIT_HI1(I)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i4_uniform_ab ( 0, 100, seed )
        j = i4_bit_hi1 ( i )
        write ( *, '(2x,i8,2x,i8)' ) i, j
      end do

      return
      end
      subroutine i4_bit_lo0_test ( )

c*********************************************************************72
c
cc I4_BIT_LO0_TEST tests I4_BIT_LO0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_uniform_ab
      integer i4_bit_lo0
      integer j
      integer seed
      integer test
      integer test_num

      seed = 123456789
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BIT_LO0_TEST'
      write ( *, '(a)' )
     &  '  I4_BIT_LO0 returns the location of the low 0 bit.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       I  I4_BIT_LO0(I)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i4_uniform_ab ( 0, 100, seed )
        j = i4_bit_lo0 ( i )
        write ( *, '(2x,i8,2x,i8)' ) i, j
      end do

      return
      end
      subroutine i4_bit_lo1_test ( )

c*********************************************************************72
c
cc I4_BIT_LO1_TEST tests I4_BIT_LO1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_uniform_ab
      integer i4_bit_lo1
      integer j
      integer seed
      integer test
      integer test_num

      test_num = 10
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BIT_LO1_TEST'
      write ( *, '(a)' )
     &  '  I4_BIT_LO1 returns the location of the low 1 bit.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       I  I4_BIT_LO1(I)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i4_uniform_ab ( 0, 100, seed )
        j = i4_bit_lo1 ( i )
        write ( *, '(2x,i8,2x,i8)' ) i, j
      end do

      return
      end
      subroutine i4_bit_reverse_test ( )

c*********************************************************************72
c
cc I4_BIT_REVERSE_TEST tests I4_BIT_REVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i_hi
      integer i4_bit_reverse
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_BIT_REVERSE_TEST'
      write ( *, '(a)' )
     &  '  I4_BIT_REVERSE bit reverses I with respect to 2^J'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I         J  I4_BIT_REVERSE(I,J)'
      write ( *, '(a)' ) ' '

      do j = 0, 4
        i_hi = 2**j - 1
        do i = 0, i_hi
          k = i4_bit_reverse ( i, j )
          write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k
        end do
      end do

      return
      end
      subroutine i4_ceiling_test ( )

c*********************************************************************72
c
cc I4_CEILING_TEST tests I4_CEILING.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4
      integer i4_ceiling
      double precision r8
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      integer seed

      r8_lo = -100.0D+00
      r8_hi =  100.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_CEILING_TEST'
      write ( *, '(a)' ) '  I4_CEILING returns the "ceiling" of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R8    I4_CEILING(R8)'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
        i4 = i4_ceiling ( r8 )
        write ( *, '(2x,f8.4,12x,i4)' ) r8, i4
      end do
 
      return
      end
      subroutine i4_characteristic_test ( )

c*********************************************************************72
c
cc I4_CHARACTERISTIC_TEST tests I4_CHARACTERISTIC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_characteristic

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_CHARACTERISTIC_TEST'
      write ( *, '(a)' )
     &  '  I4_CHARACTERISTIC computes the characteristic'
      write ( *, '(a)' ) '  of an integer Q, which is  '
      write ( *, '(a)' ) '    Q if Q is prime;'
      write ( *, '(a)' ) '    P, if Q = P**N for some prime P;'
      write ( *, '(a)' )
     &  '    0, if Q is negative, 0, 1, or the product of '
      write ( *, '(a)' ) '      more than 1 distinct prime.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  I4_CHARACTERISTIC'
      write ( *, '(a)' ) ' '

      do i = 1, 50
        write ( *, '(2x,i2,13x,i4)' ) i, i4_characteristic ( i )
      end do

      return
      end
      subroutine i4_choose_test ( )

c*********************************************************************72
c
cc I4_CHOOSE_TEST tests I4_CHOOSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer cnk
      integer i4_choose
      integer k
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_CHOOSE_TEST'
      write ( *, '(a)' ) '  I4_CHOOSE evaluates C(N,K).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N     K    CNK'
      write ( *, '(a)' ) ' '
 
      do n = 0, 4
        do k = 0, n
          cnk = i4_choose ( n, k )
          write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, k, cnk
        end do
      end do
 
      return
      end
      subroutine i4_choose_check_test ( )

c*********************************************************************72
c
cc I4_CHOOSE_CHECK_TEST tests I4_CHOOSE_CHECK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 March 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      logical check
      integer cnk
      integer i
      integer i4_choose
      logical i4_choose_check
      integer k
      integer k_test(4)
      integer n
      integer n_test(4)

      save k_test
      save n_test

      data k_test /  3,  999,   3,  10 /
      data n_test / 10, 1000, 100, 100 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_CHOOSE_CHECK_TEST'
      write ( *, '(a)' ) '  I4_CHOOSE_CHECK checks whether C(N,K)'
      write ( *, '(a)' ) '  can be computed with integer arithmetic'
      write ( *, '(a)' ) '  or not.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N     K    CHECK?    I4_CHOOSE'
      write ( *, '(a)' ) ' '
 
      do i = 1, 4
        n = n_test(i)
        k = k_test(i)
        check = i4_choose_check ( n, k )
        if ( check ) then
          cnk = i4_choose ( n, k )
          write ( *, '(2x,i4,2x,i4,8x,l1,8x,i6)' ) n, k, check, cnk
        else
          write ( *, '(2x,i4,2x,i4,8x,l1,a)' ) 
     &      n, k, check, '   Not computable'
        end if
      end do
 
      return
      end
      subroutine i4_div_rounded_test ( )

c*********************************************************************72
c
cc I4_DIV_ROUNDED_TEST tests I4_DIV_ROUNDED.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer a_hi
      integer a_lo
      integer b
      integer b_hi
      integer b_lo
      double precision c0
      integer c1
      integer c2
      integer c3
      integer c4
      integer i4_div_rounded
      integer i4_uniform_ab
      integer seed
      integer test
      integer test_num

      a_hi = 100
      a_lo = - 100
      b_hi = 10
      b_lo = - 10
      test_num = 20

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_DIV_ROUNDED_TEST'
      write ( *, '(a)' )
     &  '  I4_DIV_ROUNDED performs rounded integer division.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  C0 = real ( a ) / real ( b )'
      write ( *, '(a)' ) '  C1 = I4_DIV_ROUNDED ( A, B )'
      write ( *, '(a)' ) '  C2 = nint ( real ( a ) / real ( b ) )'
      write ( *, '(a)' ) '  C3 = A / B'
      write ( *, '(a)' ) '  C4 = int ( real ( a ) / real ( b ) )'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  C1 and C2 should be equal;'
      write ( *, '(a)' ) '  C3 and C4 should be equal.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '     A     B           C0         C1    C2      C3    C4'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, test_num
        a = i4_uniform_ab ( a_lo, a_hi, seed )
        b = i4_uniform_ab ( b_lo, b_hi, seed )
        if ( b .eq. 0 ) then
          b = 7
        end if
        c0 = dble ( a ) / dble ( b )
        c1 = i4_div_rounded ( a, b )
        c2 = nint ( dble ( a ) / dble ( b ) )
        c3 = a / b
        c4 = int ( dble ( a ) / dble ( b ) )
        write ( *, '(2x,i4,2x,i4,4x,f14.6,2x,i4,2x,i4,4x,i4,2x,i4)' )
     &    a, b, c0, c1, c2, c3, c4
      end do

      return
      end
      subroutine i4_division_test ( )

c*********************************************************************72
c
cc I4_DIVISION_TEST tests I4_DIVISION.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer a_hi
      integer a_lo
      integer b
      integer b_hi
      integer b_lo
      double precision c0
      integer c1
      integer c2
      integer c3
      integer c4
      integer i4_division
      integer i4_uniform_ab
      integer seed
      integer test
      integer test_num

      a_hi = 100
      a_lo = - 100
      b_hi = 10
      b_lo = - 10
      test_num = 20

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_DIVISION_TEST'
      write ( *, '(a)' )
     &  '  I4_DIVISION performs integer division.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  C0 = real ( a ) / real ( b )'
      write ( *, '(a)' ) '  C1 = I4_DIVISION ( A, B )'
      write ( *, '(a)' ) '  C2 = nint ( real ( a ) / real ( b ) )'
      write ( *, '(a)' ) '  C3 = A / B'
      write ( *, '(a)' ) '  C4 = int ( real ( a ) / real ( b ) )'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  C1 and C3 and C4 should be equal.'
      write ( *, '(a)' ) '  C2 may differ;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '     A     B           C0         C1    C2      C3    C4'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, test_num
        a = i4_uniform_ab ( a_lo, a_hi, seed )
        b = i4_uniform_ab ( b_lo, b_hi, seed )
        if ( b .eq. 0 ) then
          b = 7
        end if
        c0 = dble ( a ) / dble ( b )
        c1 = i4_division ( a, b )
        c2 = nint ( dble ( a ) / dble ( b ) )
        c3 = a / b
        c4 = int ( dble ( a ) / dble ( b ) )
        write ( *, '(2x,i4,2x,i4,4x,f14.6,2x,i4,2x,i4,4x,i4,2x,i4)' )
     &    a, b, c0, c1, c2, c3, c4
      end do

      return
      end
      subroutine i4_divp_test ( )

c*********************************************************************72
c
cc I4_DIVP_TEST tests I4_DIVP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer a_hi
      integer a_lo
      integer b
      integer b_hi
      integer b_lo
      integer c
      integer d
      integer i4_divp
      integer i4_uniform_ab
      integer seed
      integer test
      integer test_num

      a_hi = 100
      a_lo = - 100
      b_hi = 10
      b_lo = - 10
      test_num = 20

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_DIVP_TEST'
      write ( *, '(a)' )
     &  '  I4_DIVP(A,B) returns the smallest multiplier of J'
      write ( *, '(a)' ) '  that is less than I'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     A     B     C     D'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, test_num
        a = i4_uniform_ab ( a_lo, a_hi, seed )
        b = i4_uniform_ab ( b_lo, b_hi, seed )
        if ( b .eq. 0 ) then
          b = 7
        end if
        c = i4_divp ( a, b )
        d = c * b
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,i4)' ) a, b, c, d
      end do

      return
      end
      subroutine i4_factorial_test ( )

c*********************************************************************72
c
cc I4_FACTORIAL_TEST tests I4_FACTORIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f1
      integer f2
      integer i4_factorial
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTORIAL_TEST:'
      write ( *, '(a)' ) 
     &  '  I4_FACTORIAL evaluates the factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N      Exact         I4_Factorial(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_factorial_values ( n_data, n, f1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        f2 = i4_factorial ( n )

        write ( *, '(2x,i8,2x,i12,2x,i12)' ) n, f1, f2

      go to 10

20    continue

      return
      end
      subroutine i4_factorial2_test ( )

c*********************************************************************72
c
cc I4_FACTORIAL2_TEST tests I4_FACTORIAL2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f1
      integer f2
      integer i4_factorial2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTORIAL2_TEST:'
      write ( *, '(a)' ) 
     &  '  I4_FACTORIAL2 evaluates the double factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N      Exact         I4_Factorial2(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_factorial2_values ( n_data, n, f1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        f2 = i4_factorial2 ( n )

        write ( *, '(2x,i8,2x,i12,2x,i12)' ) n, f1, f2

      go to 10

20    continue

      return
      end
      subroutine i4_fall_test ( )

c*********************************************************************72
c
cc I4_FALL_TEST tests I4_FALL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f1
      integer f2
      integer i4_fall
      integer m
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FALL_TEST:'
      write ( *, '(a)' ) 
     &  '  I4_FALL evaluates the falling factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         M         N      Exact         I4_FALL(M,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_fall_values ( n_data, m, n, f1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        f2 = i4_fall ( m, n )

        write ( *, '(2x,i8,2x,i8,2x,i12,2x,i12)' ) m, n, f1, f2

      go to 10

20    continue

      return
      end
      subroutine i4_floor_test ( )

c*********************************************************************72
c
cc I4_FLOOR_TEST tests I4_FLOOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4
      integer i4_floor
      double precision r8
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      integer seed

      r8_lo = -100.0D+00
      r8_hi =  100.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FLOOR_TEST'
      write ( *, '(a)' ) '  I4_FLOOR returns the "floor" of an R8.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      R8    I4_FLOOR(R8)'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r8 = r8_uniform_ab ( r8_lo, r8_hi, seed )
        i4 = i4_floor ( r8 )
        write ( *, '(2x,f8.4,9x,i4)' ) r8, i4
      end do
 
      return
      end
      subroutine i4_gcd_test ( )

c*********************************************************************72
c
cc I4_GCD_TEST tests I4_GCD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 7 )

      integer i
      integer i4_gcd
      integer i_test(test_num)
      integer j
      integer j_test(test_num)
      integer test

      data i_test / 36, 49, 0, 12, 36, 1, 91 /
      data j_test / 30, -7, 71, 12, 49, 42, 28/

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_GCD_TEST'
      write ( *, '(a)' ) '  I4_GCD computes the greatest common factor,'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I     J   I4_GCD'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i_test(test)
        j = j_test(test)
        write ( *, '(2x,3i8)') i, j, i4_gcd ( i, j )
      end do

      return
      end
      subroutine i4_gcdb_test ( )

c*********************************************************************72
c
cc I4_GCDB_TEST tests I4_GCDB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 4 )

      integer i
      integer i4_gcdb
      integer i_test(test_num)
      integer j
      integer j_test(test_num)
      integer k
      integer k_test(test_num)
      integer test

      data i_test / 288,  288, 288, 288 /
      data j_test / 2880, 2880, 2880, 2880  /
      data k_test / 2, 3, 4, 5 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_GCDB_TEST'
      write ( *, '(a)' ) '  I4_GCDB computes the greatest common factor'
      write ( *, '(a)' ) '  of the form K^N'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       I       J       K  I4_GCD'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i_test(test)
        j = j_test(test)
        k = k_test(test)
        write ( *, '(4i8)') i, j, k, i4_gcdb ( i, j, k )
      end do

      return
      end
      subroutine i4_huge_test ( )

c*********************************************************************72
c
cc I4_HUGE_TEST tests I4_HUGE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4_huge

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_HUGE_TEST'
      write ( *, '(a)' ) '  I4_HUGE returns a huge integer.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  I4_HUGE() = ', i4_huge ( )

      return
      end
      subroutine i4_huge_normalizer_test ( )

c***********************************************************************72
c
cc I4_HUGE_NORMALIZER_TEST tests I4_HUGE_NORMALIZER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4
      integer i4_huge
      double precision i4_huge_normalizer
      double precision r8
      double precision value

      i4 = i4_huge ( )
      r8 = i4_huge_normalizer ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_HUGE_NORMALIZE_TEST'
      write ( *, '(a)' ) '  I4_HUGE_NORMALIZER returns 1/(I4_HUGE+1).'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  I4_HUGE() = ', i4
      write ( *, '(a,g14.6)' ) '  I4_HUGE_NORMALIZER() = ', r8

      value = dble ( i4 ) * r8

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' )
     &  '  I4_HUGE * I4_HUGE_NORMALIZER = ', value

      return
      end
      subroutine i4_is_even_test ( )

c*********************************************************************72
c
cc I4_IS_EVEN_TEST tests I4_IS_EVEN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      logical i4_is_even

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_IS_EVEN_TEST'
      write ( *, '(a)' )
     &  '  I4_IS_EVEN reports whether an I4 is even.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I     I4_IS_EVEN(I)'
      write ( *, '(a)' ) ' '

      do i = -2, 25
        write ( *, '(2x,i8,2x,l1)' ) i, i4_is_even ( i )
      end do

      return
      end
      subroutine i4_is_odd_test ( )

c*********************************************************************72
c
cc I4_IS_ODD_TEST tests I4_IS_ODD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      logical i4_is_odd

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_IS_ODD_TEST'
      write ( *, '(a)' )
     &  '  I4_IS_ODD reports whether an I4 is odd.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I     I4_IS_ODD(I)'
      write ( *, '(a)' ) ' '

      do i = -2, 25
        write ( *, '(2x,i8,2x,l1)' ) i, i4_is_odd ( i )
      end do

      return
      end
      subroutine i4_is_power_of_2_test ( )

c*********************************************************************72
c
cc I4_IS_POWER_OF_2_TEST tests I4_IS_POWER_OF_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      logical i4_is_power_of_2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_IS_POWER_OF_2_TEST'
      write ( *, '(a)' )
     &  '  I4_IS_ODD reports whether an I4 is a power of 2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I     I4_IS_POWER_OF_2(I)'
      write ( *, '(a)' ) ' '

      do i = -4, 25
        write ( *, '(2x,i8,2x,l1)' ) i, i4_is_power_of_2 ( i )
      end do

      return
      end
      subroutine i4_is_prime_test ( )

c*********************************************************************72
c
cc I4_IS_PRIME_TEST tests I4_IS_PRIME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      logical i4_is_prime

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_IS_PRIME_TEST'
      write ( *, '(a)' )
     &  '  I4_IS_PRIME reports whether an integer is prime.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I     I4_IS_PRIME(I)'
      write ( *, '(a)' ) ' '

      do i = -2, 25
        write ( *, '(2x,i8,2x,l1)' ) i, i4_is_prime ( i )
      end do

      return
      end
      subroutine i4_lcm_test ( )

c*********************************************************************72
c
cc I4_LCM_TEST tests I4_LCM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 7 )

      integer i
      integer i4_lcm
      integer i_test(test_num)
      integer j
      integer j_test(test_num)
      integer test

      save i_test
      save j_test

      data i_test / 36, 49, 0, 12, 36, 1, 91 /
      data j_test / 30, -7, 71, 12, 49, 42, 28 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_LCM_TEST'
      write ( *, '(a)' ) '  I4_LCM computes the least common multiple.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I     J   I4_LCM'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i_test(test)
        j = j_test(test)
        write ( *, '(2x,4i8)') i, j, i4_lcm ( i, j )
      end do

      return
      end
      subroutine i4_log_10_test ( )

c*********************************************************************72
c
cc I4_LOG_10_TEST tests I4_LOG_10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 13 )

      integer i4_log_10
      integer test
      integer x
      integer x_test(test_num)

      save x_test

      data x_test /
     &  0, 1, 2, 3, 9, 10, 11, 99, 101, -1, -2, -3, -9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_LOG_10_TEST'
      write ( *, '(a)' ) '  I4_LOG_10: whole part of log base 10,'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X, I4_LOG_10'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        x = x_test(test)
        write ( *, '( 2x, i8, i12 )' ) x, i4_log_10 ( x )
      end do

      return
      end
      subroutine i4_log_2_test ( )

c*********************************************************************72
c
cc I4_LOG_2_TEST tests I4_LOG_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      integer i4_log_2
      integer test
      integer x
      integer x_test(test_num)

      save x_test

      data x_test /
     &    0,    1,    2,    3,    9,
     &   10,   11,   99,  101,   -1,
     &   -2,   -3,   -9, 1000, 1023,
     & 1024, 1025 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_LOG_2_TEST'
      write ( *, '(a)' ) '  I4_LOG_2: whole part of log base 2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X     I4_LOG_2'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        x = x_test(test)
        write ( *, '( 2x, i8, i12 )' ) x, i4_log_2 ( x )
      end do

      return
      end
      subroutine i4_log_i4_test ( )

c*********************************************************************72
c
cc I4_LOG_I4_TEST tests I4_LOG_I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4
      integer i4_log_i4
      integer j4

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_LOG_I4_TEST'
      write ( *, '(a)' ) '  I4_LOG_I4: logarithm of I4 base J4,'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        I4        J4 I4_LOG_I4'
      write ( *, '(a)' ) ' '

      do j4 = 2, 5
        do i4 = 0, 10
          write ( *, '(2x, i8, 2x, i8, 2x, i8 )' )
     &      i4, j4, i4_log_i4 ( i4, j4 )
        end do
        write ( *, '(a)' ) ' '
      end do

      return
      end
      subroutine i4_log_r8_test ( )

c*********************************************************************72
c
cc I4_LOG_R8_TEST tests I4_LOG_R8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
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
      integer i4_log_r8
      integer test
      integer x

      save b_test

      data b_test /
     &  2.0D+00, 3.0D+00,  4.0D+00,  5.0D+00,   6.0D+00,
     &  7.0D+00, 8.0D+00, 16.0D+00, 32.0D+00, 256.0D+00 /

      x = 16

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_LOG_R8_TEST'
      write ( *, '(a)' ) '  I4_LOG_R8: whole part of log base B,'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  X, B, I4_LOG_R8'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        b = b_test(test)

        write ( *, '(2x, i8, g14.6, i12 )' ) x, b, i4_log_r8 ( x, b )

      end do

      return
      end
      subroutine i4_mant_test ( )

c*********************************************************************72
c
cc I4_MANT_TEST tests I4_MANT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer is
      integer j
      integer k
      integer l
      double precision x

      x = -314.159D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MANT_TEST'
      write ( *, '(a)' ) '  I4_MANT decomposes an integer,'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Number to be decomposed is X = ', x

      call i4_mant ( x, is, j, k, l )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a,i24,a,i24,a,i8)' )
     &  '  I4_MANT: X = ', is, ' * (', j, '/', k, ') * 2 ^ ', l

      return
      end
      subroutine i4_max_test ( )

c*********************************************************************72
c
cc I4_MAX_TEST tests I4_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer b
      integer c
      integer i
      integer i4_hi
      integer i4_lo
      integer i4_max
      integer i4_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MAX_TEST'
      write ( *, '(a)' ) '  I4_MAX returns the maximum of two I4''s.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       A       B      C=I4_MAX(A,B)'
      write ( *, '(a)' ) ''

      i4_lo = -100
      i4_hi = +100
      seed = 123456789
      do i = 1, 10
        a = i4_uniform_ab ( i4_lo, i4_hi, seed )
        b = i4_uniform_ab ( i4_lo, i4_hi, seed )
        c = i4_max ( a, b )
        write ( *, '(2x,i8,2x,i8,2x,i8)' ) a, b, c
      end do

      return
      end
      subroutine i4_min_test ( )

c*********************************************************************72
c
cc I4_MIN_TEST tests I4_MIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer b
      integer c
      integer i
      integer i4_hi
      integer i4_lo
      integer i4_min
      integer i4_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MIN_TEST'
      write ( *, '(a)' ) '  I4_MIN returns the minimum of two I4''s.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       A       B      C=I4_MIN(A,B)'
      write ( *, '(a)' ) ''

      i4_lo = -100
      i4_hi = +100
      seed = 123456789
      do i = 1, 10
        a = i4_uniform_ab ( i4_lo, i4_hi, seed )
        b = i4_uniform_ab ( i4_lo, i4_hi, seed )
        c = i4_min ( a, b )
        write ( *, '(2x,i8,2x,i8,2x,i8)' ) a, b, c
      end do

      return
      end
      subroutine i4_mod_inv_test ( )

c*********************************************************************72
c
cc I4_MOD_INV_TEST tests I4_MOD_INV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer b
      integer n
      integer y
      integer t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MOD_INV_TEST'
      write ( *, '(a)' ) '  I4_MOD_INV finds the inverse of B mod N,'
      write ( *, '(a)' ) '  that is, Y such that ( B * Y ) mod N = 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       B       N       Y     B*YmodN'
      write ( *, '(a)' ) ' '

      n = 17
      do b = 1, 16
        call i4_mod_inv ( b, n, y )
        t = mod ( b * y, n )
        write ( *, '(4i8)' ) b, n, y, t
      end do

      return
      end
      subroutine i4_moddiv_test ( )

c*********************************************************************72
c
cc I4_MODDIV_TEST tests I4_MODDIV;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 4 )

      integer ndivid(test_num)
      integer nmult
      integer nrem
      integer number(test_num)
      integer test

      save ndivid
      save number

      data ndivid / 50, -50, 50, -50 /
      data number / 107, 107, -107, -107 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MODDIV_TEST'
      write ( *, '(a)' ) '  I4_MODDIV factors a number'
      write ( *, '(a)' ) '    into a multiple and a remainder.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Number   Divisor  Multiple Remainder'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        call i4_moddiv ( number(test), ndivid(test), nmult, nrem )
        write ( *, '(2x,4i10)' ) number(test), ndivid(test), nmult, nrem
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Repeat using FORTRAN MOD:'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        nrem = mod ( number(test), ndivid(test) )
        nmult = number(test) / ndivid(test)
        write ( *, '(2x,4i10)' ) number(test), ndivid(test), nmult, nrem
      end do

      return
      end
      subroutine i4_modp_test ( )

c*********************************************************************72
c
cc I4_MODP_TEST tests I4_MODP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 4 )

      integer i4_modp
      integer ndivid(test_num)
      integer nmult
      integer nrem
      integer number(test_num)
      integer test

      save ndivid
      save number

      data ndivid / 50, -50, 50, -50 /
      data number / 107, 107, -107, -107 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MODP_TEST'
      write ( *, '(a)' ) '  I4_MODP factors an I4'
      write ( *, '(a)' ) '    into a multiple and a remainder.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Number   Divisor  Multiple Remainder'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        nrem = i4_modp ( number(test), ndivid(test) )
        nmult = ( number(test) - nrem ) / ndivid(test)
        write ( *, '(2x,4i10)' ) number(test), ndivid(test), nmult, nrem
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Repeat using FORTRAN MOD:'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        nrem = mod ( number(test), ndivid(test) )
        nmult = number(test) / ndivid(test)
        write ( *, '(2x,4i10)' ) number(test), ndivid(test), nmult, nrem
      end do

      return
      end
      subroutine i4_mop_test ( )

c*********************************************************************72
c
cc I4_MOP_TEST tests I4_MOP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4
      integer i4_hi
      integer i4_lo
      integer i4_mop
      integer i4_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_MOP_TEST'
      write ( *, '(a)' ) '  I4_MOP computes a minus-one-power (-1)^I.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        I4  I4_MOP(I4)'
      write ( *, '(a)' ) ' '

      i4_lo = -1000000
      i4_hi = +1000000
      seed = 123456789
  
      do i = 1, 10
        i4 = i4_uniform_ab ( i4_lo, i4_hi, seed )
        write ( *, '(2x,i8,2x,i10)' ) i4, i4_mop ( i4 )
      end do

      return
      end
      subroutine i4_power_test ( )

c*********************************************************************72
c
cc I4_POWER_TEST tests I4_POWER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 7 )

      integer i
      integer i4_power
      integer i_test(7)
      integer j
      integer j_test(7)
      integer test

      save i_test
      save j_test

      data i_test /
     &  0, 1, 2, 3, 10, -1, -2 /
      data j_test /
     &  1, 2, 3, 3, 3, 4, 5 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_POWER_TEST'
      write ( *, '(a)' ) '  I4_POWER computes I^J'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I       J  I4_POWER(I,J)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i_test(test)
        j = j_test(test)
        write ( *, '(2x,3i8)') i, j, i4_power ( i, j )
      end do

      return
      end
      subroutine i4_rise_test ( )

c*********************************************************************72
c
cc I4_RISE_TEST tests I4_RISE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f1
      integer f2
      integer i4_rise
      integer m
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_RISE_TEST:'
      write ( *, '(a)' ) 
     &  '  I4_RISE evaluates the rising factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         M         N      Exact         I4_RISE(M,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_rise_values ( n_data, m, n, f1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        f2 = i4_rise ( m, n )

        write ( *, '(2x,i8,2x,i8,2x,i12,2x,i12)' ) m, n, f1, f2

      go to 10

20    continue

      return
      end
      subroutine i4_sign_test ( )

c*********************************************************************72
c
cc I4_SIGN_TEST tests I4_SIGN.
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

      integer i4
      integer i4_sign
      integer i4_test(test_num)
      integer s
      integer test

      save i4_test

      data i4_test / -10, -7, 0, 5, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_SIGN_TEST'
      write ( *, '(a)' ) '  I4_SIGN returns the sign of an I4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    I4  I4_SIGN(I4)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i4 = i4_test(test)
        s = i4_sign ( i4 )
        write ( *, '(i6,2x,i11)' ) i4, s
      end do

      return
      end
      subroutine i4_sign3_test ( )

c*********************************************************************72
c
cc I4_SIGN3_TEST tests I4_SIGN3.
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

      integer i4
      integer i4_sign3
      integer i4_test(test_num)
      integer s
      integer test

      save i4_test

      data i4_test / -10, -7, 0, 5, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_SIGN3_TEST'
      write ( *, '(a)' ) 
     &  '  I4_SIGN3 returns the three-way sign of a number.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    I4  I4_SIGN3(I4)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i4 = i4_test(test)
        s = i4_sign3 ( i4 )
        write ( *, '(i6,2x,i12)' ) i4, s
      end do

      return
      end
      subroutine i4_swap_test ( )

c*********************************************************************72
c
cc I4_SWAP_TEST tests I4_SWAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_SWAP_TEST'
      write ( *, '(a)' ) '  I4_SWAP swaps two I4''s.'

      i = 1
      j = 202

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Before swapping: '
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  I = ', i
      write ( *, '(a,i8)' ) '  J = ', j

      call i4_swap ( i, j )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  After swapping: '
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  I = ', i
      write ( *, '(a,i8)' ) '  J = ', j

      return
      end
      subroutine i4_to_halton_test ( )

c*********************************************************************72
c
c I4_TO_HALTON_TEST tests I4_TO_HALTON. 
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

      integer dim_num
      parameter ( dim_num = 3 )

      integer base(dim_num)
      integer i
      integer leap(dim_num)
      integer n
      double precision r(dim_num)
      integer seed(dim_num)
      integer step

      save base
      save leap
      
      data base / 2, 3, 5 /
      data leap / 1, 1, 1 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'I4_TO_HALTON_TEST'
      write ( *, '(a)' ) '  I4_TO_HALTON computes a Halton sequence.'
      write ( *, '(a)' ) '  The user specifies all data explicitly.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  In this test, we call I4_TO_HALTON repeatedly.'
      write ( *, '(a)' ) '  We use distinct primes as bases.'

      n = 10

      do i = 1, dim_num
        seed(i) = 0
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   I    R(0)      R(1)      R(2)'
      write ( *, '(a)' ) ''

      do step = 0, n

        call i4_to_halton ( dim_num, step, seed, leap, base, r );
        write ( *, '(2x,i2,3(2x,f8.4))' ) step, r(1:3)

      end do

      return
      end
      subroutine i4_to_isbn_test ( )

c*********************************************************************72
c
cc I4_TO_ISBN_TEST tests I4_TO_ISBN. 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4
      character i4_to_isbn
      character s1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'I4_TO_ISBN_TEST'
      write ( *, '(a)' ) 
     &  '  I4_TO_ISBN converts an I4 digit to an ISBN symbol.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  I4   S'
      write ( *, '(a)' ) ''

      do i4 = 0, 10

        s1 = i4_to_isbn ( i4 )
        write ( *, '(2x,i2,2x,a1)' ) i4, s1

      end do

      return
      end
      subroutine i4_to_pascal_test ( )

c*********************************************************************72
c
cc I4_TO_PASCAL_TEST tests I4_TO_PASCAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j
      integer k

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'I4_TO_PASCAL_TEST'
      write ( *, '(a)' ) '  I4_TO_PASCAL converts a linear index to'
      write ( *, '(a)' ) '  Pascal triangle indices.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     K  =>   I     J'
      write ( *, '(a)' ) ''

      do k = 1, 20

        call i4_to_pascal ( k, i, j )

        write ( *, '(2x,i4,4x,i4,2x,i4)' ) k, i, j

      end do

      return
      end
      subroutine i4_to_pascal_degree_test ( )

c*********************************************************************72
c
cc I4_TO_PASCAL_DEGREE_TEST tests I4_TO_PASCAL_DEGREE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer d
      integer k

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'I4_TO_PASCAL_DEGREE_TEST'
      write ( *, '(a)' ) '  I4_TO_PASCAL_DEGREE converts a linear index'
      write ( *, '(a)' ) 
     &  '  to the degree of the corresponding Pascal triangle indices.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     K  =>   D'
      write ( *, '(a)' ) ''

      do k = 1, 20

        call i4_to_pascal_degree ( k, d )

        write ( *, '(2x,i4,4x,i4)' ) k, d

      end do

      return
      end
      subroutine i4_to_triangle_lower_test ( )

c*********************************************************************72
c
cc I4_TO_TRIANGLE_LOWER_TEST tests I4_TO_TRIANGLE_LOWER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_TO_TRIANGLE_LOWER_TEST'
      write ( *, '(a)' ) '  I4_TO_TRIANGLE converts a linear index to a'
      write ( *, '(a)' ) '  lower triangular one.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     K  =>   I   J'
      write ( *, '(a)' ) ' '

      do k = 1, 20

        call i4_to_triangle_lower ( k, i, j )

        write ( *, '(2x,i4,4x,i4,i4)' ) k, i, j

      end do
     
      return
      end
      subroutine i4_uniform_ab_test ( )

c*********************************************************************72
c
cc I4_UNIFORM_AB_TEST tests I4_UNIFORM_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4_hi
      parameter ( i4_hi = 200 )
      integer i4_lo
      parameter ( i4_lo = -100 )
      integer i
      integer i4
      integer i4_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
      write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
      write ( *, '(a)' ) '  in an interval [A,B].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  The lower endpoint A = ', i4_lo
      write ( *, '(a,i12)' ) '  The upper endpoint B = ', i4_hi
      write ( *, '(a,i12)' ) '  The initial seed is ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 20

        i4 = i4_uniform_ab ( i4_lo, i4_hi, seed )

        write ( *, '(2x,i8,2x,i8)' ) i, i4

      end do

      return
      end
      subroutine i4_walsh_1d_test ( )

c*********************************************************************72
c
cc I4_WALSH_1D_TEST tests I4_WALSH_1D;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_walsh_1d
      integer w0
      integer wm1
      integer wm2
      integer wm3
      integer wp1
      integer wp2
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_WALSH_1D_TEST'
      write ( *, '(a)' ) '  I4_WALSH_1D evaluates 1D Walsh functions:'
      write ( *, '(a)' ) ' '
      write ( *, * ) 'X  W(+2) W(+1) W(0) W(-1) W(-2) W(-3)'
      write ( *, '(a)' ) ' '

      do i = 0, 32

        x = dble ( i ) / 4.0D+00

        wp2 = i4_walsh_1d ( x,  2 )
        wp1 = i4_walsh_1d ( x,  1 )
        w0  = i4_walsh_1d ( x,  0 )
        wm1 = i4_walsh_1d ( x, -1 )
        wm2 = i4_walsh_1d ( x, -2 )
        wm3 = i4_walsh_1d ( x, -3 )

        write ( *, '(2x,f10.6,6i2)' ) x, wp2, wp1, w0, wm1, wm2, wm3

      end do

      return
      end
      subroutine i4_width_test ( )

c*********************************************************************72
c
cc I4_WIDTH_TEST tests I4_WIDTH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 13 )

      integer i4_width
      integer test
      integer x
      integer x_test(test_num)

      save x_test

      data x_test /
     &  0, 1, 2, 3, 9, 10, 11, 99, 101, -1, -2, -3, -9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_WIDTH_TEST'
      write ( *, '(a)' ) 
     &  '  I4_WIDTH determines the printing "width" of an I4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '            I4      I4_WIDTH'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        x = x_test ( test )
        write ( *, '( 2x, i12, 2x, i12 )' ) x, i4_width ( x )
      end do

      return
      end
      subroutine i4_wrap_test ( )

c*********************************************************************72
c
cc I4_WRAP_TEST tests I4_WRAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_wrap
      integer ihi
      integer ilo

      ilo = 4
      ihi = 8

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_WRAP_TEST'
      write ( *, '(a)' )
     &  '  I4_WRAP forces an integer to lie within given limits.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  ILO = ', ilo
      write ( *, '(a,i8)' ) '  IHI = ', ihi
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  I4_WRAP(I)'
      write ( *, '(a)' ) ' '

      do i = -10, 20
        write ( *, '(2x,2i8)' ) i, i4_wrap ( i, ilo, ihi )
      end do

      return
      end
      subroutine i4_xor_test ( )

c*********************************************************************72
c
cc I4_XOR_TEST tests I4_XOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer i4_xor
      integer j
      integer k
      integer l
      integer seed
      integer test
      integer test_num

      i4_hi = 100
      i4_lo = 0
      seed = 123456789
      test_num = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_XOR_TEST'
      write ( *, '(a)' ) '  I4_XOR returns the bitwise exclusive OR of'
      write ( *, '(a)' ) '  two integers.'
      write ( *, '(a)' ) '  Compare with FORTRAN90 intrinsic IEOR.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I         J    I4_XOR      IEOR'
      write ( *, '(a)' ) ' '

      do test = 1, test_num
        i = i4_uniform_ab ( i4_lo, i4_hi, seed )
        j = i4_uniform_ab ( i4_lo, i4_hi, seed )
        k = i4_xor ( i, j )
        l = ieor ( i, j )
        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k,l
      end do

      return
      end
      subroutine i4block_print_test ( )

c*********************************************************************72
c
cc I4BLOCK_PRINT_TEST tests I4BLOCK_PRINT.
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
      integer x(l,m,n) 

      save x

      data x /
     &  1,  2,  3,   4,  1, 
     &  4,  9, 16,   1,  8, 
     & 27, 64,  2,   4,  6, 
     &  8,  2,  8,  18, 32, 
     &  2, 16, 54, 128 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4BLOCK_PRINT_TEST'
      write ( *, '(a)' ) '  I4BLOCK_PRINT prints an I4BLOCK.'

      call i4block_print ( l, m, n, x, '  The 3D array:' )

      return
      end
      subroutine i4col_find_item_test ( )

c*********************************************************************72
c
cc I4COL_FIND_ITEM_TEST tests I4COL_FIND_ITEM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer test_num

      parameter ( m = 5 )
      parameter ( n = 4 )
      parameter ( test_num = 3 )

      integer a(m,n)
      integer col
      integer i
      integer item
      integer item_test(test_num)
      integer j
      integer row
      integer test

      save item_test

      data item_test / 34, 12, 90 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4COL_FIND_ITEM_TEST'
      write ( *, '(a)' ) '  I4COL_FIND_ITEM finds the first occurrence'
      write ( *, '(a)' ) '  of an item in an integer array of columns.'

      do i = 1, m
        do j = 1, n
          a(i,j) = 10 * i + j
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix of columns:' )

      do test = 1, test_num

        item = item_test(test)

        call i4col_find_item ( m, n, a, item, row, col )

        write ( *, '(a,i8,a,i8,a,i8)' ) '  Item ', item,
     &    '  occurs in row ', row, ' and column ', col

      end do

      return
      end
      subroutine i4col_find_pair_swap_test ( )

c*********************************************************************72
c
cc I4COL_FIND_PAIR_SWAP_TEST tests I4COL_FIND_PAIR_WRAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 December 2005
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
      integer test_num
      parameter ( test_num = 5 )

      integer a(m,n)
      integer col
      integer i
      integer item1
      integer item1_test(test_num)
      integer item2
      integer item2_test(test_num)
      integer j
      integer row
      integer test

      save item1_test
      save item2_test

      data item1_test / 22, 32, 22, 54, 54 /
      data item2_test / 32, 22, 23, 14, 11 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4COL_FIND_PAIR_SWAP_TEST'
      write ( *, '(a)' )
     &  '  I4COL_FIND_PAIR_WRAP finds the first occurrence of'
      write ( *, '(a)' )
     &  '  a pair of item in an integer array of columns.'
      write ( *, '(a)' )
     &  '  Items in the array are ordered by column, and'
      write ( *, '(a)' ) '  wraparound is allowed.'

      do i = 1, m
        do j = 1, n
          a(i,j) = 10 * i + j
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix of columns:' )

      do test = 1, test_num

        item1 = item1_test(test)
        item2 = item2_test(test)

        call i4col_find_pair_wrap ( m, n, a, item1, item2, row, col )

        write ( *, '(a,i8,a,i8,a,i8,a,i8)' ) '  Item ', item1,
     &    ' followed by item ', item2, ' occurs in row ',
     &    row, ' and column ', col

      end do

      return
      end
      subroutine i4col_sort_a_test ( )

c*********************************************************************72
c
cc I4COL_SORT_A_TEST tests I4COL_SORT_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 December 2010
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

      integer a(m,n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST27'
      write ( *, '(a)' ) '  I4COL_SORT_A ascending sorts an integer'
      write ( *, '(a)' ) '  array as a table of columns.'

      b = 1
      c = 10
      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  The original matrix:' )

      call i4col_sort_a ( m, n, a )

      call i4mat_print ( m, n, a, '  Ascending sorted:' )

      return
      end
      subroutine i4col_sort_d_test ( )

c*********************************************************************72
c
cc I4COL_SORT_D_TEST tests I4COL_SORT_D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 December 2010
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

      integer a(m,n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4COL_SORT_D_TEST'
      write ( *, '(a)' ) '  I4COL_SORT_D descending sorts an integer'
      write ( *, '(a)' ) '  array as a table of columns.'

      b = 1
      c = 10
      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  The original matrix:' )

      call i4col_sort_d ( m, n, a )

      call i4mat_print ( m, n, a, '  Descending sorted:' )

      return
      end
      subroutine i4col_sort2_a_test ( )

c*********************************************************************72
c
cc I4COL_SORT2_A_TEST tests I4COL_SORT2_A;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2010
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

      integer a(m,n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4COL_SORT2_A_TEST'
      write ( *, '(a)' ) '  For a rectangular integer matrix:'
      write ( *, '(a)' )
     &  '  I4COL_SORT2_D sorts the elements of the columns.'

      b = 0
      c = 20
      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  The matrix:' )

      call i4col_sort2_a ( m, n, a )

      call i4mat_print ( m, n, a,
     &  '  The element-sorted column matrix:' )

      return
      end
      subroutine i4col_sorted_singleton_count_test ( )

c*********************************************************************72
c
cc I4COL_SORTED_SINGLETON_COUNT_TEST tests I4COL_SORTED_SINGLETON_COUNT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
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

      integer a(m,n)
      integer b
      integer c
      integer seed
      integer singleton_num
      integer test
      integer test_num
      parameter ( test_num = 2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4COL_SORTED_SINGLETON_COUNT_TEST'
      write ( *, '(a)' ) '  I4COL_SORTED_SINGLETON_COUNT counts'
      write ( *, '(a)' ) '  singletons in a sorted I4COL;'

      seed = 123456789

      do test = 1, test_num

        b = 0
        c = 3

        call i4mat_uniform_ab ( m, n, b, c, seed, a )

        call i4col_sort_a ( m, n, a )

        call i4mat_print ( m, n, a, '  Ascending sorted ICOL:' )

        call i4col_sorted_singleton_count ( m, n, a, singleton_num )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a)' ) 
     &    '  Number of singletons = ', singleton_num

      end do

      return
      end
      subroutine i4col_sorted_unique_count_test ( )

c*********************************************************************72
c
cc I4COL_SORTED_UNIQUE_COUNT_TEST tests I4COL_SORTED_UNIQUE_COUNT;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
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

      integer a(m,n)
      integer b
      integer c
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 2 )
      integer unique_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4COL_SORTED_UNIQUE_COUNT_TEST'
      write ( *, '(a)' ) '  I4COL_SORTED_UNIQUE_COUNT counts the unique'
      write ( *, '(a)' ) '  entries of a sorted I4COL;'

      seed = 123456789

      do test = 1, test_num

        b = 0
        c = 3

        call i4mat_uniform_ab ( m, n, b, c, seed, a )

        call i4col_sort_a ( m, n, a )

        call i4mat_print ( m, n, a, '  Ascending sorted I4COL:' )

        call i4col_sorted_unique_count ( m, n, a, unique_num )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a)' ) 
     &    '  Number of unique entries = ', unique_num

      end do

      return
      end
      subroutine i4mat_elim_test ( )

c*********************************************************************72
c
cc I4MAT_ELIM_TEST tests I4MAT_ELIM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 July 2014
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

      integer a(m,n)
      integer col(n)
      integer factor
      integer i
      integer j
      integer k
      integer row(m)
      integer test
      integer test_num
      parameter ( test_num = 3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_ELIM_TEST'
      write ( *, '(a)' ) '  I4MAT_ELIM does exact Gauss elimination.'

      do test = 1, test_num

        if ( test .eq. 1 ) then

          k = 0
          do i = 1, m
            do j = 1, n
              k = k + 1
              a(i,j) = k
            end do
          end do

        else if ( test .eq. 2 ) then

          factor = 8 * 7 * 6 * 5 * 4 * 3 * 2

          do i = 1, m
            do j = 1, n
              a(i,j) = factor / ( i + j - 1 )
            end do
          end do

        else if ( test .eq. 3 ) then

          do i = 1, m
            do j = 1, n
              a(i,j) = i * j
            end do
          end do

        end if

        call i4mat_print ( m, n, a, '  The original matrix:' )

        call i4mat_red ( m, n, a, row, col )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The matrix, as returned by I4MAT_RED:'
        write ( *, '(a)' ) 
     &  '  (Factors are displayed in an extra row and column.'
        write ( *, '(a)' ) ' '
        do i = 1, m
          write ( *, '(2x,6i8)' )  ( a(i,j), j = 1, n ), row(i)
        end do
        write ( *, '(2x,5i8)' )  ( col(j), j = 1, n )

        call i4mat_elim ( m, n, a )

        call i4mat_print ( m, n, a, 
     &    '  The matrix returned by I4MAT_ELIM:' )

      end do

      return
      end
      subroutine i4mat_indicator_test ( )

c*********************************************************************72
c
cc I4MAT_INDICATOR_TEST tests I4MAT_INDICATOR.
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

      integer a(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_INDICATOR_TEST'
      write ( *, '(a)' ) 
     &  '  I4MAT_INDICATOR returns an indicator matrix.'

      call i4mat_indicator ( m, n, a )

      call i4mat_print ( m, n, a, '  The indicator matrix:' )

      return
      end
      subroutine i4mat_l1_inverse_test ( )

c*********************************************************************72
c
cc I4MAT_L1_INVERSE_TEST tests I4MAT_L1_INVERSE.
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

      integer n
      parameter ( n = 6 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      integer a(n,n)
      integer b(n,n)
      integer c(n,n)

      save a

      data a /
     &   1,  2,  0,  5,  0, 75, 
     &   0,  1,  0,  0,  0,  0, 
     &   0,  0,  1,  3,  0,  0, 
     &   0,  0,  0,  1,  0,  6, 
     &   0,  0,  0,  0,  1,  4, 
     &   0,  0,  0,  0,  0,  1 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_L1_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  I4MAT_L1_INVERSE inverts a unit lower triangular I4MAT.'

      call i4mat_print ( n, n, a, '  The original matrix:' )

      call i4mat_l1_inverse ( n, a, b )

      call i4mat_print ( n, n, b, '  The inverse matrix:' )

      call i4mat_mm ( n, n, n, a, b, c )
 
      call i4mat_print ( n, n, c, '  The product:' )

      return
      end
      subroutine i4mat_max_test ( )

c*********************************************************************72
c
cc I4MAT_MAX_TEST tests I4MAT_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 7 )

      integer a(m,n)
      integer b
      integer c
      integer i
      integer i4mat_max
      integer j
      integer seed

      b = 0
      c = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_MAX_TEST'
      write ( *, '(a)' ) '  I4MAT_MAX returns the maximum of an I4MAT.'

      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  Random array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  Maximum value = ', i4mat_max ( m, n, a )

      return
      end
      subroutine i4mat_max_index_test ( )

c*********************************************************************72
c
cc I4MAT_MAX_INDEX_TEST tests I4MAT_MAX_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 7 )

      integer a(m,n)
      integer b
      integer c
      integer i
      integer j
      integer seed

      b = 0
      c = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_MAX_INDEX_TEST'
      write ( *, '(a)' ) '  I4MAT_MAX_INDEX locates the maximum;'

      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  Random array:' )

      write ( *, '(a)' ) ' '
      call i4mat_max_index ( m, n, a, i, j )
      write ( *, '(a,2i8)' ) 
     &  '  Maximum I,J indices            ', i, j

      return
      end
      subroutine i4mat_min_test ( )

c*********************************************************************72
c
cc I4MAT_MIN_TEST tests I4MAT_MIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 7 )

      integer a(m,n)
      integer b
      integer c
      integer i
      integer i4mat_min
      integer j
      integer seed

      b = 0
      c = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_MIN_TEST'
      write ( *, '(a)' ) '  I4MAT_MIN returns the minimum;'

      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  Random array:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  Minimum value = ', i4mat_min ( m, n, a )

      return
      end
      subroutine i4mat_min_index_test ( )

c*********************************************************************72
c
cc I4MAT_MIN_INDEX_TEST tests I4MAT_MIN_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 7 )

      integer a(m,n)
      integer b
      integer c
      integer i
      integer j
      integer seed

      b = 0
      c = 10

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_MIN_INDEX_TEST'
      write ( *, '(a)' ) '  I4MAT_MIN_INDEX locates the minimum;'

      seed = 123456789

      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  Random array:' )

      write ( *, '(a)' ) ' '
      call i4mat_min_index ( m, n, a, i, j )
      write ( *, '(a,2i8)' ) 
     &  '  Minimum I,J indices            ', i, j

      return
      end
      subroutine i4mat_perm_uniform_test ( )

c*********************************************************************72
c
cc I4MAT_PERM_UNIFORM_TEST tests I4MAT_PERM_UNIFORM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer a(n,n)
      integer i
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_PERM_UNIFORM_TEST'
      write ( *, '(a)' ) '  I4MAT_PERM_UNIFORM applies a random'
      write ( *, '(a)' ) '  permutation to a square integer matrix.'

      seed = 123456789

      do i = 1, n
        do j = 1, n
          a(i,j) = 10 * i + j
        end do
      end do

      call i4mat_print ( n, n, a, '  The original matrix:' )

      call i4mat_perm_uniform ( n, a, seed )

      call i4mat_print ( n, n, a, '  The permuted matrix:' )

      return
      end
      subroutine i4mat_red_test ( )

c*********************************************************************72
c
cc I4MAT_RED_TEST tests I4MAT_RED.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 July 2014
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

      integer a(m,n)
      integer col(n)
      integer factor
      integer i
      integer j
      integer k
      integer row(m)
      integer test
      integer test_num
      parameter ( test_num = 3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_RED_TEST'
      write ( *, '(a)' ) 
     &  '  I4MAT_RED divides common factors in a matrix;'

      do test = 1, test_num

        if ( test .eq. 1 ) then

          k = 0
          do i = 1, m
            do j = 1, n
              k = k + 1
              a(i,j) = k
            end do
          end do

        else if ( test .eq. 2 ) then

          factor = 8 * 7 * 6 * 5 * 4 * 3 * 2

          do i = 1, m
            do j = 1, n
              a(i,j) = factor / ( i + j - 1 )
            end do
          end do

        else if ( test .eq. 3 ) then

          do i = 1, m
            do j = 1, n
              a(i,j) = i * j
            end do
          end do

        end if

        call i4mat_print ( m, n, a, '  The original matrix:' )

        call i4mat_red ( m, n, a, row, col )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The matrix, as returned by I4MAT_RED:'
        write ( *, '(a)' ) 
     &  '  (Factors are displayed in an extra row and column.'
        write ( *, '(a)' ) ' '
        do i = 1, m
          write ( *, '(2x,6i8)' )  ( a(i,j), j = 1, n ), row(i)
        end do
        write ( *, '(2x,5i8)' )  ( col(j), j = 1, n )

      end do

      return
      end
      subroutine i4mat_u1_inverse_test ( )

c*********************************************************************72
c
cc I4MAT_U1_INVERSE_TEST tests I4MAT_U1_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 September 2014
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
      integer a(n,n)
      integer b(n,n)
      integer c(n,n)

      save a

      data a /
     &  1,  0,  0,  0,  0,  0, 
     &  2,  1,  0,  0,  0,  0, 
     &  0,  0,  1,  0,  0,  0, 
     &  5,  0,  3,  1,  0,  0, 
     &  0,  0,  0,  0,  1,  0, 
     & 75,  0,  0,  6,  4,  1 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4MAT_U1_INVERSE_TEST'
      write ( *, '(a)' ) 
     &  '  I4MAT_U1_INVERSE inverts a unit upper triangular matrix.'

      call i4mat_print ( n, n, a, '  The original matrix:' )

      call i4mat_u1_inverse ( n, a, b )

      call i4mat_print ( n, n, b, '  The inverse matrix:' )

      call i4mat_mm ( n, n, n, a, b, c )

      call i4mat_print ( n, n, c, '  The product:' )

      return
      end
      subroutine i4row_max_test ( )

c*********************************************************************72
c
cc I4ROW_MAX_TEST tests I4ROW_MAX;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
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

      integer a(m,n)
      integer amax(m)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_MAX_TEST'
      write ( *, '(a)' ) '  I4ROW_MAX computes row maximums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix:' )

      call i4row_max ( m, n, a, amax )

      call i4vec_print ( m, amax, '  The row maximums:' )

      return
      end
      subroutine i4row_mean_test ( )

c*********************************************************************72
c
cc I4ROW_MEAN_TEST tests I4ROW_MEAN.
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

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      integer a(m,n)
      integer i
      integer j
      integer k
      double precision mean(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_MEAN_TEST'
      write ( *, '(a)' ) '  I4ROW_MEAN computes row means;'
 
      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix:' )

      call i4row_mean ( m, n, a, mean )

      call r8vec_print ( m, mean, '  The row means:' )

      return
      end
      subroutine i4row_min_test ( )

c*********************************************************************72
c
cc I4ROW_MIN_TEST tests I4ROW_MIN;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
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

      integer a(m,n)
      integer amin(m)
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_MIN_TEST'
      write ( *, '(a)' ) '  I4ROW_MIN computes row minimums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix:' )

      call i4row_min ( m, n, a, amin )

      call i4vec_print ( m, amin, '  The row minimums:' )

      return
      end
      subroutine i4row_sort_a_test ( )

c*********************************************************************72
c
cc I4ROW_SORT_A_TEST tests I4ROW_SORT_A;
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

      integer m
      parameter ( m = 10 )
      integer n
      parameter ( n = 4 )

      integer a(m,n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_SORT_A_TEST'
      write ( *, '(a)' ) '  For a rectangular integer matrix:'
      write ( *, '(a)' ) '  I4ROW_SORT_A sorts the rows;'

      b = 0
      c = 10
      seed = 123456789
      call i4mat_uniform_ab ( m, n, b, c, seed, a )

      call i4mat_print ( m, n, a, '  The original matrix:' )

      call i4row_sort_a ( m, n, a )

      call i4mat_print ( m, n, a, '  The row-sorted matrix:' )

      return
      end
      subroutine i4row_sort_d_test ( )

c*********************************************************************72
c
cc I4ROW_SORT_D_TEST tests I4ROW_SORT_D.
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

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 4 )

      integer a(m,n)
      integer i
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_SORT_D_TEST'
      write ( *, '(a)' ) '  For a rectangular integer matrix:'
      write ( *, '(a)' ) '  I4ROW_SORT_D sorts the rows;'

      seed = 123456789

      do i = 1, m
        do j = 1, n
          a(i,j) = 10 * i + j
        end do
      end do

      call i4mat_print ( m, n, a, '  The original matrix:' )

      call i4mat_perm2_uniform ( m, n, a, seed )

      call i4mat_print ( m, n, a, 
     &  '  The matrix, permuted by I4MAT_PERM2_UNIFORM:' )

      call i4row_sort_d ( m, n, a )

      return
      end
      subroutine i4row_sort2_d_test ( )

c*********************************************************************72
c
cc I4ROW_SORT2_D_TEST tests I4ROW_SORT2_D;
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

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 4 )

      integer a(m,n)
      integer i
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_SORT2_D_TEST'
      write ( *, '(a)' ) '  For a rectangular integer matrix:'
      write ( *, '(a)' ) 
     &  '  I4ROW_SORT2_D sorts the elements of the rows.'

      seed = 123456789

      do i = 1, m
        do j = 1, n
          a(i,j) = 10 * i + j
        end do
      end do

      call i4mat_print ( m, n, a, '  The original matrix:' )

      call i4mat_perm2_uniform ( m, n, a, seed )

      call i4mat_print ( m, n, a, 
     &  '  The matrix, permuted by I4MAT_PERM2_UNIFORM:' )

      call i4row_sort2_d ( m, n, a )

      call i4mat_print ( m, n, a, 
     &  '  The element-sorted row-sorted matrix:' )

      return
      end
      subroutine i4row_sum_test ( )

c*********************************************************************72
c
cc I4ROW_SUM_TEST tests I4ROW_SUM;
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

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4 )

      integer a(m,n)
      integer i
      integer j
      integer k
      integer rowsum(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_SUM_TEST'
      write ( *, '(a)' ) '  I4ROW_SUM computes row sums;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix:' )

      call i4row_sum ( m, n, a, rowsum )

      call i4vec_print ( m, rowsum, '  The row sums:' )

      return
      end
      subroutine i4row_swap_test ( )

c*********************************************************************72
c
cc I4ROW_SWAP_TEST tests I4ROW_SWAP;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2010
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

      integer a(m,n)
      integer i
      integer j
      integer k
      integer row1
      integer row2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_SWAP_TEST'
      write ( *, '(a)' ) '  For an integer matrix of rows,'
      write ( *, '(a)' ) '  I4ROW_SWAP swaps two rows;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix:' )

      row1 = 1
      row2 = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a,i8)' ) '  Swap rows ', row1, ' and ', row2
      write ( *, '(a)' ) ' '

      call i4row_swap ( m, n, a, row1, row2 )

      call i4mat_print ( m, n, a, '  The new matrix:' )

      return
      end
      subroutine i4row_variance_test ( )

c*********************************************************************72
c
cc I4ROW_VARIANCE_TEST tests I4ROW_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 Ostober 2014
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

      integer a(m,n)
      integer i
      integer j
      integer k
      double precision variance(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4ROW_VARIANCE_TEST'
      write ( *, '(a)' ) '  I4ROW_VARIANCE computes row variances;'

      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = k
        end do
      end do

      call i4mat_print ( m, n, a, '  The matrix:' )

      call i4row_variance ( m, n, a, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Row variances:'
      write ( *, '(a)' ) ' '
      do i = 1, m
        write ( *, '(2x,i3,2x,f10.4)' ) i, variance(i)
      end do

      return
      end
      subroutine i4vec_add_test ( )

c*********************************************************************72
c
cc I4VEC_ADD_TEST tests I4VEC_ADD;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer b(n)
      integer c(n)
      integer hi
      integer i
      integer lo
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_ADD_TEST'
      write ( *, '(a)' ) '  I4VEC_ADD adds two I4VEC''s'

      seed = 123456789

      lo = - n
      hi = n

      call i4vec_uniform_ab ( n, lo, hi, seed, a )
      call i4vec_uniform_ab ( n, lo, hi, seed, b )
      call i4vec_add ( n, a, b, c )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I     A     B     C'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(4i6)' ) i, a(i), b(i), c(i)
      end do

      return
      end
      subroutine i4vec_amax_test ( )

c*********************************************************************72
c
cc I4VEC_AMAX_TEST tests I4VEC_AMAX;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_AMAX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_AMAX:   maximum absolute entry;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_amax ( n, a, aval )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Maximum absolute value: ', aval

      return
      end
      subroutine i4vec_amax_index_test ( )

c*********************************************************************72
c
cc I4VEC_AMAX_INDEX_TEST tests I4VEC_AMAX_INDEX;
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

      integern
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_AMAX_INDEX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) 
     &  '  I4VEC_AMAX_INDEX:  index of maximum absolute entry;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call i4vec_amax_index ( n, a, ival )

      write ( *, '(a,i8)' ) '  Maximum abs index:        ', ival

      return
      end
      subroutine i4vec_amin_test ( )

c*********************************************************************72
c
cc I4VEC_AMIN_TEST tests I4VEC_AMIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_AMIN_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_AMIN:   minimum absolute entry;'
      write ( *, '(a)' ) ' '

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call i4vec_amin ( n, a, aval )

      write ( *, '(a,i8)' ) '  Minimum absolute value: ', aval

      return
      end
      subroutine i4vec_amin_index_test ( )

c*********************************************************************72
c
cc I4VEC_AMIN_INDEX_TEST tests I4VEC_AMIN_INDEX;
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

      integern
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_AMIN_INDEX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) 
     &  '  I4VEC_AMIN_INDEX:  index minimum absolute entry;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call i4vec_amin_index ( n, a, ival )

      write ( *, '(a,i8)' ) '  Minimum abs index:      ', ival

      return
      end
      subroutine i4vec_aminz_test ( )

c*********************************************************************72
c
cc I4VEC_AMINZ_TEST tests I4VEC_AMINZ.
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

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_AMINZ_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) 
     &  '  I4VEC_AMINZ:  minimum nonzero absolute entry;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call i4vec_aminz ( n, a, aval )
 
      write ( *, '(a,i8)' ) '  Minimum abs nonzero:      ', aval

      return
      end
      subroutine i4vec_aminz_index_test ( )

c*********************************************************************72
c
cc I4VEC_AMINZ_INDEX_TEST tests I4VEC_AMINZ_INDEX;
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

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer ival
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_AMINZ_INDEX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) 
     &  '  I4VEC_AMINZ_INDEX: index of minimum nonzero absolute entry;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      call i4vec_aminz_index ( n, a, ival )

      write ( *, '(a,i8)' ) '  Minimum abs nonzero index:', ival

      return
      end
      subroutine i4vec_ascend_sub_test ( )

c*********************************************************************72
c
cc I4VEC_ASCEND_SUB_TEST tests I4VEC_ASCEND_SUB
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
      parameter ( n = 14 )

      integer a(n)
      integer b
      integer c
      integer length
      integer seed
      integer sub(n)
      integer test
      integer test_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_ASCEND_SUB'
      write ( *, '(a)' ) '  I4VEC_ASCEND_SUB computes a longest'
      write ( *, '(a)' ) '  ascending subsequence of an I4VEC.'

      test_num = 6

      b = 1
      c = 10
      seed = 123456789

      do test = 1, test_num
        call i4vec_uniform_ab ( n, b, c, seed, a )
        call i4vec_print ( n, a, '  The vector to be tested:' )
        call i4vec_ascend_sub ( n, a, length, sub )
        call i4vec_print ( length, sub, 
     &    '  A longest ascending subsequence:' )
      end do

      return
      end
      subroutine i4vec_ascends_test ( )

c*********************************************************************72
c
cc I4VEC_ASCENDS_TEST tests I4VEC_ASCENDS.
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
      parameter ( n = 4 )
      integer test_num
      parameter ( test_num = 6 )

      integer i
      logical i4vec_ascends
      integer test
      integer x(n)
      integer x_test(n,test_num)

      save x_test

      data x_test /
     &  1, 3, 2, 4, 
     &  2, 2, 2, 2, 
     &  1, 2, 2, 4, 
     &  1, 2, 3, 4, 
     &  4, 4, 3, 1, 
     &  9, 7, 3, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_ASCENDS_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_ASCENDS determines if an I4VEC ascends.'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        do i = 1, n
          x(i) = x_test(i,test)
        end do

        call i4vec_print ( n, x, '  Test vector:' )

        write ( *, '(a,l1)' ) '  I4VEC_ASCENDS =  ', 
     &    i4vec_ascends ( n, x )

      end do

      return
      end
      subroutine i4vec_bracket_test ( )

c*********************************************************************72
c
cc I4VEC_BRACKET_TEST tests I4VEC_BRACKET.
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

      integer n_max
      parameter ( n_max = 20 )
      integer test_num
      parameter ( test_num = 6 )

      integer a(n_max)
      integer atest(test_num)
      integer aval
      integer i
      integer left
      integer n
      integer right
      integer test

      save atest

      data atest /
     &  -10, 2, 9, 10, 20, 24 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_BRACKET_TEST'
      write ( *, '(a)' ) '  I4VEC_BRACKET finds a pair of entries in a'
      write ( *, '(a)' ) '  sorted I4VEC which bracket a value.'

      n = 10
      do i = 1, n
        a(i) = 2 * i
      end do
      a(6) = a(5)

      call i4vec_print ( n, a, '  Sorted array:' )

      do test = 1, test_num

        aval = atest(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Search for AVAL = ', aval

        call i4vec_bracket ( n, a, aval, left, right )

        write ( *, '(a,i8)' ) '  Left = ', left
        write ( *, '(a,i8)' ) '  Right = ', right

        if ( 1 .le. left ) then
          write ( *, '(a,i8)' ) '  A(LEFT)=', a(left)
        end if

        if ( 1 .le. right ) then
          write ( *, '(a,i8)' ) '  A(RIGHT) = ', a(right)
        end if

      end do

      return
      end
      subroutine i4vec_concatenate_test ( )

c*********************************************************************72
c
cc I4VEC_CONCATENATE_TEST tests I4VEC_CONCATENATE.
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

      integer a1(n1)
      integer a2(n2)
      integer a3(n3)

      save a1
      save a2

      data a1 /
     &  91, 31, 71, 51, 31 /
      data a2 /
     &  42, 22, 12 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_CONCATENATE_TEST'
      write ( *, '(a)' ) '  I4VEC_CONCATENATE concatenates two I4VECs'

      call i4vec_print ( n1, a1, '  Array 1:' )
      call i4vec_print ( n2, a2, '  Array 2:' )
      call i4vec_concatenate ( n1, a1, n2, a2, a3 )
      call i4vec_print ( n3, a3, '  Array 3 = Array 1 + Array 2:' )

      return
      end
      subroutine i4vec_cum_test ( )

c*********************************************************************72
c
cc I4VEC_CUM_TEST tests I4VEC_CUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer a_cum(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_CUM_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_CUM:   cumulative sum;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_cum ( n, a, a_cum )

      call i4vec_print ( n, a_cum, '  Cumulative sums:' )

      return
      end
      subroutine i4vec_cum0_test ( )

c*********************************************************************72
c
cc I4VEC_CUM0_TEST tests I4VEC_CUM0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer a_cum0(0:n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_CUM0_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_CUM0:  cumulative sum, zero based;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_cum0 ( n, a, a_cum0 )

      call i4vec_print ( n + 1, a_cum0, '  0-based Cumulative sums:' )

      return
      end
      subroutine i4vec_decrement_test ( )

c*********************************************************************72
c
cc I4VEC_DECREMENT_TEST tests I4VEC_DECREMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      integer seed
      integer v(n)
      integer v_hi
      integer v_lo

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_DECREMENT_TEST'
      write ( *, '(a)' ) '  I4VEC_DECREMENT decrements an I4VEC.'

      v_lo = -5
      v_hi = 10
      seed = 123456789
      call i4vec_uniform_ab ( n, v_lo, v_hi, seed, v )
      call i4vec_print ( n, v, '  The I4VEC:' )
      call i4vec_decrement ( n, v )
      call i4vec_print ( n, v, '  The I4VEC after decrementing:' )

      return
      end
      subroutine i4vec_descends_test ( )

c*********************************************************************72
c
cc I4VEC_DESCENDS_TEST tests I4VEC_DESCENDS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
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

      logical i4vec_descends
      integer test
      integer x(n)
      integer x_test(n,test_num)

      save x_test
c
c  Each ROW of this definition is a COLUMN of the matrix.
c
      data x_test /
     &  1, 3, 2, 4, 
     &  2, 2, 2, 2, 
     &  1, 2, 2, 4, 
     &  1, 2, 3, 4, 
     &  4, 4, 3, 1, 
     &  9, 7, 3, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_DESCENDS_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_DESCENDS determines if an I4VEC descends.'

      do test = 1, test_num

        x(1:n) = x_test(1:n,test)

        call i4vec_print ( n, x, '  Test vector:' )

        write ( *, '(a,l1)' ) '  I4VEC_DESCENDS = ', 
     &    i4vec_descends ( n, x )

      end do

      return
      end
      subroutine i4vec_direct_product_test ( )

c*********************************************************************72
c
cc I4VEC_DIRECT_PRODUCT_TEST tests I4VEC_DIRECT_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2014
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
      integer factor_value(factor_order_max)
      integer i
      integer j
      integer x(factor_num,point_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_DIRECT_PRODUCT_TEST'
      write ( *, '(a)' ) '  I4VEC_DIRECT_PRODUCT forms the entries of a'
      write ( *, '(a)' ) 
     &  '  direct product of a given number of I4VEC factors.'

      do j = 1, point_num
        do i = 1, factor_num
          x(i,j) = 0
        end do
      end do

      do factor_index = 1, factor_num

        if ( factor_index .eq. 1 ) then
          factor_order = 4
          factor_value(1) = 1
          factor_value(2) = 2
          factor_value(3) = 3
          factor_value(4) = 4
        else if ( factor_index .eq. 2 ) then
          factor_order = 3
          factor_value(1) = 50
          factor_value(2) = 60
          factor_value(3) = 70
        else if ( factor_index .eq. 3 ) then
          factor_order = 2
          factor_value(1) = 800
          factor_value(2) = 900
        end if

        call i4vec_direct_product ( factor_index, factor_order, 
     &    factor_value,  factor_num, point_num, x )

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     J     X(1)  X(2)  X(3)'
      write ( *, '(a)' ) ' '

      do j = 1, point_num
        write ( *, '(2x,i4,4x,i4,2x,i4,2x,i4)' ) j, x(1:factor_num,j)
      end do

      return
      end
      subroutine i4vec_direct_product2_test ( )

c*********************************************************************72
c
cc I4VEC_DIRECT_PRODUCT2_TEST tests I4VEC_DIRECT_PRODUCT2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2014
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
      integer factor_value(factor_order_max)
      integer i
      integer j
      integer w(point_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_DIRECT_PRODUCT2_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_DIRECT_PRODUCT2 forms the entries of a'
      write ( *, '(a)' ) 
     &  '  direct product of a given number of I4VEC factors.'

      do j = 1, point_num
        w(j) = 1
      end do

      do factor_index = 1, factor_num

        if ( factor_index .eq. 1 ) then
          factor_order = 4
          factor_value(1) = 2
          factor_value(2) = 3
          factor_value(3) = 5
          factor_value(4) = 7
        else if ( factor_index .eq. 2 ) then
          factor_order = 3
          factor_value(1) = 11
          factor_value(2) = 13
          factor_value(3) = 17
        else if ( factor_index .eq. 3 ) then
          factor_order = 2
          factor_value(1) = 19
          factor_value(2) = 21
        end if

        call i4vec_direct_product2 ( factor_index, factor_order, 
     &    factor_value, factor_num, point_num, w )

      end do

      call i4vec_print ( point_num, w, '  Product W:' )

      return
      end
      subroutine i4vec_frac_test ( )

c*********************************************************************72
c
cc I4VEC_FRAC_TEST tests I4VEC_FRAC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer afrac
      integer b
      integer c
      integer k
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_FRAC_TEST'
      write ( *, '(a)' ) '  I4VEC_FRAC: K-th smallest entry.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      b = 1
      c = 2 * n
      seed = 123456789
      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  The array to search:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Fractile    Value'
      write ( *, '(a)' ) ' '

      do k = 1, n, n / 2

        call i4vec_frac ( n, a, k, afrac )

        write ( *, '(2x,2i8)' ) k, afrac

      end do

      return
      end
      subroutine i4vec_heap_a_test ( )

c*********************************************************************72
c
cc I4VEC_HEAP_A_TEST tests I4VEC_HEAP_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_HEAP_A_TEST'
      write ( *, '(a)' ) '  For an I4VEC,'
      write ( *, '(a)' ) '  I4VEC_HEAP_A: ascending heap form.'

      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      b = 0
      c = n
      seed = 123456789
      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )

      call i4vec_heap_a ( n, a )

      call i4vec_print ( n, a, '  Ascending heap form:' )
   
      return
      end
      subroutine i4vec_heap_d_test ( )

c*********************************************************************72
c
cc I4VEC_HEAP_D_TEST tests I4VEC_HEAP_D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_HEAP_D_TEST'
      write ( *, '(a)' ) '  For an I4VEC,'
      write ( *, '(a)' ) '  I4VEC_HEAP_D: descending heap form.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      b = 0
      c = n
      seed = 123456789
      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )
  
      call i4vec_heap_d ( n, a )

      call i4vec_print ( n, a, '  Descending heap form:' )

      return
      end
      subroutine i4vec_histogram_test ( )

c*********************************************************************72
c
cc I4VEC_HISTOGRAM_TEST tests I4VEC_HISTOGRAM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      integer a(n)
      integer histo_gram(0:21)
      integer histo_num
      integer i
      integer seed 

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_HISTOGRAM_TEST'
      write ( *, '(a)' ) '  I4VEC_HISTOGRAM histograms an I4VEC.'

      seed = 123456789
      call i4vec_uniform_ab ( n, 0, 25, seed, a )

      histo_num = 20

      call i4vec_histogram ( n, a, histo_num, histo_gram )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Histogram of data from 0 to ', histo_num
      write ( *, '(a)' ) ' '

      do i = 0, histo_num
        if ( 0 .lt. histo_gram(i) ) then
          write ( *, '(2x,i8,2x,i8)' ) i, histo_gram(i)
        end if
      end do

      return
      end
      subroutine i4vec_increment_test ( )

c*********************************************************************72
c
cc I4VEC_INCREMENT_TEST tests I4VEC_INCREMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      integer seed
      integer v(n)
      integer v_hi
      integer v_lo

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INCREMENT_TEST'
      write ( *, '(a)' ) '  I4VEC_INCREMENT increments an I4VEC.'

      v_lo = -5
      v_hi = 10
      seed = 123456789
      call i4vec_uniform_ab ( n, v_lo, v_hi, seed, v )
      call i4vec_print ( n, v, '  The I4VEC:' )
      call i4vec_increment ( n, v )
      call i4vec_print ( n, v, '  The I4VEC after incrementing:' )

      return
      end
      subroutine i4vec_index_test ( )

c*********************************************************************72
c
cc I4VEC_INDEX_TEST tests I4VEC_INDEX.
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
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer i4vec_index
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) 
     &  '  I4VEC_INDEX:              first index of given value;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      write ( *, '(a)' ) ' '

      aval = a(n/2)
      write ( *, '(a)' ) ' '
      j = i4vec_index ( n, a, aval )
      write ( *, '(a,i8,a,i8)' ) 
     &  '  Index of first occurrence of ', aval, ' is ', j

      aval = aval + 1
      j = i4vec_index ( n, a, aval )
      write ( *, '(a,i8,a,i8)' ) 
     &  '  Index of first occurrence of ', aval, ' is ', j

      return
      end
      subroutine i4vec_index_insert_unique_test ( )

c*********************************************************************72
c
cc I4VEC_INDEX_INSERT_UNIQUE_TEST tests I4VEC_INDEX_INSERT_UNIQUE.
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

      integer n_max
      parameter ( n_max = 20 )

      integer b
      integer c
      integer i
      integer i4_uniform_ab
      integer indx(n_max)
      integer n
      integer seed
      integer x(n_max)
      integer xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEX_INSERT_UNIQUE_TEST'
      write ( *, '(a)' ) '  I4VEC_INDEX_INSERT_UNIQUE inserts unique'
      write ( *, '(a)' ) '  values into an index sorted array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Generate some random values:'
      write ( *, '(a)' ) ' '

      b = 0
      c = 20
      seed = 123456789

      do i = 1, 20
        xval = i4_uniform_ab ( b, c, seed )
        write ( *, '(4x,i3)' ) xval
        call i4vec_index_insert_unique ( n, x, indx, xval )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of unique entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      return
      end
      subroutine i4vec_index_order_test ( )

c*********************************************************************72
c
cc I4VEC_INDEX_ORDER_TEST tests I4VEC_INDEX_ORDER.
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

      integer n_max
      parameter ( n_max = 20 )

      integer b
      integer c
      integer i
      integer i4_uniform_ab
      integer indx(n_max)
      integer n
      integer seed
      integer x(n_max)
      integer xval

      n = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEX_ORDER_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_INDEX_ORDER sorts an index sorted array.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Generate some random values:'
      write ( *, '(a)' ) ' '

      b = 0
      c = 20
      seed = 123456789

      do i = 1, 20
        xval = i4_uniform_ab ( b, c, seed )
        write ( *, '(4x,i3)' ) xval
        call i4vec_index_insert_unique ( n, x, indx, xval )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Indexed list of unique entries:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  INDX(I)  X(I)  X(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i3,6x,i3,3x,i3,9x,i3)' ) 
     &    i, indx(i), x(i), x(indx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Now call I4VEC_INDEX_ORDER to carry out the sorting:'

      call i4vec_index_order ( n, x, indx )

      call i4vec_print ( n, x, '  X:' )

      return
      end
      subroutine i4vec_indexed_heap_d_test ( )

c*********************************************************************72
c
cc I4VEC_INDEXED_HEAP_D_TEST tests I4VEC_INDEXED_HEAP_D;
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

      integer a(m)
      integer i
      integer indx(n)

      save a
      save indx

      data a /
     &  101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
     &  111, 112, 113, 114, 115, 116, 117, 118, 119, 120 /
      data indx /
     &  1, 11, 17, 5, 7, 13, 15, 3, 19, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_TEST'
      write ( *, '(a)' ) '  I4VEC_INDEXED_HEAP_D creates a descending'
      write ( *, '(a)' ) '  heap from an indexed vector.'
c
c  Print before.
c
      call i4vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Heap the data.
c
      call i4vec_indexed_heap_d ( n, a, indx )
c
c  Print afterwards.  Only INDX should change.
c
      call i4vec_print ( m, a,
     &  '  The data vector (should NOT change):' )
      call i4vec_print ( n, indx, '  The index vector (may change):' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) is now a descending heap:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do

      return
      end
      subroutine i4vec_indexed_heap_d_extract_test ( )

c*********************************************************************72
c
cc I4VEC_INDEXED_HEAP_D_EXTRACT_TEST tests I4VEC_INDEXED_HEAP_D_EXTRACT.
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

      integer a(m)
      integer i
      integer indx(n_max)
      integer indx_extract
      integer indx_insert
      integer indx_max
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_EXTRACT_TEST'
      write ( *, '(a)' ) '  For an indexed I4VEC,'
      write ( *, '(a)' )
     &  '  I4VEC_INDEXED_HEAP_D_EXTRACT extracts the maximum value;'
c
c  Set the data array.  To keep things easy, we will use the indicator vector.
c
      call i4vec_indicator1 ( m, a )
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

      call i4vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Create a descending heap from the indexed array.
c
      call i4vec_indexed_heap_d ( n, a, indx )

      call i4vec_print ( n, indx, '  The index vector after heaping:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after heaping:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Insert five entries, and monitor the maximum.
c
      do i = 1, 5

        indx_insert = indx(n+1)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' )
     &    '  Inserting value          ', a(indx_insert)

        call i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

        call i4vec_indexed_heap_d_max ( n, a, indx, indx_max )

        write ( *, '(a,i8)' ) '  Current maximum is ', a(indx_max)

      end do
      call i4vec_print ( m, a, '  The data vector after insertions:' )
      call i4vec_print ( n, indx,
     &  '  The index vector after insertions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after insertions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Extract the first 5 largest elements.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now extract the maximum several times.'
      write ( *, '(a)' ) ' '

      do i = 1, 5
        call i4vec_indexed_heap_d_extract ( n, a, indx, indx_extract )
        write ( *, '(a,i8,a,i8)' ) '  Extracting maximum element A(',
     &    indx_extract,') = ', a(indx_extract)
      end do

      call i4vec_print ( m, a, '  The data vector after extractions:' )
      call i4vec_print ( n, indx,
     &  '  The index vector after extractions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after extractions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do

      return
      end
      subroutine i4vec_indexed_heap_d_insert_test ( )

c*********************************************************************72
c
cc I4VEC_INDEXED_HEAP_D_INSERT_TEST tests I4VEC_INDEXED_HEAP_D_INSERT.
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

      integer a(m)
      integer i
      integer indx(n_max)
      integer indx_extract
      integer indx_insert
      integer indx_max
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_INSERT_TEST'
      write ( *, '(a)' ) '  For an indexed I4VEC,'
      write ( *, '(a)' )
     &  '  I4VEC_INDEXED_HEAP_D_INSERT inserts a value into the heap.'
c
c  Set the data array.  To keep things easy, we will use the indicator vector.
c
      call i4vec_indicator1 ( m, a )
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

      call i4vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Create a descending heap from the indexed array.
c
      call i4vec_indexed_heap_d ( n, a, indx )

      call i4vec_print ( n, indx, '  The index vector after heaping:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after heaping:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Insert five entries, and monitor the maximum.
c
      do i = 1, 5

        indx_insert = indx(n+1)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' )
     &    '  Inserting value          ', a(indx_insert)

        call i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )


      end do

      call i4vec_print ( m, a, '  The data vector after insertions:' )
      call i4vec_print ( n, indx,
     &  '  The index vector after insertions:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after insertions:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do

      return
      end
      subroutine i4vec_indexed_heap_d_max_test ( )

c*********************************************************************72
c
cc I4VEC_INDEXED_HEAP_D_MAX_TEST tests I4VEC_INDEXED_HEAP_D_MAX.
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

      integer a(m)
      integer i
      integer indx(n_max)
      integer indx_extract
      integer indx_insert
      integer indx_max
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_MAX_TEST'
      write ( *, '(a)' ) '  For an indexed I4VEC,'
      write ( *, '(a)' )
     &  '  I4VEC_INDEXED_HEAP_D_MAX reports the maximum value.'
c
c  Set the data array.  To keep things easy, we will use the indicator vector.
c
      call i4vec_indicator1 ( m, a )
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

      call i4vec_print ( m, a, '  The data vector:' )
      call i4vec_print ( n, indx, '  The index vector:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX):'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Create a descending heap from the indexed array.
c
      call i4vec_indexed_heap_d ( n, a, indx )

      call i4vec_print ( n, indx, '  The index vector after heaping:' )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(INDX) after heaping:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,i4)' ) i, a(indx(i))
      end do
c
c  Insert five entries, and monitor the maximum.
c
      do i = 1, 5

        indx_insert = indx(n+1)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' )
     &    '  Inserting value          ', a(indx_insert)

        call i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

        call i4vec_indexed_heap_d_max ( n, a, indx, indx_max )

        write ( *, '(a,i8)' ) '  Current maximum is ', a(indx_max)

      end do

      return
      end
      subroutine i4vec_indicator0_test ( )

c*********************************************************************72
c
cc I4VEC_INDICATOR0_TEST tests I4VEC_INDICATOR0;
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

      integer a(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INDICATOR0_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_INDICATOR0 returns an indicator vector.'

      call i4vec_indicator0 ( n, a )

      call i4vec_print ( n, a, '  The "indicator0" vector:' )

      return
      end
      subroutine i4vec_insert_test ( )

c*********************************************************************72
c
cc I4VEC_INSERT_TEST tests I4VEC_INSERT.
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

      integer n_max
      parameter ( n_max = 20 )
      integer test_num
      parameter ( test_num = 6 )

      integer a(n_max)
      integer atest(test_num)
      integer aval
      integer i
      integer left
      integer n
      integer right
      integer test

      save atest

      data atest /
     &  -10, 2, 9, 10, 20, 24 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_INSERT_TEST'
      write ( *, '(a)' ) '  I4VEC_INSERT inserts a value into a vector.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  We use these two routines to bracket a'
      write ( *, '(a)' ) '  value, and then insert it.'

      n = 10
      do i = 1, n
        a(i) = 2 * i
      end do
      a(6) = a(5)

      call i4vec_print ( n, a, '  Sorted array:' )

      do test = 1, test_num

        aval = atest(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Search for AVAL = ', aval

        call i4vec_bracket ( n, a, aval, left, right )

        write ( *, '(a,i8)' ) '  Left = ', left
        write ( *, '(a,i8)' ) '  Right = ', right

        if ( 1 .le. left ) then
          write ( *, '(a,i8)' ) '  A(LEFT)=', a(left)
        end if

        if ( 1 .le. right ) then
          write ( *, '(a,i8)' ) '  A(RIGHT) = ', a(right)
        end if
!
!  Insert the value.
!
        if ( left .eq. -1 ) then
          left = 0
        end if

        if ( left .eq. right ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  No insertion necessary.'

        else

          call i4vec_insert ( n, a, left+1, aval )

          n = n + 1

          call i4vec_print ( n, a, '  Sorted, augmented array:' )

        end if

      end do

      return
      end
      subroutine i4vec_max_test ( )

c*********************************************************************72
c
cc I4VEC_MAX_TEST tests I4VEC_MAX.
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

      integer a(n)
      integer a_max
      integer b
      integer c
      integer i4vec_max
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MAX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_MAX:           maximum entry;'

      b = 1
      c = 30
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      a_max = i4vec_max ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Maximum:                  ', a_max

      return
      end
      subroutine i4vec_max_index_test ( )

c*********************************************************************72
c
cc I4VEC_MAX_INDEX_TEST tests I4VEC_MAX_INDEX.
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
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer i4vec_max_index_last
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MAX_INDEX_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_MAX_INDEX:          a maximal index;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_max_index ( n, a, ival )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Maximum index:            ', ival

      return
      end
      subroutine i4vec_max_index_last_test ( )

c*********************************************************************72
c
cc I4VEC_MAX_INDEX_LAST_TEST tests I4VEC_MAX_INDEX_LAST;
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
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer i4vec_max_index_last
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MAX_INDEX_LAST_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) 
     &  '  I4VEC_MAX_INDEX_LAST:     last maximal index;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      ival = i4vec_max_index_last ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Last maximum index:       ', ival

      return
      end
      subroutine i4vec_mean_test ( )

c*********************************************************************72
c
cc I4VEC_MEAN_TEST tests I4VEC_MEAN;
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
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer j
      double precision mean
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MEAN_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_MEAN:          mean value;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_mean ( n, a, mean )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Mean:                     ', mean

      return
      end
      subroutine i4vec_median_test ( )

c*********************************************************************72
c
cc I4VEC_MEDIAN_TEST tests I4VEC_MEDIAN;
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
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer j
      integer median
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MEDIAN_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_MEDIAN:        median value;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_median ( n, a, median )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Median:                   ', median

      return
      end
      subroutine i4vec_merge_a_test ( )

c*********************************************************************72
c
cc I4VEC_MERGE_A_TEST tests I4VEC_MERGE_A;
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

      integer n1
      parameter ( n1 = 10 )
      integer n2
      parameter ( n2 = 10 )

      integer a1(n1)
      integer a2(n2)
      integer a3(n1+n2)
      integer b
      integer c
      integer index
      integer n3
      integer search_val
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MERGE_A_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_MERGE_A merges two ascending-sorted I4VECs;'

      seed = 123456789

      b = 0
      c = n1

      call i4vec_uniform_ab ( n1, b, c, seed, a1 )

      search_val = a1(1)

      call i4vec_sort_heap_a ( n1, a1 )

      b = 0
      c = n2

      call i4vec_uniform_ab ( n2, b, c, seed, a2 )

      call i4vec_sort_heap_a ( n2, a2 )

      call i4vec_print ( n1, a1, '  Input vector A1:' )

      call i4vec_print ( n2, a2, '  Input vector A2:' )

      call i4vec_merge_a ( n1, a1, n2, a2, n3, a3 )

      call i4vec_print ( n3, a3, '  Merged vector A3:' )

      return
      end
      subroutine i4vec_min_test ( )

c*********************************************************************72
c
cc I4VEC_MIN_TEST tests I4VEC_MIN.
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

      integer a(n)
      integer a_min
      integer b
      integer c
      integer i4vec_min
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MIN_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_MIN:           minimum entry;'

      b = 1
      c = 30
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      a_min = i4vec_min ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Minimum:                  ', a_min

      return
      end
      subroutine i4vec_min_index_test ( )

c*********************************************************************72
c
cc I4VEC_MIN_INDEX_TEST tests I4VEC_MIN_INDEX;
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
      parameter ( n = 10 )

      integer a(n)
      integer aval
      integer b
      integer c
      integer ival
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_MIN_INDEX'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_MIN_INDEX:          a minimal index;'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_min_index ( n, a, ival )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Minimum index:            ', ival

      return
      end
      subroutine i4vec_nonzero_count_test ( )

c*********************************************************************72
c
cc I4VEC_NONZERO_COUNT_TEST tests I4VEC_NONZERO_COUNT.
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
      parameter ( n = 15 )

      integer a(n)
      integer b
      integer c
      integer i4vec_nonzero_count
      integer nonzero
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_NONZERO_COUNT_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_NONZERO_COUNT: number of nonzeroes;'

      seed = 123456789

      b = -3
      c = 4

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      nonzero = i4vec_nonzero_count ( n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nonzeroes :     ', nonzero

      return
      end
      subroutine i4vec_nonzero_first_test ( )

c*********************************************************************72
c
cc I4VEC_NONZERO_FIRST_TEST tests I4VEC_NONZERO_FIRST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer a_save(n)
      integer ihi
      integer ilo
      integer indx(n)
      integer nz
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 5 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_NONZERO_FIRST_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_NONZERO_FIRST left shifts the nonzero'
      write ( *, '(a)' ) '  entries of an I4VEC so they appear first.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  ----------Before--------------    ' //
     &  '----------After---------------'
      write ( *, '(a)' ) ' '
      seed = 123456789

      ilo = -1
      ihi = +2

      do test = 1, test_num

        call i4vec_uniform_ab ( n, ilo, ihi, seed, a )
        a_save(1:n) = a(1:n)
        call i4vec_nonzero_first ( n, a, nz, indx )
        write ( *, '(2x,10i3,4x,10i3)' ) a_save(1:n), a(1:n)

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The value NZ counts the nonzeros, and'
      write ( *, '(a)' ) 
     &  '  the vector INDX indicates the original positions:'
      write ( *, '(a)' ) ' '

      call i4vec_uniform_ab ( n, ilo, ihi, seed, a )
      a_save(1:n) = a(1:n)
      call i4vec_nonzero_first ( n, a, nz, indx )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Original vector:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,10i3)' ) a_save(1:n)
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Number of nonzeros NZ = ', nz
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Shifted vector:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,10i3)' ) a(1:n)
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Index vector:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,10i3)' ) indx(1:n)

      return
      end
      subroutine i4vec_order_type_test ( )

c*********************************************************************72
c
cc I4VEC_ORDER_TYPE_TEST tests I4VEC_ORDER_TYPE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2014
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
      integer x(n)
      integer x_test(n,test_num)

      save x_test
c
c  Each ROW of the definition is a COLUMN of the matrix.
c
      data x_test /
     &  1, 3, 2, 4, 
     &  2, 2, 2, 2, 
     &  1, 2, 2, 4, 
     &  1, 2, 3, 4, 
     &  4, 4, 3, 1, 
     &  9, 7, 3, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_ORDER_TYPE_TEST'
      write ( *, '(a)' ) '  I4VEC_ORDER_TYPE classifies an I4VEC as'
      write ( *, '(a)' ) '  -1: no order'
      write ( *, '(a)' ) '   0: all equal;'
      write ( *, '(a)' ) '   1: ascending;'
      write ( *, '(a)' ) '   2: strictly ascending;'
      write ( *, '(a)' ) '   3: descending;'
      write ( *, '(a)' ) '   4: strictly descending.'

      do test = 1, test_num

        x(1:n) = x_test(1:n,test)

        call i4vec_order_type ( n, x, order )
 
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  The following vector has order type ', order
        write ( *, '(a)' ) ' '
        do j = 1, n
          write ( *, '(2x,i8,i8)' ) j, x(j)
        end do

      end do

      return
      end
      subroutine i4vec_pairwise_prime_test ( )

c*********************************************************************72
c
cc I4VEC_PAIRWISE_PRIME_TEST tests I4VEC_PAIRWISE_PRIME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2014
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

      logical i4vec_pairwise_prime
      integer test
      integer x(n)
      integer x_test(n,test_num)

      save x_test
c
c  Each ROW of the definition is a COLUMN of the matrix.
c
      data x_test /
     &   1,  3,  2,  4, 
     &   2,  2,  2,  2, 
     &   5,  7, 12, 29, 
     &   1, 13,  1, 11, 
     &   1,  4,  9, 16, 
     &   6, 35, 13, 77 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_PAIRWISE_PRIME_TEST'
      write ( *, '(a)' ) '  I4VEC_PAIRWISE_PRIME determines if an'
      write ( *, '(a)' ) '  I4VEC is pairwise prime.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '              Pairwise'
      write ( *, '(a)' ) '  Row Vector     Prime?'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        x(1:n) = x_test(1:n,test)

        write ( *, '(2x,4i3,3x,l1)' ) 
     &    x(1:n), i4vec_pairwise_prime ( n, x )

      end do

      return
      end
      subroutine i4vec_part_test ( )

c*********************************************************************72
c
cc I4VEC_PART_TEST tests I4VEC_PART.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer a(n)
      integer nval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_PART_TEST'
      write ( *, '(a)' ) '  I4VEC_PART partitions an integer.'

      nval = 17
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  NVAL = ', nval

      call i4vec_part ( n, nval, a )

      call i4vec_print ( n, a, '  Partitioned:' )

      nval = -49
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  NVAL = ', nval

      call i4vec_part ( n, nval, a )

      call i4vec_print ( n, a, '  Partitioned:' )

      return
      end
      subroutine i4vec_part_quick_a_test ( )

c*********************************************************************72
c
cc I4VEC_PART_QUICK_A_TEST tests I4VEC_PART_QUICK_A.
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
      parameter ( n = 12 )

      integer a(n)
      integer b
      integer c
      integer l
      integer r
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_PART_QUICK_A_TEST'
      write ( *, '(a)' ) '  I4VEC_PART_QUICK_A reorders an I4VEC'
      write ( *, '(a)' ) '  as part of a quick sort.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      b = 0
      c = n
      seed = 123456789
      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Before rearrangement:' )

      call i4vec_part_quick_a ( n, a, l, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Rearranged array'
      write ( *, '(a,i8)' ) '  Left index =  ', l
      write ( *, '(a,i8)' ) '  Key index =   ', l + 1
      write ( *, '(a,i8)' ) '  Right index = ', r

      call i4vec_print ( l,     a(1:l),   '  Left half:' )
      call i4vec_print ( 1,     a(l+1),   '  Key:' )
      call i4vec_print ( n-l-1, a(l+2:n), '  Right half:' )

      return
      end
      subroutine i4vec_permute_test ( )

c*********************************************************************72
c
cc I4VEC_PERMUTE_TEST tests I4VEC_PERMUTE.
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
      parameter ( n = 12 )

      integer a(n)
      integer b
      integer c
      integer p(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_PERMUTE_TEST'
      write ( *, '(a)' ) '  I4VEC_PERMUTE reorders an I4VEC'
      write ( *, '(a)' ) '  according to a given permutation.'
      write ( *, '(a,i12)' ) 
     &  '  Using initial random number seed = ', seed

      b = 0
      c = n
      seed = 123456789
      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  A, before rearrangement:' )

      call perm_uniform ( n, seed, p )

      call i4vec_print ( n, p, '  Permutation vector P:' )

      call i4vec_permute ( n, p, a )

      call i4vec_print ( n, a, '  A, after rearrangement:' )

      return
      end
      subroutine i4vec_print_test ( )

c*********************************************************************72
c
cc I4VEC_PRINT_TEST tests I4VEC_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      integer a(n)

      save a

      data a /
     & 91, 92, 93, 94 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_PRINT_TEST'
      write ( *, '(a)' ) '  I4VEC_PRINT prints an I4VEC'

      call i4vec_print ( n, a, '  The I4VEC:' )

      return
      end
      subroutine i4vec_reverse_test ( )

c*********************************************************************72
c
cc I4VEC_REVERSE_TEST tests I4VEC_REVERSE.
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
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_REVERSE_TEST'
      write ( *, '(a)' ) '  I4VEC_REVERSE reverses a list of integers.'

      b = 0
      c= 3 * n
      seed = 123456789
      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Original vector:' )

      call i4vec_reverse ( n, a )

      call i4vec_print ( n, a, '  Reversed:' )

      a(1:n) = a(n:1:-1)

      call i4vec_print ( n, a, 
     &  '  Re-reversed array using a(1:n) = a(n:1:-1):' )

      return
      end
      subroutine i4vec_run_count_test ( )

c*********************************************************************72
c
cc I4VEC_RUN_COUNT_TEST tests I4VEC_RUN_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer i
      integer run_count
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 10 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_RUN_COUNT_TEST'
      write ( *, '(a)' ) '  I4VEC_RUN_COUNT counts runs in an I4VEC'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Run Count        Sequence'
      write ( *, '(a)' ) ' '

      seed = 123456789

      do test = 1, test_num

        call i4vec_uniform_ab ( n, 0, 1, seed, a )

        call i4vec_run_count ( n, a, run_count )

        write ( *, '(2x,i8,8x,20i2)' ) run_count, ( a(i), i = 1, n )

      end do

      return
      end
      subroutine i4vec_search_binary_a_test ( )

c*********************************************************************72
c
cc I4VEC_SEARCH_BINARY_A_TEST tests I4VEC_SEARCH_BINARY_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer index
      integer search_val
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SEARCH_BINARY_A_TEST'
      write ( *, '(a)' ) '  For ascending order:'
      write ( *, '(a)' ) 
     &  '  I4VEC_SEARCH_BINARY_A searchs an I4VEC for a value;'

      seed = 123456789

      b = 0
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      search_val = a(1)

      call i4vec_sort_heap_a ( n, a )

      call i4vec_print ( n, a, '  Input vector A:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Search the array A for the value ', search_val

      call i4vec_search_binary_a ( n, a, search_val, index )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SEARCH RESULT:'
      if ( 0 .lt. index ) then
        write ( *, '(a,i8)' ) '    The value occurs in index ', index
      else
        write ( *, '(a)' ) '    The value does not occur in the array.'
      end if

      return
      end
      subroutine i4vec_sort_bubble_a_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_BUBBLE_A_TEST tests I4VEC_SORT_BUBBLE_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer unique_max
      parameter ( unique_max = 20 )
      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_BUBBLE_A_TEST'
      write ( *, '(a)' ) '  For an I4VEC,'
      write ( *, '(a)' ) '  I4VEC_SORT_BUBBLE_A ascending sorts,'

      b = 0
      c = 3 * n
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted:' )

      call i4vec_sort_bubble_a ( n, a )

      call i4vec_print ( n, a, '  Ascending sorted:' )

      return
      end
      subroutine i4vec_sort_heap_a_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_A_TEST tests I4VEC_SORT_HEAP_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_HEAP_A_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_SORT_HEAP_A ascending sorts an I4VEC,'

      b = 0
      c = 3 * n
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted:' )

      call i4vec_sort_heap_a ( n, a )

      call i4vec_print ( n, a, '  Ascending sorted:' )

      return
      end
      subroutine i4vec_sort_heap_d_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_D_TEST tests I4VEC_SORT_HEAP_D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_HEAP_D_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_SORT_HEAP_D descending sorts an I4VEC.'

      b = 0
      c = 3 * n
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted:' )

      call i4vec_sort_heap_d ( n, a )

      call i4vec_print ( n, a, '  Descending sorted:' )

      return
      end
      subroutine i4vec_sort_heap_index_a_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_INDEX_A_TEST tests I4VEC_SORT_HEAP_INDEX_A.
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
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer i
      integer indx(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_HEAP_INDEX_A_TEST'
      write ( *, '(a)' ) '  I4VEC_SORT_HEAP_INDEX_A creates an'
      write ( *, '(a)' ) '  ascending sort index for an I4VEC.'

      b = 0
      c = 3 * n
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )

      call i4vec_sort_heap_index_a ( n, a, indx )

      call i4vec_print ( n, indx, '  Sort vector INDX:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now use the index array to carry out the'
      write ( *, '(a)' ) '  permutation implicitly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,3i8)' ) i, indx(i), a(indx(i))
      end do

      return
      end
      subroutine i4vec_sort_heap_index_d_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_INDEX_D_TEST tests I4VEC_SORT_HEAP_INDEX_D.
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
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer i
      integer indx(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_HEAP_INDEX_D_TEST'
      write ( *, '(a)' ) '  I4VEC_SORT_HEAP_INDEX_D creates a'
      write ( *, '(a)' ) '  descending sort index for an I4VEC.'

      b = 0
      c = 3 * n
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )

      call i4vec_sort_heap_index_d ( n, a, indx )

      call i4vec_print ( n, indx, '  Sort vector INDX:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now use the index array to carry out the'
      write ( *, '(a)' ) '  permutation implicitly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I, INDX(I), A(INDX(I))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,3i8)' ) i, indx(i), a(indx(i))
      end do

      return
      end
      subroutine i4vec_sort_insert_a_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_INSERT_A_TEST tests I4VEC_SORT_INSERT_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 January 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_INSERT_A_TEST'
      write ( *, '(a)' ) '  I4VEC_SORT_INSERT_A sorts an integer array.'

      seed = 123456789

      b = 0
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )

      call i4vec_sort_insert_a ( n, a )

      call i4vec_print ( n, a, '  Sorted array:' )

      return
      end
      subroutine i4vec_sort_quick_a_test ( )

c*****************************************************************************80
c
cc I4VEC_SORT_QUICK_A_TEST tests I4VEC_SORT_QUICK_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_QUICK_A_TEST'
      write ( *, '(a)' ) '  I4VEC_SORT_QUICK_A sorts an I4VEC'
      write ( *, '(a)' ) '  using quick sort.'

      seed = 123456789

      b = 0
      c = 3 * n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )

      call i4vec_sort_quick_a ( n, a )

      call i4vec_print ( n, a, '  Sorted array:' )

      return
      end
      subroutine i4vec_sort_shell_a_test ( )

c*********************************************************************72
c
cc I4VEC_SORT_SHELL_A_TEST tests I4VEC_SORT_SHELL_A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORT_SHELL_A_TEST'
      write ( *, '(a)' ) '  I4VEC_SORT_SHELL_A sorts an I4VEC'
      write ( *, '(a)' ) '  using Shell''s sort.'

      seed = 123456789

      b = 0
      c = 3 * n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )

      call i4vec_sort_shell_a ( n, a )

      call i4vec_print ( n, a, '  Sorted array:' )

      return
      end
      subroutine i4vec_sorted_unique_hist_test ( )

c*********************************************************************72
c
cc I4VEC_SORTED_UNIQUE_HIST_TEST tests I4VEC_SORTED_UNIQUE_HIST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer unique_max
      parameter ( unique_max = 20 )
      integer n
      parameter ( n = 20 )

      integer a(n)
      integer acount(unique_max)
      integer auniq(unique_max)
      integer b
      integer c
      integer unique_num
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORTED_UNIQUE_HIST_TEST'
      write ( *, '(a)' ) '  For an I4VEC,'
      write ( *, '(a)' ) '  I4VEC_SORTED_UNIQUE_HIST makes a histogram'
      write ( *, '(a)' ) '  of unique entries.'

      b = 0
      c = 3 * n
      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Unsorted:' )

      call i4vec_sort_bubble_a ( n, a )

      call i4vec_print ( n, a, '  Ascending sorted:' )

      call i4vec_sorted_unique_hist ( n, a, unique_max, unique_num, 
     &  auniq, acount );

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) '  I4VEC_UNIQ3 counts ',  unique_num, 
     &  ' unique entries.'

      call i4vec2_print ( unique_num, auniq, acount, 
     &  '  Value and Multiplicity' )

      return
      end
      subroutine i4vec_sorted_unique_test ( )

c*********************************************************************72
c
cc I4VEC_SORTED_UNIQUE_TEST tests I4VEC_SORTED_UNIQUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer seed
      integer unique_num

      b = 0
      c = n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SORTED_UNIQUE_TEST'
      write ( *, '(a)' ) '  I4VEC_SORTED_UNIQUE finds unique entries'
      write ( *, '(a)' ) '  in a sorted array.'

      seed = 123456789

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_sort_heap_a ( n, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_sorted_unique ( n, a, unique_num )

      call i4vec_print ( unique_num, a, '  Unique entries:' )

      return
      end
      subroutine i4vec_sum_test ( )

c*********************************************************************72
c
cc I4VEC_SUM_TEST tests I4VEC_SUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none
 
      integer n
      parameter ( n = 5 )

      integer a(n)
      integer hi
      integer i4vec_sum
      integer lo
      integer s
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_SUM_TEST'
      write ( *, '(a)' ) '  I4VEC_SUM sums the entries of an I4VEC.'

      lo = 0
      hi = 10
      seed = 123456789

      call i4vec_uniform_ab ( n, lo, hi, seed, a )
      call i4vec_print ( n, a, '  The vector:' )

      s = i4vec_sum ( n, a )
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  The vector entries sum to ', s

      return
      end
      subroutine i4vec_transpose_print_test ( )

c*********************************************************************72
c
cc I4VEC_TRANSPOSE_PRINT_TEST tests I4VEC_TRANSPOSE_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 12 )

      integer a(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_TRANSPOSE_PRINT_TEST'
      write ( *, '(a)' )
     &  '  I4VEC_TRANSPOSE_PRINT prints an I4VEC'
      write ( *, '(a)' )
     &  '  with 5 entries to a row, and an optional title.'

      call i4vec_indicator1 ( n, a )

      call i4vec_print ( n, a, '  Output from I4VEC_PRINT:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '  Call I4VEC_TRANSPOSE_PRINT with a short title:'

      call i4vec_transpose_print ( n, a, '  My array:  ' )

      return
      end
      subroutine i4vec_undex_test ( )

c*********************************************************************72
c
cc I4VEC_UNDEX_TEST tests I4VEC_UNDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer x_num
      parameter ( x_num = 9 )

      integer i
      integer undx(x_num)
      integer x_unique_num
      integer x_val(x_num)
      integer xu_val(x_num)
      integer xdnu(x_num)

      save x_val

      data x_val /
     &  33, 55, 11, 11, 55, 33, 22, 22, 11 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_UNDEX_TEST'
      write ( *, '(a)' ) 
     &  '  I4VEC_UNDEX produces index vectors which create a sorted'
      write ( *, '(a)' ) 
     &  '  list of the unique elements of an (unsorted) I4VEC,'
      write ( *, '(a)' ) 
     &  '  and a map from the original vector to the (implicit)'
      write ( *, '(a)' ) '  vector of sorted unique elements.'

      call i4vec_print ( x_num, x_val, '  The vector X:' )

      call i4vec_unique_count ( x_num, x_val, x_unique_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique entries in X is ', x_unique_num

      call i4vec_undex ( x_num, x_val, x_unique_num, undx, xdnu )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  UNDX can be used to list the unique elements of X'
      write ( *, '(a)' ) '  in sorted order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  UNDX   X(UNDX)'
      write ( *, '(a)' ) ' '

      do i = 1, x_unique_num
        write ( *, '(2x,i4,2x,i4,2x,i8)' ) i, undx(i), x_val(undx(i))
      end do

      do i = 1, x_unique_num
        xu_val(i) = x_val(undx(i))
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  UNDX can be used to created XU, a copy of X'
      write ( *, '(a)' ) 
     &  '  containing only the unique elements, in sorted order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  UNDX XU(I)'
      write ( *, '(a)' ) ' '
      do i = 1, x_unique_num
        write ( *, '(2x,i4,2x,i4,2x,i4)' ) i, undx(i), xu_val(i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  XDNU can be used to match each element of X with one of the'
      write ( *, '(a)' ) '  unique elements'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  XDNU  X(I)   XU(XDNU(I))'
      write ( *, '(a)' ) ' '

      do i = 1, x_num
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,i12)' ) 
     &    i, xdnu(i), x_val(i), xu_val(xdnu(i))
      end do

      return
      end
      subroutine i4vec_uniform_ab_test ( )

c*********************************************************************72
c
cc I4VEC_UNIFORM_AB_TEST tests I4VEC_UNIFORM_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a
      parameter ( a = -100 )
      integer b
      parameter ( b = 200 )
      integer i4_uniform_ab
      integer seed
      integer v(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_UNIFORM_AB_TEST'
      write ( *, '(a)' ) '  I4VEC_UNIFORM_AB computes pseudorandom'
      write ( *, '(a)' ) '  values in an interval [A,B].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
      write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
      write ( *, '(a,i12)' ) '  The initial seed is ', seed
      write ( *, '(a)' ) ' '

      call i4vec_uniform_ab ( n, a, b, seed, v )

      call i4vec_print ( n, v, '  The random vector:' )

      return
      end
      subroutine i4vec_unique_index_test ( )

c*********************************************************************72
c
cc I4VEC_UNIQUE_INDEX_TEST tests I4VEC_UNIQUE_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c   30 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer b
      integer c
      integer i
      integer seed
      integer unique_index(n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_UNIQUE_INDEX_TEST'
      write ( *, '(a)' )
     &  '  I4VEC_UNIQUE_INDEX, for each entry in an I4VEC'
      write ( *, '(a)' ) '  indexes the unique elements.'

      b = 1
      c = 5

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_unique_index ( n, a, unique_index )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I      A(I)    UNIQUE'
      write ( *, '(a)' ) ' '

      do i = 1, n
        write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, a(i), unique_index(i)
      end do

      return
      end
      subroutine i4vec_value_index_test ( )

c*********************************************************************72
c
cc I4VEC_VALUE_INDEX_TEST tests I4VEC_VALUE_INDEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer max_index
      parameter ( max_index = 3 )
      integer n
      parameter ( n = 25 )

      integer a(n)
      integer b
      integer c
      integer n_index
      integer seed
      integer value
      integer value_index(max_index)

      seed = 123456789

      value = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_VALUE_INDEX_TEST'
      write ( *, '(a)' ) '  I4VEC_VALUE_INDEX indexes entries equal to'
      write ( *, '(a)' ) '  a given value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The desired value is ', value
      write ( *, '(a,i8)' )
     &  '  Maximum number of indices to find is ', max_index

      b = 1
      c = 5

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector A:' )

      call i4vec_value_index ( n, a, value, max_index, n_index,
     &  value_index )

      call i4vec_print ( n_index, value_index,
     &  '  Indices of entries equal to given value: ' )

      return
      end
      subroutine i4vec_variance_test ( )

c*********************************************************************72
c
cc I4VEC_VARIANCE_TEST tests I4VEC_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 December 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(n)
      integer b
      integer c
      integer seed
      double precision variance

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC_VARIANCE_TEST'
      write ( *, '(a)' ) '  For an I4VEC:'
      write ( *, '(a)' ) '  I4VEC_VARIANCE:      variance.'

      seed = 123456789

      b = -n
      c = n

      call i4vec_uniform_ab ( n, b, c, seed, a )

      call i4vec_print ( n, a, '  Input vector:' )

      call i4vec_variance ( n, a, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Variance:                 ', variance

      return
      end
      subroutine i4vec2_sort_a_test ( )

c*********************************************************************72
c
cc I4VEC2_SORT_A_TEST tests I4VEC2_SORT_A.
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

      integer b
      integer c
      integer ivec(n)
      integer jvec(n)
      integer unique_num
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC2_SORT_A_TEST'
      write ( *, '(a)' ) '  For a pair of I4VECs:'
      write ( *, '(a)' ) '  I4VEC2_SORT_A ascending sorts;'

      b = 1
      c = 3

      call i4vec_uniform_ab ( n, b, c, seed, ivec )

      call i4vec_uniform_ab ( n, b, c, seed, jvec )

      ivec(3) = ivec(1)
      jvec(3) = jvec(1)

      ivec(5) = ivec(2)
      jvec(5) = jvec(2)

      ivec(9) = ivec(1)
      jvec(9) = jvec(1)

      call i4vec2_print ( n, ivec, jvec, '  The array:' )

      call i4vec2_sort_a ( n, ivec, jvec )

      call i4vec2_print ( n, ivec, jvec, '  After ascending sort:' )

      return
      end
      subroutine i4vec2_sort_d_test ( )

c*********************************************************************72
c
cc I4VEC2_SORT_D_TEST tests I4VEC2_SORT_D.
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

      integer b
      integer c
      integer ivec(n)
      integer jvec(n)
      integer unique_num
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC2_SORT_D_TEST'
      write ( *, '(a)' ) '  For a pair of I4VECs:'
      write ( *, '(a)' ) '  I4VEC2_SORT_D descending sorts;'

      b = 1
      c = 3

      call i4vec_uniform_ab ( n, b, c, seed, ivec )

      call i4vec_uniform_ab ( n, b, c, seed, jvec )

      ivec(3) = ivec(1)
      jvec(3) = jvec(1)

      ivec(5) = ivec(2)
      jvec(5) = jvec(2)

      ivec(9) = ivec(1)
      jvec(9) = jvec(1)

      call i4vec2_print ( n, ivec, jvec, '  The array:' )

      call i4vec2_sort_d ( n, ivec, jvec )

      call i4vec2_print ( n, ivec, jvec, '  After descending sort:' )

      return
      end
      subroutine i4vec2_sorted_unique_test ( )

c*********************************************************************72
c
cc I4VEC2_SORTED_UNIQUE_TEST tests I4VEC2_SORTED_UNIQUE.
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

      integer b
      integer c
      integer ivec(n)
      integer jvec(n)
      integer unique_num
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4VEC2_SORTED_UNIQUE_TEST'
      write ( *, '(a)' ) '  For a pair of I4VECs:'
      write ( *, '(a)' ) '  I4VEC2_SORTED_UNIQUE counts unique entries.'

      b = 1
      c = 3

      call i4vec_uniform_ab ( n, b, c, seed, ivec )

      call i4vec_uniform_ab ( n, b, c, seed, jvec )

      ivec(3) = ivec(1)
      jvec(3) = jvec(1)

      ivec(5) = ivec(2)
      jvec(5) = jvec(2)

      ivec(9) = ivec(1)
      jvec(9) = jvec(1)

      call i4vec2_print ( n, ivec, jvec, '  The array:' )

      call i4vec2_sort_a ( n, ivec, jvec )

      call i4vec2_print ( n, ivec, jvec, '  After ascending sort:' )

      call i4vec2_sorted_unique ( n, ivec, jvec, unique_num )

      call i4vec2_print ( unique_num, ivec, jvec, '  After UNIQ:' )

      return
      end
      subroutine pascal_to_i4_test ( )

c*********************************************************************72
c
cc PASCAL_TO_I4_TEST tests PASCAL_TO_I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer d
      integer i
      integer j
      integer k

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PASCAL_TO_I4_TEST'
      write ( *, '(a)' ) 
     &  '  PASCAL_TO_I4 converts Pascal triangle indices'
      write ( *, '(a)' ) '  to a linear index.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I     J =>    K'
      write ( *, '(a)' ) ''

      do d = 0, 4
        do i = d, 0, -1
          j = d - i
          call pascal_to_i4 ( i, j, k )
          write ( *, '(2x,i4,2x,i4,4x,i4)' ) i, j, k
        end do
        write ( *, '(a)' ) ''
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
      subroutine prime_test ( )

c*********************************************************************72
c
cc PRIME_TEST tests PRIME.
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

      integer i
      integer n
      integer prime
      integer prime_max

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PRIME_TEST'
      write ( *, '(a)' ) '  PRIME returns primes from a table.'

      n = -1
      prime_max = prime ( n )
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) 
     &  '  Number of primes stored is ', prime_max
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    Prime(I)'
      write ( *, '(a)' ) ''
      do i = 1, 10
        write ( *, '(4x,i4,2x,i6)' ) i, prime(i)
      end do
      write ( *, '(a)' ) ''
      do i = prime_max - 10, prime_max
        write ( *, '(4x,i4,2x,i6)' ) i, prime(i)
      end do
  
      return
      end
      subroutine triangle_lower_to_i4_test ( )

c*********************************************************************72
c
cc TRIANGLE_LOWER_TO_I4_TEST tests TRIANGLE_LOWER_TO_I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_LOWER_TO_I4_TEST'
      write ( *, '(a)' ) '  TRIANGLE_LOWER_TO_I4 converts a lower'
      write ( *, '(a)' ) '  triangular index to a linear one.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I, J  =>   K'
      write ( *, '(a)' ) ' '

      do i = 1, 4
        do j = 1, i
          call triangle_lower_to_i4 ( i, j, k )
          write ( *, '(2x,i4,i4,4x,i4)' ) i, j, k
        end do
      end do
     
      return
      end
