      program main

c*********************************************************************72
c
cc MAIN is the main program for LPP_PRB.
c
c  Discussion:
c
c    LPP_PRB tests the LEGENDRE_PRODUCT_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LPP_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Test the LEGENDRE_PRODUCT_POLYNOMIAL library.'

      call i4_choose_test ( )
      call i4_uniform_ab_test ( )

      call i4vec_permute_test ( )
      call i4vec_print_test ( )
      call i4vec_sort_heap_index_a_test ( )
      call i4vec_sum_test ( )
      call i4vec_uniform_ab_test ( )

      call r8vec_permute_test ( )
      call r8vec_print_test ( )
      call r8vec_uniform_ab_test ( )

      call perm_uniform_test ( )

      call comp_enum_test ( )
      call comp_next_grlex_test ( )
      call comp_random_grlex_test ( )
      call comp_rank_grlex_test ( )
      call comp_unrank_grlex_test ( )

      call mono_next_grlex_test ( )
      call mono_print_test ( )
      call mono_rank_grlex_test ( )
      call mono_unrank_grlex_test ( )
      call mono_upto_enum_test ( )
      call mono_upto_next_grlex_test ( )
      call mono_upto_random_test ( )
      call mono_value_test ( )

      call polynomial_compress_test ( )
      call polynomial_print_test ( )
      call polynomial_sort_test ( )
      call polynomial_value_test ( )

      call lp_coefficients_test ( )
      call lp_value_test ( )
      call lp_values_test ( )

      call lpp_to_polynomial_test ( )
      call lpp_value_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LPP_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine comp_enum_test ( )

c*********************************************************************72
c
cc COMP_ENUM_TEST tests COMP_ENUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 October 2014
c
c  Author:
c
c    John Burkardt
c 
      implicit none

      integer k
      integer n
      integer num

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMP_ENUM_TEST'
      write ( *, '(a)' ) '  COMP_ENUM counts compositions;'
      write ( *, '(a)' ) ''
      do n = 0, 10
        do k = 1, 10
          call comp_enum ( n, k, num )
          write ( *, '(2x,i6)', advance = 'no' ) num
        end do
        write ( *, '(a)' ) ''
      end do

      return
      end
      subroutine comp_next_grlex_test ( )

c*****************************************************************************80
c
cc COMP_NEXT_GRLEX_TEST tests COMP_NEXT_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer kc
      parameter ( kc = 3 )

      integer i4vec_sum
      integer j
      integer nc
      integer rank
      integer xc(kc)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMP_NEXT_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  A COMP is a composition of an integer N into K parts.'
      write ( *, '(a)' ) 
     &  '  Each part is nonnegative.  The order matters.'
      write ( *, '(a)' ) '  COMP_NEXT_GRLEX determines the next COMP in'
      write ( *, '(a)' ) '  graded lexicographic (grlex) order.'
      
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rank:     NC       COMP    '
      write ( *, '(a)' ) '  ----:     --   ------------'

      do rank = 1, 71

        if ( rank .eq. 1 ) then
          do j = 1, kc
            xc(j) = 0
          end do
        else
          call comp_next_grlex ( kc, xc )
        end if

        nc = i4vec_sum ( kc, xc )

        write ( *, '(3x,i3,a)', advance = 'no' ) rank, ': '
        write ( *, '(4x,i2,a)', advance = 'no' ) nc, ' = '
        do j = 1, kc - 1
          write ( *, '(i2,a)', advance = 'no' ) xc(j), ' + '
        end do
        write ( *, '(i2)', advance = 'yes' ) xc(kc)
c
c  When XC(1) == NC, we have completed the compositions associated with
c  a particular integer, and are about to advance to the next integer.
c
        if ( xc(1) .eq. nc ) then
          write ( *, '(a)' ) '  ----:     --   ------------'
        end if

      end do

      return
      end
      subroutine comp_random_grlex_test ( )

c*********************************************************************72
c
cc COMP_RANDOM_GRLEX_TEST tests COMP_RANDOM_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer kc
      parameter ( kc = 3 )

      integer i4vec_sum
      integer j
      integer nc
      integer rank
      integer rank1
      integer rank2
      integer seed
      integer test
      integer xc(kc)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMP_RANDOM_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  A COMP is a composition of an integer N into K parts.'
      write ( *, '(a)' ) 
     &  '  Each part is nonnegative.  The order matters.'
      write ( *, '(a)' ) '  COMP_RANDOM_GRLEX selects a random COMP in'
      write ( *, '(a)' ) '  graded lexicographic (grlex) order between'
      write ( *, '(a)' ) '  indices RANK1 and RANK2.'
      write ( *, '(a)' ) ''

      rank1 = 20
      rank2 = 60
      seed = 123456789

      do test = 1, 5

        call comp_random_grlex ( kc, rank1, rank2, seed, xc, rank )
        nc = i4vec_sum ( kc, xc )

        write ( *, '(a,i3,a)', advance = 'no' ) '   ', rank, ': '
        write ( *, '(a,i2,a)', advance = 'no' ) '    ', nc, ' = '
        do j = 1, kc - 1
          write ( *, '(i2,a)', advance = 'no' ) xc(j), ' + '
        end do
        write ( *, '(i2)' ) xc(kc)
      end do

      return
      end
      subroutine comp_rank_grlex_test ( )

c*********************************************************************72
c
cc COMP_RANK_GRLEX_TEST tests COMP_RANK_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer kc
      parameter ( kc = 3 )

      integer i4vec_sum
      integer nc
      integer rank1
      integer rank2
      integer rank3
      integer rank4
      integer seed
      integer test
      integer xc(kc)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMP_RANK_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  A COMP is a composition of an integer N into K parts.'
      write ( *, '(a)' ) 
     &  '  Each part is nonnegative.  The order matters.'
      write ( *, '(a)' ) 
     &  '  COMP_RANK_GRLEX determines the rank of a COMP'
      write ( *, '(a)' ) '  from its parts.'
     
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '        Actual  Inferred'
      write ( *, '(a)' ) '  Test    Rank      Rank'
      write ( *, '(a)' ) ''

      rank1 = 20
      rank2 = 60
      seed = 123456789

      do test = 1, 5

        call comp_random_grlex ( kc, rank1, rank2, seed, xc, rank3 )
        call comp_rank_grlex ( kc, xc, rank4 )
        write ( *, '(2x,i4,2x,i6,2x,i8)' ) test, rank3, rank4
      end do

      return
      end
      subroutine comp_unrank_grlex_test ( )

c*********************************************************************72
c
cc COMP_UNRANK_GRLEX_TEST tests COMP_UNRANK_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer kc
      parameter ( kc = 3 )

      integer i4vec_sum
      integer j
      integer nc
      integer rank1
      integer xc(kc)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMP_UNRANK_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  A COMP is a composition of an integer N into K parts.'
      write ( *, '(a)' ) 
     &  '  Each part is nonnegative.  The order matters.'
      write ( *, '(a)' ) '  COMP_UNRANK_GRLEX determines the parts'
      write ( *, '(a)' ) '  of a COMP from its rank.'
     
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rank: ->  NC       COMP'
      write ( *, '(a)' ) '  ----:     --   ------------'

      do rank1 = 1, 71

        call comp_unrank_grlex ( kc, rank1, xc )
        nc = i4vec_sum ( kc, xc )

        write ( *, '(3x,i3,a)', advance = 'no' ) rank1, ': '
        write ( *, '(4x,i2,a)', advance = 'no' ) nc, ' = '
        do j = 1, kc - 1
          write ( *, '(i2,a)', advance = 'no' ) xc(j), ' + '
        end do
        write ( *, '(i2)', advance = 'yes' ) xc(kc)
c
c  When XC(1) == NC, we have completed the compositions associated with
c  a particular integer, and are about to advance to the next integer.
c
        if ( xc(1) .eq. nc ) then
          write ( *, '(a)' ) '  ----:     --   ------------'
        end if

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

      integer a
      parameter ( a = -100 )
      integer b
      parameter ( b = 200 )
      integer i
      integer i4_uniform_ab
      integer j
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
      write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
      write ( *, '(a)' ) '  in an interval [A,B].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
      write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
      write ( *, '(a,i12)' ) '  The initial seed is ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 20

        j = i4_uniform_ab ( a, b, seed )

        write ( *, '(2x,i8,2x,i8)' ) i, j

      end do

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
      subroutine lp_coefficients_test ( )

c*********************************************************************72
c
cc LP_COEFFICIENTS_TEST tests LP_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision c(n_max+1)
      integer e(n_max+1)
      integer f(n_max+1)
      integer i
      character * ( 255 ) label
      integer m 
      integer n
      integer o

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LP_COEFFICIENTS_TEST'
      write ( *, '(a)' ) 
     &  '  LP_COEFFICIENTS: coefficients of Legendre polynomial P(n,x).'
      write ( *, '(a)' ) ''
  
      do n = 0, n_max

        call lp_coefficients ( n, o, c, f )

        m = 1

        do i = 1, o
          e(i) = f(i) + 1
        end do

        write ( label, '(a,i2,a)' ) '  P(', n, ',x) ='
        call polynomial_print ( m, o, c, e, label )

       end do

      return
      end
      subroutine lp_value_test ( )

c*********************************************************************72
c
cc LP_VALUE_TEST tests LP_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision e
      integer n
      integer n_data
      integer o
      double precision x
      double precision fx1
      double precision fx2

      n = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LP_VALUE_TEST:'
      write ( *, '(a)' ) '  LP_VALUE evaluates a Legendre polynomial.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '                        Tabulated                 Computed'
      write ( *, '(a)', advance = 'no' )
     &  '     O        X           L(O,X)                    L(O,X)'
      write ( *, '(a)' ) '                   Error'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call lp_values ( n_data, o, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call lp_value ( n, o, x, fx2 )

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.8,2x,g24.16,2x,g24.16,2x,g8.2)' )     
     &  o, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine lp_values_test ( )

c*********************************************************************72
c
cc LP_VALUES_TEST tests LP_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      integer o
      double precision x
      double precision fx

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LP_VALUES_TEST:'
      write ( *, '(a)' ) '  LP_VALUES stores values of'
      write ( *, '(a)' ) '  the Legendre polynomial P(o,x).'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '                        Tabulated'
      write ( *, '(a)' ) '     O        X           L(O,X)'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call lp_values ( n_data, o, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i4,2x,f12.8,2x,g24.16)' ) o, x, fx

      go to 10

20    continue

      return
      end
      subroutine lpp_to_polynomial_test ( )

c*********************************************************************72
c
cc LPP_TO_POLYNOMIAL_TEST tests LPP_TO_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer o_max_max
      parameter ( o_max_max = 10 )

      double precision c(o_max_max)
      integer e(o_max_max)
      integer i
      integer l(m)
      character * ( 255 ) label
      integer o
      integer o_max
      integer rank

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LPP_TO_POLYNOMIAL_TEST:'
      write ( *, '(a)' ) 
     &  '  LPP_TO_POLYNOMIAL is given a Legendre product polynomial'
      write ( *, '(a)' ) 
     &  '  and determines its polynomial representation.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i2)' ) '  Using spatial dimension M = ', m

      do rank = 1, 11

        call comp_unrank_grlex ( m, rank, l )

        o_max = 1
        do i = 1, m
          o_max = o_max * ( ( l(i) + 2 ) / 2 )
        end do

        call lpp_to_polynomial ( m, l, o_max, o, c, e )

        write ( label, '(a,i2,a,i2,a,i2,a)' )
     &    '  LPP #', rank,    
     &    ' = L(', l(1),
     &    ',X)*L(', l(2),
     &    ',Y) ='

        write ( *, '(a)' ) ''
        call polynomial_print ( m, o, c, e, label )

      end do

      return
      end
      subroutine lpp_value_test ( )

c*********************************************************************72
c
cc LPP_VALUE_TEST tests LPP_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 1 )
      integer o_max_max
      parameter ( o_max_max = 5 )

      double precision c(o_max_max)
      integer e(o_max_max)
      integer i
      integer l(m)
      integer o
      integer o_max
      integer rank
      integer seed
      double precision v1(n)
      double precision v2(n)
      double precision x(m,n)
      double precision xhi
      double precision xlo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LPP_VALUE_TEST:'
      write ( *, '(a)' ) 
     &  '  LPP_VALUE evaluates a Legendre product polynomial.'
      write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

      xlo = -1.0D+00
      xhi = +1.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( m, xlo, xhi, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,3g14.6)' ) '  Evaluate at X = ', x(1:3,1)
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Rank  I1  I2  I3:  L(I1,X1)*L(I2,X2)*L(I3,X3)    P(X1,X2,X3)'
      write ( *, '(a)' ) ''

      do rank = 1, 20

        call comp_unrank_grlex ( m, rank, l )
c
c  Evaluate the LPP directly.
c
        call lpp_value ( m, n, l, x, v1 )
c
c  Convert the LPP to a polynomial.
c
        o_max = 1
        do i = 1, m
          o_max = o_max * ( ( l(i) + 2 ) / 2 )
        end do

        call lpp_to_polynomial ( m, l, o_max, o, c, e )
c
c  Evaluate the polynomial.
c
        call polynomial_value ( m, o, c, e, n, x, v2 )
c
c  Compare results.
c
        write ( *, '(2x,i4,2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6)' ) 
     &    rank, l(1:m), v1(1), v2(1)

      end do

      return
      end
      subroutine mono_next_grlex_test ( )

c*********************************************************************72
c
cc MONO_NEXT_GRELX_TEST tests MONO_NEXT_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )

      integer a
      integer b
      integer i
      integer j
      integer seed
      integer x(m)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_NEXT_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  MONO_NEXT_GRLEX returns the next monomial'
      write ( *, '(a)' ) '  in graded lexicographic order.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i2)' ) '  Let M =  ', m

      a = 0
      b = 3
      seed = 123456789

      do i = 1, 10

        call i4vec_uniform_ab ( m, a, b, seed, x )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,4i2)' ) x(1:m)

        do j = 1, 5
          call mono_next_grlex ( m, x )
          write ( *, '(2x,4i2)' ) x(1:m)
        end do

      end do

      return
      end
      subroutine mono_print_test ( )

c*****************************************************************************80
c
cc MONO_PRINT_TEST tests MONO_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f1(1)
      integer f2(1)
      integer f3(4)
      integer f4(3)
      integer m

      save f1
      save f2
      save f3
      save f4

      data f1 / 5 /
      data f2 / - 5 /
      data f3 / 2, 1, 0, 3 /
      data f4 / 17, -3, 199 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_PRINT_TEST'
      write ( *, '(a)' ) '  MONO_PRINT can print out a monomial.'
      write ( *, '(a)' ) ''

      m = 1
      call mono_print ( m, f1, '  Monomial [5]:' )

      m = 1
      call mono_print ( m, f2, '  Monomial [5]:' )

      m = 4
      call mono_print ( m, f3, '  Monomial [2,1,0,3]:' )

      m = 3
      call mono_print ( m, f4, '  Monomial [17,-3,199]:' )

      return
      end
      subroutine mono_rank_grlex_test ( )

c*********************************************************************72
c
cc MONO_RANK_GRLEX_TEST tests MONO_RANK_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer test_num
      parameter ( test_num = 8 )

      integer i
      integer n
      integer rank
      integer test
      integer x(m)
      integer x_test(m,test_num)

      save x_test

      data x_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 0, 1,
     &  0, 2, 0,
     &  1, 0, 2,
     &  0, 3, 1,
     &  3, 2, 1,
     &  5, 2, 1 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_RANK_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  MONO_RANK_GRLEX returns the rank of a monomial in the'
      write ( *, '(a)' ) 
     &  '  setquence of all monomials in M dimensions '
      write ( *, '(a)' ) '  of degree N or less.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Print a monomial sequence with ranks assigned.'

      n = 4

      write ( *, '(a)' ) ''
      write ( *, '(a,i2)' ) '  Let M = ', m
      write ( *, '(a,i2)' ) '      N = ', n
      write ( *, '(a)' ) ''

      x(1) = 0
      x(2) = 0
      x(3) = 0

      i = 1

10    continue

        write ( *, '(2x,i3,4x,3i2)' ) i, x(1:m)

        if ( x(1) .eq. n ) then
          go to 20
        end if

        call mono_upto_next_grlex ( m, n, x )
        i = i + 1

      go to 10

20    continue

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Now, given a monomial, retrieve its rank in the sequence:'
      write ( *, '(a)' ) ''

      do test = 1, test_num
        do i = 1, m
          x(i) = x_test(i,test)
        end do
        call mono_rank_grlex ( m, x, rank )
        write ( *, '(2x,i3,4x,3i2)' ) rank, x(1:m)
      end do

      return
      end
      subroutine mono_unrank_grlex_test ( )

c*********************************************************************72
c
cc MONO_UNRANK_GRLEX_TEST tests MONO_UNRANK_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )

      integer i
      integer i4_uniform_ab
      integer mono_upto_enum
      integer n
      integer rank
      integer rank_max
      integer seed
      integer test
      integer test_num
      integer x(m)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_UNRANK_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  MONO_UNRANK_GRLEX is given a rank, and returns the'
      write ( *, '(a)' ) 
     &  '  corresponding monomial in the sequence of all monomials '
      write ( *, '(a)' ) '  in M dimensions of degree N or less.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  For reference, print a monomial sequence with ranks.'

      n = 4
      rank_max = mono_upto_enum ( m, n )

      write ( *, '(a)' ) ''
      write ( *, '(a,i3)' ) '  Let M = ', m
      write ( *, '(a,i3)' ) '      N = ', n
      write ( *, '(a)' ) ''

      do i = 1, m
        x(i) = 0
      end do

      i = 1

10    continue

        write ( *, '(2x,i3,4x,3i2)' ) i, x(1:m)

        if ( x(m) .eq. n ) then
          go to 20
        end if

        call mono_upto_next_grlex ( m, n, x )
        i = i + 1

      go to 10

20    continue

      write ( *, '(a)' ) ''
      write ( *, '(a,i3)' ) 
     &  '  Now choose random ranks between 1 and ', rank_max
      write ( *, '(a)' ) ''

      seed = 123456789
      test_num = 5

      do test = 1, test_num

        rank = i4_uniform_ab ( 1, rank_max, seed )
        call mono_unrank_grlex ( m, rank, x )
        write ( *, '(2x,i3,4x,3i2)' ) rank, x(1:m)

      end do

      return
      end
      subroutine mono_upto_enum_test ( )

c*********************************************************************72
c
cc MONO_UPTO_ENUM_TEST tests MONO_UPTO_ENUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer mono_upto_enum
      integer n
      integer v

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_UPTO_ENUM_TEST'
      write ( *, '(a)' ) 
     &  '  MONO_UPTO_ENUM can enumerate the number of monomials'
      write ( *, '(a)' ) '  in M variables, of total degree 0 up to N.'

      write ( *, '(a)' ) ''
      write ( *, '(a)', advance = 'no' ) '    N:'
      do n = 0, 8
        write ( *, '(2x,i4)', advance = 'no' ) n
      end do
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   M +------------------------------------------------------'
      do m = 1, 8
        write ( *, '(2x,i2,a)', advance = 'no' ) m, ' |'
        do n = 0, 8
          v = mono_upto_enum ( m, n )
          write ( *, '(1x,i5)', advance = 'no' ) v
        end do
        write ( *, '(a)' ) ''
      end do

      return
      end
      subroutine mono_upto_next_grlex_test ( )

c*********************************************************************72
c
cc MONO_UPTO_NEXT_GRLEX_TEST tests MONO_UPTO_NEXT_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )

      integer i
      integer j
      integer n
      integer x(m)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  MONO_UPTO_NEXT_GRLEX can list the monomials'
      write ( *, '(a)' ) '  in M variables, of total degree up to N,'
      write ( *, '(a)' ) '  in graded lexicographic order,'
      write ( *, '(a)' ) '  one at a time.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  We start the process with (0,0,...,0,0).'
      write ( *, '(a)' ) '  The process ends with (N,0,...,0,0)'

      n = 4

      write ( *, '(a)' ) ''
      write ( *, '(a,i2)' ) '  Let M = ', m
      write ( *, '(a,i2)' ) '      N = ', n
      write ( *, '(a)' ) ''

      x(1) = 0
      x(2) = 0
      x(3) = 0

      i = 1

10    continue

        write ( *, '(2x,i2,4x,3i2)' ) i, x(1:m)

        if ( x(1) .eq. n ) then
          go to 20
        end if

        call mono_upto_next_grlex ( m, n, x )
        i = i + 1

      go to 10

20    continue

      return
      end
      subroutine mono_upto_random_test ( )

c*********************************************************************72
c
cc MONO_UPTO_RANDOM_TEST tests MONO_UPTO_RANDOM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )

      integer j
      integer n
      integer rank
      integer seed
      integer test
      integer test_num
      integer x(m)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_UPTO_RANDOM_TEST'
      write ( *, '(a)' ) 
     &  '  MONO_UPTO_RANDOM selects at random a monomial'
      write ( *, '(a)' ) 
     &  '  in M dimensions of total degree no greater than N.'

      n = 4

      write ( *, '(a)' ) ''
      write ( *, '(a,i3)' ) '  Let M = ', m
      write ( *, '(a,i3)' ) '      N = ', n
      write ( *, '(a)' ) ''

      seed = 123456789
      test_num = 5

      do test = 1, test_num
        call mono_upto_random ( m, n, seed, rank, x )
        write ( *, '(2x,i3,4x,3i2)' ) rank, x(1:m)
      end do

      return
      end
      subroutine mono_value_test ( )

c*********************************************************************72
c
cc MONO_VALUE_TEST tests MONO_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer nx
      parameter ( nx = 2 )

      integer f(m)
      integer j
      integer n
      integer rank
      integer seed
      integer test
      integer test_num
      double precision v(nx)
      double precision x(m,nx)

      save x

      data x /
     &   1.0D+00, 2.0D+00, 3.0D+00, 
     &  -2.0D+00, 4.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MONO_VALUE_TEST'
      write ( *, '(a)' ) '  MONO_VALUE evaluates a monomial.'

      n = 6

      write ( *, '(a)' ) ''
      write ( *, '(a,i3)' ) '  Let M = ', m
      write ( *, '(a,i3)' ) '      N = ', n

      seed = 123456789
      test_num = 5

      do test = 1, test_num

        call mono_upto_random ( m, n, seed, rank, f )
        write ( *, '(a)' ) ''
        call mono_print ( m, f, '  M(X) = ' )
        call mono_value ( m, nx, f, x, v )
        do j = 1, nx
          write ( *, '(a,f4.0,a,f4.0,a,f4.0,a,g14.6)' ) 
     &      '  M(', x(1,j), ',', x(2,j), ',', x(3,j), ') = ', v(j)
        end do
    
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
      subroutine polynomial_compress_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_COMPRESS_TEST tests POLYNOMIAL_COMPRESS.
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

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 10 )

      double precision c(o)
      double precision c2(o)
      integer e(o)
      integer e2(o)
      integer o2
      character * ( 80 ) title

      save c
      save e

      data c /
     &  7.0, - 5.0, 5.0, 9.0, 11.0, 3.0, 6.0, 0.0, - 13.0, 1.0E-20 /
      data e /
     & 1, 2, 2, 4, 5, 5, 5, 12, 33, 35 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_COMPRESS_TEST'
      write ( *, '(a)' ) 
     &  '  POLYNOMIAL_COMPRESS compresses a polynomial.'

      write ( *, '(a)' ) ''
      title = '  Uncompressed P(X) ='
      call polynomial_print ( m, o, c, e, title )

      call polynomial_compress ( o, c, e, o2, c2, e2 )

      write ( *, '(a)' ) ''
      title = '  Compressed P(X) ='
      call polynomial_print ( m, o2, c2, e2, title )

      return
      end
      subroutine polynomial_print_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_PRINT_TEST tests POLYNOMIAL_PRINT.
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

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      character * ( 80 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_PRINT_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_PRINT prints a polynomial.'

      c(1) = 7.0
      c(2) = - 5.0
      c(3) = 9.0
      c(4) = 11.0
      c(5) = 0.0
      c(6) = - 13.0

      e(1) = 1
      e(2) = 2
      e(3) = 4
      e(4) = 5
      e(5) = 12
      e(6) = 33

      title = '  P1(X) ='

      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_sort_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_SORT_TEST tests POLYNOMIAL_SORT.
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

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      character * ( 80 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_SORT_TEST'
      write ( *, '(a)' ) 
     &  '  POLYNOMIAL_SORT sorts a polynomial by exponent index.'

      c(1) =    0.0
      c(2) =    9.0
      c(3) =  - 5.0
      c(4) =  - 13.0
      c(5) =     7.0
      c(6) =    11.0

      e(1) = 12
      e(2) =  4
      e(3) =  2
      e(4) = 33
      e(5) =  1
      e(6) =  5

      title = '  Unsorted polynomial'
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      call polynomial_sort ( o, c, e )

      title = '  Sorted polynomial'
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_value_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_VALUE_TEST tests POLYNOMIAL_VALUE.
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

      integer m
      parameter ( m = 3 )
      integer nx
      parameter ( nx = 2 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      integer j
      double precision p(nx)
      character * ( 80 ) title
      double precision x(m,nx)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_VALUE_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

      c(1) = 7.0
      c(2) =  - 5.0
      c(3) =  9.0
      c(4) =  11.0
      c(5) =  0.0
      c(6) =  - 13.0

      e(1) = 1
      e(2) = 2
      e(3) = 4
      e(4) = 5
      e(5) = 12
      e(6) = 33

      title = '  P(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      x(1,1) = 1.0
      x(2,1) = 2.0
      x(3,1) = 3.0

      x(1,2) = -2.0
      x(2,2) = 4.0
      x(3,2) = 1.0

      call polynomial_value ( m, o, c, e, nx, x, p )
      do j = 1, nx
        write ( *, '(a,f10.4,a,f10.4,a,f10.4,a,g14.6)' ) 
     &    '  P(', x(1,j), ',', x(2,j), ',', x(3,j), ') = ', p(j)
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
