      program main

c*********************************************************************72
c
cc MAIN is the main program for HPP_PRB.
c
c  Discussion:
c
c    HPP_PRB tests the HERMITE_PRODUCT_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HPP_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Test the HERMITE_PRODUCT_POLYNOMIAL library.'

      call hpp_test01 ( )
      call hpp_test015 ( )
      call hpp_test02 ( )
      call hpp_test03 ( )
      call hpp_test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HPP_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine hpp_test01 ( )

c*********************************************************************72
c
cc HPP_TEST01 tests routines for the GRLEX ordering of compositions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer k
      parameter ( k = 2 )

      integer i
      integer i4_uniform_ab
      integer i4vec_sum
      integer rank
      integer rank1
      integer rank2
      integer seed
      integer test
      integer x(k)
      integer x_sum
      integer x_sum_old

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HPP_TEST01:'
      write ( *, '(a)' ) 
     &  '  COMP_NEXT_GRLEX is given a composition, and computes the '
      write ( *, '(a)' ) '  next composition in grlex order.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rank   Sum   Components'

      do i = 1, k
        x(i) = 0
      end do
      x_sum_old = -1
      rank = 1

10    continue

        x_sum = i4vec_sum ( k, x )

        if ( x_sum_old .lt. x_sum ) then
          x_sum_old = x_sum
          write ( *, '(a)' ) ''
        end if

        write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
        do i = 1, k
          write ( *, '(i4)', advance = 'no' ) x(i)
        end do
        write ( *, '(a)' ) ''

        if ( 20 .le. rank ) then
          go to 20
        end if

        call comp_next_grlex ( k, x )
        rank = rank + 1

      go to 10

20    continue

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  COMP_UNRANK_GRLEX is given a rank and returns the'
      write ( *, '(a)' ) '  corresponding set of multinomial exponents.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rank   Sum   Components'
      write ( *, '(a)' ) ''

      seed = 123456789

      do test = 1, 5
        rank = i4_uniform_ab ( 1, 20, seed )
        call comp_unrank_grlex ( k, rank, x )
        x_sum = i4vec_sum ( k, x )
        write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
        do i = 1, k
          write ( *, '(i4)', advance = 'no' ) x(i)
        end do
        write ( *, '(a)' ) ''
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  COMP_RANDOM_GRLEX randomly selects a composition'
      write ( *, '(a)' ) '  between given lower and upper ranks.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rank   Sum   Components'
      write ( *, '(a)' ) ''

      seed = 123456789
      rank1 = 5
      rank2 = 20

      do test = 1, 5
        call comp_random_grlex ( k, rank1, rank2, seed, x, rank )
        x_sum = i4vec_sum ( k, x )
        write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
        do i = 1, k
          write ( *, '(i4)', advance = 'no' ) x(i)
        end do
        write ( *, '(a)' ) ''
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  COMP_RANK_GRLEX returns the rank of a given composition.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rank   Sum   Components'
      write ( *, '(a)' ) ''

      x(1) = 4
      x(2) = 0
      call comp_rank_grlex ( k, x, rank )
      x_sum = i4vec_sum ( k, x )
      write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
      do i = 1, k
        write ( *, '(i4)', advance = 'no' ) x(i)
      end do
      write ( *, '(a)' ) ''

      x(1) = 11
      x(2) = 5
      call comp_rank_grlex ( k, x, rank )
      x_sum = i4vec_sum ( k, x )
      write ( *, '(2x,i4,2x,i4,a)', advance = 'no' ) rank, x_sum, ':'
      do i = 1, k
        write ( *, '(i4)', advance = 'no' ) x(i)
      end do
      write ( *, '(a)' ) ''

      return
      end
      subroutine hpp_test015 ( )

c*********************************************************************72
c
cc HPP_TEST015 tests HP_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c(3)
      integer e(3)
      integer f(3)
      integer l(1)
      integer m
      integer n
      integer o
      integer o_max
      character * ( 255 ) title

      m = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HPP_TEST015:'
      write ( *, '(a)' ) 
     &  '  HEP_COEFFICIENTS computes the coefficients and'
      write ( *, '(a)' ) '  exponents of the Hermite polynomial He(n,x).
     &'

      do n = 1, 5

        call hep_coefficients ( n, o, c, f )

        l(1) = n
        o_max = o

        call hepp_to_polynomial ( m, l, o_max, o, c, e )

        write ( *, '(a)' ) ''
        write ( title, '(a,i1,a)' ) '  He(', n, ',x) ='

        call polynomial_print ( m, o, c, e, title )

      end do

      return
      end
      subroutine hpp_test02 ( )

c*********************************************************************72
c
cc HPP_TEST02 tests HP_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision e
      integer m
      integer n_data
      integer o
      double precision x
      double precision fx1
      double precision fx2

      m = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HPP_TEST02:'
      write ( *, '(a)' ) '  HEP_VALUES stores values of'
      write ( *, '(a)' ) '  the Hermite polynomial He(n,x).'
      write ( *, '(a)' ) '  HEP_VALUE evaluates a Hermite polynomial.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '                        Tabulated                 Computed'
      write ( *, '(a)', advance = 'no' )
     &  '     O        X          He(O,X)                   He(O,X)'
      write ( *, '(a)' ) '                   Error'
      write ( *, '(a)' ) ''

      n_data = 0

10    continue

        call hep_values ( n_data, o, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call hep_value ( m, o, x, fx2 )

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.8,2x,g24.16,2x,g24.16,2x,g8.2)' )     
     &    o, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine hpp_test03 ( )

c*********************************************************************72
c
cc HPP_TEST03 tests HEPP_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2014
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

      double precision c(10)
      integer e(10)
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
      write ( *, '(a)' ) 'HPP_TEST03:'
      write ( *, '(a)' ) 
     &  '  HEPP_VALUE evaluates a Hermite product polynomial.'
      write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

      xlo = -1.0D+00
      xhi = +1.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( m, xlo, xhi, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,3g14.6)' ) '  Evaluate at X = ', x(1:3,1)
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Rank  I1  I2  I3:  He(I1,X1)*He(I2,X2)*He(I3,X3)' //
     &  '    P(X1,X2,X3)'
      write ( *, '(a)' ) ''

      do rank = 1, 20

        call comp_unrank_grlex ( m, rank, l )
c
c  Evaluate the HePP directly.
c
        call hepp_value ( m, n, l, x, v1 )
c
c  Convert the HePP to a polynomial.
c
        o_max = 1
        do i = 1, m
          o_max = o_max * ( ( l(i) + 2 ) / 2 )
        end do

        call hepp_to_polynomial ( m, l, o_max, o, c, e )
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
      subroutine hpp_test04 ( )

c*********************************************************************72
c
cc HPP_TEST04 tests HEPP_TO_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )

      double precision c(10)
      integer e(10)
      integer i
      integer l(m)
      character * ( 255 ) label
      integer o
      integer o_max
      integer rank

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HPP_TEST04:'
      write ( *, '(a)' ) 
     &  '  HEPP_TO_POLYNOMIAL is given a Hermite product polynomial'
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

        call hepp_to_polynomial ( m, l, o_max, o, c, e )

        write ( label, '(a,i2,a,i2,a,i2,a)' ) '  HePP #', rank,   
     &    ' = L(', l(1),       ',X)*L(', l(2),       ',Y) ='

        write ( *, '(a)' ) ''
        call polynomial_print ( m, o, c, e, label )

      end do

      return
      end
