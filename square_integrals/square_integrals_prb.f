      program main

c*********************************************************************72
c
cc MAIN is the main program for SQUARE_INTEGRALS_PRB.
c
c  Discussion:
c
c    SQUARE_INTEGRALS_PRB tests the SQUARE_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SQUARE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SQUARE_INTEGRALS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SQUARE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 compares exact and estimated integrals over the unit square in 2D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n
      parameter ( n = 4192 )

      integer e(m)
      double precision error
      double precision exact
      double precision r8vec_sum
      double precision result
      integer seed
      double precision square01_area
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Compare exact and estimated integrals'
      write ( *, '(a)' ) '  over the interior of the unit square in 2D.'
c
c  Get sample points.
c
      seed = 123456789
      call square01_sample ( n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose exponents.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Ex  Ey     MC-Estimate           Exact      Error'
      write ( *, '(a)' ) ''

      do test = 1, test_num

        call i4vec_uniform_ab ( m, 0, 7, seed, e )

        call monomial_value ( m, n, e, x, value )

        result = square01_area ( ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call square01_monomial_integral ( e, exact )
        error = abs ( result - exact )

        write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
