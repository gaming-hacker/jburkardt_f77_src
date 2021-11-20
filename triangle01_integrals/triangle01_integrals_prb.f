      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE01_INTEGRALS_PRB.
c
c  Discussion:
c
c    TRIANGLE01_INTEGRALS_PRB tests the TRIANGLE01_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE01_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TRIANGLE01_INTEGRALS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE01_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses TRIANGLE01_SAMPLE to compare exact and estimated monomial integrals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2014
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
      integer i
      integer j
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision triangle01_area
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Estimate monomial integrals using Monte Carlo'
      write ( *, '(a)' ) 
     &  '  over the interior of the unit triangle in 2D.'
c
c  Get sample points.
c
      seed = 123456789
      call triangle01_sample ( n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose X, Y exponents.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  We restrict this test to randomly chosen even exponents.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Ex  Ey     MC-Estimate      Exact           Error'
      write ( *, '(a)' ) ''

      do i = 0, 4
        e(1) = i
        do j = 0, 4
          e(2) = j

          call monomial_value ( m, n, e, x, value )

          result = triangle01_area ( ) * r8vec_sum ( n, value ) 
     &      / dble ( n )
          call triangle01_monomial_integral ( e, exact )
          error = abs ( result - exact )

          write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &      e(1:m), result, exact, error

        end do
      end do

      return
      end
