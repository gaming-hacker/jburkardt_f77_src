      program main

c*********************************************************************72
c
cc MAIN is the main program for HYPERBALL_INTEGRALS_PRB.
c
c  Discussion:
c
c    HYPERBALL_INTEGRALS_PRB tests the HYPERBALL_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERBALL_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BALL_INTEGRALS library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERBALL_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses HYPERBALL01_SAMPLE to compare exact and estimated integrals in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 4192 )

      integer e(m)
      double precision error
      double precision exact
      double precision hyperball01_volume
      integer i
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Use the Monte Carlo method to estimate integrals over'
      write ( *, '(a)' ) 
     &  '  the interior of the unit hyperball in M dimensions.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
c
c  Get sample points.
c
      seed = 123456789
      call hyperball01_sample ( m, n, seed, x )
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose exponents between 0 and 8.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  If any exponent is odd, the integral is zero.'
      write ( *, '(a)' ) 
     &  '  We restrict this test to randomly chosen even exponents.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Ex  Ey  Ez     MC-Estimate           Exact      Error'
      write ( *, '(a)' ) ''

      do test = 1, test_num

        call i4vec_uniform_ab ( m, 0, 4, seed, e )

        do i = 1, m
          e(i) = e(i) * 2
        end do

        call monomial_value ( m, n, e, x, value )

        result = hyperball01_volume ( m ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call hyperball01_monomial_integral ( m, e, exact )
        error = abs ( result - exact )

        write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses HYPERBALL01_SAMPLE to compare exact and estimated integrals in 6D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 4192 )

      integer e(m)
      double precision error
      double precision exact
      double precision hyperball01_volume
      integer i
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Use the Monte Carlo method to estimate integrals over'
      write ( *, '(a)' ) 
     &  '  the interior of the unit hyperball in M dimensions.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
c
c  Get sample points.
c
      seed = 123456789
      call hyperball01_sample ( m, n, seed, x )
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose exponents between 0 and 6.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  If any exponent is odd, the integral is zero.'
      write ( *, '(a)' ) 
     &  '  We restrict this test to randomly chosen even exponents.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  E1  E2  E3  E4  E5  E6     MC-Estimate           ' //
     &  'Exact      Error'
      write ( *, '(a)' ) ''

      do test = 1, test_num

        call i4vec_uniform_ab ( m, 0, 3, seed, e )

        do i = 1, m
          e(i) = e(i) * 2
        end do

        call monomial_value ( m, n, e, x, value )

        result = hyperball01_volume ( m ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call hyperball01_monomial_integral ( m, e, exact )
        error = abs ( result - exact )

        write ( *, '(6(2x,i2),2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
