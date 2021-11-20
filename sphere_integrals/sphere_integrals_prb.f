      program main

c*********************************************************************72
c
cc MAIN is the main program for SPHERE_INTEGRALS_PRB.
c
c  Discussion:
c
c    SPHERE_INTEGRALS_PRB tests the SPHERE_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2014
c
c  Author:
c
c    John Burkardt
c
      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_INTEGRALS_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPHERE_INTEGRALS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses SPHERE01_SAMPLE to estimate monomial integrands.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2014
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
      integer i
      double precision r8vec_sum
      double precision result
      integer seed
      double precision sphere01_area
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Estimate monomial integrals using Monte Carlo'
      write ( *, '(a)' ) '  over the surface of the unit sphere in 3D.'
c
c  Get sample points.
c
      seed = 123456789
      call sphere01_sample ( n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose X,Y,Z exponents between (0,0,0) and (9,9,9).
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
       
        result = sphere01_area ( ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call sphere01_monomial_integral ( e, exact )
        error = abs ( result - exact )

        write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:3), result, exact, error

      end do

      return
      end

