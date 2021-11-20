      program main

c*********************************************************************72
c
cc MAIN is the main program for HYPERSPHERE_INTEGRALS_PRB.
c
c  Discussion:
c
c    HYPERSPHERE_INTEGRALS_PRB tests the HYPERSPHERE_INTEGRALS library.
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

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERSPHERE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the HYPERSPHERE_INTEGRALS library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERSPHERE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses HYPERSPHERE01_SAMPLE to estimate monomial integrands in 3D.
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
      double precision hypersphere01_area
      integer i
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Estimate monomial integrals using Monte Carlo'
      write ( *, '(a)' ) 
     &  '  over the surface of the unit hypersphere in 3D.'
c
c  Get sample points.
c
      seed = 123456789
      call hypersphere01_sample ( m, n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
!
!  Randomly choose X,Y,Z exponents between (0,0,0) and (8,8,8).
!
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

        result = hypersphere01_area ( m ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call hypersphere01_monomial_integral ( m, e, exact )
        error = abs ( result - exact )

        write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses HYPERSPHERE01_SAMPLE to estimate monomial integrands in 6D.
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
      parameter ( m = 6 )
      integer n
      parameter ( n = 4192 )

      integer e(m)
      double precision error
      double precision exact
      double precision hypersphere01_area
      integer i
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Estimate monomial integrals using Monte Carlo'
      write ( *, '(a)' ) 
     &  '  over the surface of the unit hypersphere in 6D.'
c
c  Get sample points.
c
      seed = 123456789
      call hypersphere01_sample ( m, n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
!
!  Randomly choose X,Y,Z exponents between (0,0,0) and (6,6,6).
!
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

        call i4vec_uniform_ab ( m, 0, 3, seed, e )

        do i = 1, m
          e(i) = e(i) * 2
        end do

        call monomial_value ( m, n, e, x, value )

        result = hypersphere01_area ( m ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call hypersphere01_monomial_integral ( m, e, exact )
        error = abs ( result - exact )

        write ( *, '(6(2x,i2),2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
