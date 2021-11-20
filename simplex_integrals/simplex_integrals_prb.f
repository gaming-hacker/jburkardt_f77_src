      program main

c*********************************************************************72
c
cc MAIN is the main program for SIMPLEX_INTEGRALS_PRB.
c
c  Discussion:
c
c    SIMPLEX_INTEGRALS_PRB tests the SIMPLEX_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIMPLEX_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SIMPLEX_INTEGRALS library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIMPLEX_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses SIMPLEX01_SAMPLE to compare exact and estimated integrals in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2014
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
      double precision r8vec_sum
      double precision result
      integer seed
      double precision simplex01_volume
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
     &  '  over the interior of the unit simplex in M dimensions.'
c
c  Get sample points.
c
      seed = 123456789
      call simplex01_sample ( m, n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose exponents.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  We randomly choose the exponents.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Ex  Ey  Ez     MC-Estimate      Exact           Error'
      write ( *, '(a)' ) ''

      do test = 1, test_num

        call i4vec_uniform_ab ( m, 0, 4, seed, e )

        call monomial_value ( m, n, e, x, value )

        result = simplex01_volume ( m ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call simplex01_monomial_integral ( m, e, exact )
        error = abs ( result - exact )

        write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses SIMPLEX01_SAMPLE to compare exact and estimated integrals in 6D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2014
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
      double precision r8vec_sum
      double precision result
      integer seed
      double precision simplex01_volume
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
     &  '  over the interior of the unit simplex in M dimensions.'
c
c  Get sample points.
c
      seed = 123456789
      call simplex01_sample ( m, n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Randomly choose exponents.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  We randomly choose the exponents.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  E1  E2  E3  E4  E5  E6     MC-Estimate' //
     &  '      Exact           Error'
      write ( *, '(a)' ) ''

      do test = 1, test_num

        call i4vec_uniform_ab ( m, 0, 3, seed, e )

        call monomial_value ( m, n, e, x, value )

        result = simplex01_volume ( m ) * r8vec_sum ( n, value ) 
     &    / dble ( n )
        call simplex01_monomial_integral ( m, e, exact )
        error = abs ( result - exact )

        write ( *, '(6(2x,i2),2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e(1:m), result, exact, error

      end do

      return
      end
