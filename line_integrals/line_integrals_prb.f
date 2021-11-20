      program main

c*********************************************************************72
c
cc MAIN is the main program for LINE_INTEGRALS_PRB.
c
c  Discussion:
c
c    LINE_INTEGRALS_PRB tests the LINE_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LINE_INTEGRALS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 compares exact and estimated monomial integrals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 1 )
      integer n
      parameter ( n = 4192 )

      integer e
      double precision error
      double precision exact
      double precision line01_length
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 11 )
      double precision value(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Compare exact and estimated integrals '
      write ( *, '(a)' ) '  over the length of the unit line in 1D.'
c
c  Get sample points.
c
      seed = 123456789
      call line01_sample ( n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   E     MC-Estimate      Exact           Error'
      write ( *, '(a)' ) ''

      do test = 1, test_num

        e = test - 1

        call monomial_value_1d ( n, e, x, value )

        result = line01_length ( ) * r8vec_sum ( n, value ) / dble ( n )
        call line01_monomial_integral ( e, exact )
        error = abs ( result - exact )

        write ( *, '(2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &    e, result, exact, error

      end do

      return
      end
