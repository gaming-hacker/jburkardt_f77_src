      program main

c*********************************************************************72
c
cc MAIN is the main program for TETRAHEDRON_INTEGRALS_PRB.
c
c  Discussion:
c
c    TETRAHEDRON_INTEGRALS_PRB tests the TETRAHEDRON_INTEGRALS library.
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
      write ( *, '(a)' ) 'TETRAHEDRON_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TETRAHEDRON_INTEGRALS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_INTEGRALS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses TETRAHEDRON01_SAMPLE to compare exact and estimated integrals.
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
      integer i
      integer j
      integer k
      double precision r8vec_sum
      double precision result
      integer seed
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision tetrahedron01_volume
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Estimate monomial integrals using Monte Carlo'
      write ( *, '(a)' ) 
     &  '  over the interior of the unit tetrahedron in 3D.'
c
c  Get sample points.
c
      seed = 123456789
      call tetrahedron01_sample ( n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
c
c  Run through the exponents.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Ex  Ey  Ez     MC-Estimate      Exact           Error'
      write ( *, '(a)' ) ''

      do i = 0, 3
        e(1) = i
        do j = 0, 3
          e(2) = j
          do k = 0, 3
            e(3) = k

            call monomial_value ( m, n, e, x, value )

            result = tetrahedron01_volume ( ) 
     &        * r8vec_sum ( n, value ) / dble ( n )

            call tetrahedron01_monomial_integral ( e, exact )

            error = abs ( result - exact )

            write ( *, 
     &        '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &        e(1:m), result, exact, error

          end do
        end do
      end do

      return
      end
