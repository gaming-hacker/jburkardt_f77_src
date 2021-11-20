      program main

c*********************************************************************72
c
cc MAIN is the main program for PYRAMID_INTEGRALS_PRB.
c
c  Discussion:
c
c    PYRAMID_INTEGRALS_PRB tests the PYRAMID_INTEGRALS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_INTEGRALS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PYRAMID_INTEGRALS library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_INTEGRALS_PRB'
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
c    14 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 500000 )

      integer e_max
      parameter ( e_max = 6 )
      integer e1
      integer e2
      integer e3
      integer expon(m)
      double precision error
      double precision exact
      double precision pyramid01_volume
      double precision q
      double precision r8vec_sum
      integer seed
      double precision value(n)
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Compare exact and estimated integrals '
      write ( *, '(a)' ) '  over the unit pyramid in 3D.'
c
c  Get sample points.
c
      seed = 123456789
      call pyramid01_sample ( n, seed, x )

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of sample points used is ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   E1  E2  E3     MC-Estimate      Exact           Error'
      write ( *, '(a)' ) ''
c
c  Check all monomials, with only even dependence on X or Y, 
c  up to total degree E_MAX.
c
      do e3 = 0, e_max
        expon(3) = e3
        do e2 = 0, e_max - e3, 2
          expon(2) = e2
          do e1 = 0, e_max - e3 - e2, 2
            expon(1) = e1

            call monomial_value ( m, n, expon, x, value )

            q = pyramid01_volume ( ) * r8vec_sum ( n, value ) 
     &        / dble ( n )
            call pyramid01_integral ( expon, exact )
            error = abs ( q - exact )

            write ( *, 
     &        '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6,2x,e10.2)' ) 
     &        expon(1:3), q, exact, error

          end do
        end do
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 examines the sample points in the pyramid
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 20 )

      integer seed
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Print sample points in the unit pyramid in 3D.'
      seed = 123456789
      call pyramid01_sample ( n, seed, x )
      call r8mat_transpose_print ( 3, n, x, '  Unit pyramid points' )

      return
      end

