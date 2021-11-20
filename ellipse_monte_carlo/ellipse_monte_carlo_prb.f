      program main

c*********************************************************************72
c
cc MAIN is the main program for ELLIPSE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    ELLIPSE_MONTE_CARLO_PRB tests the ELLIPSE_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 November 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPSE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ELLIPSE_MONTE_CARLO library.'

      call ellipse_area1_test ( )
      call ellipse_area2_test ( )
      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPSE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine ellipse_area1_test ( )

c*********************************************************************72
c
cc ELLIPSE_AREA1_TEST tests ELLIPSE_AREA1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 November 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a(2,2)
      double precision area
      double precision ellipse_area1
      double precision r

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELLIPSE_AREA1_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  ELLIPSE_AREA1 computes the area of an ellipse.'

      r = 10.0D+00

      a(1,1) = 5.0D+00
      a(2,1) = 1.0D+00
      a(1,2) = 1.0D+00
      a(2,2) = 2.0D+00

      area = ellipse_area1 ( a, r )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  R = ', r
      call r8mat_print ( 2, 2, a, 
     &  '  Matrix A in ellipse definition x*A*x=r^2' )
      write ( *, '(a,g14.6)' ) '  Area = ', area
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELLIPSE_AREA1_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'

      return
      end
      subroutine ellipse_area2_test ( )

c*********************************************************************72
c
cc ELLIPSE_AREA2_TEST tests ELLIPSE_AREA2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 November 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision area
      double precision b
      double precision c
      double precision d
      double precision ellipse_area2
 
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELLIPSE_AREA2_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  ELLIPSE_AREA2 computes the area of an ellipse.'

      a = 5.0D+00
      b = 2.0D+00
      c = 2.0D+00
      d = 10.0D+00

      area = ellipse_area2 ( a, b, c, d )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,a,g14.6,a,g14.6,a,g14.6)' ) 
     &  '  Ellipse: ', a, ' * x^2 + ', b, 
     &  ' * xy + ', c, ' * y^2 = ', d
      write ( *, '(a,g14.6)' ) '  Area = ', area
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELLIPSE_AREA2_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses ELLIPSE01_SAMPLE with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65536 )

      double precision a(2,2)
      double precision disk01_area
      integer e(2)
      integer e_test(2,7)
      double precision ellipse_area1
      double precision error
      double precision exact
      integer j
      integer n
      double precision r
      parameter ( r = 2.0D+00 )
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision value(n_max)
      double precision x(2,n_max)

      save a
      save e_test

      data a /
     &  9.0, 1.0, 
     &  1.0, 4.0 /
      data e_test /
     &  0, 0, 
     &  1, 0, 
     &  0, 1, 
     &  2, 0, 
     &  1, 1, 
     &  0, 2, 
     &  3, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use ELLIPSE01_SAMPLE to estimate integrals'
      write ( *, '(a)' ) '  in the ellipse x'' * A * x <= r^2.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1              X               Y  ' //
     &  '             X^2               XY             Y^2   ' //
     &  '          X^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 65536 ) then

        call ellipse_sample ( n, a, r, seed, x )

        do j = 1, 7

          e(1) = e_test(1,j)
          e(2) = e_test(2,j)

          call monomial_value ( 2, n, e, x, value )

          result(j) = ellipse_area1 ( a, r ) * r8vec_sum ( n, value ) 
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      return
      end
