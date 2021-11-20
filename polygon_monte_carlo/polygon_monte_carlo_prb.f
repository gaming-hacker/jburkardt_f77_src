      program main

c*********************************************************************72
c
cc MAIN is the main program for POLYGON_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    POLYGON_MONTE_CARLO_PRB tests the POLYGON_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nv1 
      parameter ( nv1 = 4 )

      double precision v1(2,nv1)

      save v1

      data v1 /
     &  -1.0, -1.0, 
     &   1.0, -1.0, 
     &   1.0,  1.0, 
     &  -1.0,  1.0 /

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYGON_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the POLYGON_MONTE_CARLO library.'

      call test01 ( nv1, v1 )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYGON_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( nv, v )

c*****************************************************************************80
c
c! TEST01 estimates integrals over a polygon in 2D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65536 )
      integer nv

      integer e(2)
      integer e_test(2,7)
      double precision error
      double precision exact
      integer j
      integer n
      double precision polygon_area
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision v(2,nv)
      double precision value(n_max)
      double precision x(2,n_max)

      save e_test

      data e_test /
     &  0, 0, 
     &  2, 0, 
     &  0, 2, 
     &  4, 0, 
     &  2, 2, 
     &  0, 4, 
     &  6, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use POLYGON_SAMPLE to estimate integrals '
      write ( *, '(a)' ) '  over the interior of a polygon in 2D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N' // 
     &  '        1' // 
     &  '              X^2 ' // 
     &  '             Y^2' // 
     &  '             X^4' // 
     &  '           X^2Y^2' // 
     &  '             Y^4' // 
     &  '           X^6'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 65536 ) then

        call polygon_sample ( nv, v, n, seed, x )

        do j = 1, 7

          e(1:2) = e_test(1:2,j)

          call monomial_value ( 2, n, e, x, value )

          result(j) = polygon_area ( nv, v ) * r8vec_sum ( n, value ) 
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      write ( *, '(a)' ) ' '

      do j = 1, 7

        e(1:2) = e_test(1:2,j)

        call polygon_monomial_integral ( nv, v, e, result(j) )

      end do

      write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

      return
      end
