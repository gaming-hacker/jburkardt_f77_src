      program main

c*********************************************************************72
c
cc SPHERE_FIBONACCI_GRID_TEST tests the SPHERE_FIBONACCI_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_FIBONACCI_GRID_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPHERE_FIBONACCI_GRID library.'

      call sphere_fibonacci_grid_points_test ( )
      call sphere_fibonacci_grid_display_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_FIBONACCI_GRID_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine sphere_fibonacci_grid_points_test ( )

c*********************************************************************72
c
cc SPHERE_FIBONACCI_GRID_POINTS_TEST tests SPHERE_FIBONACCI_GRID_POINTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ng
      parameter ( ng = 1000 )

      character * ( 255 ) filename
      double precision xg(3,ng)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_FIBONACCI_GRID_POINTS_TEST'
      write ( *, '(a)' ) 
     &  '  SPHERE_FIBONACCI_GRID_POINTS computes points on a sphere'
      write ( *, '(a)' ) '  that lie on a Fibonacci spiral.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of points NG = ', ng

      call sphere_fibonacci_grid_points ( ng, xg )

      call r8mat_transpose_print_some ( 3, ng, xg, 1, 1, 3, 10, 
     &  '  Part of the grid array:' )
c
c  Write the nodes to a file.
c
      filename = 'sphere_fibonacci_grid_n1000.xyz'

      call r8mat_write ( filename, 3, ng, xg )

      return
      end
      subroutine sphere_fibonacci_grid_display_test ( )

c*********************************************************************72
c
cc SPHERE_FIBONACCI_GRID_DISPLAY_TEST tests SPHERE_FIBONACCI_GRID_DISPLAY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ng
      parameter ( ng = 1000 )

      character * ( 255 ) prefix
      double precision xg(3,ng)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_FIBONACCI_GRID_DISPLAY_TEST'
      write ( *, '(a)' ) 
     &  '  SPHERE_FIBONACCI_GRID_DISPLAY displays points'
      write ( *, '(a)' ) '  on a sphere that lie on a Fibonacci spiral.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of points NG = ', ng

      call sphere_fibonacci_grid_points ( ng, xg )
c
c  Display the nodes on a sphere.
c
      prefix = 'sphere_fibonacci_grid_n1000'

      call sphere_fibonacci_grid_display ( ng, xg, prefix )

      return
      end
