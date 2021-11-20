      program main

c*********************************************************************72
c
cc POLYGON_GRID_TEST tests the POLYGON_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_GRID_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the POLYGON_GRID library.'

      call polygon_grid_count_test ( )

      call polygon_grid_points_test01 ( )
      call polygon_grid_points_test02 ( )
      call polygon_grid_points_test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_GRID_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine polygon_grid_count_test ( )

c*********************************************************************72
c
cc POLYGON_GRID_COUNT_TEST tests POLYGON_GRID_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer ng
      integer nv

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_GRID_COUNT_TEST:'
      write ( *, '(a)' ) 
     &  '  POLYGON_GRID_COUNT counts NG, the number of points in'
      write ( *, '(a)' ) 
     &  '  a grid defined with N+1 points on each side of a'
      write ( *, '(a)' ) '  polygon of NV vertices.'

      do nv = 3, 5
        write ( *, '(a)' ) ''
        write ( *, '(a,i2)' ) '  Polygonal vertex count NV = ', nv
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '   N     NG'
        write ( *, '(a)' ) ''
        do n = 0, 5
          call polygon_grid_count ( n, nv, ng )
          write ( *, '(2x,i2,2x,i5)' ) n, ng
        end do
      end do

      return
      end
      subroutine polygon_grid_points_test01 ( )

c*********************************************************************72
c
cc POLYGON_GRID_POINTS_TEST01 tests POLYGON_GRID_POINTS
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nv
      parameter ( nv = 3 )

      character * ( 255 ) filename
      integer n
      integer ng
      character * ( 255 ) prefix
      double precision v(2,nv)
      double precision xg(2,1000)

      save v

      data v /
     &  0.0D+00, 0.0D+00, 
     &  1.0D+00, 0.0D+00, 
     &  0.5D+00, 0.86602540378443860D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_GRID_POINTS_TEST01:'
      write ( *, '(a)' ) 
     &  '  POLYGON_GRID_POINTS returns grid points for a polygon'
      write ( *, '(a)' ) '  of NV vertices, with N+1 points on a side'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  For this test, the polygon is a triangle.'

      call r8mat_transpose_print ( 2, nv, v, '  Polygon vertices:' )
c
c  Count the grid points.
c
      n = 5
      call polygon_grid_count ( n, nv, ng )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  N = ', n
      write ( *, '(a,i4)' ) '  Number of grid points will be NG = ', ng
c
c  Compute the grid points.
c
      call polygon_grid_points ( n, nv, v, ng, xg )

      call r8mat_transpose_print ( 2, ng, xg, '  Grid points:' )
c
c  Display the points.
c
      prefix = 'triangle'

      call polygon_grid_display ( n, nv, v, ng, xg, prefix )
c
c  Write the points to a file.
c
      filename = 'triangle.xy'

      call r8mat_write ( filename, 2, ng, xg )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Data written to "' // trim ( filename ) // '"'

      return
      end
      subroutine polygon_grid_points_test02 ( )

c*********************************************************************72
c
cc POLYGON_GRID_POINTS_TEST02 tests POLYGON_GRID_POINTS
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nv
      parameter ( nv = 4 )

      character * ( 255 ) filename
      integer n
      integer ng
      character * ( 255 ) prefix
      double precision v(2,nv)
      double precision xg(2,1000)

      save v

      data v /
     &  1.0D+00, 1.0D+00, 
     &  2.0D+00, 0.0D+00, 
     &  4.0D+00, 3.0D+00, 
     &  0.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_GRID_POINTS_TEST02:'
      write ( *, '(a)' ) 
     &  '  POLYGON_GRID_POINTS returns grid points for a polygon'
      write ( *, '(a)' ) '  of NV vertices, with N+1 points on a side'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  For this test, the polygon is a convex quadrilateral'
      write ( *, '(a)' ) '  with sides of varying length.'
c
c  Define the polygon.
c
      call r8mat_transpose_print ( 2, nv, v, '  Polygon vertices:' )
c
c  Count the grid points.
c
      n = 7
      call polygon_grid_count ( n, nv, ng )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  N = ', n
      write ( *, '(a,i4)' ) '  Number of grid points will be NG = ', ng
c
c  Compute the grid points.
c
      call polygon_grid_points ( n, nv, v, ng, xg )

      call r8mat_transpose_print ( 2, ng, xg, '  Grid points:' )
c
c  Display the points.
c
      prefix = 'quad'

      call polygon_grid_display ( n, nv, v, ng, xg, prefix )
c
c  Write the points to a file.
c
      filename = 'quad.xy'

      call r8mat_write ( filename, 2, ng, xg )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Data written to "' // trim ( filename ) // '"'

      return
      end
      subroutine polygon_grid_points_test03 ( )

c*********************************************************************72
c
cc POLYGON_GRID_POINTS_TEST03 tests POLYGON_GRID_POINTS
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nv
      parameter ( nv = 6 )

      character * ( 255 ) filename
      integer n
      integer ng
      character * ( 255 ) prefix
      double precision v(2,nv)
      double precision xg(2,1000)

      save v

      data v /
     &  0.0D+00, 0.0D+00, 
     &  2.0D+00, 0.0D+00, 
     &  2.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 
     &  1.0D+00, 2.0D+00, 
     &  0.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYGON_GRID_POINTS_TEST03:'
      write ( *, '(a)' ) 
     &  '  POLYGON_GRID_POINTS returns grid points for a polygon'
      write ( *, '(a)' ) '  of NV vertices, with N+1 points on a side'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  For this test, the polygon is nonconvex and six sided.'
      write ( *, '(a)' ) 
     &  '  Two degenerate triangles are created, and some grid points'
      write ( *, '(a)' ) '  are generated several times.'
c
c  Define the polygon.
c
      call r8mat_transpose_print ( 2, nv, v, '  Polygon vertices:' )
c
c  Count the grid points.
c
      n = 5
      call polygon_grid_count ( n, nv, ng )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  N = ', n
      write ( *, '(a,i4)' ) '  Number of grid points will be NG = ', ng
c
c  Compute the grid points.
c
      call polygon_grid_points ( n, nv, v, ng, xg )

      call r8mat_transpose_print ( 2, ng, xg, '  Grid points:' )
c
c  Display the points.
c
      prefix = 'ell'

      call polygon_grid_display ( n, nv, v, ng, xg, prefix )
c
c  Write the points to a file.
c
      filename = 'ell.xy'

      call r8mat_write ( filename, 2, ng, xg )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Data written to "' // trim ( filename ) // '"'

      return
      end

