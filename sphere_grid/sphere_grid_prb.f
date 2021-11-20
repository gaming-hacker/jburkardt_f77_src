      program main

c*********************************************************************72
c
cc MAIN is the main program for SPHERE_GRID_PRB.
c
c  Discussion:
c
c    SPHERE_GRID_PRB tests routines from the SPHERE_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_GRID_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPHERE_GRID library.'
     
      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )

      call test10 ( )
      call test11 ( )
      call test12 ( )
      call test13 ( )
      call test14 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_GRID_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests SPHERE_GRID_ICOS_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer edge_num
      integer factor
      integer factor_log
      integer node_num
      integer triangle_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  SPHERE_GRID_ICOS_SIZE determines the size'
      write ( *, '(a)' ) 
     &  '  (number of vertices, edges and faces) in a grid'
      write ( *, '(a)' ) '  on a sphere, made by subdividing an initial'
      write ( *, '(a)' ) '  projected icosahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  N determines the number of subdivisions of each'
      write ( *, '(a)' ) '  edge of the icosahedral faces.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N         V         E         F'
      write ( *, '(a)' ) '  --------  --------  --------  --------'
      write ( *, '(a)' ) ' '

      do factor = 1, 20
        call sphere_grid_icos_size ( factor, node_num, edge_num, 
     &    triangle_num )
        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) 
     &    factor, node_num, edge_num, triangle_num
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Repeat, using N constrained by doubling:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N         V         E         F'
      write ( *, '(a)' ) '  --------  --------  --------  --------'
      write ( *, '(a)' ) ' '

      factor = 1
      do factor_log = 0, 10
        call sphere_grid_icos_size ( factor, node_num, edge_num, 
     &    triangle_num )
        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) 
     &    factor, node_num, edge_num, triangle_num
        factor = factor * 2
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests SPHERE_GRIDPOINTS_ICOS1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer node_max
      parameter ( node_max = 4002 )

      integer edge_num
      integer factor
      character ( len = 80 ) filename
      integer node
      integer node_num
      double precision node_xyz(3,node_max)
      integer triangle_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRID_ICOS_SIZE "sizes" a grid generated'
      write ( *, '(a)' ) 
     &  '  on an icosahedron and projected to a sphere.'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRIDPOINTS_ICOS1 creates the grid points.'

      factor = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Sizing factor =       ', factor

      call sphere_grid_icos_size ( factor, node_num, edge_num, 
     &  triangle_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of vertices =  ', node_num
      write ( *, '(a,i8)' ) '  Number of edges =     ', edge_num
      write ( *, '(a,i8)' ) '  Number of faces =     ', triangle_num

      call sphere_gridpoints_icos1 ( factor, node_num, node_xyz )

      call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 
     &  20, '  Initial part of NODE_XYZ array:' )
c
c  Write the nodes to a file.
c
      write ( filename, '(a,i1,a)' ) 
     &  'sphere_grid_icos1_f', factor, '.xyz'

      call r8mat_write ( filename, 3, node_num, node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Wrote grid nodes to "' // trim ( filename ) // '".'

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests SPHERE_GRIDPOINTS_ICOS2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer node_max
      parameter ( node_max = 4002 )

      integer edge_num
      integer factor
      character ( len = 80 ) filename
      integer node
      integer node_num
      double precision node_xyz(3,node_max)
      integer triangle_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRID_ICOS_SIZE "sizes" a grid generated'
      write ( *, '(a)' ) 
     &  '  on an icosahedron and projected to a sphere.'
      write ( *, '(a)' ) '  SPHERE_GRIDPOINTS_ICOS2 creates the grid.'

      factor = 3

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Sizing factor FACTOR = ', factor

      call sphere_grid_icos_size ( factor, node_num, edge_num, 
     &  triangle_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes =     ', node_num
      write ( *, '(a,i8)' ) '  Number of edges =     ', edge_num
      write ( *, '(a,i8)' ) '  Number of triangles = ', triangle_num

      call sphere_gridpoints_icos2 ( factor, node_num, node_xyz )

      call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 
     &  20, '  Initial part of NODE_XYZ array:' )
c
c  Write the nodes to a file.
c
      write ( filename, '(a,i1,a)' ) 
     &  'sphere_grid_icos2_f', factor, '.xyz'

      call r8mat_write ( filename, 3, node_num, node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Wrote grid nodes to "' // trim ( filename ) // '".'

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests SPHERE_LL_POINTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer lat_num
      parameter ( lat_num = 3 )
      integer long_num
      parameter ( long_num = 4 )

      integer i
      integer j
      integer k
      integer node_num
      double precision node_xyz(3,2+lat_num*long_num)
      double precision pc(3)
      double precision r

      pc(1) = 0.0D+00
      pc(2) = 0.0D+00
      pc(3) = 0.0D+00
      r = 10.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  SPHERE_LL_POINTS produces latitude'
      write ( *, '(a)' ) '  /longitude points on a sphere in 3D.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Radius = ', r

      call r8vec_print ( 3, pc, '  Center:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of latitudes =  ', lat_num
      write ( *, '(a,i8)' ) '  The number of longitudes = ', long_num

      call sphere_ll_point_num ( lat_num, long_num, node_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of grid points is ', node_num
      write ( *, '(a)' ) ' '

      call sphere_ll_points ( r, pc, lat_num, long_num, node_num,
     &  node_xyz )

      k = 1
      write ( *, '(2x,i8,3g14.6)' ) k, node_xyz(1:3,k)

      do i = 1, lat_num
        write ( *, '(a)' ) ' '
        do j = 0, long_num - 1
          k = k + 1
          write ( *, '(2x,i8,3g14.6)' ) k, node_xyz(1:3,k)
        end do
      end do

      k = k + 1
      write ( *, '(a)' ) ' '
      write ( *, '(2x,i8,3g14.6)' ) k, node_xyz(1:3,k)

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests SPHERE_SPIRALPOINTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 July 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer node_num
      parameter ( node_num = 500 )

      double precision center_xyz(3)
      character ( len = 80 ) filename
      integer node
      double precision node_xyz(3,node_num)
      double precision r

      center_xyz(1) = 0.0D+00
      center_xyz(2) = 0.0D+00
      center_xyz(3) = 0.0D+00
      r = 1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  SPHERE_SPIRALPOINTS produces a spiral of'
      write ( *, '(a)' ) '  points on an implicit sphere in 3D.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Radius = ', r

      call r8vec_print ( 3, center_xyz, '  Center:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The number of spiral points is ', node_num

      call sphere_spiralpoints ( r, center_xyz, node_num, node_xyz )

      call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 
     &  10, '  The spiral points:' )
c
c  Write the nodes to a file.
c
      write ( filename, '(a,i4.4,a)' ) 
     &  'sphere_grid_spiral_n', node_num, '.xyz'

      call r8mat_write ( filename, 3, node_num, node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Wrote grid nodes to "' // trim ( filename ) // '".'

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests SPHERE_LL_LINES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer line_max
      parameter ( line_max = 1000 )

      integer i
      integer lat_num
      integer line(2,line_max)
      integer line_num
      integer long_num

      lat_num = 3
      long_num = 4

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  SPHERE_LL_LINES produces latitude'
      write ( *, '(a)' ) '  /longitude lines on a sphere in 3D.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of latitudes is  ', lat_num
      write ( *, '(a,i8)' ) '  Number of longitudes is ', long_num

      call sphere_ll_lines ( lat_num, long_num, line_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of line segments is ', line_num

      call sphere_ll_lines ( lat_num, long_num, line_num, line )

      call i4mat_transpose_print ( 2, line_num, line, 
     &  '  Grid line vertices:' )

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests SPHERE_GRID_Q4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 July 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer lat_num
      parameter ( lat_num = 3 )
      integer long_num
      parameter ( long_num = 4 )
      integer rectangle_num
      parameter ( rectangle_num = lat_num * long_num )

      integer rectangle_node(4,rectangle_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  SPHERE_GRID_Q4 computes a grid of Q4'
      write ( *, '(a)' ) '  rectangular elements on a sphere in 3D.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of latitudes is      ', lat_num
      write ( *, '(a,i8)' ) '  Number of longitudes is     ', long_num
      write ( *, '(a,i8)' ) 
     &  '  The number of rectangles is ', rectangle_num

      call sphere_grid_q4 ( lat_num, long_num, rectangle_node )

      call i4mat_transpose_print ( 4, rectangle_num, rectangle_node, 
     &  '  Rectangle vertices:' )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests SPHERE_GRID_T3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer lat_num
      parameter ( lat_num = 3 )
      integer long_num
      parameter ( long_num = 4 )

      integer triangle_num
      integer triangle_node(3,2*(lat_num+1)*long_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  SPHERE_GRID_T3 computes a grid'
      write ( *, '(a)' ) '  of T3 triangular elements on a sphere.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of latitudes is  ', lat_num
      write ( *, '(a,i8)' ) '  Number of longitudes is ', long_num

      call sphere_grid_t3 ( lat_num, long_num, triangle_node )

      triangle_num = 2 * ( lat_num + 1 ) * long_num
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The number of triangles is ', triangle_num

      call i4mat_transpose_print ( 3, triangle_num, triangle_node, 
     &  '  Triangle vertices:' )

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 tests SPHERE_UNIT_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer node_num
      parameter ( node_num = 1000 )

      character ( len = 80 ) filename
      integer node
      double precision node_xyz(3,node_num )
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) '  For the unit sphere in 3 dimensions:'
      write ( *, '(a)' ) '  SPHERE_UNIT_SAMPLE does a random sampling.'

      call sphere_unit_sample ( node_num, seed, node_xyz )

      call r8mat_transpose_print_some ( 3, node_num, node_xyz, 1, 1, 3, 
     &  10, '  First 10 values:' )
c
c  Write the nodes to a file.
c
      write ( filename, '(a,i6.6,a)' ) 
     &  'sphere_sample_n', node_num, '.xyz'

      call r8mat_write ( filename, 3, node_num, node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Wrote grid nodes to "' // trim ( filename ) // '".'

      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 tests SPHERE_CUBED_POINTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 80 ) filename
      integer j
      integer n
      integer ns
      double precision xyz(3,602)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) 
     &  '  SPHERE_CUBED_POINTS computes points on a cubed sphere grid.'

      n = 10
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of divisions on each face = ', n

      call sphere_cubed_points_size ( n, ns );
      write ( *, '(a,i6)' ) '  Total number of points = ', ns

      call sphere_cubed_points ( n, ns, xyz )

      call r8mat_transpose_print_some ( 3, ns, xyz, 1, 1, 3, 20, 
     &  '  Initial part of XYZ array:' )
!
!  Write the nodes to a file.
!
      write ( filename, '(a,i6.6,a)' ) 'sphere_cubed_f', n, '.xyz'

      call r8mat_write ( filename, 3, ns, xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Wrote grid nodes to "' // trim ( filename ) // '".'

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 is a dummy routine.  Refer to the MATLAB source for details.
c
      return
      end
      subroutine test12 ( )

c*********************************************************************72
c
cc TEST12 is a dummy routine.  Refer to the MATLAB source for details.
c
      return
      end
      subroutine test13 ( )

c*********************************************************************72
c
cc TEST13 is a dummy routine.  Refer to the MATLAB source for details.
c
      return
      end
      subroutine test14 ( )

c*********************************************************************72
c
cc TEST14 tests SPHERE_FIBONACCI_POINTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      integer j
      double precision xyz(3,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST14'
      write ( *, '(a)' ) 
     &  '  SPHERE_FIBONACCI_POINTS computes points on a sphere'
      write ( *, '(a)' ) '  that lie on a Fibonacci spiral.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of points N = ', n

      call sphere_fibonacci_points ( n, xyz )

      call r8mat_transpose_print_some ( 3, n, xyz, 1, 1, 3, 20, 
     &  '  Initial part of XYZ array:' )
c
c  Create graphics data files.
c
      call get_unit ( data_unit )
      data_filename = 'sphere_grid_test14_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, n
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xyz(1:3,j)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Create graphics command file.
c
      call get_unit ( command_unit )
      command_filename = 'sphere_grid_test14_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "sphere_grid_test14.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 'set title "Sphere Fibonacci Grid"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set view equal xyz'
      write ( command_unit, '(a)' ) 
     &  'splot "' // trim ( data_filename ) // '"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )

      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
