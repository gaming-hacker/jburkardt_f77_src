      program main

c*********************************************************************72
c
cc MAIN is the main program for DISK_GRID_PRB.
c
c  Discussion:
c
c    DISK_GRID_PRB tests the DISK_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DISK_GRID_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the DISK_GRID library.'

      call disk_grid_test01 ( )
      call disk_grid_test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DISK_GRID_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine disk_grid_test01 ( )

c*********************************************************************72
c
cc DISK_GRID_TEST01 tests DISK_GRID.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c(2)
      double precision cg(2,4000)
      character * ( 80 ) boundary_filename
      integer boundary_unit
      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      character * ( 80 ) filename
      integer i
      integer n
      integer ng
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  DISK_GRID can define a grid of points'
      write ( *, '(a)' ) 
     &  '  with N+1 points on a horizontal or vertical radius,'
      write ( *, '(a)' ) '  based on any disk.'

      n = 20
      r = 2.0D+00
      c(1) = 1.0D+00
      c(2) = 5.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  We use N = ', n
      write ( *, '(a,g14.6)' ) '  Radius R = ', r
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  Center C = (', c(1), ',', c(2), ')'

      call disk_grid_count ( n, r, c, ng )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of grid points will be ', ng

      call disk_grid ( n, r, c, ng, cg )

      call r82vec_print_part ( ng, cg, 20, 
     &  '  Part of the grid point array:' )
c
c  Write the coordinate data to a file.
c
      filename = 'disk_grid_test01.xy'

      call r8mat_write ( filename, 2, ng, cg )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Data written to the file "' // trim ( filename ) // '".'
c
c  Create graphics data files.
c
      call get_unit ( boundary_unit )
      boundary_filename = 'disk_grid_test01_boundary.txt'
      open ( unit = boundary_unit, file = boundary_filename, 
     &  status = 'replace' )
      do i = 0, 50
        t = 2.0D+00 * pi * dble ( i ) / 50.0D+00
        write ( boundary_unit, '(2x,g14.6,2x,g14.6)' ) 
     &    c(1) + r * cos ( t ), c(2) + r * sin ( t )
      end do
      close ( unit = boundary_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created boundary file "' // 
     &  trim ( boundary_filename ) // '".'

      call get_unit ( data_unit )
      data_filename = 'disk_grid_test01_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, ng
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) cg(1:2,i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Create graphics command file.
c
      call get_unit ( command_unit )
      command_filename = 'disk_grid_test01_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "disk_grid_test01.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set title "Disk Grid"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) // 
     &  '" using 1:2 with points lt 3 pt 3,\'
      write ( command_unit, '(a)' ) '    "' // 
     &  trim ( boundary_filename ) // 
     & '" using 1:2 lw 3 linecolor rgb "black"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine disk_grid_test02 ( )

c*********************************************************************72
c
cc DISK_GRID_TEST02 tests DISK_GRID_FIBONACCI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision c(2)
      character * ( 80 ) boundary_filename
      integer boundary_unit
      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      character * ( 80 ) filename
      double precision g(2,n)
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  DISK_GRID_FIBONACCI can define a grid of N points'
      write ( *, '(a)' ) 
     &  '  based on a Fibonacci spiral inside a disk.'

      r = 2.0D+00
      c(1) = 1.0D+00
      c(2) = 5.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  We use N = ', n
      write ( *, '(a,g14.6)' ) '  Radius R = ', r
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  Center C = (', c(1), ',', c(2), ')'

      call disk_grid_fibonacci ( n, r, c, g )

      call r82vec_print_part ( n, g, 20, 
     &  '  Part of the grid point array:' )
c
c  Write the coordinate data to a file.
c
      filename = 'disk_grid_test02.xy'

      call r8mat_write ( filename, 2, n, g )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Coordinate data written to the file "' 
     &  // trim ( filename ) // '".'
c
c  Create graphics data files.
c
      call get_unit ( boundary_unit )
      boundary_filename = 'disk_grid_test02_boundary.txt'
      open ( unit = boundary_unit, file = boundary_filename, 
     &  status = 'replace' )
      do i = 0, 50
        t = 2.0D+00 * pi * dble ( i ) / 50.0D+00
        write ( boundary_unit, '(2x,g14.6,2x,g14.6)' ) 
     &    c(1) + r * cos ( t ), c(2) + r * sin ( t )
      end do
      close ( unit = boundary_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created boundary file "' // 
     &  trim ( boundary_filename ) // '".'

      call get_unit ( data_unit )
      data_filename = 'disk_grid_test02_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) g(1:2,i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Create graphics command file.
c
      call get_unit ( command_unit )
      command_filename = 'disk_grid_test02_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "disk_grid_test02.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set title "Fibonacci Disk Grid"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) // 
     &  '" using 1:2 with points lt 3 pt 3,\'
      write ( command_unit, '(a)' ) '    "' // 
     &  trim ( boundary_filename ) // 
     & '" using 1:2 lw 3 linecolor rgb "black"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
