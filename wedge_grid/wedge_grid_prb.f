      program main

c*****************************************************************************80
c
cc MAIN is the main program for WEDGE_GRID_PRB.
c
c  Discussion:
c
c    WEDGE_GRID_PRB tests the WEDGE_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDGE_GRID_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the WEDGE_GRID library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDGE_GRID_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*****************************************************************************80
c
c! TEST01 tests WEDGE_GRID.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer ng_max
      parameter ( ng_max = 126 )

      double precision g(3,ng_max)
      integer j
      integer ng
      character * ( 255 ) output_filename
      integer output_unit

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  WEDGE_GRID can define a grid of points'
      write ( *, '(a)' ) '  with N+1 points on a side'
      write ( *, '(a)' ) '  over the interior of the unit wedge in 3D.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Grid order N = ', n

      call wedge_grid_size ( n, ng )

      write ( *, '(a,i6)' ) '  Grid count NG = ', ng

      call wedge_grid ( n, ng, g )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     J      X                Y               Z'
      write ( *, '(a)' ) ' '
      do j = 1, ng
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) j, g(1:3,j)
      end do

      output_filename = 'wedge_grid.xy'

      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename, 
     &  status = 'replace' )
      do j = 1, ng
        write ( output_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) g(1:3,j)
      end do
      close ( unit = output_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Data written to "' // trim ( output_filename ) // '".'

      return
      end
      subroutine test02 ( )

c*****************************************************************************80
c
c! TEST02 tests WEDGE_GRID_PLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer ng_max
      parameter ( ng_max = 126 )

      double precision g(3,ng_max)
      character * ( 255 ) header
      integer j
      integer ng

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  WEDGE_GRID_PLOT can create GNUPLOT data files'
      write ( *, '(a)' ) '  for displaying a wedge grid.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Grid order N = ', n

      call wedge_grid_size ( n, ng )

      write ( *, '(a,i6)' ) '  Grid count NG = ', ng

      call wedge_grid ( n, ng, g )

      header = 'wedge'

      call wedge_grid_plot ( n, ng, g, header )

      return
      end

