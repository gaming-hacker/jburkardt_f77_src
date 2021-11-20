      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
      subroutine wedge_grid ( n, ng, g )

c*********************************************************************72
c
cc WEDGE_GRID computes grid points in the unit wedge in 3D.
c
c  Discussion:
c
c    The interior of the unit wedge in 3D is defined by the constraints:
c      0 <= X
c      0 <= Y
c           X + Y <= 1
c     -1 <= Z <= +1
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
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c    0 <= N.
c
c    Input, integer NG, the number of grid points.
c    This can be computed by WEDGE_GRID_SIZE, or else determined by
c    NG =(N+1)*((N+1)*(N+2))/2.
c
c    Output, double precision G(3,NG), the coordinates
c    of the grid points.
c
      implicit none

      integer n
      integer ng

      integer i
      double precision ir
      integer j
      double precision jr
      integer k
      double precision kr
      double precision nr
      integer p
      double precision g(3,ng)

      if ( n .eq. 0 ) then
        g(1,1) = 0.5D+00
        g(2,1) = 0.5D+00
        g(3,1) = 0.0D+00
        return
      end if

      p = 0
      nr = dble ( n )

      do k = 0, n
        kr = dble ( 2 * k - n ) / nr
        do j = 0, n
          jr = dble ( j ) / nr
          do i = 0, n - j
            ir = dble ( i ) / nr
            p = p + 1
            g(1,p) = ir
            g(2,p) = jr
            g(3,p) = kr
          end do
        end do
      end do

      return
      end
      subroutine wedge_grid_size ( n, ng )

c*********************************************************************72
c
cc WEDGE_GRID_SIZE counts the points in a grid of the unit wedge in 3D.
c
c  Discussion:
c
c    The interior of the unit wedge in 3D is defined by the constraints:
c      0 <= X
c      0 <= Y
c           X + Y <= 1
c     -1 <= Z <= +1
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
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c    0 <= N.
c
c    Output, integer NG, the number of grid points.

      implicit none

      integer n
      integer ng

      ng = ( n + 1 ) * ( ( n + 1 ) * ( n + 2 ) ) / 2

      return
      end
      subroutine wedge_grid_plot ( n, ng, g, header )

c*********************************************************************72
c
cc WEDGE_GRID_PLOT sets up a GNUPLOT plot of a unit wedge grid.
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
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer G, the number of nodes.
c
c    Input, double precision PG(3,NG), the grid point coordinates.
c
c    Input, character * ( * ) HEADER, the header for the files.
c
      implicit none

      integer ng

      character * ( 255 ) command_filename
      integer command_unit
      double precision g(3,ng)
      character * ( * ) header
      integer i
      integer j
      integer l
      integer n
      character * ( 255 ) node_filename
      integer node_unit
      character * ( 255 ) plot_filename
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision v4(3)
      double precision v5(3)
      double precision v6(3)
      character * ( 255 ) vertex_filename
      integer vertex_unit
c
c  Create the vertex file.
c
      call wedge_vertices ( v1, v2, v3, v4, v5, v6 )

      call get_unit ( vertex_unit )
      vertex_filename = trim ( header ) // '_vertices.txt'
      open ( unit = vertex_unit, file = vertex_filename,
     &  status = 'replace' )

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v2(1), v2(2), v2(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v3(1), v3(2), v3(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v4(1), v4(2), v4(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v5(1), v5(2), v5(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v6(1), v6(2), v6(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v4(1), v4(2), v4(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v4(1), v4(2), v4(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v2(1), v2(2), v2(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v5(1), v5(2), v5(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v3(1), v3(2), v3(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v6(1), v6(2), v6(3)
      write ( vertex_unit, '(a)' ) ''

      close ( unit = vertex_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created vertex file "' //
     &  trim ( vertex_filename ) // '".'
c
c  Create the node file.
c
      call get_unit ( node_unit )
      node_filename = trim ( header ) // '_nodes.txt'
      open ( unit = node_unit, file = node_filename,
     &  status = 'replace' )
      do j = 1, ng
        write ( node_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) g(1:3,j)
      end do
      close ( unit = node_unit )
      write ( *, '(a)' ) '  Created node file "' //
     &  trim ( node_filename ) // '".'
c
c  Create the command file.
c
      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename,
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' //
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      plot_filename = trim ( header ) // '.png'
      write ( command_unit, '(a)' ) 'set output "' //
     &  trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' )     'set title "' //
     &  trim ( header ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) '#set view equal xyz'
      write ( command_unit, '(a)' ) 'set view 80, 85'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'splot "' //
     &  trim ( vertex_filename ) //     '" with lines lw 3, \'
      write ( command_unit, '(a)' ) '     "' //
     &  trim ( node_filename ) // '" with points pt 7 lt 0'
      close ( unit = command_unit )

      write ( *, '(a)' )     '  Created command file "' //
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine wedge_vertices ( v1, v2, v3, v4, v5, v6 )

c*********************************************************************72
c
cc WEDGE_VERTICES returns the vertices of the unit wege.
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
c  Parameters:
c
c    Output, double precision V1(3), V2(3), V3(3), V4(3), V5(3), V6(3),
c    the vertices.
c
      implicit none

      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision v4(3)
      double precision v5(3)
      double precision v6(3)

      v1(1) =  0.0D+00
      v1(2) =  0.0D+00
      v1(3) = -1.0D+00

      v2(1) =  1.0D+00
      v2(2) =  0.0D+00
      v2(3) = -1.0D+00

      v3(1) =  0.0D+00
      v3(2) =  1.0D+00
      v3(3) = -1.0D+00

      v4(1) =  0.0D+00
      v4(2) =  0.0D+00
      v4(3) = +1.0D+00

      v5(1) =  1.0D+00
      v5(2) =  0.0D+00
      v5(3) = +1.0D+00

      v6(1) =  0.0D+00
      v6(2) =  1.0D+00
      v6(3) = +1.0D+00

      return
      end
