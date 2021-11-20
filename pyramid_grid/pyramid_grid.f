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
      function pyramid_grid_size ( n )

c*********************************************************************72
c
cc PYRAMID_GRID_SIZE sizes a pyramid grid.
c
c  Discussion:
c
c    0:  x
c
c    1:  x  x
c        x  x
c
c    2:  x  x  x
c        x  x  x
c        x  x  x
c
c    3:  x  x  x  x
c        x  x  x  x
c        x  x  x  x
c        x  x  x  x
c
c    N  Size
c
c    0     1
c    1     5 = 1 + 4
c    2    14 = 1 + 4 + 9
c    3    30 = 1 + 4 + 9 + 16
c    4    55 = 1 + 4 + 9 + 16 + 25
c    5    91 = 1 + 4 + 9 + 16 + 25 + 36
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Output, integer PYRAMID_GRID_SIZE, the number of
c    nodes in the grid of size N.
c
      implicit none

      integer n
      integer np1
      integer pyramid_grid_size
      integer value

      np1 = n + 1

      value = ( np1 * ( np1 + 1 ) * ( 2 * np1 + 1 ) ) / 6

      pyramid_grid_size = value

      return
      end
      subroutine pyramid_unit_grid ( n, ng, pg )

c*********************************************************************72
c
cc PYRAMID_UNIT_GRID computes grid points in the unit pyramid.
c
c  Discussion:
c
c    The unit pyramid has base (-1,-1,0), (+1,1,0), (+1,+1,0), (-1,+1,0)
c    and vertex (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer NG, the number of nodes to generate,
c    as determined by pyramid_grid_size().
c
c    Output, double precision PG(3,NG), the grid point coordinates.
c
      implicit none

      integer ng

      integer g
      integer hi
      integer i
      integer j
      integer k
      integer lo
      integer n
      double precision pg(3,ng)

      g = 0

      do k = n, 0, -1
        hi = n - k
        lo = - hi
        do j = lo, hi, 2
          do i = lo, hi, 2
            g = g + 1
            pg(1,g) = dble ( i ) / real ( n )
            pg(2,g) = dble ( j ) / real ( n )
            pg(3,g) = dble ( k ) / real ( n )
          end do
        end do
      end do

      return
      end
      subroutine pyramid_unit_grid_plot ( n, ng, pg, header )

c*********************************************************************72
c
cc PYRAMID_UNIT_GRID_PLOT sets up a GNUPLOT plot of a unit pyramid grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer NG, the number of nodes to generate,
c    as determined by pyramid_grid_size().
c
c    Input, double precision PG(3,NG), the grid point coordinates.
c
c    Input, character * ( * ) HEADER, the header for the files.
c
      implicit none

      integer ng

      character * ( 255 ) command_filename
      integer command_unit
      character * ( * ) header
      integer i
      integer j
      integer l
      integer n
      character * ( 255 ) node_filename
      integer node_unit
      double precision pg(3,ng)
      character * ( 255 ) plot_filename
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision v4(3)
      double precision v5(3)
      character * ( 255 ) vertex_filename
      integer vertex_unit
c
c  Create the vertex file.
c
      call pyramid_unit_vertices ( v1, v2, v3, v4, v5 )

      call get_unit ( vertex_unit )
      vertex_filename = trim ( header ) // '_vertices.txt'
      open ( unit = vertex_unit, file = vertex_filename,
     &  status = 'replace' )

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v2(1), v2(2), v2(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v3(1), v3(2), v3(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v4(1), v4(2), v4(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v5(1), v5(2), v5(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v2(1), v2(2), v2(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v2(1), v2(2), v2(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) 
     &  v3(1), v3(2), v3(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v4(1), v4(2), v4(3)
      write ( vertex_unit, '(a)' ) ''

      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) 
     &  v1(1), v1(2), v1(3)
      write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' )
     &  v5(1), v5(2), v5(3)

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
        write ( node_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) pg(1:3,j)
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
      write ( command_unit, '(a)' ) 'set view equal xyz'
      write ( command_unit, '(a)' ) 'set view 80, 40'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'splot "' //
     &  trim ( vertex_filename ) //     '" with lines lw 3, \'
      write ( command_unit, '(a)' ) '     "' //
     &  trim ( node_filename ) // '" with points pt 7 lt 0'
      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine pyramid_unit_vertices ( v1, v2, v3, v4, v5 )

c*********************************************************************72
c
cc PYRAMID_UNIT_VERTICES returns the vertices of the unit pyramid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision V1(3), V2(3), V3(3), V4(3), V5(3), the vertices.
c
      implicit none

      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision v4(3)
      double precision v5(3)

      v1(1) =  0.0D+00
      v1(2) =  0.0D+00
      v1(3) = +1.0D+00

      v2(1) = -1.0D+00
      v2(2) = -1.0D+00
      v2(3) =  0.0D+00

      v3(1) = +1.0D+00
      v3(2) = -1.0D+00
      v3(3) =  0.0D+00

      v4(1) = +1.0D+00
      v4(2) = +1.0D+00
      v4(3) =  0.0D+00

      v5(1) = -1.0D+00
      v5(2) = +1.0D+00
      v5(3) =  0.0D+00

      return
      end
      subroutine r8_print ( r, title )

c*********************************************************************72
c
cc R8_PRINT prints an R8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the value.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      double precision r
      character * ( * ) title

      write ( *, '(a,2x,g14.6)' ) trim ( title ), r

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

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
