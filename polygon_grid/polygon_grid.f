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
      subroutine polygon_grid_count ( n, nv, ng )

c*********************************************************************72
c
cc POLYGON_GRID_COUNT counts the grid points inside a polygon.
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
c  Parameters:
c
c    Input, integer N, the number of subintervals on a side.
c
c    Input, integer NV, the number of vertices.
c    3 <= NV.
c
c    Output, integer NG, the number of grid points.
c
      implicit none

      integer n
      integer ng
      integer nv

      ng = 1 + nv * ( n * ( n + 1 ) ) / 2

      return
      end
      subroutine polygon_grid_display ( n, nv, v, ng, xg, prefix )

c*********************************************************************72
c
cc POLYGON_GRID_DISPLAY displays grid points inside a polygon.
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
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer NV, the number of vertices in the polygon.
c
c    Input, double precision V(2,NV), the coordinates of the vertices.
c
c    Input, integer NG, the number of grid points.
c
c    Input, double precision XG(2,NG), the grid points.
c
c    Input, character * ( * ) PREFIX, a string used to name the files.
c
      implicit none

      integer ng
      integer nv

      integer command_unit
      character * ( 255 ) command_filename
      integer grid_unit
      character * ( 255 ) grid_filename
      integer j
      integer n
      character * ( 255 ) plot_filename
      character * ( * ) prefix
      double precision v(2,nv)
      double precision vc(2)
      integer vertex_unit
      character * ( 255 ) vertex_filename
      double precision xg(2,ng)
c
c  Get the centroid.
c
      vc(1) = 0.0D+00
      vc(2) = 0.0D+00
      do j = 1, nv
        vc(1) = vc(1) + v(1,j)
        vc(2) = vc(2) + v(2,j)
      end do
      vc(1) = vc(1) / dble ( nv )
      vc(2) = vc(2) / dble ( nv )
c
c  Write the vertex file.
c
      call get_unit ( vertex_unit )
      vertex_filename = trim ( prefix ) // '_vertex.txt'
      open ( unit = vertex_unit, file = vertex_filename, 
     &  status = 'replace' )
      do j = 1, nv
        write ( vertex_unit, '(2x,g14.6,2x,g14.6)' ) v(1,j), v(2,j)
      end do
      write ( vertex_unit, '(2x,g14.6,2x,g14.6)' ) v(1,1), v(2,1)
      do j = 1, nv
        write ( vertex_unit, '(a)' ) ''
        write ( vertex_unit, '(2x,g14.6,2x,g14.6)' ) v(1,j), v(2,j)
        write ( vertex_unit, '(2x,g14.6,2x,g14.6)' ) vc(1), vc(2)
      end do
      close ( unit = vertex_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created vertex file "' // trim ( vertex_filename ) // '".'
c
c  Write the gridpoint file.
c
      call get_unit ( grid_unit )
      grid_filename = trim ( prefix ) // '_grid.txt'
      open ( unit = grid_unit, file = grid_filename, 
     &  status = 'replace' )
      do j = 1, ng
        write ( grid_unit, '(2x,g14.6,2x,g14.6)' ) xg(1,j), xg(2,j)
      end do
      close ( unit = grid_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created grid file "' // trim ( grid_filename ) // '".'
c
c  Write the command file.
c
      plot_filename = trim ( prefix ) // '.png'
      call get_unit ( command_unit )
      command_filename = trim ( prefix ) // '_commands.txt'
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
     &  'set output "' // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( prefix ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) 'set style data lines'

      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( grid_filename ) // 
     &  '" using 1:2 with points lt 3 pt 3,\'
      write ( command_unit, '(a)' ) 
     &  '    "' // trim ( vertex_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "black"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )

      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine polygon_grid_points ( n, nv, v, ng, xg )

c*********************************************************************72
c
cc POLYGON_GRID_POINTS computes points on a polygonal grid.
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
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer NV, the number of vertices in the polygon.
c
c    Input, double precision V(2,NV), the coordinates of the vertices.
c
c    Input, integer NG, the number of grid points.
c
c    Output, double precision XG(2,NG), the coordinates of the grid points.
c
      implicit none

      integer ng
      integer nv

      integer i
      integer j
      integer k
      integer l
      integer lp1
      integer n
      integer p
      double precision v(2,nv)
      double precision vc(2)
      double precision xg(2,ng)
c
c  Get the centroid.
c
      vc(1) = 0.0D+00
      vc(2) = 0.0D+00
      do j = 1, nv
        vc(1) = vc(1) + v(1,j)
        vc(2) = vc(2) + v(2,j)
      end do
      vc(1) = vc(1) / dble ( nv )
      vc(2) = vc(2) / dble ( nv )
c
c  Determine the centroid, and use it as the first grid point.
c
      p = 1
      xg(1,p) = vc(1)
      xg(2,p) = vc(2)
c
c  Consider each triangle formed by two consecutive vertices and the centroid,
c  but skip the first line of points.
c
      do l = 1, nv
        lp1 = mod ( l, nv ) + 1
        do i = 1, n
          do j = 0, n - i
            k = n - i - j
            p = p + 1
            xg(1,p) = ( dble ( i ) * v(1,l)   
     &                + dble ( j ) * v(1,lp1) 
     &                + dble ( k ) * vc(1) )  
     &                / dble ( n )
            xg(2,p) = ( dble ( i ) * v(2,l)   
     &                + dble ( j ) * v(2,lp1) 
     &                + dble ( k ) * vc(2) )  
     &                / dble ( n )
          end do
        end do
      end do

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
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
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
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

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