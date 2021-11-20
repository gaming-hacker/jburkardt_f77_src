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
      subroutine sphere_fibonacci_grid_display ( ng, xg, prefix )

c*********************************************************************72
c
cc SPHERE_FIBONACCI_GRID_DISPLAY displays sphere points on a Fibonacci spiral.
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
c  Parameters:
c
c    Input, integer NG, the number of points.
c
c    Input, double precision XG(3,NG), the Fibonacci spiral points.
c
c    Input, character * ( * ) PREFIX, a prefix for the filenames.
c
      implicit none

      integer ng

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer j
      character * ( 255 ) plot_filename
      character * ( * ) prefix
      double precision xg(3,ng)
c
c  Create graphics data files.
c
      call get_unit ( data_unit )
      data_filename = trim ( prefix ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, ng
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xg(1:3,j)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'
c
c  Create graphics command file.
c
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
      plot_filename = trim ( prefix ) // '.png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( prefix ) // '"'
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
      subroutine sphere_fibonacci_grid_points ( ng, xyz )

c*********************************************************************72
c
cc SPHERE_FIBONACCI_GRID_POINTS computes sphere points on a Fibonacci spiral.
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
c  Reference:
c
c    Richard Swinbank, James Purser,
c    Fibonacci grids: A novel approach to global modelling,
c    Quarterly Journal of the Royal Meteorological Society,
c    Volume 132, Number 619, July 2006 Part B, pages 1769-1793.
c
c  Parameters:
c
c    Input, integer NG, the number of points.
c
c    Output, double precision XYZ(3,NG), the Fibonacci spiral points.
c
      implicit none

      integer ng

      double precision cphi
      integer i
      double precision i_r8
      integer j
      double precision ng_r8
      double precision r8_phi
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision sphi
      double precision theta
      double precision xyz(3,ng)

      r8_phi = ( 1.0D+00 + sqrt ( 5.0D+00 ) ) / 2.0D+00
      ng_r8 = dble ( ng )

      do j = 1, ng
        i_r8 = dble ( - ng - 1 + 2 * j )
        theta = 2.0D+00 * r8_pi * i_r8 / r8_phi
        sphi = i_r8 / ng_r8
        cphi = sqrt ( ( ng_r8 + i_r8 ) * ( ng_r8 - i_r8 ) ) / ng_r8
        xyz(1,j) = cphi * sin ( theta )
        xyz(2,j) = cphi * cos ( theta )
        xyz(3,j) = sphi
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
