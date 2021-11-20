      subroutine ball_grid ( n, r, c, ng, bg )

c*********************************************************************72
c
cc BALL_GRID computes grid points inside a ball.
c
c  Discussion:
c
c    The grid is defined by specifying the radius and center of the ball,
c    and the number of subintervals N into which the horizontal radius
c    should be divided.  Thus, a value of N = 2 will result in 5 points
c    along that horizontal line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, double precision R, the radius of the ball.
c
c    Input, double precision C(3), the coordinates of the center of the ball.
c
c    Input, integer NG, the number of grid points, as determined by
c    BALL_GRID_COUNT.
c
c    Output, double precision BG(3,NG), the grid points inside the ball.
c
      implicit none

      integer ng

      double precision bg(3,ng)
      double precision c(3)
      integer i
      integer j
      integer k
      integer n
      integer p
      double precision r
      double precision x
      double precision y
      double precision z

      p = 0

      do i = 0, n

        x = c(1) + r * dble ( 2 * i ) / dble ( 2 * n + 1 )
        
        do j = 0, n

          y = c(2) + r * dble ( 2 * j ) / dble ( 2 * n + 1 )

          do k = 0, n

            z = c(3) + r * dble ( 2 * k ) / dble ( 2 * n + 1 )

            if ( r * r .lt. 
     &        ( x - c(1) )**2 + ( y - c(2) )**2 + ( z - c(3) )**2 ) then
              go to 10
            end if

            p = p + 1
            bg(1,p) = x
            bg(2,p) = y
            bg(3,p) = z

            if ( 0 .lt. i ) then
              p = p + 1
              bg(1,p) = 2.0D+00 * c(1) - x
              bg(2,p) = y
              bg(3,p) = z
            end if

            if ( 0 .lt. j ) then
              p = p + 1
              bg(1,p) = x
              bg(2,p) = 2.0D+00 * c(2) - y
              bg(3,p) = z
            end if

            if ( 0 .lt. k ) then
              p = p + 1
              bg(1,p) = x
              bg(2,p) = y
              bg(3,p) = 2.0D+00 * c(3) - z
            end if

            if ( 0 .lt. i .and. 0 .lt. j ) then
              p = p + 1
              bg(1,p) = 2.0D+00 * c(1) - x
              bg(2,p) = 2.0D+00 * c(2) - y
              bg(3,p) = z
            end if

            if ( 0 .lt. i .and. 0 .lt. k ) then
              p = p + 1
              bg(1,p) = 2.0D+00 * c(1) - x
              bg(2,p) = y
              bg(3,p) = 2.0D+00 * c(3) - z
            end if

            if ( 0 .lt. j .and. 0 .lt. k ) then
              p = p + 1
              bg(1,p) = x
              bg(2,p) = 2.0D+00 * c(2) - y
              bg(3,p) = 2.0D+00 * c(3) - z
            end if

            if ( 0 .lt. i .and. 0 .lt. j .and. 0 .lt. k ) then
              p = p + 1
              bg(1,p) = 2.0D+00 * c(1) - x
              bg(2,p) = 2.0D+00 * c(2) - y
              bg(3,p) = 2.0D+00 * c(3) - z
            end if

          end do

10        continue

        end do
      end do

      return
      end
      subroutine ball_grid_count ( n, r, c, ng )

c*********************************************************************72
c
cc BALL_GRID computes grid points inside a ball.
c
c  Discussion:
c
c    The grid is defined by specifying the radius and center of the ball,
c    and the number of subintervals N into which the horizontal radius
c    should be divided.  Thus, a value of N = 2 will result in 5 points
c    along that horizontal line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of subintervals.
c
c    Input, double precision R, the radius of the ball.
c
c    Input, double precision C(3), the coordinates of the center of the ball.
c
c    Output, integer NG, the number of grid points inside the ball.
c
      implicit none

      double precision c(3)
      integer i
      integer j
      integer k
      integer n
      integer ng
      double precision r
      double precision x
      double precision y
      double precision z

      ng = 0

      do i = 0, n

        x = c(1) + r * dble ( 2 * i ) / dble ( 2 * n + 1 )
        
        do j = 0, n

          y = c(2) + r * dble ( 2 * j ) / dble ( 2 * n + 1 )

          do k = 0, n

            z = c(3) + r * dble ( 2 * k ) / dble ( 2 * n + 1 )

            if ( r * r .lt. 
     &        ( x - c(1) )**2 + ( y - c(2) )**2 + ( z - c(3) )**2 ) then
              go to 10
            end if

            ng = ng + 1

            if ( 0 .lt. i ) then
              ng = ng + 1
            end if

            if ( 0 .lt. j ) then
              ng = ng + 1
            end if

            if ( 0 .lt. k ) then
              ng = ng + 1
            end if

            if ( 0 .lt. i .and. 0 .lt. j ) then
              ng = ng + 1
            end if

            if ( 0 .lt. i .and. 0 .lt. k ) then
              ng = ng + 1
            end if

            if ( 0 .lt. j .and. 0 .lt. k ) then
              ng = ng + 1
            end if

            if ( 0 .lt. i .and. 0 .lt. j .and. 0 .lt. k ) then
              ng = ng + 1
            end if

          end do

10        continue

        end do
      end do

      return
      end
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
      subroutine r83vec_print_part ( n, a, max_print, title )

c*********************************************************************72
c
cc R83VEC_PRINT_PART prints "part" of an R83VEC.
c
c  Discussion:
c
c    The user specifies MAX_PRINT, the maximum number of lines to print.
c
c    If N, the size of the vector, is no more than MAX_PRINT, then
c    the entire vector is printed, one entry per line.
c
c    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
c    followed by a line of periods suggesting an omission,
c    and the last entry.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries of the vector.
c
c    Input, double precision A(3,N), the vector to be printed.
c
c    Input, integer MAX_PRINT, the maximum number of lines
c    to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(3,n)
      integer i
      integer max_print
      character * ( * )  title

      if ( max_print .le. 0 ) then
        return
      end if

      if ( n .le. 0 ) then
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '

      if ( n .le. max_print ) then

        do i = 1, n
          write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      i, ':', a(1,i), a(2,i), a(3,i)
        end do

      else if ( 3 .le. max_print ) then

        do i = 1, max_print - 2
          write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      i, ':', a(1,i), a(2,i), a(3,i)
        end do
        write ( *, '(a)' ) 
     &    '  ........  ..............  ..............  ..............'
        i = n
        write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, ':', a(1,i), a(2,i), a(3,i)

      else

        do i = 1, max_print - 1
          write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      i, ':', a(1,i), a(2,i), a(3,i)
        end do
        i = max_print
        write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6,2x,a)' ) 
     &    i, ':', a(1,i), a(2,i), a(3,i), '...more entries...'

      end if

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
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
