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
      subroutine r8_to_i4 ( xmin, xmax, x, ixmin, ixmax, ix )

c*********************************************************************72
c
cc R8_TO_I4 maps X in [XMIN, XMAX] to integer IX in [IXMIN, IXMAX].
c
c  Formula:
c
c    IX := IXMIN + ( IXMAX - IXMIN ) * ( X - XMIN ) / ( XMAX - XMIN )
c    IX := min ( IX, max ( IXMIN, IXMAX ) )
c    IX := max ( IX, min ( IXMIN, IXMAX ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision XMIN, XMAX, the range.  XMAX and
c    XMIN must not be equal.  It is not necessary that XMIN be less than XMAX.
c
c    Input, double precision X, the number to be converted.
c
c    Input, integer IXMIN, IXMAX, the allowed range of the output
c    variable.  IXMAX corresponds to XMAX, and IXMIN to XMIN.
c    It is not necessary that IXMIN be less than IXMAX.
c
c    Output, integer IX, the value in the range [IXMIN,IXMAX] that
c    corresponds to X.
c
      implicit none

      integer ix
      integer ixmax
      integer ixmin
      double precision temp
      double precision x
      double precision xmax
      double precision xmin

      if ( xmax .eq. xmin ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  XMAX = XMIN, making a zero divisor.'
        write ( *, '(a,g14.6)' ) '  XMAX = ', xmax
        write ( *, '(a,g14.6)' ) '  XMIN = ', xmin
        stop 1
      end if

      temp =
     &    ( ( xmax - x        ) * dble ( ixmin )
     &    + (        x - xmin ) * dble ( ixmax ) )
     &    / ( xmax     - xmin )

      if ( 0.0D+00 .le. temp ) then
        temp = temp + 0.5D+00
      else
        temp = temp - 0.5D+00
      end if

      ix = int ( temp )

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
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
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
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
c    25 January 2007
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
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
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

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine s_blank_delete ( s )

c*********************************************************************72
c
cc S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
c
c  Discussion:
c
c    All TAB characters are also removed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character*(*) S, the string to be transformed.
c
      implicit none

      character ch
      integer get
      integer put
      character*(*) s
      integer s_length
      character tab

      tab = char ( 9 )

      put = 0
      s_length = len_trim ( s )

      do get = 1, s_length

        ch = s(get:get)

        if ( ch .ne. ' ' .and. ch .ne. tab ) then
          put = put + 1
          s(put:put) = ch
        end if

      end do

      s(put+1:s_length) = ' '

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
      subroutine triangle_svg ( plot_filename, t, p_num, p )

c*********************************************************************72
c
cc TRIANGLE_SVG plots triangles and points in SVG format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) PLOT_FILENAME, the name of the output file.
c
c    Input, double precision T(2,3), points forming a triangle.
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(2,P_NUM), the points.
c
      implicit none

      integer p_num

      integer i4
      integer i4_max
      integer i4_min
      integer ios
      integer j
      integer j4
      integer j4_max
      integer j4_min
      integer output
      double precision p(2,p_num)
      character * ( * ) plot_filename
      integer r
      character * ( 255 ) string
      double precision t(2,3)
      double precision t1
      double precision t2
      double precision x_max
      double precision x_min
      double precision x_scale
      double precision y_max
      double precision y_min
      double precision y_scale
c
c  Determine SCALE, the maximum data range.
c
      x_max = p(1,1)
      x_min = p(1,1)
      do j = 1, p_num
       x_max = max ( x_max, p(1,j) )
       x_min = min ( x_min, p(1,j) )
      end do
      do j = 1, 3
        x_max = max ( x_max, t(1,j) )
        x_min = min ( x_min, t(1,j) )
      end do
      x_scale = x_max - x_min
      x_max = x_max + 0.05D+00 * x_scale
      x_min = x_min - 0.05D+00 * x_scale
      x_scale = x_max - x_min

      y_max = p(2,1)
      y_min = p(2,1)
      do j = 1, p_num
       y_max = max ( y_max, p(2,j) )
       y_min = min ( y_min, p(2,j) )
      end do
      do j = 1, 3
        y_max = max ( y_max, t(2,j) )
        y_min = min ( y_min, t(2,j) )
      end do
      y_scale = y_max - y_min
      y_max = y_max + 0.05D+00 * y_scale
      y_min = y_min - 0.05D+00 * y_scale
      y_scale = y_max - y_min

      i4_min = 1
      j4_min = 1
      if ( x_scale .lt. y_scale ) then
        i4_max = int ( 0.5D+00 + 500.0D+00 * x_scale / y_scale )
        j4_max = 500
      else
        i4_max = 500
        j4_max = int ( 0.5D+00 + 500.0D+00 * y_scale / x_scale )
      end if
c
c  Open the file.
c
      call get_unit ( output )

      open ( unit = output, file = plot_filename, status = 'replace', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRIANGLE_SVG - Fatal error!'
        write ( *, '(a)' ) '  Could not open the output file.'
        stop 1
      end if
c
c  Write that junk.
c
      write ( output, '(a)' ) 
     &  '<?xml version = "1.0" standalone="no"?>'
      write ( output, '(a)' ) ''
      write ( output, '(a)' ) 
     &  '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"'
      write ( output, '(a)' ) 
     &  '  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'
      write ( output, '(a)' ) ''
      write ( output, '(a)' ) '<svg'
      write ( string, '(a,i3,a)' ) 'width="', i4_max, '"'
      call s_blank_delete ( string )
      write ( output, '(2x,a)' ) trim ( string )

      write ( string, '(a,i3,a)' ) 'height="', j4_max, '"'
      call s_blank_delete ( string )
      write ( output, '(2x,a)' ) trim ( string )

      write ( string, '(a,i3,a1,i3,a1,i3,a1,i3,a)' ) 
     &  '  viewbox="', i4_min, ',', j4_min, ',', i4_max, ',', 
     &  j4_max, '"'
      call s_blank_delete ( string )
      write ( output, '(2x,a)' ) trim ( string )
      write ( output, '(a)' ) '  xmlns="http://www.w3.org/2000/svg"'
      write ( output, '(a)' ) '  version="1.1"'
      write ( output, '(a)' ) '>'
      write ( output, '(a)' ) '  <desc>'
      write ( output, '(a)' ) 
     &  '    Triangulation created by triangle_svg.f'
      write ( output, '(a)' ) '  </desc>'
c
c  Draw the triangle.
c
      write ( output, '(a)' ) '  <polygon'
      write ( output, '(a)' ) '    fill="pink"'
      write ( output, '(a)' ) '    stroke="black"'
      write ( output, '(a)' ) '    stroke-width="2"'
      write ( output, '(a)' ) '    points="'

      do j = 1, 3
        call r8_to_i4 ( x_min, x_max, t(1,j), i4_min, i4_max, i4 )
        call r8_to_i4 ( y_max, y_min, t(2,j), j4_min, j4_max, j4 )
        write ( string, '(i3,a,i3)' ) i4, ',', j4
        call s_blank_delete ( string )
        write ( output, '(6x,a)' ) trim ( string )
      end do

      write ( output, '(a)' ) '  "/>'
c
c  Draw points.
c
      do j = 1, p_num

        call r8_to_i4 ( x_min, x_max, p(1,j), i4_min, i4_max, i4 )
        call r8_to_i4 ( y_max, y_min, p(2,j), j4_min, j4_max, j4 )
        r = 5

        write ( output, '(a)' ) '  <circle'
        write ( string, '(a,i3,a)' ) 'cx="', i4, '"'
        call s_blank_delete ( string )
        write ( output, '(4x,a)' ) trim ( string )
        write ( string, '(a,i3,a)' ) 'cy="', j4, '"'
        call s_blank_delete ( string )
        write ( output, '(4x,a)' ) trim ( string )
        write ( string, '(a,i3,a)' ) 'r="', r, '"'
        call s_blank_delete ( string )
        write ( output, '(4x,a)' ) trim ( string )
        write ( output, '(a)' ) '    fill="blue"'
        write ( output, '(a)' ) '    stroke="black"'
        write ( output, '(a)' ) '    stroke-width="2"'
        write ( output, '(a)' ) '  />'

      end do
c
c  End of plot.
c
      write ( output, '(a)' ) '</svg>'

      close ( unit = output )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Graphics data written to file "' 
     &  // trim ( plot_filename ) // '"'

      return
      end
