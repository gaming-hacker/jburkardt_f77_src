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
      subroutine naca4_cambered ( m, p, t, c, n, xc, xu, yu, xl, yl )

c*********************************************************************72
c
cc NACA4_CAMBERED: (xu,yu), (xl,yl) for a NACA cambered 4-digit airfoil.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Eastman Jacobs, Kenneth Ward, Robert Pinkerton,
c    "The characteristics of 78 related airfoil sections from tests in
c    the variable-density wind tunnel",
c    NACA Report 460, 1933.
c
c  Parameters:
c
c    Input, double precision M, the maximum camber.
c    0.0 < M.
c
c    Input, double precision P, the location of maximum camber.
c    0.0 < P < 1.0
c
c    Input, double precision T, the maximum relative thickness.
c    0.0 < T <= 1.0
c
c    Input, double precision C, the chord length.
c    0.0 < C.
c
c    Input, integer N, the number of sample points.
c
c    Input, double precision XC(N), points along the chord length.  
c    0.0 <= XC(*) <= C.
c
c    Output, double precision XU(N), YU(N), XL(N), YL(N), for each value of 
c    XC, measured along the camber line, the corresponding values (XU,YU) 
c    on the upper airfoil surface and (XL,YL) on the lower airfoil surface.
c
      implicit none

      integer n

      double precision c
      double precision divisor
      double precision dycdx
      integer i
      double precision m
      double precision p
      double precision t
      double precision theta
      double precision xc(n)
      double precision xl(n)
      double precision xu(n)
      double precision yc
      double precision yl(n)
      double precision yt
      double precision yu(n)

      do i = 1, n

        if ( 0.0D+00 .le. xc(i) / c .and. xc(i) / c .le. p ) then
          divisor = p ** 2
        else if ( p .le. xc(i) / c .and. xc(i) / c .le. 1.0D+00 ) then
          divisor = ( 1.0D+00 - p ) ** 2
        else
          divisor = 1.0D+00
        end if

        dycdx = 2.0D+00 * m * ( p - xc(i) / c ) / divisor

        theta = atan ( dycdx )
   
        yt = 5.0D+00 * t * c * ( 
     &    0.2969D+00 * sqrt ( xc(i) / c ) 
     &     + (((( 
     &       - 0.1015D+00 ) * ( xc(i) / c ) 
     &       + 0.2843D+00 ) * ( xc(i) / c ) 
     &       - 0.3516D+00 ) * ( xc(i) / c ) 
     &       - 0.1260D+00 ) * ( xc(i) / c ) )

        if ( 0.0D+00 .le. xc(i) / c .and. xc(i) / c .le. p ) then
          yc = m * xc(i) * ( 2.0D+00 * p - xc(i) / c ) / p ** 2
        else if ( p .le. xc(i) / c .and. xc(i) / c .le. 1.0D+00 ) then
          yc = m * ( xc(i) - c ) 
     &      * ( 2.0D+00 * p - xc(i) / c - 1.0D+00 ) 
     &      / ( 1.0D+00 - p ) ** 2
        else
          yc = 0.0D+00
        end if

        xu(i) = xc(i) - yt * sin ( theta )
        yu(i) = yc + yt * cos ( theta )
        xl(i) = xc(i) + yt * sin ( theta )
        yl(i) = yc - yt * cos ( theta )

      end do

      return
      end
      subroutine naca4_symmetric ( t, c, n, x, y )

c*********************************************************************72
c
cc NACA4_SYMMETRIC evaluates y(x) for a NACA symmetric 4-digit airfoil.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Eastman Jacobs, Kenneth Ward, Robert Pinkerton,
c    "The characteristics of 78 related airfoil sections from tests in
c    the variable-density wind tunnel",
c    NACA Report 460, 1933.
c
c  Parameters:
c
c    Input, double precision T, the maximum relative thickness.
c
c    Input, double precision C, the chord length.
c
c    Input, integer N, the number of sample points.
c
c    Input, double precision X(N), points along the chord length.  
c    0.0 <= X(*) <= C.
c
c    Output, double precision Y(N), for each value of X, the corresponding
c    value of Y so that (X,Y) is on the upper wing surface, and (X,-Y) is on the
c    lower wing surface.
c
      implicit none

      integer n

      double precision c
      integer i
      double precision t
      double precision x(n)
      double precision y(n)

      do i = 1, n
        y(i) = 5.0D+00 * t * c * ( 
     &    0.2969D+00 * sqrt ( x(i) / c ) 
     &    + (((( 
     &      - 0.1015D+00 ) * ( x(i) / c ) 
     &      + 0.2843D+00 ) * ( x(i) / c ) 
     &      - 0.3516D+00 ) * ( x(i) / c ) 
     &      - 0.1260D+00 ) * ( x(i) / c ) )
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
      subroutine r8vec_linspace ( n, a, b, x )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      function r8vec_max ( n, a )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_max
      double precision value

      value = a(1)
      do i = 2, n
        value = max ( value, a(i) )
      end do

      r8vec_max = value

      return
      end
      function r8vec_min ( n, a )

c*********************************************************************72
c
cc R8VEC_MIN returns the minimum value in an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MIN, the value of the smallest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_min
      double precision value

      value = a(1)
      do i = 2, n
        value = min ( value, a(i) )
      end do

      r8vec_min = value

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
