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
      subroutine grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

c*********************************************************************72
c
cc GRID_2D returns a regular 2D grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X_NUM, the number of X values to use.
c
c    Input, double precision X_LO, X_HI, the range of X values.
c
c    Input, integer Y_NUM, the number of Y values to use.
c
c    Input, double precision Y_LO, Y_HI, the range of Y values.
c
c    Output, double precision X(X_NUM,Y_NUM), Y(X_NUM,Y_NUM),
c    the coordinates of the grid.
c
      implicit none

      integer x_num
      integer y_num

      integer i
      integer j
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision xi
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo
      double precision yj

      if ( x_num .eq. 1 ) then
        do j = 1, y_num
          do i = 1, x_num
            x(i,j) = ( x_lo + x_hi ) / 2.0D+00
          end do
        end do
      else
        do i = 1, x_num
          xi = ( dble ( x_num - i     ) * x_lo   
     &         + dble (         i - 1 ) * x_hi ) 
     &         / dble ( x_num     - 1 )
          do j = 1, y_num
            x(i,j) = xi
          end do
        end do
      end if

      if ( y_num .eq. 1 ) then
        do j = 1, y_num
          do i = 1, x_num
            y(i,j) = ( y_lo + y_hi ) / 2.0D+00
          end do
        end do
      else
        do j = 1, y_num
          yj = ( dble ( y_num - j     ) * y_lo   
     &         + dble (         j - 1 ) * y_hi ) 
     &         / dble ( y_num     - 1 )
          do i = 1, x_num
            y(i,j) = yj
          end do
        end do
      end if

      return
      end
      function r8vec_amax ( n, a )

c*********************************************************************72
c
cc R8VEC_AMAX returns the maximum absolute value in an R8VEC.
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
c    02 June 2009
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
c    Output, double precision R8VEC_AMAX, the value of the entry
c    of largest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_amax
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = max ( value, abs ( a(i) ) )
      end do

      r8vec_amax = value

      return
      end
      function r8vec_amin ( n, a )

c*********************************************************************72
c
cc R8VEC_AMIN returns the minimum absolute value in an R8VEC.
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
c    02 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precisionA(N), the array.
c
c    Output, double precision R8VEC_AMIN, the value of the entry
c    of smallest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_amin
      double precision value

      value = r8_huge
      do i = 1, n
        value = min ( value, abs ( a(i) ) )
      end do

      r8vec_amin = value

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
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_max
      double precision value

      value = - r8_huge
      do i = 1, n
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
c    18 January 2015
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
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_min
      double precision value

      value = r8_huge
      do i = 1, n
        value = min ( value, a(i) )
      end do

      r8vec_min = value

      return
      end
      subroutine r8vec_uniform_ab ( n, a, b, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
c
c  Discussion:
c
c    Each dimension ranges from A to B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine resid_spiral ( n, x, y, c, pr )

c*********************************************************************72
c
cc RESID_SPIRAL computes the residual for a spiral velocity vector field.
c
c  Discussion:
c
c    Note that the continuous velocity field (U,V)(X,Y) that is discretely
c    sampled here satisfies the homogeneous continuity equation, that is,
c    it has zero divergence.  In other words:
c
c      dU/dX + dV/dY = 0.
c
c    This is by construction, since we have
c
c      U(X,Y) =  10 * d/dY ( PHI(X) * PHI(Y) )
c      V(X,Y) = -10 * d/dX ( PHI(X) * PHI(Y) )
c
c    which guarantees zero divergence.
c
c    The underlying function PHI is defined by
c
c      PHI(Z) = ( 1 - cos ( C * pi * Z ) ) * ( 1 - Z )^2
c
c    where C is a parameter.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision  X(N), Y(N), the coordinates of the
c    evaluation points.
c
c    Input, double precision C, a parameter, typically between 0 and 2 * PI.
c
c    Output, double precision PR(N), the residual in the continuity equation.
c
      implicit none

      integer n

      double precision c
      integer i
      double precision pr(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision u
      double precision ux
      double precision v
      double precision vy
      double precision x(n)
      double precision y(n)

      do i = 1, n

        u =   10.0D+00 * ( 1.0D+00 - cos ( c * r8_pi * x(i) ) ) 
     &         * ( 1.0D+00 - x(i) ) ** 2 
     &         * ( 
     &             c * r8_pi * sin ( c * r8_pi * y(i) ) 
     &           * ( 1.0D+00 - y(i) ) ** 2 
     &           - ( 1.0D+00 - cos ( c * r8_pi * y(i) ) ) 
     &           * 2.0D+00 * ( 1.0D+00 - y(i) ) 
     &           )

        ux =   10.0D+00 * 
     &    ( 
     &      c * r8_pi * sin ( c * r8_pi * x(i) ) 
     &      * ( 1.0D+00 - x(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * x(i) ) ) 
     &      * 2.0D+00 * ( 1.0D+00 - x(i) ) 
     &    ) 
     &    * 
     &    ( 
     &      c * r8_pi * sin ( c * r8_pi * y(i) ) 
     &      * ( 1.0D+00 - y(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * y(i) ) ) 
     &      * 2.0D+00 * ( 1.0D+00 - y(i) ) 
     &    )

        v = - 10.0D+00 * ( 1.0D+00 - cos ( c * r8_pi * y(i) ) ) 
     &    * ( 1.0D+00 - y(i) ) ** 2 
     &    * ( 
     &        c * r8_pi * sin ( c * r8_pi * x(i) ) 
     &      * ( 1.0D+00 - x(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * x(i) ) ) 
     &      * 2.0D+00 * ( 1.0D+00 - x(i) ) 
     &      )

        vy =  - 10.0D+00 * 
     &    ( 
     &      c * r8_pi * sin ( c * r8_pi * x(i) ) 
     &      * ( 1.0D+00 - x(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * x(i) ) ) 
     &      * 2.0D+00 * ( 1.0D+00 - x(i) ) 
     &    ) 
     &    * 
     &    ( 
     &      c * r8_pi * sin ( c * r8_pi * y(i) ) 
     &      * ( 1.0D+00 - y(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * y(i) ) ) 
     &      * 2.0D+00 * ( 1.0D+00 - y(i) ) 
     &    )

        pr(i) = ux + vy

      end do

      return
      end
      subroutine spiral_gnuplot ( header, n, x, y, u, v, s )

c*********************************************************************72
c
cc SPIRAL_GNUPLOT writes the spiral vector field to files for GNUPLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) HEADER, a header to be used to name the files.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, real X(N), Y(N), the coordinates of the evaluation points.
c
c    Input, real U(N), V(N), the velocity components.
c
c    Input, real S, a scale factor for the velocity vectors.
c
      implicit none

      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) header
      integer i
      character * ( 255 ) plot_filename
      double precision s
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)
c
c  Write the data file.
c
      data_filename = trim ( header ) // '_data.txt'

      call get_unit ( data_unit )

      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )

      do i = 1, n
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' )    
     &    x(i), y(i), s * u(i), s * v(i)
      end do

      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Data written to "' // 
     &  trim ( data_filename ) // '".'
c
c  Write the command file.
c
      command_filename = trim ( header ) // '_commands.txt'
      call get_unit ( command_unit )

      plot_filename = trim ( header ) // '.png'

      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '#  ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '#  Add titles and labels.'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set title "Spiral velocity flow"'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '#  Add grid lines.'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '#  Timestamp the plot.'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) //
     &  '" using 1:2:3:4 with vectors \'
      write ( command_unit, '(a)' ) 
     &  '  head filled lt 2 linecolor rgb "blue"'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Commands written to "' //
     &  trim ( command_filename ) // '".'

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
      subroutine uv_spiral ( n, x, y, c, u, v )

c*********************************************************************72
c
cc UV_SPIRAL computes a spiral velocity vector field.
c
c  Discussion:
c
c    Note that the continuous velocity field (U,V)(X,Y) that is discretely
c    sampled here satisfies the homogeneous continuity equation, that is,
c    it has zero divergence.  In other words:
c
c      dU/dX + dV/dY = 0.
c
c    This is by construction, since we have
c
c      U(X,Y) =  10 * d/dY ( PHI(X) * PHI(Y) )
c      V(X,Y) = -10 * d/dX ( PHI(X) * PHI(Y) )
c
c    which guarantees zero divergence.
c
c    The underlying function PHI is defined by
c
c      PHI(Z) = ( 1 - cos ( C * pi * Z ) ) * ( 1 - Z )^2
c
c    where C is a parameter.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the
c    evaluation points.
c
c    Input, double precision C, a parameter, typically between 0 and 2 * PI.
c
c    Output, double precision U(N), V(N), the velocity components.
c
      implicit none

      integer n

      double precision c
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      do i = 1, n

        u(i) =   10.0D+00 * ( 1.0D+00 - cos ( c * r8_pi * x(i) ) ) 
     &    * ( 1.0D+00 - x(i) ) ** 2 
     &    * ( 
     &        c * r8_pi * sin ( c * r8_pi * y(i) ) 
     &        * ( 1.0D+00 - y(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * y(i) ) ) 
     &        * 2.0D+00 * ( 1.0D+00 - y(i) ) 
     &      )

        v(i) = - 10.0D+00 * ( 1.0D+00 - cos ( c * r8_pi * y(i) ) ) 
     &    * ( 1.0D+00 - y(i) ) ** 2 
     &    * ( 
     &        c * r8_pi * sin ( c * r8_pi * x(i) ) 
     &        * ( 1.0D+00 - x(i) ) ** 2 
     &      - ( 1.0D+00 - cos ( c * r8_pi * x(i) ) ) 
     &      * 2.0D+00 * ( 1.0D+00 - x(i) ) 
     &      )

      end do

      return
      end
