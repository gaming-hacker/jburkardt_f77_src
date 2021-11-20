      subroutine chebyshev1 ( n, x )

c*********************************************************************72
c
cc CHEBYSHEV1 returns the Type 1 Chebyshev points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)

      do i = 1, n
        angle = r8_pi * dble ( 2 * ( i - 1 ) + 1 ) / dble ( 2 * n )
        x(i) = cos ( angle )
      end do

      return
      end
      subroutine chebyshev2 ( n, x )

c*********************************************************************72
c
cc CHEBYSHEV2 returns the Type 2 Chebyshev points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)

      if ( n .eq. 1 ) then
        x(1) = 0.0D+00
      else
        do i = 1, n
          angle = r8_pi * dble ( n - i ) / dble ( n - 1 )
          x(i) = cos ( angle )
        end do
      end if

      return
      end
      subroutine chebyshev3 ( n, x )

c*********************************************************************72
c
cc CHEBYSHEV3 returns the Type 3 Chebyshev points.
c
c  Discussion:
c
c    Note that this point set is NOT symmetric in [-1,+1].
c    It is sometimes augmented by the value -1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)

      do i = 1, n
        angle = r8_pi * dble ( 2 * n - 2 * i + 1 ) 
     &                / dble ( 2 * n         + 1 )
        x(i) = cos ( angle )
      end do

      return
      end
      subroutine chebyshev4 ( n, x )

c*********************************************************************72
c
cc CHEBYSHEV4 returns the Type 4 Chebyshev points.
c
c  Discussion:
c
c    Note that this point set is NOT symmetric in [-1,+1].
c    It is sometimes augmented by the value +1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)

      do i = 1, n
        angle = r8_pi * dble ( 2 * ( n - i + 1 ) ) 
     &                / dble ( 2 * n + 1      )
        x(i) = cos ( angle )
      end do

      return
      end
      subroutine equidistant1 ( n, x )

c*********************************************************************72
c
cc EQUIDISTANT1 returns the Type 1 Equidistant points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      integer i
      double precision x(n)

      do i = 1, n
        x(i) = dble ( - n  - 1 + 2 * i ) / dble ( n + 1 )
      end do

      return
      end
      subroutine equidistant2 ( n, x )

c*********************************************************************72
c
cc EQUIDISTANT2 returns the Type 2 Equidistant points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      integer i
      double precision x(n)

      if ( n .eq. 1 ) then
        x(1) = 0.0D+00
      else
        do i = 1, n
          x(i) = dble ( - n  - 1 + 2 * i ) / dble ( n - 1 )
        end do
      end if

      return
      end
      subroutine equidistant3 ( n, x )

c*********************************************************************72
c
cc EQUIDISTANT3 returns the Type 3 Equidistant points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      integer i
      double precision x(n)

      do i = 1, n
        x(i) = dble ( - n  - 1 + 2 * i ) / dble ( n )
      end do

      return
      end
      subroutine fejer1 ( n, x )

c*********************************************************************72
c
cc FEJER1 returns the Type 1 Fejer points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision theta
      double precision x(n)

      do i = 1, n
        theta = r8_pi * dble ( 2 * n + 1 - 2 * i ) / dble ( 2 * n )
        x(i) = cos ( theta )
      end do

      return
      end
      subroutine fejer2 ( n, x )

c*********************************************************************72
c
cc FEJER2 returns the Type 2 Fejer points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the points.
c
      implicit none

      integer n

      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision theta
      double precision x(n)

      do i = 1, n
        theta = r8_pi * dble ( n + 1 - i ) / dble ( n + 1 )
        x(i) = cos ( theta )
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
      subroutine lagrange_value ( data_num, t_data, interp_num, 
     &  t_interp, l_interp )

c*********************************************************************72
c
cc LAGRANGE_VALUE evaluates the Lagrange polynomials.
c
c  Discussion:
c
c    Given DATA_NUM distinct abscissas, T_DATA(1:DATA_NUM),
c    the I-th Lagrange polynomial L(I)(T) is defined as the polynomial of
c    degree DATA_NUM - 1 which is 1 at T_DATA(I) and 0 at the DATA_NUM - 1
c    other abscissas.
c
c    A formal representation is:
c
c      L(I)(T) = Product ( 1 <= J <= DATA_NUM, I /= J )
c       ( T - T(J) ) / ( T(I) - T(J) )
c
c    This routine accepts a set of INTERP_NUM values at which all the Lagrange
c    polynomials should be evaluated.
c
c    Given data values P_DATA at each of the abscissas, the value of the
c    Lagrange interpolating polynomial at each of the interpolation points
c    is then simple to compute by matrix multiplication:
c
c      P_INTERP(1:INTERP_NUM) =
c        P_DATA(1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
c
c    or, in the case where P is multidimensional:
c
c      P_INTERP(1:M,1:INTERP_NUM) =
c        P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DATA_NUM, the number of data points.
c    DATA_NUM must be at least 1.
c
c    Input, double precision T_DATA(DATA_NUM), the data points.
c
c    Input, integer INTERP_NUM, the number of
c    interpolation points.
c
c    Input, double precision T_INTERP(INTERP_NUM), the
c    interpolation points.
c
c    Output, double precision L_INTERP(DATA_NUM,INTERP_NUM), the values
c    of the Lagrange polynomials at the interpolation points.
c
      implicit none

      integer data_num
      integer interp_num

      integer i
      integer i1
      integer i2
      integer j
      double precision l_interp(data_num,interp_num)
      double precision t_data(data_num)
      double precision t_interp(interp_num)
c
c  Evaluate the polynomial.
c
      do j = 1, interp_num
        do i = 1, data_num
          l_interp(i,j) = 1.0D+00
        end do
      end do

      do i1 = 1, data_num

        do i2 = 1, data_num

          if ( i1 .ne. i2 ) then

            do j = 1, interp_num
              l_interp(i1,j) = l_interp(i1,j) 
     &          * ( t_interp(j) - t_data(i2) ) 
     &          / ( t_data(i1) - t_data(i2) )

            end do
          end if

        end do

      end do

      return
      end
      subroutine lebesgue_constant ( n, x, nfun, xfun, lmax )

c*********************************************************************72
c
cc LEBESGUE_CONSTANT estimates the Lebesgue constant for a set of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Jean-Paul Berrut, Lloyd Trefethen,
c    Barycentric Lagrange Interpolation,
c    SIAM Review,
c    Volume 46, Number 3, September 2004, pages 501-517.
c
c  Parameters:
c
c    Input, integer N, the number of interpolation points.
c
c    Input, double precision X(N), the interpolation points.
c
c    Input, integer NFUN, the number of evaluation points.
c
c    Input, double precision XFUN(*), the evaluation points.
c
c    Output, double precision LMAX, an estimate of the Lebesgue constant 
c    for the points.
c
      implicit none

      integer n
      integer nfun

      double precision lfun(nfun)
      double precision lmax
      double precision r8vec_max
      double precision x(n)
      double precision xfun(nfun)

      call lebesgue_function ( n, x, nfun, xfun, lfun )

      lmax = r8vec_max ( nfun, lfun )

      return
      end
      subroutine lebesgue_function ( n, x, nfun, xfun, lfun )

c*********************************************************************72
c
cc LEBESGUE_FUNCTION evaluates the Lebesgue function for a set of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    Original MATLAB version by Greg von Winckel.
c    FORTRAN90 version by John Burkardt.
c
c  Parameters:
c
c    Jean-Paul Berrut, Lloyd Trefethen,
c    Barycentric Lagrange Interpolation,
c    SIAM Review,
c    Volume 46, Number 3, September 2004, pages 501-517.
c
c  Parameters:
c
c    Input, integer N, the number of interpolation points.
c
c    Input, double precision X(N), the interpolation points.
c
c    Input, integer NFUN, the number of evaluation points.
c
c    Input, double precision XFUN(NFUN), the evaluation points.
c
c    Output, double precision LFUN(NFUN), the Lebesgue function values.
c
      implicit none

      integer n
      integer nfun

      integer i
      integer j
      double precision lfun(nfun)
      double precision llfun(n,nfun)
      double precision x(n)
      double precision xfun(nfun)
c
c  Handle special case.
c
      if ( n .eq. 1 ) then
        do j = 1, nfun
          lfun(j) = 1.0D+00
        end do
        return
      end if

      call lagrange_value ( n, x, nfun, xfun, llfun ) 

      do j = 1, nfun
        lfun(j) = 0.0D+00
        do i = 1, n
          lfun(j) = lfun(j) + abs ( llfun(i,j) )
        end do
      end do

      return
      end
      subroutine lebesgue_plot ( n, x, nfun, xfun, label, filename )

c*********************************************************************72
c
cc LEBESGUE_PLOT plots the Lebesgue function for a set of points.
c
c  Discussion:
c
c    The interpolation interval is assumed to be [min(XFUN), max(XFUN)].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Jean-Paul Berrut, Lloyd Trefethen,
c    Barycentric Lagrange Interpolation,
c    SIAM Review,
c    Volume 46, Number 3, September 2004, pages 501-517.
c
c  Parameters:
c
c    Input, integer N, the number of interpolation points.
c
c    Input, double precision X(N), the interpolation points.
c
c    Input, integer NFUN, the number of evaluation points.
c
c    Input, double precision XFUN(NFUN), the evaluation points.  
c
c    Input, character * ( * ) LABEL, a title for the plot.
c
c    Input, character * ( * ) FILENAME, a partial filename.
c    The program will create "filename_commands.txt', 'filename_data.txt',
c    and 'filename.png'.
c
      implicit none

      integer n
      integer nfun

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) filename
      integer i
      character * ( * ) label
      double precision lfun(nfun)
      character * ( 255 ) png_filename
      double precision x(n)
      double precision xfun(nfun)

      call lebesgue_function ( n, x, nfun, xfun, lfun )
c
c  Create data file.
c
      data_filename = trim ( filename ) // '_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, nfun
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) xfun(i), lfun(i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created graphics data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Create command file.
c
      command_filename = trim ( filename ) // '_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'

      png_filename = trim ( filename ) // '.png'
      write ( command_unit, '(a)' ) 'set output "'// 
     &  trim ( png_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Lebesgue(X) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( label ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "red"'

      close ( unit = command_unit )
      write ( *, '(a)' ) '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

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
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
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
c    Output, double precision R8VEC_MAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_max

      r8vec_max = a(1)
      do i = 2, n
        r8vec_max = max ( r8vec_max, a(i) )
      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
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
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
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
