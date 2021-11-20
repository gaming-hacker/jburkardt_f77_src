      function p00_fun ( prob, x )

c*********************************************************************72
c
cc P00_FUN evaluates the function for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROB, the number of the desired test problem.
c
c    Input, double precision X, the point at which the function
c    is to be evaluated.
c
c    Output, double precision P00_FUN, the value of the function at X.
c
      implicit none

      integer prob
      double precision p00_fun
      double precision p01_fun
      double precision p02_fun
      double precision p03_fun
      double precision p04_fun
      double precision p05_fun
      double precision value
      double precision x

      if ( prob .eq. 1 ) then
        value = p01_fun ( x )
      else if ( prob .eq. 2 ) then
        value = p02_fun ( x )
      else if ( prob .eq. 3 ) then
        value = p03_fun ( x )
      else if ( prob .eq. 4 ) then
        value = p04_fun ( x )
      else if ( prob .eq. 5 ) then
        value = p05_fun ( x )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_FUN - Fatal error!'
        write ( *, '(a,i6)' ) '  Illegal problem number = ', prob
        value = 0.0D+00
        stop
      end if

      p00_fun = value

      return
      end
      subroutine p00_lim ( prob, a, b )

c*********************************************************************72
c
cc P00_LIM returns the limits of the approximation interval for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROB, the number of the desired test problem.
c
c    Output, double precision A, B, the lower and upper limits of
c    the approximation interval.
c
      implicit none

      double precision a
      double precision b
      integer prob

      if ( prob .eq. 1 ) then
        call p01_lim ( a, b )
      else if ( prob .eq. 2 ) then
        call p02_lim ( a, b )
      else if ( prob .eq. 3 ) then
        call p03_lim ( a, b )
      else if ( prob .eq. 4 ) then
        call p04_lim ( a, b )
      else if ( prob .eq. 5 ) then
        call p05_lim ( a, b )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_LIM - Fatal error!'
        write ( *, '(a,i6)' ) '  Illegal problem number = ', prob
        stop
      end if

      return
      end
      subroutine p00_prob_num ( prob_num )

c*********************************************************************72
c
cc P00_PROB_NUM returns the number of problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer PROB_NUM, the number of problems.
c
      implicit none

      integer prob_num

      prob_num = 5

      return
      end
      subroutine p00_story ( prob )

c*********************************************************************72
c
cc P00_STORY prints the "story" for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
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

      integer prob

      if ( prob .eq. 1 ) then
        call p01_story ( )
      else if ( prob .eq. 2 ) then
        call p02_story ( )
      else if ( prob .eq. 3 ) then
        call p03_story ( )
      else if ( prob .eq. 4 ) then
        call p04_story ( )
      else if ( prob .eq. 5 ) then
        call p05_story ( )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_STORY - Fatal error!'
        write ( *, '(a)' ) '  Unexpected input value of PROB.'
        stop
      end if

      return
      end
      subroutine p00_title ( prob, title )

c*********************************************************************72
c
cc P00_TITLE returns the title of any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROB, the number of the desired test problem.
c
c    Output, character * ( * ) TITLE, the title of the problem.
c
      implicit none

      integer prob
      character * ( * ) title

      if ( prob .eq. 1 ) then
        call p01_title ( title )
      else if ( prob .eq. 2 ) then
        call p02_title ( title )
      else if ( prob .eq. 3 ) then
        call p03_title ( title )
      else if ( prob .eq. 4 ) then
        call p04_title ( title )
      else if ( prob .eq. 5 ) then
        call p05_title ( title )
      else
        title = ' '
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_TITLE - Fatal error!'
        write ( *, '(a,i6)' ) '  Illegal problem number = ', prob
        stop
      end if

      return
      end
      function p01_fun ( x )

c*********************************************************************72
c
cc P01_FUN evaluates the function for problem 1.
c
c  Discussion:
c
c    This is a famous example, due to Runge.  If equally spaced
c    abscissas are used, the sequence of interpolating polynomials Pn(X)
c    diverges, in the sense that the max norm of the difference
c    between Pn(X) and F(X) becomes arbitrarily large as N increases.
c
c  Dimension:
c
c    N = 1
c
c  Interval:
c
c    -5 <= X <= 5
c
c  Function:
c
c    1 / ( X * X + 1 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the point at which the function
c    is to be evaluated.
c
c    Output, double precision P01_FUN, the value of the function at X.
c
      implicit none

      double precision p01_fun
      double precision x

      p01_fun = 1.0D+00 / ( x * x + 1.0D+00 )

      return
      end
      subroutine p01_lim ( a, b )

c*********************************************************************72
c
cc P01_LIM returns the limits of the approximation interval for problem 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision A, B, the limits of the interval
c    of approximation.
c
      implicit none

      double precision a
      double precision b

      a = -5.0D+00
      b =  5.0D+00

      return
      end
      subroutine p01_story ( )

c*********************************************************************72
c
cc P01_STORY prints the "story" for problem 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  This is a famous example, due to Runge.'
      write ( *, '(a)' ) '  If equally spaced abscissas are used,'
      write ( *, '(a)' ) '  the sequence of interpolating polynomials'
      write ( *, '(a)' ) '  Pn(X) diverges, in the sense that the max'
      write ( *, '(a)' ) '  norm of the difference between Pn(X) and '
      write ( *, '(a)' ) '  F(X) becomes arbitrarily large '
      write ( *, '(a)' ) '  as N increases.'

      return
      end
      subroutine p01_title ( title )

c*********************************************************************72
c
cc P01_TITLE returns the title of problem 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character * ( * ) TITLE, the title of the problem.
c
      implicit none

      character * ( * ) title

      title = 'Runge example, f(x) = 1 / ( x * x + 1 ), [-5,5]'

      return
      end
      function p02_fun ( x )

c*********************************************************************72
c
cc P02_FUN evaluates the function for problem 2.
c
c  Discussion:
c
c    This example is due to Bernstein.  If equally spaced
c    abscissas are used, the sequence of interpolating polynomials Pn(X)
c    only converges to F(X) at -1, 0, and 1.
c
c  Dimension:
c
c    N = 1
c
c  Interval:
c
c    -1 <= X <= 1
c
c  Function:
c
c    ABS ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the point at which the function
c    is to be evaluated.
c
c    Output, double precision P02_FUN, the value of the function at X.
c
      implicit none

      double precision p02_fun
      double precision x

      p02_fun = abs ( x )

      return
      end
      subroutine p02_lim ( a, b )

c*********************************************************************72
c
cc P02_LIM returns the limits of the approximation interval for problem 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision A, B, the limits of the interval
c    of approximation.
c
      implicit none

      double precision a
      double precision b

      a = -1.0D+00
      b =  1.0D+00

      return
      end
      subroutine p02_story ( )

c*********************************************************************72
c
cc P02_STORY prints the "story" for problem 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  This example is due to Bernstein.'
      write ( *, '(a)' ) '  If equally spaced abscissas are used,'
      write ( *, '(a)' ) '  the sequence of interpolating'
      write ( *, '(a)' ) '  polynomials Pn(X) only converges to '
      write ( *, '(a)' ) '  F(X) at -1, 0, and 1.'

      return
      end
      subroutine p02_title ( title )

c*********************************************************************72
c
cc P02_TITLE returns the title of problem 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character * ( * ) TITLE, the title of the problem.
c
      implicit none

      character * ( * ) title

      title = 'Bernstein example, f(x) = abs ( x ), [-1,1]'

      return
      end
      function p03_fun ( x )

c*********************************************************************72
c
cc P03_FUN evaluates the function for problem 3.
c
c  Discussion:
c
c    This is a step function with a jump from 0 to 1 at 0.
c
c  Dimension:
c
c    N = 1
c
c  Interval:
c
c    -1 <= X <= 1
c
c  Function:
c
c    F(X) = 0 if X < 0
c           1 if 0 < X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the point at which the function
c    is to be evaluated.
c
c    Output, double precision P03_FUN, the value of the function at X.
c
      implicit none

      double precision p03_fun
      double precision value
      double precision x

      if ( x .lt. 0.0D+00 ) then
        value = 0.0D+00
      else
        value = 1.0D+00
      end if

      p03_fun = value

      return
      end
      subroutine p03_lim ( a, b )

c*********************************************************************72
c
cc P03_LIM returns the limits of the approximation interval for problem 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision A, B, the limits of the interval
c    of approximation.
c
      implicit none

      double precision a
      double precision b

      a = -1.0D+00
      b =  1.0D+00

      return
      end
      subroutine p03_story ( )

c*********************************************************************72
c
cc P03_STORY prints the "story" for problem 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The step function is discontinuous.'
      write ( *, '(a)' ) '  Attempts to approximate this function'
      write ( *, '(a)' ) '  by high degree polynomials '
      write ( *, '(a)' ) '  will rapidly diverge.'

      return
      end
      subroutine p03_title ( title )

c*********************************************************************72
c
cc P03_TITLE returns the title of problem 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character * ( * ) TITLE, the title of the problem.
c
      implicit none

      character * ( * ) title

      title = 'Step function, f jumps from 0 to 1 at 0.'

      return
      end
      function p04_fun ( x )

c*********************************************************************72
c
cc P04_FUN evaluates the function for problem 4.
c
c  Discussion:
c
c    This function is highly oscillatory near X = 0.
c
c  Dimension:
c
c    N = 1
c
c  Interval:
c
c    0 <= X <= 1
c
c  Function:
c
c    F(X) = sqrt ( x * ( 1 - x ) ) * sin ( 2.1 * pi / ( x + 0.05 ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the point at which the function
c    is to be evaluated.
c
c    Output, double precision P04_FUN, the value of the function at X.
c
      implicit none

      double precision p04_fun
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision value
      double precision x

      value = sqrt ( x * ( 1.0D+00 - x ) ) 
     &  * sin ( 2.1D+00 * pi / ( x + 0.05D+00 ) )

      p04_fun = value

      return
      end
      subroutine p04_lim ( a, b )

c*********************************************************************72
c
cc P04_LIM returns the limits of the approximation interval for problem 4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision A, B, the limits of the interval
c    of approximation.
c
      implicit none

      double precision a
      double precision b

      a = 0.0D+00
      b = 1.0D+00

      return
      end
      subroutine p04_story ( )

c*********************************************************************72
c
cc P04_STORY prints the "story" for problem 4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The Doppler function is continuous, but'
      write ( *, '(a)' ) '  highly oscillatory near the value X = 0.'

      return
      end
      subroutine p04_title ( title )

c*********************************************************************72
c
cc P04_TITLE returns the title of problem 4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character * ( * ) TITLE, the title of the problem.
c
      implicit none

      character * ( * ) title

      title = 'Doppler function, highly oscillatory near X = 0.'

      return
      end
      function p05_fun ( x )

c*********************************************************************72
c
cc P05_FUN evaluates the function for problem 5.
c
c  Discussion:
c
c    This example is difficult to interpolate because it has a piecewise
c    definition, and the character of the function changes dramatically
c    from piece to piece.
c
c  Dimension:
c
c    N = 1
c
c  Interval:
c
c    0 <= X <= 10
c
c  Function:
c
c    max ( sin(x) + sin(x^2), 1 - abs(x-5)/5 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the point at which the function
c    is to be evaluated.
c
c    Output, double precision P05_FUN, the value of the function at X.
c
      implicit none

      double precision p05_fun
      double precision x

      p05_fun = max ( sin ( x ) + sin ( x * x ), 
     &                1.0D+00 - abs ( x - 5.0D+00 ) / 5.0D+00 )

      return
      end
      subroutine p05_lim ( a, b )

c*********************************************************************72
c
cc P05_LIM returns the limits of the approximation interval for problem 5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision A, B, the limits of the interval
c    of approximation.
c
      implicit none

      double precision a
      double precision b

      a =  0.0D+00
      b = 10.0D+00

      return
      end
      subroutine p05_story ( )

c*********************************************************************72
c
cc P05_STORY prints the "story" for problem 5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  This example is very difficult to '
      write ( *, '(a)' ) '  interpolate.  It is essentially defined '
      write ( *, '(a)' ) '  as a piecewise function, alternating '
      write ( *, '(a)' ) '  between a straight line and a '
      write ( *, '(a)' ) '  sinusoidal curve.'

      return
      end
      subroutine p05_title ( title )

c*********************************************************************72
c
cc P05_TITLE returns the title of problem 5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character * ( * ) TITLE, the title of the problem.
c
      implicit none

      character * ( * ) title

      title = 
     &  'Rabbit ears, f(x) = max(sin(x)+sin(x^2),1-abs(x-5)/5), [0,10]'

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
