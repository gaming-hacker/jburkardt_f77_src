      program main

c*********************************************************************72
c
cc local_min_test() tests local_min().
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2021
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision g_01
      external g_01
      double precision g_02
      external g_02
      double precision g_03
      external g_03
      double precision g_04
      external g_04
      double precision g_05
      external g_05
      double precision g_06
      external g_06
      double precision g_07
      external g_07

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'local_min_test():'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  local_min() seeks a local minimizer of'
      write ( *, '(a)' ) '  a function F(X) in an interval [A,B].'

      a = 0.0D+00
      b = 3.141592653589793D+00
      call local_min_example ( a, b, g_01,
     &  'g_01(x) = ( x - 2 ) * ( x - 2 ) + 1' )

      a = 0.0D+00
      b = 1.0D+00
      call local_min_example ( a, b, g_02,
     &  'g_02(x) = x * x + exp ( - x )' )

      a = -2.0D+00
      b =  2.0D+00
      call local_min_example ( a, b, g_03,
     &  'g_03(x) = x^4 + 2x^2 + x + 3' )

      a =  0.0001D+00
      b =  1.0D+00
      call local_min_example ( a, b, g_04,
     &  'g_04(x) = exp ( x ) + 1 / ( 100 x )' )

      a =  0.0002D+00
      b = 2.0D+00
      call local_min_example ( a, b, g_05,
     &  'g_05(x) = exp ( x ) - 2x + 1/(100x) - 1/(1000000x^2)' )

      a =  1.8D+00
      b =  1.9D+00
      call local_min_example ( a, b, g_06,
     &  'g_06(x) = -x*sin(10*pi*x)-1.0' )

      a = -1.2D+00
      b =  2.7D+00
      call local_min_example ( a, b, g_07,
     &  'g_07(x) = max(-2(x-1),8(x-1)) + 25*(x-1)^2' )

c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'local_min_test():'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 0
      end
      subroutine local_min_example ( a, b, f, title )

c*********************************************************************72
c
cc local_min_example() tests local_min() on one test function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 May 2021
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision A, B, the endpoints of the interval.
c
c    external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose local minimum is being sought.
c
c    character * ( * ) TITLE, a title for the problem.
c
      implicit none

      double precision a
      double precision b
      integer calls
      double precision eps
      double precision f
      external f
      double precision fa
      double precision fb
      double precision fx
      double precision local_min
      double precision r8_epsilon
      double precision t
      character*(*) title
      double precision x

      eps = r8_epsilon ( )
      t = sqrt ( eps )

      fx = local_min ( a, b, eps, t, f, x, calls )
      fa = f ( a )
      fb = f ( b )

      write ( *, '(a)' ) ' '
      write ( *, '(2x,a)' ) title
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A                 X             B'
      write ( *, '(a)' ) '    F(A)              F(X)          F(B)'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,f14.8,2x,f14.8,2x,f14.8)' ) a,  x,  b
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) fa, fx, fb
      write ( *, '(a,i8)' ) '  Number of calls to F = ', calls

      return
      end
      function g_01 ( x )

c*********************************************************************72
c
cc g_01() evaluates (x-2)^2 + 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_01, the value of the function at X.
c
      implicit none

      double precision g_01
      double precision x

      g_01 = ( x - 2.0D+00 ) * ( x - 2.0D+00 ) + 1.0D+00

      return
      end
      function g_02 ( x )

c*********************************************************************72
c
cc g_02() evaluates x^2 + exp ( - x ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_02, the value of the function at X.
c
      implicit none

      double precision g_02
      double precision x

      g_02 = x * x + exp ( - x )

      return
      end
      function g_03 ( x )

c*********************************************************************72
c
cc g_03() evaluates x^4+2x^2+x+3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_03, the value of the function at X.
c
      implicit none

      double precision g_03
      double precision x

      g_03 = ( ( x * x + 2.0D+00 ) * x + 1.0D+00 ) * x + 3.0D+00

      return
      end
      function g_04 ( x )

c*********************************************************************72
c
cc g_04() evaluates exp(x)+1/(100X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_04, the value of the function at X.
c
      implicit none

      double precision g_04
      double precision x

      g_04 = exp ( x ) + 0.01D+00 / x

      return
      end
      function g_05 ( x )

c*********************************************************************72
c
cc g_05() evaluates exp(x) - 2x + 1/(100x) - 1/(1000000x^2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_05, the value of the function at X.
c
      implicit none

      double precision g_05
      double precision x

      g_05 = exp ( x ) - 2.0D+00 * x + 0.01D+00 / x
     &  - 0.000001D+00 / x / x

      return
      end
      function g_06 ( x )

c*********************************************************************72
c
cc g_06() evaluates - x * sin(10 pi x ) - 1.0;
c
c  Discussion:
c
c    There is a local minimum between 1.80 and 1.90 at about
c    1.850547466.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_06, the value of the function at X.
c
      implicit none

      double precision g_06
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x

      g_06 = - x * sin ( 10.0D+00 * r8_pi * x ) - 1.0D+00

      return
      end
      function g_07 ( x )

c*********************************************************************72
c
cc g_07() evaluates max(-2(x-1), 8(x-1)) + 25 (x-1)^2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2019
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision X, the evaluation point.
c
c  Output:
c
c    double precision G_07, the value of the function at X.
c
      implicit none

      double precision g_07
      double precision x

      g_07 = max ( -2.0D+00 * ( x - 1.0D+00 ), 
     &              8.0D+00 * ( x - 1.0D+00 ) ) 
     &     + 25.0D+00 * ( x - 1.0D+00 ) ** 2

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc timestamp() prints out the current YMDHMS date as a timestamp.
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

