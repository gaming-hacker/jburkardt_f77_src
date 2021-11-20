      program main

c*********************************************************************72
c
cc zero_test() tests zero().
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2021
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision f_01
      external f_01
      double precision f_02
      external f_02
      double precision f_03
      external f_03
      double precision f_04
      external f_04
      double precision f_05
      external f_05
      character * 80 title

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'brent_test():'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  zero() seeks a root of a function F(X)'
      write ( *, '(a)' ) '  in an interval [A,B].'

      a = 1.0D+00
      b = 2.0D+00
      title = 'f_01(x) = sin ( x ) - x / 2'
      call zero_example ( a, b, f_01, title )

      a = 0.0D+00
      b = 1.0D+00
      title = 'f_02(x) = 2 * x - exp ( - x )'
      call zero_example ( a, b, f_02, title )

      a = -1.0D+00
      b =  0.5D+00
      title = 'f_03(x) = x * exp ( - x )'
      call zero_example ( a, b, f_03, title )

      a =  0.0001D+00
      b =  20.0D+00
      title = 'f_04(x) = exp ( x ) - 1 / ( 100 * x * x )'
      call zero_example ( a, b, f_04, title )

      a = -5.0D+00
      b =  2.0D+00
      title = 'f_05(x) = (x+3) * (x-1) * (x-1)'
      call zero_example ( a, b, f_05, title )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'zero_test():'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 0
      end
      subroutine zero_example ( a, b, f, title )

c*********************************************************************72
c
cc zero_example() tests zero() on one test function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2021
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision A, B, the endpoints of the change of sign interval.
c
c    external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose zero is being sought.
c
c    character * ( * ) TITLE, a title for the problem.
c
      implicit none

      double precision a
      double precision b
      integer calls
      double precision f
      external f
      double precision fa
      double precision fb
      double precision fz
      double precision machep
      parameter ( machep = 2.220446049250313D-016 )
      double precision t
      character*(*) title
      double precision z
      double precision zero

      t = machep

      z = zero ( a, b, t, f, calls )
      fz = f ( z )
      fa = f ( a )
      fb = f ( b )

      write ( *, '(a)' ) ' '
      write ( *, '(2x,a)' ) title
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A                 Z             B'
      write ( *, '(a)' ) '    F(A)              F(Z)          F(B)'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,f14.8,2x,f14.8,2x,f14.8)' ) a,  z,  b
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) fa, fz, fb
      write ( *, '(a,i8)' ) '  Number of calls to F = ', calls

      return
      end
      function f_01 ( x )

c*********************************************************************72
c
cc f_01() evaluates sin ( x ) - x / 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
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
c    double precision F_01, the value of the function at X.
c
      implicit none

      double precision f_01
      double precision x

      f_01 = sin ( x ) - 0.5D+00 * x

      return
      end
      function f_02 ( x )

c*********************************************************************72
c
cc f_02() evaluates 2*x-exp(-x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
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
c    double precision F_02, the value of the function at X.
c
      implicit none

      double precision f_02
      double precision x

      f_02 = 2.0D+00 * x - exp ( - x )

      return
      end
      function f_03 ( x )

c*********************************************************************72
c
cc f_03() evaluates x*exp(-x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
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
c    double precision F_03, the value of the function at X.
c
      implicit none

      double precision f_03
      double precision x

      f_03 = x * exp ( - x )

      return
      end
      function f_04 ( x )

c*********************************************************************72
c
cc f_04() evaluates exp(x) - 1 / (100*x*x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
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
c    double precision F_04, the value of the function at X.
c
      implicit none

      double precision f_04
      double precision x

      f_04 = exp ( x ) - 1.0D+00 / 100.0D+00 / x / x

      return
      end
      function f_05 ( x )

c*********************************************************************72
c
cc f_05() evaluates (x+3)*(x-1)*(x-1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
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
c    double precision F_05, the value of the function at X.
c
      implicit none

      double precision f_05
      double precision x

      f_05 = ( x + 3.0D+00 ) * ( x - 1.0D+00 ) * ( x - 1.0D+00 )

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

