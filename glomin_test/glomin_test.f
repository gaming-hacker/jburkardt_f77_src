      program main

c*********************************************************************72
c
cc glomin_test() tests glomin().
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2021
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision e
      double precision h_01
      external h_01
      double precision h_02
      external h_02
      double precision h_03
      external h_03
      double precision h_04
      external h_04
      double precision h_05
      external h_05
      double precision m
      double precision machep
      double precision r8_epsilon
      double precision t

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'glomin_test():'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  glomin() seeks a global minimizer of a'
      write ( *, '(a)' ) '  a function F(X) in an interval [A,B],'
      write ( *, '(a)' ) '  given some upper bound M for F".'

      machep = r8_epsilon ( )
      e = sqrt ( machep )
      t = sqrt ( machep )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Tolerances:'
      write ( *, '(a,g14.6)' ) '  e = ', e
      write ( *, '(a,g14.6)' ) '  t = ', t

      a = 7.0D+00
      b = 9.0D+00
      c = ( a + b ) / 2.0D+00
      m = 0.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_01,
     &  'h_01(x) = 2 - x' )

      a = 7.0D+00
      b = 9.0D+00
      c = ( a + b ) / 2.0D+00
      m = 100.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_01,
     &  'h_01(x) = 2 - x' )

      a = -1.0D+00
      b = +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 2.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_02,
     &  'h_02(x) = x * x' )

      a = -1.0D+00
      b = +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 2.1D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_02,
     &  'h_02(x) = x * x' )

      a = -0.5D+00
      b =  +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 14.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_03,
     &  'h_03(x) = x^3 + x^2' )

      a = -0.5D+00
      b =  +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 28.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_03,
     &  'h_03(x) = x^3 + x^2' )

      a = -10.0D+00
      b = +10.0D+00
      c = ( a + b ) / 2.0D+00
      m = 72.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_04,
     &  'h_04(x) = ( x + sin(x) ) * exp(-x*x)' )

      a = -10.0D+00
      b = +10.0D+00
      c = ( a + b ) / 2.0D+00
      m = 72.0D+00
      call glomin_example ( a, b, c, m, machep, e, t, h_05,
     &  'h_05(x) = ( x - sin(x) ) * exp(-x*x)' )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'glomin_test():'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 0
      end
      subroutine glomin_example ( a, b, c, m, machep, e, t, f, title )

c*********************************************************************72
c
cc glomin_example() tests glomin() on one test function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2021
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    double precision A, B, the endpoints of the interval.
c
c    double precision C, an initial guess for the global
c    minimizer.  If no good guess is known, C = A or B is acceptable.
c
c    double precision M, the bound on the second derivative.
c
c    double precision MACHEP, an estimate for the relative machine
c    precision.
c
c    double precision E, a positive tolerance, a bound for the
c    absolute error in the evaluation of F(X) for any X in [A,B].
c
c    double precision T, a positive absolute error tolerance.
c
c    external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose global minimum is being sought.
c
c    character * ( * ) TITLE, a title for the problem.
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer calls
      double precision e
      double precision f
      external f
      double precision fa
      double precision fb
      double precision fx
      double precision glomin
      double precision m
      double precision machep
      double precision t
      character*(*) title
      double precision x

      fx = glomin ( a, b, c, m, machep, e, t, f, x, calls )
      fa = f ( a )
      fb = f ( b )

      write ( *, '(a)' ) ' '
      write ( *, '(2x,a)' ) title
      write ( *, '(a,g14.6)' ) '  M = ', m
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A                 X             B'
      write ( *, '(a)' ) '    F(A)              F(X)          F(B)'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,f14.8,2x,f14.8,2x,f14.8)' ) a,  x,  b
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) fa, fx, fb
      write ( *, '(a,i8)' ) '  Number of calls to F = ', calls

      return
      end
      function h_01 ( x )

c*********************************************************************72
c
cc h_01() evaluates 2 - x.
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
c    double precision H_01, the value of the function at X.
c
      implicit none

      double precision h_01
      double precision x

      h_01 = 2.0D+00 - x

      return
      end
      function h_02 ( x )

c*********************************************************************72
c
cc h_02() evaluates x^2.
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
c    double precision H_02, the value of the function at X.
c
      implicit none

      double precision h_02
      double precision x

      h_02 = x * x

      return
      end
      function h_03 ( x )

c*********************************************************************72
c
cc h_03() evaluates x^3+x^2.
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
c    double precision H_03, the value of the function at X.
c
      implicit none

      double precision h_03
      double precision x

      h_03 = x * x * ( x + 1.0D+00 )

      return
      end
      function h_04 ( x )

c*********************************************************************72
c
cc h_04() evaluates ( x + sin ( x ) ) * exp ( - x * x ).
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
c    double precision H_04, the value of the function at X.
c
      implicit none

      double precision h_04
      double precision x

      h_04 = ( x + sin ( x ) ) * exp ( - x * x )

      return
      end
      function h_05 ( x )

c*********************************************************************72
c
cc h_05() evaluates ( x - sin ( x ) ) * exp ( - x * x ).
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
c    double precision H_05, the value of the function at X.
c
      implicit none

      double precision h_05
      double precision x

      h_05 = ( x - sin ( x ) ) * exp ( - x * x )

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

