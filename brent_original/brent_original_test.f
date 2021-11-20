      program main

c*********************************************************************72
c
cc BRENT_ORIGINAL_TEST tests BRENT_ORIGINAL.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BRENT_ORIGINAL_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test BRENT_ORIGINAL.'

      call test_zero_all ( )
      call test_local_min_all ( )
      call test_glomin_all ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BRENT_ORIGINAL_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop 0
      end
      subroutine test_zero_all ( )

c*********************************************************************72
c
cc TEST_ZERO_ALL tests ZERO on all test functions.
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
      double precision machep
      double precision r8_epsilon
      double precision t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_ZERO_ALL'
      write ( *, '(a)' ) '  Test the Brent ZERO routine, which seeks'
      write ( *, '(a)' ) '  a root of a function F(X)'
      write ( *, '(a)' ) '  in an interval [A,B].'

      machep = r8_epsilon ( )
      t = machep

      a = 1.0D+00
      b = 2.0D+00

      call test_zero_one ( a, b, machep, t, f_01,
     &  'f_01(x) = sin ( x ) - x / 2' )

      a = 0.0D+00
      b = 1.0D+00

      call test_zero_one ( a, b, machep, t, f_02,
     &  'f_02(x) = 2 * x - exp ( - x )' )

      a = -1.0D+00
      b =  0.5D+00

      call test_zero_one ( a, b, machep, t, f_03,
     &  'f_03(x) = x * exp ( - x )' )

      a =  0.0001D+00
      b =  20.0D+00

      call test_zero_one ( a, b, machep, t, f_04,
     &  'f_04(x) = exp ( x ) - 1 / ( 100 * x * x )' )

      a = -5.0D+00
      b =  2.0D+00

      call test_zero_one ( a, b, machep, t, f_05,
     &  'f_05(x) = (x+3) * (x-1) * (x-1)' )

      return
      end
      subroutine test_local_min_all ( )

c*********************************************************************72
c
cc TEST_LOCAL_MIN_ALL tests LOCAL_MIN on all test functions.
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
      implicit none

      double precision a
      double precision b
      double precision eps
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
      double precision r8_epsilon
      double precision t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_LOCAL_MIN_ALL'
      write ( *, '(a)' ) '  Test the LOCAL_MIN routine, which seeks'
      write ( *, '(a)' ) '  a local minimizer of a function F(X)'
      write ( *, '(a)' ) '  in an interval [A,B].'

      eps = 10.0D+00 * sqrt ( r8_epsilon ( ) )
      t = eps

      a = 0.0D+00
      b = 3.141592653589793D+00
      call test_local_min_one ( a, b, eps, t, g_01,
     &  'g_01(x) = ( x - 2 ) * ( x - 2 ) + 1' )

      a = 0.0D+00
      b = 1.0D+00
      call test_local_min_one ( a, b, eps, t, g_02,
     &  'g_02(x) = x * x + exp ( - x )' )

      a = -2.0D+00
      b =  2.0D+00
      call test_local_min_one ( a, b, eps, t, g_03,
     &  'g_03(x) = x^4 + 2x^2 + x + 3' )

      a =  0.0001D+00
      b =  1.0D+00
      call test_local_min_one ( a, b, eps, t, g_04,
     &  'g_04(x) = exp ( x ) + 1 / ( 100 x )' )

      a =  0.0002D+00
      b = 2.0D+00
      call test_local_min_one ( a, b, eps, t, g_05,
     &  'g_05(x) = exp ( x ) - 2x + 1/(100x) - 1/(1000000x^2)' )

      a =  1.8D+00
      b =  1.9D+00
      call test_local_min_one ( a, b, eps, t, g_06,
     &  'g_06(x) = -x*sin(10*pi*x)-1.0' )

      a = -1.2D+00
      b =  2.7D+00
      eps = 1.0D-05;
      t = 1.0D-07;
      call test_local_min_one ( a, b, eps, t, g_07,
     &  'g_07(x) = max(-2(x-1),8(x-1)) + 25*(x-1)^2' )

      return
      end
      subroutine test_glomin_all ( )

c*********************************************************************72
c
cc TEST_GLOMIN_ALL tests GLOMIN on all test functions.
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

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'test_glomin_all():'
      write ( *, '(a)' ) '  Test the Brent GLOMIN routine, which seeks'
      write ( *, '(a)' ) '  a global minimizer of a function F(X)'
      write ( *, '(a)' ) '  in an interval [A,B],'
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

      call test_glomin_one ( a, b, c, m, machep, e, t, h_01,
     &  'h_01(x) = 2 - x' )

      a = 7.0D+00
      b = 9.0D+00
      c = ( a + b ) / 2.0D+00
      m = 100.0D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_01,
     &  'h_01(x) = 2 - x' )

      a = -1.0D+00
      b = +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 2.0D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_02,
     &  'h_02(x) = x * x' )

      a = -1.0D+00
      b = +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 2.1D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_02,
     &  'h_02(x) = x * x' )

      a = -0.5D+00
      b =  +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 14.0D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_03,
     &  'h_03(x) = x^3 + x^2' )

      a = -0.5D+00
      b =  +2.0D+00
      c = ( a + b ) / 2.0D+00
      m = 28.0D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_03,
     &  'h_03(x) = x^3 + x^2' )

      a = -10.0D+00
      b = +10.0D+00
      c = ( a + b ) / 2.0D+00
      m = 72.0D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_04,
     &  'h_04(x) = ( x + sin(x) ) * exp(-x*x)' )

      a = -10.0D+00
      b = +10.0D+00
      c = ( a + b ) / 2.0D+00
      m = 72.0D+00

      call test_glomin_one ( a, b, c, m, machep, e, t, h_05,
     &  'h_05(x) = ( x - sin(x) ) * exp(-x*x)' )

      return
      end
      subroutine test_zero_one ( a, b, machep, t, f, title )

c*********************************************************************72
c
cc TEST_ZERO_ONE tests ZERO on one test function.
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
c  Parameters:
c
c    Input, double precision A, B, the two endpoints of the change of sign
c    interval.
c
c    Input, double precision MACHEP, an estimate for the relative machine
c    precision.
c
c    Input, double precision T, a positive error tolerance.
c
c    Input, external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose zero is being sought.
c
c    Input, character * ( * ) TITLE, a title for the problem.
c
      implicit none

      double precision a
      double precision b
      double precision f
      external f
      double precision fa
      double precision fb
      double precision fz
      double precision machep
      double precision t
      character*(*) title
      double precision z
      double precision zero

      z = zero ( a, b, machep, t, f )
      fz = f ( z )
      fa = f ( a )
      fb = f ( b )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A                 Z             B'
      write ( *, '(a)' ) '    F(A)              F(Z)          F(B)'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,f14.8,2x,f14.8,2x,f14.8)' ) a,  z,  b
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) fa, fz, fb

      return
      end
      subroutine test_local_min_one ( a, b, eps, t, f, title )

c*********************************************************************72
c
cc TEST_LOCAL_MIN_ONE tests LOCAL_MIN on one test function.
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
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, double precision EPS, a positive relative error tolerance.
c
c    Input, double precision T, a positive absolute error tolerance.
c
c    Input, external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose local minimum is being sought.
c
c    Input, character * ( * ) TITLE, a title for the problem.
c
      implicit none

      double precision a
      double precision b
      double precision eps
      double precision f
      external f
      double precision fa
      double precision fb
      double precision fx
      double precision localm
      double precision t
      character*(*) title
      double precision x

      fx = localm ( a, b, eps, t, f, x )
      fa = f ( a )
      fb = f ( b )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A                 X             B'
      write ( *, '(a)' ) '    F(A)              F(X)          F(B)'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,f14.8,2x,f14.8,2x,f14.8)' ) a,  x,  b
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) fa, fx, fb

      return
      end
      subroutine test_glomin_one ( a, b, c, m, machep, e, t, f, title )

c*********************************************************************72
c
cc test_glomin_one() tests GLOMIN on one test function.
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
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, double precision C, an initial guess for the global
c    minimizer.  If no good guess is known, C = A or B is acceptable.
c
c    Input, double precision M, the bound on the second derivative.
c
c    Input, double precision MACHEP, an estimate for the relative machine
c    precision.
c
c    Input, double precision E, a positive tolerance, a bound for the
c    absolute error in the evaluation of F(X) for any X in [A,B].
c
c    Input, double precision T, a positive absolute error tolerance.
c
c    Input, external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose global minimum is being sought.
c
c    Input, character * ( * ) TITLE, a title for the problem.
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
      write ( *, '(a,i8)' ) '  Calls = ', calls

      return
      end
      function f_01 ( x )

c*********************************************************************72
c
cc F_01 evaluates sin ( x ) - x / 2.
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision F_01, the value of the function at X.
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
cc F_02 evaluates 2*x-exp(-x).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision F_02, the value of the function at X.
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
cc F_03 evaluates x*exp(-x).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision F_03, the value of the function at X.
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
cc F_04 evaluates exp(x) - 1 / (100*x*x).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision F_04, the value of the function at X.
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
cc F_05 evaluates (x+3)*(x-1)*(x-1).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision F_05, the value of the function at X.
c
      implicit none

      double precision f_05
      double precision x

      f_05 = ( x + 3.0D+00 ) * ( x - 1.0D+00 ) * ( x - 1.0D+00 )

      return
      end
      function g_01 ( x )

c*********************************************************************72
c
cc G_01 evaluates (x-2)^2 + 1.
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision G_01, the value of the function at X.
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
cc G_02 evaluates x^2 + exp ( - x ).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision G_02, the value of the function at X.
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
cc G_03 evaluates x^4+2x^2+x+3.
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision G_03, the value of the function at X.
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
cc G_04 evaluates exp(x)+1/(100X).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision G_04, the value of the function at X.
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
cc G_05 evaluates exp(x) - 2x + 1/(100x) - 1/(1000000x^2).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision G_05, the value of the function at X.
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
cc G_06 evaluates - x * sin(10 pi x ) - 1.0;
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
c  Parameters:
c
c    Input, real X, the point at which F is to be evaluated.
c
c    Output, real G_06, the value of the function at X.
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
cc G_07 evaluates max(-2(x-1), 8(x-1)) + 25 (x-1)^2.
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
c  Parameters:
c
c    Input, real X, the point at which F is to be evaluated.
c
c    Output, real G_07, the value of the function at X.
c
      implicit none

      double precision g_07
      double precision x

      g_07 = max ( -2.0D+00 * ( x - 1.0D+00 ), 
     &              8.0D+00 * ( x - 1.0D+00 ) ) 
     &     + 25.0D+00 * ( x - 1.0D+00 ) ** 2

      return
      end

      function h_01 ( x )

c*********************************************************************72
c
cc H_01 evaluates 2 - x.
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision H_01, the value of the function at X.
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
cc H_02 evaluates x^2.
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision H_02, the value of the function at X.
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
cc H_03 evaluates x^3+x^2.
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision H_03, the value of the function at X.
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
cc H_04 evaluates ( x + sin ( x ) ) * exp ( - x * x ).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision H_04, the value of the function at X.
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
cc H_05 evaluates ( x - sin ( x ) ) * exp ( - x * x ).
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
c  Parameters:
c
c    Input, double precision X, the point at which F is to be evaluated.
c
c    Output, double precision H_05, the value of the function at X.
c
      implicit none

      double precision h_05
      double precision x

      h_05 = ( x - sin ( x ) ) * exp ( - x * x )

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

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
c    12 June 2014
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end
