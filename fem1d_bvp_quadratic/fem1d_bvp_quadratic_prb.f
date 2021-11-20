      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM1D_BVP_QUADRATIC_PRB.
c
c  Discussion:
c
c    FEM1D_BVP_QUADRATIC_PRB tests the FEM1D_BVP_QUADRATIC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM1D_BVP_QUADRATIC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FEM1D_BVP_QUADRATIC library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM1D_BVP_QUADRATIC_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 carries out test case #1.
c
c  Discussion:
c
c    Use A1, C1, F1, EXACT1, EXACT_UX1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a1
      external a1
      double precision c1
      external c1
      double precision exact1
      external exact1
      double precision exact_ux1
      external exact_ux1
      double precision f1
      external f1
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  A1(X)  = 1.0'
      write ( *, '(a)' ) '  C1(X)  = 0.0'
      write ( *, '(a)' ) '  F1(X)  = X * ( X + 3 ) * exp ( X )'
      write ( *, '(a)' ) '  U1(X)  = X * ( 1 - X ) * exp ( X )'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a1, c1, f1, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact1 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact1, e1 )
      call l2_error_quadratic ( n, x, u, exact1, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux1, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a1 ( x )

c*********************************************************************72
c
cc A1 evaluates A function #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A1, the value of A(X).
c
      implicit none

      double precision a1
      double precision x

      a1 = 1.0D+00

      return
      end
      function c1 ( x )

c*********************************************************************72
c
cc C1 evaluates C function #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C1, the value of C(X).
c
      implicit none

      double precision c1
      double precision x

      c1 = 0.0D+00

      return
      end
      function exact1 ( x )

c*********************************************************************72
c
cc EXACT1 evaluates exact solution #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT1, the value of U(X).
c
      implicit none

      double precision exact1
      double precision x

      exact1 = x * ( 1.0D+00 - x ) * exp ( x )

      return
      end
      function exact_ux1 ( x )

c*********************************************************************72
c
cc EXACT_UX1 evaluates the derivative of exact solution #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX1, the value of dUdX(X).
c
      implicit none

      double precision exact_ux1
      double precision x

      exact_ux1 = ( 1.0D+00 - x - x * x ) * exp ( x )

      return
      end
      function f1 ( x )

c*********************************************************************72
c
cc F1 evaluates right hand side function #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F1, the value of F(X).
c
      implicit none

      double precision f1
      double precision x

      f1 = x * ( x + 3.0D+00 ) * exp ( x )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 carries out test case #2.
c
c  Discussion:
c
c    Use A2, C2, F2, EXACT2, EXACT_UX2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a2
      external a2
      double precision c2
      external c2
      double precision exact2
      external exact2
      double precision exact_ux2
      external exact_ux2
      double precision f2
      external f2
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  A2(X)  = 1.0'
      write ( *, '(a)' ) '  C2(X)  = 2.0'
      write ( *, '(a)' ) '  F2(X)  = X * ( 5 - X ) * exp ( X )'
      write ( *, '(a)' ) '  U2(X)  = X * ( 1 - X ) * exp ( X )'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a2, c2, f2, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact2 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact2, e1 )
      call l2_error_quadratic ( n, x, u, exact2, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux2, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a2 ( x )

c*********************************************************************72
c
cc A2 evaluates A function #2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A2, the value of A(X).
c
      implicit none

      double precision a2
      double precision x

      a2 = 1.0D+00

      return
      end
      function c2 ( x )

c*********************************************************************72
c
cc C2 evaluates C function #2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C2, the value of C(X).
c
      implicit none

      double precision c2
      double precision x

      c2 = 2.0D+00

      return
      end
      function exact2 ( x )

c*********************************************************************72
c
cc EXACT2 evaluates exact solution #2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT2, the value of U(X).
c
      implicit none

      double precision exact2
      double precision x

      exact2 = x * ( 1.0D+00 - x ) * exp ( x )

      return
      end
      function exact_ux2 ( x )

c*********************************************************************72
c
cc EXACT_UX2 evaluates the derivative of exact solution #2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX2, the value of dUdX(X).
c
      implicit none

      double precision exact_ux2
      double precision x

      exact_ux2 = ( 1.0D+00 - x - x * x ) * exp ( x )

      return
      end
      function f2 ( x )

c*********************************************************************72
c
cc F2 evaluates right hand side function #2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F2, the value of F(X).
c
      implicit none

      double precision f2
      double precision x

      f2 = x * ( 5.0D+00 - x ) * exp ( x )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 carries out test case #3.
c
c  Discussion:
c
c    Use A3, C3, F3, EXACT3, EXACT_UX3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a3
      external a3
      double precision c3
      external c3
      double precision exact3
      external exact3
      double precision exact_ux3
      external exact_ux3
      double precision f3
      external f3
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  A3(X)  = 1.0'
      write ( *, '(a)' ) '  C3(X)  = 2.0 * X'
      write ( *, '(a)' ) 
     &  '  F3(X)  = - X * ( 2 * X * X - 3 * X - 3 ) * exp ( X )'
      write ( *, '(a)' ) '  U3(X)  = X * ( 1 - X ) * exp ( X )'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a3, c3, f3, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact3 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact3, e1 )
      call l2_error_quadratic ( n, x, u, exact3, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux3, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a3 ( x )

c*********************************************************************72
c
cc A3 evaluates A function #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A3, the value of A(X).
c
      implicit none

      double precision a3
      double precision x

      a3 = 1.0D+00

      return
      end
      function c3 ( x )

c*********************************************************************72
c
cc C3 evaluates C function #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C3, the value of C(X).
c
      implicit none

      double precision c3
      double precision x

      c3 = 2.0D+00 * x

      return
      end
      function exact3 ( x )

c*********************************************************************72
c
cc EXACT3 evaluates exact solution #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT3, the value of U(X).
c
      implicit none

      double precision exact3
      double precision x

      exact3 = x * ( 1.0D+00 - x ) * exp ( x )

      return
      end
      function exact_ux3 ( x )

c*********************************************************************72
c
cc EXACT_UX3 evaluates the derivative of exact solution #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX3, the value of dUdX(X).
c
      implicit none

      double precision exact_ux3
      double precision x

      exact_ux3 = ( 1.0D+00 - x - x * x ) * exp ( x )

      return
      end
      function f3 ( x )

c*********************************************************************72
c
cc F3 evaluates right hand side function #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F3, the value of F(X).
c
      implicit none

      double precision f3
      double precision x

      f3 = - x * ( 2.0D+00 * x * x - 3.0D+00 * x - 3.0D+00 ) * exp ( x )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 carries out test case #4.
c
c  Discussion:
c
c    Use A4, C4, F4, EXACT4, EXACT_UX4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a4
      external a4
      double precision c4
      external c4
      double precision exact4
      external exact4
      double precision exact_ux4
      external exact_ux4
      double precision f4
      external f4
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  A4(X)  = 1.0 + X * X'
      write ( *, '(a)' ) '  C4(X)  = 0.0'
      write ( *, '(a)' ) 
     &  '  F4(X)  = ( X + 3 X^2 + 5 X^3 + X^4 ) * exp ( X )'
      write ( *, '(a)' ) '  U4(X)  = X * ( 1 - X ) * exp ( X )'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a4, c4, f4, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact4 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact4, e1 )
      call l2_error_quadratic ( n, x, u, exact4, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux4, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a4 ( x )

c*********************************************************************72
c
cc A4 evaluates A function #4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A4, the value of A(X).
c
      implicit none

      double precision a4
      double precision x

      a4 = 1.0D+00 + x * x

      return
      end
      function c4 ( x )

c*********************************************************************72
c
cc C4 evaluates C function #4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C4, the value of C(X).
c
      implicit none

      double precision c4
      double precision x

      c4 = 0.0D+00

      return
      end
      function exact4 ( x )

c*********************************************************************72
c
cc EXACT4 evaluates exact solution #4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT4, the value of U(X).
c
      implicit none

      double precision exact4
      double precision x

      exact4 = x * ( 1.0D+00 - x ) * exp ( x )

      return
      end
      function exact_ux4 ( x )

c*********************************************************************72
c
cc EXACT_UX4 evaluates the derivative of exact solution #4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX4, the value of dUdX(X).
c
      implicit none

      double precision exact_ux4
      double precision x

      exact_ux4 = ( 1.0D+00 - x - x * x ) * exp ( x )

      return
      end
      function f4 ( x )

c*********************************************************************72
c
cc F4 evaluates right hand side function #4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F4, the value of F(X).
c
      implicit none

      double precision f4
      double precision x

      f4 = ( x + 3.0D+00 * x * x 
     &  + 5.0D+00 * x * x * x + x * x * x * x ) * exp ( x )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 carries out test case #5.
c
c  Discussion:
c
c    Use A5, C5, F5, EXACT5, EXACT_UX5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a5
      external a5
      double precision c5
      external c5
      double precision exact5
      external exact5
      double precision exact_ux5
      external exact_ux5
      double precision f5
      external f5
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  A5(X)  = 1.0 + X * X for X .le. 1/3'
      write ( *, '(a)' ) '         = 7/9 + X     for      1/3 < X'
      write ( *, '(a)' ) '  C5(X)  = 0.0'
      write ( *, '(a)' ) 
     &  '  F5(X)  = ( X + 3 X^2 + 5 X^3 + X^4 ) * exp ( X )'
      write ( *, '(a)' ) '                       for X .le. 1/3'
      write ( *, '(a)' ) 
     &  '         = ( - 1 + 10/3 X + 43/9 X^2 + X^3 ) .* exp ( X )'
      write ( *, '(a)' ) '                       for      1/3 .le. X'
      write ( *, '(a)' ) '  U5(X)  = X * ( 1 - X ) * exp ( X )'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a5, c5, f5, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact5 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact5, e1 )
      call l2_error_quadratic ( n, x, u, exact5, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux5, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a5 ( x )

c*********************************************************************72
c
cc A5 evaluates A function #5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A5, the value of A(X).
c
      implicit none

      double precision a5
      double precision x

      if ( x .le. 1.0D+00 / 3.0D+00 ) then
        a5 = 1.0D+00 + x * x
      else
        a5 = x + 7.0D+00 / 9.0D+00
      end if

      return
      end
      function c5 ( x )

c*********************************************************************72
c
cc C5 evaluates C function #5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C5, the value of C(X).
c
      implicit none

      double precision c5
      double precision x

      c5 = 0.0D+00

      return
      end
      function exact5 ( x )

c*********************************************************************72
c
cc EXACT5 evaluates exact solution #5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT5, the value of U(X).
c
      implicit none

      double precision exact5
      double precision x

      exact5 = x * ( 1.0D+00 - x ) * exp ( x )

      return
      end
      function exact_ux5 ( x )

c*********************************************************************72
c
cc EXACT_UX5 evaluates the derivative of exact solution #5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX5, the value of dUdX(X).
c
      implicit none

      double precision exact_ux5
      double precision x

      exact_ux5 = ( 1.0D+00 - x - x * x ) * exp ( x )

      return
      end
      function f5 ( x )

c*********************************************************************72
c
cc F5 evaluates right hand side function #5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F5, the value of F(X).
c
      implicit none

      double precision f5
      double precision x

      if ( x .le. 1.0D+00 / 3.0D+00 ) then
        f5 = ( x + 3.0D+00 * x * x + 5.0D+00 * x**3 + x**4 ) * exp ( x )
      else
        f5 = ( - 1.0D+00 + ( 10.0D+00 / 3.0D+00 ) * x 
     &    + ( 43.0D+00 / 9.0D+00 ) * x * x + x * x * x ) * exp ( x )
      end if

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 does an error analysis.
c
c  Discussion:
c
c    Use A6, C6, F6, EXACT6, EXACT_UX6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a6
      external a6
      double precision c6
      external c6
      double precision exact6
      external exact6
      double precision exact_ux6
      external exact_ux6
      double precision f6
      external f6
      integer i
      double precision e1
      double precision e2
      integer n
      double precision h1s
      double precision u(161)
      double precision uexact
      double precision x(161)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  A6(X)  = 1.0 '
      write ( *, '(a)' ) '  C6(X)  = 0.0'
      write ( *, '(a)' ) '  F6(X)  = pi*pi*sin(pi*X)'
      write ( *, '(a)' ) '  U6(X)  = sin(pi*x)'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Compute L2 norm and seminorm of error for various N.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N        L1 error        L2 error ' // 
     &  '      Seminorm error'
      write ( *, '(a)' ) ' '

      n = 11

      do i = 0, 4
c
c  Geometry definitions.
c
        x_first = 0.0D+00
        x_last = 1.0D+00

        call r8vec_even ( n, x_first, x_last, x )

        call fem1d_bvp_quadratic ( n, a6, c6, f6, x, u )

        call l1_error ( n, x, u, exact6, e1 )
        call l2_error_quadratic ( n, x, u, exact6, e2 )
        call h1s_error_quadratic ( n, x, u, exact_ux6, 
     &    h1s )

        write ( *, '(2x,i4,2x,f14.6,2x,f14.6,2x,f14.6)' ) 
     &    n, e1, e2, h1s

        n = 2 * ( n - 1 ) + 1

      end do

      return
      end
      function a6 ( x )

c*********************************************************************72
c
cc A6 evaluates A function #6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A6, the value of A(X).
c
      implicit none

      double precision a6
      double precision x

      a6 = 1.0D+00

      return
      end
      function c6 ( x )

c*********************************************************************72
c
cc C6 evaluates C function #6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C6, the value of C(X).
c
      implicit none

      double precision c6
      double precision x

      c6 = 0.0D+00

      return
      end
      function exact6 ( x )

c*********************************************************************72
c
cc EXACT6 evaluates exact solution #6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT6, the value of U(X).
c
      implicit none

      double precision exact6
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      exact6 = sin ( pi * x )

      return
      end
      function exact_ux6 ( x )

c*********************************************************************72
c
cc EXACT_UX6 evaluates the derivative of exact solution #6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX6, the value of dUdX(X).
c
      implicit none

      double precision exact_ux6
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      exact_ux6 = pi * cos ( pi * x )

      return
      end
      function f6 ( x )

c*********************************************************************72
c
cc F6 evaluates right hand side function #6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F6, the value of F(X).
c
      implicit none

      double precision f6
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      f6 = pi * pi * sin ( pi * x )

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 does an error analysis.
c
c  Discussion:
c
c    Use A7, C7, F7, EXACT7, EXACT_UX7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Eric Becker, Graham Carey, John Oden,
c    Finite Elements, An Introduction, Volume I,
c    Prentice-Hall, 1981, page 123-124,
c    ISBN: 0133170578,
c    LC: TA347.F5.B4.
c
      implicit none

      integer n_max
      parameter ( n_max = 161 )

      double precision a7
      external a7
      double precision c7
      external c7
      double precision exact7
      external exact7
      double precision exact_ux7
      external exact_ux7
      double precision f7
      external f7
      integer i
      double precision e1
      double precision e2
      integer n
      double precision h1s
      double precision u(n_max)
      double precision uexact
      double precision x(n_max)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  Becker/Carey/Oden example'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Compute L2 norm and seminorm of error for various N.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N        L1 error      L2 error ' // 
     &  '      Seminorm error'
      write ( *, '(a)' ) ' '

      n = 11

      do i = 0, 4
c
c  Geometry definitions.
c
        x_first = 0.0D+00
        x_last = 1.0D+00
        call r8vec_even ( n, x_first, x_last, x )

        call fem1d_bvp_quadratic ( n, a7, c7, f7, x, u )

        call l1_error ( n, x, u, exact7, e1 )
        call l2_error_quadratic ( n, x, u, exact7, e2 )
        call h1s_error_quadratic ( n, x, u, exact_ux7, 
     &    h1s )

        write ( *, '(2x,i4,2x,f14.6,2x,f14.6,2x,f14.6)' ) 
     &    n, e1, e2, h1s

        n = 2 * ( n - 1 ) + 1

      end do

      return
      end
      function a7 ( x )

c*********************************************************************72
c
cc A7 evaluates A function #7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A7, the value of A(X).
c
      implicit none

      double precision a7
      double precision alpha
      double precision x
      double precision x0

      alpha = 30.0D+00
      x0 = 1.0D+00 / 3.0D+00
      a7 = 1.0D+00 / alpha + alpha * ( x - x0 ) ** 2

      return
      end
      function c7 ( x )

c*********************************************************************72
c
cc C7 evaluates C function #7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C7, the value of C(X).
c
      implicit none

      double precision c7
      double precision x

      c7 = 0.0D+00

      return
      end
      function exact7 ( x )

c*********************************************************************72
c
cc EXACT7 evaluates exact solution #7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT7, the value of U(X).
c
      implicit none

      double precision alpha
      double precision exact7
      double precision x
      double precision x0

      alpha = 30.0D+00
      x0 = 1.0D+00 / 3.0D+00
      exact7 = ( 1.0D+00 - x ) 
     &  * ( atan ( alpha * ( x - x0 ) ) + atan ( alpha * x0 ) )

      return
      end
      function exact_ux7 ( x )

c*********************************************************************72
c
cc EXACT_UX7 evaluates the derivative of exact solution #7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX7, the value of dUdX(X).
c
      implicit none

      double precision alpha
      double precision exact_ux7
      double precision x
      double precision x0
  
      alpha = 30.0D+00
      x0 = 1.0D+00 / 3.0D+00
      exact_ux7 = - atan ( alpha * ( x - x0 ) ) - atan ( alpha * x0 ) 
     &  + ( 1.0 - x ) * alpha 
     &  / ( 1.0 + alpha * alpha * ( x - x0 ) ** 2 )

      return
      end
      function f7 ( x )

c*********************************************************************72
c
cc F7 evaluates right hand side function #7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F7, the value of F(X).
c
      implicit none

      double precision alpha
      double precision f7
      double precision x
      double precision x0

      alpha = 30.0D+00
      x0 = 1.0D+00 / 3.0D+00
      f7 = 2.0D+00 * ( 1.0D+00 + alpha * ( x - x0 ) * 
     &  ( atan ( alpha * ( x - x0 ) ) + atan ( alpha * x0 ) ) )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 carries out test case #8.
c
c  Discussion:
c
c    Use A8, C8, F8, EXACT8, EXACT_UX8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a8
      external a8
      double precision c8
      external c8
      double precision exact8
      external exact8
      double precision exact_ux8
      external exact_ux8
      double precision f8
      external f8
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
      write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
      write ( *, '(a)' ) '  A8(X)  = 1.0'
      write ( *, '(a)' ) '  C8(X)  = 0.0'
      write ( *, '(a)' ) 
     &  '  F8(X)  = X * ( X + 3 ) * exp ( X ),   X .le. 2/3'
      write ( *, '(a)' ) 
     &  '         = 2 * exp ( 2/3),                   2/3 < X'
      write ( *, '(a)' ) 
     &  '  U8(X)  = X * ( 1 - X ) * exp ( X ),   X .le. 2/3'
      write ( *, '(a)' ) 
     &  '         = X * ( 1 - X ) * exp ( 2/3 ),      2/3 < X'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a8, c8, f8, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact8 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact8, e1 )
      call l2_error_quadratic ( n, x, u, exact8, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux8, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a8 ( x )

c*********************************************************************72
c
cc A8 evaluates A function #8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A8, the value of A(X).
c
      implicit none

      double precision a8
      double precision x

      a8 = 1.0D+00

      return
      end
      function c8 ( x )

c*********************************************************************72
c
cc C8 evaluates C function #8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C8, the value of C(X).
c
      implicit none

      double precision c8
      double precision x

      c8 = 0.0D+00

      return
      end
      function exact8 ( x )

c*********************************************************************72
c
cc EXACT8 evaluates exact solution #8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT8, the value of U(X).
c
      implicit none

      double precision exact8
      double precision x

      if ( x .le. 2.0D+00 / 3.0D+00 ) then
        exact8 = x * ( 1.0D+00 - x ) * exp ( x )
      else
        exact8 = x * ( 1.0D+00 - x ) * exp ( 2.0D+00 / 3.0D+00 )
      end if

      return
      end
      function exact_ux8 ( x )

c*********************************************************************72
c
cc EXACT_UX8 evaluates the derivative of exact solution #8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX8, the value of dUdX(X).
c
      implicit none

      double precision exact_ux8
      double precision x

      if ( x .le. 2.0D+00 / 3.0D+00 ) then
        exact_ux8 = ( 1.0D+00 - x - x * x ) * exp ( x )
      else
        exact_ux8 = ( 1.0D+00 - 2.0D+00 * x ) 
     &    * exp ( 2.0D+00 / 3.0D+00 )
      end if

      return
      end
      function f8 ( x )

c*********************************************************************72
c
cc F8 evaluates the F function for case #8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F8, the value of F(X).
c
      implicit none

      double precision f8
      double precision x

      if ( x .le. 2.0D+00 / 3.0D+00 ) then
        f8 = x * ( x + 3.0D+00 ) * exp ( x )
      else
        f8 = 2.0D+00 * exp ( 2.0D+00 / 3.0D+00 )
      end if

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 carries out test case #9.
c
c  Discussion:
c
c    Use A9, C9, F9, EXACT9, EXACT_UX9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dianne O'Leary,
c    Scientific Computing with Case Studies,
c    SIAM, 2008,
c    ISBN13: 978-0-898716-66-5,
c    LC: QA401.O44.
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a9
      external a9
      double precision c9
      external c9
      double precision exact9
      external exact9
      double precision exact_ux9
      external exact_ux9
      double precision f9
      external f9
      integer i
      double precision e1
      double precision e2
      double precision h1s
      double precision u(n)
      double precision uexact
      double precision x(n)
      double precision x_first
      double precision x_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) '  Solve -( A(x) U''(x) )'' + C(x) U(x) = F(x)'
      write ( *, '(a)' ) '  for 0 < x < 1, with U(0) = U(1) = 0.'
      write ( *, '(a)' ) '  A8(X)  = 1.0'
      write ( *, '(a)' ) '  C8(X)  = 0.0'
      write ( *, '(a)' ) 
     &  '  F9(X)  = X * ( X + 3 ) * exp ( X ),   X .le. 2/3'
      write ( *, '(a)' ) 
     &  '         = 2 * exp ( 2/3),                   2/3 < X'
      write ( *, '(a)' ) 
     &  '  U9(X)  = X * ( 1 - X ) * exp ( X ),   X .le. 2/3'
      write ( *, '(a)' ) 
     &  '         = X * ( 1 - X ),                    2/3 < X'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', n
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( n, x_first, x_last, x )

      call fem1d_bvp_quadratic ( n, a9, c9, f9, x, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I    X         U         Uexact    Error'
      write ( *, '(a)' ) ' '

      do i = 1, n
        uexact = exact9 ( x(i) )
        write ( *, '(2x,i4,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), u(i), uexact, abs ( u(i) - uexact )
      end do

      call l1_error ( n, x, u, exact9, e1 )
      call l2_error_quadratic ( n, x, u, exact9, e2 )
      call h1s_error_quadratic ( n, x, u, exact_ux9, h1s )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a9 ( x )

c*********************************************************************72
c
cc A9 evaluates A function #9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision A9, the value of A(X).
c
      implicit none

      double precision a9
      double precision x

      a9 = 1.0D+00

      return
      end
      function c9 ( x )

c*********************************************************************72
c
cc C9 evaluates C function #9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision C9, the value of C(X).
c
      implicit none

      double precision c9
      double precision x

      c9 = 0.0D+00

      return
      end
      function exact9 ( x )

c*********************************************************************72
c
cc EXACT9 evaluates exact solution #9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT9, the value of U(X).
c
      implicit none

      double precision exact9
      double precision x

      if ( x .le. 2.0D+00 / 3.0D+00 ) then
        exact9 = x * ( 1.0D+00 - x ) * exp ( x )
      else
        exact9 = x * ( 1.0D+00 - x )
      end if

      return
      end
      function exact_ux9 ( x )

c*********************************************************************72
c
cc EXACT_UX9 evaluates the derivative of exact solution #9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision EXACT_UX9, the value of dUdX(X).
c
      implicit none

      double precision exact_ux9
      double precision x

      if ( x .le. 2.0D+00 / 3.0D+00 ) then
        exact_ux9 = ( 1.0D+00 - x - x * x ) * exp ( x )
      else
        exact_ux9 = 1.0D+00 - 2.0D+00 * x
      end if

      return
      end
      function f9 ( x )

c*********************************************************************72
c
cc F9 evaluates the F function for case #9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision F9, the value of F(X).
c
      implicit none

      double precision f9
      double precision x

      if ( x .le. 2.0D+00 / 3.0D+00 ) then
        f9 = x * ( x + 3.0D+00 ) * exp ( x )
      else
        f9 = 2.0D+00
      end if

      return
      end
