      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM2D_BVP_QUADRATIC.
c
c  Discussion:
c
c    FEM2D_BVP_QUADRATIC_PRB tests the FEM2D_BVP_QUADRATIC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM2D_BVP_QUADRATIC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FEM2D_BVP_QUADRATIC library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM2D_BVP_QUADRATIC_PRB'
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
c    Use A1, C1, F1, EXACT1, EXACT_UX1, EXACT_UY1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 3 )
      integer ny
      parameter ( ny = 3 )

      double precision a1
      external a1
      double precision c1
      external c1
      double precision exact1
      external exact1
      double precision exact_ux1
      external exact_ux1
      double precision exact_uy1
      external exact_uy1
      double precision f1
      external f1
      double precision e1
      double precision e2
      double precision h1s
      integer i
      integer j
      double precision u(nx,ny)
      double precision uexact
      double precision x(nx)
      double precision x_first
      double precision x_last
      double precision y(nx)
      double precision y_first
      double precision y_last

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Solve - del ( A del U ) + C U = F '
      write ( *, '(a)' ) 
     &  '  on the unit square with zero boundary conditions.'
      write ( *, '(a)' ) '  A1(X,Y) = 1.0'
      write ( *, '(a)' ) '  C1(X,Y) = 0.0'
      write ( *, '(a)' ) '  F1(X,Y) = 2*X*(1-X)+2*Y*(1-Y)'
      write ( *, '(a)' ) '  U1(X,Y) = X * ( 1 - X ) * Y * ( 1 - Y )'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of X grid values NX = ', nx
      write ( *, '(a,i8)' ) '  Number of Y grid values NY = ', ny
c
c  Geometry definitions.
c
      x_first = 0.0D+00
      x_last = 1.0D+00
      call r8vec_even ( nx, x_first, x_last, x )

      y_first = 0.0D+00
      y_last = 1.0D+00
      call r8vec_even ( ny, y_first, y_last, y )

      call fem2d_bvp_quadratic ( nx, ny, a1, c1, f1, x, y, u )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I     J    X         Y         U         
     &Uexact    Error'
      write ( *, '(a)' ) ' '

      do j = 1, ny
        do i = 1, nx
          uexact = exact1 ( x(i), y(j) )
          write ( *, 
     &      '(2x,i4,2x,i4,2x,f8.2,2x,f8.2,2x,g14.6,2x,g14.6,2x,g14.6)' )
     &      i, j, x(i), y(j), u(i,j), uexact, abs ( u(i,j) - uexact )
        end do
      end do

      call fem2d_l1_error ( nx, ny, x, y, u, exact1, e1 )
      call fem2d_l2_error_quadratic ( nx, ny, x, y, u, exact1, e2 )
      call fem2d_h1s_error_quadratic ( nx, ny, x, y, u, exact_ux1, 
     &  exact_uy1, h1s )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  l1 norm of error  = ', e1
      write ( *, '(a,g14.6)' ) '  L2 norm of error  = ', e2
      write ( *, '(a,g14.6)' ) '  Seminorm of error = ', h1s

      return
      end
      function a1 ( x, y )

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
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision A1, the value of A(X,Y).
c
      implicit none

      double precision a1
      double precision x
      double precision y

      a1 = 1.0D+00

      return
      end
      function c1 ( x, y )

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
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision C1, the value of C(X,Y).
c
      implicit none

      double precision c1
      double precision x
      double precision y

      c1 = 0.0D+00

      return
      end
      function exact1 ( x, y )

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
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision EXACT1, the value of U(X,Y).
c
      implicit none

      double precision exact1
      double precision x
      double precision y

      exact1 = x * ( 1.0D+00 - x ) * y * ( 1.0D+00 - y )

      return
      end
      function exact_ux1 ( x, y )

c*********************************************************************72
c
cc EXACT_UX1 evaluates the X derivative of the exact solution #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision EXACT_UX1, the value of dUdX(X,Y).
c
      implicit none

      double precision exact_ux1
      double precision x
      double precision y

      exact_ux1 = ( 1.0D+00 - 2.0D+00 * x ) * ( y - y * y )

      return
      end
      function exact_uy1 ( x, y )

c*********************************************************************72
c
cc EXACT_UY1 evaluates the Y derivative of the exact solution #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision EXACT_UY1, the value of dUdY(X,Y).
c
      implicit none

      double precision exact_uy1
      double precision x
      double precision y

      exact_uy1 = ( x - x * x ) * ( 1.0D+00 - 2.0D+00 * y )

      return
      end
      function f1 ( x, y )

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
c    24 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision F1, the value of F(X,Y).
c
      implicit none

      double precision f1
      double precision x
      double precision y

      f1 = 2.0D+00 * x * ( 1.0D+00 - x )     
     &   + 2.0D+00 * y * ( 1.0D+00 - y )

      return
      end

