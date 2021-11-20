      program main

c*********************************************************************72
c
cc FEM2D_BVP_SERENE_TEST tests the FEM2D_BVP_SERENE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FEM2D_BVP_SERENE_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FEM2D_BVP_SERENE library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FEM2D_BVP_SERENE_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 5 )
      integer ny
      parameter ( ny = 5 )

      integer node_num
      parameter ( node_num = nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)

      double precision a1
      external a1
      double precision c1
      external c1
      double precision e1
      double precision e2
      double precision exact1
      external exact1
      double precision exact_ux1
      external exact_ux1
      double precision exact_uy1
      external exact_uy1
      double precision f1
      external f1
      double precision h1s
      integer i
      integer inc
      integer j
      integer k
      double precision one
      parameter ( one = 1.0D+00 )
      logical show11
      double precision u(node_num)
      double precision uexact
      double precision x(nx)
      double precision y(ny)
      double precision zero
      parameter ( zero = 0.0D+00 )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) ' TEST01'
      write ( *, '(a)' ) '  Solve - del ( A del U ) + C U = F '
      write ( *, '(a)' ) 
     &  '  on the unit square with zero boundary conditions.'
      write ( *, '(a)' ) '  A1(X,Y) = 1.0'
      write ( *, '(a)' ) '  C1(X,Y) = 0.0'
      write ( *, '(a)' ) '  F1(X,Y) = 2*X*(1-X)+2*Y*(1-Y).'
      write ( *, '(a)' ) '  U1(X,Y) = X * ( 1 - X ) * Y * ( 1 - Y )'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4,a,i4,a)' ) 
     &  '  The grid uses ', nx, ' by ', ny, ' nodes.'
      write ( *, '(a,i4)' ) '  The number of nodes is ', node_num
c
c  Geometry definitions.
c
      call r8vec_linspace ( nx, zero, one, x )
      call r8vec_linspace ( ny, zero, one, y )

      show11 = .false.
      call fem2d_bvp_serene ( nx, ny, a1, c1, f1, x, y, show11, u )

      if ( nx * ny .le. 25 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &  '     I     J      X         Y         U' //
     &  '         Uexact    Error'
        write ( *, '(a)' ) ''

        k = 0

        do j = 1, ny

          if ( mod ( j, 2 ) .eq. 1 ) then
            inc = 1
          else
            inc = 2
          end if

          do i = 1, nx, inc
            k = k + 1
            uexact = exact1 ( x(i), y(j) )
            write ( *, 
     &        '(2x,i4,2x,i4,2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4,2x,e10.2)' )
     &        i, j, x(i), y(j), u(k), uexact, abs ( u(k) - uexact )
          end do
        end do

      end if

      call fem2d_l1_error_serene ( nx, ny, x, y, u, exact1, e1 )
      call fem2d_l2_error_serene ( nx, ny, x, y, u, exact1, e2 )
      call fem2d_h1s_error_serene ( nx, ny, x, y, u, exact_ux1, 
     &  exact_uy1, h1s )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  l1 error   = ', e1
      write ( *, '(a,g14.6)' ) '  L2 error   = ', e2
      write ( *, '(a,g14.6)' ) '  H1S error  = ', h1s

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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real A1, the value of A(X).
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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real C1, the value of C(X).
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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real EXACT1, the value of the solution.
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
cc EXACT_UX1 evaluates the derivative dUdX of exact solution #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real EXACT_UX1, the value of dUdX.
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
cc EXACT_UY1 evaluates the derivative dUdY of exact solution #1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real EXACT_UY1, the value of dUdX.
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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real F1, the value of the right hand side.
c
      implicit none

      double precision f1
      double precision x
      double precision y

      f1 = 2.0D+00 * x * ( 1.0D+00 - x ) 
     &   + 2.0D+00 * y * ( 1.0D+00 - y )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 checks the basis functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j
      double precision r8_uniform_01
      double precision r8vec_sum
      integer seed
      double precision v(8,8)
      double precision vx(8)
      double precision vy(8)
      double precision xe
      double precision xq
      double precision xw
      double precision xx(8)
      double precision yn
      double precision yq
      double precision ys
      double precision yy(8)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Basis function checks.'
c
c  Check that V is identity matrix at nodes.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  The matrix Aij = V(i)(X(j),Y(j)) should be the identity.'
      write ( *, '(a)' ) ''

      xw = 0.0D+00
      ys = 3.0D+00
      xe = 2.0D+00
      yn = 5.0D+00
      xx(1) = 2.0
      xx(2) = 1.0
      xx(3) = 0.0
      xx(4) = 0.0
      xx(5) = 0.0
      xx(6) = 1.0
      xx(7) = 2.0
      xx(8) = 2.0
      yy(1) = 5.0
      yy(2) = 5.0
      yy(3) = 5.0
      yy(4) = 4.0
      yy(5) = 3.0
      yy(6) = 3.0
      yy(7) = 3.0
      yy(8) = 4.0

      do j = 1, 8
        xq = xx(j)
        yq = yy(j)
        call basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v(1:8,j) )
      end do

      call r8mat_print ( 8, 8, v, '  V(i)(X(j),Y(j))' )
c
c  Check that VX and VY sum to zero anywhere.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  The vectors dVdX(1:8)(X,Y) and dVdY(1:8)(X,Y)'
      write ( *, '(a)' ) '  should both sum to zero for any (X,Y).'

      seed = 123456789
      xq = 2.0 * r8_uniform_01 ( seed )
      yq = 3.0 + 2.0 * r8_uniform_01 ( seed )
      xw = 0.0
      ys = 3.0
      xe = 2.0
      yn = 5.0
      xx(1) = 2.0
      xx(2) = 1.0
      xx(3) = 0.0
      xx(4) = 0.0
      xx(5) = 0.0
      xx(6) = 1.0
      xx(7) = 2.0
      xx(8) = 2.0
      yy(1) = 5.0
      yy(2) = 5.0
      yy(3) = 5.0
      yy(4) = 4.0
      yy(5) = 3.0
      yy(6) = 3.0
      yy(7) = 3.0
      yy(8) = 4.0
      call basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, vx, vy )
      
      write ( *, '(a)' ) ''
      write ( *, '(a,g10.4,a,g10.4,a)' ) 
     &  '  Random evaluation point is (', xq, ',', yq, ')'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '              dVdX        dVdY'
      write ( *, '(a)' ) ''
      do i = 1, 8
        write ( *, '(2x,i4,2x,g10.4,2x,g10.4)' ) i, vx(i), vy(i)
      end do
      write ( *, '(a)' ) ''
      write ( *, '(a,g10.4,2x,g10.4)' ) 
     &  '  Sum:  ', r8vec_sum ( 8, vx ), r8vec_sum ( 8, vy )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 carries out test case #3.
c
c  Discussion:
c
c    Use A3, C3, F3, EXACT3, EXACT_UX3, EXACT_UY3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 5 )
      integer ny
      parameter ( ny = 5 )
      integer node_num
      parameter ( node_num = nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)

      double precision a3
      external a3
      double precision amat(8,8)
      double precision c3
      external c3
      double precision e1
      double precision e2
      double precision exact3
      external exact3
      double precision exact_ux3
      external exact_ux3
      double precision exact_uy3
      external exact_uy3
      double precision f3
      external f3
      double precision h1s
      integer i
      integer inc
      integer j
      integer k
      double precision one
      parameter ( one = 1.0D+00 )
      double precision scale
      logical show11
      double precision u(node_num)
      double precision uexact
      double precision x(nx)
      double precision y(ny)
      double precision zero
      parameter ( zero = 0.0D+00 )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Solve - del ( A del U ) + C U = F '
      write ( *, '(a)' ) 
     &  '  on the unit square with zero boundary conditions.'
      write ( *, '(a)' ) '  A1(X,Y) = 0.0'
      write ( *, '(a)' ) '  C1(X,Y) = 1.0'
      write ( *, '(a)' ) '  F1(X,Y) = X * ( 1 - X ) * Y * ( 1 - Y ).'
      write ( *, '(a)' ) '  U1(X,Y) = X * ( 1 - X ) * Y * ( 1 - Y )'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  This example is contrived so that the system matrix'
      write ( *, '(a)' ) '  is the WATHEN matrix.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4,a,i4,a)' ) 
     &  '  The grid uses ', nx, ' by ', ny, ' nodes.'
      write ( *, '(a,i4)' ) '  The number of nodes is ', node_num
c
c  Geometry definitions.
c
      call r8vec_linspace ( nx, zero, one, x )
      call r8vec_linspace ( ny, zero, one, y )

      show11 = .true.

      call fem2d_bvp_serene ( nx, ny, a3, c3, f3, x, y, show11, u )

      if ( nx * ny .le. 25 ) then

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '     I     J      X         Y' //
     &    '         U         Uexact    Error'
        write ( *, '(a)' ) ''

        k = 0

        do j = 1, ny

          if ( mod ( j, 2 ) .eq. 1 ) then
            inc = 1
          else
            inc = 2
          end if

          do i = 1, nx, inc
            k = k + 1
            uexact = exact3 ( x(i), y(j) )
            write ( *, 
     &        '(2x,i4,2x,i4,2x,f8.4,2x,f8.4,2x,f8.4,2x,f8.4,2x,e10.2)' )
     &        i, j, x(i), y(j), u(k), uexact, abs ( u(k) - uexact )
          end do
        end do

      end if

      call fem2d_l1_error_serene ( nx, ny, x, y, u, exact3, e1 )
      call fem2d_l2_error_serene ( nx, ny, x, y, u, exact3, e2 )
      call fem2d_h1s_error_serene ( nx, ny, x, y, u, exact_ux3, 
     &  exact_uy3, h1s )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  l1 error   = ', e1
      write ( *, '(a,g14.6)' ) '  L2 error   = ', e2
      write ( *, '(a,g14.6)' ) '  H1S error  = ', h1s
c
c  Pull out the Wathen matrix from MATLAB.
c  It will have been multiplied by a random scale factor.
c  While my numbering scheme is
c    3  2  1
c    4     8
c    5  6  7
c  the numbering scheme used here is 
c    1  2  3
c    4     5
c    6  7  8
c    
      call wathen ( 1, 1, 8, amat )
     
      scale = 0.5 * amat(1,3)
      do j = 1, 8
        do i = 1, 8
          amat(i,j) = amat(i,j) / scale
        end do
      end do

      call r8mat_print ( 8, 8, amat, '  WATHEN matrix (permuted)' )

      return
      end
      function a3 ( x, y )

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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real A3, the value of A(X).
c
      implicit none

      double precision a3
      double precision x
      double precision y

      a3 = 0.0D+00

      return
      end
      function c3 ( x, y )

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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real C3, the value of C(X).
c
      implicit none

      double precision c3
      double precision x
      double precision y

      c3 = 1.0D+00

      return
      end
      function exact3 ( x, y )

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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real EXACT3, the value of the solution.
c
      implicit none

      double precision exact3
      double precision x
      double precision y

      exact3 = x * ( 1.0D+00 - x ) * y * ( 1.0D+00 - y )

      return
      end
      function exact_ux3 ( x, y )

c*********************************************************************72
c
cc EXACT_UX3 evaluates the derivative dUdX of exact solution #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real EXACT_UX3, the value of dUdX.
c
      implicit none

      double precision exact_ux3
      double precision x
      double precision y

      exact_ux3 = ( 1.0D+00 - 2.0D+00 * x ) * ( y - y * y )

      return
      end
      function exact_uy3 ( x, y )

c*********************************************************************72
c
cc EXACT_UY3 evaluates the derivative dUdY of exact solution #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real EXACT_UY3, the value of dUdX.
c
      implicit none

      double precision exact_uy3
      double precision x
      double precision y

      exact_uy3 = ( x - x * x ) * ( 1.0D+00 - 2.0D+00 * y )

      return
      end
      function f3 ( x, y )

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
c    01 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, Y, the evaluation point.
c
c    Output, real F3, the value of the right hand side.
c
      implicit none

      double precision f3
      double precision x
      double precision y

      f3 = x * ( 1.0D+00 - x ) * y * ( 1.0D+00 - y )

      return
      end

