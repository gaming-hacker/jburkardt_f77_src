      subroutine basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v )

c*********************************************************************72
c
cc BASIS_SERENE evaluates the serendipity basis functions.
c
c  Discussion:
c
c    This procedure assumes that a serendipity element has been defined,
c    whose sides are parallel to coordinate axes.
c
c    The local element numbering is
c
c      YN  3--2--1
c       |  |     |
c       |  4     8
c       |  |     |
c      YS  5--6--7
c       |
c       +--XW---XE--
c
c    We note that each basis function can be written as the product of
c    three linear terms, which never result in an x^2y^2 term.
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
c    Input, double precision XQ, YQ, the evaluation point.
c
c    Input, double precision XW, YS, the coordinates of the lower left corner.
c
c    Input, double precision XE, YN, the coordinates of the upper right corner.
c
c    Input, double precision XX(8), YY(8), the coordinates of the 8 nodes.
c
c    Output, double precision V(8), the value of the basis functions 
c    at (XQ,YQ).
c
      implicit none

      double precision not1
      external  not1
      double precision not2
      external not2
      double precision v(8)
      double precision xe
      double precision xq
      double precision xw
      double precision xx(8)
      double precision yn
      double precision yq
      double precision ys
      double precision yy(8)

      v(1) = not1 ( xq, xw, xx(1) ) 
     &     * not1 ( yq, ys, yy(1) ) 
     &     * not2 ( xq, yq, xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) )

      v(2) = not1 ( xq, xw, xx(2) ) 
     &     * not1 ( xq, xe, xx(2) ) 
     &     * not1 ( yq, ys, yy(2) )

      v(3) = not1 ( xq, xe, xx(3) ) 
     &     * not1 ( yq, ys, yy(3) ) 
     &     * not2 ( xq, yq, xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) )

      v(4) = not1 ( xq, xe, xx(4) ) 
     &     * not1 ( yq, yn, yy(4) ) 
     &     * not1 ( yq, ys, yy(4) )

      v(5) = not1 ( xq, xe, xx(5) ) 
     &     * not1 ( yq, yn, yy(5) ) 
     &     * not2 ( xq, yq, xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) )

      v(6) = not1 ( xq, xe, xx(6) ) 
     &     * not1 ( xq, xw, xx(6) ) 
     &     * not1 ( yq, yn, yy(6) )

      v(7) = not1 ( xq, xw, xx(7) ) 
     &     * not1 ( yq, yn, yy(7) ) 
     &     * not2 ( xq, yq, xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) )

      v(8) = not1 ( yq, ys, yy(8) ) 
     &     * not1 ( yq, yn, yy(8) ) 
     &     * not1 ( xq, xw, xx(8) )

      return
      end
      subroutine basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, vx, 
     &  vy )

c*********************************************************************72
c
cc BASISD_SERENE differentiates the serendipity basis functions.
c
c  Discussion:
c
c    This procedure assumes that a serendipity element has been defined,
c    whose sides are parallel to coordinate axes.
c
c    The local element numbering is
c
c      YN  3--2--1
c       |  |     |
c       |  4     8
c       |  |     |
c      YS  5--6--7
c       |
c       +--XW---XE--
c
c    We note that each basis function can be written as the product of
c    three linear terms, which never result in an x^2y^2 term.
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
c    Input, double precision XQ, YQ, the evaluation point.
c
c    Input, double precision XW, YS, the coordinates of the lower left corner.
c
c    Input, double precision XE, YN, the coordinates of the upper right corner.
c
c    Input, double precision XX(8), YY(8), the coordinates of the 8 nodes.
c
c    Output, double precision VX(8), VY(8), the derivatives of the basis 
c    functions at (XQ,YQ) with respect to X and Y.
c
      implicit none

      double precision not1
      external not1
      double precision not1d
      external not1d
      double precision not2
      external not2
      double precision not2dx
      external not2dx
      double precision not2dy
      external not2dy
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

      vx(1) = 
     &    not1d ( xw, xx(1) ) 
     &  * not1 ( yq, ys, yy(1) ) 
     &  * not2 ( xq, yq, xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) ) 
     &  + not1 ( xq, xw, xx(1) ) 
     &  * not1 ( yq, ys, yy(1) ) 
     &  * not2dx ( xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) )

      vy(1) = 
     &    not1 ( xq, xw, xx(1) ) 
     &  * not1d ( ys, yy(1) ) 
     &  * not2 ( xq, yq, xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) ) 
     &  + not1 ( xq, xw, xx(1) ) 
     &  * not1 ( yq, ys, yy(1) ) 
     &  * not2dy ( xx(8), yy(8), xx(2), yy(2), xx(1), yy(1) )

      vx(2) = 
     &    not1d ( xw, xx(2) ) 
     &  * not1 ( xq, xe, xx(2) ) 
     &  * not1 ( yq, ys, yy(2) ) 
     &  + not1 ( xq, xw, xx(2) ) 
     &  * not1d ( xe, xx(2) ) 
     &  * not1 ( yq, ys, yy(2) )

      vy(2) = 
     &    not1 ( xq, xw, xx(2) ) 
     &  * not1 ( xq, xe, xx(2) ) 
     &  * not1d ( ys, yy(2) )

      vx(3) = 
     &    not1d ( xe, xx(3) ) 
     &  * not1 ( yq, ys, yy(3) ) 
     &  * not2 ( xq, yq, xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) ) 
     &  + not1 ( xq, xe, xx(3) ) 
     &  * not1 ( yq, ys, yy(3) ) 
     &  * not2dx ( xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) )

      vy(3) = not1 ( xq, xe, xx(3) ) 
     &  * not1d ( ys, yy(3) ) 
     &  * not2 ( xq, yq, xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) ) 
     &  + not1 ( xq, xe, xx(3) ) 
     &  * not1 ( yq, ys, yy(3) ) 
     &  * not2dy ( xx(2), yy(2), xx(4), yy(4), xx(3), yy(3) )

      vx(4) = 
     &    not1d ( xe, xx(4) ) 
     &  * not1 ( yq, yn, yy(4) ) 
     &  * not1 ( yq, ys, yy(4) )

      vy(4) = 
     &    not1 ( xq, xe, xx(4) ) 
     &  * not1d ( yn, yy(4) ) 
     &  * not1 ( yq, ys, yy(4) ) 
     &  + not1 ( xq, xe, xx(4) ) 
     &  * not1 ( yq, yn, yy(4) ) 
     &  * not1d ( ys, yy(4) )

      vx(5) = 
     &    not1d ( xe, xx(5) ) 
     &  * not1 ( yq, yn, yy(5) ) 
     &  * not2 ( xq, yq, xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) ) 
     &  + not1 ( xq, xe, xx(5) ) 
     &  * not1 ( yq, yn, yy(5) ) 
     &  * not2dx ( xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) )

      vy(5) = 
     &    not1 ( xq, xe, xx(5) ) 
     &  * not1d ( yn, yy(5) ) 
     &  * not2 ( xq, yq, xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) ) 
     &  + not1 ( xq, xe, xx(5) ) 
     &  * not1 ( yq, yn, yy(5) ) 
     &  * not2dy ( xx(4), yy(4), xx(6), yy(6), xx(5), yy(5) )

      vx(6) = 
     &    not1d ( xe, xx(6) ) 
     &  * not1 ( xq, xw, xx(6) ) 
     &  * not1 ( yq, yn, yy(6) ) 
     &  + not1 ( xq, xe, xx(6) ) 
     &  * not1d ( xw, xx(6) ) 
     &  * not1 ( yq, yn, yy(6) )

      vy(6) = 
     &    not1 ( xq, xe, xx(6) ) 
     &  * not1 ( xq, xw, xx(6) ) 
     &  * not1d ( yn, yy(6) )

      vx(7) = 
     &    not1d ( xw, xx(7) ) 
     &  * not1 ( yq, yn, yy(7) ) 
     &  * not2 ( xq, yq, xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) ) 
     &  + not1 ( xq, xw, xx(7) ) 
     &  * not1 ( yq, yn, yy(7) ) 
     &  * not2dx ( xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) )

      vy(7) = 
     &    not1 ( xq, xw, xx(7) ) 
     &  * not1d ( yn, yy(7) ) 
     &  * not2 ( xq, yq, xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) ) 
     &  + not1 ( xq, xw, xx(7) ) 
     &  * not1 ( yq, yn, yy(7) ) 
     &  * not2dy ( xx(6), yy(6), xx(8), yy(8), xx(7), yy(7) )

      vx(8) = 
     &    not1 ( yq, ys, yy(8) ) 
     &  * not1 ( yq, yn, yy(8) ) 
     &  * not1d ( xw, xx(8) )

      vy(8) = 
     &    not1d ( ys, yy(8) ) 
     &  * not1 ( yq, yn, yy(8) ) 
     &  * not1 ( xq, xw, xx(8) ) 
     &  + not1 ( yq, ys, yy(8) ) 
     &  * not1d ( yn, yy(8) ) 
     &  * not1 ( xq, xw, xx(8) )

      return
      end
      subroutine fem2d_bvp_serene ( nx, ny, a, c, f, x, y, show11, u )

c*********************************************************************72
c
cc FEM2D_BVP_SERENE solves boundary value problem on a rectangle.
c
c  Discussion:
c
c    The program uses the finite element method, with piecewise 
c    serendipity basis functions to solve a 2D boundary value problem 
c    over a rectangle.
c
c    The following differential equation is imposed inside the region:
c
c      - d/dx a(x,y) du/dx - d/dy a(x,y) du/dy + c(x,y) * u(x,y) = f(x,y)
c
c    where a(x,y), c(x,y), and f(x,y) are given functions.
c
c    On the boundary, the solution is constrained to have the value 0.
c
c    The finite element method will use a regular grid of NX nodes in X, and 
c    NY nodes in Y.  Both NX and NY must be odd.
c
c    The local element numbering is
c
c      3--2--1
c      |     |
c      4     8
c      |     |
c      5--6--7
c
c    The serendipity element mass matrix is a multiple of:
c
c       6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0
c      -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0
c       2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0
c      -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0
c       3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0
c      -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0
c       2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0
c      -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0
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
c    Input, integer NX, NY, the number of X and Y grid values.
c    NX and NY must be odd and at least 3.
c
c    Input, function A(X,Y), evaluates a(x,y)
c
c    Input, function C(X,Y), evaluates c(x,y)
c
c    Input, function F(X,Y), evaluates f(x,y)
c
c    Input, double precision X(NX), Y(NY), the mesh points.
c
c    Input, integer SHOW11, is 1 to print out the element matrix
c    for the element in row 1, column 1.
c
c    Output, double precision U(MN), the finite element coefficients, which 
c    are also the value of the computed solution at the mesh points.
c
      implicit none

      integer nx
      integer ny
      integer quad_num
      parameter ( quad_num = 3 )

      double precision a
      external a
      double precision abscissa(quad_num)
      double precision ae(8,8)
      double precision amat(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2,
     &  nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
      double precision aq
      double precision b(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
      double precision be(8)
      double precision c
      external c
      integer cc
      double precision cq
      integer e
      integer ex
      integer ex_num
      integer ey
      integer ey_num
      double precision f
      external f
      integer fem2d_bvp_serene_node_num
      double precision fq
      integer i
      integer ierror
      integer ihi
      integer ii
      integer inc
      integer j
      integer jj
      integer k
      integer mm
      integer mn
      integer n
      integer node(8)
      integer qx
      integer qy
      integer s
      double precision scale
      logical show11
      double precision u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
      double precision v(8)
      double precision vx(8)
      double precision vy(8)
      integer w
      double precision weight(quad_num)
      double precision wq
      double precision x(nx)
      double precision xc
      double precision xe
      double precision xq
      double precision xw
      double precision xx(8)
      double precision y(ny)
      double precision ym
      double precision yn
      double precision yq
      double precision ys
      double precision yy(8)

      save abscissa
      save weight

      data abscissa /
     &  -0.774596669241483377035853079956D+00,
     &   0.000000000000000000000000000000D+00,
     &   0.774596669241483377035853079956D+00 /
      data weight /
     &  0.555555555555555555555555555556D+00,
     &  0.888888888888888888888888888889D+00,
     &  0.555555555555555555555555555556D+00 /
c
c  Make room for the matrix A and right hand side b.
c
      mn = fem2d_bvp_serene_node_num ( nx, ny )

      do j = 1, mn
        do i = 1, mn
          amat(i,j) = 0.0D+00
        end do
      end do

      do i = 1, mn
        b(i) = 0.0D+00
      end do
c
c  Compute the matrix entries by integrating over each element.
c
      ex_num = ( nx - 1 ) / 2
      ey_num = ( ny - 1 ) / 2

      do ey = 1, ey_num

        s = 2 * ey - 1
        mm = 2 * ey
        n = 2 * ey + 1

        ys = y(s)
        ym = y(mm)
        yn = y(n)

        yy(1) = y(n)
        yy(2) = y(n)
        yy(3) = y(n)
        yy(4) = y(mm)
        yy(5) = y(s)
        yy(6) = y(s)
        yy(7) = y(s)
        yy(8) = y(mm)

        do ex = 1, ex_num

          w = 2 * ex - 1
          cc = 2 * ex
          e = 2 * ex + 1

          xe = x(e)
          xc = x(cc)
          xw = x(w)

          xx(1) = x(e)
          xx(2) = x(cc)
          xx(3) = x(w)
          xx(4) = x(w)
          xx(5) = x(w)
          xx(6) = x(cc)
          xx(7) = x(e)
          xx(8) = x(e)
c
c  Node indices
c
c  3  2  1  wn  cn  en
c  4     8  wm      em
c  5  6  7  ws  cs  es
c
          node(1) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex + 1
          node(2) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex
          node(3) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex - 1
          node(4) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex - 1
          node(5) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 3
          node(6) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 2
          node(7) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 1
          node(8) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex

          if ( show11 ) then
            if ( ey .eq. 1 .and. ex .eq. 1 ) then
              ae(1:8,1:8) = 0.0D+00
              be(1:8) = 0.0D+00
            end if
          end if

          do qx = 1, quad_num

            xq = ( ( 1.0D+00 - abscissa(qx) ) * x(e)   
     &           + ( 1.0D+00 + abscissa(qx) ) * x(w) ) 
     &             / 2.0D+00

            do qy = 1, quad_num

              yq = ( ( 1.0D+00 - abscissa(qy) ) * y(n)
     &             + ( 1.0D+00 + abscissa(qy) ) * y(s) )
     &             /   2.0D+00

              wq = weight(qx) * ( x(e) - x(w) ) / 2.0D+00 
     &           * weight(qy) * ( y(n) - y(s) ) / 2.0D+00

              call basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v )
              call basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, vx, 
     &          vy )

              aq = a ( xq, yq )
              cq = c ( xq, yq )
              fq = f ( xq, yq )
c
c  Build the element matrix.
c
              if ( show11 ) then
                if ( ey .eq. 1 .and. ex .eq. 1 ) then
                  do i = 1, 8
                    do j = 1, 8
                      ae(i,j) = ae(i,j) + wq * ( vx(i) * aq * vx(j) 
     &                                         + vy(i) * aq * vy(j) 
     &                                         + v(i)  * cq * v(j) )
                    end do
                    be(i) = be(i) + wq * ( v(i) * fq )
                  end do  
                end if
              end if

              do i = 1, 8
                ii = node(i)
                do j = 1, 8
                  jj = node(j)
                  amat(ii,jj) = amat(ii,jj) + wq * ( vx(i) * aq * vx(j) 
     &                                             + vy(i) * aq * vy(j) 
     &                                             + v(i)  * cq * v(j) )
                end do
                b(ii) = b(ii) + wq * ( v(i) * fq )
              end do

            end do
          end do
c
c  Print a sample element matrix.
c
          if ( show11 ) then
            if ( ey .eq. 1 .and. ex .eq. 1 ) then
              scale = 0.5D+00 * ae(1,3)
              ae(1:8,1:8) = ae(1:8,1:8) / scale
              call r8mat_print ( 8, 8, ae, 
     &          '  Wathen elementary mass matrix:' )
            end if
          end if

        end do
      end do
c
c  Where a node is on the boundary, 
c  replace the finite element equation by a boundary condition.
c
      k = 0

      do j = 1, ny

        if ( mod ( j, 2 ) .eq. 1 ) then
          inc = 1
        else
          inc = 2
        end if

        do i = 1, nx, inc
          k = k + 1
          if ( i .eq. 1 .or. i .eq. nx .or. 
     &         j .eq. 1 .or. j .eq. ny ) then
            amat(k,1:mn) = 0.0D+00
            amat(1:mn,k) = 0.0D+00
            amat(k,k) = 1.0D+00
            b(k) = 0.0D+00
          end if
        end do

      end do
c
c  Solve the linear system.
c
      call r8mat_solve2 ( mn, amat, b, u, ierror )

      return
      end
      function fem2d_bvp_serene_node_num ( nx, ny )

c*********************************************************************72
c
cc FEM2D_BVP_SERENE_NODE_NUM counts the number of nodes.
c
c  Discussion:
c
c    The program uses the finite element method, with piecewise serendipity 
c    basis functions to solve a 2D boundary value problem over a rectangle.
c
c    The grid uses NX nodes in the X direction and NY nodes in the Y direction.
c
c    Both NX and NY must be odd.
c
c    Because of the peculiar shape of the serendipity elements, counting the
c    number of nodes and variables is a little tricky.  Here is a grid for
c    the case when NX = 7 and NY = 5, for which there are 29 nodes 
c    and variables.
c
c     23 24 25 26 27 28 29
c     19    20    21    22
c     12 13 14 15 16 17 18
c      8     9    10    11
c      1  2  3  4  5  6  7
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
c    Input, integer NX, NY, the number of X and Y grid values.
c    NX and NY must be odd and at least 3.
c
c    Output, integer NODE_NUM, the number of nodes and variables.
c
      implicit none

      integer fem2d_bvp_serene_node_num
      integer nx
      integer ny

      fem2d_bvp_serene_node_num = 
     &      nx           * ( ny + 1 ) / 2 
     &  + ( nx + 1 ) / 2 * ( ny - 1 ) / 2 

      return
      end
      subroutine fem2d_h1s_error_serene ( nx, ny, x, y, u, exact_ux, 
     &  exact_uy, h1s )

c*********************************************************************72
c
cc FEM2D_H1S_ERROR_SERENE: seminorm error of a finite element solution.
c
c  Discussion:
c
c    We assume the finite element method has been used, over a product region
c    involving a grid of NX*NY nodes, with serendipity elements used 
c    for the basis.
c
c    The finite element solution U(x,y) has been computed, and formulas for the
c    exact derivatives Vx(x,y) and Vy(x,y) are known.
c
c    This function estimates the H1 seminorm of the error:
c
c      H1S = sqrt ( integral ( x, y )   ( Ux(x,y) - Vx(x,y) )^2 
c                                     + ( Uy(x,y) - Vy(x,y) )^2 dx dy )
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
c    Input, integer NX, NY, the number of nodes.
c
c    Input, double precision X(NX), Y(NY), the grid coordinates.
c
c    Input, double precision U(*), the finite element coefficients.
c
c    Input, function EQX = EXACT_UX(X,Y), function EQY = EXACT_UY(X,Y)
c    returns the exact derivatives with respect to X and Y.
c
c    Output, double precision H1S, the estimated seminorm of the error.
c
      implicit none

      integer nx
      integer ny
      integer quad_num
      parameter ( quad_num = 3 )

      double precision abscissa(quad_num)
      integer cc
      integer e
      integer ex
      integer ex_num
      integer ey
      integer ey_num
      double precision exact_ux
      external exact_ux
      double precision exact_uy
      external exact_uy
      double precision exq
      double precision eyq
      double precision h1s
      integer k
      integer mm
      integer n
      integer node(8)
      integer qx
      integer qy
      integer s
      double precision u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
      double precision uxq
      double precision uyq
      double precision vx(8)
      double precision vy(8)
      integer w
      double precision weight(quad_num)
      double precision wq
      double precision x(nx)
      double precision xc
      double precision xe
      double precision xq
      double precision xw
      double precision xx(8)
      double precision y(ny)
      double precision ym
      double precision yn
      double precision yq
      double precision ys
      double precision yy(8)

      save abscissa
      save weight

      data abscissa /
     &  -0.774596669241483377035853079956D+00,
     &   0.000000000000000000000000000000D+00,
     &   0.774596669241483377035853079956D+00 /
      data weight /
     &  0.555555555555555555555555555556D+00,
     &  0.888888888888888888888888888889D+00,
     &  0.555555555555555555555555555556D+00 /

      h1s = 0.0D+00
c
c  Quadrature definitions.
c
      ex_num = ( nx - 1 ) / 2
      ey_num = ( ny - 1 ) / 2

      do ey = 1, ey_num

        s = 2 * ey - 1
        mm = 2 * ey
        n = 2 * ey + 1

        ys = y(s)
        ym = y(mm)
        yn = y(n)

        yy(1) = y(n)
        yy(2) = y(n)
        yy(3) = y(n)
        yy(4) = y(mm)
        yy(5) = y(s)
        yy(6) = y(s)
        yy(7) = y(s)
        yy(8) = y(mm)

        do ex = 1, ex_num

          w = 2 * ex - 1
          cc = 2 * ex
          e = 2 * ex + 1

          xe = x(e)
          xc = x(cc)
          xw = x(w)

          xx(1) = x(e)
          xx(2) = x(cc)
          xx(3) = x(w)
          xx(4) = x(w)
          xx(5) = x(w)
          xx(6) = x(cc)
          xx(7) = x(e)
          xx(8) = x(e)
c
c  Node indices
c
c  3  2  1  wn  cn  en
c  4     8  wm      em
c  5  6  7  ws  cs  es
c
          node(1) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex + 1
          node(2) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex
          node(3) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex - 1
          node(4) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex - 1
          node(5) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 3
          node(6) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 2
          node(7) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 1
          node(8) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex

          do qx = 1, quad_num

            xq = ( ( 1.0D+00 - abscissa(qx) ) * x(e)   
     &           + ( 1.0D+00 + abscissa(qx) ) * x(w) ) 
     &             / 2.0D+00

            do qy = 1, quad_num

              yq = ( ( 1.0D+00 - abscissa(qy) ) * y(n)   
     &             + ( 1.0D+00 + abscissa(qy) ) * y(s) ) 
     &               / 2.0D+00

              wq = weight(qx) * ( x(e) - x(w) ) / 2.0D+00 
     &           * weight(qy) * ( y(n) - y(s) ) / 2.0D+00

              call basisd_serene ( xq, yq, xw, ys, xe, yn, xx, yy, 
     &          vx, vy )

              uxq = 0.0D+00
              uyq = 0.0D+00
              do k = 1, 8
                uxq = uxq + u(node(k)) * vx(k)
                uyq = uyq + u(node(k)) * vy(k)
              end do

              exq = exact_ux ( xq, yq )
              eyq = exact_uy ( xq, yq )

              h1s = h1s + wq * ( ( uxq - exq )**2 + ( uyq - eyq )**2 )
     
            end do
          end do
        end do
      end do

      h1s = sqrt ( h1s )

      return
      end
      subroutine fem2d_l1_error_serene ( nx, ny, x, y, u, exact, e1 )

c*********************************************************************72
c
cc FEM2D_L1_ERROR_SERENE: l1 error norm of a finite element solution.
c
c  Discussion:
c
c    We assume the finite element method has been used, over a product
c    region with NX*NY nodes and the serendipity element.
c
c    The coefficients U(1:NX,1:NY) have been computed, and a formula for the
c    exact solution is known.
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
c    Input, integer NX, NY, the number of nodes in the X and 
c    Y directions.
c
c    Input, double precision X(NX), Y(NY), the X and Y grid values.
c
c    Input, double precision U(*), the finite element coefficients.
c
c    Input, function EQ = EXACT(X,Y), returns the value of the exact
c    solution at the point (X,Y).
c
c    Output, double precision E1, the little l1 norm of the error.
c
      implicit none

      integer nx
      integer ny

      double precision e1
      double precision exact
      external exact
      integer i
      integer inc
      integer j
      integer k
      double precision u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
      double precision x(nx)
      double precision y(ny)

      e1 = 0.0D+00
      k = 0
      do j = 1, ny
        if ( mod ( j, 2 ) .eq. 1 ) then
          inc = 1
        else
          inc = 2
        end if
        do i = 1, nx, inc
          k = k + 1
          e1 = e1 + abs ( u(k) - exact ( x(i), y(j) ) )
        end do
      end do
c
c  Average the error.
c
      e1 = e1 / dble ( k )

      return
      end
      subroutine fem2d_l2_error_serene ( nx, ny, x, y, u, exact, e2 )

c*********************************************************************72
c
cc FEM2D_L2_ERROR_SERENE: L2 error norm of a finite element solution.
c
c  Discussion:
c
c    The finite element method has been used, over a rectangle,
c    involving a grid of NXxNY nodes, with serendipity elements used 
c    for the basis.
c
c    The finite element coefficients have been computed, and a formula for the
c    exact solution is known.
c
c    This function estimates E2, the L2 norm of the error:
c
c      E2 = Integral ( X, Y ) ( U(X,Y) - EXACT(X,Y) )^2 dX dY
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
c    Input, integer NX, NY, the number of nodes in the X 
c    and Y directions.
c
c    Input, double precision X(NX), Y(NY), the grid coordinates.
c
c    Input, double precision U(*), the finite element coefficients.
c
c    Input, function EQ = EXACT(X,Y), returns the value of the exact
c    solution at the point (X,Y).
c
c    Output, double precision E2, the estimated L2 norm of the error.
c
      implicit none

      integer nx
      integer ny
      integer quad_num
      parameter ( quad_num = 3 )

      double precision abscissa(quad_num)
      integer cc
      integer e
      double precision e2
      double precision eq
      integer ex
      integer ex_num
      double precision exact
      external exact
      integer ey
      integer ey_num
      integer k
      integer mm
      integer n
      integer node(8)
      integer qx
      integer qy
      integer s
      double precision u(nx*(ny+1)/2+(nx+1)/2*(ny-1)/2)
      double precision uq
      double precision v(8)
      integer w
      double precision weight(quad_num)
      double precision wq
      double precision x(nx)
      double precision xc
      double precision xe
      double precision xq
      double precision xw
      double precision xx(8)
      double precision y(ny)
      double precision ym
      double precision yn
      double precision yq
      double precision ys
      double precision yy(8)

      save abscissa
      save weight

      data abscissa /
     &  -0.774596669241483377035853079956D+00,
     &   0.000000000000000000000000000000D+00,
     &   0.774596669241483377035853079956D+00 /
      data weight /
     &  0.555555555555555555555555555556D+00,
     &  0.888888888888888888888888888889D+00,
     &  0.555555555555555555555555555556D+00 /

      e2 = 0.0D+00
c
c  Compute the matrix entries by integrating over each element.
c
      ex_num = ( nx - 1 ) / 2
      ey_num = ( ny - 1 ) / 2

      do ey = 1, ey_num

        s = 2 * ey - 1
        mm = 2 * ey
        n = 2 * ey + 1

        ys = y(s)
        ym = y(mm)
        yn = y(n)

        yy(1) = y(n)
        yy(2) = y(n)
        yy(3) = y(n)
        yy(4) = y(mm)
        yy(5) = y(s)
        yy(6) = y(s)
        yy(7) = y(s)
        yy(8) = y(mm)

        do ex = 1, ex_num

          w = 2 * ex - 1
          cc = 2 * ex
          e = 2 * ex + 1

          xe = x(e)
          xc = x(cc)
          xw = x(w)

          xx(1) = x(e)
          xx(2) = x(cc)
          xx(3) = x(w)
          xx(4) = x(w)
          xx(5) = x(w)
          xx(6) = x(cc)
          xx(7) = x(e)
          xx(8) = x(e)
c
c  Node indices
c
c  3  2  1  wn  cn  en
c  4     8  wm      em
c  5  6  7  ws  cs  es
c
          node(1) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex + 1
          node(2) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex
          node(3) = ( 3 * ey     ) * ey_num + 2 * ey + 2 * ex - 1
          node(4) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex - 1
          node(5) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 3
          node(6) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 2
          node(7) = ( 3 * ey - 3 ) * ey_num + 2 * ey + 2 * ex - 1
          node(8) = ( 3 * ey - 1 ) * ey_num + 2 * ey +     ex

          do qx = 1, quad_num

            xq = ( ( 1.0D+00 - abscissa(qx) ) * x(e)   
     &           + ( 1.0D+00 + abscissa(qx) ) * x(w) ) 
     &             / 2.0D+00

            do qy = 1, quad_num

              yq = ( ( 1.0D+00 - abscissa(qy) ) * y(n)   
     &             + ( 1.0D+00 + abscissa(qy) ) * y(s) ) 
     &               / 2.0D+00

              wq = weight(qx) * ( x(e) - x(w) ) / 2.0D+00 
     &           * weight(qy) * ( y(n) - y(s) ) / 2.0D+00

              call basis_serene ( xq, yq, xw, ys, xe, yn, xx, yy, v )

              uq = 0.0D+00
              do k = 1, 8
                uq = uq + u(node(k)) * v(k)
              end do

              eq = exact ( xq, yq )
              e2 = e2 + wq * ( uq - eq )**2
     
            end do
          end  do
        end do
      end do

      e2 = sqrt ( e2 )

      return
      end
      function not1 ( x1, x2, x3 )

c*********************************************************************72
c
cc NOT1 evaluates a factor for serendipity basis functions.
c
c  Discussion:
c
c    not1(x1,x2,x3) evaluates at the point x1, the basis factor that
c    is 0 at x2 and 1 at x3:
c
c      not1(x1,x2,x3) = ( x1 - x2 ) / ( x3 - x2 )  
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
c    Input, double precision X1, the evaluation point.
c
c    Input, double precision X2, X3, values that define the factor.
c
c    Output, double precision NOT1, the value of the basis function factor.
c
      implicit none

      double precision not1
      double precision x1
      double precision x2
      double precision x3

      not1 = ( x1 - x2 ) / ( x3 - x2 )

      return
      end
      function not1d ( x2, x3 )

c*********************************************************************72
c
cc NOT1D differentiates a factor for serendipity basis functions.
c
c  Discussion:
c
c    not1(x1,x2,x3) evaluates at the point x1, the basis factor that
c    is 0 at x2 and 1 at x3:
c
c      not1(x1,x2,x3) = ( x1 - x2 ) / ( x3 - x2 )  
c
c    This function returns the derivative of the factor with respect to x1:
c
c      not1d(x1,x2,x3) = 1 / ( x3 - x2 )
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
c    Input, double precision X2, X3, values that define the factor.
c
c    Output, double precision NOT1D, the derivative of the basis function 
c    factor.
c
      implicit none

      double precision not1d
      double precision x2
      double precision x3

      not1d = 1.0D+00 / ( x3 - x2 )

      return
      end
      function not2 ( x1, y1, x2, y2, x3, y3, x4, y4 )

c*********************************************************************72
c
cc NOT2 evaluates a factor for serendipity basis functions.
c
c  Discussion:
c
c    not2(x1,y1,x2,y2,x3,y3,x4,y4) evaluates at the point (x1,y1), the basis 
c    factor that is 0 at (x2,y2) and (x3,y3) and 1 at (x4,y4):
c
c          ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) )
c        / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )
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
c    Input, double precision X1, Y2, the evaluation point.
c
c    Input, double precision X2, Y2, X3,Y3, values that define the factor.
c
c    Output, double precision NOT2, the value of the basis function factor.
c
      implicit none

      double precision not2
      double precision x1
      double precision x2
      double precision x3
      double precision x4
      double precision y1
      double precision y2
      double precision y3
      double precision y4

      not2 = ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) ) 
     &     / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )

      return
      end
      function not2dx ( x2, y2, x3, y3, x4, y4 )

c*********************************************************************72
c
cc NOT2DX evaluates a factor for serendipity basis functions.
c
c  Discussion:
c
c    not2(x1,y1,x2,y2,x3,y3,x4,y4) evaluates at the point (x1,y1), the basis 
c    factor that is 0 at (x2,y2) and (x3,y3) and 1 at (x4,y4):
c
c          ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) )
c        / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )
c
c    not2dx returns the derivative of this function with respect to X1.
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
c    Input, double precision X2, Y2, X3,Y3, values that define the factor.
c
c    Output, double precision NOT2DX, the derivative of the basis function 
c    factor with respect to X1.
c
      implicit none

      double precision not2dx
      double precision x2
      double precision x3
      double precision x4
      double precision y2
      double precision y3
      double precision y4

      not2dx = (   1.0D+00   * ( y3 - y2 ) +                 0.0D+00   ) 
     &       / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )

      return
      end
      function not2dy ( x2, y2, x3, y3, x4, y4 )

c*********************************************************************72
c
cc NOT2DY evaluates a factor for serendipity basis functions.
c
c  Discussion:
c
c    not2(x1,y1,x2,y2,x3,y3,x4,y4) evaluates at the point (x1,y1), the basis 
c    factor that is 0 at (x2,y2) and (x3,y3) and 1 at (x4,y4):
c
c          ( ( x1 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y1 - y2 ) )
c        / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )
c
c    not2dy returns the derivatives of this function with respect to Y1.
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
c    Input, double precision X2, Y2, X3,Y3, values that define the factor.
c
c    Output, double precision NOT2DY, the derivative of the basis function 
c    factor with respect to Y1.
c
      implicit none

      double precision not2dy
      double precision x2
      double precision x3
      double precision x4
      double precision y2
      double precision y3
      double precision y4

      not2dy = (   0.0D+00                 - ( x3 - x2 ) *   1.0D+00   ) 
     &       / ( ( x4 - x2 ) * ( y3 - y2 ) - ( x3 - x2 ) * ( y4 - y2 ) )

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine r8mat_solve2 ( n, a, b, x, ierror )

c*********************************************************************72
c
cc R8MAT_SOLVE2 computes the solution of an N by N linear system.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    The linear system may be represented as
c
c      A*X = B
c
c    If the linear system is singular, but consistent, then the routine will
c    still produce a solution.
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
c    Input, integer N, the number of equations.
c
c    Input/output, double precision A(N,N).
c    On input, A is the coefficient matrix to be inverted.
c    On output, A has been overwritten.
c
c    Input/output, double precision B(N).
c    On input, B is the right hand side of the system.
c    On output, B has been overwritten.
c
c    Output, double precision X(N), the solution of the linear system.
c
c    Output, integer IERROR.
c    0, no error detected.
c    1, consistent singularity.
c    2, inconsistent singularity.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision amax
      double precision b(n)
      integer i
      integer ierror
      integer imax
      integer ipiv(n)
      integer j
      integer k
      double precision x(n)

      ierror = 0

      do i = 1, n
        ipiv(i) = 0
      end do

      do i = 1, n
        x(i) = 0.0D+00
      end do
c
c  Process the matrix.
c
      do k = 1, n
c
c  In column K:
c    Seek the row IMAX with the properties that:
c      IMAX has not already been used as a pivot;
c      A(IMAX,K) is larger in magnitude than any other candidate.
c
        amax = 0.0D+00
        imax = 0
        do i = 1, n
          if ( ipiv(i) .eq. 0 ) then
            if ( amax .lt. abs ( a(i,k) ) ) then
              imax = i
              amax = abs ( a(i,k) )
            end if
          end if
        end do
c
c  If you found a pivot row IMAX, then,
c    eliminate the K-th entry in all rows that have not been used for pivoting.
c
        if ( imax .ne. 0 ) then

          ipiv(imax) = k
          do j = k + 1, n
            a(imax,j) = a(imax,j) / a(imax,k)
          end do
          b(imax) = b(imax) / a(imax,k)
          a(imax,k) = 1.0D+00

          do i = 1, n

            if ( ipiv(i) .eq. 0 ) then
              do j = k + 1, n
                a(i,j) = a(i,j) - a(i,k) * a(imax,j)
              end do
              b(i) = b(i) - a(i,k) * b(imax)
              a(i,k) = 0.0D+00
            end if

          end do

        end if

      end do
c
c  Now, every row with nonzero IPIV begins with a 1, and
c  all other rows are all zero.  Begin solution.
c
      do j = n, 1, -1

        imax = 0
        do k = 1, n
          if ( ipiv(k) .eq. j ) then
            imax = k
          end if
        end do

        if ( imax .eq. 0 ) then

          x(j) = 0.0D+00

          if ( b(j) .eq. 0.0D+00 ) then
            ierror = 1
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'R8MAT_SOLVE2 - Warning:'
            write ( *, '(a,i8)' )
     &        '  Consistent singularity, equation = ', j
          else
            ierror = 2
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'R8MAT_SOLVE2 - Error:'
            write ( *, '(a,i8)' )
     &        '  Inconsistent singularity, equation = ', j
          end if

        else

          x(j) = b(imax)

          do i = 1, n
            if ( i .ne. imax ) then
              b(i) = b(i) - a(i,j) * x(j)
            end if
          end do

        end if

      end do

      return
      end
      subroutine r8vec_linspace ( n, a, b, x )

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
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

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
      subroutine wathen ( nx, ny, n, a )

c*********************************************************************72
c
cc WATHEN returns the WATHEN matrix.
c
c  Discussion:
c
c    The Wathen matrix is a finite element matrix which is sparse.
c
c    The entries of the matrix depend in part on a physical quantity
c    related to density.  That density is here assigned random values between
c    0 and 100.
c
c    The matrix order N is determined by the input quantities NX and NY,
c    which would usually be the number of elements in the X and Y directions.
c    The value of N is
c
c      N = 3*NX*NY + 2*NX + 2*NY + 1,
c
c    and sufficient storage in A must have been set aside to hold
c    the matrix.
c
c    A is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.  
c
c    The local element numbering is
c
c      3--2--1
c      |     |
c      4     8
c      |     |
c      5--6--7
c
c    Here is an illustration for NX = 3, NY = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c  Properties:
c
c    A is symmetric positive definite for any positive values of the
c    density RHO(NX,NY), which is here given the value 1.
c
c    The problem could be reprogrammed so that RHO is nonconstant,
c    but positive.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, Number 4, October 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision em(8,8)

      integer i
      integer j
      integer kcol
      integer krow
      integer nx
      integer ny
      integer node(8)
      double precision r8_uniform_01
      double precision rho
      integer seed

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0, 
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, 
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0, 
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, 
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0, 
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0,
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0,
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      seed = 123456789

      do j = 1, ny

        do i = 1, nx
c
c  For the element (I,J), determine the indices of the 8 nodes.
c
          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2

          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(8) = node(4) + 1

          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
c
c  The density RHO can also be set to a random positive value.
c
          rho = 100.0D+00 * r8_uniform_01 ( seed ) 

          do krow = 1, 8
            do kcol = 1, 8

              a(node(krow),node(kcol)) = a(node(krow),node(kcol)) 
     &          + rho * em(krow,kcol)

            end do
          end do

        end do
      end do

      return
      end
