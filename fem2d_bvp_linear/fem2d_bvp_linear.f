      subroutine fem2d_bvp_linear ( nx, ny, a, c, f, x, y, u )

c*********************************************************************72
c
cc FEM2D_BVP_LINEAR solves boundary value problem on a rectangle.
c
c  Discussion:
c
c    The program uses the finite element method, with piecewise linear basis
c    functions to solve a 2D boundary value problem over a rectangle
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
c    NY nodes in Y.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NX, NY, the number of X and Y grid values.
c
c    Input, function A ( X ), evaluates a(x);
c
c    Input, function C ( X ), evaluates c(x);
c
c    Input, function F ( X ), evaluates f(x);
c
c    Input, double precision X(NX), Y(NY), the mesh points.
c
c    Output, double precision U(NX,NY), the finite element coefficients, which
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
      double precision amat(nx*ny,nx*ny)
      double precision aq
      double precision b(nx*ny)
      double precision c
      external c
      double precision cq
      integer e
      integer ex
      integer ey
      double precision f
      external f
      double precision fq
      integer i
      integer ierror
      integer ii
      integer j
      integer jj
      integer k
      integer mn
      integer n
      integer ne
      integer nw
      integer qx
      integer qy
      integer s
      integer se
      integer sw
      integer w
      double precision weight(quad_num)
      double precision wq
      double precision u(nx,ny)
      double precision vne
      double precision vnex
      double precision vney
      double precision vnw
      double precision vnwx
      double precision vnwy
      double precision vse
      double precision vsex
      double precision vsey
      double precision vsw
      double precision vswx
      double precision vswy
      double precision x(nx)
      double precision xe
      double precision xq
      double precision xw
      double precision y(ny)
      double precision yn
      double precision yq
      double precision ys

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
c  Zero out the matrix and right hand side.
c
      mn = nx * ny

      do j = 1, mn
        do i = 1, mn
          amat(i,j) = 0.0D+00
        end do
      end do

      do i = 1, mn
        b(i) = 0.0D+00
      end do

      do ex = 1, nx - 1

        w = ex
        e = ex + 1

        xw = x(w)
        xe = x(e)

        do ey = 1, ny - 1

          s = ey
          n = ey + 1

          ys = y(s)
          yn = y(n)

          sw = ( ey - 1 ) * nx + ex
          se = ( ey - 1 ) * nx + ex + 1
          nw =   ey       * nx + ex
          ne =   ey       * nx + ex + 1

          do qx = 1, quad_num

            xq = ( ( 1.0D+00 - abscissa(qx) ) * xw
     &           + ( 1.0D+00 + abscissa(qx) ) * xe )
     &             / 2.0D+00

            do qy = 1, quad_num

              yq = ( ( 1.0D+00 - abscissa(qy) ) * ys
     &             + ( 1.0D+00 + abscissa(qy) ) * yn )
     &               / 2.0D+00

              wq = weight(qx) * ( xe - xw ) / 2.0D+00
     &           * weight(qy) * ( yn - ys ) / 2.0D+00

              aq = a ( xq, yq )
              cq = c ( xq, yq )
              fq = f ( xq, yq )

              vsw  = ( xe - xq ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vswx = (-1.0D+00 ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vswy = ( xe - xq ) / ( xe - xw ) 
     &             * (-1.0D+00 ) / ( yn - ys )

              vse  = ( xq - xw ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vsex = ( 1.0D+00 ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vsey = ( xq - xw ) / ( xe - xw ) 
     &             * (-1.0D+00 ) / ( yn - ys )

              vnw  = ( xe - xq ) / ( xe - xw ) 
     &             * ( yq - ys ) / ( yn - ys )
              vnwx = (-1.0D+00 ) / ( xe - xw ) 
     &             * ( yq - ys ) / ( yn - ys )
              vnwy = ( xe - xq ) / ( xe - xw ) 
     &             * ( 1.0D+00 ) / ( yn - ys )

              vne  = ( xq - xw ) / ( xe - xw ) 
     &             * ( yq - ys ) / ( yn - ys )
              vnex = ( 1.0D+00 ) / ( xe - xw ) 
     &             * ( yq - ys ) / ( yn - ys )
              vney = ( xq - xw ) / ( xe - xw ) 
     &             * ( 1.0D+00 ) / ( yn - ys )

              amat(sw,sw) = amat(sw,sw) + wq * ( vswx * aq * vswx       
     &                                         + vswy * aq * vswy           
     &                                         + vsw  * cq * vsw )
              amat(sw,se) = amat(sw,se) + wq * ( vswx * aq * vsex       
     &                                         + vswy * aq * vsey           
     &                                         + vsw  * cq * vse )
              amat(sw,nw) = amat(sw,nw) + wq * ( vswx * aq * vnwx       
     &                                         + vswy * aq * vnwy           
     &                                         + vsw  * cq * vnw )
              amat(sw,ne) = amat(sw,ne) + wq * ( vswx * aq * vnex       
     &                                         + vswy * aq * vney           
     &                                         + vsw  * cq * vne )
              b(sw) =    b(sw)          + wq * ( vsw  * fq )

              amat(se,sw) = amat(se,sw) + wq * ( vsex * aq * vswx       
     &                                         + vsey * aq * vswy           
     &                                         + vse  * cq * vsw )
              amat(se,se) = amat(se,se) + wq * ( vsex * aq * vsex       
     &                                         + vsey * aq * vsey           
     &                                         + vse  * cq * vse )
              amat(se,nw) = amat(se,nw) + wq * ( vsex * aq * vnwx       
     &                                         + vsey * aq * vnwy           
     &                                         + vse  * cq * vnw )
              amat(se,ne) = amat(se,ne) + wq * ( vsex * aq * vnex       
     &                                         + vsey * aq * vney           
     &                                         + vse  * cq * vne )
              b(se) =    b(se)          + wq * ( vse  * fq )

              amat(nw,sw) = amat(nw,sw) + wq * ( vnwx * aq * vswx       
     &                                         + vnwy * aq * vswy           
     &                                         + vnw  * cq * vsw )
              amat(nw,se) = amat(nw,se) + wq * ( vnwx * aq * vsex       
     &                                         + vnwy * aq * vsey           
     &                                         + vnw  * cq * vse )
              amat(nw,nw) = amat(nw,nw) + wq * ( vnwx * aq * vnwx       
     &                                         + vnwy * aq * vnwy           
     &                                         + vnw  * cq * vnw )
              amat(nw,ne) = amat(nw,ne) + wq * ( vnwx * aq * vnex       
     &                                         + vnwy * aq * vney           
     &                                         + vnw  * cq * vne )
              b(nw) =    b(nw)          + wq * ( vnw  * fq )

              amat(ne,sw) = amat(ne,sw) + wq * ( vnex * aq * vswx       
     &                                         + vney * aq * vswy           
     &                                         + vne  * cq * vsw )
              amat(ne,se) = amat(ne,se) + wq * ( vnex * aq * vsex       
     &                                         + vney * aq * vsey           
     &                                         + vne  * cq * vse )
              amat(ne,nw) = amat(ne,nw) + wq * ( vnex * aq * vnwx       
     &                                         + vney * aq * vnwy           
     &                                         + vne  * cq * vnw )
              amat(ne,ne) = amat(ne,ne) + wq * ( vnex * aq * vnex       
     &                                         + vney * aq * vney           
     &                                         + vne  * cq * vne )
              b(ne) =    b(ne)          + wq * ( vne  * fq )


            end do

          end do

        end do

      end do
c
c  Where a node is on the boundary,
c  replace the finite element equation by a boundary condition.
c
      k = 0
      do j = 1, ny
        do i = 1, nx
          k = k + 1
          if ( i .eq. 1 .or. 
     &         i .eq. nx .or. 
     &         j .eq. 1 .or. 
     &         j .eq. ny ) then
            do jj = 1, mn
              amat(k,jj) = 0.0D+00
            end do
            do ii = 1, mn
              amat(ii,k) = 0.0D+00
            end do
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
      subroutine fem2d_h1s_error_linear ( nx, ny, x, y, u, exact_ux, 
     &  exact_uy, h1s )

c*********************************************************************72
c
cc FEM2D_H1S_ERROR_LINEAR: seminorm error of a finite element solution.
c
c  Discussion:
c
c    The finite element method has been used, over a rectangle,
c    involving a grid of NX*NY nodes, with piecewise linear elements used
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
c    20 June 2014
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
c    Input, double precision U(NX,NY), the finite element coefficients.
c
c    Input, function EXACT_UX(X,Y), EXACT_UY(X,Y), return the exact
c    derivatives with respect to X and Y, at the point (X,Y).
c
c    Output, double precision H1S, the estimated seminorm of
c    the error.
c
      implicit none

      integer nx
      integer ny
      integer quad_num
      parameter ( quad_num = 3 )

      double precision abscissa(quad_num)
      integer e
      integer ex
      integer ey
      double precision exact_ux
      external exact_ux
      double precision exact_uy
      external exact_uy
      double precision exq
      double precision eyq
      double precision h1s
      integer n
      integer ne
      integer nw
      integer qx
      integer qy
      integer s
      integer se
      integer sw
      double precision u(nx,ny)
      double precision uxq
      double precision uyq
      double precision vnex
      double precision vney
      double precision vnwx
      double precision vnwy
      double precision vsex
      double precision vsey
      double precision vswx
      double precision vswy
      integer w
      double precision weight(quad_num)
      double precision wq
      double precision x(nx)
      double precision xe
      double precision xq
      double precision xw
      double precision y(ny)
      double precision yn
      double precision yq
      double precision ys

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

      do ex = 1, nx - 1

        w = ex
        e = ex + 1

        xw = x(w)
        xe = x(e)

        do ey = 1, ny - 1

          s = ey
          n = ey + 1

          ys = y(s)
          yn = y(n)

          sw = ( ey - 1 ) * nx + ex
          se = ( ey - 1 ) * nx + ex + 1
          nw =   ey       * nx + ex
          ne =   ey       * nx + ex + 1

          do qx = 1, quad_num

            xq = ( ( 1.0D+00 - abscissa(qx) ) * xw
     &           + ( 1.0D+00 + abscissa(qx) ) * xe )
     &             / 2.0D+00

            do qy = 1, quad_num

              yq = ( ( 1.0D+00 - abscissa(qy) ) * ys
     &             + ( 1.0D+00 + abscissa(qy) ) * yn )
     &               / 2.0D+00

              wq = weight(qx) * ( xe - xw ) / 2.0D+00
     &           * weight(qy) * ( yn - ys ) / 2.0D+00

              vswx = (-1.0D+00 ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vswy = ( xe - xq ) / ( xe - xw ) 
     &             * (-1.0D+00 ) / ( yn - ys )

              vsex = ( 1.0D+00 ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vsey = ( xq - xw ) / ( xe - xw ) 
     &             * (-1.0D+00 ) / ( yn - ys )

              vnwx = (-1.0D+00 ) / ( xe - xw ) 
     &             * ( yq - ys ) / ( yn - ys )
              vnwy = ( xe - xq ) / ( xe - xw ) 
     &             * ( 1.0D+00 ) / ( yn - ys )

              vnex = ( 1.0D+00 ) / ( xe - xw ) 
     &             * ( yq - ys ) / ( yn - ys )
              vney = ( xq - xw ) / ( xe - xw ) 
     &             * ( 1.0D+00 ) / ( yn - ys )
c
c  Note that the south-west component of U is stored in U(W,S), not U(S,W)!
c
              uxq = u(w,s) * vswx + u(e,s) * vsex 
     &            + u(w,n) * vnwx + u(e,n) * vnex
              uyq = u(w,s) * vswy + u(e,s) * vsey 
     &            + u(w,n) * vnwy + u(e,n) * vney

              exq = exact_ux ( xq, yq )
              eyq = exact_uy ( xq, yq )

              h1s = h1s + wq * ( ( uxq - exq ) ** 2 
     &                         + ( uyq - eyq ) ** 2 )

            end do
          end do
        end do
      end do

      h1s = sqrt ( h1s )

      return
      end
      subroutine fem2d_l1_error ( nx, ny, x, y, u, exact, e1 )

c*********************************************************************72
c
cc FEM2D_L1_ERROR estimates the l1 error norm of a finite element solution.
c
c  Discussion:
c
c    The finite element method has been used, over a rectangle,
c    involving a grid of NX*NY nodes, with piecewise linear elements used
c    for the basis.
c
c    The finite element coefficients have been computed, and a formula for the
c    exact solution is known.
c
c    This function estimates the little l1 norm of the error:
c      E1 = sum ( 1 <= I <= NX, 1 <= J <= NY )
c        abs ( U(i,j) - EXACT(X(i),Y(j)) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 June 2014
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
c    Input, double precision U(NX,NY), the finite element coefficients.
c
c    Input, function EQ = EXACT ( X, Y ), returns the value of the exact
c    solution at the point (X,Y).
c
c    Output, double precision E1, the estimated L2 norm of the error.
c
      implicit none

      integer nx
      integer ny

      double precision e1
      double precision exact
      external exact
      integer i
      integer j
      double precision u(nx,ny)
      double precision x(nx)
      double precision y(ny)

      e1 = 0.0D+0
      do j = 1, ny
        do i = 1, nx
          e1 = e1 + abs ( u(i,j) - exact ( x(i), y(j) ) )
        end do
      end do

      e1 = e1 / dble ( nx ) / dble ( ny )

      return
      end
      subroutine fem2d_l2_error_linear ( nx, ny, x, y, u, exact, e2 )

c*********************************************************************72
c
cc FEM2D_L2_ERROR_LINEAR: L2 error norm of a finite element solution.
c
c  Discussion:
c
c    The finite element method has been used, over a rectangle,
c    involving a grid of NX*NY nodes, with piecewise linear elements used
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
c    20 June 2014
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
c    Input, double precision U(NX,NY), the finite element coefficients.
c
c    Input, double precision, external EXACT(X,Y), returns the value of
c    the exact solution at the point (X,Y).
c
c    Output, double precision E2, the estimated L2 norm of the error.
c
      implicit none

      integer nx
      integer ny
      integer quad_num
      parameter ( quad_num = 3 )

      double precision abscissa(quad_num)
      integer e
      double precision e2
      double precision eq
      integer ex
      double precision exact 
      external exact
      integer ey
      integer n
      integer ne
      integer nw
      integer qx
      integer qy
      integer s
      integer se
      integer sw
      double precision u(nx,ny)
      double precision uq
      double precision vne
      double precision vnw
      double precision vse
      double precision vsw
      integer w
      double precision weight(quad_num)
      double precision wq
      double precision x(nx)
      double precision xe
      double precision xq
      double precision xw
      double precision y(ny)
      double precision yn
      double precision yq
      double precision ys

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
c  Integrate over each interval.
c
      do ex = 1, nx - 1

        w = ex
        e = ex + 1

        xw = x(w)
        xe = x(e)

        do ey = 1, ny - 1

          s = ey
          n = ey + 1

          ys = y(s)
          yn = y(n)

          sw = ( ey - 1 ) * nx + ex
          se = ( ey - 1 ) * nx + ex + 1
          nw =   ey       * nx + ex
          ne =   ey       * nx + ex + 1

          do qx = 1, quad_num

            xq = ( ( 1.0D+00 - abscissa(qx) ) * xw
     &           + ( 1.0D+00 + abscissa(qx) ) * xe )
     &             / 2.0D+00

            do qy = 1, quad_num

              yq = ( ( 1.0D+00 - abscissa(qy) ) * ys
     &             + ( 1.0D+00 + abscissa(qy) ) * yn )
     &               / 2.0D+00

              wq = weight(qx) * ( xe - xw ) / 2.0D+00
     &           * weight(qy) * ( yn - ys ) / 2.0D+00

              vsw  = ( xe - xq ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vse  = ( xq - xw ) / ( xe - xw ) 
     &             * ( yn - yq ) / ( yn - ys )
              vnw  = ( xe - xq ) / ( xe - xw )
     &             * ( yq - ys ) / ( yn - ys )
              vne  = ( xq - xw ) / ( xe - xw )
     &             * ( yq - ys ) / ( yn - ys )
c
c  Note that the south-west component of U is stored in U(W,S), not U(S,W)!
c
              uq = u(w,s) * vsw + u(e,s) * vse 
     &           + u(w,n) * vnw + u(e,n) * vne
              eq = exact ( xq, yq )

              e2 = e2 + wq * ( uq - eq ) ** 2

            end do
          end do
        end do
      end do

      e2 = sqrt ( e2 )

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
      subroutine r8vec_even ( n, alo, ahi, a )

c*********************************************************************72
c
cc R8VEC_EVEN returns an R8VEC of evenly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    If N is 1, then the midpoint is returned.
c
c    Otherwise, the two endpoints are returned, and N-2 evenly
c    spaced points between them.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values.
c
c    Input, double precision ALO, AHI, the low and high values.
c
c    Output, double precision A(N), N evenly spaced values.
c    Normally, A(1) = ALO and A(N) = AHI.
c    However, if N = 1, then A(1) = 0.5*(ALO+AHI).
c
      implicit none

      integer n

      double precision a(n)
      double precision ahi
      double precision alo
      integer i

      if ( n .eq. 1 ) then

        a(1) = 0.5D+00 * ( alo + ahi )

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * alo
     &           + dble (     i - 1 ) * ahi )
     &           / dble ( n     - 1 )
        end do

      end if

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
