      subroutine fem1d_bvp_quadratic ( n, a, c, f, x, u )

c*********************************************************************72
c
cc FEM1D_BVP_QUADRATIC solves a two point boundary value problem.
c
c  Discussion:
c
c    The finite element method is used, with a mesh of piecewise quadratic
c    elements.
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
c  Parameters:
c
c    Input, integer N, the number of nodes.
c
c    Input, function A ( X ), evaluates a(x);
c
c    Input, function C ( X ), evaluates c(x);
c
c    Input, function F ( X ), evaluates f(x);
c
c    Input, double precision X(N), the mesh points.
c
c    Output, double precision U(N), the finite element coefficients, which 
c    are also the value of the computed solution at the mesh points.
c
      implicit none

      integer n
      integer quad_num
      parameter ( quad_num = 3 )

      double precision a
      external a
      double precision abscissa(quad_num)
      double precision al
      double precision am
      double precision ar
      double precision amat(n,n)
      double precision axq
      double precision b(n)
      double precision bm
      double precision c
      external c
      double precision cxq
      integer e
      integer e_num
      double precision f
      external f
      double precision fxq
      integer i
      integer ierror
      integer j
      integer l
      integer m
      integer q
      integer r
      double precision weight(quad_num)
      double precision wq
      double precision u(n)
      double precision vl
      double precision vlp
      double precision vm
      double precision vmp
      double precision vr
      double precision vrp
      double precision x(n)
      double precision xl
      double precision xm
      double precision xq
      double precision xr
c
c  Quadrature definitions.
c
      abscissa(1) = -0.774596669241483377035853079956D+00
      abscissa(2) = 0.000000000000000000000000000000D+00
      abscissa(3) = 0.774596669241483377035853079956D+00
      weight(1) = 0.555555555555555555555555555556D+00
      weight(2) = 0.888888888888888888888888888889D+00
      weight(3) = 0.555555555555555555555555555556D+00
c
c  Zero out the matrix and right hand side.
c
      do j = 1, n
        do i = 1, n
          amat(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        b(i) = 0.0D+00
      end do
c
c  Integrate over element E.
c
      e_num = ( n - 1 ) / 2

      do e = 1, e_num
c
c  Element E uses nodes
c    L = 2 * E - 1
c    M = 2 * E
c    R = 2 * E + 1
c
        l = 2 * e - 1
        m = 2 * e
        r = 2 * e + 1

        xl = x(l)
        xm = x(m)
        xr = x(r)

        do q = 1, quad_num

          xq = ( ( 1.0 - abscissa(q) ) * xl   
     &         + ( 1.0 + abscissa(q) ) * xr ) 
     &         / 2.0;

          wq = weight(q) * ( xr - xl ) / 2.0

          axq = a ( xq );
          cxq = c ( xq );
          fxq = f ( xq );

          vl = ( ( xq - xm ) / ( xl - xm ) ) 
     &       * ( ( xq - xr ) / ( xl - xr ) )

          vm = ( ( xq - xl ) / ( xm - xl ) ) 
     &       * ( ( xq - xr ) / ( xm - xr ) )

          vr = ( ( xq - xl ) / ( xr - xl ) ) 
     &       * ( ( xq - xm ) / ( xr - xm ) )

          vlp = (     1.0D+00 / ( xl - xm ) ) 
     &        * ( ( xq - xr ) / ( xl - xr ) ) 
     &        + ( ( xq - xm ) / ( xl - xm ) ) 
     &        * (     1.0D+00 / ( xl - xr ) )

          vmp = (     1.0D+00 / ( xm - xl ) ) 
     &        * ( ( xq - xr ) / ( xm - xr ) ) 
     &        + ( ( xq - xl ) / ( xm - xl ) ) 
     &        * (     1.0D+00 / ( xm - xr ) )

          vrp = (     1.0D+00 / ( xr - xl ) ) 
     &        * ( ( xq - xm ) / ( xr - xm ) ) 
     &        + ( ( xq - xl ) / ( xr - xl ) ) 
     &        * (     1.0D+00 / ( xr - xm ) )

          amat(l,l) = amat(l,l) 
     &      + wq * ( vlp * axq * vlp + vl * cxq * vl )
          amat(l,m) = amat(l,m) 
     &      + wq * ( vlp * axq * vmp + vl * cxq * vm )
          amat(l,r) = amat(l,r) 
     &      + wq * ( vlp * axq * vrp + vl * cxq * vr )
          b(l)   = b(l)   + wq * ( vl * fxq )

          amat(m,l) = amat(m,l) 
     &      + wq * ( vmp * axq * vlp + vm * cxq * vl )
          amat(m,m) = amat(m,m) 
     &      + wq * ( vmp * axq * vmp + vm * cxq * vm )
          amat(m,r) = amat(m,r) 
     &      + wq * ( vmp * axq * vrp + vm * cxq * vr )
          b(m) =   b(m)   + wq * ( vm * fxq )

          amat(r,l) = amat(r,l) 
     &      + wq * ( vrp * axq * vlp + vr * cxq * vl )
          amat(r,m) = amat(r,m) 
     &      + wq * ( vrp * axq * vmp + vr * cxq * vm )
          amat(r,r) = amat(r,r) 
     &      + wq * ( vrp * axq * vrp + vr * cxq * vr )
          b(r) =   b(r)   + wq * ( vr * fxq )

        end do

      end do
c
c  Equation 1 is the left boundary condition, U(0.0) = 0.0;
c
      do j = 1, n
        amat(1,j) = 0.0D+00
      end do
      amat(1,1) = 1.0D+00
      b(1) = 0.0D+00
c
c  Equation N is the right boundary condition, U(1.0) = 0.0;
c
      do j = 1, n
        amat(n,j) = 0.0D+00
      end do
      amat(n,n) = 1.0D+00
      b(n) = 0.0D+00
c
c  Solve the linear system.
c
      call r8mat_solve2 ( n, amat, b, u, ierror )

      return
      end
      subroutine h1s_error_quadratic ( n, x, u, exact_ux, h1s )

c*********************************************************************72
c
cc H1S_ERROR_QUADRATIC: seminorm error of a finite element solution.
c
c  Discussion:
c
c    We assume the finite element method has been used, over an interval [A,B]
c    involving N nodes, with piecewise quadratic elements used for the basis.
c    The coefficients U(1:N) have been computed, and a formula for the
c    exact derivative is known.
c
c    This function estimates the seminorm of the error:
c
c      SEMINORM = Integral ( A <= X <= B ) ( dU(X)/dx - EXACT_UX(X) )^2 dX
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
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), the mesh points.
c
c    Input, double precision U(N), the finite element coefficients.
c
c    Input, function EQ = EXACT_UX ( X ), returns the value of the exact
c    derivative at the point X.
c
c    Output, double precision H1S the estimated seminorm of 
c    the error.
c
      implicit none

      integer n
      integer quad_num
      parameter ( quad_num = 3 )

      double precision abscissa(quad_num)
      integer e
      integer e_num
      double precision exact_ux
      external exact_ux
      double precision exq
      double precision h1s
      integer l
      integer m
      integer q
      integer r
      double precision u(n)
      double precision uxq
      double precision vlp
      double precision vmp
      double precision vrp
      double precision weight(quad_num)
      double precision wq
      double precision x(n)
      double precision xl
      double precision xm
      double precision xq
      double precision xr

      h1s = 0.0D+00
c
c  Quadrature definitions.
c
      abscissa(1) = -0.774596669241483377035853079956D+00
      abscissa(2) = 0.000000000000000000000000000000D+00
      abscissa(3) = 0.774596669241483377035853079956D+00
      weight(1) = 0.555555555555555555555555555556D+00
      weight(2) = 0.888888888888888888888888888889D+00
      weight(3) = 0.555555555555555555555555555556D+00
c
c  Integrate over element E.
c
      e_num = ( n - 1 ) / 2

      do e = 1, e_num
c
c  Element E uses nodes
c    L = 2 * E - 1
c    M = 2 * E
c    R = 2 * E + 1
c
        l = 2 * e - 1
        m = 2 * e
        r = 2 * e + 1

        xl = x(l)
        xm = x(m)
        xr = x(r)

        do q = 1, quad_num

          xq = ( ( 1.0D+00 - abscissa(q) ) * xl   
     &         + ( 1.0D+00 + abscissa(q) ) * xr ) 
     &         /   2.0D+00

          wq = weight(q) * ( xr - xl ) / 2.0

          vlp = (     1.0D+00 / ( xl - xm ) ) 
     &        * ( ( xq - xr ) / ( xl - xr ) ) 
     &        + ( ( xq - xm ) / ( xl - xm ) ) 
     &        * (     1.0D+00 / ( xl - xr ) )

          vmp = (     1.0D+00 / ( xm - xl ) ) 
     &        * ( ( xq - xr ) / ( xm - xr ) ) 
     &        + ( ( xq - xl ) / ( xm - xl ) ) 
     &        * (     1.0D+00 / ( xm - xr ) )

          vrp = (     1.0D+00 / ( xr - xl ) ) 
     &        * ( ( xq - xm ) / ( xr - xm ) ) 
     &        + ( ( xq - xl ) / ( xr - xl ) ) 
     &        * (     1.0D+00 / ( xr - xm ) )

          uxq = u(l) * vlp + u(m) * vmp + u(r) * vrp

          exq = exact_ux ( xq )

          h1s = h1s + wq * ( uxq - exq ) ** 2

        end do

      end do

      h1s = sqrt ( h1s )

      return
      end
      subroutine l1_error ( n, x, u, exact, e1 )

c*********************************************************************72
c
cc L1_ERROR estimates the l1 error norm of a finite element solution.
c
c  Discussion:
c
c    We assume the finite element method has been used, over an interval [A,B]
c    involving N nodes.
c
c    The coefficients U(1:N) have been computed, and a formula for the
c    exact solution is known.
c
c    This function estimates the little l1 norm of the error:
c      L1_NORM = sum ( 1 <= I <= N ) abs ( U(i) - EXACT(X(i)) )
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
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), the mesh points.
c
c    Input, double precision U(N), the finite element coefficients.
c
c    Input, function EQ = EXACT ( X ), returns the value of the exact
c    solution at the point X.
c
c    Output, double precision E1, the estimated L2 norm of the error.
c
      implicit none

      integer n

      double precision e1
      double precision exact
      external exact
      integer i
      double precision u(n)
      double precision x(n)

      e1 = 0.0D+0
      do i = 1, n
        e1 = e1 + abs ( u(i) - exact ( x(i) ) )
      end do

      e1 = e1 / dble ( n )

      return
      end
      subroutine l2_error_quadratic ( n, x, u, exact, e2 )

c*********************************************************************72
c
cc L2_ERROR_QUADRATIC estimates the L2 error norm of a finite element solution.
c
c  Discussion:
c
c    We assume the finite element method has been used, over an interval [A,B]
c    involving N nodes, with piecewise quadratic elements used for the basis.
c
c    The coefficients U(1:N) have been computed, and a formula for the
c    exact solution is known.
c
c    This function estimates the L2 norm of the error:
c
c      L2_NORM = Integral ( A <= X <= B ) ( U(X) - EXACT(X) )^2 dX
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
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), the mesh points.
c
c    Input, double precision U(N), the finite element coefficients.
c
c    Input, function EQ = EXACT ( X ), returns the value of the exact
c    solution at the point X.
c
c    Output, double precision E2, the estimated L2 norm of the error.
c
      implicit none

      integer n
      integer quad_num
      parameter ( quad_num = 3 )

      double precision abscissa(quad_num)
      integer e
      integer e_num
      double precision e2
      double precision eq
      double precision exact
      external exact
      integer l
      integer m
      integer q
      integer r
      double precision u(n)
      double precision vl
      double precision vm
      double precision vr
      double precision uq
      double precision weight(quad_num)
      double precision wq
      double precision x(n)
      double precision xl
      double precision xm
      double precision xq
      double precision xr

      e2 = 0.0D+00
c
c  Quadrature definitions.
c
      abscissa(1) = -0.774596669241483377035853079956D+00
      abscissa(2) = 0.000000000000000000000000000000D+00
      abscissa(3) = 0.774596669241483377035853079956D+00
      weight(1) = 0.555555555555555555555555555556D+00
      weight(2) = 0.888888888888888888888888888889D+00
      weight(3) = 0.555555555555555555555555555556D+00
c
c  Integrate over element E.
c
      e_num = ( n - 1 ) / 2

      do e = 1, e_num
c
c  Element E uses nodes
c    L = 2 * E - 1
c    M = 2 * E
c    R = 2 * E + 1
c
        l = 2 * e - 1
        m = 2 * e
        r = 2 * e + 1

        xl = x(l)
        xm = x(m)
        xr = x(r)

        do q = 1, quad_num

          xq = ( ( 1.0D+00 - abscissa(q) ) * xl   
     &         + ( 1.0D+00 + abscissa(q) ) * xr ) 
     &         /   2.0D+00

          wq = weight(q) * ( xr - xl ) / 2.0D+00

          vl = ( ( xq - xm ) / ( xl - xm ) ) 
     &       * ( ( xq - xr ) / ( xl - xr ) )

          vm = ( ( xq - xl ) / ( xm - xl ) ) 
     &       * ( ( xq - xr ) / ( xm - xr ) )

          vr = ( ( xq - xl ) / ( xr - xl ) ) 
     &       * ( ( xq - xm ) / ( xr - xm ) )

          uq = u(l) * vl + u(m) * vm + u(r) * vr
          eq = exact ( xq )

          e2 = e2 + wq * ( uq - eq ) ** 2

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
