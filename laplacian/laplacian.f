      subroutine cholesky_upper_error ( n, a, c, error_frobenius )

c*********************************************************************72
c
cc CHOLESKY_UPPER_ERROR determines the error in an upper Cholesky factor.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix.
c
c    Input, double precision C(N,N), the upper triangular Cholesky factor.
c
c    Output, double precision ERROR_FROBENIUS, the Frobenius norm
c    of the difference matrix A - C' * C.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision c(n,n)
      double precision ctc(n,n)
      double precision error_frobenius
      integer i
      integer j

      call r8mat_mtm ( n, n, n, c, c, ctc )

      error_frobenius = 0.0D+00
      do j = 1, n
        do i = 1, n
          error_frobenius = error_frobenius + ( a(i,j) - ctc(i,j) )**2
        end do
      end do
      error_frobenius = sqrt ( error_frobenius )

      return
      end
      subroutine eigen_error ( n, k, a, x, lambda, error_frobenius )

c*********************************************************************72
c
cc EIGEN_ERROR determines the error in a (right) eigensystem.
c
c  Discussion:
c
c    An R8MAT is a matrix of double precision values.
c
c    This routine computes the Frobenius norm of
c
c      A * X - X * LAMBDA
c
c    where
c
c      A is an N by N matrix,
c      X is an N by K matrix (each of K columns is an eigenvector)
c      LAMBDA is a K by K diagonal matrix of eigenvalues.
c
c    This routine assumes that A, X and LAMBDA are all real!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer K, the number of eigenvectors.
c    K is usually 1 or N.
c
c    Input, double precision A(N,N), the matrix.
c
c    Input, double precision X(N,K), the K eigenvectors.
c
c    Input, double precision LAMBDA(K), the K eigenvalues.
c
c    Output, double precision ERROR_FROBENIUS, the Frobenius norm
c    of the difference matrix A * X - X * LAMBDA, which would be exactly zero
c    if X and LAMBDA were exact eigenvectors and eigenvalues of A.
c
      implicit none

      integer k
      integer n

      double precision a(n,n)
      double precision c(n,k)
      double precision error_frobenius
      integer i
      integer j
      double precision lambda(k)
      double precision x(n,k)

      call r8mat_mm ( n, n, k, a, x, c )

      do j = 1, k
        do i = 1, n
          c(i,j) = c(i,j) - lambda(j) * x(i,j)
        end do
      end do

      error_frobenius = 0.0D+00
      do j = 1, n
        do i = 1, n
          error_frobenius = error_frobenius + c(i,j) ** 2
        end do
      end do
      error_frobenius = sqrt ( error_frobenius )

      return
      end
      subroutine inverse_error ( n, a, b, error_frobenius )

c*********************************************************************72
c
cc INVERSE_ERROR determines the error in an inverse matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix.
c
c    Input, double precision B(N,N), the inverse.
c
c    Output, double precision ERROR_FROBENIUS, the Frobenius norm
c    of (A*B-I) + (B*A-I).
c
      implicit none

      integer n

      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)
      double precision error_ab
      double precision error_ba
      double precision error_frobenius
      integer i
      integer j

      call r8mat_mm ( n, n, n, a, b, c )

      do j = 1, n
        c(j,j) = c(j,j) - 1.0D+00
      end do

      error_ab = 0.0D+00
      do j = 1, n
        do i = 1, n
          error_ab = error_ab + c(i,j) ** 2
        end do
      end do
      error_ab = sqrt ( error_ab )

      call r8mat_mm ( n, n, n, b, a, c )

      do j = 1, n
        c(j,j) = c(j,j) - 1.0D+00
      end do

      error_ba = 0.0D+00
      do j = 1, n
        do i = 1, n
          error_ba = error_ba + c(i,j) ** 2
        end do
      end do
      error_ba = sqrt ( error_ba )

      error_frobenius = error_ab + error_ba

      return
      end
      subroutine l1dd_apply ( n, h, u, lu )

c*********************************************************************72
c
cc L1DD_APPLY applies the 1D DD Laplacian to a vector.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with Dirichlet boundary conditions
c    at both ends of [0,6] is applied to a vector of 7 values, with a spacing
c    of H = 6/(N+1) = 1 at the points X:
c
c      0  1  2  3  4  5  6
c
c    and has the matrix form L:
c
c       2 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Input, double precision U(N), the value at each point.
c
c    Output, double precision LU(N), the Laplacian evaluated at each point.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision lu(n)
      double precision u(n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DD_APPLY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      i = 1
      lu(i) = ( 2.0D+00 * u(i) - u(i+1) ) / h / h
      do i = 2, n - 1
        lu(i) = ( - u(i-1) + 2.0D+00 * u(i) - u(i+1) ) / h / h
      end do
      i = n
      lu(i) = ( - u(i-1) + 2.0D+00 * u(i) ) / h / h

      return
      end
      subroutine l1dd_cholesky ( n, h, c )

c*********************************************************************72
c
cc L1DD_CHOLESKY computes the Cholesky factor of the 1D DD Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision C(N,N), the Cholesky factor.
c
      implicit none

      integer n

      double precision c(n,n)
      double precision h
      integer i
      integer j

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DD_CHOLESKY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          c(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        c(i,i) = sqrt ( dble ( i + 1 ) ) / sqrt ( dble ( i ) )
      end do

      do i = 1, n - 1
        c(i,i+1) = - sqrt ( dble ( i ) ) / sqrt ( dble ( i + 1 ) )
      end do

      do j = 1, n
        do i = 1, n
          c(i,j) = c(i,j) / h
        end do
      end do

      return
      end
      subroutine l1dd_eigen ( n, h, v, lambda )

c*********************************************************************72
c
cc L1DD_EIGEN returns eigeninformation for the 1D DD Laplacian.
c
c  Discussion:
c
c    The grid points are assumed to be evenly spaced by H.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision V(N,N), the eigenvectors.
c
c    Output, double precision LAMBDA(N), the eigenvalues.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision j_r8
      double precision lambda(n)
      double precision n_r8
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision s
      double precision theta
      double precision v(n,n)

      n_r8 = dble ( n )

      do j = 1, n
        j_r8 = dble ( j )
        theta = 0.5D+00 * pi * j_r8 / ( n_r8 + 1.0D+00 )
        lambda(j) = 4.0D+00 * ( sin ( theta ) / h ) ** 2
        do i = 1, n
          i_r8 = dble ( i )
          theta = pi * i_r8 * j_r8 / ( n_r8 + 1.0D+00 )
          v(i,j) = sqrt ( 2.0D+00 / ( n_r8 + 1.0D+00 ) ) * sin ( theta )
        end do
      end do

      return
      end
      subroutine l1dd ( n, h, l )

c*********************************************************************72
c
cc L1DD stores the 1D DD Laplacian as a full matrix.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with Dirichlet boundary conditions
c    at both ends of [0,6] has the matrix form L:
c
c       2 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DD - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      i = 1
      l(1,1) =  2.0D+00 / h / h
      l(1,2) = -1.0D+00 / h / h

      do i = 2, n - 1
        l(i,i-1) = -1.0D+00 / h / h
        l(i,i) =    2.0D+00 / h / h
        l(i,i+1) = -1.0D+00 / h / h
      end do

      i = n
      l(n,n-1) = -1.0D+00 / h / h
      l(n,n) =    2.0D+00 / h / h

      return
      end
      subroutine l1dd_inverse ( n, h, l )

c*********************************************************************72
c
cc L1DD_INVERSE stores the inverse of the 1D DD Laplacian.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with Dirichlet boundary conditions
c    at both ends of [0,6] has the matrix form L:
c
c       2 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the inverse of the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DD_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = dble ( min ( i, j ) * ( n + 1 - max ( i, j ) ) ) 
     &           * h * h / dble ( n + 1 )
        end do
      end do

      return
      end
      subroutine l1dd_lu ( n, h, l, u )

c*********************************************************************72
c
cc L1DD_LU computes the LU factors of the 1D DD Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ( kind = 4 ) N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), U(N,N), the LU factors.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision l(n,n)
      double precision u(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DD_LU - Fatal error!'
        write ( *, '(a)' ) '  N < 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        l(i,i) = 1.0D+00
      end do

      do i = 2, n
        i_r8 = dble ( i )
        l(i,i-1) = - ( i_r8 - 1.0D+00 ) / i_r8
      end do

      do j = 1, n
        do i = 1, n
          l(i,j) = l(i,j) / h
        end do
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        i_r8 = dble ( i )
        u(i,i) = ( i_r8 + 1.0D+00 ) / i_r8
      end do

      do i = 1, n - 1
       u(i,i+1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = u(i,j) / h
        end do
      end do

      return
      end
      subroutine l1dn_apply ( n, h, u, lu )

c*********************************************************************72
c
cc L1DN_APPLY applies the 1D DN Laplacian to a vector.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with left Dirichlet and right
c    Neumann condition on [0,6] has the matrix form L:
c
c       2 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Input, double precision U(N), the value at each point.
c
c    Output, double precision LU(N), the Laplacian evaluated at each point.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision lu(n)
      double precision u(n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DN_APPLY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      i = 1
      lu(i) = ( 2.0D+00 * u(i) - u(i+1) ) / h / h
      do i = 2, n - 1
        lu(i) = ( - u(i-1) + 2.0D+00 * u(i) - u(i+1) ) / h / h
      end do
      i = n
      lu(i) = ( - u(i-1) + u(i) ) / h / h

      return
      end
      subroutine l1dn_cholesky ( n, h, c )

c*********************************************************************72
c
cc L1DN_CHOLESKY computes the Cholesky factor of the 1D DN Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision C(N,N), the Cholesky factor.
c
      implicit none

      integer n

      double precision c(n,n)
      double precision h
      integer i
      double precision i_r8
      integer j

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DN_CHOLESKY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          c(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1
        i_r8 = dble ( i )
        c(i,i)   =   sqrt ( i_r8 + 1.0D+00 ) / sqrt ( i_r8 )
        c(i,i+1) = - sqrt ( i_r8 ) / sqrt ( i_r8 + 1.0D+00 )
      end do
      c(n,n) = 1.0D+00 / sqrt ( dble ( n ) )

      do j = 1, n
        do i = 1, n
          c(i,j) = c(i,j) / h
        end do
      end do

      return
      end
      subroutine l1dn_eigen ( n, h, v, lambda )

c*********************************************************************72
c
cc L1DN_EIGEN returns eigeninformation for the 1D DN Laplacian.
c
c  Discussion:
c
c    The grid points are assumed to be evenly spaced by H.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision V(N,N), the eigenvectors.
c
c    Output, double precision LAMBDA(N), the eigenvalues.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision j_r8
      double precision lambda(n)
      double precision n_r8
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision s
      double precision theta
      double precision v(n,n)

      n_r8 = dble ( n )

      do j = 1, n
        j_r8 = dble ( j )
        theta = pi * ( j_r8 - 0.5D+00 ) / ( 2.0D+00 * n_r8 + 1.0D+00 )
        lambda(j) = ( 2.0D+00 * sin ( theta ) / h ) ** 2
        do i = 1, n
          i_r8 = dble ( i )
          theta = pi * i_r8 * ( 2.0D+00 * j_r8 - 1.0D+00 ) / 
     &      ( 2.0D+00 * n_r8 + 1.0D+00 )
          v(i,j) = sqrt ( 2.0D+00 / ( n_r8 + 0.5D+00 ) ) * sin ( theta )
        end do
      end do

      return
      end
      subroutine l1dn ( n, h, l )

c*********************************************************************72
c
cc L1DN stores the 1D DN Laplacian as a full matrix.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with left Dirichlet and right
c    Neumann condition on [0,6] has the matrix form L:
c
c       2 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DN - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      i = 1
      l(1,1) =  2.0D+00 / h / h
      l(1,2) = -1.0D+00 / h / h

      do i = 2, n - 1
        l(i,i-1) = -1.0D+00 / h / h
        l(i,i) =    2.0D+00 / h / h
        l(i,i+1) = -1.0D+00 / h / h
      end do

      i = n
      l(n,n-1) = -1.0D+00 / h / h
      l(n,n) =    1.0D+00 / h / h

      return
      end
      subroutine l1dn_inverse ( n, h, l )

c*********************************************************************72
c
cc L1DN_INVERSE stores the inverse of the 1D DN Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the inverse of the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DN_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = dble ( min ( i, j ) ) * h * h
        end do
      end do

      return
      end
      subroutine l1dn_lu ( n, h, l, u )

c*********************************************************************72
c
cc L1DN_LU computes the LU factors of the 1D DN Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ( kind = 4 ) N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), U(N,N), the LU factors.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision l(n,n)
      double precision u(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DN_LU - Fatal error!'
        write ( *, '(a)' ) '  N < 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        l(i,i) = 1.0D+00
      end do

      do i = 2, n
        i_r8 = dble ( i )
        l(i,i-1) = - ( i_r8 - 1.0D+00 ) / i_r8
      end do

      do j = 1, n
        do i = 1, n
          l(i,j) = l(i,j) / h
        end do
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1
        i_r8 = dble ( i )
        u(i,i) = ( i_r8 + 1.0D+00 ) / i_r8
      end do
      i = n
      i_r8 = dble ( i )
      u(i,i) = 1.0D+00 / i_r8

      do i = 1, n - 1
       u(i,i+1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = u(i,j) / h
        end do
      end do

      return
      end
      subroutine l1nd_apply ( n, h, u, lu )

c*********************************************************************72
c
cc L1ND_APPLY applies the 1D ND Laplacian to a vector.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with left Neumann and right Dirichlet
c    boundary conditions on [0,6] has the matrix form L:
c
c       1 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Input, double precision U(N), the value at each point.
c
c    Output, double precision LU(N), the Laplacian evaluated at each point.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision lu(n)
      double precision u(n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1ND_APPLY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      i = 1
      lu(i) = ( u(i) - u(i+1) ) / h / h
      do i = 2, n - 1
        lu(i) = ( - u(i-1) + 2.0D+00 * u(i) - u(i+1) ) / h / h
      end do
      i = n
      lu(i) = ( - u(i-1) + 2.0D+00 * u(i) ) / h / h

      return
      end
      subroutine l1nd_cholesky ( n, h, c )

c*********************************************************************72
c
cc L1ND_CHOLESKY computes the Cholesky factor of the 1D ND Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision C(N,N), the Cholesky factor.
c
      implicit none

      integer n

      double precision c(n,n)
      double precision h
      integer i
      double precision i_r8
      integer j

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1DN_CHOLESKY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          c(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        c(i,i) = 1.0D+00
      end do

      do i = 1, n - 1
        c(i,i+1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          c(i,j) = c(i,j) / h
        end do
      end do

      return
      end
      subroutine l1nd_eigen ( n, h, v, lambda )

c*********************************************************************72
c
cc L1ND_EIGEN returns eigeninformation for the 1D ND Laplacian.
c
c  Discussion:
c
c    The grid points are assumed to be evenly spaced by H.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision V(N,N), the eigenvectors.
c
c    Output, double precision LAMBDA(N), the eigenvalues.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision j_r8
      double precision lambda(n)
      double precision n_r8
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision s
      double precision theta
      double precision v(n,n)

      n_r8 = dble ( n )

      do j = 1, n
        j_r8 = dble ( j )
        theta = pi * ( j_r8 - 0.5D+00 ) / ( 2.0D+00 * n_r8 + 1.0D+00 )
        lambda(j) = 4.0D+00 * ( sin ( theta ) / h ) ** 2
        do i = 1, n
          i_r8 = dble ( i )
          theta = pi * ( i_r8 - 0.5D+00 ) * ( 2.0D+00 * j_r8 - 1.0D+00 )
     &      / ( 2.0D+00 * n_r8 + 1.0D+00 )
          v(i,j) = sqrt ( 2.0D+00 / ( n_r8 + 0.5D+00 ) ) * cos ( theta )
        end do
      end do

      return
      end
      subroutine l1nd ( n, h, l )

c*********************************************************************72
c
cc L1ND stores the 1D ND Laplacian as a full matrix.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with left Neumann and right Dirichlet
c    boundary conditions on [0,6] has the matrix form L:
c
c       1 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1ND - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      i = 1
      l(1,1) =  1.0D+00 / h / h
      l(1,2) = -1.0D+00 / h / h

      do i = 2, n - 1
        l(i,i-1) = -1.0D+00 / h / h
        l(i,i) =    2.0D+00 / h / h
        l(i,i+1) = -1.0D+00 / h / h
      end do

      i = n
      l(n,n-1) = -1.0D+00 / h / h
      l(n,n) =    2.0D+00 / h / h

      return
      end
      subroutine l1nd_inverse ( n, h, l )

c*********************************************************************72
c
cc L1ND_INVERSE stores the inverse of the 1D ND Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the inverse of the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1ND_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = dble ( n + 1 - max ( i, j ) ) * h * h
        end do
      end do

      return
      end
      subroutine l1nd_lu ( n, h, l, u )

c*********************************************************************72
c
cc L1ND_LU computes the LU factors of the 1D ND Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ( kind = 4 ) N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), U(N,N), the LU factors.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)
      double precision u(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1ND_LU - Fatal error!'
        write ( *, '(a)' ) '  N < 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        l(i,i) = 1.0D+00
      end do

      do i = 2, n
        l(i,i-1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          l(i,j) = l(i,j) / h
        end do
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        u(i,i) = 1.0D+00
      end do

      do i = 1, n - 1
       u(i,i+1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = u(i,j) / h
        end do
      end do

      return
      end
      subroutine l1nn_apply ( n, h, u, lu )

c*********************************************************************72
c
cc L1NN_APPLY applies the 1D NN Laplacian to a vector.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with left Neumann and right Neumann
c    boundary conditions on [0,6] has the matrix form L:
c
c       1 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Input, double precision U(N), the value at each point.
c
c    Output, double precision LU(N), the Laplacian evaluated at each point.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision lu(n)
      double precision u(n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1NN_APPLY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      i = 1
      lu(i) = ( u(i) - u(i+1) ) / h / h
      do i = 2, n - 1
        lu(i) = ( - u(i-1) + 2.0D+00 * u(i) - u(i+1) ) / h / h
      end do
      i = n
      lu(i) = ( - u(i-1) +  u(i) ) / h / h

      return
      end
      subroutine l1nn_cholesky ( n, h, c )

c*********************************************************************72
c
cc L1NN_CHOLESKY computes the Cholesky factor of the 1D NN Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision C(N,N), the Cholesky factor.
c
      implicit none

      integer n

      double precision c(n,n)
      double precision h
      integer i
      integer j

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1NN_CHOLESKY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          c(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1
        c(i,i)   = + 1.0D+00
        c(i,i+1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          c(i,j) = c(i,j) / h
        end do
      end do

      return
      end
      subroutine l1nn_eigen ( n, h, v, lambda )

c*********************************************************************72
c
cc L1NN_EIGEN returns eigeninformation for the 1D NN Laplacian.
c
c  Discussion:
c
c    The grid points are assumed to be evenly spaced by H.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision V(N,N), the eigenvectors.
c
c    Output, double precision LAMBDA(N), the eigenvalues.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision j_r8
      double precision lambda(n)
      double precision n_r8
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision s
      double precision theta
      double precision v(n,n)

      n_r8 = dble ( n )

      do j = 1, n
        j_r8 = dble ( j )
        theta = pi * ( j_r8 - 1.0D+00 ) / ( 2.0D+00 * n_r8 )
        lambda(j) = 4.0D+00 * ( sin ( theta ) / h ) ** 2
        if ( j == 1 ) then
          do i = 1, n
            v(i,j) = sqrt ( n_r8 )
          end do
        else
          do i = 1, n
            i_r8 = dble ( i )
            theta = pi * ( i_r8 - 0.5D+00 ) * ( j_r8 - 1.0D+00 ) / n_r8
            v(i,j) = sqrt ( 2.0D+00 / n_r8 ) * cos ( theta )
          end do
        end if
      end do

      return
      end
      subroutine l1nn ( n, h, l )

c*********************************************************************72
c
cc L1NN stores the 1D NN Laplacian as a full matrix.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with Neumann boundary conditions
c    at both ends of [0,6] has the matrix form L:
c
c       1 -1  0  0  0
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c       0  0  0 -1  1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1NN - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      i = 1
      l(1,1) =  1.0D+00 / h / h
      l(1,2) = -1.0D+00 / h / h

      do i = 2, n - 1
        l(i,i-1) = -1.0D+00 / h / h
        l(i,i) =    2.0D+00 / h / h
        l(i,i+1) = -1.0D+00 / h / h
      end do

      i = n
      l(n,n-1) = -1.0D+00 / h / h
      l(n,n) =    1.0D+00 / h / h

      return
      end
      subroutine l1nn_lu ( n, h, l, u )

c*********************************************************************72
c
cc L1NN_LU computes the LU factors of the 1D NN Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ( kind = 4 ) N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), U(N,N), the LU factors.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)
      double precision u(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1NN_LU - Fatal error!'
        write ( *, '(a)' ) '  N < 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        l(i,i) = 1.0D+00
      end do

      do i = 2, n
        l(i,i-1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          l(i,j) = l(i,j) / h
        end do
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1
        u(i,i) = 1.0D+00
      end do
      u(n,n) = 0.0D+00

      do i = 1, n - 1
       u(i,i+1) = - 1.0D+00
      end do

      do j = 1, n
        do i = 1, n
          u(i,j) = u(i,j) / h
        end do
      end do

      return
      end
      subroutine l1pp_apply ( n, h, u, lu )

c*********************************************************************72
c
cc L1PP_APPLY applies the 1D PP Laplacian to a vector.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with periodic boundary conditions
c    on [0,6] has the matrix form L:
c
c       2 -1  0  0 -1
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c      -1  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Input, double precision U(N), the value at each point.
c
c    Output, double precision LU(N), the Laplacian evaluated at each point.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision lu(n)
      double precision u(n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1PP_APPLY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      i = 1
      lu(i) = ( - u(n) + 2.0D+00 * u(i) - u(i+1) ) / h / h
      do i = 2, n - 1
        lu(i) = ( - u(i-1) + 2.0D+00 * u(i) - u(i+1) ) / h / h
      end do
      i = n
      lu(i) = ( - u(i-1) + 2.0D+00 * u(i) - u(1) ) / h / h

      return
      end
      subroutine l1pp_cholesky ( n, h, c )

c*********************************************************************72
c
cc L1PP_CHOLESKY computes the Cholesky factor of the 1D PP Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision C(N,N), the Cholesky factor.
c
      implicit none

      integer n

      double precision c(n,n)
      double precision h
      integer i
      double precision i_r8
      integer j

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1PP_CHOLESKY - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          c(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1
        i_r8 = dble ( i )
        c(i,i) = sqrt ( i_r8 + 1.0D+00 ) / sqrt ( i_r8 )
      end do

      do i = 1, n - 2
        i_r8 = dble ( i )
        c(i,i+1) = - i_r8 / ( i_r8 + 1.0D+00 ) * sqrt ( i_r8 + 1.0D+00 )
     &    / sqrt ( i_r8 )
      end do

      do i = 1, n - 2
        i_r8 = dble ( i )
        c(i,n) = - 1.0D+00 / ( i_r8 + 1.0D+00 ) 
     &    * sqrt ( i_r8 + 1.0D+00 ) / sqrt ( i_r8 )
      end do

      i = n - 1
      i_r8 = dble ( i )
      c(i,n) = - dble ( n ) / ( i_r8 + 1.0D+00 ) 
     &  * sqrt ( i_r8 + 1.0D+00 )  / sqrt ( i_r8 )

      do j = 1, n
        do i = 1, n
          c(i,j) = c(i,j) / h
        end do
      end do

      return
      end
      subroutine l1pp_eigen ( n, h, v, lambda )

c*********************************************************************72
c
cc L1PP_EIGEN returns eigeninformation for the 1D PP Laplacian.
c
c  Discussion:
c
c    The grid points are assumed to be evenly spaced by H.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision V(N,N), the eigenvectors.
c
c    Output, double precision LAMBDA(N), the eigenvalues.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision j_r8
      double precision lambda(n)
      double precision n_r8
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision s
      double precision theta
      double precision v(n,n)

      n_r8 = dble ( n )

      do j = 1, n

        j_r8 = dble ( j )
        if ( mod ( j, 2 ) .eq. 1 ) then
          theta = pi * ( j_r8 - 1.0D+00 ) / ( 2.0D+00 * n_r8 )
        else
          theta = pi *   j_r8             / ( 2.0D+00 * n_r8 )
        end if
        lambda(j) = 4.0D+00 * ( sin ( theta ) / h ) ** 2

        if ( mod ( j, 2 ) .eq. 1 ) then
          if ( j .eq. 1 ) then
            do i = 1, n
              v(i,j) = 1.0D+00 / sqrt ( n_r8 )
            end do
          else
            do i = 1, n
              i_r8 = dble ( i )
              theta = pi * ( i_r8 - 0.5D+00 ) * ( j_r8 - 1.0D+00 ) 
     &          /  n_r8
              v(i,j) = sqrt ( 2.0D+00 / n_r8 ) * cos ( theta )
            end do
          end if
        else
          if ( j .eq. n ) then
            s = - 1.0D+00 / sqrt ( n_r8 )
            do i = 1, n
              v(i,j) = s
              s = - s
            end do
          else
            do i = 1, n
              i_r8 = dble ( i )
              theta = pi * ( i_r8 - 0.5D+00 ) * j_r8 / n_r8
              v(i,j) = sqrt ( 2.0D+00 / n_r8 ) * sin ( theta )
            end do
          end if
        end if

      end do

      return
      end
      subroutine l1pp ( n, h, l )

c*********************************************************************72
c
cc L1PP stores the 1D PP Laplacian as a full matrix.
c
c  Discussion:
c
c    The N grid points are assumed to be evenly spaced by H.
c
c    For N = 5, the discrete Laplacian with periodic boundary conditions
c    has the matrix form L:
c
c       2 -1  0  0 -1
c      -1  2 -1  0  0
c       0 -1  2 -1  0
c       0  0 -1  2 -1
c      -1  0  0 -1  2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), the Laplacian matrix.
c
      implicit none

      integer n

      double precision h
      integer i
      integer j
      double precision l(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1PP - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      i = 1
      l(1,1) =  2.0D+00 / h / h
      l(1,2) = -1.0D+00 / h / h
      l(1,n) = -1.0D+00 / h / h

      do i = 2, n - 1
        l(i,i-1) = -1.0D+00 / h / h
        l(i,i) =    2.0D+00 / h / h
        l(i,i+1) = -1.0D+00 / h / h
      end do

      i = n
      l(n,1) =   -1.0D+00 / h / h
      l(n,n-1) = -1.0D+00 / h / h
      l(n,n) =    2.0D+00 / h / h

      return
      end
      subroutine l1pp_lu ( n, h, l, u )

c*********************************************************************72
c
cc L1PP_LU computes the LU factors of the 1D PP Laplacian.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    N must be at least 3.
c
c    Input, double precision H, the spacing between points.
c
c    Output, double precision L(N,N), U(N,N), the LU factors.
c
      implicit none

      integer n

      double precision h
      integer i
      double precision i_r8
      integer j
      double precision l(n,n)
      double precision u(n,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'L1PP_LU - Fatal error!'
        write ( *, '(a)' ) '  N < 3.'
        stop 1
      end if

      do j = 1, n 
        do i = 1, n
          l(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        l(i,i) = 1.0D+00
      end do

      do i = 2, n - 1
        i_r8 = dble ( i )
        l(i,i-1) = - ( i_r8 - 1.0D+00 ) / i_r8
        l(n,i-1) =          - 1.0D+00   / i_r8
      end do
      l(n,n-1) = -1.0D+00

      do j = 1, n 
        do i = 1, n
          l(i,j) = l(i,j) / h
        end do
      end do

      do j = 1, n 
        do i = 1, n
          u(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 2
        i_r8 = dble ( i )
        u(i,i) =   ( i_r8 + 1.0D+00 ) / i_r8
        u(i,i+1) = - 1.0D+00
        u(i,n) =   - 1.0D+00 / i_r8
      end do

      i = n - 1
      i_r8 = dble ( i )
      u(i,i) =     ( i_r8 + 1.0D+00 ) / i_r8
      u(i,i+1) = - ( i_r8 + 1.0D+00 ) / i_r8

      i = n
      u(i,i) = 0.0D+00

      do j = 1, n 
        do i = 1, n
          u(i,j) = u(i,j) / h
        end do
      end do

      return
      end
      subroutine laplacian_1d_uneven_apply ( bc, n, x, u, lu )

c*********************************************************************72
c
cc LAPLACIAN_1D_UNEVEN_APPLY applies the 1D Discrete Laplacian to a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2013
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

      integer n

      double precision alpha
      integer bc(2)
      double precision beta
      double precision dxl
      double precision dxr
      double precision gamma
      integer i
      double precision lu(n)
      double precision u(n)
      double precision x(n)

      if ( bc(1) .eq. 0 ) then
        lu(1) = u(1)
      else if ( bc(1) .eq. 1 ) then
        lu(1) = ( u(2) - u(1) ) / ( x(2) - x(1) )
      else if ( bc(1) .eq. 2 ) then
        dxl = x(n) - x(n-1)
        dxr = x(2) - x(1)
        alpha =  2.0 *   dxr         / dxl / ( dxl + dxr ) / dxr
        beta = - 2.0 * ( dxr + dxl ) / dxl / ( dxl + dxr ) / dxr
        gamma =  2.0 *         dxl   / dxl / ( dxl + dxr ) / dxr
        lu(1) = alpha * u(n-1) + beta * u(1) + gamma * u(2)
      end if

      do i = 2, n - 1

        dxl = x(i)   - x(i-1)
        dxr = x(i+1) - x(i)

        alpha =  2.0 *   dxr         / dxl / ( dxl + dxr ) / dxr
        beta = - 2.0 * ( dxr + dxl ) / dxl / ( dxl + dxr ) / dxr
        gamma =  2.0 *         dxl   / dxl / ( dxl + dxr ) / dxr

        lu(i) = alpha * u(i-1) + beta * u(i) + gamma * u(i+1)

      end do

      if ( bc(2) .eq. 0 ) then
        lu(n) = u(n)
      else if ( bc(2) .eq. 1 ) then
        lu(n) = ( u(n) - u(n-1) ) / ( x(n) - x(n-1) )
      else if ( bc(2) .eq. 2 ) then
        dxl = x(n) - x(n-1)
        dxr = x(2) - x(1)
        alpha =  2.0 *   dxr         / dxl / ( dxl + dxr ) / dxr
        beta = - 2.0 * ( dxr + dxl ) / dxl / ( dxl + dxr ) / dxr
        gamma =  2.0 *         dxl   / dxl / ( dxl + dxr ) / dxr
        lu(n) = alpha * u(n-1) + beta * u(n) + gamma * u(2)
      end if

      return
      end
      subroutine lu_error ( n, a, l, u, error_frobenius )

c*********************************************************************72
c
cc LU_ERROR determines the error in an LU factorization.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix.
c
c    Input, double precision L(N,N), U(N,N), the LU factors.
c
c    Output, double precision ERROR_FROBENIUS, the Frobenius norm
c    of the difference matrix A -L * U.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision error_frobenius
      integer i
      integer j
      double precision l(n,n)
      double precision lu(n,n)
      double precision u(n,n)

      call r8mat_mm ( n, n, n, l, u, lu )

      error_frobenius = 0.0D+00
      do j = 1, n
        do i = 1, n
          error_frobenius = error_frobenius + ( a(i,j) - lu(i,j) )**2
        end do
      end do
      error_frobenius = sqrt ( error_frobenius )

      return
      end
      subroutine r8mat_mm ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MM multiplies two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N2,N3), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n2,n3)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do

      do j = 1, n3
        do i = 1, n1
          c(i,j) = c1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_mtm ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MTM multiplies computes C = A' * B for two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = matmul ( transpose ( A(1:N2,1:N1) ), B(1:N2,1:N3) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N2,N1), B(N2,N3), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A' * B.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n2,n1)
      double precision b(n2,n3)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(k,i) * b(k,j)
          end do
        end do
      end do

      do j = 1, n3
        do i = 1, n1
          c(i,j) = c1(i,j)
        end do
      end do

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
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
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
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

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
