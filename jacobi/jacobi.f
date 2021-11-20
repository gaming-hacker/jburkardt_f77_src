      subroutine dif2 ( m, n, a )

c*********************************************************************72
c
cc DIF2 returns the DIF2 matrix.
c
c  Example:
c
c    N = 5
c
c    2 -1  .  .  .
c   -1  2 -1  .  .
c    . -1  2 -1  .
c    .  . -1  2 -1
c    .  .  . -1  2
c
c  Properties:
c
c    A is banded, with bandwidth 3.
c
c    A is tridiagonal.
c
c    Because A is tridiagonal, it has property A (bipartite).
c
c    A is a special case of the TRIS or tridiagonal scalar matrix.
c
c    A is integral, therefore det ( A ) is integral, and 
c    det ( A ) * inverse ( A ) is integral.
c
c    A is Toeplitz: constant along diagonals.
c
c    A is symmetric: A' = A.
c
c    Because A is symmetric, it is normal.
c
c    Because A is normal, it is diagonalizable.
c
c    A is persymmetric: A(I,J) = A(N+1-J,N+1-I).
c
c    A is positive definite.
c
c    A is an M matrix.
c
c    A is weakly diagonally dominant, but not strictly diagonally dominant.
c
c    A has an LU factorization A = L * U, without pivoting.
c
c      The matrix L is lower bidiagonal with subdiagonal elements:
c
c        L(I+1,I) = -I/(I+1)
c
c      The matrix U is upper bidiagonal, with diagonal elements
c
c        U(I,I) = (I+1)/I
c
c      and superdiagonal elements which are all -1.
c
c    A has a Cholesky factorization A = L * L', with L lower bidiagonal.
c
c      L(I,I) =    sqrt ( (I+1) / I )
c      L(I,I-1) = -sqrt ( (I-1) / I )
c
c    The eigenvalues are
c
c      LAMBDA(I) = 2 + 2 * COS(I*PI/(N+1))
c                = 4 SIN^2(I*PI/(2*N+2))
c
c    The corresponding eigenvector X(I) has entries
c
c       X(I)(J) = sqrt(2/(N+1)) * sin ( I*J*PI/(N+1) ).
c
c    Simple linear systems:
c
c      x = (1,1,1,...,1,1),   A*x=(1,0,0,...,0,1)
c
c      x = (1,2,3,...,n-1,n), A*x=(0,0,0,...,0,n+1)
c
c    det ( A ) = N + 1.
c
c    The value of the determinant can be seen by induction,
c    and expanding the determinant across the first row:
c
c      det ( A(N) ) = 2 * det ( A(N-1) ) - (-1) * (-1) * det ( A(N-2) )
c                = 2 * N - (N-1)
c                = N + 1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Gregory, David Karney,
c    A Collection of Matrices for Testing Computational Algorithms,
c    Wiley, 1969,
c    ISBN: 0882756494,
c    LC: QA263.68
c
c    Morris Newman, John Todd,
c    Example A8,
c    The evaluation of matrix inversion programs,
c    Journal of the Society for Industrial and Applied Mathematics,
c    Volume 6, Number 4, pages 466-476, 1958.
c
c    John Todd,
c    Basic Numerical Mathematics,
c    Volume 2: Numerical Algebra,
c    Birkhauser, 1980,
c    ISBN: 0817608117,
c    LC: QA297.T58.
c
c    Joan Westlake,
c    A Handbook of Numerical Matrix Inversion and Solution of 
c    Linear Equations,
c    John Wiley, 1968,
c    ISBN13: 978-0471936756,
c    LC: QA263.W47.
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Output, double precision A(M,N), the matrix.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m

          if ( j .eq. i - 1 ) then
            a(i,j) = -1.0D+00
          else if ( j .eq. i ) then
            a(i,j) = 2.0D+00
          else if ( j .eq. i + 1 ) then
            a(i,j) = -1.0D+00
          else
            a(i,j) = 0.0D+00
          end if

        end do
      end do

      return
      end
      subroutine jacobi1 ( n, a, b, x, x_new )

c*********************************************************************72
c
cc JACOBI1 carries out one step of the Jacobi iteration.
c
c  Discussion:
c
c    The linear system A*x=b is to be solved.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
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
c    Input, double precision B(N), the right hand side.
c
c    Input, double precision X(N), the current solution estimate.
c
c    Output, double precision X_NEW(N), the solution estimate updated by
c    one step of the Jacobi iteration.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision b(n)
      integer i
      integer j
      double precision x(n)
      double precision x_new(n)

      do i = 1, n
        x_new(i) = b(i) 
        do j = 1, n
          if ( j .ne. i ) then
           x_new(i) = x_new(i) - a(i,j) * x(j)
          end if
        end do
        x_new(i) = x_new(i) / a(i,i)
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
