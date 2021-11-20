      subroutine differ_backward ( h, o, p, c, x )

c*********************************************************************72
c
cc DIFFER_BACKWARD computes backward difference coefficients.
c
c  Discussion:
c
c    We determine coefficients C to approximate the derivative at X0
c    of order O and precision P, using equally spaced backward
c    differences, so that 
c
c      d^o f(x)/dx^o = sum ( 0 <= i <= o+p-1 ) c(i) f(x-ih) + O(h^(p))
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision H, the spacing.  0 < H.
c
c    Input, integer O, the order of the derivative to be 
c    approximated.  1 <= O.
c
c    Input, integer P, the order of the error, as a power of H.
c
c    Output, double precision C(O+P), the coefficients.
c
c    Output, double precision X(O+P), the evaluation points.
c
      implicit none

      integer o
      integer p

      double precision b(o+p)
      double precision c(o+p)
      double precision h
      integer i
      integer info
      integer job
      integer n
      double precision r8_factorial
      double precision t
      double precision x(o+p)

      n = o + p

      do i = 1, n
        x(i) = - dble ( n - i ) * h
      end do

      do i = 1, n
        b(i) = 0.0D+00
      end do
      b(o+1) = 1.0D+00

      job = 0
      call r8vm_sl ( n, x, b, c, job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIFFER_BACKWARD - Fatal error!'
        write ( *, '(a)' ) '  Vandermonde linear system is singular.'
        stop 1
      end if

      t = r8_factorial ( o )
      do i = 1, n
        c(i) = c(i) * t
      end do

      return
      end
      subroutine differ_central ( h, o, p, c, x )

c*********************************************************************72
c
cc DIFFER_CENTRAL computes central difference coefficients.
c
c  Discussion:
c
c    We determine coefficients C to approximate the derivative at X0
c    of order O and precision P, using equally spaced central
c    differences, so that 
c
c      d^o f(x)/dx^o = sum ( 0 <= i <= o+p-1 ) c(i) f(x+(2*i-o-p+1)*h/2) 
c        + O(h^(p))
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision H, the spacing.  0 < H.
c
c    Input, integer O, the order of the derivative to 
c    be approximated.  1 <= O.
c
c    Input, integer P, the order of the error, as a power of H.
c
c    Output, double precision C(O+P), the coefficients.
c
c    Output, double precision X(O+P), the evaluation points.
c
      implicit none

      integer o
      integer p

      double precision b(o+p)
      double precision c(o+p)
      double precision h
      integer i
      integer info
      integer job
      integer n
      double precision r8_factorial
      double precision t
      double precision x(o+p)

      n = o + p

      do i = 1, n
        x(i) = dble ( - n - 1 + 2 * i ) * h / 2.0D+00
      end do

      do i = 1, n
        b(i) = 0.0D+00
      end do
      b(o+1) = 1.0D+00

      job = 0
      call r8vm_sl ( n, x, b, c, job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIFFER_CENTRAL - Fatal error!'
        write ( *, '(a)' ) '  Vandermonde linear system is singular.'
        stop 1
      end if

      t = r8_factorial ( o )
      do i = 1, n
        c(i) = c(i) * t
      end do

      return
      end
      subroutine differ_forward ( h, o, p, c, x )

c*********************************************************************72
c
cc DIFFER_FORWARD computes forward difference coefficients.
c
c  Discussion:
c
c    We determine coefficients C to approximate the derivative at X0
c    of order O and precision P, using equally spaced forward
c    differences, so that 
c
c      d^o f(x)/dx^o = sum ( 0 <= i <= o+p-1 ) c(i) f(x+ih) + O(h^(p))
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real H, the spacing.  0 < H.
c
c    Input, integer O, the order of the derivative to be approximated.
c    1 <= O.
c
c    Input, integer P, the order of the error, as a power of H.
c
c    Output, real C(O+P), the coefficients.
c
c    Output, real X(O+P), the evaluation points.
c
      implicit none

      integer o
      integer p

      double precision b(o+p)
      double precision c(o+p)
      double precision h
      integer i
      integer info
      integer job
      integer n
      double precision r8_factorial
      double precision t
      double precision x(o+p)

      n = o + p

      do i = 1, n
        x(i) = dble ( i - 1 ) * h
      end do

      do i = 1, n
        b(i) = 0.0D+00
      end do
      b(o+1) = 1.0D+00

      job = 0
      call r8vm_sl ( n, x, b, c, job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIFFER_FORWARD - Fatal error!'
        write ( *, '(a)' ) '  Vandermonde linear system is singular.'
        stop 1
      end if

      t = r8_factorial ( o )
      do i = 1, n
        c(i) = c(i) * t
      end do

      return
      end
      subroutine differ_inverse ( n, stencil, a )

c*********************************************************************72
c
cc DIFFER_INVERSE returns the inverse of the DIFFER matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision STENCIL(N), the values that define A.
c    The entries of STENCIL must be distinct.
c    No entry of STENCIL may be 0.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer indx
      integer j
      integer k
      double precision stencil(n)

      do j = 1, n
        do i = 1, n
          if ( j .eq. 1 ) then
            a(i,j) = 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      do i = 1, n

        indx = 0

        do k = 1, n

          if ( k .ne. i ) then

            indx = indx + 1

            do j = indx + 1, 1, -1

              a(i,j) = - stencil(k) * a(i,j) 
     &          / ( stencil(i) - stencil(k) )

              if ( 1 .lt. j ) then
                a(i,j) = a(i,j) + a(i,j-1) 
     &            / ( stencil(i) - stencil(k) )
              end if

            end do

          end if

        end do

      end do

      do j = 1, n
        do i = 1, n
          a(i,j) = a(i,j) / stencil(i)
        end do
      end do

      return
      end
      subroutine differ_matrix ( n, stencil, a )

c*********************************************************************72
c
cc DIFFER_MATRIX computes the stencil matrix from the stencil vector.
c
c  Discussion:
c
c    If N = 4, and STENCIL = ( -3, -2, -1, 1 ), then A will be
c
c    -3  -2  -1  1
c     9   4   1  1
c   -27  -8  -1  1
c    81  16   1  1
c
c    This matrix is a generalized form of a Vandermonde matrix A2:
c
c     1   1   1  1
c    -3  -2  -1  1
c     9   4   1  1
c   -27  -8  -1  1    
c
c    and if A * x = b, the A2 * x2 = b, where x2(i) = x(i) * stencil(i)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of stencil points.
c
c    Input, double precision STENCIL(N), the stencil vector.
c    The entries in this vector must be distinct.
c    No entry of STENCIL may be 0.
c
c    Output, double precision A(N,N), the stencil matrix.
c
      implicit none

      integer n
  
      double precision a(n,n)
      integer i
      integer j
      double precision stencil(n)

      do j = 1, n
       a(1,j) = stencil(j)
        do i = 2, n
          a(i,j) = a(i-1,j) * stencil(j)
        end do
      end do

      return
      end
      subroutine differ_solve ( n, stencil, order, c )

c*********************************************************************72
c
cc DIFFER_SOLVE solves for finite difference coefficients.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of stencil points.
c
c    Input, double precision STENCIL(N), the stencil vector.
c    The entries in this vector must be distinct.
c    No entry of STENCIL may be 0.
c
c    Input, integer ORDER, the order of the derivative to be approximated.
c    1 <= ORDER <= N.
c
c    Output, double precision C(N), the coefficients to be used
c    to multiply U(STENCIL(I))-U(0), so that the sum forms an
c    approximation to the derivative of order ORDER, with error 
c    of order H^(N+1-ORDER).
c
      implicit none

      integer n
  
      double precision a(n,n)
      double precision b(n)
      double precision c(n)
      integer i
      integer info
      integer order
      double precision stencil(n)

      call differ_matrix ( n, stencil, a )

      do i = 1, n
        b(i) = 0.0D+00
      end do
      b(order) = 1.0D+00
c
c  Solve A * C = B.
c
      do i = 1, n
        c(i) = b(i)
      end do

      call r8mat_fs ( n, a, c, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIFFER_SOLVE - Fatal errorc'
        write ( *, '(a)' ) '  DIFFER system is singular.'
        stop
      end if

      return
      end
      subroutine differ_stencil ( x0, o, p, x, c )

c*********************************************************************72
c
cc DIFFER_STENCIL computes finite difference coefficients.
c
c  Discussion:
c
c    We determine coefficients C to approximate the derivative at X0
c    of order O and precision P, using finite differences, so that 
c
c      d^o f(x)/dx^o (x0) = sum ( 0 <= i <= o+p-1 ) c(i) f(x(i)) 
c        + O(h^(p))
c
c    where H is the maximum spacing between X0 and any X(I).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X0, the point where the derivative is to 
c    be approximated.
c
c    Input, integer O, the order of the derivative to be 
c    approximated.  1 <= O.
c
c    Input, integer P, the order of the error, as a power of H.
c
c    Input, double precision X(O+P), the evaluation points.
c
c    Output, double precision C(O+P), the coefficients.
c
      implicit none

      integer o
      integer p

      double precision b(o+p)
      double precision c(o+p)
      double precision dx(o+p)
      integer i
      integer info
      integer job
      integer n
      double precision r8_factorial
      double precision t
      double precision x(o+p)
      double precision x0

      n = o + p

      do i = 1, n
        dx(i) = x(i) - x0
      end do

      do i = 1, n
        b(i) = 0.0D+00
      end do
      b(o+1) = 1.0D+00

      job = 0
      call r8vm_sl ( n, dx, b, c, job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIFFER_STENCIL - Fatal error!'
        write ( *, '(a)' ) '  Vandermonde linear system is singular.'
        stop 1
      end if

      t = r8_factorial ( o )
      do i = 1, n
        c(i) = c(i) * t
      end do

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
      function r8_factorial ( n )

c*********************************************************************72
c
cc R8_FACTORIAL computes the factorial of N.
c
c  Discussion:
c
c    factorial ( N ) = product ( 1 <= I <= N ) I
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the factorial function.
c    If N is less than 1, the function value is returned as 1.
c
c    Output, double precision R8_FACTORIAL, the factorial of N.
c
      implicit none

      integer i
      integer n
      double precision r8_factorial

      r8_factorial = 1.0D+00

      do i = 1, n
        r8_factorial = r8_factorial * dble ( i )
      end do

      return
      end
      subroutine r8mat_fs ( n, a, b, info )

c*********************************************************************72
c
cc R8MAT_FS factors and solves a system with one right hand side.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c    This routine differs from R8MAT_FSS in two ways:
c    * only one right hand side is allowed;
c    * the input matrix A is not modified.
c
c    This routine uses partial pivoting, but no pivot vector is required.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A(N,N), the coefficient matrix.
c
c    Input/output, double precision B(N).
c    On input, B is the right hand side of the linear system.
c    On output, B is the solution of the linear system.
c
c    Output, integer INFO, singularity flag.
c    0, no singularity detected.
c    nonzero, the factorization failed on the INFO-th step.
c
      implicit none

      integer n
      integer nb

      double precision a(n,n)
      double precision a2(n,n)
      double precision b(n)
      integer i
      integer info
      integer ipiv
      integer j
      integer jcol
      integer k
      double precision piv
      double precision temp
c
c  Copy the matrix.
c
      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      info = 0

      do jcol = 1, n
c
c  Find the maximum element in column I.
c
        piv = abs ( a2(jcol,jcol) )
        ipiv = jcol
        do i = jcol + 1, n
          if ( piv .lt. abs ( a2(i,jcol) ) ) then
            piv = abs ( a2(i,jcol) )
            ipiv = i
          end if
        end do

        if ( piv .eq. 0.0D+00 ) then
          info = jcol
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8MAT_FS - Fatal error!'
          write ( *, '(a,i8)' ) '  Zero pivot on step ', info
          return
        end if
c
c  Switch rows JCOL and IPIV, and B.
c
        if ( jcol .ne. ipiv ) then

          do j = 1, n
            temp = a2(jcol,j)
            a2(jcol,j) = a2(ipiv,j)
            a2(ipiv,j) = temp
          end do

          temp = b(jcol)
          b(jcol) = b(ipiv)
          b(ipiv) = temp

        end if
c
c  Scale the pivot row.
c
        do j = jcol + 1, n
          a2(jcol,j) = a2(jcol,j) / a2(jcol,jcol)
        end do
        b(jcol) = b(jcol) / a2(jcol,jcol)
        a2(jcol,jcol) = 1.0D+00
c
c  Use the pivot row to eliminate lower entries in that column.
c
        do i = jcol + 1, n
          if ( a2(i,jcol) .ne. 0.0D+00 ) then
            temp = - a2(i,jcol)
            a2(i,jcol) = 0.0D+00
            do j = jcol + 1, n
              a2(i,j) = a2(i,j) + temp * a2(jcol,j)
            end do
            b(i) = b(i) + temp * b(jcol)
          end if
        end do

      end do
c
c  Back solve.
c
      do k = n, 2, -1
        do i = 1, k - 1
          b(i) = b(i) - a2(i,k) * b(k)
        end do
      end do

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
      subroutine r8mat_mv ( m, n, a, x, y )

c*********************************************************************72
c
cc R8MAT_MV multiplies a matrix times a vector.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    In FORTRAN90, this operation can be more efficiently carried
c    out by the command
c
c      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision Y(M), the product A*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(n)
      double precision y(m)
      double precision y1(m)

      do i = 1, m
        y1(i) = 0.0D+00
        do j = 1, n
          y1(i) = y1(i) + a(i,j) * x(j)
        end do
      end do

      do i = 1, m
        y(i) = y1(i)
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
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
c    17 July 2006
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
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine r8vec2_print ( n, a1, a2, title )

c*********************************************************************72
c
cc R8VEC2_PRINT prints an R8VEC2.
c
c  Discussion:
c
c    An R8VEC2 is a dataset consisting of N pairs of R8s, stored
c    as two separate vectors A1 and A2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A1(N), A2(N), the vectors to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a1(i), a2(i)
      end do

      return
      end
      subroutine r8vm_sl ( n, a, b, x, job, info )

c*********************************************************************72
c
cc R8VM_SL solves an R8VM linear system.
c
c  Discussion:
c
c    The R8VM storage format is used for an M by N Vandermonde matrix.
c    An M by N Vandermonde matrix is defined by the values in its second
c    row, which will be written here as X(1:N).  The matrix has a first 
c    row of 1's, a second row equal to X(1:N), a third row whose entries
c    are the squares of the X values, up to the M-th row whose entries
c    are the (M-1)th powers of the X values.  The matrix can be stored
c    compactly by listing just the values X(1:N).
c
c    Vandermonde systems are very close to singularity.  The singularity
c    gets worse as N increases, and as any pair of values defining
c    the matrix get close.  Even a system as small as N = 10 will
c    involve the 9th power of the defining values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 September 2003
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Gene Golub, Charles Van Loan,
c    Matrix Computations,
c    Third Edition,
c    Johns Hopkins, 1996.
c
c  Parameters:
c
c    Input, integer N, the number of rows and columns of 
c    the matrix.
c
c    Input, double precision A(N), the R8VM matrix.
c
c    Input, double precision B(N), the right hand side.
c
c    Output, double precision X(N), the solution of the linear system.
c
c    Input, integer JOB, specifies the system to solve.
c    0, solve A * x = b.
c    nonzero, solve A' * x = b.
c
c    Output, integer INFO.
c    0, no error.
c    nonzero, at least two of the values in A are equal.
c
      implicit none

      integer n

      double precision a(n)
      double precision b(n)
      integer i
      integer info
      integer j
      integer job
      double precision x(n)
c
c  Check for explicit singularity.
c
      info = 0

      do j = 1, n - 1
        do i = j + 1, n
          if ( a(i) .eq. a(j) ) then
            info = 1
            return
          end if
        end do
      end do

      do i = 1, n
        x(i) = b(i)
      end do

      if ( job .eq. 0 ) then

        do j = 1, n - 1
          do i = n, j + 1, -1
            x(i) = x(i) - a(j) * x(i-1)
          end do
        end do

        do j = n - 1, 1, -1

          do i = j+1, n
            x(i) = x(i) / ( a(i) - a(i-j) )
          end do

          do i = j, n - 1
            x(i) = x(i) - x(i+1)
          end do

        end do

      else

        do j = 1, n - 1
          do i = n, j + 1, -1
            x(i) = ( x(i) - x(i-1) ) / ( a(i) - a(i-j) )
          end do
        end do

        do j = n - 1, 1, -1
          do i = j, n - 1
            x(i) = x(i) - x(i+1) * a(j)
          end do
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

