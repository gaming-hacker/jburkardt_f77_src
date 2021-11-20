      subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, 
     &  rot_num )

c*********************************************************************72
c
cc JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
c
c  Discussion:
c
c    This function computes the eigenvalues and eigenvectors of a
c    real symmetric matrix, using Rutishauser's modfications of the classical
c    Jacobi rotation method with threshold pivoting.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, Charles VanLoan,
c    Matrix Computations,
c    Third Edition,
c    Johns Hopkins, 1996,
c    ISBN: 0-8018-4513-X,
c    LC: QA188.G65.
c
c  Input:
c
c    integer N, the order of the matrix.
c
c    double precision A(N,N), the matrix, which must be square, real,
c    and symmetric.
c
c    integer IT_MAX, the maximum number of iterations.
c
c  Output:
c
c    double precision V(N,N), the matrix of eigenvectors.
c
c    double precision D(N), the eigenvalues, in descending order.
c
c    integer IT_NUM, the total number of iterations.
c
c    integer ROT_NUM, the total number of rotations.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision bw(n)
      double precision c
      double precision d(n)
      double precision g
      double precision gapq
      double precision h
      integer i
      integer it_max
      integer it_num
      integer j
      integer k
      integer l
      integer m
      integer p
      integer q
      integer rot_num
      double precision s
      double precision t
      double precision tau
      double precision term
      double precision termp
      double precision termq
      double precision theta
      double precision thresh
      double precision v(n,n)
      double precision w(n)
      double precision zw(n)

      do j = 1, n 
        do i = 1, n 
          v(i,j) = 0.0D+00
        end do
        v(j,j) = 1.0D+00
      end do

      do i = 1, n
        d(i) = a(i,i)
      end do

      do i = 1, n
        bw(i) = d(i)
        zw(i) = 0.0D+00
      end do

      it_num = 0
      rot_num = 0

10    continue

      if ( it_num .lt. it_max ) then

        it_num = it_num + 1
c
c  The convergence threshold is based on the size of the elements in
c  the strict upper triangle of the matrix.
c
        thresh = 0.0D+00
        do j = 1, n
          do i = 1, j - 1
            thresh = thresh + a(i,j) ** 2
          end do
        end do

        thresh = sqrt ( thresh ) / dble ( 4 * n )

        if ( thresh .eq. 0.0D+00 ) then
          go to 20
        end if

        do p = 1, n
          do q = p + 1, n

            gapq = 10.0D+00 * abs ( a(p,q) )
            termp = gapq + abs ( d(p) )
            termq = gapq + abs ( d(q) )
c
c  Annihilate tiny offdiagonal elements.
c
            if ( 4 .lt. it_num .and.
     &           termp .eq. abs ( d(p) ) .and.
     &           termq .eq. abs ( d(q) ) ) then

              a(p,q) = 0.0D+00
c
c  Otherwise, apply a rotation.
c
            else if ( thresh .le. abs ( a(p,q) ) ) then

              h = d(q) - d(p)
              term = abs ( h ) + gapq

              if ( term .eq. abs ( h ) ) then
                t = a(p,q) / h
              else
                theta = 0.5D+00 * h / a(p,q)
                t = 1.0D+00 / 
     &            ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
                if ( theta .lt. 0.0D+00 ) then
                  t = - t
                end if
              end if

              c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
              s = t * c
              tau = s / ( 1.0D+00 + c )
              h = t * a(p,q)
c
c  Accumulate corrections to diagonal elements.
c
              zw(p) = zw(p) - h
              zw(q) = zw(q) + h
              d(p) = d(p) - h
              d(q) = d(q) + h

              a(p,q) = 0.0D+00
c
c  Rotate, using information from the upper triangle of A only.
c
              do j = 1, p - 1
                g = a(j,p)
                h = a(j,q)
                a(j,p) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = p + 1, q - 1
                g = a(p,j)
                h = a(j,q)
                a(p,j) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = q + 1, n
                g = a(p,j)
                h = a(q,j)
                a(p,j) = g - s * ( h + g * tau )
                a(q,j) = h + s * ( g - h * tau )
              end do
c
c  Accumulate information in the eigenvector matrix.
c
              do j = 1, n
                g = v(j,p)
                h = v(j,q)
                v(j,p) = g - s * ( h + g * tau )
                v(j,q) = h + s * ( g - h * tau )
              end do

              rot_num = rot_num + 1

            end if

          end do
        end do

        do i = 1, n
          bw(i) = bw(i) + zw(i)
          d(i) = bw(i)
          zw(i) = 0.0D+00
        end do

        go to 10

      end if

20    continue
c
c  Restore upper triangle of input matrix.
c
      do j = 1, n
        do i = 1, j - 1
          a(i,j) = a(j,i)
        end do
      end do
c
c  Ascending sort the eigenvalues and eigenvectors.
c
      do k = 1, n - 1

        m = k

        do l = k + 1, n
          if ( d(l) .lt. d(m) ) then
            m = l
          end if
        end do

        if ( m .ne. k ) then

          t    = d(m)
          d(m) = d(k)
          d(k) = t

          do i = 1, n
            w(i)   = v(i,m)
            v(i,m) = v(i,k)
            v(i,k) = w(i)
          end do

        end if

      end do

      return
      end
      subroutine r8mat_diag_get_vector ( n, a, v )

c*********************************************************************72
c
cc R8MAT_DIAG_GET_VECTOR gets the value of the diagonal of an R8MAT.
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
c    02 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    integer N, the number of rows and columns of
c    the matrix.
c
c    double precision A(N,N), the N by N matrix.
c
c  Output:
c
c    double precision V(N), the diagonal entries
c    of the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      double precision v(n)

      do i = 1, n
        v(i) = a(i,i)
      end do

      return
      end
      subroutine r8mat_identity ( n, a )

c*********************************************************************72
c
cc R8MAT_IDENTITY stores the identity matrix in an R8MAT.
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
c    24 March 2000
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    integer N, the order of A.
c
c  Output:
c
c    double precision A(N,N), the N by N identity matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        a(i,i) = 1.0D+00
      end do

      return
      end
      subroutine r8mat_is_eigen_right ( n, k, a, x, lambda, 
     &  error_frobenius )

c*********************************************************************72
c
cc R8MAT_IS_EIGEN_RIGHT determines the error in a (right) eigensystem.
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
c    09 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    integer N, the order of the matrix.
c
c    integer K, the number of eigenvectors.
c    K is usually 1 or N.
c
c    double precision A(N,N), the matrix.
c
c    double precision X(N,K), the K eigenvectors.
c
c    double precision LAMBDA(K), the K eigenvalues.
c
c  Output:
c
c    double precision ERROR_FROBENIUS, the Frobenius norm
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
      double precision r8mat_norm_fro
      double precision x(n,k)

      call r8mat_mm ( n, n, n, a, x, c )
 
      do j = 1, k
        do i = 1, n
          c(i,j) = c(i,j) - lambda(j) * x(i,j)
        end do
      end do

      error_frobenius = r8mat_norm_fro ( n, k, c )

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
c  Input:
c
c    integer N1, N2, N3, the order of the matrices.
c
c    double precision A(N1,N2), B(N2,N3), the matrices to multiply.
c
c  Output:
c
c    double precision C(N1,N3), the product matrix C = A * B.
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
      function r8mat_norm_fro ( m, n, a )

c*********************************************************************72
c
cc R8MAT_NORM_FRO returns the Frobenius norm of an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    The Frobenius norm is defined as
c
c      R8MAT_NORM_FRO = sqrt (
c        sum ( 1 .le. I .le. M ) sum ( 1 .le. j .le. N ) A(I,J)^2 )
c
c    The matrix Frobenius norm is not derived from a vector norm, but
c    is compatible with the vector L2 norm, so that:
c
c      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Input:
c
c    integer M, the number of rows in A.
c
c    integer N, the number of columns in A.
c
c    double precision A(M,N), the matrix whose Frobenius
c    norm is desired.
c
c  Output:
c
c    double precision R8MAT_NORM_FRO, the Frobenius norm of A.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision r8mat_norm_fro
      double precision value

      value = 0.0D+00
      do j = 1, n
        do i = 1, m
          value = value + a(i,j) * a(i,j)
        end do
      end do
      value = sqrt ( value )

      r8mat_norm_fro = value

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
c  Input:
c
c    integer M, the number of rows in A.
c
c    integer N, the number of columns in A.
c
c    double precision A(M,N), the matrix.
c
c    character ( len = * ) TITLE, a title.
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
c  Input:
c
c    integer M, N, the number of rows and columns.
c
c    double precision A(M,N), an M by N matrix to be printed.
c
c    integer ILO, JLO, the first row and column to print.
c
c    integer IHI, JHI, the last row and column to print.
c
c    character ( len = * ) TITLE, a title.
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
c  Input:
c
c    integer N, the number of components of the vector.
c
c    double precision A(N), the vector to be printed.
c
c    character * ( * ) TITLE, a title.
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
