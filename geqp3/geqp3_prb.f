      program main

c*********************************************************************72
c
cc MAIN is the main program for GEQP3_PRB.
c
c  Discussion:
c
c    GEQP3_PRB tests the GEQP3 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEQP3_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Test the GEQP3 library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEQP3_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests DGEQP3 for QR factorization of a double precision real matrix.
c
c  Discussion:
c
c    DGEQP3 requires that N <= M.
c
c    This example uses a 6x5 linear system.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mmax
      parameter ( mmax = 8 )
      integer nb
      parameter ( nb = 64 )
      integer nmax
      parameter ( nmax = 8 )
      integer nrhsmx
      parameter ( nrhsmx = 2 )

      integer lda
      parameter ( lda = mmax )
      integer ldb
      parameter ( ldb = mmax )
      integer lwork
      parameter ( lwork = 2 * nmax + ( nmax + 1 ) * nb )

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 5 )
      integer nrhs
      parameter ( nrhs = 2 )

      double precision a(lda,nmax)
      double precision a_data(m,n)
      double precision alpha
      double precision b(ldb,nrhsmx)
      double precision b_data(m,nrhs)
      double precision dnrm2
      integer i
      integer info
      integer j
      integer jpvt(nmax)
      integer k
      double precision r(m,nrhs)
      integer rank
      double precision rnorm(nmax)
      double precision tau(nmax)
      double precision tol
      parameter ( tol = 0.01D+00 )
      double precision work(lwork)
      double precision x(n,nrhs)

      save a_data
      save b_data

      data a_data /
     & -0.09D+00, -1.56D+00, -1.48D+00, -1.09D+00,  0.08D+00, -1.59D+00,
     &  0.14D+00,  0.20D+00, -0.43D+00,  0.84D+00,  0.55D+00, -0.72D+00,
     & -0.46D+00,  0.29D+00,  0.89D+00,  0.77D+00, -1.13D+00,  1.06D+00,
     &  0.68D+00,  1.09D+00, -0.71D+00,  2.11D+00,  0.14D+00,  1.24D+00,
     &  1.29D+00,  0.51D+00, -0.96D+00, -1.27D+00,  1.74D+00,  0.34D+00
     &  /

      data b_data /
     &  7.4D+00,  4.2D+00, -8.3D+00, 1.8D+00, 8.6D+00,  2.1D+00,
     &  2.7D+00, -3.0D+00, -9.6D+00, 1.1D+00, 4.0D+00, -5.7D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  DGEQP3 computes the QR factorization,'
      write ( *, '(a)' ) '  with column pivoting,'
      write ( *, '(a)' ) '  of a M by N rectangular matrix A = Q * R,'
      write ( *, '(a)' ) '  with N <= M,'
      write ( *, '(a)' ) '  using double precision real arithmetic.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Least squares solutions of A*X=B can then'
      write ( *, '(a)' ) '  be computed by calling:'
      write ( *, '(a)' ) '  DORMQR to compute QTB = Q'' * B, and'
      write ( *, '(a)' ) '  DTRSM to solve R * X = QTB.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  M = ', m
      write ( *, '(a,i4)' ) '  N = ', n
      write ( *, '(a,i4)' ) '  NRHS = ', nrhs
      write ( *, '(a,g14.6)' ) '  TOL = ', tol
c
c  Transfer the packed arrays A_DATA and B_DATA into the loose arrays A and B.
c
      call r8mat_to_r8cmat ( lda, m, n,    a_data, a )
      call r8cmat_print ( lda, m, n, a, '  The R8CMAT A:' )

      call r8mat_to_r8cmat ( ldb, m, nrhs, b_data, b )
      call r8cmat_print ( ldb, m, nrhs, b, '  The R8CMAT B:' )
c
c  Set JPVT zero, which means that all columns are free.
c
      call i4vec_zero ( n, jpvt )
c
c  Compute the QR factorization of A.
c
      call dgeqp3 ( m, n, a, lda, jpvt, tau, work, lwork, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'GEQP3_PRB - Fatal error!'
        write ( *, '(a,i4)' ) '  DGEQP3 returned INFO = ', info
        stop 1
      end if
c
c  Determine and print the rank of R using the relative tolerance TOL.
c
      rank = min ( m, n )

      do k = 1, min ( m, n )
        if ( abs ( a(k,k) ) .le. tol * abs ( a(1,1) ) ) then
          rank = k - 1
          go to 10
        end if
      end do

  10  continue

      write ( *, '(a,i4)' ) '  Estimated rank of A = ', rank
c
c  Compute C = (C1) = Q' * B, storing the result in B
c              (C2)
c
c  Here:
c    Q is of order MxM,
c    B is of order MxNRHS,
c    C will be of order MxNRHS.
c
      call dormqr ( 'Left', 'Transpose', m, nrhs, n, a, lda, tau, b, 
     &  ldb, work, lwork, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'GEQP3_PRB - Fatal error!'
        write ( *, '(a,i4)' ) '  DORMQR returned INFO = ', info
        stop 1
      end if
c
c  Compute least-squares solutions by solving the system
c    R(1:K,1:K) * B = C1.
c  where
c    R is the leading KxK upper triangular submatrix in A,
c    C1 is a K by NRHS vector,
c    B is the K by NRHS solution.
c
      alpha = 1.0D+00
      k = rank

      call dtrsm ( 'Left', 'Upper', 'No transpose', 'Non-Unit', k, 
     &  nrhs, alpha, a, lda, b, ldb )
c
c  Estimate the square roots of the residual sums of squares 
c  (the 2-norm of each of the columns of C2)
c
      do j = 1, nrhs
        rnorm(j) = dnrm2 ( m - k, b(k+1:m,j), 1 )
      end do
c
c  Set the remaining elements of the solutions to zero (to give
c  the basic solutions.)
c
      do j = 1, nrhs
        do i = k + 1, n
          b(i,j) = 0.0D+00
        end do
      end do
c
c  Permute the least-squares solutions stored in B to give X = P*Y
c
      do j = 1, nrhs
        do i = 1, n
          work(jpvt(i)) = b(i,j)
        end do
        do i = 1, n
          b(i,j) = work(i)
        end do
      end do
c
c  Print the NRHS least-squares solution vectors of length N.
c
      call r8cmat_print ( ldb, n, nrhs, b, 
     &  '  Least-squares solutions:' )
c
c  Print the square roots of the residual sums of squares
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Square root of residual sums of squares:'
      write ( *, '(a)' ) ' '
      write ( *, '(6x,6e14.6)' ) ( rnorm(j), j = 1, nrhs )
!
!  Compute the NRHS residual vectors explicitly from the original data.
!
      call r8mat_mm ( m, n, nrhs, a_data, b(1:n,1:nrhs), r )

      do j = 1, nrhs
        do i = 1, m
          r(i,j) = r(i,j) - b_data(i,j)
        end do
      end do

      call r8cmat_print ( m, m, nrhs, r, '  Residuals:' )

      return
      end
      subroutine i4vec_zero ( n, a )

c*********************************************************************72
c
cc I4VEC_ZERO sets the entries of an I4VEC to 0.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Output, integer A(N), the vector, which has been set to zero.
c
      implicit none

      integer n

      integer a(n)
      integer i

      do i = 1, n
        a(i) = 0
      end do

      return
      end
      subroutine r8cmat_print ( lda, m, n, a, title )

c*********************************************************************72
c
cc R8CMAT_PRINT prints an R8CMAT.
c
c  Discussion:
c
c    An R8CMAT is an M by N array of R8's stored with leading dimension LD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer LDA, the leading dimension of A.
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(LDA,N), the M by N matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer lda
      integer m
      integer n

      double precision a(lda,n)
      character ( len = * ) title

      call r8cmat_print_some ( lda, m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8cmat_print_some ( lda, m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8CMAT_PRINT_SOME prints some of an R8CMAT.
c
c  Discussion:
c
c    An R8CMAT is an M by N array of R8's stored with leading dimension LD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer LDA, the leading dimension of A.
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(LDA,N), the M by N matrix.
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
      integer lda
      integer m
      integer n

      double precision a(lda,n)
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
      subroutine r8mat_to_r8cmat ( lda, m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_TO_R8CMAT transfers data from an R8MAT to an R8CMAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> (I+J*M).
c
c    An R8CMAT is an MxN array of R8's, stored with a leading dimension LD,
c    stored by (I,J) -> (I+J*LD).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer LDA, the leading dimension of A2.
c
c    Input, integer M, the number of rows of data.
c    M <= LDA.
c
c    Input, integer N, the number of columns of data.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(LDA,N), contains a copy of the
c    information in A1, in the MxN submatrix.
c
      implicit none

      integer lda
      integer m
      integer n

      double precision a1(m,n)
      double precision a2(lda,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
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
