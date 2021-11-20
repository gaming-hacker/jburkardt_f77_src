      subroutine bartlett_sample ( m, df, sigma, t )

c*********************************************************************72
c
cc BARTLETT_SAMPLE samples the Bartlett distribution.
c
c  Discussion:
c
c    If the matrix T is sampled from the Bartlett distribution, then 
c    the matrix W = T' * T is a sample from the Wishart distribution.
c
c    This function requires functions from the PDFLIB and RNGLIB libraries.
c
c    The "initialize()" function from RNGLIB must be called before using
c    this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Patrick Odell, Alan Feiveson,
c    A numerical procedure to generate a sample covariance matrix,
c    Journal of the American Statistical Association,
c    Volume 61, Number 313, March 1966, pages 199-203.
c
c    Stanley Sawyer,
c    Wishart Distributions and Inverse-Wishart Sampling,
c    Washington University,
c    30 April 2007, 12 pages.
c
c  Parameters:
c
c    Input, integer M, the order of the matrix.
c
c    Input, integer DF, the number of degrees of freedom.
c    M <= DF.
c
c    Input, double precision SIGMA(M,M), the covariance matrix, which should be 
c    a symmetric positive definite matrix.
c
c    Output, double precision T(M,M), the sample matrix from 
c    the Bartlett distribution.
c
      implicit none

      integer m

      integer df
      integer flag;
      double precision r(m,m)
      double precision sigma(m,m)
      double precision t(m,m)
      double precision tu(m,m)

      if ( df .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BARTLETT_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
        stop 1
      end if
c
c  Get the upper triangular Cholesky factor of SIGMA.
c
      call r8mat_cholesky_factor_upper ( m, sigma, r, flag )

      if ( flag .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'BARTLETT_SAMPLE - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Unexpected error return from R8MAT_CHOLESKY_FACTOR_UPPER.'
        write ( *, '(a,i4)' ) '  FLAG = ', flag
        stop 1
      end if
c
c  Sample the unit Bartlett distribution.
c
      call bartlett_unit_sample ( m, df, tu )
c
c  Construct the matrix T = TU * R.
c
      call r8mat_mm ( m, m, m, tu, r, t )

      return
      end
      subroutine bartlett_unit_sample ( m, df, t )

c*********************************************************************72
c
cc BARTLETT_UNIT_SAMPLE samples the unit Bartlett distribution.
c
c  Discussion:
c
c    If the matrix T is sampled from the unit Bartlett distribution, then 
c    the matrix W = T' * T is a sample from the unit Wishart distribution.
c 
c    This function requires functions from the PDFLIB and RNGLIB libraries.
c
c    The "initialize()" function from RNGLIB must be called before using
c    this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Patrick Odell, Alan Feiveson,
c    A numerical procedure to generate a sample covariance matrix,
c    Journal of the American Statistical Association,
c    Volume 61, Number 313, March 1966, pages 199-203.
c
c    Stanley Sawyer,
c    Wishart Distributions and Inverse-Wishart Sampling,
c    Washington University,
c    30 April 2007, 12 pages.
c
c  Parameters:
c
c    Input, integer M, the order of the matrix.
c
c    Input, integer DF, the number of degrees of freedom.
c    M <= DF.
c
c    Output, double precision T(M,M), the sample matrix from the 
c    unit Bartlett distribution.
c
      implicit none

      integer m

      integer df
      double precision df_chi
      integer i
      integer j
      double precision r8_chi_sample
      double precision r8_normal_01_sample
      double precision t(m,m)

      if ( df .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BARTLETT_UNIT_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
        stop 1
      end if

      do i = 1, m

        do j = 1, i - 1
          t(i,j) = 0.0D+00
        end do

        df_chi = dble ( df + 1 - i )
        t(i,i) = sqrt ( r8_chi_sample ( df_chi ) )

        do j = i + 1, m
          t(i,j) = r8_normal_01_sample ( )
        end do

      end do

      return
      end
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
c    15 July 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix, which must be square, real,
c    and symmetric.
c
c    Input, integer IT_MAX, the maximum number of iterations.
c
c    Output, double precision V(N,N), the matrix of eigenvectors.
c
c    Output, double precision D(N), the eigenvalues, in descending order.
c
c    Output, integer IT_NUM, the total number of iterations.
c
c    Output, integer ROT_NUM, the total number of rotations.
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

      call r8mat_identity ( n, v )

      call r8mat_diag_get_vector ( n, a, d )

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
c  Descending sort the eigenvalues and eigenvectors.
c
      do k = 1, n - 1

        m = k

        do l = k + 1, n
          if ( d(m) .lt. d(l) ) then
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
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      subroutine r8mat_cholesky_factor_upper ( n, a, c, flag )

!*********************************************************************72
!
!! R8MAT_CHOLESKY_FACTOR_UPPER: upper Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    The lower Cholesky factor is a lower triangular matrix L such that
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of rows and columns of
!    the matrix A.
!
!    Input, double precision A(N,N), the N by N matrix.
!
!    Output, double precision C(N,N), the N by N upper triangular
!    Cholesky factor.
!
!    Output, integer FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!    2, the matrix is not nonnegative definite.
!
      implicit none

      integer n

      double precision a(n,n)
      double precision c(n,n)
      integer flag
      integer i
      integer j
      integer k
      double precision sum2
      double precision tol

      flag = 0

      do j = 1, n
        do i = 1, n
          c(i,j) = a(i,j)
        end do
      end do

      do j = 1, n

        do i = 1, j - 1
          c(j,i) = 0.0D+00
       end do

        do i = j, n

          sum2 = c(i,j)
          do k = 1, j - 1
            sum2 = sum2 - c(k,j) * c(k,i)
          end do

          if ( i .eq. j ) then
            if ( sum2 .le. 0.0D+00 ) then
              flag = 1
              return
            else
              c(j,i) = sqrt ( sum2 )
            end if
          else
            if ( c(j,j) .ne. 0.0D+00 ) then
              c(j,i) = sum2 / c(j,j)
            else
              c(j,i) = 0.0D+00
            end if
          end if

        end do

      end do

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
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
c  Parameters:
c
c    Input, integer N, the number of rows and columns of
c    the matrix.
c
c    Input, double precision A(N,N), the N by N matrix.
c
c    Output, double precision V(N), the diagonal entries
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
      subroutine r8mat_diagonal ( n, diag, a )

c*********************************************************************72
c
cc R8MAT_DIAGONAL returns a diagonal matrix as an R8MAT.
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
c    31 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of A.
c
c    Input, double precision DIAG(N), the diagonal entries.
c
c    Output, double precision A(N,N), the N by N identity matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision diag(n)
      integer i
      integer j

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        a(i,i) = diag(i)
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
c  Parameters:
c
c    Input, integer N, the order of A.
c
c    Output, double precision A(N,N), the N by N identity matrix.
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
      subroutine r8mat_mmt ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MMT multiplies computes C = A * B' for two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = matmul ( A(1:N1,1;N2), transpose ( B(1:N3,1:N2) ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N3,N2), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B'.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n3,n2)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(j,k)
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

      call r8mat_copy ( n1, n3, c1, c )

      return
      end
      function r8mat_norm_fro_affine ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_NORM_FRO_AFFINE returns the Frobenius norm of an R8MAT difference.
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
c    26 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, double precision A1(M,N), A2(M,N), the matrices for which the
c    Frobenius norm of the difference is desired.
c
c    Output, double precision R8MAT_NORM_FRO_AFFINE, the Frobenius norm 
C    of A1 - A2.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j
      double precision r8mat_norm_fro_affine
      double precision value

      value = 0.0D+00
      do j = 1, n
        do i = 1, m
          value = value + ( a1(i,j) - a2(i,j) )**2 
        end do
      end do
      value = sqrt ( value )

      r8mat_norm_fro_affine = value

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
      subroutine r8ut_inverse ( n, a )

c*********************************************************************72
c
cc R8UT_INVERSE computes the inverse of an R8UT matrix.
c
c  Discussion:
c
c    The R8UT storage format is used for an M by N upper triangular 
c    matrix.  The format stores all M*N entries, even those which are zero.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    04 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input/output, double precision A(N,N).
c    On input, the upper triangular matrix to be inverted.
c    On output, the inverse of the upper triangular matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer j
c
c  Check.
c
      do i = 1, n
        if ( a(i,i) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8UT_INVERSE - Fatal error!'
          write ( *, '(a)' ) '  Zero diagonal element.'
          stop 1
        end if
      end do

      do j = n, 1, -1

        do i = n, 1, -1

          if ( j .lt. i ) then

            a(i,j) = 0.0D+00

          else if ( i .eq. j ) then

            a(i,j) = 1.0D+00 / a(i,j)

          else if ( i .lt. j ) then

            a(i,j) = - sum ( a(i,i+1:j) * a(i+1:j,j) ) / a(i,i)

          end if

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
      subroutine wishart_sample ( m, df, sigma, a )

c*********************************************************************72
c
cc WISHART_SAMPLE samples the Wishart distribution.
c
c  Discussion:
c
c    This function requires functions from the PDFLIB and RNGLIB libraries.
c
c    The "initialize()" function from RNGLIB must be called before using
c    this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Patrick Odell, Alan Feiveson,
c    A numerical procedure to generate a sample covariance matrix,
c    Journal of the American Statistical Association,
c    Volume 61, Number 313, March 1966, pages 199-203.
c
c    Stanley Sawyer,
c    Wishart Distributions and Inverse-Wishart Sampling,
c    Washington University,
c    30 April 2007, 12 pages.
c
c  Parameters:
c
c    Input, integer M, the order of the matrix.
c
c    Input, integer DF, the number of degrees of freedom.
c    M <= DF.
c
c    Input, double precision SIGMA(M,M), the covariance matrix, which should be 
c    a symmetric positive definite matrix.
c
c    Output, double precision A(M,M), the sample matrix from 
c    the Wishart distribution.
c
      implicit none

      integer m

      double precision a(m,m)
      double precision au(m,m)
      double precision aur(m,m)
      integer df
      integer flag
      double precision r(m,m)
      double precision sigma(m,m)

      if ( df .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WISHART_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
        stop 1
      end if
c
c  Get R, the upper triangular Cholesky factor of SIGMA.
c
      call r8mat_cholesky_factor_upper ( m, sigma, r, flag )

      if ( flag .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'WISHART_SAMPLE - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Unexpected error return from R8MAT_CHOLESKY_FACTOR_UPPER.'
        write ( *, '(a,i4)' ) '  FLAG = ', flag
        stop 1
      end if
c
c  Get AU, a sample from the unit Wishart distribution.
c
      call wishart_unit_sample ( m, df, au )
c
c  Construct the matrix A = R' * AU * R.
c
      call r8mat_mm ( m, m, m, au, r, aur )

      call r8mat_mtm ( m, m, m, r, aur, a )

      return
      end
      subroutine wishart_sample_inverse ( m, df, sigma, a )

c*********************************************************************72
c
cc WISHART_SAMPLE_INVERSE returns the inverse of a sample Wishart matrix.
c
c  Discussion:
c
c    This function requires functions from the PDFLIB and RNGLIB libraries.
c
c    The "initialize()" function from RNGLIB must be called before using
c    this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Patrick Odell, Alan Feiveson,
c    A numerical procedure to generate a sample covariance matrix,
c    Journal of the American Statistical Association,
c    Volume 61, Number 313, March 1966, pages 199-203.
c
c    Stanley Sawyer,
c    Wishart Distributions and Inverse-Wishart Sampling,
c    Washington University,
c    30 April 2007, 12 pages.
c
c  Parameters:
c
c    Input, integer M, the order of the matrix.
c
c    Input, integer DF, the number of degrees of freedom.
c    M <= DF.
c
c    Input, double precision SIGMA(M,M), the covariance matrix, which should be 
c    a symmetric positive definite matrix.
c
c    Output, double precision A(M,M), the inverse of a sample matrix from the 
c    Wishart distribution.
c
      implicit none

      integer m

      double precision a(m,m)
      integer df
      integer flag
      integer i
      integer j
      double precision r(m,m)
      double precision s(m,m)
      double precision sigma(m,m)
      double precision ua(m,m)
      double precision uas(m,m)

      if ( df .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WISHART_SAMPLE_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
        stop 1
      end if
!
!  Get R, the upper triangular Cholesky factor of SIGMA.
!
      call r8mat_cholesky_factor_upper ( m, sigma, r, flag )

      if ( flag .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'WISHART_SAMPLE_INVERSE - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Unexpected error return from R8MAT_CHOLESKY_FACTOR_UPPER.'
        write ( *, '(a,i4)' ) '  FLAG = ', flag
        stop 1
      end if
!
!  Get S, the inverse of R.
!
      do j = 1, m
        do i = 1, m
          s(i,j) = r(i,j)
        end do
      end do

      call r8ut_inverse ( m, s )
!
!  Get UA, the inverse of a sample from the unit Wishart distribution.
!
      call wishart_unit_sample_inverse ( m, df, ua )
!
!  Construct the matrix A = S * UA * S'.
!
      call r8mat_mmt ( m, m, m, ua, s, uas )
      call r8mat_mm ( m, m, m, s, uas, a )

      return
      end
      subroutine wishart_unit_sample ( m, df, a )

c*********************************************************************72
c
cc WISHART_UNIT_SAMPLE samples the unit Wishart distribution.
c
c  Discussion:
c
c    This function requires functions from the PDFLIB and RNGLIB libraries.
c
c    The "initialize()" function from RNGLIB must be called before using
c    this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Patrick Odell, Alan Feiveson,
c    A numerical procedure to generate a sample covariance matrix,
c    Journal of the American Statistical Association,
c    Volume 61, Number 313, March 1966, pages 199-203.
c
c    Stanley Sawyer,
c    Wishart Distributions and Inverse-Wishart Sampling,
c    Washington University,
c    30 April 2007, 12 pages.
c
c  Parameters:
c
c    Input, integer M, the order of the matrix.
c
c    Input, integer DF, the number of degrees of freedom.
c    M <= DF.
c
c    Output, double A(M,M), the sample matrix from the 
c    unit Wishart distribution.
c
      implicit none

      integer m

      double precision a(m,m)
      double precision c(m,m)
      integer df
      double precision df_chi
      integer i
      integer j
      double precision r8_chi_sample
      double precision r8_normal_01_sample

      if ( df .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WISHART_UNIT_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  DF = ', df, ' < M = ', m
        stop 1
      end if

      do i = 1, m

        do j = 1, i - 1
          c(i,j) = 0.0D+00
        end do

        df_chi = dble ( df + 1 - i )
        c(i,i) = sqrt ( r8_chi_sample ( df_chi ) )

        do j = i + 1, m
          c(i,j) = r8_normal_01_sample ( )
        end do

      end do

      call r8mat_mtm ( m, m, m, c, c, a )

      return
      end
      subroutine wishart_unit_sample_inverse ( m, df, a )

c*********************************************************************72
c
cc WISHART_UNIT_SAMPLE_INVERSE inverts a unit Wishart sample matrix.
c
c  Discussion:
c
c    This function requires functions from the PDFLIB and RNGLIB libraries.
c
c    The "initialize()" function from RNGLIB must be called before using
c    this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Patrick Odell, Alan Feiveson,
c    A numerical procedure to generate a sample covariance matrix,
c    Journal of the American Statistical Association,
c    Volume 61, Number 313, March 1966, pages 199-203.
c
c    Stanley Sawyer,
c    Wishart Distributions and Inverse-Wishart Sampling,
c    Washington University,
c    30 April 2007, 12 pages.
c
c  Parameters:
c
c    Input, integer M, the order of the matrix.
c
c    Input, integer DF, the number of degrees of freedom.
c    M <= DF.
c
c    Output, double precision A(M,M), the inverse of a sample matrix from 
c    the unit Wishart distribution.
c
      implicit none

      integer m

      double precision a(m,m)
      double precision b(m,m)
      double precision c(m,m)
      integer ( kind = 4 ) df
      double precision df_chi
      integer i
      integer j
      double precision r8_chi_sample
      double precision r8_normal_01_sample

      if ( df .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WISHART_UNIT_SAMPLE_INVERSE - Fatal error!'
        write ( *, '(a,i6,a,i6)' ) '  DF = ', df, ' < M = ', m
        stop 1
      end if
c
c  Compute C, an upper triangular matrix such that the
c  Wishart sample matrix is C' * C.
c
      do i = 1, m
        df_chi = dble ( df - i + 1 )
        c(i,i) = sqrt ( r8_chi_sample ( df_chi ) )
        do j = i + 1, m
          c(i,j) = r8_normal_01_sample ( )
        end do
      end do
c
c  Compute B, the inverse of C.
c
      do j = 1, m
        do i = 1, m
          b(i,j) = c(i,j)
        end do
      end do

      call r8ut_inverse ( m, b )
c
c  The inverse of the Wishart sample matrix C'*C is inv(C) * C'.
c
      call r8mat_mmt ( m, m, m, b, b, a )

      return
      end
