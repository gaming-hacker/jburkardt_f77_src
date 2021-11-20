      program main

c*********************************************************************72
c
cc MAIN is the main program for PDFLIB_TEST.
c
c  Discussion:
c
c    PDFLIB_TEST tests the PDFLIB library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PDFLIB_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PDFLIB library.'
c
c  Initialize the random number generator package.
c
      call initialize ( )

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PDFLIB_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests R8MAT_POFAC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision diff
      integer i
      integer j
      double precision r1(n,n)
      double precision r2(n,n)
      double precision r8_uniform_01_sample
      double precision r8mat_norm_fro_affine

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  R8MAT_POFAC computes the Cholesky factor R of a'
      write ( *, '(a)' ) 
     &  '  positive definite matrix A, so that A = R''* R.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Start with random R1;'
      write ( *, '(a)' ) '  Compute A = R1'' * R1.'
      write ( *, '(a)' ) 
     &  '  Call R8MAT_POFAC and see if you recover R2 = R1.'
c
c  Generate a random upper triangular matrix with positive diagonal.
c
      do j = 1, n
        do i = 1, j
          if ( i == j ) then
            r1(i,j) = abs ( r8_uniform_01_sample ( ) )
          else
            r1(i,j) = r8_uniform_01_sample ( )
          end if
        end do
        do i = j + 1, n
          r1(i,j) = 0.0D+00
        end do
      end do

      call r8mat_print ( n, n, r1, '  R1:' )
c
c  Compute a positive definite symmetric matrix A.
c
      call r8mat_mtm ( n, n, n, r1, r1, a )

      call r8mat_print ( n, n, a, '  A:' )

      call r8mat_pofac ( n, a, r2 )

      diff = r8mat_norm_fro_affine ( n, n, r1, r2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Frobenius difference between R1 and R2 = ', diff

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests R8VEC_MULTINORMAL_PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision c(n,n)
      double precision c_det
      double precision c_inv(n,n)
      double precision c_inv_y(n)
      double precision eps
      integer i
      integer j
      double precision mu(n)
      double precision pdf1
      double precision pdf2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r1(n,n)
      double precision r2(n,n)
      double precision r8_normal_01_sample
      double precision r8_uniform_01_sample
      double precision r8vec_dot_product
      double precision r8vec_multinormal_pdf
      double precision x(n)
      double precision xcx
      double precision y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  R8VEC_MULTINORMAL_PDF evaluates the PDF for the'
      write ( *, '(a)' ) '  multinormal distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The covariance matrix is C.'
      write ( *, '(a)' ) '  The definition uses the inverse of C;'
      write ( *, '(a)' ) 
     &  '  R8VEC_MULTINORMAL_PDF uses the Cholesky factor.'
      write ( *, '(a)' ) '  Verify that the algorithms are equivalent.'
c
c  Generate a random upper triangular matrix with positive diagonal.
c
      do j = 1, n
        do i = 1, j
          if ( i .eq. j ) then
            r1(i,j) = abs ( r8_uniform_01_sample ( ) )
          else
            r1(i,j) = r8_uniform_01_sample ( )
          end if
        end do
        do i = j + 1, n
          r1(i,j) = 0.0D+00
        end do
      end do

      call r8mat_print ( n, n, r1, '  R1:' )
c
c  Compute a positive definite symmetric covariance matrix C.
c
      call r8mat_mtm ( n, n, n, r1, r1, c )
 
      call r8mat_print ( n, n, c, '  C:' )
c
c  Compute the Cholesky factor R.
c
      call r8mat_pofac ( n, c, r2 )

      call r8mat_print ( n, n, r2, '  R2:' )
c
c  Compute the determinant of C.
c
      call r8mat_podet ( n, r2, c_det )
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Determinant of C = ', c_det
c
c  Compute the inverse of C.
c
      call r8mat_poinv ( n, r2, c_inv )
      call r8mat_print ( n, n, c_inv, '  C_INV:' )
c
c  Compute a random set of means.
c
      do i = 1, n
        mu(i) = r8_normal_01_sample ( )
      end do
c
c  Compute X as small variations from MU.
c
      do i = 1, n
        eps = 0.01D+00 * r8_normal_01_sample ( )
        x(i) = ( 1.0D+00 + eps ) * mu(i)
      end do
c
c  Compute PDF1 from the function.
c
      pdf1 = r8vec_multinormal_pdf ( n, mu, r2, c_det, x )
c
c  Compute PDF2 from the definition.
c
      do i = 1, n
        y(i) = x(i) - mu(i)
      end do

      call r8mat_mv ( n, n, c_inv, y, c_inv_y )

      xcx = r8vec_dot_product ( n, y, c_inv_y )

      pdf2 = 1.0D+00 / sqrt ( ( 2.0D+00 * pi ) ** n ) 
     &  * 1.0D+00 / sqrt ( c_det ) 
     &  * exp ( - 0.5D+00 * xcx )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF1 = ', pdf1
      write ( *, '(a,g14.6)' ) '  PDF2 = ', pdf2

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 calls R8_CHI_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision df
      integer g
      integer i
      double precision r8_chi_sample
      double precision r8_uniform_01_sample
      double precision u

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  R8_CHI_SAMPLE ( DF ) samples the Chi distribution with'
      write ( *, '(a)' ) '  DF degrees of freedom.'
c
c  Initialize the package.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  INITIALIZE initializes the random number generator.'
      write ( *, '(a)' ) 
     &  '  It only needs to be called once before using the package.'

      call initialize ( )
c
c  Set the current generator index to #2.
c
      g = 2
      call cgn_set ( g )
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Current generator index = ', g
c
c  Repeatedly call R8_CHI_SAMPLE ( DF ).
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I       DF       R8_CHI_SAMPLE ( DF )'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        df = 5.0D+00 * r8_uniform_01_sample ( ) + 1.0D+00
        u = r8_chi_sample ( df )
        write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) i, df, u
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
c    An R8MAT is an array of R8's.
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

      call r8vec_copy ( m, y1, y )

      return
      end
      function r8mat_norm_fro_affine ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_NORM_FRO_AFFINE returns the Frobenius norm of an R8MAT difference.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c    The Frobenius norm is defined as
c
c      R8MAT_NORM_FRO = sqrt (
c        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
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
c    24 March 2000
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
c    Input, double precision A1(M,N), A2(M,N), the matrices for whose
c    difference the Frobenius norm is desired.
c
c    Output, double precision R8MAT_NORM_FRO_AFFINE, the Frobenius
c    norm of A1 - A2.
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
          value = value + ( a1(i,j) - a2(i,j) ) ** 2
        end do
      end do

      r8mat_norm_fro_affine = sqrt ( value )

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
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
