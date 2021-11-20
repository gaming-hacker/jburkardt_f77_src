      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS3_S_PRB.
c
c  Discussion:
c
c    BLAS3_S_PRB tests the BLAS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_S_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BLAS library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_S_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c********************************************************************72
c
cc TEST01 tests SGEMM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real a(10*10)
      real alpha
      real b(10*10)
      real beta
      real c(10*10)
      integer k
      integer lda
      integer ldb
      integer ldc
      integer m
      integer n
      character transa
      character transb
      character transc

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  SGEMM multiplies two matrices.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  1: C = alpha * A  * B  + beta * C;'
      write ( *, '(a)' ) '  2: C = alpha * A'' * B  + beta * C;'
      write ( *, '(a)' ) '  3: C = alpha * A  * B'' + beta * C;'
      write ( *, '(a)' ) '  4: C = alpha * A'' * B'' + beta * C;'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  We carry out all four calculations, but in each case,'
      write ( *, '(a)' ) 
     &  '  we choose our input matrices so that we get the same result.'
c
c  C = alpha * A * B + beta * C.
c
      transa = 'N'
      transb = 'N'
      transc = 'N'
      m = 4
      n = 5
      k = 3
      alpha = 2.0E+00
      lda = m
      call r4mat_test ( transa, lda, m, k, a )
      ldb = k
      call r4mat_test ( transb, ldb, k, n, b )
      beta = 3.0E+00
      ldc = m
      call r4mat_test ( transc, ldc, m, n, c )

      call sgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r4mat_print ( m, n, c, 
     &  '  C = alpha * A * B + beta * C:' )
c
c  C = alpha * A' * B + beta * C.
c
      transa = 'T'
      transb = 'N'
      transc = 'N'
      m = 4
      n = 5
      k = 3
      alpha = 2.0E+00
      lda = k
      call r4mat_test ( transa, lda, m, k, a )
      ldb = k
      call r4mat_test ( transb, ldb, k, n, b )
      beta = 3.0E+00
      ldc = m
      call r4mat_test ( transc, ldc, m, n, c )

      call sgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r4mat_print ( m, n, c, 
     &  '  C = alpha * A'' * B + beta * C:' )
c
c  C = alpha * A * B' + beta * C.
c
      transa = 'N'
      transb = 'T'
      transc = 'N'
      m = 4
      n = 5
      k = 3
      alpha = 2.0E+00
      lda = m
      call r4mat_test ( transa, lda, m, k, a )
      ldb = n
      call r4mat_test ( transb, ldb, k, n, b )
      beta = 3.0E+00
      ldc = m
      call r4mat_test ( transc, ldc, m, n, c )

      call sgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r4mat_print ( m, n, c, 
     &  '  C = alpha * A * B'' + beta * C:' )
c
c  C = alpha * A' * B' + beta * C.
c
      transa = 'T'
      transb = 'T'
      transc = 'N'
      m = 4
      n = 5
      k = 3
      alpha = 2.0E+00
      lda = k
      call r4mat_test ( transa, lda, m, k, a )
      ldb = n
      call r4mat_test ( transb, ldb, k, n, b )
      beta = 3.0E+00
      ldc = m
      call r4mat_test ( transc, ldc, m, n, c )

      call sgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r4mat_print ( m, n, c, 
     &  '  C = alpha * A'' * B'' + beta * C:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests DTRMM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 5 )

      integer lda
      parameter ( lda = m )
      integer ldb
      parameter ( ldb = m )

      real a(lda,m)
      real alpha
      real b(ldb,n)
      character diag
      integer i
      integer j
      character side
      character transa
      character transb
      character uplo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  STRMM multiplies a triangular matrix A and a'
      write ( *, '(a)' ) '  rectangular matrix B'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  1: B = alpha * A  * B;'
      write ( *, '(a)' ) '  2: B = alpha * A'' * B;'
c
c  B = alpha * A * B.
c
      side = 'L'
      uplo = 'U'
      transa = 'N'
      diag = 'N'
      alpha = 2.0E+00

      do j = 1, m
        do i = 1, j
          a(i,j) = real ( i + j )
        end do
        do i = j + 1, m
          a(i,j) = 0.0E+00
        end do
      end do

      transb = 'N'
      call r4mat_test ( transb, ldb, m, n, b )

      call strmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, 
     &  ldb )

      call r4mat_print ( m, n, b, '  B = alpha * A * B:' );
c
c  B = alpha * A' * B.
c
      side = 'L'
      uplo = 'U'
      transa = 'T'
      diag = 'N'
      alpha = 2.0E+00

      do j = 1, m
        do i = 1, j
          a(i,j) = real ( i + j )
        end do
        do i = j + 1, m
          a(i,j) = 0.0E+00
        end do
      end do

      transb = 'N'
      call r4mat_test ( transb, ldb, m, n, b )

      call strmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, 
     &  ldb )

      call r4mat_print ( m, n, b, '  B = alpha * A * B:' );

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests STRSM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 5 )

      real amm(m,m)
      real ann(n,n)
      real alpha
      real b(m,n)
      character diag
      integer i
      integer j
      integer lda
      integer ldb
      character side
      character transa
      character transb
      character uplo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  STRSM solves a linear system involving a triangular'
      write ( *, '(a)' ) '  matrix A and a rectangular matrix B.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  1: Solve A  * X  = alpha * B;'
      write ( *, '(a)' ) '  2: Solve A'' * X  = alpha * B;'
      write ( *, '(a)' ) '  3: Solve X  * A  = alpha * B;'
      write ( *, '(a)' ) '  4: Solve X  * A'' = alpha * B;'
c
c  Solve A * X = alpha * B.
c
      side = 'L'
      uplo = 'U'
      transa = 'N'
      diag = 'N'
      alpha = 2.0E+00
      lda = m
      ldb = m

      do j = 1, m
        do i = 1, j
          amm(i,j) = real ( i + j )
        end do
        do i = j + 1, m
          amm(i,j) = 0.0E+00
        end do
      end do

      transb = 'N'
      call r4mat_test ( transb, ldb, m, n, b )

      call strsm ( side, uplo, transa, diag, m, n, alpha, amm, lda, b, 
     &  ldb )

      call r4mat_print ( m, n, b, '  X = inv ( A ) * alpha * B:' );
c
c  Solve A' * X = alpha * B.
c
      side = 'L'
      uplo = 'U'
      transa = 'T'
      diag = 'N'
      alpha = 2.0E+00
      lda = m
      ldb = m

      do j = 1, m
        do i = 1, j
          amm(i,j) = real ( i + j )
        end do
        do i = j + 1, m
          amm(i,j) = 0.0E+00
        end do
      end do

      transb = 'N'
      call r4mat_test ( transb, ldb, m, n, b )

      call strsm ( side, uplo, transa, diag, m, n, alpha, amm, lda, b, 
     &  ldb )

      call r4mat_print ( m, n, b, '  X = inv ( A'' ) * alpha * B:' );
c
c  Solve X * A = alpha * B.
c
      side = 'R'
      uplo = 'U'
      transa = 'N'
      diag = 'N'
      alpha = 2.0E+00
      lda = n
      ldb = m

      do j = 1, n
        do i = 1, j
          ann(i,j) = real ( i + j )
        end do
        do i = j + 1, n
          ann(i,j) = 0.0E+00
        end do
      end do

      transb = 'N'
      call r4mat_test ( transb, ldb, m, n, b )

      call strsm ( side, uplo, transa, diag, m, n, alpha, ann, lda, b, 
     &  ldb )

      call r4mat_print ( m, n, b, '  X = alpha * B * inv ( A ):' );
c
c  Solve X * A'' = alpha * B.
c
      side = 'R'
      uplo = 'U'
      transa = 'T'
      diag = 'N'
      alpha = 2.0E+00
      lda = n
      ldb = m

      do j = 1, n
        do i = 1, j
          ann(i,j) = real ( i + j )
        end do
        do i = j + 1, n
          ann(i,j) = 0.0E+00
        end do
      end do

      transb = 'N'
      call r4mat_test ( transb, ldb, m, n, b )

      call strsm ( side, uplo, transa, diag, m, n, alpha, ann, lda, b, 
     &  ldb )

      call r4mat_print ( m, n, b, '  X = alpha * B * inv ( A'' ):' );

      return
      end

