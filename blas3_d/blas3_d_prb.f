      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS3_D_PRB.
c
c  Discussion:
c
c    BLAS3_D_PRB tests the BLAS library.
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
      write ( *, '(a)' ) 'BLAS3_D_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BLAS library.'

      call dgemm_test ( )
      call dtrmm_test ( )
      call dtrsm_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_D_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine dgemm_test ( )

c********************************************************************72
c
cc DGEMM_TEST tests DGEMM.
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

      double precision a(10*10)
      double precision alpha
      double precision b(10*10)
      double precision beta
      double precision c(10*10)
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
      write ( *, '(a)' ) 'DGEMM_TEST'
      write ( *, '(a)' ) '  DGEMM carries out matrix multiplications'
      write ( *, '(a)' ) '  for double precision real matrices.'
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
      alpha = 2.0D+00
      lda = m
      call r8mat_test ( transa, lda, m, k, a )
      ldb = k
      call r8mat_test ( transb, ldb, k, n, b )
      beta = 3.0D+00
      ldc = m
      call r8mat_test ( transc, ldc, m, n, c )

      call dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r8mat_print ( m, n, c, 
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
      alpha = 2.0D+00
      lda = k
      call r8mat_test ( transa, lda, m, k, a )
      ldb = k
      call r8mat_test ( transb, ldb, k, n, b )
      beta = 3.0D+00
      ldc = m
      call r8mat_test ( transc, ldc, m, n, c )

      call dgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r8mat_print ( m, n, c, 
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
      alpha = 2.0D+00
      lda = m
      call r8mat_test ( transa, lda, m, k, a )
      ldb = n
      call r8mat_test ( transb, ldb, k, n, b )
      beta = 3.0D+00
      ldc = m
      call r8mat_test ( transc, ldc, m, n, c )

      call dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r8mat_print ( m, n, c, 
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
      alpha = 2.0D+00
      lda = k
      call r8mat_test ( transa, lda, m, k, a )
      ldb = n
      call r8mat_test ( transb, ldb, k, n, b )
      beta = 3.0D+00
      ldc = m
      call r8mat_test ( transc, ldc, m, n, c )

      call dgemm ( transa, transb,  m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call r8mat_print ( m, n, c, 
     &  '  C = alpha * A'' * B'' + beta * C:' )

      return
      end
      subroutine dtrmm_test ( )

c*********************************************************************72
c
cc DTRMM_TEST tests DTRMM.
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

      double precision a(lda,m)
      double precision alpha
      double precision b(ldb,n)
      character diag
      integer i
      integer j
      character side
      character transa
      character transb
      character uplo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DTRMM_TEST'
      write ( *, '(a)' ) 
     &  '  DTRMM multiplies a triangular matrix A and a'
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
      alpha = 2.0D+00

      do j = 1, m
        do i = 1, j
          a(i,j) = dble ( i + j )
        end do
        do i = j + 1, m
          a(i,j) = 0.0D+00
        end do
      end do

      transb = 'N'
      call r8mat_test ( transb, ldb, m, n, b )

      call dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, 
     &  ldb )

      call r8mat_print ( m, n, b, '  B = alpha * A * B:' );
c
c  B = alpha * A' * B.
c
      side = 'L'
      uplo = 'U'
      transa = 'T'
      diag = 'N'
      alpha = 2.0D+00

      do j = 1, m
        do i = 1, j
          a(i,j) = dble ( i + j )
        end do
        do i = j + 1, m
          a(i,j) = 0.0D+00
        end do
      end do

      transb = 'N'
      call r8mat_test ( transb, ldb, m, n, b )

      call dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, 
     &  ldb )

      call r8mat_print ( m, n, b, '  B = alpha * A * B:' );

      return
      end
      subroutine dtrsm_test ( )

c*********************************************************************72
c
cc DTRSM_TEST tests DTRSM.
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

      double precision amm(m,m)
      double precision ann(n,n)
      double precision alpha
      double precision b(m,n)
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
      write ( *, '(a)' ) 'DTRSM_TEST'
      write ( *, '(a)' ) 
     &  '  DTRSM solves a linear system involving a triangular'
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
      alpha = 2.0D+00
      lda = m
      ldb = m

      do j = 1, m
        do i = 1, j
          amm(i,j) = dble ( i + j )
        end do
        do i = j + 1, m
          amm(i,j) = 0.0D+00
        end do
      end do

      transb = 'N'
      call r8mat_test ( transb, ldb, m, n, b )

      call dtrsm ( side, uplo, transa, diag, m, n, alpha, amm, lda, b, 
     &  ldb )

      call r8mat_print ( m, n, b, '  X = inv ( A ) * alpha * B:' );
c
c  Solve A' * X = alpha * B.
c
      side = 'L'
      uplo = 'U'
      transa = 'T'
      diag = 'N'
      alpha = 2.0D+00
      lda = m
      ldb = m

      do j = 1, m
        do i = 1, j
          amm(i,j) = dble ( i + j )
        end do
        do i = j + 1, m
          amm(i,j) = 0.0D+00
        end do
      end do

      transb = 'N'
      call r8mat_test ( transb, ldb, m, n, b )

      call dtrsm ( side, uplo, transa, diag, m, n, alpha, amm, lda, b, 
     &  ldb )

      call r8mat_print ( m, n, b, '  X = inv ( A'' ) * alpha * B:' );
c
c  Solve X * A = alpha * B.
c
      side = 'R'
      uplo = 'U'
      transa = 'N'
      diag = 'N'
      alpha = 2.0D+00
      lda = n
      ldb = m

      do j = 1, n
        do i = 1, j
          ann(i,j) = dble ( i + j )
        end do
        do i = j + 1, n
          ann(i,j) = 0.0D+00
        end do
      end do

      transb = 'N'
      call r8mat_test ( transb, ldb, m, n, b )

      call dtrsm ( side, uplo, transa, diag, m, n, alpha, ann, lda, b, 
     &  ldb )

      call r8mat_print ( m, n, b, '  X = alpha * B * inv ( A ):' );
c
c  Solve X * A'' = alpha * B.
c
      side = 'R'
      uplo = 'U'
      transa = 'T'
      diag = 'N'
      alpha = 2.0D+00
      lda = n
      ldb = m

      do j = 1, n
        do i = 1, j
          ann(i,j) = dble ( i + j )
        end do
        do i = j + 1, n
          ann(i,j) = 0.0D+00
        end do
      end do

      transb = 'N'
      call r8mat_test ( transb, ldb, m, n, b )

      call dtrsm ( side, uplo, transa, diag, m, n, alpha, ann, lda, b, 
     &  ldb )

      call r8mat_print ( m, n, b, '  X = alpha * B * inv ( A'' ):' );

      return
      end

