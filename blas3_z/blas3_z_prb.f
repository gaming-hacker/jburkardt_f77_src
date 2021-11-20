      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS3_Z_PRB.
c
c  Discussion:
c
c    BLAS3_Z_PRB tests the BLAS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_Z_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BLAS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_Z_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 demonstrates the use of ZGEMM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer k
      parameter ( k = 3 )
      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 3 )

      integer lda
      parameter ( lda = m )
      integer ldb
      parameter ( ldb = k )
      integer ldc
      parameter ( ldc = m )

      double complex a(lda,k)
      double complex alpha
      double complex b(ldb,n)
      double complex beta
      double complex c(ldc,n)
      character transa
      character transb

      transa = 'N'
      transb = 'N'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  ZGEMM can combine scale, multiply and add matrices'
      write ( *, '(a)' ) 
     &  '  using single precision complex arithmetic.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Here, we simply compute C = A * B.'
      write ( *, '(a)' ) 
     &  '  Because B is inverse ( A ), C should be the identity.'
 
      call c8mat_test ( m, a )
      call c8mat_print ( m, m, a, '  Matrix A:' )

      call c8mat_test_inverse ( b )
      call c8mat_print ( m, m, b, '  Matrix B:' )

      alpha = ( 1.0D+00, 0.0D+00 )
      beta = ( 0.0D+00, 0.0D+00 )

      call zgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call c8mat_print ( m, n, c, '  Product C = A * B:' )

      return
      end
