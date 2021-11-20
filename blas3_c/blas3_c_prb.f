      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS3_C_PRB.
c
c  Discussion:
c
c    BLAS3_C_PRB tests the BLAS library.
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
      write ( *, '(a)' ) 'BLAS3_C_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BLAS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS3_C_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 demonstrates the use of CGEMM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2014
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

      complex a(lda,k)
      complex alpha
      complex b(ldb,n)
      complex beta
      complex c(ldc,n)
      character transa
      character transb

      transa = 'N'
      transb = 'N'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  CGEMM can combine scale, multiply and add matrices'
      write ( *, '(a)' ) 
     &  '  using single precision complex arithmetic.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Here, we simply compute C = A * B.'
      write ( *, '(a)' ) 
     &  '  Because B is inverse ( A ), C should be the identity.'
 
      call c4mat_test ( m, a )
      call c4mat_print ( m, m, a, '  Matrix A:' )

      call c4mat_test_inverse ( m, b )
      call c4mat_print ( m, m, b, '  Matrix B:' )

      alpha = ( 1.0E+00, 0.0E+00 )
      beta = ( 0.0E+00, 0.0E+00 )

      call cgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, 
     &  beta, c, ldc )

      call c4mat_print ( m, n, c, '  Product C = A * B:' )

      return
      end
