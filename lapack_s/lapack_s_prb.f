      program main

c*********************************************************************72
c
cc MAIN is the main program for LAPACK_S_PRB.
c
c  Discussion:
c
c    LAPACK_S_PRB calls some of the single precision real LAPACK routines.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAPACK_S_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Test the single precision real routines'
      write ( *, '(a)' ) '  in the LAPACK linear algebra library.'

      call test01
      call test02
      call test03
      call test04
      call test05
      call test06
      call test07
      call test08
      call test09

      call test10
      call test11
      call test12
      call test13
      call test14
      call test15
      call test16
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAPACK_S_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01 tests SGBTRF and SGBTRS.
c
c  Discussion:
c
c    The problem is just an enlarged version of the
c    problem for n = 5, which is:
c
c    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
c                (-1  2 -1  0  0)                          (0)
c                ( 0 -1  2 -1  0)                          (0)
c                ( 0  0 -1  2 -1)                          (0)
c                ( 0  0  0 -1  2)                          (1)
c
c
c    Solution is   (1)
c                  (1)
c                  (1)
c                  (1)
c                  (1)
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer ml
      integer mu
      integer lda

      parameter ( n = 25 )
      parameter ( ml = 1 )
      parameter ( mu = 1 )
      parameter ( lda = 2 * ml + mu + 1 )

      real a(lda,n)
      real b(n)
      integer i
      integer info
      integer ipiv(n)
      integer j
      integer m

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general band storage mode (GB):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGBTRF factors a general band matrix.'
      write ( *, '(a)' ) '  SGBTRS solves a factored system.'
      write ( *, '(a)' ) ' '
c
c  Assign values to matrix A and right hand side b.
c
      b(1) = 1.0E+00
      do i = 2, n-1
        b(i) = 0.0E+00
      end do
      b(n) = 1.0E+00
c
c  Zero out the matrix.
c
      do i = 1, lda
        do j = 1, n
          a(i,j) = 0.0E+00
        end do
      end do

      m = ml + mu + 1
c
c  Superdiagonal,
c  Diagonal,
c  Subdiagonal.
c
      do j = 2, n
        a(m-1,j) = -1.0E+00
      end do

      do j = 1, n
        a(m,j) = 2.0E+00
      end do

      do j = 1, n-1
        a(m+1,j) = -1.0E+00
      end do
c
c  Factor the matrix.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Bandwidth is ', m
      write ( *, '(a)' ) ' '

      call sgbtrf ( n, n, ml, mu, a, lda, ipiv, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01'
        write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
        return
      end if
c
c  Solve the linear system.
c
      call sgbtrs ( 'n', n, ml, mu, 1, a, lda, ipiv, b, n, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01'
        write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
        return
      end if

      call r4vec_print_some ( n, b, 5, 
     &  '  Partial solution (all should be 1)' )

      return
      end
      subroutine test02

c*********************************************************************72
c
cc TEST02 tests SGBTRF and SGBTRS.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer ml
      integer mu
      integer lda

      parameter ( n = 25 )
      parameter ( ml = 3 )
      parameter ( mu = 3 )
      parameter ( lda = 2 * ml + mu + 1 )

      real a(lda,n)
      real b(n)
      integer i
      integer ihi
      integer ilo
      integer info
      integer ipiv(n)
      integer j
      integer m
      real temp

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general band storage mode (GB):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGBTRF factors a general band matrix.'
      write ( *, '(a)' ) '  SGBTRS solves a factored system.'
      write ( *, '(a)' ) ' '
c
c  Assign values to matrix A and right hand side b.
c
c  We want to try a problem with a significant bandwidth.
c
      m = ml + mu + 1

      do j = 1, n
        ilo = max ( 1, j-mu )
        ihi = min ( n, j+ml )
 
        temp = 0.0E+00
        do i = ilo, ihi
          a(i-j+m,j) = -1.0E+00
          temp = temp - 1.0E+00
        end do
        temp = temp + 1.0E+00
 
        a(m,j) = 4.0E+00 - temp
      end do
c
c  Right hand side.
c
      do j = 1, n
        b(j) = 4.0E+00
      end do
c
c  Factor the matrix.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Bandwidth is ', m
 
      call sgbtrf ( n, n, ml, mu, a, lda, ipiv, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
        return
      end if
c
c  Solve the linear system.
c
      call sgbtrs ( 'n', n, ml, mu, 1, a, lda, ipiv, b, n, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
      end if

      call r4vec_print_some ( n, b, 5, 
     &  '  Partial solution (all should be 1)' )

      return
      end
      subroutine test03

c*********************************************************************72
c
cc TEST03 tests SGECON and SGETRF.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer lda
      integer lwork

      parameter ( n = 3 )
      parameter ( lda = n )
      parameter ( lwork = 4 * n )

      real a(lda,n)
      real anorm
      integer info
      integer ipiv(n)
      integer iwork(n)
      real rcond
      real r4mat_norm_li
      real work(lwork)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGETRF computes the LU factorization;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGECON computes the condition number '
      write ( *, '(a)' ) '  of a factored matrix'
c
c  Set the matrix.
c
      a(1,1) = 1.0E+00
      a(1,2) = 2.0E+00
      a(1,3) = 3.0E+00

      a(2,1) = 4.0E+00
      a(2,2) = 5.0E+00
      a(2,3) = 6.0E+00

      a(3,1) = 7.0E+00
      a(3,2) = 8.0E+00
      a(3,3) = 0.0E+00

      call r4mat_print ( n, n, a, '  The matrix A:' )
c
c  Compute the L-Infinity norm of the matrix.
c
      anorm = r4mat_norm_li ( n, n, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Matrix L-infinity norm is ', anorm
c
c  Factor the matrix.
c
      call sgetrf ( n, n, a, lda, ipiv, info )
c
c  Get the condition number.
c
      call sgecon ( 'I', n, a, lda, anorm, rcond, work, iwork, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Condition number calculation failedc'
        write ( *, '(a,i8)' ) '  INFO = ', info
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Matrix reciprocal condition number = ', rcond
 
      return
      end
      subroutine test04

c*********************************************************************72
c
cc TEST04 tests SGESVD.
c
c  Discussion:
c
c    SGESVD computes the singular value decomposition:
c
c      A = U * S * V'
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer lwork

      parameter ( m = 6 )
      parameter ( n = 4 )
c     parameter ( lwork = 3*min(m,n) + max ( max(m,n), 2*min(m,n) ) )
      parameter ( lwork = 3*4 + 8 )

      real a(m,n)
      real b(m,n)
      integer i
      integer info
      integer j
      integer k
      integer lda
      integer ldu
      integer ldvt
      character jobu
      character jobvt
      integer mn
      real s(m+n)
      integer seed
      real sigma(m,n)
      real u(m,m)
      real vt(n,n)
      real work(lwork)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGESVD computes the singular value '
      write ( *, '(a)' ) '  decomposition:'
      write ( *, '(a)' ) '    A = U * S * V'''
c
c  Set A.
c
      seed = 123456789

      call r4mat_uniform_01 ( m, n, seed, a )

      call r4mat_print ( m, n, a, '  The matrix A:' )
c
c  Compute the eigenvalues and eigenvectors.
c
      jobu = 'A'
      jobvt = 'A'
      lda = m
      ldu = m
      ldvt = n
      
      call sgesvd ( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, 
     &  work, lwork, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SGESVD returned nonzero INFO = ', info
        return
      end if

      mn = min ( m, n )

      call r4vec_print ( mn, s, '  Singular values' )

      call r4mat_print ( m, m, u, '  Left singular vectors U:' )
      call r4mat_print ( n, n, vt, '  Right singular vectors V'':' )

      do i = 1, m
        do j = 1, n
          sigma(i,j) = 0.0E+00
        end do
      end do

      do i = 1, min ( m, n )
        sigma(i,i) = s(i)
      end do

      do i = 1, m
        do j = 1, n
          b(i,j) = 0.0E+00
          do k = 1, n
            b(i,j) = b(i,j) + sigma(i,k) * vt(k,j)
          end do
        end do
      end do

      do i = 1, m
        do j = 1, n
          a(i,j) = 0.0E+00
          do k = 1, m
            a(i,j) = a(i,j) + u(i,k) * b(k,j)
          end do
        end do
      end do

      call r4mat_print ( m, n, a, '  The product U * S * V'':' )

      return
      end
      subroutine test05

c*********************************************************************72
c
cc TEST05 tests SGETRF and SGETRI.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer lda
      integer lwork

      parameter ( n = 3 )
      parameter ( lda = n )
      parameter ( lwork = n )

      real a(lda,n)
      integer info
      integer ipiv(n)
      real work(lwork)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGETRF factors a general matrix;'
      write ( *, '(a)' ) '  SGETRI computes the inverse.'
c
c  Set the matrix.
c
      a(1,1) = 1.0E+00
      a(1,2) = 2.0E+00
      a(1,3) = 3.0E+00

      a(2,1) = 4.0E+00
      a(2,2) = 5.0E+00
      a(2,3) = 6.0E+00

      a(3,1) = 7.0E+00
      a(3,2) = 8.0E+00
      a(3,3) = 0.0E+00

      call r4mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      call sgetrf ( n, n, a, lda, ipiv, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SGETRF returned INFO = ', info
        write ( *, '(a)' ) '  The matrix is numerically singular.'
        return
      end if
c
c  Compute the inverse matrix.
c
      call sgetri ( n, a, lda, ipiv, work, lwork, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The inversion procedure failedc'
        write ( *, '(a,i8)' ) '  INFO = ', info
        return
      end if

      call r4mat_print ( n, n, a, '  The inverse matrix:' )
 
      return
      end
      subroutine test06

c*********************************************************************72
c
cc TEST06 tests SGETRF and SGETRS.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer lda

      parameter ( n = 3 )
      parameter ( lda = n )

      real a(lda,n)
      real b(n)
      integer info
      integer ipiv(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGETRF computes the LU factorization;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGETRS solves linear systems using '
      write ( *, '(a)' ) '  the LU factors;'
c
c  Set the matrix.
c
      a(1,1) = 1.0E+00
      a(1,2) = 2.0E+00
      a(1,3) = 3.0E+00

      a(2,1) = 4.0E+00
      a(2,2) = 5.0E+00
      a(2,3) = 6.0E+00

      a(3,1) = 7.0E+00
      a(3,2) = 8.0E+00
      a(3,3) = 0.0E+00

      call r4mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      call sgetrf ( n, n, a, lda, ipiv, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SGETRF returned INFO = ', info
        write ( *, '(a)' ) '  The matrix is numerically singular.'
        return
      end if
c
c  Set the right hand side.
c
      b(1) = 14.0E+00
      b(2) = 32.0E+00
      b(3) = 23.0E+00

      call r4vec_print ( n, b, '  Right hand side B' )
c
c  Solve the linear system.
c
      call sgetrs ( 'n', n, 1, a, lda, ipiv, b, n, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Solution procedure failedc'
        write ( *, '(a,i8)' ) '  INFO = ', info
        return
      end if

      call r4vec_print ( n, b, '  The solution X' )
 
      return
      end
      subroutine test07
c
c*********************************************************************72
c
cc TEST07 tests SGETRF and SGETRS.
c
c  Discussion:
c
c    The problem is just an enlarged version of the
c    problem for n = 5, which is:
c
c    Matrix A is ( N -1 -1 -1 -1)    right hand side b is  (1)
c                (-1  N -1 -1 -1)                          (1)
c                (-1 -1  N -1 -1)                          (1)
c                (-1 -1 -1  N -1)                          (1)
c                (-1 -1 -1 -1  N)                          (1)
c
c    Solution is   (1)
c                  (1)
c                  (1)
c                  (1)
c                  (1)
c
c    For this problem, no pivoting is required.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer lda

      parameter ( n = 25 )
      parameter ( lda = n )

      real a(lda,n)
      real b(n)
      integer i
      integer info
      integer ipiv(n)
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGETRF factors a general matrix;'
      write ( *, '(a)' ) '  SGETRS solves a linear system;'
      write ( *, '(a)' ) ' '
c
c  Assign values to matrix A and right hand side b.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = real ( n )
          else
            a(i,j) = -1.0E+00
          end if
        end do
      end do

      do i = 1, n
        b(i) = 1.0E+00
      end do
c
c  Factor the matrix.
c
      call sgetrf ( n, n, a, lda, ipiv, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Matrix is singular, INFO = ', info
        return
      end if
c
c  Solve the linear system.
c
      call sgetrs ( 'n', n, 1, a, lda, ipiv, b, n, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  Solution procedure failed, INFO = ', info
        return
      end if

      call r4vec_print_some ( n, b, 5, 
     &  '  Partial solution (all should be 1)' )

      return
      end
      subroutine test08

c*********************************************************************72
c
cc TEST08 tests SGTSV.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 100 )

      real b(n)
      real c(n-1)
      real d(n)
      real e(n-1)
      integer i
      integer info
      integer ldb
      integer nrhs

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general tridiagonal storage mode (GT):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGTSV factors and solves a linear system'
      write ( *, '(a)' ) '  with a general tridiagonal matrix.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The system is of order N = ', n
      write ( *, '(a)' ) ' '
c
c  Right hand side.
c 
      do i = 1, n-1
        b(i) = 0.0E+00
      end do
      b(n) = n + 1
c
c  Subdiagonal.
c  Diagonal.
c  Superdiagonal.
c 
      do i = 1, n-1
        c(i) = -1.0E+00
      end do

      do i = 1, n
        d(i) = 2.0E+00
      end do

      do i = 1, n-1
        e(i) = -1.0E+00
      end do
 
      nrhs = 1
      ldb = n
c
c  Factor and solve the linear system.
c
      call sgtsv ( n, nrhs, c, d, e, b, ldb, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Solution procedure failed.'
        write ( *, '(a,i8)' ) '  INFO = ', info
        return
      end if

      call r4vec_print_some ( n, b, 5, 
     &  '  Partial solution (Should be 1,2,3...)' )

      return
      end
      subroutine test09
c
c*********************************************************************72
c
cc TEST09 tests SPBTRF.
c
c  Discussion:
c
c    We want to compute the lower triangular Cholesky factor L
c    of a positive definite symmetric band matrix.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer nband

      parameter ( n = 5 )
      parameter ( nband = 1 )

      real a(nband+1,n)
      integer i
      integer info
      integer j
      real l(nband+1,n)
      real l_row(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) 
     &  '  in positive definite band storage mode (PB):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SPBTRF computes'
      write ( *, '(a)' ) '    the lower Cholesky factor A = L*L'' or'
      write ( *, '(a)' ) '    the upper Cholesky factor A = U''*U;'
      write ( *, '(a)' ) ' '
c
c  Zero out the matrix.
c
      do i = 1, nband+1
        do j = 1, n
          a(i,j) = 0.0E+00
        end do
      end do
c
c  Store the diagonal of a symmetric band matrix.
c
      do j = 1, n
        a(1,j) = 2.0E+00
      end do
c
c  Store the subdiagonal of a symmetric band matrix.
c
      do j = 1, n-1
        a(2,j) = -1.0E+00
      end do
c
c  Get the lower triangular Cholesky factor L:
c
      do i = 1, nband+1
        do j = 1, n
          l(i,j) = a(i,j)
        end do
      end do

      call spbtrf ( 'L', n, nband, l, nband+1, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
        return
      end if
c
c  Print the relevant entries of L:
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The lower Cholesky factor L:'
      write ( *, '(a)' ) ' '

      do i = 1, n
        do j = 1, n

          if ( 0 .le. i - j .and. i-j .le. nband ) then
            l_row(j) = l(i-j+1,j)
          else
            l_row(j) = 0.0E+00
          end if

        end do

        write ( *, '(5f10.6)' ) ( l_row(j), j = 1, n )

      end do

      return
      end
      subroutine test10

c*********************************************************************72
c
cc TEST10 tests SPBTRF and SPBTRS.
c
c  Discussion:
c
c    The problem is just an enlarged version of the
c    problem for n = 5, which is:
c
c    Matrix A is ( 2 -1  0  0  0)    right hand side b is  (1)
c                (-1  2 -1  0  0)                          (0)
c                ( 0 -1  2 -1  0)                          (0)
c                ( 0  0 -1  2 -1)                          (0)
c                ( 0  0  0 -1  2)                          (1)
c
c
c    Solution is   (1)
c                  (1)
c                  (1)
c                  (1)
c                  (1)
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer nband
      integer lda

      parameter ( n = 25 )
      parameter ( nband = 1 )
      parameter ( lda = nband + 1 )

      real a(lda,n)
      real b(n)
      integer i
      integer info
      integer j 
      integer nrhs

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) '  For a single precision real matrix (S) in'
      write ( *, '(a)' ) '  positive definite band storage mode (PB):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For a positive definite symmetric band'
      write ( *, '(a)' ) '  matrix:'
      write ( *, '(a)' ) '  SPBTRF factors;'
      write ( *, '(a)' ) '  SPBTRS solves linear systems.'
      write ( *, '(a)' ) ' '
c
c  Zero out the matrix.
c
      do i = 1, lda
        do j = 1, n
          a(i,j) = 0.0E+00
        end do
      end do
c
c  Super (and sub) diagonal.
c
      do j = 2, n
        a(1,j) = -1.0E+00
      end do
c
c  Diagonal.
c
      do j = 1, n
        a(2,j) = 2.0E+00
      end do
c
c  Set the right hand side.
c
      b(1) = 1.0E+00
      do i = 2, n-1
        b(i) = 0.0E+00
      end do
      b(n) = 1.0E+00
c
c  Factor the matrix.
c
      call spbtrf ( 'u', n, nband, a, lda, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Factorization failed, INFO = ', info
        return
      end if
c
c  Solve the linear system.
c
      nrhs = 1
      call spbtrs ( 'u', n, nband, nrhs, a, lda, b, n, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Solution failed, INFO = ', info
      end if

      call r4vec_print_some ( n, b, 5, 
     &  '  Partial solution (all should be 1)' )

      return
      end
      subroutine test11

c*********************************************************************72
c
cc TEST11 tests SPOTRF and SPOTRI.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer lda

      parameter ( n = 5 )
      parameter ( lda = n )

      real a(n,n)
      real a_inv(n,n)
      integer i
      integer info
      integer j
      integer k
      real r(n,n)
      real temp(n,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in positive definite storage mode (PO):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SPOTRF computes the Cholesky factor '
      write ( *, '(a)' ) '    A = R''*R;'
      write ( *, '(a)' ) '  SPOTRI computes the inverse.'
      write ( *, '(a)' ) ' '
c
c  Zero out the matrix.
c
      do i = 1, n
        do j = 1, n
          a(i,j) = 0.0E+00
        end do
      end do
c
c  Subdiagonal.
c
      do i = 2, n
        a(i,i-1) = -1.0E+00
      end do
c
c  Diagonal.
c
      do i = 1, n
        a(i,i) = 2.0E+00
      end do
c
c  Superdiagonal.
c
      do i = 1, n - 1
        a(i,i+1) = -1.0E+00
      end do

      call r4mat_print ( n, n, a, '  The matrix A:' )
c
c  Factor the matrix.
c
      do i = 1, n
        do j = 1, n
          r(i,j) = a(i,j)
        end do
      end do

      call spotrf ( 'u', n, r, lda, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a,i8)' ) '  SPOTRF returns INFO = ', info
        return
      end if

      do i = 1, n
        do j = 1, i-1
          r(i,j) = 0.0E+00
        end do
      end do

      call r4mat_print ( n, n, r, '  The Cholesky factor R:' )

      do i = 1, n
        do j = 1, n
          temp(i,j) = 0.0E+00
          do k = 1, n
            temp(i,j) = temp(i,j) + r(i,k) * r(k,j)
          end do
        end do
      end do

      call r4mat_print ( n, n, temp, '  The product R'' * R' )
c
c  Compute the inverse matrix.
c
      do i = 1, n
        do j = 1, n
          a_inv(i,j) = r(i,j)
        end do
      end do

      call spotri ( 'u', n, a_inv, lda, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  The inversion procedure failed, INFO = ', info
        return
      end if

      do i = 1, n
        do j = 1, i-1
          a_inv(i,j) = a_inv(j,i)
        end do
      end do

      call r4mat_print ( n, n, a_inv, '  The inverse matrix B:' )

      do i = 1, n
        do j = 1, n
          temp(i,j) = 0.0E+00
          do k = 1, n
            temp(i,j) = temp(i,j) + a_inv(i,k) * a(k,j)
          end do
        end do
      end do

      call r4mat_print ( n, n, temp, '  The product B * A' )

      return
      end
      subroutine test12

c*********************************************************************72
c
cc TEST12 tests SGEQRF and SORGQR.
c
c  Discussion:
c
c    SGEQRF computes the QR factorization of an M by N matrix A:
c
c      A(MxN) = Q(MxK) * R(KxN)
c
c    where K = min ( M, N ).
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer k
      integer lwork

      parameter ( m = 8 )
      parameter ( n = 6 )
c     parameter ( k = min ( m, n ) )
      parameter ( k = 6 )
      parameter ( lwork = n )

      real a(m,n)
      integer i
      integer info
      integer j
      integer l
      integer lda
      real q(m,k)
      real r(k,n)
      integer seed
      real tau(k)
      real work(lwork)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST12'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGEQRF computes the QR factorization:'
      write ( *, '(a)' ) '    A = Q * R'
      write ( *, '(a)' ) '  SORGQR computes the explicit form of '
      write ( *, '(a)' ) '  the Q factor.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In this case, our M x N matrix A has more'
      write ( *, '(a)' ) '  rows than columns:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  M = ', m
      write ( *, '(a,i8)' ) '  N = ', n
c
c  Set A.
c
      call r4mat_uniform_01 ( m, n, seed, a )

      call r4mat_print ( m, n, a, '  The matrix A:' )
c
c  Compute the QR factorization.
c
      lda = m

      call sgeqrf ( m, n, a, lda, tau, work, lwork, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SGEQRF returned nonzero INFO = ', info
        return
      end if

      do i = 1, k
        do j = 1, n
          r(i,j) = 0.0E+00
        end do
      end do

      do i = 1, k
        do j = i, n
          r(i,j) = a(i,j)
        end do
      end do
c
c  Construct Q explicitly.
c
      do i = 1, m
        do j = 1, k
          q(i,j) = a(i,j)
        end do
      end do

      call sorgqr ( m, k, k, q, lda, tau, work, lwork, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SORGQR returned nonzero INFO = ', info
        return
      end if

      call r4mat_print ( m, k, q, '  The Q factor:' )

      call r4mat_print ( k, n, r, '  The R factor:' )

      do i = 1, m
        do j = 1, n
          a(i,j) = 0.0E+00
          do l = 1, k
            a(i,j) = a(i,j) + q(i,l) * r(l,j)
          end do
        end do
      end do

      call r4mat_print ( m, n, a, '  The product Q * R:' )

      return
      end
      subroutine test13

c*********************************************************************72
c
cc TEST13 tests SGEQRF and SORGQR.
c
c  Discussion:
c
c    SGEQRF computes the QR factorization of an M by N matrix A:
c
c      A(MxN) = Q(MxK) * R(KxN)
c
c    where K = min ( M, N ).
c
c    In order to get an M x M matrix Q, we are going to pad our
c    original matrix A with extra columns of zeroesc
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer n2
      integer k
      integer lwork

      parameter ( m = 8 )
      parameter ( n = 6 )
      parameter ( n2 = 8 )
!     parameter ( k = min ( m, n2 ) )
      parameter ( k = 8 )
      parameter ( lwork = n2 )

      real a(m,n)
      real a2(m,n2)
      integer i
      integer info
      integer j
      integer l
      integer lda
      real q(m,k)
      real r(k,n2)
      integer seed
      real tau(k)
      real work(lwork)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST13'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGEQRF computes the QR factorization:'
      write ( *, '(a)' ) '    A = Q * R'
      write ( *, '(a)' ) '  SORGQR computes the explicit form of '
      write ( *, '(a)' ) '  the Q factor.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In this case, our M x N matrix A has more'
      write ( *, '(a)' ) '  rows than columns:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  M = ', m
      write ( *, '(a,i8)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Normally, LAPACK will only return an'
      write ( *, '(a)' ) '  M x min(M,N) portion of Q.  When N .lt. M, '
      write ( *, '(a)' ) '  we lose information.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Here, we force the computation of the '
      write ( *, '(a)' ) '  full Q by making a copy of A with N-M '
      write ( *, '(a)' ) '  extra zero columns.'
      write ( *, '(a)' ) ' '
c
c  Set A.
c
      call r4mat_uniform_01 ( m, n, seed, a )

      call r4mat_print ( m, n, a, '  The matrix A:' )
c
c  Copy A, and pad with extra columns.
c
      do i = 1, m
        do j = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      do i = 1, m
        do j = n+1, m
          a2(i,j) = 0.0E+00
        end do
      end do
c
c  Compute the QR factorization.
c
      lda = m

      call sgeqrf ( m, n2, a2, lda, tau, work, lwork, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SGEQRF returned nonzero INFO = ', info
        return
      end if

      do i = 1, k
        do j = 1, n2
          r(i,j) = 0.0E+00
        end do
      end do

      do i = 1, k
        do j = i, n2
          r(i,j) = a2(i,j)
        end do
      end do
c
c  Construct Q explicitly.
c
      do i = 1, m
        do j = 1, k
          q(i,j) = a2(i,j)
        end do
      end do

      call sorgqr ( m, k, k, q, lda, tau, work, lwork, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SORGQR returned nonzero INFO = ', info
        return
      end if

      call r4mat_print ( m, k, q, '  The Q factor:' )

      call r4mat_print ( k, n, r, '  The R factor:' )

      do i = 1, m
        do j = 1, n2
          a2(i,j) = 0.0E+00
          do l = 1, k
            a2(i,j) = a2(i,j) + q(i,l) * r(l,j)
          end do
        end do
      end do

      call r4mat_print ( m, n2, a2, '  The product Q * R:' )

      return
      end
      subroutine test14

c*********************************************************************72
c
cc TEST14 tests SGEQRF and SORGQR.
c
c  Discussion:
c
c    SGEQRF computes the QR factorization of an M by N matrix A:
c
c      A(MxN) = Q(MxK) * R(KxN)
c
c    where K = min ( M, N ).
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer k
      integer lwork

      parameter ( m = 6 )
      parameter ( n = 8 )
c     parameter ( k = min ( m, n ) )
      parameter ( k = 6 )
      parameter ( lwork = n )

      real a(m,n)
      integer i
      integer info
      integer j
      integer l
      integer lda
      real q(m,k)
      real r(k,n)
      integer seed
      real tau(k)
      real work(lwork)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST14'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in general storage mode (GE):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SGEQRF computes the QR factorization:'
      write ( *, '(a)' ) '    A = Q * R'
      write ( *, '(a)' ) '  SORGQR computes the explicit form of'
      write ( *, '(a)' ) '  the Q factor.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In this case, our M x N matrix A has more'
      write ( *, '(a)' ) '  columns than rows:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  M = ', m
      write ( *, '(a,i8)' ) '  N = ', n
c
c  Set A.
c
      call r4mat_uniform_01 ( m, n, seed, a )

      call r4mat_print ( m, n, a, '  The matrix A:' )
c
c  Compute the QR factorization.
c
      lda = m

      call sgeqrf ( m, n, a, lda, tau, work, lwork, info )
 
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SGEQRF returned nonzero INFO = ', info
        return
      end if

      do i = 1, k
        do j = 1, n
          r(i,j) = 0.0E+00
        end do
      end do

      do i = 1, k
        do j = i, n
          r(i,j) = a(i,j)
        end do
      end do
c
c  Construct Q explicitly.
c
      do i = 1, m
        do j = 1, k
          q(i,j) = a(i,j)
        end do
      end do

      call sorgqr ( m, k, k, q, lda, tau, work, lwork, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SORGQR returned nonzero INFO = ', info
        return
      end if

      call r4mat_print ( m, k, q, '  The Q factor:' )

      call r4mat_print ( k, n, r, '  The R factor:' )

      do i = 1, m
        do j = 1, n
          a(i,j) = 0.0E+00
          do l = 1, k
            a(i,j) = a(i,j) + q(i,l) * r(l,j)
          end do
        end do
      end do

      call r4mat_print ( m, n, a, '  The product Q*R' )

      return
      end
      subroutine test15

c*********************************************************************72
c
cc TEST15 tests SSBGVX.
c
c  Discussion:
c
c    SSBGVX deals with the generalized eigenvalue problem:
c
c      A * x = lambda * B * x
c
c    where A and B are symmetric and banded (and stored in LAPACK symmetric
c    band storage mode).  B is additionally assumed to be positive definite.
c
c    This is an "expert" interface, and the user is requesting
c    only some of the eigenvalues and eigenvectors.  In this example,
c    only the largest and smallest (in magnitude) eigenvalues will
c    be requested.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    real AB(LDAB,N), contains, on input, the upper or lower
c    triangle of the symmetric band matrix A, stored in the first KA+1 rows
c    of the array AB.  
c    If UPLO = 'U', then
c      AB(KA+1+I-J,J) = A(I,J) for max(1,J-KA) .le. I .le. J;
c    If UPLO = 'L', then
c      AB(1+I-J,J) = A(I,J) for J .le. I .le. min(N,J+KA).
c
c    real ABSTOL, the absolute error tolerance for the eigenvalues.
c    If the input value of ABSTOL is not positive, then an appropriate
c    value will be determined internally and used instead.
c
c    real BB(LDBB,N), contains, on input, the upper or lower
c    triangle of the positive definite symmetric band matrix B, stored in 
c    the first KB+1 rows of the array BB.  
c    If UPLO = 'U', then
c      BB(KB+1+I-J,J) = B(I,J) for max(1,J-KB) .le. I .le. J;
c    If UPLO = 'L', then
c      BB(1+I-J,J) = B(I,J) for J .le. I .le. min(N,J+KB).
c
c    integer IFAIL(N), if JOBZ = 'V', then if INFO is 0, the first
c    M elements of IFAIL have been set to zero by DSBGVX, but if INFO
c    is nonzero, IFAIL contains the indices of the eigenvalues
c    for which the eigenvectors failed to converge.  If JOBZ = 'N',
c    then IFAIL is not referenced.
c
c    integer IL, IU, the indices of the first (smallest) and last
c    (largest) eigenvalues to be returned.  These values are only
c    used if RANGE = 'I'.  It must be the case that 1 .le. IL .le. IU .le. N.
c
c    Integer INFO, is 0 for a successful computation, 
c    negative if an input argument was illegal (the index of that
c    argument is the value of -INFO), or positive, in which case,
c    if 0 .lt. INFO .le. N, then INFO off diagonal elements of an
c    intermediate tridiagonal form did not converge to zero, or
c    if N .lt. INFO, B is not positive definite and the computation
c    could not be completed.
c
c    integer IWORK(5*N), workspace.
c
c    character JOBZ, is 'N' if only eigenvalues are desired, or 'V'
c    if eigenvectors will also be required.
c
c    Integer KA, the number of superdiagonals (if UPLO = 'U') or
c    subdiagonals (if UPLO = 'L') of A that are nonzero.
c
c    integer KB, the number of superdiagonals (if UPLO = 'U') or
c    subdiagonals (if UPLO = 'L') of B that are nonzero.
c
c    integer LDAB, the leading dimension of the array AB, which
c    must be at least KA+1.
c
c    integer LDBB, the leading dimension of the array BB, which
c    must be at least KB+1.
c
c    integer LDQ, the leading dimension of the array Q.
c    If JOBZ = 'N', then Q is not used, and LDQ should be any
c    positive value such as 1.  If JOBZ = 'V', then LDQ must be
c    at least N.
c
c    integer LDZ, the leading dimension of the array Z.
c    If JOBZ = 'N', then Z is not used, and LDZ should be any
c    positive value such as 1.  If JOBZ = 'V', then LDZ must be
c    at least N.
c
c    integer M, the number of eigenvalues found by DSBGVX.
c
c    integer N, the order of the matrices A and B.
c
c    real Q(LDQ,N), if JOBZ = 'V', the N by N matrix used to 
c    reduce the problem to standard form: "C * x = lambda * x"
c    and then to reduce the matrix C to tridiagonal form.  But
c    if JOBZ is not 'V', Q is not referenced.
c
c    character RANGE, specifies which eigenvalues are desired.
c    'A' means all, 'V' means a real interval will be specified in which
c    eigenvalues are to be sought, 'I' means a range of indices will
c    be specified.
c
c    character UPLO, is 'U' if the upper triangles of A and B are stored,
c    'L' if the lower triangles are stored.
c
c    real VL, VU, the lower and upper bounds of an interval to be
c    searched for eigenvalues.  In this case, VL must be less than VU.  
c    These values are used only if RANGE = 'V'.
c
c    real W(N), the requested eigenvalues, in ascending order.
c
c    real WORK(7*N), workspace.
c
c    real Z(LDZ,N), if JOBZ = 'V', the I-th column of Z contains
c    the eigenvector associated with the I-th eigenvalue W(I).
c
      implicit none

      integer n
      integer ka
      integer kb
      integer ldab
      integer ldbb
      integer ldq
      integer ldz

      parameter ( n = 11 )
      parameter ( ka = 2 )
      parameter ( kb = 1 )
      parameter ( ldab = ka+1 )
      parameter ( ldbb = kb+1 )
      parameter ( ldq = 1 )
      parameter ( ldz = 1 )

      real ab(ldab,n)
      real abstol
      real bb(ldbb,n)
      integer i
      integer ifail(n)
      integer il
      integer ilo
      integer info
      integer iu
      integer iwork(5*n)
      integer j
      character jobz
      integer m
      real q(ldq,n)
      character range
      integer test
      character uplo
      real value
      real vl
      real vu
      real w(n)
      real work(7*n)
      real z(ldz,n)

      abstol = 0.0E+00
      jobz = 'N'
      range = 'I'
      uplo = 'U'
      vl = 0.0E+00  
      vu = 1.0E+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST15'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in symmetric band storage mode (SB):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For a symmetric banded NxN matrix A, '
      write ( *, '(a)' ) '  and a symmetric banded positive definite '
      write ( *, '(a)' ) '  NxN matrix B, SSBGVX solves the '
      write ( *, '(a)' ) '  generalized eigenvalue problem'
      write ( *, '(a)' ) '    A * X = LAMBDA * B * X'
      write ( *, '(a)' ) ' '

      do test = 1, 2
c
c  Set A.
c
        do j = 1, n
          ilo = max ( j - ka, 1 )
          do i = ilo, j

            if ( j .eq. i-2 ) then
              value = -1.0E+00
            else if ( j .eq. i-1 ) then
              value = -1.0E+00
            else if ( j .eq. i ) then
              value = +4.0E+00
            else if ( j .eq. i+1 ) then
              value = -1.0E+00
            else if ( j .eq. i+2 ) then
              value = -1.0E+00
            else
              value = 0.0E+00
            end if

            ab(ka+1+i-j,j) = value

          end do
        end do
c
c  Set B.
c
        do j = 1, n
          ilo = max ( j - kb, 1 )
          do i = ilo, j

            if ( j .eq. i-1 ) then
              value = -1.0E+00
            else if ( j .eq. i ) then
              value = +2.0E+00
            else if ( j .eq. i+1 ) then
              value = -1.0E+00
            else
              value = 0.0E+00
            end if

            bb(kb+1+i-j,j) = value

          end do
        end do
c
c  Request the value of the SMALLEST or LARGEST eigenvalue:
c
        if ( test .eq. 1 ) then
          il = 1
          iu = 1
        else if ( test .eq. 2 ) then
          il = n
          iu = n
        end if

        call ssbgvx ( jobz, range, uplo, n, ka, kb, ab, ldab, bb, 
     &    ldbb, q, ldq, vl, vu, il, iu, abstol, m, w, z, ldz, work, 
     &    iwork, ifail, info )
 
        if ( info .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a,i8)' ) 
     &      '  Illegal value for input argument ', -info
          return
        else if ( 0 .lt. info ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  The eigenvalue or eigenvector'
          write ( *, '(a)' ) '  iterations did not converge.'
          cycle
        end if

        call r4vec_print ( m, w, '  Computed eigenvalues' )

      end do

      return
      end
      subroutine test16

c*********************************************************************72
c
cc TEST16 tests SSYEV.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer lwork

      parameter ( n = 7 )
      parameter ( lwork = 3 * n - 1 )

      real a(n,n)
      integer info
      character jobz
      real lambda(n)
      character uplo
      real work(lwork)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST16'
      write ( *, '(a)' ) '  For a single precision real matrix (S)'
      write ( *, '(a)' ) '  in symmetric storage mode (SY):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For a symmetric matrix in general storage,'
      write ( *, '(a)' ) '  SSYEV computes eigenvalues and '
      write ( *, '(a)' ) '  eigenvectors;'
      write ( *, '(a)' ) ' '
c
c  Set A.
c
      call clement2 ( n, a )

      call r4mat_print ( n, n, a, '  The matrix A:' )
c
c  Compute the eigenvalues and eigenvectors.
c
      jobz = 'V'
      uplo = 'U'

      call ssyev ( jobz, uplo, n, a, n, lambda, work, lwork, info )
 
      if ( info .ne. 0 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  SSYEV returned nonzero INFO = ', info

      else

        call r4vec_print ( n, lambda, '  The eigenvalues:' )

        if ( jobz .eq. 'V' ) then
          call r4mat_print ( n, n, a, '  The eigenvector matrix:' )
        end if
      
      end if

      return
      end
      subroutine clement2 ( n, a )

c*********************************************************************72
c
cc CLEMENT2 returns the Clement2 matrix.
c
c  Formula:
c
c    if ( J = I+1 )
c      A(I,J) = sqrt(I*(N-I))
c    else if ( I = J+1 )
c      A(I,J) = sqrt(J*(N-J))
c    else
c      A(I,J) = 0
c
c  Example:
c
c    N = 5
c
c       .    sqrt(4)    .       .       .
c    sqrt(4)    .    sqrt(6)    .       .
c       .    sqrt(6)    .    sqrt(6)    .
c       .       .    sqrt(6)    .    sqrt(4)
c       .       .       .    sqrt(4)    .
c
c  Properties:
c
c    A is tridiagonal.
c
c    Because A is tridiagonal, it has property A (bipartite).
c
c    A is symmetric: A' = A.
c
c    Because A is symmetric, it is normal.
c
c    Because A is normal, it is diagonalizable.
c
c    A is persymmetric: A(I,J) = A(N+1-J,N+1-I).
c
c    The diagonal of A is zero.
c
c    A is singular if N is odd.
c
c    About 64 percent of the entries of the inverse of A are zero.
c
c    The eigenvalues are plus and minus the numbers
c
c      N-1, N-3, N-5, ..., (1 or 0).
c
c    If N is even,
c
c      det ( A ) = (-1)**(N/2) * (N-1) * (N+1)**(N/2)
c
c    and if N is odd,
c
c      det ( A ) = 0
c
c  Reference:
c
c    P A Clement,
c    A class of triple-diagonal matrices for test purposes,
c    SIAM Review,
c    Volume 1, 1959, pages 50-52.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of A.
c
c    Output, real A(N,N), the matrix.
c
      implicit none

      integer n

      real a(n,n)
      integer i
      integer j

      do i = 1, n
        do j = 1, n

          if ( j .eq. i + 1 ) then
            a(i,j) = sqrt ( real ( i * ( n - i ) ) )
          else if ( i .eq. j + 1 ) then
            a(i,j) = sqrt ( real ( j * ( n - j ) ) )
          else
            a(i,j) = 0.0E+00
          end if

        end do
      end do

      return
      end
      function r4mat_norm_li ( m, n, a )

c*********************************************************************72
c
cc R4MAT_NORM_LI returns the matrix L-infinity norm of an R4MAT.
c
c  Definition:
c
c    The matrix L-infinity norm is defined as:
c
c      R4MAT_NORM_LI =  Max ( 1 .le. I .le. M ) Sum ( 1 .le. J .le. N ) abs ( A(I,J) ).
c
c    The matrix L-infinity norm is derived from the vector L-infinity norm,
c    and satisifies:
c
c      R4VEC_NORM_LI ( A*x ) .le. R4MAT_NORM_LI ( A ) * R4VEC_NORM_LI ( x ).
c
c  Modified:
c
c    12 July 2008
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
c    Input, real A(M,N), the matrix whose L-infinity norm is
c    desired.
c
c    Output, real R4MAT_NORM_LI, the L-infinity norm of A.
c
      implicit none

      integer m
      integer n

      real a(m,n)
      integer i
      integer j
      real r4mat_norm_li
      real temp

      r4mat_norm_li = 0.0E+00

      do i = 1, m
        temp = 0.0E+00
        do j = 1, n
          temp = temp + abs ( a(i,j) )
        end do
        r4mat_norm_li = max ( r4mat_norm_li, temp )
      end do

      return
      end
      subroutine r4mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R4MAT_PRINT prints an R4MAT.
c
c  Modified:
c
c    12 July 2008
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
c    Input, real A(M,N), the matrix.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer m
      integer n

      real a(m,n)
      character * ( * ) title

      call r4mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc R4MAT_PRINT_SOME prints some of an R4MAT.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, real A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, an optional title.
c
      implicit none

      integer incx
      integer m
      integer n

      parameter ( incx = 5 )

      real a(m,n)
      character ( len = 14 ) ctemp(incx)
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
      write ( *, '(a)' ) title

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

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc)
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( a(i,j) .eq. real ( int ( a(i,j) ) ) ) then
              write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
            else
              write ( ctemp(j2), '(g14.6)' ) a(i,j)
            end if

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )

        end do

      end do

      write ( *, '(a)' ) ' '

      return
      end
      subroutine r4mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R4MAT_UNIFORM_01 fills an array with unit pseudorandom numbers.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, L E Schrage,
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
c    P A Lewis, A S Goodman, J M Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, real R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      real r(m,n)

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = real ( seed ) * 4.656612875E-10

        end do
      end do

      return
      end
      subroutine r4vec_print ( n, a, title )

c*********************************************************************72
c
cc R4VEC_PRINT prints an R4VEC.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, real A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, an optional title.
c
      implicit none

      integer n

      real a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title
      write ( *, '(a)' ) ' '

      do i = 1, n
        write ( *, '(2x,i8,g16.8)' ) i, a(i)
      end do

      return
      end
      subroutine r4vec_print_some ( n, a, max_print, title )

c*********************************************************************72
c
cc R4VEC_PRINT_SOME prints "some" of an R4VEC.
c
c  Discussion:
c
c    The user specifies MAX_PRINT, the maximum number of lines to print.
c
c    If N, the size of the vector, is no more than MAX_PRINT, then
c    the entire vector is printed, one entry per line.
c
c    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
c    followed by a line of periods suggesting an omission,
c    and the last entry.
c
c  Modified:
c
c    12 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries of the vector.
c
c    Input, real A(N), the vector to be printed.
c
c    Input, integer MAX_PRINT, the maximum number of lines to print.
c
c    Input, character * ( * ) TITLE, an optional title.
c
      implicit none

      integer n

      real a(n)
      integer i
      integer max_print
      character * ( * ) title

      if ( max_print .le. 0 ) then
        return
      end if

      if ( n .le. 0 ) then
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title
      write ( *, '(a)' ) ' '

      if ( n .le. max_print ) then

        do i = 1, n
          write ( *, '(2x,i8,2x,g14.6)' ) i, a(i)
        end do

      else if ( 3 .le. max_print ) then

        do i = 1, max_print-2
          write ( *, '(2x,i8,2x,g14.6)' ) i, a(i)
        end do
        write ( *, '(a)' ) '  ......  ..............'
        i = n
        write ( *, '(2x,i8,2x,g14.6)' ) i, a(i)

      else

        do i = 1, max_print - 1
          write ( *, '(2x,i8,2x,g14.6)' ) i, a(i)
        end do
        i = max_print
        write ( *, '(2x,i8,2x,g14.6,2x,a)' ) 
     &    i, a(i), '...more entries...'

      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
c
c  Modified:
c
c    16 September 2005
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

      character ( len = 8 ) date
      character ( len = 10 ) time

      call date_and_time ( date, time )

      write ( *, '(a8,2x,a10)' ) date, time

      return
      end
