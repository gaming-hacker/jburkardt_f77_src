      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_EIGEN_TEST.
c
c  Discussion:
c
c    TEST_EIGEN_TEST tests the TEST_EIGEN library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_EIGEN_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_EIGEN library.'

      call r8symm_gen_test ( )
      call r8nsymm_gen_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_EIGEN_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)') ' '
      call timestamp ( )

      stop 0
      end
      subroutine r8symm_gen_test ( )

c*********************************************************************72
c
cc R8SYMM_GEN_TEST tests R8SYMM_GEN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 100 )
      integer bin_num
      parameter ( bin_num = 10 )

      double precision a(n,n)
      double precision aq(n,n)
      integer bin(0:bin_num+1)
      double precision bin_limit(0:bin_num)
      integer I
      integer j
      integer k
      double precision lambda(n)
      double precision lambda2(n)
      double precision lambda_dev
      parameter ( lambda_dev = 1.0D+00 )
      double precision lambda_max
      double precision lambda_mean
      parameter ( lambda_mean = 1.0D+00 )
      double precision lambda_min
      double precision q(n,n)
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8SYMM_GEN_TEST'
      write ( *, '(a)' ) '  R8SYMM_GEN makes an arbitrary size '
      write ( *, '(a)' ) '  symmetric matrix with known eigenvalues '
      write ( *, '(a)' ) '  and eigenvectors.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Real data is declared as "DOUBLE PRECISION".'

      call r8symm_gen ( n, lambda_mean, lambda_dev, seed, a, q, 
     &  lambda )
c
c  Get the eigenvalue range.
c
      lambda_min = lambda(1)
      lambda_max = lambda(1)

      do i = 2, n
        lambda_min = min ( lambda_min, lambda(i) )
        lambda_max = max ( lambda_max, lambda(i) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
      write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max
c
c  Bin the eigenvalues.
c
      call r8vec_bin ( n, lambda, bin_num, lambda_min, lambda_max, bin, 
     &  bin_limit )

      call r8bin_print ( bin_num, bin, bin_limit, '  Lambda bins:' )

      if ( .false. ) then
        call r8mat_print ( n, n, a, '  The matrix A:' )
      end if

      if ( .false. ) then
        call r8mat_print ( n, n, q, '  The eigenvector matrix Q:' )
      end if

      do i = 1, n
        do j = 1, n
          aq(i,j) = 0.0D+00
          do k = 1, n
            aq(i,j) = aq(i,j) + a(i,k) * q(k,j)
          end do
        end do
      end do

      do j = 1, n
        lambda2(j) = 0.0D+00
        do i = 1, n
          lambda2(j) = lambda2(j) + aq(i,j) ** 2
        end do
        lambda2(j) = sqrt ( lambda2(j) )
      end do

      call r8vec2_print ( n, lambda, lambda2, 
     &  '  LAMBDA versus the column norms of A*Q:' )

      return
      end
      subroutine r8nsymm_gen_test ( )

c*********************************************************************72
c
cc R8NSYMM_GEN_TEST tests R8NSYMM_GEN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 100 )
      integer bin_num
      parameter ( bin_num = 10 )

      double precision a(n,n)
      double precision aqr(n,n)
      integer bin(0:bin_num+1)
      double precision bin_limit(0:bin_num)
      integer I
      integer j
      integer k
      double precision lambda(n)
      double precision lambda2(n)
      double precision lambda_dev
      parameter ( lambda_dev = 1.0D+00 )
      double precision lambda_max
      double precision lambda_mean
      parameter ( lambda_mean = 1.0D+00 )
      double precision lambda_min
      double precision ql(n,n)
      double precision qr(n,n)
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8NSYMM_GEN_TEST'
      write ( *, '(a)' ) '  R8NSYMM_GEN makes an arbitrary size '
      write ( *, '(a)' ) '  nonsymmetric matrix with known eigenvalues '
      write ( *, '(a)' ) '  and eigenvectors.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Real data is declared as "DOUBLE PRECISION".'

      call r8nsymm_gen ( n, lambda_mean, lambda_dev, seed, a, ql, 
     &  qr, lambda )
c
c  Get the eigenvalue range.
c
      lambda_min = lambda(1)
      lambda_max = lambda(1)

      do i = 2, n
        lambda_min = min ( lambda_min, lambda(i) )
        lambda_max = max ( lambda_max, lambda(i) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
      write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max
c
c  Bin the eigenvalues.
c
      call r8vec_bin ( n, lambda, bin_num, lambda_min, lambda_max, bin, 
     &  bin_limit )

      call r8bin_print ( bin_num, bin, bin_limit, '  Lambda bins:' )

      if ( .false. ) then
        call r8mat_print ( n, n, a, '  The matrix A:' )
      end if

      if ( .false. ) then
        call r8mat_print ( n, n, qr, 
     &    '  The right eigenvector matrix QR:' )
      end if

      do i = 1, n
        do j = 1, n
          aqr(i,j) = 0.0D+00
          do k = 1, n
            aqr(i,j) = aqr(i,j) + a(i,k) * qr(k,j)
          end do
        end do
      end do

      do j = 1, n
        lambda2(j) = 0.0D+00
        do i = 1, n
          lambda2(j) = lambda2(j) + aqr(i,j) ** 2
        end do
        lambda2(j) = sqrt ( lambda2(j) )
      end do

      call r8vec2_print ( n, lambda, lambda2, 
     &  '  LAMBDA versus the column norms of A*QR:' )

      return
      end
