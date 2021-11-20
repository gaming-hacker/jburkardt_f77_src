      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS2_D_PRB.
c
c  Discussion:
c
c    BLAS2_D_PRB tests the BLAS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS2_D_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BLAS library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS2_D_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests DGEMV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mn_max
      parameter ( mn_max = 5 )

      double precision a(mn_max,mn_max)
      double precision alpha
      double precision beta
      integer i
      integer incx
      integer incy
      integer j
      integer lda
      integer m
      integer n
      character trans
      double precision x(mn_max)
      double precision y(mn_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For a general matrix A,'
      write ( *, '(a)' ) 
     &  '  DGEMV computes y := alpha * A * x + beta * y'
      write ( *, '(a)' ) 
     &  '  or             y := alpha * A'' * x + beta * y.'
c
c  y = alpha * A * x + beta * y
c
      trans = 'N'
      m = 5
      n = 4
      alpha = 2.0D+00
      lda = m
      call r8mat_test ( trans, lda, m, n, a )
      do i = 1, n
        x(i) = dble ( i )
      end do
      incx = 1
      beta = 3.0D+00
      do i = 1, m
        y(i) = dble ( 10 * i )
      end do
      incy = 1

      call r8mat_print ( m, n, a, '  Matrix A:' )
      call r8vec_print ( n, x, '  Vector X:' )
      call r8vec_print ( m, y, '  Vector Y:' )

      call dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

      call r8vec_print ( m, y, 
     &  '  Result Y = alpha * A  * x + beta * y' )
c
c  y = alpha * A' * x + beta * y
c
      trans = 'T'
      m = 5
      n = 4
      alpha = 2.0D+00
      lda = m
      call r8mat_test ( trans, lda, n, m, a )
      do i = 1, m
        x(i) = dble ( i )
      end do
      incx = 1
      beta = 3.0D+00
      do i = 1, n
        y(i) = dble ( 10 * i )
      end do
      incy = 1

      call r8mat_print ( m, n, a, '  Matrix A:' )
      call r8vec_print ( m, x, '  Vector X:' )
      call r8vec_print ( n, y, '  Vector Y:' )

      call dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

      call r8vec_print ( n, y, 
     &  '  Result Y = alpha * A'' * x + beta * y' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests DGBMV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2005
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer kl
      integer ku
      integer lda

      parameter ( m = 5 )
      parameter ( n = 5 )
      parameter ( kl = 1 )
      parameter ( ku = 1 )
      parameter ( lda = kl + 1 + ku )

      double precision a(lda,n)
      double precision alpha
      double precision beta
      integer i
      integer incx
      integer incy
      integer j
      integer jhi
      integer jlo
      character trans
      double precision x(n)
      double precision y(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For a general band matrix A,'
      write ( *, '(a)' ) '  DGBMV computes '
      write ( *, '(a)' ) '  y := alpha * A * x + beta * y'

      trans = 'N'
      alpha = 2.0D+00
      incx = 1
      beta = 3.0D+00
      incy = 1

      do i = 1, m

        jlo = max ( 1, i - kl )
        jhi = min ( n, i + ku )

        do j = jlo, jhi

          if ( i == j ) then
            a(ku+1+i-j,j) = 2.0D+00
          else if ( i == j - 1 .or. i == j + 1 ) then
            a(ku+1+i-j,j) = -1.0D+00
          else
            a(ku+1+i-j,j) = 0.0D+00
          end if

        end do
      end do

      do i = 1, n
        x(i) = dble ( i )
      end do

      do i = 1, m
        y(i) = dble ( 10 * i )
      end do

      call dgbmv ( trans, m, n, kl, ku, alpha, a, lda, x,
     &  incx, beta, y, incy )

      call r8vec_print ( m, y, '  Result vector Y' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests DSYMV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2005
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

      double precision a(lda,n)
      double precision alpha
      double precision beta
      integer i
      integer incx
      integer incy
      integer j
      character uplo
      double precision x(n)
      double precision y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For a general symmetric matrix A,'
      write ( *, '(a)' ) '  DSYMV computes '
      write ( *, '(a)' ) '  y := alpha * A * x + beta * y'

      uplo = 'U'
      alpha = 2.0D+00
      incx = 1
      beta = 3.0D+00
      incy = 1

      do i = 1, n
        do j = 1, n
          if ( i == j ) then
            a(i,j) = 2.0D+00
          else if ( i == j - 1 ) then
            a(i,j) = -1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      do i = 1, n
        x(i) = dble ( i )
      end do

      do i = 1, n
        y(i) = dble ( 10 * i )
      end do

      call dsymv ( uplo, n, alpha, a, lda, x, incx, beta, y, incy )

      call r8vec_print ( n, y, '  Result vector Y' )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests DSBMV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2005
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer k
      integer lda

      parameter ( m = 5 )
      parameter ( n = 5 )
      parameter ( k = 1 )
      parameter ( lda = k + 1 )

      double precision a(lda,n)
      double precision alpha
      double precision beta
      integer i
      integer incx
      integer incy
      integer j
      integer jhi
      integer jlo
      character uplo
      double precision x(n)
      double precision y(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  For a symmetric band matrix A,'
      write ( *, '(a)' ) '  DSBMV computes '
      write ( *, '(a)' ) '  y := alpha * A * x + beta * y'

      uplo = 'U'
      alpha = 2.0D+00
      incx = 1
      beta = 3.0D+00
      incy = 1

      do i = 1, m

        jhi = min ( n, i + k )

        do j = i, jhi

          if ( i == j ) then
            a(k+1+i-j,j) = 2.0D+00
          else if ( i == j - 1 ) then
            a(k+1+i-j,j) = -1.0D+00
          else
            a(k+1+i-j,j) = 0.0D+00
          end if

        end do
      end do

      do i = 1, n
        x(i) = dble ( i )
      end do

      do i = 1, m
        y(i) = dble ( 10 * i )
      end do

      call dsbmv ( uplo, n, k, alpha, a, lda, x, incx, beta, y, incy )

      call r8vec_print ( m, y, '  Result vector Y' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests DGER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      integer lda
      parameter ( lda = m )

      double precision a(lda,n)
      double precision alpha
      integer i
      integer incx
      integer incy
      character trans
      double precision x(m)
      double precision y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  For a general matrix A,'
      write ( *, '(a)' ) '  DGER computes A := A + alpha * x * y'''

      alpha = 2.0D+00
      trans = 'N'
      call r8mat_test ( trans, lda, m, n, a )

      do i = 1, m
        x(i) = dble ( i )
      end do
      incx = 1

      do i = 1, n
        y(i) = dble ( 10 * i )
      end do
      incy = 1

      call r8mat_print ( m, n, a, '  Matrix A:' )
      call r8vec_print ( m, x, '  Vector X:' )
      call r8vec_print ( n, y, '  Vector Y:' )

      call dger ( m, n, alpha, x, incx, y, incy, a, lda )

      call r8mat_print ( m, n, a, '  Result A = A + alpha * x * y' )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests DTRMV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )

      integer lda
      parameter ( lda = m )
      integer n
      parameter ( n = m )

      double precision a(lda,n)
      character diag
      integer i
      integer incx
      integer j
      integer test
      character trans
      character uplo
      double precision x(n)
      double precision y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  For a triangular matrix A,'
      write ( *, '(a)' ) '  DTRMV computes y := A * x or y := A'' * x'

      do test = 1, 2

        uplo = 'U'

        if ( test .eq. 1 ) then
          trans = 'N'
        else
          trans = 'T'
        end if

        diag = 'N'

        do j = 1, n
          do i = 1, j
            a(i,j) = dble ( i + j )
          end do
          do i = j + 1, m
            a(i,j) = 0.0D+00
          end do
        end do

        incx = 1
        do i = 1, n
          x(i) = dble ( i )
        end do

        call dtrmv ( uplo, trans, diag, n, a, lda, x, incx )

        if ( trans .eq. 'N' ) then
          call r8vec_print ( n, x, '  Result y = A * x' );
        else
          call r8vec_print ( n, x, '  Result y = A'' * x' );
        end if

      end do

      return
      end
