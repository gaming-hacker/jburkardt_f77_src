      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS2_S_PRB.
c
c  Discussion:
c
c    BLAS2_S_PRB tests the BLAS library.
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
      write ( *, '(a)' ) 'BLAS2_S_PRB'
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
      write ( *, '(a)' ) 'BLAS2_S_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests SGEMV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mn_max
      parameter ( mn_max = 5 )

      real a(mn_max,mn_max)
      real alpha
      real beta
      integer i
      integer incx
      integer incy
      integer j
      integer lda
      integer m
      integer n
      character trans
      real x(mn_max)
      real y(mn_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For a general matrix A,'
      write ( *, '(a)' ) 
     &  '  SGEMV computes y := alpha * A * x + beta * y'
      write ( *, '(a)' ) 
     &  '  or             y := alpha * A'' * x + beta * y.'
c
c  y = alpha * A * x + beta * y
c
      trans = 'N'
      m = 5
      n = 4
      alpha = 2.0E+00
      lda = m
      call r4mat_test ( trans, lda, m, n, a )
      do i = 1, n
        x(i) = dble ( i )
      end do
      incx = 1
      beta = 3.0E+00
      do i = 1, m
        y(i) = dble ( 10 * i )
      end do
      incy = 1

      call r4mat_print ( m, n, a, '  Matrix A:' )
      call r4vec_print ( n, x, '  Vector X:' )
      call r4vec_print ( m, y, '  Vector Y:' )

      call sgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

      call r4vec_print ( m, y, 
     &  '  Result Y = alpha * A  * x + beta * y' )
c
c  y = alpha * A' * x + beta * y
c
      trans = 'T'
      m = 5
      n = 4
      alpha = 2.0E+00
      lda = m
      call r4mat_test ( trans, lda, n, m, a )
      do i = 1, m
        x(i) = dble ( i )
      end do
      incx = 1
      beta = 3.0E+00
      do i = 1, n
        y(i) = dble ( 10 * i )
      end do
      incy = 1

      call r4mat_print ( m, n, a, '  Matrix A:' )
      call r4vec_print ( m, x, '  Vector X:' )
      call r4vec_print ( n, y, '  Vector Y:' )

      call sgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

      call r4vec_print ( n, y, 
     &  '  Result Y = alpha * A'' * x + beta * y' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests SGBMV.
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

      real a(lda,n)
      real alpha
      real beta
      integer i
      integer incx
      integer incy
      integer j
      integer jhi
      integer jlo
      character trans
      real x(n)
      real y(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For a general band matrix A,'
      write ( *, '(a)' ) '  SGBMV computes '
      write ( *, '(a)' ) '  y := alpha * A * x + beta * y'

      trans = 'N'
      alpha = 2.0E+00
      incx = 1
      beta = 3.0E+00
      incy = 1

      do i = 1, m

        jlo = max ( 1, i - kl )
        jhi = min ( n, i + ku )

        do j = jlo, jhi

          if ( i == j ) then
            a(ku+1+i-j,j) = 2.0E+00
          else if ( i == j - 1 .or. i == j + 1 ) then
            a(ku+1+i-j,j) = -1.0E+00
          else
            a(ku+1+i-j,j) = 0.0E+00
          end if

        end do
      end do

      do i = 1, n
        x(i) = real ( i )
      end do

      do i = 1, m
        y(i) = real ( 10 * i )
      end do

      call sgbmv ( trans, m, n, kl, ku, alpha, a, lda, x,
     &  incx, beta, y, incy )

      call r4vec_print ( m, y, '  Result vector Y' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests SSYMV.
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

      real a(lda,n)
      real alpha
      real beta
      integer i
      integer incx
      integer incy
      integer j
      character uplo
      real x(n)
      real y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For a general symmetric matrix A,'
      write ( *, '(a)' ) '  SSYMV computes '
      write ( *, '(a)' ) '  y := alpha * A * x + beta * y'

      uplo = 'U'
      alpha = 2.0E+00
      incx = 1
      beta = 3.0E+00
      incy = 1

      do i = 1, n
        do j = 1, n
          if ( i == j ) then
            a(i,j) = 2.0E+00
          else if ( i == j - 1 ) then
            a(i,j) = -1.0E+00
          else
            a(i,j) = 0.0E+00
          end if
        end do
      end do

      do i = 1, n
        x(i) = real ( i )
      end do

      do i = 1, n
        y(i) = real ( 10 * i )
      end do

      call ssymv ( uplo, n, alpha, a, lda, x, incx, beta, y, incy )

      call r4vec_print ( n, y, '  Result vector Y' )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests SSBMV.
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

      real a(lda,n)
      real alpha
      real beta
      integer i
      integer incx
      integer incy
      integer j
      integer jhi
      integer jlo
      character uplo
      real x(n)
      real y(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  For a symmetric band matrix A,'
      write ( *, '(a)' ) '  SSBMV computes '
      write ( *, '(a)' ) '  y := alpha * A * x + beta * y'

      uplo = 'U'
      alpha = 2.0E+00
      incx = 1
      beta = 3.0E+00
      incy = 1

      do i = 1, m

        jhi = min ( n, i + k )

        do j = i, jhi

          if ( i == j ) then
            a(k+1+i-j,j) = 2.0E+00
          else if ( i == j - 1 ) then
            a(k+1+i-j,j) = -1.0E+00
          else
            a(k+1+i-j,j) = 0.0E+00
          end if

        end do
      end do

      do i = 1, n
        x(i) = real ( i )
      end do

      do i = 1, m
        y(i) = real ( 10 * i )
      end do

      call ssbmv ( uplo, n, k, alpha, a, lda, x, incx, beta, y, incy )

      call r4vec_print ( m, y, '  Result vector Y' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests SGER.
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

      real a(lda,n)
      real alpha
      integer i
      integer incx
      integer incy
      character trans
      real x(m)
      real y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  For a general matrix A,'
      write ( *, '(a)' ) '  SGER computes A := A + alpha * x * y'''

      alpha = 2.0E+00
      trans = 'N'
      call r4mat_test ( trans, lda, m, n, a )

      do i = 1, m
        x(i) = real ( i )
      end do
      incx = 1

      do i = 1, n
        y(i) = real ( 10 * i )
      end do
      incy = 1

      call r4mat_print ( m, n, a, '  Matrix A:' )
      call r4vec_print ( m, x, '  Vector X:' )
      call r4vec_print ( n, y, '  Vector Y:' )

      call sger ( m, n, alpha, x, incx, y, incy, a, lda )

      call r4mat_print ( m, n, a, '  Result A = A + alpha * x * y' )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests STRMV.
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

      real a(lda,n)
      character diag
      integer i
      integer incx
      integer j
      integer test
      character trans
      character uplo
      real x(n)
      real y(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  For a triangular matrix A,'
      write ( *, '(a)' ) '  STRMV computes y := A * x or y := A'' * x'

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
            a(i,j) = real ( i + j )
          end do
          do i = j + 1, m
            a(i,j) = 0.0E+00
          end do
        end do

        incx = 1
        do i = 1, n
          x(i) = real ( i )
        end do

        call strmv ( uplo, trans, diag, n, a, lda, x, incx )

        if ( trans .eq. 'N' ) then
          call r4vec_print ( n, x, '  Result y = A * x' );
        else
          call r4vec_print ( n, x, '  Result y = A'' * x' );
        end if

      end do

      return
      end
