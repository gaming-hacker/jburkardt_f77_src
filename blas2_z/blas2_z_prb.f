      program main

c*********************************************************************72
c
cc MAIN is the main program for BLAS2_Z_PRB.
c
c  Discussion:
c
c    BLAS2_Z_PRB tests the BLAS library.
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
      write ( *, '(a)' ) 'BLAS2_Z_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BLAS library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BLAS2_Z_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CGEMV.
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

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 5 )

      double complex a(m,n)
      double complex alpha
      double complex beta
      integer i
      integer incx
      integer incy
      integer j
      integer lda
      character trans
      double complex x(n)
      real x1
      real x2
      double complex y(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For a general matrix A,'
      write ( *, '(a)' ) 
     &  '  ZGEMV computes y := alpha * A * x + beta * y'
    
      trans = 'N'
      alpha = ( 10.0D+00, 1.0D+00 ) 
      lda = m
      incx = 1
      beta = 3.0D+00
      incy = 1

      do i = 1, m
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = ( 2.0D+00, 0.0D+00 )
          else if ( i .eq. j - 1 .or. i .eq. j + 1 ) then
            a(i,j) = ( -1.0D+00, 0.0D+00 )
          else
            a(i,j) = ( 0.0D+00, 0.0D+00 )
          end if
        end do
      end do

      x1 = 0.0D+00
      x2 = dble ( n )
      do i = 1, n
        x(i) = dcmplx ( x1, x2 )
        x1 = x1 + 1.0D+00
        x2 = x2 - 2.0D+00
      end do

      do i = 1, m
        y(i) = ( 100.0D+00, 1.0d+00 )
      end do

      call zgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Result vector Y = '
      write ( *, '(a)' ) ' '

      do i = 1, m
        write ( *, '(2x,2g14.6)' ) y(i)
      end do

      return
      end

