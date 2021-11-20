      program main

c*********************************************************************72
c
cc MAIN is the main program for JACOBI_EIGENVALUE_PRB.
c
c  Discussion:
c
c    JACOBI_EIGENVALUE_PRB tests the JACOBI_EIGENVALUE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'JACOBI_EIGENVALUE_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the JACOBI_EIGENVALUE library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'JACOBI_EIGENVALUE_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses a 4x4 test matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n 
      parameter ( n = 4 )

      double precision a(n,n)
      double precision d(n)
      double precision error_frobenius
      integer it_max
      integer it_num
      integer rot_num
      double precision v(n,n)

      save a

      data a /
     &   4.0,  -30.0,    60.0,   -35.0,
     & -30.0,  300.0,  -675.0,   420.0, 
     &  60.0, -675.0,  1620.0, -1050.0,
     & -35.0,  420.0, -1050.0,  700.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For a symmetric matrix A,'
      write ( *, '(a)' ) 
     &  '  JACOBI_EIGENVALUE computes the eigenvalues D'
      write ( *, '(a)' ) '  and eigenvectors V so that A * V = D * V.'

      call r8mat_print ( n, n, a, '  Input matrix A:' )

      it_max = 100

      call jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of iterations = ', it_num
      write ( *, '(a,i4)' ) '  Number of rotations  = ', rot_num

      call r8vec_print ( n, d, '  Eigenvalues D:' )

      call r8mat_print ( n, n, v, '  Eigenvector matrix V:' )
c
c  Compute eigentest.
c
      call r8mat_is_eigen_right ( n, n, a, v, d, error_frobenius )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' )
     &  '  Frobenius norm error in eigensystem A*V-D*V = ',
     &  error_frobenius


      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses a 4x4 test matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision d(n)
      double precision error_frobenius
      integer it_max
      integer it_num
      integer rot_num
      double precision v(n,n)

      save a

      data a /
     & 4.0, 0.0, 0.0, 0.0,
     & 0.0, 1.0, 0.0, 0.0,
     & 0.0, 0.0, 3.0, 0.0,   
     & 0.0, 0.0, 0.0, 2.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For a symmetric matrix A,'
      write ( *, '(a)' ) 
     &  '  JACOBI_EIGENVALUE computes the eigenvalues D'
      write ( *, '(a)' ) '  and eigenvectors V so that A * V = D * V.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'As a sanity check, input a diagonal matrix.'

      call r8mat_print ( n, n, a, '  Input matrix A:' )

      it_max = 100

      call jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of iterations = ', it_num
      write ( *, '(a,i4)' ) '  Number of rotations  = ', rot_num

      call r8vec_print ( n, d, '  Eigenvalues D:' )

      call r8mat_print ( n, n, v, '  Eigenvector matrix V:' )
c
c  Compute eigentest.
c
      call r8mat_is_eigen_right ( n, n, a, v, d, error_frobenius )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' )
     &  '  Frobenius norm error in eigensystem A*V-D*V = ',
     &  error_frobenius

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses a 5x5 test matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision d(n)
      double precision error_frobenius
      integer i
      integer it_max
      integer it_num
      integer j
      integer rot_num
      double precision v(n,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For a symmetric matrix A,'
      write ( *, '(a)' ) 
     &  '  JACOBI_EIGENVALUE computes the eigenvalues D'
      write ( *, '(a)' ) '  and eigenvectors V so that A * V = D * V.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Use the discretized second derivative matrix.'

      do j = 1, n
        do i = 1, n
          if ( i .eq. j ) then
            a(i,j) = -2.0D+00
          else if ( i .eq. j + 1 .or. i .eq. j - 1 ) then
            a(i,j) = 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a, '  Input matrix A:' )

      it_max = 100

      call jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of iterations = ', it_num
      write ( *, '(a,i4)' ) '  Number of rotations  = ', rot_num

      call r8vec_print ( n, d, '  Eigenvalues D:' )

      call r8mat_print ( n, n, v, '  Eigenvector matrix V:' )
c
c  Compute eigentest.
c
      call r8mat_is_eigen_right ( n, n, a, v, d, error_frobenius )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' )
     &  '  Frobenius norm error in eigensystem A*V-D*V = ',
     &  error_frobenius

      return
      end
