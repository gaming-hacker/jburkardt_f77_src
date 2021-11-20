      program main

c*********************************************************************72
c
cc MAIN is the main program for SOLVE_PRB.
c
c  Discussion:
c
c    SOLVE_PRB tests the SOLVE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SOLVE_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SOLVE library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SOLVE_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 demonstrates how a 3X3 linear system can be set up and solved.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      double precision b(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  R8MAT_FS solves a linear system with Gauss elimination.'
c
c  Set the array.
c
      a(1,1) = 1.0
      a(1,2) = 2.0
      a(1,3) = 3.0

      a(2,1) = 4.0
      a(2,2) = 5.0
      a(2,3) = 6.0

      a(3,1) = 7.0
      a(3,2) = 8.0
      a(3,3) = 0.0
c
c  Set the right hand side.
c
      b(1) = 14.0
      b(2) = 32.0
      b(3) = 23.0
c
c  Request the solution of A*x=b.
c
      call r8mat_fs ( n, a, b, x )

      call r8vec_print ( n, x, '  Solution:' );

      return
      end

