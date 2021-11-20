      program main

c*********************************************************************72
c
cc MAIN is the main program for MATRIX_EXPONENTIAL_PRB.
c
c  Discussion:
c
c    MATRIX_EXPONENTIAL_PRB tests the MATRIX_EXPONENTIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the MATRIX_EXPONENTIAL library.'
      write ( *, '(a)' ) '  The R8LIB and C8LIB libraries are needed.'
      write ( *, '(a)' ) 
     &  '  This test needs the TEST_MATRIX_EXPONENTIAL library.'

      call matrix_exponential_test01 ( )
      call matrix_exponential_test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine matrix_exponential_test01 ( )

c*********************************************************************72
c
cc MATRIX_EXPONENTIAL_TEST01 compares matrix exponential algorithms.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a(n_max,n_max)
      double precision a_exp(n_max,n_max)
      integer n
      integer test
      integer test_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_TEST01:'
      write ( *, '(a)' ) 
     &  '  R8MAT_EXPM1 is an M-file equivalent to MATLAB''s EXPM'
      write ( *, '(a)' ) '  R8MAT_EXPM2 uses a Taylor series approach'
      write ( *, '(a)' ) 
     &  '  R8MAT_EXPM3 relies on an eigenvalue calculation.'

      call r8mat_exp_test_num ( test_num )

      do test = 1, test_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Test #', test

        call r8mat_exp_story ( test )

        call r8mat_exp_n ( test, n )

        write ( *, '(a,i4)' ) '  Matrix order N = ', n

        call r8mat_exp_a ( test, n, a )

        call r8mat_print ( n, n, a, '  Matrix:' )

        call r8mat_expm1 ( n, a, a_exp )
        call r8mat_print ( n, n, a_exp, '  EXPM1(A):' )

        call r8mat_expm2 ( n, a, a_exp )
        call r8mat_print ( n, n, a_exp, '  EXPM2(A):' )

c       call r8mat_expm3 ( n, a, a_exp )
c       call r8mat_print ( n, n, a_exp, '  EXPM3(A):' )

        call r8mat_exp_expa ( test, n, a_exp )
        call r8mat_print ( n, n, a_exp, '  Exact Exponential:' )

      end do

      return
      end
      subroutine matrix_exponential_test02 ( )

c*********************************************************************72
c
cc MATRIX_EXPONENTIAL_TEST02 compares matrix exponential algorithms.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double complex a(n_max,n_max)
      double complex a_exp(n_max,n_max)
      integer n
      integer test
      integer test_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_TEST02:'
      write ( *, '(a)' ) 
     &  '  C8MAT_EXPM1 is an M-file equivalent to MATLAB''s EXPM'
      write ( *, '(a)' ) '  C8MAT_EXPM2 uses a Taylor series approach'
      write ( *, '(a)' ) 
     &  '  C8MAT_EXPM3 relies on an eigenvalue calculation.'

      call c8mat_exp_test_num ( test_num )

      do test = 1, test_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Test #', test

        call c8mat_exp_story ( test )

        call c8mat_exp_n ( test, n )

        write ( *, '(a,i4)' ) '  Matrix order N = ', n

        call c8mat_exp_a ( test, n, a )

        call c8mat_print ( n, n, a, '  Matrix:' )

        call c8mat_expm1 ( n, a, a_exp )
        call c8mat_print ( n, n, a_exp, '  EXPM1(A):' )

c       call c8mat_expm2 ( n, a, a_exp )
c       call c8mat_print ( n, n, a_exp, '  EXPM2(A):' )

c       call c8mat_expm3 ( n, a, a_exp )
c       call c8mat_print ( n, n, a_exp, '  EXPM3(A):' )

        call c8mat_exp_expa ( test, n, a_exp )
        call c8mat_print ( n, n, a_exp, '  Exact Exponential:' )

      end do

      return
      end
