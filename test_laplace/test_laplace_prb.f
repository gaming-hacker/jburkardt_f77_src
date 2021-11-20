      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_LAPLACE_PRB.
c
c  Discussion:
c
c    TEST_LAPLACE_PRB demonstrates the TEST_LAPLACE test problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer problem_num

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_LAPLACE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sample problems for TEST_LAPLACE, '
      write ( *, '(a)' ) '  a collection of Laplace transform '
      write ( *, '(a)' ) '  / inverse transform pairs.'
c
c  Find out how many problems are available.
c
      call p00_problem_num ( problem_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) 
     &  '  There are ', problem_num, ' test functions.'
c
c  Print the title of each problem.
c
      call p00_title_test ( problem_num )
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_LAPLACE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine p00_title_test ( problem_num )

c*********************************************************************72
c
cc P00_TITLE_TEST prints the problem titles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM_NUM, the number of problems.
c
      implicit none


      integer problem
      integer problem_num
      character*80 title_backward
      character*80 title_forward

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'P00_TITLE_TEST'
      write ( *, '(a)' ) '  List the problem title'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Problem    Title'

      do problem = 1, problem_num

        call p00_title_forward ( problem, title_forward )
        call p00_title_backward ( problem, title_backward )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,i8,2x,a)' ) problem
        write ( *, '(2x,a8,2x,a)' ) 'Forward ', trim ( title_forward )
        write ( *, '(2x,a8,2x,a)' ) 'Backward', trim ( title_backward )

      end do

      return
      end
