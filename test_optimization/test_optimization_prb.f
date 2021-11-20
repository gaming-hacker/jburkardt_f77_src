      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_OPTIMIZATION_PRB.
c
c  Discussion:
c
c    TEST_OPTIMIZATION_PRB tests the TEST_OPTIMIZATION library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp (  )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_OPTIMIZATION_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_OPTIMIZATION library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_OPTIMIZATION_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp (  )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 simply prints the title of each problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer problem
      integer problem_num
      character * ( 80 ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For each problem, print the title.'
c
c  Get the number of problems.
c
      call p00_problem_num ( problem_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Problem  Title'
      write ( *, '(a)' ) ' '

      do problem = 1, problem_num

        call p00_title ( problem, title )

        write ( *, '(2x,i7,2x,a)' ) problem, title

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 samples the function at 1,000 points and prints the minimum.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n
      parameter ( n = 1000 )

      double precision a(m)
      double precision b(m)
      double precision f(n)
      double precision f_min
      double precision fmn
      double precision fmx
      integer know
      integer problem
      integer problem_num
      integer seed
      character * ( 80 ) title
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For each problem, using dimension M = 2'
      write ( *, '(a)' ) '  sample the function at N = 1000 points,'
      write ( *, '(a)' ) '  and print the minimum and maximum.'

      seed = 123456789
c
c  Get the number of problems.
c
      call p00_problem_num ( problem_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Problem     Minimum  Sample Minimum  Sample Maximum'
      write ( *, '(a)' ) ' '

      do problem = 1, problem_num

        know = 0
        call p00_sol ( problem, m, know, x )
        if ( know .ne. 0 ) then
          call p00_f ( problem, m, 1, x, f )
          f_min = f(1)
        end if

        call p00_ab ( problem, m, a, b )
        call r8col_uniform ( m, n, a, b, seed, x )
        call p00_f ( problem, m, n, x, f )
        call r8vec_max ( n, f, fmx )
        call r8vec_min ( n, f, fmn )
        
        if ( know .ne. 0 ) then
          write ( *, '(2x,i7,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      problem, f_min, fmn, fmx
        else
          write ( *, '(2x,i7,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &      problem,         fmn, fmx
        end if

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tries Compass Search on each problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m 
      parameter ( m = 2 )
      integer n
      parameter ( n = 1000 )

      double precision a(m)
      double precision b(m)
      double precision delta_init
      double precision delta_tol
      double precision f(n)
      double precision fx
      integer i
      integer k
      integer k_max
      integer know
      integer problem
      integer problem_num
      integer seed
      double precision t
      character * ( 80 ) title
      double precision x(m)
      double precision x0(m)

      delta_tol = 0.000001D+00
      k_max = 20000

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For each problem, using dimension M = 2'
      write ( *, '(a)' ) '  try compass search.'
c
c  Get the number of problems.
c
      call p00_problem_num ( problem_num )

      do problem = 1, problem_num

        seed = 123456789

        call p00_ab ( problem, m, a, b )
        call r8col_uniform ( m, 1, a, b, seed, x0 )
        call p00_f ( problem, m, 1, x0, fx )
        t = 0.0D+00
        do i = 1, m
          t = t + x0(i)**2
        end do
        delta_init = 0.3D+00 * sqrt ( t ) / dble ( m )
        delta_init = max ( delta_init, 1000.0D+00 * delta_tol )
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a,g14.6)' ) 
     &    '  Problem ', problem, '  DELTA_INIT = ', delta_init
        write ( *, '(a,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    '  Initial:', x0, fx
        call p00_compass_search ( problem, m, x0, delta_tol, delta_init, 
     &    k_max, x, fx, k )
        write ( *, '(a,2x,g14.6,2x,g14.6,2x,g14.6,2x,a,i8)' ) 
     &    '  Final:  ', x, fx, '  Steps = ', k

        know = 0
        do
          call p00_sol ( problem, m, know, x )
          if ( know == 0 ) then
            exit
          end if
          call p00_f ( problem, m, 1, x, fx )
          write ( *, '(a,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      '  Exact:  ', x, fx
        end do

      end do

      return
      end
