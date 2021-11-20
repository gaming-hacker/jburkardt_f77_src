      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE01_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    TRIANGLE01_MONTE_CARLO_PRB tests the TRIANGLE01_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE01_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TRIANGLE01_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE01_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses TRIANGLE01_SAMPLE with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n_max
      parameter ( n_max = 65536 )

      integer e(m)
      integer e_test(m,7)
      double precision error
      double precision exact
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision triangle01_area
      double precision value(n_max)
      double precision x(m,n_max)

      save e_test

      data e_test /
     &  0, 0, 
     &  1, 0, 
     &  0, 1, 
     &  2, 0, 
     &  1, 1, 
     &  0, 2, 
     &  3, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Use TRIANGLE01_SAMPLE for a Monte Carlo estimate of an'
      write ( *, '(a)' ) 
     &  '  integral over the interior of the unit triangle in 2D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1               X               Y     ' // 
     &  '         X^2               XY             Y^2             X^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 65536 ) then

        call triangle01_sample ( n, seed, x )

        do j = 1, 7

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = triangle01_area ( ) * r8vec_sum ( n, value ) 
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      write ( *, '(a)' ) ' '

      do j = 1, 7

        do i = 1, m
          e(i) = e_test(i,j)
        end do

        call triangle01_monomial_integral ( e, result(j) )

      end do

      write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

      return
      end
