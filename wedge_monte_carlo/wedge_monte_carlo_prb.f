      program main

c*********************************************************************72
c
cc MAIN is the main program for WEDGE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    WEDGE_MONTE_CARLO_PRB tests the WEDGE_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDGE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the WEDGE_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDGE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses WEDGE01_SAMPLE with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n_max
      parameter ( n_max = 65536 )

      integer e(m)
      integer e_test(m,8)
      double precision error
      double precision exact
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(8)
      integer seed
      double precision value(n_max)
      double precision x(m,n_max)
      double precision wedge01_volume

      save e_test

      data e_test /
     &  0, 0, 0,   
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  1, 1, 0,    
     &  0, 0, 2,
     &  3, 0, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use WEDGE01_SAMPLE for a Monte Carlo estimat
     &e of an'
      write ( *, '(a)' ) '  integral over the interior of the unit wedge
     & in 3D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1               X               Y ' //
     &  '              Z                X^2            XY    ' //
     &  '          Z^2    ' //     '        X^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. n_max ) then

        call wedge01_sample ( n, seed, x )

        do j = 1, 8

          e(1:m) = e_test(1:m,j)

          call monomial_value ( m, n, e, x, value )

          result(j) = wedge01_volume ( ) * r8vec_sum ( n, value ) 
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,8(2x,g14.6))' ) n, result(1:8)

        n = 2 * n

        go to 10

      end if

      write ( *, '(a)' ) ' '

      do j = 1, 8

        do i = 1, m
          e(i) = e_test(i,j)
        end do

        call wedge01_integral ( e, result(j) )

      end do

      write ( *, '(2x,a8,8(2x,g14.6))' ) '   Exact', result(1:8)

      return
      end
