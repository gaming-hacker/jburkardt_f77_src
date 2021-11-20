      program main

c*********************************************************************72
c
cc MAIN is the main program for CUBE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    CUBE_MONTE_CARLO_PRB tests the CUBE_MONTE_CARLO library.
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
      write ( *, '(a)' ) 'CUBE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CUBE_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 estimates integrals over the unit cube in 3D.
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
      parameter ( m = 3 )
      integer n_max
      parameter ( n_max = 65536 )

      double precision cube01_volume
      integer e(m)
      integer e_test(m,10)
      double precision error
      double precision exact
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(10)
      integer seed
      double precision value(n_max)
      double precision x(m,n_max)

      save e_test

      data e_test /
     & 0, 0, 0, 
     & 1, 0, 0, 
     & 0, 1, 0, 
     & 0, 0, 1, 
     & 2, 0, 0, 
     & 1, 1, 0, 
     & 1, 0, 1, 
     & 0, 2, 0, 
     & 0, 1, 1, 
     & 0, 0, 2/

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use CUBE01_SAMPLE to estimate integrals '
      write ( *, '(a)' ) '  over the interior of the unit cube in 3D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N' // 
     &  '        1' // 
     &  '               X' // 
     &  '               Y ' // 
     &  '              Z' // 
     &  '               X^2' // 
     &  '              XY' // 
     &  '             XZ' // 
     &  '              Y^2' // 
     &  '             YZ' // 
     &  '               Z^2'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 65536 ) then

        call cube01_sample ( n, seed, x )

        do j = 1, 10

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = cube01_volume ( ) * r8vec_sum ( n, value ) 
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,10(2x,g14.6))' ) n, result(1:10)

        n = 2 * n

        go to 10

      end if

      write ( *, '(a)' ) ' '

      do j = 1, 10

        do i = 1, m
          e(i) = e_test(i,j)
        end do

        call cube01_monomial_integral ( e, result(j) )

      end do

      write ( *, '(2x,a8,10(2x,g14.6))' ) '   Exact', result(1:10)

      return
      end
