      program main

c*********************************************************************72
c
cc MAIN is the main program for CIRCLE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    CIRCLE_MONTE_CARLO_PRB tests the CIRCLE_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CIRCLE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CIRCLE_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CIRCLE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses CIRCLE01_SAMPLE with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65536 )

      double precision circle01_length
      integer e(2)
      integer e_test(2,7)
      double precision error
      double precision exact
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision value(n_max)
      double precision x(2,n_max)

      save e_test

      data e_test /
     &  0, 0, 
     &  2, 0, 
     &  0, 2, 
     &  4, 0, 
     &  2, 2, 
     &  0, 4, 
     &  6, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use CIRCLE01_SAMPLE to estimate integrals '
      write ( *, '(a)' ) '  along the length of the unit circle in 2D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1              X^2             Y^2    ' // 
     &  '         X^4           X^2Y^2             Y^4           X^6'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 65536 ) then

        call circle01_sample ( n, seed, x )

        do j = 1, 7

          do i = 1, 2
            e(i) = e_test(i,j)
          end do

          call monomial_value ( 2, n, e, x, value )

          result(j) = circle01_length ( ) * r8vec_sum ( n, value ) 
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      write ( *, '(a)' ) ' '

      do j = 1, 7

        do i = 1, 2
          e(i) = e_test(i,j)
        end do

        call circle01_monomial_integral ( e, result(j) )

      end do

      write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

      return
      end
