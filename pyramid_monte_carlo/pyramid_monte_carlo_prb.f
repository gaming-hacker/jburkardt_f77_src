      program main

c*********************************************************************72
c
cc MAIN is the main program for PYRAMID_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    PYRAMID_MONTE_CARLO_PRB tests the PYRAMID_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PYRAMID_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*****************************************************************************80
c
cc TEST01 estimates integrals over the unit pyramid in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2014
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
      integer test_num
      parameter ( test_num = 10 )

      double precision pyramid01_volume
      integer e(m)
      integer e_test(m,test_num)
      double precision error
      double precision exact
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(test_num)
      integer seed
      double precision value(n_max)
      double precision x(m,n_max)

      save e_test

      data e_test /
     &  0, 0, 0, 
     &  0, 0, 1, 
     &  2, 0, 0, 
     &  0, 2, 0, 
     &  0, 0, 2, 
     &  2, 0, 1, 
     &  0, 2, 1, 
     &  0, 0, 3, 
     &  2, 2, 0, 
     &  2, 0, 2 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use PYRAMID01_SAMPLE to estimate integrals'
      write ( *, '(a)' ) 
     &  '  over the interior of the unit pyramid in 3D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )
     &  '         N' //
     &  '        1' //
     &  '               Z' // 
     &  '             X^2 ' // 
     &  '             Y^2' // 
     &  '             Z^2' // 
     &  '            X^2Z' // 
     &  '            Y^2Z' // 
     &  '             Z^3' // 
     &  '          X^2Y^2' // 
     &  '          X^2Z^2'
      write ( *, '(a)' ) ' '
    
      n = 1

10    continue

      if ( n .le. 65536 ) then

        call pyramid01_sample ( n, seed, x )

        do j = 1, test_num

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = pyramid01_volume ( ) * r8vec_sum ( n, value ) 
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

        call pyramid01_integral ( e, result(j) )

      end do

      write ( *, '(2x,a8,10(2x,g14.6))' ) '   Exact', result(1:10)

      return
      end
