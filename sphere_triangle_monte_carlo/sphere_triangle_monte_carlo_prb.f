      program main

c*********************************************************************72
c
cc MAIN is the main program for SPHERE_TRIANGLE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    SPHERE_TRIANGLE_MONTE_CARLO_PRB tests the SPHERE_TRIANGLE_MONTE_CARLO 
c    library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_TRIANGLE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Test the SPHERE_TRIANGLE_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_TRIANGLE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses SPHERE_TRIANGLE_SAMPLE_01 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n_max
      parameter ( n_max = 4 * 65536 )

      double precision area
      integer e(m)
      integer e_test(m,7)
      double precision error
      integer i 
      integer j
      integer k
      integer n
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision shrink
      double precision v1(m)
      double precision v2(m)
      double precision v3(m)
      double precision wc(m)
      double precision w1(m)
      double precision w2(m)
      double precision w3(m)
      double precision value(n_max)
      double precision x(3,n_max)

      save e_test

      data e_test /
     &  0, 0, 0, 
     &  2, 0, 0, 
     &  0, 2, 0, 
     &  0, 0, 2, 
     &  4, 0, 0, 
     &  2, 2, 0, 
     &  0, 0, 4 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Estimate monomial integrals over a sphere'
      write ( *, '(a)' ) '  triangle using the Monte Carlo method.'

      seed = 123456789
c
c  Choose three points at random to define a spherical triangle.
c
      call sphere01_sample ( 1, seed, w1 )
      call sphere01_sample ( 1, seed, w2 )
      call sphere01_sample ( 1, seed, w3 )

      do i = 1, m
        wc(i) = ( w1(i) + w2(i) + w3(i) ) / 3.0D+00
      end do
      call r8vec_normalize ( m, wc )
c
c  Shrink triangle by factor F.
c
      shrink = 2.0D+00

      do k = 1, 3

        shrink = shrink / 2.0D+00

        do i = 1, m
          v1(i) = wc(i) + shrink * ( w1(i) - wc(i) )
          v2(i) = wc(i) + shrink * ( w2(i) - wc(i) )
          v3(i) = wc(i) + shrink * ( w3(i) - wc(i) )
        end do

        call r8vec_normalize ( m, v1 )
        call r8vec_normalize ( m, v2 )
        call r8vec_normalize ( m, v3 )

        call sphere01_triangle_vertices_to_area ( v1, v2, v3, area )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Vertices of random spherical triangle'
        write ( *, '(a,g14.6)' ) '  with shrink factor = ', shrink
        write ( *, '(a,g14.6)' ) '  and area = ', area
        write ( *, '(a)' ) ' '
        write ( *, '(a,3g14.6)' ) '  V1:', v1(1:m)
        write ( *, '(a,3g14.6)' ) '  V2:', v2(1:m)
        write ( *, '(a,3g14.6)' ) '  V3:', v3(1:m)
c
c  Estimate integrals.
c
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '         N        1              X^2             Y^2' // 
     &    '             Z^2             X^4           X^2Y^2   ' //
     &    '        Z^4'
        write ( *, '(a)' ) ' '

        n = 1

10      continue

        if ( n .le. 4 * 65536 ) then

          call sphere01_triangle_sample ( n, v1, v2, v3, seed, x )

          do j = 1, 7

            do i = 1, m
              e(i) = e_test(i,j)
            end do

            call monomial_value ( m, n, e, x, value )

            result(j) = area * r8vec_sum ( n, value ) / dble ( n )

          end do

          write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

          n = 2 * n

          go to 10

        end if

      end do

      return
      end
