      program main

c*********************************************************************72
c
cc MAIN is the main program for ELLIPSOID_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    ELLIPSOID_MONTE_CARLO_PRB tests the ELLIPSOID_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPSOID_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ELLIPSOID_MONTE_CARLO library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPSOID_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses ELLIPSOID_SAMPLE on a 2D ellipse centered at (0,0).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
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

      double precision a(m,m)
      integer e(m)
      integer e_test(m,7)
      double precision ellipsoid_volume
      integer i
      integer j
      integer n
      double precision r 
      parameter ( r = 2.0D+00 )
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision v(m)
      double precision value(n_max)
      double precision volume
      double precision x(m,n_max)

      save a
      save e_test
      save v

      data a /
     &  9.0, 1.0,
     &  1.0, 4.0 /
      data e_test /
     &  0, 0,
     &  1, 0,
     &  0, 1,
     &  2, 0,
     &  1, 1,
     &  0, 2,
     &  3, 0 /
      data v / 
     &  0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use ELLIPSOID_SAMPLE to estimate integrals'
      write ( *, '(a)' ) '  in a 2D ellipse x'' * A * x <= r^2.'

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Ellipsoid radius R = ', r
      call r8vec_print ( m, v, '  Ellipsoid center V:' )
      call r8mat_print ( m, m, a, '  Ellipsoid matrix A:' )

      volume = ellipsoid_volume ( m, a, v, r )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Ellipsoid volume = ', volume

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1              X               Y       ' //
     &  '        X^2               XY             Y^2             X^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. n_max ) then

        call ellipsoid_sample ( m, n, a, v, r, seed, x )

        do j = 1, 7

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = volume * r8vec_sum ( n, value ) / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses ELLIPSOID_SAMPLE on a 2D ellipse centered at (2,3).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
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

      double precision a(m,m)
      integer e(m)
      integer e_test(m,7)
      double precision ellipsoid_volume
      integer i
      integer j
      integer n
      double precision r 
      parameter ( r = 2.0D+00 )
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision v(m)
      double precision value(n_max)
      double precision volume
      double precision x(m,n_max)

      save a
      save e_test
      save v

      data a /
     &  9.0, 1.0,
     &  1.0, 4.0 /
      data e_test /
     &  0, 0,
     &  1, 0,
     &  0, 1,
     &  2, 0,
     &  1, 1,
     &  0, 2,
     &  3, 0 /
      data v / 
     &  2.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Use ELLIPSOID_SAMPLE to estimate integrals'
      write ( *, '(a)' ) '  in a 2D ellipse (x-v)'' * A * (x-v) <= r^2.'

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Ellipsoid radius R = ', r
      call r8vec_print ( m, v, '  Ellipsoid center V:' )
      call r8mat_print ( m, m, a, '  Ellipsoid matrix A:' )

      volume = ellipsoid_volume ( m, a, v, r )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Ellipsoid volume = ', volume

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1              X               Y       ' //
     &  '        X^2               XY             Y^2             X^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. n_max ) then

        call ellipsoid_sample ( m, n, a, v, r, seed, x )

        do j = 1, 7

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = volume * r8vec_sum ( n, value ) / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses ELLIPSOID_SAMPLE on a 3D ellipse centered at (1,2,3).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
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

      double precision a(m,m)
      integer e(m)
      integer e_test(m,7)
      double precision ellipsoid_volume
      integer i
      integer j
      integer n
      double precision r 
      parameter ( r = 0.5D+00 )
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision v(m)
      double precision value(n_max)
      double precision volume
      double precision x(m,n_max)

      save a
      save e_test
      save v

      data a /
     &  9.0, 6.0, 3.0,
     &  6.0, 5.0, 4.0,
     &  3.0, 4.0, 9.0 /
      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  0, 0, 3 /
      data v / 
     &  1.0D+00, 2.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Use ELLIPSOID_SAMPLE to estimate integrals'
      write ( *, '(a)' ) '  in a 3D ellipse (x-v)'' * A * (x-v) <= r^2.'

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Ellipsoid radius R = ', r
      call r8vec_print ( m, v, '  Ellipsoid center V:' )
      call r8mat_print ( m, m, a, '  Ellipsoid matrix A:' )

      volume = ellipsoid_volume ( m, a, v, r )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Ellipsoid volume = ', volume

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1              X               Y       ' //
     &  '         Z                X^2            YZ              Z^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. n_max ) then

        call ellipsoid_sample ( m, n, a, v, r, seed, x )

        do j = 1, 7

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = volume * r8vec_sum ( n, value ) / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      return
      end
