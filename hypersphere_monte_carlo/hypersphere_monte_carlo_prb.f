      program main

c*********************************************************************72
c
cc MAIN is the main program for HYPERSPHERE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    HYPERSPHERE_MONTE_CARLO_PRB tests the HYPERSPHERE_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERSPHERE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the HYPERSPHERE_MONTE_CARLO library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERSPHERE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses HYPERSPHERE01_SAMPLE to estimate hypersphere integrals in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
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
      integer e_test(m,7)
      double precision hypersphere01_area
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision value(n_max)
      double precision x(m,n_max)

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
      write ( *, '(a)' ) 
     &  '  Use HYPERSPHERE01_SAMPLE to estimate integrals over ' //
     &  ' the surface of the unit hypersphere in M dimensions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Spatial dimension M = ', m

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N        1              X^2             Y^2  ' // 
     &  '           Z^2             X^4           X^2Y^2           Z^4'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .lt. 65536 ) then

        call hypersphere01_sample ( m, n, seed, x )

        do j = 1, 7

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = hypersphere01_area ( m ) * r8vec_sum ( n, value ) 
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

        call hypersphere01_monomial_integral ( m, e, result(j) )

      end do

      write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses HYPERSPHERE01_SAMPLE to estimate hypersphere integrals in 6D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 6 )
      integer n_max
      parameter ( n_max = 65536 )

      integer e(m)
      integer e_test(m,7)
      double precision hypersphere01_area
      integer i
      integer j
      integer n
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision value(n_max)
      double precision x(m,n_max)

      save e_test

      data e_test /
     &  0, 0, 0, 0, 0, 0, 
     &  1, 0, 0, 0, 0, 0, 
     &  0, 2, 0, 0, 0, 0, 
     &  0, 2, 2, 0, 0, 0, 
     &  0, 0, 0, 4, 0, 0, 
     &  2, 0, 0, 0, 2, 2, 
     &  0, 0, 0, 0, 0, 6 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Use HYPERSPHERE01_SAMPLE to estimate integrals over ' //
     &  ' the surface of the unit hypersphere in M dimensions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i2)' ) '  Spatial dimension M = ', m

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N' //
     &  '        1      ' // 
     &  '        U      ' // 
     &  '         V^2   ' // 
     &  '         V^2W^2' // 
     &  '         X^4   ' // 
     &  '         Y^2Z^2' // 
     &  '         Z^6'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .lt. 65536 ) then

        call hypersphere01_sample ( m, n, seed, x )

        do j = 1, 7

          do i = 1, m
            e(i) = e_test(i,j)
          end do

          call monomial_value ( m, n, e, x, value )

          result(j) = hypersphere01_area ( m ) * r8vec_sum ( n, value ) 
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

        call hypersphere01_monomial_integral ( m, e, result(j) )

      end do

      write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

      return
      end
