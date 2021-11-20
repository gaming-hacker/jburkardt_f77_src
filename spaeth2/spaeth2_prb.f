      program main

c*********************************************************************72
c
cc MAIN is the main program for SPAETH2_PRB.
c
c  Discussion:
c
c    SPAETH2_PRB tests the SPAETH2 library.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/spaeth2/spaeth2.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPAETH2_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPAETH2 library.'

      call kmeans_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPAETH2_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine kmeans_test ( )

c*********************************************************************72
c
cc KMEANS_TEST tests KMEANS.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/spaeth2/spaeth2.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 22 )
      integer l
      parameter ( l = 2 )
      integer nmax
      parameter ( nmax = 10 )

      double precision d
      double precision d1
      double precision e(nmax)
      integer i
      integer idr
      integer iy
      integer j
      integer k
      integer n
      integer p(m)
      integer q(nmax)
      integer r
      double precision s(nmax,l)
      integer test
      double precision x(m,l)

      data x /
     &  -57.0D+00, 54.0D+00, 46.0D+00,  8.0D+00, -36.0D+00,
     &  -22.0D+00, 34.0D+00, 74.0D+00, -6.0D+00, 21.0D+00,
     &   37.0D+00,-38.0D+00, -5.0D+00, 70.0D+00, 59.0D+00,
     &  114.0D+00, 83.0D+00,-40.0D+00, 21.0D+00,  0.0D+00,
     &   50.0D+00,-20.0D+00,
     &   28.0D+00,-65.0D+00, 79.0D+00,111.0D+00, 52.0D+00,
     &  -76.0D+00,129.0D+00,  6.0D+00,-41.0D+00, 45.0D+00,
     &  155.0D+00, 35.0D+00,-24.0D+00,-74.0D+00,-26.0D+00,
     &  -56.0D+00,-41.0D+00,-28.0D+00,-12.0D+00, 71.0D+00,
     &  140.0D+00, 70.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KMEANS_TEST'
      write ( *, '(a)' ) 
     &  '  KMEANS uses a variance diminishing procedure.'
      write ( *, '(a,i6)' ) 
     &  '  The number of data items is M =        ', m
      write ( *, '(a,i6)' ) 
     &  '  The dimension of the data items is L = ', l

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Data to be clustered:'
      write ( *, '(a)' ) ''
      do i = 1, m
        write ( *, '(2x,g14.6,2x,g14.6)' ) ( x(i,j), j = 1, l )
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  N    Initial       Final'
      write ( *, '(a)' ) '       Variance      Variance'

      idr = 0

      do n = 2, nmax

        iy = 123456789
        write ( *, '(a)' ) ''

        do test = 1, 3
c
c  Random cluster assignments.
c
          call randp ( m, n, p, iy )

          if ( 2 .le. idr ) then
            write ( *, '(a)' ) ''
            write ( *, '(a)' ) '  Initial random cluster assignments:'
            write ( *, '(a)' ) ''
            do i = 1, m
              write ( *, '(2x,i4,2x,i4)' ) i, p(i)
            end do
          end if
c
c  Get cluster populations.
c
          do k = 1, n
            q(k) = 0
          end do

          do i = 1, m
            k = p(i)
            q(k) = q(k) + 1
          end do
c
c  Determine cluster centroids.
c
          do j = 1, l
            do k = 1, n
              s(k,j) = 0.0D+00
            end do
          end do

          do i = 1, m
            k = p(i)
            do j = 1, l
              s(k,j) = s(k,j) + x(i,j)
            end do
          end do

          do k = 1, n
            do j = 1, l
              s(k,j) = s(k,j) / dble ( max ( q(k), 1 ) )
            end do
          end do
c
c  Individual cluster variances.
c
          do k = 1, n
            e(k) = 0.0D+00
          end do

          do i = 1, m
            k = p(i)
            do j = 1, l
              e(k) = e(k) + ( s(k,j) - x(i,j) ) ** 2
            end do
          end do
c
c  Total cluster variance.
c
          d1 = 0.0D+00
          do k = 1, n
            d1 = d1 + e(k)
          end do

          call kmeans ( m, l, x, p, n, s, e, d, idr )
 
          if ( 2 .le. idr ) then
            write ( *, '(a)' ) ''
            write ( *, '(a)' ) '  Final cluster assignments:'
            write ( *, '(a)' ) ''
            do k = 1, m
              write ( *, '(2x,i4,2x,i4)' ) k, p(k)
            end do
          end if

          write ( *, '(2x,i2,2g14.6)' ) n, d1, d

        end do

      end do

      return
      end
