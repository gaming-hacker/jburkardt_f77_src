      subroutine kmeans ( m, l, x, p, n, s, e, d, idr )

c*********************************************************************72
c
cc KMEANS clusters data using the K-Means algorithm.
c
c  Discussion:
c
c    The initial assignment of the vectors X(M,L) to N clusters
c    is given by the array P, where P(I) is the cluster number
c    of the I-th vector.  Thus, each P(I) must be such that
c    1 <= P(I) <= N, and for each J = 1,...,N at least on I
c    with P(I) = J must exist.
c
c    Let D denote the sum of the E(J), where E(J) is the sum
c    of the squares of the distances between the members of
c    the J-th cluster and their centroid.
c
c    The subroutine minimizes D as far as possible by 
c    repeated exchanges of cluster members.  The P(I)
c    are correspondingly modified without, however,
c    changing the number of clusters.
c
c    The subroutine returns the values at the final
c    configuration for the centroids S(N,L), the 
c    sums E(N), and D.
c
c    If IDR = 1, the current values of D and of the 
c    vector P are printed at each iteration.
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
c    Helmut Spaeth
c
c  Reference:
c
c    Helmuth Spaeth,
c    Cluster Analysis Algorithms
c    for Data Reduction and Classification of Objects,
c    Ellis Horwood, 1980, page 72-74,
c    QA278 S6813.
c
c  Parameters:
c
c    Input, integer M, the number of rows of X.
c
c    Input, integer L, the number of columns of data.
c
c    Input, double precision X(M,L), the data to be clustered.
c
c    Input/output, integer P(M), the cluster assignments.
c
c    Input, integer N, the number of clusters created.
c
c    Workspace, double precision S(N,L).
c
c    Output, double precision E(N), the cluster variances.
c
c    Output, double precision D, the total variance.
c
c    Input, integer IDR, controls output.  If its value is 1, then
c    the current values of D and P are printed at each iteration.
c
      implicit none

      integer l
      integer m
      integer n

      double precision a
      double precision b
      double precision d
      double precision e(n)
      double precision f
      double precision g
      double precision h
      integer i
      integer idr
      integer it
      integer j
      integer k
      integer ko
      integer p(m)
      integer q(n)
      integer r
      double precision s(n,l)
      double precision t
      integer u
      integer v
      integer w
      double precision x(m,l)

      ko = 6

      do j = 1, n
        q(j) = 0
        e(j) = 0.0D+00
        do k = 1, l
          s(j,k) = 0.0D+00
        end do
      end do

      do i = 1, m
        r = p(i)
        if ( r .lt. 1 .or. r .gt. n ) then
          return
        end if
        q(r) = q(r) + 1
        do k = 1, l
          s(r,k) = s(r,k) + x(i,k)
        end do
      end do

      do j = 1, n
        r = q(j)
        if ( r .eq. 0 ) then
          return
        end if
        f = 1.0D+00 / dble ( r )
        do k = 1, l
          s(j,k) = s(j,k) * f
        end do
      end do

      do i = 1, m
        r = p(i)
        f = 0.0D+00
        do k = 1, l
          t = s(r,k) - x(i,k)
          f = f + t * t
        end do
        e(r) = e(r) + f
      end do

      d = 0.0D+00
      do j = 1, n
        d = d + e(j)
      end do

      i = 0
      it = 0

10    continue

      i = i + 1

      if ( i .gt. m ) then
        i = i - m
      end if

      if ( it .eq. m ) then
        return
      end if

      r = p(i)

      u = q(r)

      if ( u .le. 1 ) then
        go to 10
      end if

      h = dble ( u )
      h = h / ( h - 1.0D+00 )

      f = 0.0D+00
      do k = 1, l
        t = s(r,k) - x(i,k)
        f = f + t * t
      end do

      a = h * f
      b = 1.0D+30

      do j = 1, n
        if ( j .ne. r ) then
          u = q(j)
          h = float ( u )
          h = h / ( h + 1.0D+00 )
          f = 0.0D+00
          do k = 1, l
            t = s(j,k) - x(i,k)
            f = f + t * t
          end do
          f = h * f
          if ( f .le. b ) then
            b = f
            v = j
            w = u
          end if
        end if
      end do

      if ( b .gt. a ) then
        it = it + 1
        go to 10
      end if

      it = 0
      e(r) = e(r) - a
      e(v) = e(v) + b
      d = d - a + b
      h = dble ( q(r) )
      g = dble ( w )
      a = 1.0D+00 / ( h - 1.0D+00 )
      b = 1.0D+00 / ( g + 1.0D+00 )
      do k = 1, l
        f = x(i,k)
        s(r,k) = ( h * s(r,k) - f ) * a
        s(v,k) = ( g * s(v,k) + f ) * b
      end do

      p(i) = v
      q(r) = q(r) - 1
      q(v) = q(v) + 1

      if ( idr .eq. 1 ) then
        write ( ko, 16 ) i, d, ( p(u), u = 1, m )
      end if

16    format ( 1x, i4, f12.2, 23i3 / ( 17x, 23i3 ) )

      go to 10

      end
      subroutine randp ( m, n, z, iy )

c*****************************************************************************80
c
cc RANDP randomly partitions a set of M items into N clusters.
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
c    Helmut Spaeth
c
c  Reference:
c
c    Helmuth Spaeth,
c    Cluster Dissection and Analysis,
c    Theory, FORTRAN Programs, Examples,
c    Ellis Horwood, 1985, page 143,
c    QA278 S68213.
c
c  Parameters:
c
c    Input, integer M, the number of items to assign.
c
c    Input, integer N, the number of clusters.
c
c    Output, integer Z(M), the cluster to which each item 
c    is assigned.
c
c    Input/output, integer IY, a seed used by the random
c    number generator.
c
      implicit none

      integer m

      double precision f
      double precision h
      integer i
      integer iy
      integer j
      integer k
      integer n
      double precision ps
      double precision urand
      integer z(m)
      integer zi

      do i = 1, m
        z(i) = 1
      end do

      if ( n .le. 1 .or. m .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RANDP - Fatal error!'
        write ( *, '(a)' ) '  N <= 1 or M <= 1.'
        stop 1
      end if

      h = 1.0D+00 / dble ( n )
      do i = 1, m
        ps = urand ( iy )
        f = h
        zi = 1
2       continue
        if ( zi .lt. n ) then
          if ( ps .ge. f ) then
            f = f + h
            zi = zi + 1
            go to 2
          end if
        end if
        z(i) = zi
      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
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
c    12 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ', 
     &  'May      ', 'June     ', 'July     ', 'August   ', 
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *, 
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end
      function urand ( iy )

c*****************************************************************************80
c
cc URAND returns a pseudo-random number uniformly distributed in [0,1].
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
c    Helmut Spaeth
c
c  Reference:
c
c    Helmuth Spaeth,
c    Cluster Dissection and Analysis,
c    Theory, FORTRAN Programs, Examples,
c    Ellis Horwood, 1985, page 143,
c    QA278 S68213.
c
c  Parameters:
c
c    Input/output, integer ( kind = 4 ) SEED, a seed for the random
c    number generator.
c
c    Output, real ( kind = 8 ) URAND, the pseudo-random number.
c
      implicit none

      double precision halfm
      integer ia
      integer ic
      integer iy
      integer m
      integer m2
      integer mic
      double precision s
      double precision urand

      save ia
      save ic
      save halfm
      save m2
      save mic
      save s

      data m2 / 0 /
 
      if ( m2 .eq. 0 ) then

        m = 1

10      continue

        m2 = m
        m = 2 * m

        if ( m > m2 ) then
          go to 10
        end if

        halfm = dble ( m2 )
        ia = 5 + 8 * int ( halfm * atan ( 1.0D+0 ) / 8.0D+0 )
        ic = 1 + 2 * 
     &    int ( halfm * ( 0.5D0 - sqrt ( 3.0D+00 ) / 6.0D+00 ) )
        mic = ( m2 - ic ) + m2
        s = 0.5D+00 / halfm

      end if

      iy = iy * ia

      if ( iy .gt. mic ) then
        iy = ( iy - m2 ) - m2
      end if

      iy = iy + ic

      if ( iy / 2 > m2 ) then
        iy = ( iy - m2 ) - m2
      end if

      if ( iy < 0 ) then
        iy = ( iy + m2 ) + m2
      end if

      urand = real ( iy, kind = 8 ) * s

      return
      end
