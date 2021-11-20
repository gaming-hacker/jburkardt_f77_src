      subroutine monomial_value ( m, n, e, x, v )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 <= i <= m ) x(i)^e(i)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points at which the
c    monomial is to be evaluated.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the point coordinates.
c
c    Output, double precision V(N), the value of the monomial.
c
      implicit none

      integer m
      integer n

      integer e(m)
      integer i
      integer j
      double precision v(n)
      double precision x(m,n)

      do j = 1, n
        v(j) = 1.0D+00
      end do

      do i = 1, m
        if ( 0 .ne. e(i) ) then
          do j = 1, n
            v(j) = v(j) * x(i,j) ** e(i)
          end do
        end if
      end do

      return
      end
      function polygon_area ( nv, v )

c*********************************************************************72
c
cc POLYGON_AREA determines the area of a polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NV, the number of vertices of the polygon.
c
c    Input, double precision V(2,NV), the vertex coordinates.
c
c    Output, double precision POLYGON_AREA, the area of the polygon.
c
      implicit none

      integer nv

      double precision area
      integer e(2)
      double precision polygon_area
      double precision v(2,nv)

      e(1) = 0
      e(2) = 0

      call polygon_monomial_integral ( nv, v, e, area )

      polygon_area = area

      return
      end
      subroutine polygon_monomial_integral ( nv, v, e, nu_pq )

c*********************************************************************72
c
cc POLYGON_MONOMIAL_INTEGRAL integrates a monomial over a polygon.
c
c  Discussion:
c
c    Nu(P,Q) = Integral ( x, y in polygon ) x^e(1) y^e(2) dx dy
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 October 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carsten Steger,
c    On the calculation of arbitrary moments of polygons,
c    Technical Report FGBV-96-05,
c    Forschungsgruppe Bildverstehen, Informatik IX,
c    Technische Universitaet Muenchen, October 1996.
c
c  Parameters:
c
c    Input, integer NV, the number of vertices of the polygon.
c
c    Input, double precision V(2,NV), the vertex coordinates.
c
c    Input, integer E(2), the exponents of the monomial.
c
c    Output, double precision NU_PQ, the unnormalized moment Nu(P,Q).
c
      implicit none

      integer nv

      integer e(2)
      integer i
      integer k
      integer l
      double precision nu_pq
      integer p
      integer q
      double precision r8_choose
      double precision s_pq
      double precision v(2,nv)
      double precision xi
      double precision xj
      double precision yi
      double precision yj

      p = e(1)
      q = e(2)

      nu_pq = 0.0D+00

      xj = v(1,nv)
      yj = v(2,nv)

      do i = 1, nv

        xi = v(1,i)
        yi = v(2,i)

        s_pq = 0.0D+00
        do k = 0, p
          do l = 0, q
            s_pq = s_pq 
     &        + r8_choose ( k + l, l ) 
     &        * r8_choose ( p + q - k - l, q - l ) 
     &        * xi ** k * xj ** ( p - k ) 
     &        * yi ** l * yj ** ( q - l )
          end do
        end do

        nu_pq = nu_pq + ( xj * yi - xi * yj ) * s_pq

        xj = xi
        yj = yi

      end do

      nu_pq = nu_pq / dble ( p + q + 2 ) 
     &  / dble ( p + q + 1 ) 
     &  / r8_choose ( p + q, p )

      return
      end
      subroutine polygon_sample ( nv, v, n, seed, s )

c*********************************************************************72
c
cc POLYGON_SAMPLE uniformly samples a polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NV, the number of vertices.
c
c    Input, double precision V(2,NV), the vertices of the polygon, listed in
c    counterclockwise order.
c
c    Input, integer N, the number of points to create.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision S(2,N), the points.
c
      implicit none

      integer n
      integer nv

      double precision area_cumulative(nv-2)
      double precision area_polygon
      double precision area_relative(nv-2)
      double precision area_triangle(nv-2)
      double precision area_percent
      integer i
      integer ip1
      integer j
      integer k
      double precision r(2)
      double precision r8_uniform_01
      double precision r8vec_sum
      integer seed
      double precision triangle_area
      integer triangles(3,nv-2)
      double precision s(2,n)
      double precision v(2,nv)
c
c  Triangulate the polygon.
c
      call polygon_triangulate ( nv, v(1,1:nv), v(2,1:nv), triangles )
c
c  Determine the areas of each triangle.
c
      do i = 1, nv - 2
        area_triangle(i) = triangle_area (
     &    v(1,triangles(1,i)), v(2,triangles(1,i)),
     &    v(1,triangles(2,i)), v(2,triangles(2,i)),
     &    v(1,triangles(3,i)), v(2,triangles(3,i)) )
      end do
c
c  Normalize the areas.
c
      area_polygon = r8vec_sum ( nv - 2, area_triangle )
      do i = 1, nv - 2
        area_relative(i) = area_triangle(i) / area_polygon
      end do
c
c  Replace each area by the sum of itself and all previous ones.
c
      area_cumulative(1) = area_relative(1)
      do i = 2, nv - 2
        area_cumulative(i) = area_relative(i) + area_cumulative(i-1)
      end do

      do j = 1, n
c
c  Choose triangle I at random, based on areas.
c
        area_percent = r8_uniform_01 ( seed )

        do k = 1, nv - 2

          i = k

          if ( area_percent .le. area_cumulative(k) ) then
            go to 10
          end if

        end do

10      continue
c
c  Now choose a point at random in triangle I.
c
        call r8vec_uniform_01 ( 2, seed, r )

        if ( 1.0D+00 .lt. r(1) + r(2) ) then
          r(1) = 1.0D+00 - r(1)
          r(2) = 1.0D+00 - r(2)
        end if

        s(1:2,j) = ( 1.0D+00 - r(1) - r(2) ) * v(1:2,triangles(1,i))    
     &                       + r(1)          * v(1:2,triangles(2,i))     
     &                              + r(2)   * v(1:2,triangles(3,i))
      end do

      return
      end
      function r8_choose ( n, k )

c*********************************************************************72
c
cc R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in R8 arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, double precision R8_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer k
      integer mn
      integer mx
      integer n
      double precision r8_choose
      double precision value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0.0D+00

      else if ( mn .eq. 0 ) then

        value = 1.0D+00

      else

        mx = max ( k, n - k )
        value = dble ( mx + 1 )

        do i = 2, mn
          value = ( value * dble ( mx + i ) ) / dble ( i )
        end do

      end if

      r8_choose = value

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
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
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
