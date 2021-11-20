      subroutine angle_half ( p1, p2, p3, p4 )

c*********************************************************************72
c
cc ANGLE_HALF finds half an angle.
c
c  Discussion:
c
c    The original angle is defined by the sequence of points P1, P2 and P3.
c
c    The point P4 is calculated so that:
c
c      (P1,P2,P4) = (P1,P2,P3) / 2
c
c        P1
c        /
c       /   P4
c      /  .
c     / .
c    P2--------->P3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), P3(2), points defining the angle.
c
c    Input, double precision P4(2), a point defining the half angle.
c    The vector P4 - P2 will have unit norm.
c
      implicit none

      integer i
      double precision norm
      double precision p1(2)
      double precision p2(2)
      double precision p3(2)
      double precision p4(2)

      do i = 1, 2

        p4(i) = 0.5D+00 *
     &    ( ( p1(i) - p2(i) )
     &    / sqrt ( ( p1(1) - p2(1) )**2 + ( p1(2) - p2(2) )**2 )
     &    + ( p3(i) - p2(i) )
     &    / sqrt ( ( p3(1) - p2(1) )**2 + ( p3(2) - p2(2) )**2 ) )

      end do

      norm = sqrt ( p4(1)**2 + p4(2)**2 )

      do i = 1, 2
        p4(i) = p2(i) + p4(i) / norm
      end do

      return
      end
      function angle_rad ( p1, p2, p3 )

c*********************************************************************72
c
cc ANGLE_RAD returns the angle in radians swept out between two rays.
c
c  Discussion:
c
c    Except for the zero angle case, it should be true that
c
c      ANGLE_RAD ( P1, P2, P3 ) + ANGLE_RAD ( P3, P2, P1 ) = 2 * PI
c
c        P1
c        /
c       /
c      /
c     /
c    P2--------->P3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), P3(2), define the rays
c    P1 - P2 and P3 - P2 which define the angle.
c
c    Output, double precision ANGLE_RAD, the angle swept out by the rays,
c    in radians.  0 .le. ANGLE_RAD .lt. 2 * PI.  If either ray has zero
c    length, then ANGLE_RAD is set to 0.
c
      implicit none

      double precision angle_rad
      double precision p(2)
      double precision p1(2)
      double precision p2(2)
      double precision p3(2)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )

      p(1) = ( p3(1) - p2(1) ) * ( p1(1) - p2(1) )
     &     + ( p3(2) - p2(2) ) * ( p1(2) - p2(2) )

      p(2) = ( p3(1) - p2(1) ) * ( p1(2) - p2(2) )
     &     - ( p3(2) - p2(2) ) * ( p1(1) - p2(1) )

      if ( p(1) .eq. 0.0D+00 .and. p(2) .eq. 0.0D+00  ) then
        angle_rad = 0.0D+00
        return
      end if

      angle_rad = atan2 ( p(2), p(1) )

      if ( angle_rad .lt. 0.0D+00 ) then
        angle_rad = angle_rad + 2.0D+00 * r8_pi
      end if

      return
      end
      function i4_modp ( i, j )

c*********************************************************************72
c
cc I4_MODP returns the nonnegative remainder of integer division.
c
c  Discussion:
c
c    If
c      NREM = I4_MODP ( I, J )
c      NMULT = ( I - NREM ) / J
c    then
c      I = J * NMULT + NREM
c    where NREM is always nonnegative.
c
c    The MOD function computes a result with the same sign as the
c    quantity being divided.  Thus, suppose you had an angle A,
c    and you wanted to ensure that it was between 0 and 360.
c    Then mod(A,360) would do, if A was positive, but if A
c    was negative, your result would be between -360 and 0.
c
c    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
c
c  Example:
c
c        I     J     MOD I4_MODP    Factorization
c
c      107    50       7       7    107 =  2 *  50 + 7
c      107   -50       7       7    107 = -2 * -50 + 7
c     -107    50      -7      43   -107 = -3 *  50 + 43
c     -107   -50      -7      43   -107 =  3 * -50 + 43
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the number to be divided.
c
c    Input, integer J, the number that divides I.
c
c    Output, integer I4_MODP, the nonnegative remainder when I is
c    divided by J.
c
      implicit none

      integer i
      integer i4_modp
      integer j
      integer value

      if ( j .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODP - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
        stop
      end if

      value = mod ( i, j )

      if ( value .lt. 0 ) then
        value = value + abs ( j )
      end if

      i4_modp = value

      return
      end
      function i4_wrap ( ival, ilo, ihi )

c*********************************************************************72
c
cc I4_WRAP forces an I4 to lie between given limits by wrapping.
c
c  Example:
c
c    ILO = 4, IHI = 8
c
c    I  Value
c
c    -2     8
c    -1     4
c     0     5
c     1     6
c     2     7
c     3     8
c     4     4
c     5     5
c     6     6
c     7     7
c     8     8
c     9     4
c    10     5
c    11     6
c    12     7
c    13     8
c    14     4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IVAL, an integer value.
c
c    Input, integer ILO, IHI, the desired bounds for the integer value.
c
c    Output, integer I4_WRAP, a "wrapped" version of IVAL.
c
      implicit none

      integer i4_modp
      integer i4_wrap
      integer ihi
      integer ilo
      integer ival
      integer jhi
      integer jlo
      integer value
      integer wide

      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )

      wide = jhi - jlo + 1

      if ( wide .eq. 1 ) then
        value = jlo
      else
        value = jlo + i4_modp ( ival - jlo, wide )
      end if

      i4_wrap = value

      return
      end
      subroutine polygon_angles ( n, v, angle )

c*********************************************************************72
c
cc POLYGON_ANGLES computes the interior angles of a polygon.
c
c  Discussion:
c
c    The vertices should be listed in counter clockwise order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c
c    Input, double precision V(2,N), the vertices.
c
c    Output, double precision ANGLE(N), the angles of the polygon,
c    in radians.
c
      implicit none

      integer n

      double precision angle(n)
      double precision angle_rad
      integer i
      integer i4_wrap
      integer im1
      integer ip1
      double precision v(2,n)

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_ANGLES - Fatal error!'
        write ( *, '(a)' ) '  Must be at least 3 vertices.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop 1
      end if

      do i = 1, n

        im1 = i4_wrap ( i - 1, 1, n )
        ip1 = i4_wrap ( i + 1, 1, n )

        angle(i) = angle_rad ( v(1:2,im1), v(1:2,i), v(1:2,ip1) )

      end do

      return
      end
      subroutine polygon_area ( n, v, area )

c*********************************************************************72
c
cc POLYGON_AREA computes the area of a polygon.
c
c  Discussion:
c
c    AREA = 1/2 * abs ( sum ( 1 <= I <= N ) X(I) * ( Y(I+1) - Y(I-1) ) )
c    where Y(0) should be replaced by Y(N), and Y(N+1) by Y(1).
c
c    If the vertices are given in counter clockwise order, the area
c    will be positive.  If the vertices are given in clockwise order,
c    the area will be negative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c
c    Input, double precision V(2,N), the vertices.
c
c    Output, double precision AREA, the absolute area of the polygon.
c
      implicit none

      integer n

      double precision area
      integer i
      integer i4_wrap
      integer im1
      integer ip1
      double precision v(2,n)

      area = 0.0D+00

      do i = 1, n

        im1 = i4_wrap ( i-1, 1, n )
        ip1 = i4_wrap ( i+1, 1, n )

        area = area + v(1,i) * ( v(2,ip1) - v(2,im1) )

      end do

      area = 0.5D+00 * area

      return
      end
      subroutine polygon_area_2 ( n, v, area )

c*********************************************************************72
c
cc POLYGON_AREA_2 computes the area of a polygon.
c
c  Discussion:
c
c    The area is the sum of the areas of the triangles formed by
c    node N with consecutive pairs of nodes.
c
c    If the vertices are given in counter clockwise order, the area
c    will be positive.  If the vertices are given in clockwise order,
c    the area will be negative.
c
c    Thanks to Martin Pineault for noticing that an earlier version
c    of this routine would not correctly compute the area of a nonconvex
c    polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Adrian Bowyer, John Woodwark,
c    A Programmer's Geometry,
c    Butterworths, 1983,
c    ISBN: 0408012420.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c
c    Input, double precision V(2,N), the vertices.
c
c    Output, double precision AREA, the absolute area of the polygon.
c
      implicit none

      integer n

      double precision area
      double precision area_triangle
      integer i
      double precision triangle_area
      double precision v(2,n)

      area = 0.0D+00

      do i = 1, n - 2

        area_triangle = triangle_area (
     &    v(1,i),   v(2,i),
     &    v(1,i+1), v(2,i+1),
     &    v(1,n),   v(2,n) )

        area = area + area_triangle

      end do

      return
      end
      subroutine polygon_centroid ( n, v, centroid )

c*********************************************************************72
c
cc POLYGON_CENTROID computes the centroid of a polygon.
c
c  Discussion:
c
c    Denoting the centroid coordinates by CENTROID, then
c
c      CENTROID(1) = Integral ( Polygon interior ) x dx dy / Area ( Polygon )
c      CENTROID(2) = Integral ( Polygon interior ) y dx dy / Area ( Polygon ).
c
c    Green's theorem states that for continuously differentiable functions
c    M(x,y) and N(x,y),
c
c      Integral ( Polygon boundary ) ( M dx + N dy ) =
c      Integral ( Polygon interior ) ( dN/dx - dM/dy ) dx dy.
c
c    Using M(x,y) = 0 and N(x,y) = x*x/2, we get:
c
c      CENTROID(1) = 0.5 * Integral ( Polygon boundary ) x*x dy
c                  / Area ( Polygon ),
c
c    which becomes
c
c      CENTROID(1) = 1/6 sum ( 1 <= I <= N )
c        ( X(I+1) + X(I) ) * ( X(I) * Y(I+1) - X(I+1) * Y(I))
c        / Area ( Polygon )
c
c    where, when I = N, the index "I+1" is replaced by 1.
c
c    A similar calculation gives us a formula for CENTROID(2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 July 2003
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gerard Bashein, Paul Detmer,
c    Centroid of a Polygon,
c    in Graphics Gems IV,
c    edited by Paul Heckbert,
c    AP Professional, 1994,
c    T385.G6974.
c
c  Parameters:
c
c    Input, integer N, the number of sides of the polygon.
c
c    Input, double precision V(2,N), the coordinates of the vertices.
c
c    Output, double precision CENTROID(2), the coordinates of the centroid.
c
      implicit none

      integer n

      double precision area
      double precision centroid(2)
      integer i
      integer ip1
      double precision temp
      double precision v(2,n)

      area = 0.0D+00
      centroid(1:2) = 0.0D+00

      do i = 1, n

        if ( i .lt. n ) then
          ip1 = i + 1
        else
          ip1 = 1
        end if

        temp = ( v(1,i) * v(2,ip1) - v(1,ip1) * v(2,i) )

        area = area + temp

        centroid(1:2) = centroid(1:2) 
     &    + ( v(1:2,ip1) + v(1:2,i) ) * temp

      end do

      area = area / 2.0D+00

      if ( area .eq. 0.0D+00 ) then
        centroid(1:2) = v(1:2,1)
      else
        centroid(1:2) = centroid(1:2) / ( 6.0D+00 * area )
      end if

      return
      end
      subroutine polygon_centroid_2 ( n, v, centroid )

c*********************************************************************72
c
cc POLYGON_CENTROID_2 computes the centroid of a polygon.
c
c  Discussion:
c
c    The centroid is the area-weighted sum of the centroids of
c    disjoint triangles that make up the polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 July 2003
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Adrian Bowyer, John Woodwark,
c    A Programmer's Geometry,
c    Butterworths, 1983,
c    ISBN: 0408012420.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c
c    Input, double precision V(2,N), the coordinates of the vertices.
c
c    Output, double precision CENTROID(2), the coordinates of the centroid.
c
      implicit none

      integer n

      double precision area_polygon
      double precision area_triangle
      double precision centroid(2)
      integer i
      double precision triangle_area
      double precision v(2,n)

      area_polygon = 0.0D+00
      centroid(1:2) = 0.0D+00

      do i = 1, n - 2

        area_triangle = triangle_area (
     &    v(1,i),   v(2,i),
     &    v(1,i+1), v(2,i+1),
     &    v(1,n),   v(2,n) )

        area_polygon = area_polygon + area_triangle

        centroid(1:2) = centroid(1:2) + area_triangle
     &    * ( v(1:2,i) + v(1:2,i+1) + v(1:2,n) ) / 3.0D+00

      end do

      if ( area_polygon .eq. 0.0D+00 ) then
        centroid(1:2) = v(1:2,1)
      else
        centroid(1:2) = centroid(1:2) / area_polygon
      end if

      return
      end
      subroutine polygon_contains_point ( n, v, p, inside )

c*********************************************************************72
c
cc POLYGON_CONTAINS_POINT finds if a point is inside a simple polygon.
c
c  Discussion:
c
c    A simple polygon is one whose boundary never crosses itself.
c    The polygon does not need to be convex.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    M Shimrat,
c    ACM Algorithm 112,
c    Position of Point Relative to Polygon,
c    Communications of the ACM,
c    Volume 5, Number 8, page 434, August 1962.
c
c  Parameters:
c
c    Input, integer N, the number of nodes or vertices in
c    the polygon.  N must be at least 3.
c
c    Input, double precision V(2,N), the vertices of the polygon.
c
c    Input, double precision P(2), the coordinates of the point to be tested.
c
c    Output, logical INSIDE, is TRUE if the point is inside the polygon.
c
      implicit none

      integer n

      integer i
      logical inside
      integer ip1
      double precision p(2)
      double precision t
      double precision v(2,n)

      inside = .false.

      do i = 1, n

        if ( i .lt. n ) then
          ip1 = i + 1
        else
          ip1 = 1
        end if

        if ( ( v(2,i) .lt.  p(2) .and. p(2) .le. v(2,ip1) ) .or.         
     &    ( p(2) .le. v(2,i) .and. v(2,ip1) .lt. p(2) ) ) then

          t = ( p(1) - v(1,i) ) - ( p(2) - v(2,i) ) 
     &      * ( v(1,ip1) - v(1,i) ) / ( v(2,ip1) - v(2,i) )

          if ( t .lt. 0.0D+00 ) then
            inside = .not. inside
          end if

        end if

      end do

      return
      end
      subroutine polygon_contains_point_2 ( n, v, p, inside )

c*********************************************************************72
c
cc POLYGON_CONTAINS_POINT_2: is a point inside a convex polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of nodes or vertices in the
c    polygon.  N must be at least 3.
c
c    Input, double precision V(2,N), the vertices of the polygon.
c
c    Input, double precision P(2), the coordinates of the point to be tested.
c
c    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is inside
c    the polygon or on its boundary.
c
      implicit none

      integer n

      integer i
      logical inside
      double precision p(2)
      double precision t(2,3)
      double precision v(2,n)

      inside = .false.
c
c  A point is inside a convex polygon if and only if it is inside
c  one of the triangles formed by X(1),Y(1) and any two consecutive
c  points on the polygon's circumference.
c
      t(1:2,1) = v(1:2,1)

      do i = 2, n - 1

        t(1:2,2) = v(1:2,i)
        t(1:2,3) = v(1:2,i+1)

        call triangle_contains_point_1 ( t, p, inside )

        if ( inside ) then
          return
        end if

      end do

      return
      end
      subroutine polygon_diameter ( n, v, diameter )

c*********************************************************************72
c
cc POLYGON_DIAMETER computes the diameter of a polygon.
c
c  Discussion:
c
c    The diameter of a polygon is the maximum distance between any
c    two points on the polygon.  It is guaranteed that this maximum
c    distance occurs between two vertices of the polygon.  It is
c    sufficient to check the distance between all pairs of vertices.
c    This is an N^2 algorithm.  There is an algorithm by Shamos which
c    can compute this quantity in order N time instead.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c
c    Input, double precision V(2,N), the vertices.
c
c    Output, double precision DIAMETER, the diameter of the polygon.
c
      implicit none

      integer n

      double precision diameter
      integer i
      integer j
      double precision v(2,n)

      diameter = 0.0D+00

      do i = 1, n

        do j = i+1, n
          diameter = max ( diameter,
     &      sqrt ( ( v(1,i) - v(1,j) )**2 
     &           + ( v(2,i) - v(2,j) )**2 ) )
        end do

      end do

      return
      end
      subroutine polygon_expand ( n, v, h, w )

c*********************************************************************72
c
cc POLYGON_EXPAND expands a polygon.
c
c  Discussion:
c
c    This routine simple moves each vertex of the polygon outwards
c    in such a way that the sides of the polygon advance by H.
c
c    This approach should always work if the polygon is convex, or
c    star-shaped.  But for general polygons, it is possible
c    that this procedure, for large enough H, will create a polygon
c    whose sides intersect.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of sides of the polygon.
c
c    Input, double precision V(2,N), the coordinates of the vertices.
c
c    Input, double precision H, the expansion amount.
c
c    Output, double precision W(2,N), the "expanded" coordinates.
c
      implicit none

      integer n

      double precision angle
      double precision angle_rad
      double precision h
      double precision h2
      integer i
      integer i4_wrap
      integer im1
      integer ip1
      double precision p4(2)
      double precision v(2,n)
      double precision w(2,n)
c
c  Consider each angle, formed by the nodes P(I-1), P(I), P(I+1).
c
      do i = 1, n

        im1 = i4_wrap ( i-1, 1, n )
        ip1 = i4_wrap ( i+1, 1, n )
c
c        P1
c        /
c       /   P4
c      /  .
c     / .
c    P2--------->P3
c
        call angle_half ( v(1:2,im1), v(1:2,i), v(1:2,ip1), p4 )
c
c  Compute the value of the half angle.
c
        angle = angle_rad ( v(1:2,im1), v(1:2,i), p4(1:2) )
c
c  The stepsize along the ray must be adjusted so that the sides
c  move out by H.
c
        h2 = h / sin ( angle )

        w(1:2,i) = v(1:2,i) - h2 * ( p4(1:2) - v(1:2,i) )

      end do

      return
      end
      subroutine polygon_inrad_data ( n, radin, area, radout, side )

c*********************************************************************72
c
cc POLYGON_INRAD_DATA determines polygonal data from its inner radius.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 September 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of sides of the polygon.
c    N must be at least 3.
c
c    Input, double precision RADIN, the inner radius of the polygon, that is,
c    the radius of the largest circle that can be inscribed within
c    the polygon.
c
c    Output, double precision AREA, the area of the regular polygon.
c
c    Output, double precision RADOUT, the outer radius of the polygon, that is,
c    the radius of the smallest circle that can be described about
c    the polygon.
c
c    Output, double precision SIDE, the length of one side of the polygon.
c
      implicit none

      double precision angle
      double precision area
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision radin
      double precision radout
      double precision side

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INRAD_DATA - Fatal error!'
        write ( *, '(a)' ) '  Input value of N must be at least 3'
        write ( *, '(a,i8)' ) '  but your input value was N = ', n
        stop 1
      end if

      angle = r8_pi / dble ( n )
      area = dble ( n ) * radin * radin * tan ( angle )
      side = 2.0D+00 * radin * tan ( angle )
      radout = 0.5D+00 * side / sin ( angle )

      return
      end
      subroutine polygon_integral_1 ( n, v, result )

c*********************************************************************72
c
cc POLYGON_INTEGRAL_1 integrates the function 1 over a polygon.
c
c  Discussion:
c
c    The polygon is bounded by the points (X(1:N), Y(1:N)).
c
c    INTEGRAL = 0.5 * sum ( 1 <= I <= N )
c      ( X(I) + X(I-1) ) * ( Y(I) - Y(I-1) )
c
c    where X(0) and Y(0) should be replaced by X(N) and Y(N).
c
c    The integral of 1 over a polygon is the area of the polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    SF Bockman,
c    Generalizing the Formula for Areas of Polygons to Moments,
c    American Mathematical Society Monthly,
c    1989, pages 131-132.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c    N should be at least 3 for a nonzero result.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  These vertices should be given in counter clockwise order.
c
c    Output, double precision RESULT, the value of the integral.
c
      implicit none

      integer n

      integer i
      integer im1
      double precision result
      double precision v(2,n)

      result = 0.0D+00

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INTEGRAL_1 - Fatal error!'
        write ( *, '(a)' ) '  Number of vertices must be at least 3.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop 1
      end if

      do i = 1, n

        if ( i .eq. 1 ) then
          im1 = n
        else
          im1 = i - 1
        end if

        result = result + 0.5D+00 * 
     &    ( v(1,i) + v(1,im1) ) * ( v(2,i) - v(2,im1) )

      end do

      return
      end
      subroutine polygon_integral_x ( n, v, result )

c*********************************************************************72
c
cc POLYGON_INTEGRAL_X integrates the function X over a polygon.
c
c  Discussion:
c
c    The polygon is bounded by the points (X(1:N), Y(1:N)).
c
c    INTEGRAL = (1/6) * sum ( 1 <= I <= N )
c      ( X(I)*X(I) + X(I) * X(I-1) + X(I-1)*X(I-1) ) * ( Y(I) - Y(I-1) )
c
c    where X(0) and Y(0) should be replaced by X(N) and Y(N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    SF Bockman,
c    Generalizing the Formula for Areas of Polygons to Moments,
c    American Mathematical Society Monthly,
c    1989, pages 131-132.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c    N should be at least 3 for a nonzero result.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  These vertices should be given in counter clockwise order.
c
c    Output, double precision RESULT, the value of the integral.
c
      implicit none

      integer n

      integer i
      integer im1
      double precision result
      double precision v(2,n)

      result = 0.0D+00

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INTEGRAL_X - Fatal error!'
        write ( *, '(a)' ) '  Number of vertices must be at least 3.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop 1
      end if

      do i = 1, n

        if ( i .eq. 1 ) then
          im1 = n
        else
          im1 = i - 1
        end if

        result = result + 
     &    ( v(1,i)**2 
     &    + v(1,i) * v(1,im1) 
     &    + v(1,im1)**2 ) * ( v(2,i) - v(2,im1) )

      end do

      result = result / 6.0D+00

      return
      end
      subroutine polygon_integral_xx ( n, v, result )

c*********************************************************************72
c
cc POLYGON_INTEGRAL_XX integrates the function X*X over a polygon.
c
c  Discussion:
c
c    The polygon is bounded by the points (X(1:N), Y(1:N)).
c
c    INTEGRAL = (1/12) * sum ( 1 <= I <= N )
c      ( X(I)^3 + X(I)^2 * X(I-1) + X(I) * X(I-1)^2 + X(I-1)^3 )
c      * ( Y(I) - Y(I-1) )
c
c    where X(0) and Y(0) should be replaced by X(N) and Y(N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    SF Bockman,
c    Generalizing the Formula for Areas of Polygons to Moments,
c    American Mathematical Society Monthly,
c    1989, pages 131-132.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c    N should be at least 3 for a nonzero result.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  These vertices should be given in
c    counter clockwise order.
c
c    Output, double precision RESULT, the value of the integral.
c
      implicit none

      integer n

      integer i
      integer im1
      double precision result
      double precision v(2,n)

      result = 0.0D+00

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INTEGRAL_XX - Fatal error!'
        write ( *, '(a)' ) '  Number of vertices must be at least 3.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop 1
      end if

      do i = 1, n

        if ( i .eq. 1 ) then
          im1 = n
        else
          im1 = i - 1
        end if

        result = result + 
     &    ( v(1,i)**3 
     &    + v(1,i)**2 * v(1,im1)
     &    + v(1,i) * v(1,im1)**2 
     &    + v(1,im1)**3 ) * ( v(2,i) - v(2,im1) )

      end do

      result = result / 12.0D+00

      return
      end
      subroutine polygon_integral_xy ( n, v, result )

c*********************************************************************72
c
cc POLYGON_INTEGRAL_XY integrates the function X*Y over a polygon.
c
c  Discussion:
c
c    The polygon is bounded by the points (X(1:N), Y(1:N)).
c
c    INTEGRAL = (1/24) * sum ( 1 <= I <= N )
c      ( Y(I)   * ( 3 * X(I)^2 + 2 * X(I) * X(I-1) +     X(I-1)^2 )
c      + Y(I-1) * (     X(I)^2 + 2 * X(I) * X(I-1) + 3 * X(I-1)^2 ) )
c      * ( Y(I) - Y(I-1) )
c
c    where X(0) and Y(0) should be replaced by X(N) and Y(N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    SF Bockman,
c    Generalizing the Formula for Areas of Polygons to Moments,
c    American Mathematical Society Monthly,
c    1989, pages 131-132.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c    N should be at least 3 for a nonzero result.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  These vertices should be given in
c    counter clockwise order.
c
c    Output, double precision RESULT, the value of the integral.
c
      implicit none

      integer n

      integer i
      integer im1
      double precision result
      double precision v(2,n)

      result = 0.0D+00

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INTEGRAL_XY - Fatal error!'
        write ( *, '(a)' ) '  Number of vertices must be at least 3.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop 1
      end if

      do i = 1, n

        if ( i .eq. 1 ) then
          im1 = n
        else
          im1 = i - 1
        end if

        result = result + 
     &    ( v(2,i) * ( 3.0D+00 * v(1,i)**2 
     &    + 2.0D+00 * v(1,i) * v(1,im1) + v(1,im1)**2 ) 
     &    + v(2,im1) * ( v(1,i)**2 + 2.0D+00 * v(1,i) * v(1,im1)
     &    + 3.0D+00 * v(1,im1)**2 ) ) * ( v(2,i) - v(2,im1) )

      end do

      result = result / 24.0D+00

      return
      end
      subroutine polygon_integral_y ( n, v, result )

c*********************************************************************72
c
cc POLYGON_INTEGRAL_Y integrates the function Y over a polygon.
c
c  Discussion:
c
c    The polygon is bounded by the points (X(1:N), Y(1:N)).
c
c    INTEGRAL = (1/6) * sum ( 1 <= I <= N )
c      - ( Y(I)^2 + Y(I) * Y(I-1) + Y(I-1)^2 ) * ( X(I) - X(I-1) )
c
c    where X(0) and Y(0) should be replaced by X(N) and Y(N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    SF Bockman,
c    Generalizing the Formula for Areas of Polygons to Moments,
c    American Mathematical Society Monthly,
c    1989, pages 131-132.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c    N should be at least 3 for a nonzero result.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  These vertices should be given in
c    counter clockwise order.
c
c    Output, double precision RESULT, the value of the integral.
c
      implicit none

      integer n

      integer i
      integer im1
      double precision result
      double precision v(2,n)

      result = 0.0D+00

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INTEGRAL_Y - Fatal error!'
        write ( *, '(a)' ) '  Number of vertices must be at least 3.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop 1
      end if

      do i = 1, n

        if ( i .eq. 1 ) then
          im1 = n
        else
          im1 = i - 1
        end if

        result = result - 
     &    ( v(2,i)**2 
     &    + v(2,i) * v(2,im1) 
     &    + v(2,im1)**2 ) * ( v(1,i) - v(1,im1) )

      end do

      result = result / 6.0D+00

      return
      end
      subroutine polygon_integral_yy ( n, v, result )

c*********************************************************************72
c
cc POLYGON_INTEGRAL_YY integrates the function Y*Y over a polygon.
c
c  Discussion:
c
c    The polygon is bounded by the points (X(1:N), Y(1:N)).
c
c    INTEGRAL = (1/12) * sum ( 1 <= I <= N )
c      - ( Y(I)^3 + Y(I)^2 * Y(I-1) + Y(I) * Y(I-1)^2 + Y(I-1)^3 )
c      * ( X(I) - X(I-1) )
c
c    where X(0) and Y(0) should be replaced by X(N) and Y(N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    SF Bockman,
c    Generalizing the Formula for Areas of Polygons to Moments,
c    American Mathematical Society Monthly,
c    1989, pages 131-132.
c
c  Parameters:
c
c    Input, integer N, the number of vertices of the polygon.
c    N should be at least 3 for a nonzero result.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  These vertices should be given in
c    counter clockwise order.
c
c    Output, double precision RESULT, the value of the integral.
c
      implicit none

      integer n

      integer i
      integer im1
      double precision result
      double precision v(2,n)

      result = 0.0D+00

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_INTEGRAL_YY - Fatal error!'
        write ( *, '(a)' ) '  Number of vertices must be at least 3.'
        write ( *, '(a,i8)' ) '  The input polygon has N = ', n
        stop 1
      end if

      do i = 1, n

        if ( i .eq. 1 ) then
          im1 = n
        else
          im1 = i - 1
        end if

        result = result - 
     &    ( v(2,i)**3 
     &    + v(2,i)**2 * v(2,im1)
     &    + v(2,i) * v(2,im1)**2 
     &    + v(2,im1)**3 ) * ( v(1,i) - v(1,im1) )

      end do

      result = result / 12.0D+00

      return
      end
      function polygon_is_convex ( n, v )

c*********************************************************************72
c
cc POLYGON_IS_CONVEX determines whether a polygon is convex.
c
c  Discussion:
c
c    If the polygon has less than 3 distinct vertices, it is
c    classified as convex degenerate.
c
c    If the polygon "goes around" more than once, it is classified
c    as NOT convex.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Peter Schorn, Frederick Fisher,
c    Testing the Convexity of a Polygon,
c    in Graphics Gems IV,
c    edited by Paul Heckbert,
c    AP Professional, 1994,
c    T385.G6974.
c
c  Parameters
c
c    Input, integer N, the number of vertices.
c
c    Input/output, double precision V(2,N), the coordinates of the vertices
c    of the polygon.  On output, duplicate consecutive points have been
c    deleted, and the vertices have been reordered so that the
c    lexicographically least point comes first.
c
c    Output, integer POLYGON_IS_CONVEX:
c    -1, the polygon is not convex;
c     0, the polygon has less than 3 vertices; it is "degenerately" convex;
c     1, the polygon is convex and counter clockwise;
c     2, the polygon is convex and clockwise.
c
      implicit none

      integer n

      double precision angle
      integer CONVEX_CCW
      parameter ( CONVEX_CCW = 1 )
      integer CONVEX_CW
      parameter ( CONVEX_CW = 2 )
      double precision cross
      integer DEGENERATE_CONVEX
      parameter ( DEGENERATE_CONVEX = 0 )
      double precision dot
      double precision exterior_total
      integer i
      integer ip1
      integer ip2
      integer NOT_CONVEX
      parameter ( NOT_CONVEX = -1 )
      integer polygon_is_convex
      double precision RAD_TO_DEG 
      parameter ( RAD_TO_DEG = ( 180.0D+00 / 3.141592653589793D+00 ) )
      double precision sense
      double precision tol
      parameter ( tol = 1.0D+00 )
      double precision v(2,n)

      exterior_total = 0.0D+00
c
c  If there are not at least 3 distinct vertices, we are done.
c
      if ( n .lt. 3 ) then
        polygon_is_convex = DEGENERATE_CONVEX
        return
      end if

      sense = 0.0D+00
c
c  Consider each polygonal vertex I.
c
      do i = 1, n

        ip1 = i + 1
        if ( n .lt. ip1 ) then
          ip1 = ip1 - n
        end if

        ip2 = i + 2
        if ( n .lt. ip2 ) then
          ip2 = ip2 - n
        end if

        dot = ( v(1,ip2) - v(1,ip1) ) * ( v(1,i) - v(1,ip1) )         
     &    + ( v(2,ip2) - v(2,ip1) ) * ( v(2,i) - v(2,ip1) )

        cross = ( v(1,ip2) - v(1,ip1) ) * ( v(2,i) - v(2,ip1) )       
     &    - ( v(1,i)   - v(1,ip1) ) * ( v(2,ip2) - v(2,ip1) )

        angle = atan2 ( cross, dot )
c
c  See if the turn defined by this vertex is our first indication of
c  the "sense" of the polygon, or if it disagrees with the previously
c  defined sense.
c
        if ( sense .eq. 0.0D+00 ) then

          if ( angle .lt. 0.0D+00 ) then
            sense = -1.0D+00
          else if ( 0.0D+00 .lt. angle ) then
            sense = +1.0D+00
          end if

        else if ( sense .eq. 1.0D+00 ) then

          if ( angle .lt. 0.0D+00 ) then
            polygon_is_convex = NOT_CONVEX
            return
          end if

        else if ( sense .eq. -1.0D+00 ) then

          if ( 0.0D+00 .lt. angle ) then
            polygon_is_convex = NOT_CONVEX
            return
          end if

        end if
c
c  If the exterior total is greater than 360, then the polygon is
c  going around again.
c
        angle = atan2 ( -cross, -dot )

        exterior_total = exterior_total + angle

        if ( 360.0D+00 + tol .lt. 
     &    abs ( exterior_total ) * RAD_TO_DEG ) then
          polygon_is_convex = NOT_CONVEX
          return
        end if

      end do

      if ( sense .eq. +1.0D+00 ) then
        polygon_is_convex = CONVEX_CCW
      else if ( sense .eq. -1.0D+00 ) then
        polygon_is_convex = CONVEX_CW
      end if

      return
      end
      subroutine polygon_lattice_area ( i, b, area )

c*********************************************************************72
c
cc POLYGON_LATTICE_AREA computes the area of a lattice polygon.
c
c  Discussion:
c
c    We define a lattice to be the 2D plane, in which the points
c    whose (X,Y) coordinates are both integers are given a special
c    status as "lattice points".
c
c    A lattice polygon is a polygon whose vertices are lattice points.
c
c    The area of a lattice polygon can be computed by Pick's Theorem:
c
c      Area = I + B / 2 - 1
c
c    where
c
c      I = the number of lattice points contained strictly inside the polygon;
c
c      B = the number of lattice points that lie exactly on the boundary.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Branko Gruenbaum, Geoffrey Shephard,
c    Pick's Theorem,
c    The American Mathematical Monthly,
c    Volume 100, Number 2, February 1993, pages 150-161.
c
c  Parameters:
c
c    Input, integer I, the number of interior lattice points.
c
c    Input, integer B, the number of boundary lattice points.
c
c    Output, double precision AREA, the area of the lattice polygon.
c
      implicit none

      double precision area
      integer b
      integer i

      area = dble ( i ) + dble ( b ) / 2.0D+00 - 1.0D+00

      return
      end
      subroutine polygon_outrad_data ( n, radout, area, radin, side )

c*********************************************************************72
c
cc POLYGON_OUTRAD_DATA determines polygonal data from its outer radius.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 September 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of sides of the polygon.
c    N must be at least 3.
c
c    Input, double precision RADOUT, the outer radius of the polygon, that is,
c    the radius of the smallest circle that can be described
c    around the polygon.
c
c    Output, double precision AREA, the area of the regular polygon.
c
c    Output, double precision RADIN, the inner radius of the polygon, that is,
c    the radius of the largest circle that can be inscribed
c    within the polygon.
c
c    Output, double precision SIDE, the length of one side of the polygon.
c
      implicit none

      double precision angle
      double precision area
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision radin
      double precision radout
      double precision side

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_OUTRAD_DATA - Fatal error!'
        write ( *, '(a)' ) '  Input value of N must be at least 3'
        write ( *, '(a,i8)' ) '  but your input value was N = ', n
        stop 1
      end if

      angle = r8_pi / dble ( n )
      area = 0.5D+00 * dble ( n ) * radout * radout 
     &  * sin ( 2.0D+00 * angle )
      side = 2.0D+00 * radout * sin ( angle )
      radin = 0.5D+00 * side / tan ( angle )

      return
      end
      subroutine polygon_point_dist ( n, v, p, dist )

c*********************************************************************72
c
cc POLYGON_POINT_DIST: distance ( polygon, point ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of vertices.
c
c    Input, double precision V(2,N), the triangle vertices.
c
c    Input, double precision P(2), the point to be checked.
c
c    Output, double precision DIST, the distance from the point to the
c    polygon.
c
      implicit none

      integer n

      double precision dist
      double precision dist2
      integer i4_wrap
      integer j
      integer jp1
      double precision p(2)
      double precision r8_huge
      double precision v(2,n)
c
c  Find the distance to each of the line segments.
c
      dist = r8_huge ( )

      do j = 1, n

        jp1 = i4_wrap ( j+1, 1, n )

        call segment_point_dist ( v(1:2,j), v(1:2,jp1), p, dist2 )

        if ( dist2 .lt. dist ) then
          dist = dist2
        end if

      end do

      return
      end
      subroutine polygon_point_near ( n, v, p, pn, dist )

c*********************************************************************72
c
cc POLYGON_POINT_NEAR computes the nearest point on a polygon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V(2,N), the polygon vertices.
c
c    Input, double precision P(2), the point whose nearest polygon point
c    is to be determined.
c
c    Output, double precision PN(2), the nearest point to P.
c
c    Output, double precision DIST, the distance from the point to the
c    polygon.
c
      implicit none

      integer n

      double precision dist
      double precision dist2
      integer i4_wrap
      integer j
      integer jp1
      double precision p(2)
      double precision pn(2)
      double precision pn2(2)
      double precision r8_huge
      double precision tval
      double precision v(2,n)
c
c  Find the distance to each of the line segments that make up the edges
c  of the polygon.
c
      dist = r8_huge ( )
      pn(1) = 0.0D+00
      pn(2) = 0.0D+00

      do j = 1, n

        jp1 = i4_wrap ( j+1, 1, n )

        call segment_point_near ( v(1:2,j), v(1:2,jp1), p,
     &    pn2, dist2, tval )

        if ( dist2 .lt. dist ) then
          dist = dist2
          pn(1:2) = pn2(1:2)
        end if

      end do

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
      subroutine polygon_side_data ( n, side, area, radin, radout )

c*********************************************************************72
c
cc POLYGON_SIDE_DATA determines polygonal data from its side length.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of sides of the polygon.
c    N must be at least 3.
c
c    Input, double precision SIDE, the length of one side of the polygon.
c
c    Output, double precision AREA, the area of the regular polygon.
c
c    Output, double precision RADIN, the inner radius of the polygon, that is,
c    the radius of the largest circle that can be inscribed within
c    the polygon.
c
c    Output, double precision RADOUT, the outer radius of the polygon, that is,
c    the radius of the smallest circle that can be described about
c    the polygon.
c
      implicit none

      double precision angle
      double precision area
      integer n
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision radin
      double precision radout
      double precision side

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_SIDE_DATA - Fatal error!'
        write ( *, '(a)' ) '  Input value of N must be at least 3'
        write ( *, '(a,i8)' ) '  but your input value was N = ', n
        stop 1
      end if

      angle = r8_pi / dble ( n )
      area = 0.25D+00 * dble ( n ) * side * side / tan ( angle )
      radin = 0.5D+00 * side / tan ( angle )
      radout = 0.5D+00 * side / sin ( angle )

      return
      end
      function r8_degrees ( radians )

c*********************************************************************72
c
cc R8_DEGREES converts an angle from radian to degree measure.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision RADIANS, the angle measurement in radians.
c
c    Output, double precision R8_DEGREES, the angle measurement in degrees.
c
      implicit none

      double precision r8_degrees
      double precision r8_pi
      parameter ( r8_pi = 3.1415926535897932384626434D+00 )
      double precision radians

      r8_degrees = radians * 180.0D+00 / r8_pi

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

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
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_solve ( n, rhs_num, a, info )

c*********************************************************************72
c
cc R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer RHS_NUM, the number of right hand sides.
c    RHS_NUM must be at least 0.
c
c    Input/output, double precision A(N,N+rhs_num), contains in rows and
c    columns 1 to N the coefficient matrix, and in columns N+1 through
c    N+rhs_num, the right hand sides.  On output, the coefficient matrix
c    area has been destroyed, while the right hand sides have
c    been overwritten with the corresponding solutions.
c
c    Output, integer INFO, singularity flag.
c    0, the matrix was not singular, the solutions were computed;
c    J, factorization failed on step J, and the solutions could not
c    be computed.
c
      implicit none

      integer n
      integer rhs_num

      double precision a(n,n+rhs_num)
      double precision apivot
      double precision factor
      integer i
      integer info
      integer ipivot
      integer j
      integer k
      double precision t

      info = 0

      do j = 1, n
c
c  Choose a pivot row.
c
        ipivot = j
        apivot = a(j,j)

        do i = j+1, n
          if ( abs ( apivot ) .lt. abs ( a(i,j) ) ) then
            apivot = a(i,j)
            ipivot = i
          end if
        end do

        if ( apivot .eq. 0.0D+00 ) then
          info = j
          return
        end if
c
c  Interchange.
c
        do i = 1, n + rhs_num
          t = a(ipivot,i)
          a(ipivot,i) = a(j,i)
          a(j,i) = t
        end do
c
c  A(J,J) becomes 1.
c
        a(j,j) = 1.0D+00
        do k = j + 1, n + rhs_num
          a(j,k) = a(j,k) / apivot
        end do
c
c  A(I,J) becomes 0.
c
        do i = 1, n

          if ( i .ne. j ) then

            factor = a(i,j)
            a(i,j) = 0.0D+00
            do k = j + 1, n + rhs_num
              a(i,k) = a(i,k) - factor * a(j,k)
            end do

          end if

        end do

      end do

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
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
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

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
      subroutine segment_point_dist ( p1, p2, p, dist )

c*********************************************************************72
c
cc SEGMENT_POINT_DIST: distance ( line segment, point ).
c
c  Discussion:
c
c    A line segment is the finite portion of a line that lies between
c    two points P1 and P2.
c
c    The nearest point will satisfy the condition
c
c      PN = (1-T) * P1 + T * P2.
c
c    T will always be between 0 and 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 May 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), the endpoints of the line segment.
c
c    Input, double precision P(2), the point whose nearest neighbor on the line
c    segment is to be determined.
c
c    Output, double precision DIST, the distance from the point to the
c    line segment.
c
      implicit none

      double precision bot
      double precision dist
      double precision p(2)
      double precision p1(2)
      double precision p2(2)
      double precision pn(2)
      double precision t
c
c  If the line segment is actually a point, then the answer is easy.
c
      if ( p1(1) .eq. p2(1) .and.
     &     p1(2) .eq. p2(2) ) then

        t = 0.0D+00

      else

        bot = ( ( p2(1) - p1(1) )**2 + ( p2(2) - p1(2) )**2 )

        t = ( ( p(1)  - p1(1) ) * ( p2(1) - p1(1) ) 
     &      + ( p(2)  - p1(2) ) * ( p2(2) - p1(2) ) ) / bot

        t = max ( t, 0.0D+00 )
        t = min ( t, 1.0D+00 )

      end if

      pn(1) = p1(1) + t * ( p2(1) - p1(1) )
      pn(2) = p1(2) + t * ( p2(2) - p1(2) )

      dist = sqrt ( ( p(1) - pn(1) )**2 
     &            + ( p(2) - pn(2) )**2 )

      return
      end
      subroutine segment_point_near ( p1, p2, p, pn, dist, t )

c*********************************************************************72
c
cc SEGMENT_POINT_NEAR: nearest point on line segment to point.
c
c  Discussion:
c
c    A line segment is the finite portion of a line that lies between
c    two points P1 and P2.
c
c    The nearest point will satisfy the condition
c
c      PN = (1-T) * P1 + T * P2.
c
c    T will always be between 0 and 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 May 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), the endpoints of the line segment.
c
c    Input, double precision P(2), the point whose nearest neighbor
c    on the line segment is to be determined.
c
c    Output, double precision PN(2), the point on the line segment which is
c    nearest the point P.
c
c    Output, double precision DIST, the distance from the point to the
c    nearest point on the line segment.
c
c    Output, double precision T, the relative position of the point PN
c    to the points P1 and P2.
c
      implicit none

      double precision bot
      double precision dist
      double precision p(2)
      double precision p1(2)
      double precision p2(2)
      double precision pn(2)
      double precision t
c
c  If the line segment is actually a point, then the answer is easy.
c
      if ( p1(1) .eq. p2(1) .and.
     &     p1(2) .eq. p2(2) ) then

        t = 0.0D+00

      else

        bot = ( ( p2(1) - p1(1) )**2 + ( p2(2) - p1(2) )**2 )

        t = ( ( p(1)  - p1(1) ) * ( p2(1) - p1(1) ) 
     &      + ( p(2)  - p1(2) ) * ( p2(2) - p1(2) ) ) / bot

        t = max ( t, 0.0D+00 )
        t = min ( t, 1.0D+00 )

      end if

      pn(1) = p1(1) + t * ( p2(1) - p1(1) )
      pn(2) = p1(2) + t * ( p2(2) - p1(2) )

      dist = sqrt ( ( p(1) - pn(1) )**2 
     &            + ( p(2) - pn(2) )**2 )

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
      function triangle_area ( xa, ya, xb, yb, xc, yc )

c*********************************************************************72
c
cc TRIANGLE_AREA computes the signed area of a triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision XA, YA, XB, YB, XC, YC, the coordinates of
c    the vertices of the triangle, given in counterclockwise order.
c
c    Output, double precision TRIANGLE_AREA, the signed area of the triangle.
c
      implicit none

      double precision triangle_area
      double precision value
      double precision xa
      double precision xb
      double precision xc
      double precision ya
      double precision yb
      double precision yc

      value = 0.5D+00 * (
     &   ( xb - xa ) * ( yc - ya )
     & - ( xc - xa ) * ( yb - ya ) )

      triangle_area = value

      return
      end
      subroutine triangle_barycentric ( t, p, xsi )

c*********************************************************************72
c
cc TRIANGLE_BARYCENTRIC finds the barycentric coordinates of a point.
c
c  Discussion:
c
c    The barycentric coordinate of point P related to vertex A can be
c    interpreted as the ratio of the area of the triangle with
c    vertex A replaced by vertex P to the area of the original
c    triangle.
c
c    This routine assumes that the triangle vertices are given in
c    counter clockwise order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c    The vertices should be given in counter clockwise order.
c
c    Input, double precision P(2), the point to be checked.
c
c    Output, double precision XSI(3), the barycentric coordinates of P
c    with respect to the triangle.
c
      implicit none

      double precision a(2,3)
      integer info
      double precision p(2)
      double precision t(2,3)
      double precision xsi(3)
c
c  Set up the linear system
c
c    ( X2-X1  X3-X1 ) XSI(1)  = X-X1
c    ( Y2-Y1  Y3-Y1 ) XSI(2)    Y-Y1
c
c  which is satisfied by the barycentric coordinates of P.
c
      a(1,1) = t(1,2) - t(1,1)
      a(1,2) = t(1,3) - t(1,1)
      a(1,3) = p(1)   - t(1,1)

      a(2,1) = t(2,2) - t(2,1)
      a(2,2) = t(2,3) - t(2,1)
      a(2,3) = p(2)   - t(2,1)
c
c  Solve the linear system.
c
      call r8mat_solve ( 2, 1, a, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_BARYCENTRIC - Fatal error!'
        write ( *, '(a)' ) '  The linear system is singular.'
        write ( *, '(a)' ) '  Data does not form a proper triangle.'
        stop 1
      end if

      xsi(1) = a(1,3)
      xsi(2) = a(2,3)
      xsi(3) = 1.0D+00 - xsi(1) - xsi(2)

      return
      end
      subroutine triangle_contains_point_1 ( t, p, inside )

c*********************************************************************72
c
cc TRIANGLE_CONTAINS_POINT_1 finds if a point is inside a triangle.
c
c  Discussion:
c
c    It is conventional to list the triangle vertices in counter clockwise
c    order.  However, this routine does not require a particular order
c    for the vertices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Input, double precision P(2), the point to be checked.
c
c    Output, logical INSIDE, is TRUE if the point is inside the triangle.
c
      implicit none

      integer i
      logical inside
      double precision p(2)
      double precision t(2,3)
      double precision xsi(3)

      call triangle_barycentric ( t, p, xsi )

      inside = .true.

      do i = 1, 3
        if ( xsi(i) .lt. 0.0D+00 ) then
          inside = .false.
        end if
      end do

      return
      end
