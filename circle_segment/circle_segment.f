      function ch_is_digit ( c )

c*********************************************************************72
c
cc CH_IS_DIGIT returns TRUE if a character is a decimal digit.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C, the character to be analyzed.
c
c    Output, logical CH_IS_DIGIT, TRUE if C is a digit, FALSE otherwise.
c
      implicit none

      character c
      logical ch_is_digit

      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then
        ch_is_digit = .true.
      else
        ch_is_digit = .false.
      end if

      return
      end
      subroutine ch_to_digit ( c, digit )

c*********************************************************************72
c
cc CH_TO_DIGIT returns the integer value of a base 10 digit.
c
c  Example:
c
c     C   DIGIT
c    ---  -----
c    '0'    0
c    '1'    1
c    ...  ...
c    '9'    9
c    ' '    0
c    'X'   -1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C, the decimal digit, '0' through '9' or blank
c    are legal.
c
c    Output, integer DIGIT, the corresponding integer value.  If C was
c    'illegal', then DIGIT is -1.
c
      implicit none

      character c
      integer digit

      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

        digit = ichar ( c ) - 48

      else if ( c .eq. ' ' ) then

        digit = 0

      else

        digit = -1

      end if

      return
      end
      subroutine circle_segment_angle_from_chord ( r, c, p1, p2, theta )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_ANGLE_FROM_CHORD computes the angle of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    06 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision P1(2), P2(2), the ends of the chord.
c
c    Output, double precision THETA, the angle of the circle segment.
c    0 .le. THETA .lt. 2 * PI.
c
      implicit none

      double precision c(2)
      integer i
      double precision p1(2)
      double precision p2(2)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_atan
      double precision theta
      double precision v1(2)
      double precision v2(2)
c
c  Compute the radial vectors V1 and V2.
c
      v1(1) = p1(1) - c(1)
      v1(2) = p1(2) - c(2)
      v2(1) = p2(1) - c(1)
      v2(2) = p2(2) - c(2)
c
c  The arc cosine will only give us an answer between 0 and PI.
c
      theta = r8_atan ( v2(2), v2(1) ) - r8_atan ( v1(2), v1(1) )
c
c  Force 0 .le. THETA .lt. 2 * PI.
c
10    continue

      if ( theta .lt. 0.0D+00 ) then
        theta = theta + 2.0D+00 * pi
        go to 10
      end if

20    continue

      if ( 2.0D+00 * pi .le. theta ) then
        theta = theta - 2.0D+00 * pi
        go to 20
      end if

      return
      end
      subroutine circle_segment_angle_from_chord_angles ( omega1, 
     &  omega2, theta )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_ANGLE_FROM_CHORD_ANGLES computes angle of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision OMEGA1, OMEGA2, the angles of the points P1 
c    and P2.  OMEGA1 .le. OMEGA2.
c
c    Output, double precision THETA, the angle of the circle segment.
c    Essentially, THETA = OMEGA2 - OMEGA1.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision omega1
      double precision omega2
      double precision theta

10    continue

      if ( omega2 .lt. omega1 ) then
        omega2 = omega2 + 2.0D+00 * pi
        go to 10
      end if

      theta = omega2 - omega1

      return
      end
      subroutine circle_segment_angle_from_height ( r, h, theta )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_ANGLE_FROM_HEIGHT computes the angle of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
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
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision H, the "height" of the circle segment.
c    0 .le. H .le. 2 * R.
c
c    Output, double precision THETA, the angle of the circle segment.
c
      implicit none

      double precision h
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_asin
      double precision t
      double precision theta

      if ( h .le. 0.0D+00 ) then

        theta = 0.0D+00

      else if ( h .le. r ) then

        t = sqrt ( r * r - ( r - h ) * ( r - h ) ) / r
        theta = 2.0D+00 * r8_asin ( t )

      else if ( h .le. 2.0D+00 * r ) then

        t = sqrt ( r * r - ( r - h ) * ( r - h ) ) / r
        theta = 2.0D+00 * r8_asin ( t )
        theta = 2.0D+00 * pi - theta

      else

        theta = 2.0D+00 * pi

      end if

      return
      end
      subroutine circle_segment_area_from_angle ( r, theta, area )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_AREA_FROM_ANGLE computes the area of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision THETA, the angle of the circle segment.
c
c    Output, double precision AREA, the area of the circle segment.
c
      implicit none

      double precision area
      double precision r
      double precision theta

      area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

      return
      end
      subroutine circle_segment_area_from_chord ( r, c, p1, p2, area )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_AREA_FROM_CHORD computes the area of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision P1(2), P2(2), the ends of the chord.
c
c    Output, double precision AREA, the area of the circle segment.
c
      implicit none

      double precision area
      double precision c(2)
      double precision p1(2)
      double precision p2(2)
      double precision r
      double precision theta

      call circle_segment_angle_from_chord ( r, c, p1, p2, theta )

      area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

      return
      end
      subroutine circle_segment_area_from_height ( r, h, area )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_AREA_FROM_HEIGHT computes the area of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision H, the height of the circle segment.
c    0 .le. H .le. 2 * R.
c
c    Output, double precision AREA, the area of the circle segment.
c
      implicit none

      double precision area
      double precision h
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_asin
      double precision t
      double precision theta

      if ( h .le. 0.0D+00 ) then

        area = 0.0D+00

      else if ( h .le. r ) then

        t = sqrt ( r * r - ( r - h ) * ( r - h ) ) / r
        theta = 2.0D+00 * r8_asin ( t )
        area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

      else if ( h .le. 2.0D+00 * r ) then

        t = sqrt ( r * r - ( r - h ) * ( r - h ) ) / r
        theta = 2.0D+00 * r8_asin ( t )
        theta = 2.0D+00 * pi - theta
        area = r * r * ( theta - sin ( theta ) ) / 2.0D+00

      else

        area = pi * r * r

      end if

      return
      end
      subroutine circle_segment_area_from_sample ( r, c, p1, p2, n, 
     &  seed, area )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_AREA_FROM_SAMPLE computes the area of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2,1), the center of the circle.
c
c    Input, double precision P1(2,1), P2(2,1), the ends of the chord.
c
c    Input, integer N, the number of sample points.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision AREA, the area of the circle segment.
c
      implicit none

      integer n

      double precision angle(n)
      double precision area
      double precision c(2)
      integer i
      integer m
      double precision omega1
      double precision omega2
      double precision p(2)
      double precision p1(2)
      double precision p2(2)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r2(n)
      double precision r8_atan
      double precision rmh
      integer seed
      double precision vdotp(n)
      double precision x(n)
      double precision y(n)
c
c  Determine the angles of the chord endpoints.
c
      omega1 = r8_atan ( p1(2) - c(2), p1(1) - c(1) )

10    continue

      if ( omega1 .lt. 0.0D+00 ) then
        omega1 = omega1 + 2.0D+00 * pi
        go to 10
      end if

      omega2 = r8_atan ( p2(2) - c(2), p2(1) - c(1) )

20    continue

      if ( omega2 .lt. omega1 ) then
        omega2 = omega2 + 2.0D+00 * pi
        go to 20
      end if
c
c  Get N random points in the circle.
c  To simplify angle measurement, take OMEGA1 as your smallest angle.
c  That way, the check OMEGA1 .le. ANGLE .le. OMEGA2 will be legitimate.
c
      call r8vec_uniform_01 ( n, seed, angle )

      do i = 1, n
        angle(i) = omega1 + 2.0D+00 * pi * angle(i)
      end do
     
      call r8vec_uniform_01 ( n, seed, r2 )

      do i = 1, n
        r2(i) = sqrt ( r2(i) )
        x(i) = c(1) + r2(i) * cos ( angle(i) )
        y(i) = c(2) + r2(i) * sin ( angle(i) )
      end do
c
c  Determine the vector that touches the circle segment base.
c
      p(1) = 0.5D+00 * ( p1(1) + p2(1) ) - c(1)
      p(2) = 0.5D+00 * ( p1(2) + p2(2) ) - c(2)

      rmh = sqrt ( p(1)**2 + p(2)**2 )

      p(1) = p(1) / rmh
      p(2) = p(2) / rmh

      if ( pi .lt. omega2 - omega1 ) then
        p(1) = - p(1)
        p(2) = - p(2)
        rmh =  - rmh
      end if
c
c  Compute the projection of each point onto P.
c
      do i = 1, n
        vdotp(i) = ( x(i) - c(1) ) * p(1) + ( y(i) - c(2) ) * p(2)
      end do
c
c  Points in the segment lie in the sector, and project at least RMH onto P.
c
      m = 0
      do i = 1, n
        if ( omega1 .lt. angle(i) .and. 
     &                angle(i) .lt. omega2 .and. 
     &       rmh .lt. vdotp(i) ) then
          m = m + 1
        end if
      end do
c
c  The area of the segment is its relative share of the circle area.
c
      area = pi * r**2 * dble ( m ) / dble ( n )

      return
      end
      subroutine circle_segment_cdf ( r, h, h2, cdf )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_CDF computes a CDF related to a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c    Now, suppose we want to assign a cumulative density function or CDF
c    based on a variable H2 which measures the height of the circle segment
c    formed by an arbitrary point in the circle segment.  CDF(H2) will
c    measure the probability that a point drawn uniformly at random from
c    the circle segment defines a (smaller) circle segment of height H2.
c
c    If we can define this CDF, then we will be able to sample uniformly
c    from the circle segment, since our "Y" value can be determined from H2,
c    and our X value is chosen uniformly over the horizontal chord 
c    through (0,Y).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision H, the "height" of the circle segment.
c    0 .le. H .le. 2 * R.
c
c    Input, double precision H2, the "height" of the new circle segment 
c    defined by a given point in the circle segment.  0 .le. H2 .le. H.
c
c    Output, double precision CDF, the cumulative density function for H2, 
c    the probability that a point chosen at random in the circle segment 
c    would define a smaller circle segment of height H2 or less.
c
      implicit none

      double precision a
      double precision a2
      double precision cdf
      double precision h
      double precision h2
      double precision r

      if ( h2 .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else if ( h .le. h2 ) then
        cdf = 1.0D+00
      else
        call circle_segment_area_from_height ( r, h,  a  )
        call circle_segment_area_from_height ( r, h2, a2 )
        cdf = a2 / a
      end if

      return
      end
      subroutine circle_segment_centroid_from_chord ( r, c, p1, p2, d )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_CENTROID_FROM_CHORD computes the centroid of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c    For this function, we assume that the center of the circle is at (0,0),
c    that the chord is horizontal, and that the circle segment is at the top.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision P1(2), P2(2), the coordinates of the endpoints 
c    of the chord.
c
c    Output, double precision D(2), the coordinates of the centroid.
c
      implicit none

      double precision c(2)
      double precision d(2)
      double precision p1(2)
      double precision p2(2)
      double precision r
      double precision s
      double precision theta
      double precision thetah
      double precision v1(2)
c
c  Get the angle subtended by P1:P2.
c
      call circle_segment_angle_from_chord ( r, c, p1, p2, theta )
c
c  Construct V1, the vector from C to P1.
c
      v1(1) = p1(1) - c(1)
      v1(2) = p1(2) - c(2)
c
c  Rotate V1 through THETA / 2.
c
      thetah = theta / 2.0D+00

      d(1) = cos ( thetah ) * v1(1) - sin ( thetah ) * v1(2)
      d(2) = sin ( thetah ) * v1(1) + cos ( thetah ) * v1(2)
c
c  Scale this vector so it represents the distance to the centroid
c  relative to R.
c
      s = 4.0D+00 * ( sin ( theta / 2.0 ) ) ** 3 
     &  / 3.0D+00 / ( theta - sin ( theta ) )

      d(1) = s * d(1)
      d(2) = s * d(2)
c
c  Add the center.
c
      d(1) = d(1) + c(1)
      d(2) = d(2) + c(2)

      return
      end
      subroutine circle_segment_centroid_from_height ( r, h, d )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_CENTROID_FROM_HEIGHT computes centroid of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c    For this function, we assume that the center of the circle is at (0,0).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision H, the "height" of the circle segment.
c    0 .le. H .le. 2 * R.
c
c    Output, double precision D(2), the coordinates of the centroid.
c
      implicit none

      double precision d(2)
      double precision h
      double precision r
      double precision theta
      double precision x
      double precision y

      call circle_segment_angle_from_height ( r, h, theta )

      d(1) = 0.0D+00
      d(2) = 4.0D+00 * r * ( sin ( theta / 2.0D+00 ) ) ** 3 / 3.0D+00 
     &  / ( theta - sin ( theta ) )

      return
      end
      subroutine circle_segment_centroid_from_sample ( r, c, p1, p2, 
     &  n, seed, d )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_CENTROID_FROM_SAMPLE estimates a circle segment centroid.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision P1(2), P2(2), the ends of the chord.
c
c    Input, integer N, the number of sample points.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision D(2), the estimated centroid of the 
c    circle segment.
c
      implicit none

      integer n

      double precision c(2)
      double precision d(2)
      double precision p1(2)
      double precision p2(2)
      double precision r
      double precision r8vec_sum
      integer seed
      double precision x(n)
      double precision y(n)

      call circle_segment_sample_from_chord ( r, c, p1, p2, n, seed, 
     &  x, y )

      d(1) = r8vec_sum ( n, x ) / dble ( n )
      d(2) = r8vec_sum ( n, y ) / dble ( n )

      return
      end
      subroutine circle_segment_contains_point ( r, c, omega1, omega2, 
     &  xy, value )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_CONTAINS_POINT reports whether a point is in a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c    In this function, we allow the circle to have an arbitrary center C,
c    arbitrary radius R, and we describe the points P1 and P2 by specifying
c    their angles OMEGA1 and OMEGA2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision OMEGA1, OMEGA2, the angles of the two points on 
c    the circumference of the circle that define the circle segment.
c    OMEGA1 .lt. OMEGA2 .le. OMEGA1 + 2 * PI
c
c    Input, double precision XY(2), a point.
c
c    Output, integer VALUE, is TRUE if the point is inside 
c    the circle segment.
c
      implicit none

      double precision c(2)
      double precision h
      double precision omega1
      double precision omega2
      double precision omegah
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision theta
      double precision v(2)
      double precision v_omega
      double precision v_project
      double precision v_r
      integer value
      double precision xy(2)

      if ( r .le. 0.0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_CONTAINS_POINT - Fatal error!'
        write ( *, '(a)' ) '  R .le. 0.0.'
        stop
      end if

10    continue

      if ( omega2 .lt. omega1 ) then
        omega2 = omega2 + 2.0D+00 * pi
        go to 10
      end if
c
c  Compute the vector V = XY - C:
c
      v(1) = xy(1) - c(1)
      v(2) = xy(2) - c(2)
c
c  a: Point must be inside the circle, so ||V|| .le. R.
c
      v_r = sqrt ( v(1) ** 2 + v(2) ** 2 )

      if ( r .lt. v_r ) then
        value = 0
        return
      end if
c
c  b: Angle made by the vector V must be between OMEGA1 and OMEGA2.
c
      v_omega = atan2 ( v(2), v(1) )

20    continue

      if ( omega1 .le. v_omega + 2.0D+00 * pi ) then
        v_omega = v_omega - 2.0D+00 * pi
        go to 20
      end if

30    continue

      if ( v_omega + 2.0D+00 * pi .le. omega1 ) then
        v_omega = v_omega + 2.0D+00 * pi
        go to 30
      end if

      if ( omega2 .lt. v_omega ) then
        value = 0
        return
      end if
c
c  c: Projection of V onto unit centerline must be at least R-H.
c
      omegah = 0.5D+00 * ( omega1 + omega2 )
      v_project = v(1) * cos ( omegah ) + v(2) * sin ( omegah )

      theta = omega2 - omega1
      call circle_segment_height_from_angle ( r, theta, h )

      if ( v_project .lt. r - h ) then
        value = 0
        return
      end if

      value = 1
      
      return
      end
      subroutine circle_segment_height_from_angle ( r, angle, h )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE: height of a circle segment from angle.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c    This function is given the radius R and angle of the segment, and
c    determines the corresponding height.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision ANGLE, the angle of the circle segment.
c    0 .le. ANGLE .le. 2.0 * PI.
c
c    Output, double precision H, the height of the circle segment.
c
      implicit none

      double precision angle
      double precision h
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r

      if ( angle .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE - Fatal error!'
        write ( *, '(a)' ) '  ANGLE .lt. 0.0.'
        stop
      end if

      if ( angle == 0.0D+00 ) then
        h = 0.0D+00
        return
      end if

      if ( angle == 2.0D+00 * pi ) then
        h = 2.0D+00 * r
        return
      end if

      if ( 2.0D+00 * pi .lt. angle ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE - Fatal error!'
        write ( *, '(a)' ) '  2.0 * pi .lt. ANGLE.'
        stop
      end if

      if ( r .le. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE - Fatal error!'
        write ( *, '(a)' ) '  R .le. 0.0.'
        stop
      end if

      if ( angle .le. pi ) then
        h = r * ( 1.0D+00 - cos ( angle   / 2.0D+00 ) )
      else
        h = r * ( 1.0D+00 + cos ( ( 2.0D+00 * pi - angle ) / 2.0D+00 ) )
      end if

      return
      end
      subroutine circle_segment_height_from_area ( r, area, h )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_HEIGHT_FROM_AREA: height of a circle segment from area.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c    This function is given the radius R and area of the segment, and
c    determines the corresponding height.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision AREA, the area of the circle segment.
c    0 .le. AREA .le. 2.0 * PI * R^2.
c
c    Output, double precision H, the height of the circle segment.
c
      implicit none

      double precision a
      double precision a1
      double precision a2
      double precision area
      double precision area_circle
      double precision eps
      double precision h
      double precision h1
      double precision h2
      integer it
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_epsilon

      if ( area .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_HEIGHT_FROM_AREA - Fatal error!'
        write ( *, '(a)' ) '  AREA .lt. 0.0.'
        stop
      end if

      area_circle = 2.0D+00 * pi * r ** 2

      if ( area .eq. 0.0D+00 ) then
        h = 0.0D+00
        return
      end if

      if ( area .eq. area_circle ) then
        h = 2.0D+00 * r
        return
      end if

      if ( area_circle .lt. area ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_HEIGHT_FROM_AREA - Fatal error!'
        write ( *, '(a)' ) '  2.0 * pi * r^2 .lt. AREA.'
        stop
      end if

      if ( r .le. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 
     &    'CIRCLE_SEGMENT_HEIGHT_FROM_AREA - Fatal error!'
        write ( *, '(a)' ) '  R .le. 0.0.'
        stop
      end if

      h1 = 0.0D+00
      call circle_segment_area_from_height ( r, h1, a1 )
      h2 = 2.0D+00 * r
      call circle_segment_area_from_height ( r, h2, a2 )

      it = 0
      eps = r8_epsilon ( )

10    continue
      if ( it .lt. 30 ) then

        h = 0.5D+00 * ( h1 + h2 )
        call circle_segment_area_from_height ( r, h, a )
        it = it + 1

        if ( abs ( a - area ) .lt. sqrt ( eps ) * area ) then
          go to 20
        end if

        if ( a .lt. area ) then
          h1 = h
          a1 = a
        else
          h2 = h
          a2 = a
        end if

        go to 10

      end if

20    continue

      return
      end
      subroutine circle_segment_height_from_chord ( r, c, p1, p2, h )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_HEIGHT_FROM_CHORD: height of a circle segment from chord.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the coordinates of the circle center.
c
c    Input, double precision P1(2), P2(2), the coordinates of the 
c    chord endpoints.
c
c    Output, double precision H, the height of the circle segment.
c
      implicit none

      double precision c(2)
      double precision h
      double precision p1(2)
      double precision p2(2)
      double precision r
      double precision theta

      call circle_segment_angle_from_chord ( r, c, p1, p2, theta )

      call circle_segment_height_from_angle ( r, theta, h )

      return
      end
      subroutine circle_segment_rotation_from_chord ( r, c, p1, p2, 
     &  alpha )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_ROTATION_FROM_CHORD computes the rotation of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 < R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision P1(2), P2(2), the ends of the chord.
c    Warning! If P1 = P2, we can't tell whether the segment is the whole
c    circle or none of it!
c
c    Output, double precision ALPHA, the rotation of the circle segment.
c    0 <= ALPHA < 2 * PI.
c
      implicit none

      double precision alpha
      double precision c(2)
      double precision p1(2)
      double precision p2(2)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_atan
      double precision rho1
      double precision rho2
      double precision theta
      double precision v1(2)
      double precision v2(2)
!
!  Compute the radial vectors V1 and V2.
!
      v1(1) = p1(1) - c(1)
      v1(2) = p1(2) - c(2)
      v2(1) = p2(1) - c(1)
      v2(2) = p2(2) - c(2)
!
!  Use R8_ATAN to guarantee that 0 <= RHO1, RHO2 <= 2 * PI.
!
      rho1 = r8_atan ( v1(2), v1(1) )
      rho2 = r8_atan ( v2(2), v2(1) )
!
!  Force RHO2 to be bigger than RHO1.
!
10    continue

      if ( rho2 <= rho1 ) then
        rho2 = rho2 + 2.0D+00 * pi
        go to 10
      end if
!
!  Compute THETA.
!
      theta = rho2 - rho1
!
!  ALPHA is RHO1, plus half of the angular distance between P1 and P2.
!
      alpha = rho1 + 0.5D+00 * theta

20    continue

      if ( 2.0D+00 * pi <= alpha ) then
        alpha = alpha - 2.0D+00 * pi
        go to 20
      end if

      return
      end
      subroutine circle_segment_sample_from_chord ( r, c, p1, p2, n, 
     &  seed, x, y )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_SAMPLE_FROM_CHORD samples points from a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision C(2), the center of the circle.
c
c    Input, double precision P1(2), P2(2), the endpoints of the chord.
c
c    Input, integer N, the number of sample points.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X(N), Y(N), the sample points.
c
      implicit none

      integer n

      double precision c(2)
      double precision c2(2)
      double precision eta(n)
      double precision h
      integer i
      double precision p1(2)
      double precision p2(2)
      double precision r
      integer seed
      double precision t
      double precision vc(2)
      double precision vr(2)
      double precision x(n)
      double precision xi(n)
      double precision y(n)
c
c  Determine unit vectors VR and VC.
c  VR points to the center of the chord from the radius.
c  VC points along the chord, from P1 to P2.
c
      vr(1) = 0.5D+00 * ( p1(1) + p2(1) ) - c(1)
      vr(2) = 0.5D+00 * ( p1(2) + p2(2) ) - c(2)
      t = sqrt ( vr(1)**2 + vr(2)**2 )
      vr(1) = vr(1) / t
      vr(2) = vr(2) / t

      vc(1) = p2(1) - p1(1)
      vc(2) = p2(2) - p1(2)
      t = sqrt ( vc(1)**2 + vc(2)**2 )
      vc(1) = vc(1) / t
      vc(2) = vc(2) / t
c
c  Get the height of the circle segment.
c
      c2(1) = 0.0D+00
      c2(2) = 0.0D+00
      call circle_segment_height_from_chord ( r, c2, p1, p2, h )
c
c  Sample (xi,eta) in the reference coordinates, where the chord
c  is horizontal.
c
      call circle_segment_sample_from_height ( r, h, n, seed, xi, eta )
c
c  XI is the left/right coordinate along VC.
c  ETA is the distance along VR.
c
      do i = 1, n
        x(i) = c(1) + eta(i) * vr(1) + xi(i) * vc(1)
        y(i) = c(2) + eta(i) * vr(2) + xi(i) * vc(2)
      end do

      return
      end
      subroutine circle_segment_sample_from_height ( r, h, n, seed, 
     &  x, y )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_SAMPLE_FROM_HEIGHT samples points from a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision H, the height of the circle segment.
c    0 .le. H .le. 2 * R.
c
c    Input, integer N, the number of sample points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(N), Y(N), the sample points.
c
      implicit none

      integer n

      double precision area
      double precision area2(n)
      double precision h
      double precision h2(n)
      integer i
      double precision r
      integer seed
      double precision u(n)
      double precision wh(n)
      double precision x(n)
      double precision y(n)

      call circle_segment_area_from_height ( r, h, area )
c
c  Pick CDF's randomly.
c
      call r8vec_uniform_01 ( n, seed, u )
c
c  Choose points randomly by choosing ordered areas randomly.
c
      do i = 1, n
        area2(i) = u(i) * area
      end do
c
c  Each area corresponds to a height H2.  Find it.
c
      do i = 1, n
        call circle_segment_height_from_area ( r, area2(i), h2(i) )
      end do
c
c  Determine the half-width WH of the segment for each H2.
c
      do i = 1, n
        wh(i) = sqrt ( h2(i) * ( 2.0 * r - h2(i) ) )
      end do
c
c  Choose an X position randomly in [-WH,+WH].
c
      call r8vec_uniform_01 ( n, seed, u )

      do i = 1, n
        x(i) = ( 2.0D+00 * u(i) - 1.0 ) * wh(i)
      end do
c
c  Our circle center is at (0,0).  Our height of H2 is subtracted
c  from the height R at the peak of the circle.  Determine the Y
c  coordinate using this fact.
c
      do i = 1, n
        y(i) = r - h2(i)
      end do

      return
      end
      subroutine circle_segment_width_from_height ( r, h, w )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_WIDTH_FROM_HEIGHT computes the width of a circle segment.
c
c  Discussion:
c
c    Begin with a circle of radius R.  Choose two points P1 and P2 on the
c    circle, and draw the chord P1:P2.  This chord divides the circle
c    into two pieces, each of which is called a circle segment.
c    Consider one of the pieces.  The "angle" of this segment is the angle 
c    P1:C:P2, where C is the center of the circle.  Let Q be the point on 
c    the chord P1:P2 which is closest to C.  The "height" of the segment
c    is the distance from Q to the perimeter of the circle.  The "width"
c    of the circle segment is the length of P1:P2.
c
c    This function is given the radius R and height H of the segment, and
c    determines the corresponding width W.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the circle.
c    0 .lt. R.
c
c    Input, double precision H, the height of the circle segment.
c    0 .le. H .le. 2 * R.
c
c    Output, double precision W, the width of the circle segment.
c
      implicit none

      double precision h
      double precision r
      double precision w

      w = 2.0D+00 * sqrt ( h * ( 2.0D+00 * r - h ) )

      return
      end
      subroutine digit_inc ( c )

c*********************************************************************72
c
cc DIGIT_INC increments a decimal digit.
c
c  Example:
c
c    Input  Output
c    -----  ------
c    '0'    '1'
c    '1'    '2'
c    ...
c    '8'    '9'
c    '9'    '0'
c    'A'    'A'
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character C, a digit to be incremented.
c
      implicit none

      character c
      integer digit

      call ch_to_digit ( c, digit )

      if ( digit .eq. -1 ) then
        return
      end if

      digit = digit + 1

      if ( digit .eq. 10 ) then
        digit = 0
      end if

      call digit_to_ch ( digit, c )

      return
      end
      subroutine digit_to_ch ( digit, c )

c*********************************************************************72
c
cc DIGIT_TO_CH returns the character representation of a decimal digit.
c
c  Example:
c
c    DIGIT   C
c    -----  ---
c      0    '0'
c      1    '1'
c    ...    ...
c      9    '9'
c     17    '*'
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIGIT, the digit value between 0 and 9.
c
c    Output, character C, the corresponding character, or '*' if DIGIT
c    was illegal.
c
      implicit none

      character c
      integer digit

      if ( 0 .le. digit .and. digit .le. 9 ) then

        c = char ( digit + 48 )

      else

        c = '*'

      end if

      return
      end
      subroutine filename_inc ( filename )

c*********************************************************************72
c
cc FILENAME_INC increments a partially numeric filename.
c
c  Discussion:
c
c    It is assumed that the digits in the name, whether scattered or
c    connected, represent a number that is to be increased by 1 on
c    each call.  If this number is all 9's on input, the output number
c    is all 0's.  Non-numeric letters of the name are unaffected.
c
c    If the name is empty, then the routine stops.
c
c    If the name contains no digits, the empty string is returned.
c
c  Example:
c
c      Input            Output
c      -----            ------
c      'a7to11.txt'     'a7to12.txt'
c      'a7to99.txt'     'a8to00.txt'
c      'a9to99.txt'     'a0to00.txt'
c      'cat.txt'        ' '
c      ' '              STOP!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character ( len = * ) FILENAME.
c    On input, a character string to be incremented.
c    On output, the incremented string.
c
      implicit none

      character c
      logical ch_is_digit
      integer change
      integer digit
      character*(*) filename
      integer i
      integer lens

      lens = len_trim ( filename )

      if ( lens .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILENAME_INC - Fatal error!'
        write ( *, '(a)' ) '  The input string is empty.'
        stop
      end if

      change = 0

      do i = lens, 1, -1

        c = filename(i:i)

        if ( ch_is_digit ( c ) ) then

          change = change + 1

          call digit_inc ( c )

          filename(i:i) = c

          if ( c .ne. '0' ) then
            return
          end if

        end if

      end do
c
c  No digits were found.  Return blank.
c
      if ( change .eq. 0 ) then
        filename = ' '
        return
      end if

      return
      end
      subroutine gauss ( n, alpha, beta, x, w )

c*********************************************************************72
c
cc GAUSS computes a Gauss quadrature rule.
c
c  Discussion:
c
c    Given a weight function W encoded by the first N recurrence coefficients 
c    ALPHA and BETA for the associated orthogonal polynomials, the call 
c      call gauss ( n, alpha, beta, x, w ) 
c    generates the nodes and weights of the N-point Gauss quadrature rule 
c    for the weight function W.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    Original MATLAB version by Walter Gautschi.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Walter Gautschi,
c    Orthogonal Polynomials: Computation and Approximation,
c    Oxford, 2004,
c    ISBN: 0-19-850672-4,
c    LC: QA404.5 G3555.
c
c  Parameters:
c
c    Input, integer N, the order of the desired quadrature rule.
c
c    Input, double precision ALPHA(N), BETA(N), the alpha and beta recurrence 
c    coefficients for the othogonal polynomials associated with the
c    weight function.
c
c    Output, double precision X(N), W(N), the nodes and  weights of the desired 
c    quadrature rule.  The nodes are listed in increasing order.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision alpha(n)
      double precision beta(n)
      integer i
      integer it_max
      integer it_num
      integer j
      integer rot_num
      double precision t
      double precision v(n,n)
      double precision w(n)
      double precision x(n)
c
c  Define the tridiagonal Jacobi matrix.
c
      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        a(i,i) = alpha(i)
      end do

      do i = 2, n
        t = sqrt ( beta(i) )
        a(i,i-1) = t
        a(i-1,i) = t
      end do
c
c  Get the eigenvectors and eigenvalues.
c
      it_max = 100

      call jacobi_eigenvalue ( n, a, it_max, v, x, it_num, rot_num )

      do j = 1, n
        w(j) = beta(1) * v(1,j)**2
      end do

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

      return
      end
      subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, 
     &  rot_num )

c*********************************************************************72
c
cc JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
c
c  Discussion:
c
c    This function computes the eigenvalues and eigenvectors of a
c    real symmetric matrix, using Rutishauser's modfications of the classical
c    Jacobi rotation method with threshold pivoting.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix, which must be square, real,
c    and symmetric.
c
c    Input, integer IT_MAX, the maximum number of iterations.
c
c    Output, double precision V(N,N), the matrix of eigenvectors.
c
c    Output, double precision D(N), the eigenvalues, in descending order.
c
c    Output, integer IT_NUM, the total number of iterations.
c
c    Output, integer ROT_NUM, the total number of rotations.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision bw(n)
      double precision c
      double precision d(n)
      double precision g
      double precision gapq
      double precision h
      integer i
      integer it_max
      integer it_num
      integer j
      integer k
      integer l
      integer m
      integer p
      integer q
      integer rot_num
      double precision s
      double precision t
      double precision tau
      double precision term
      double precision termp
      double precision termq
      double precision theta
      double precision thresh
      double precision v(n,n)
      double precision w(n)
      double precision zw(n)

      do j = 1, n
        do i = 1, n
          if ( i .eq. j ) then
            v(i,j) = 1.0D+00
          else
            v(i,j) = 0.0D+00
          end if
        end do
      end do

      do i = 1, n
        d(i) = a(i,i)
      end do

      do i = 1, n
        bw(i) = d(i)
        zw(i) = 0.0D+00
      end do

      it_num = 0
      rot_num = 0

10    continue

      if ( it_num .lt. it_max ) then

        it_num = it_num + 1
c
c  The convergence threshold is based on the size of the elements in
c  the strict upper triangle of the matrix.
c
        thresh = 0.0D+00
        do j = 1, n
          do i = 1, j - 1
            thresh = thresh + a(i,j) ** 2
          end do
        end do

        thresh = sqrt ( thresh ) / dble ( 4 * n )

        if ( thresh .eq. 0.0D+00 ) then
          go to 20
        end if

        do p = 1, n
          do q = p + 1, n

            gapq = 10.0D+00 * abs ( a(p,q) )
            termp = gapq + abs ( d(p) )
            termq = gapq + abs ( d(q) )
c
c  Annihilate tiny offdiagonal elements.
c
            if ( 4 .lt. it_num .and.
     &           termp .eq. abs ( d(p) ) .and.
     &           termq .eq. abs ( d(q) ) ) then

              a(p,q) = 0.0D+00
c
c  Otherwise, apply a rotation.
c
            else if ( thresh .le. abs ( a(p,q) ) ) then

              h = d(q) - d(p)
              term = abs ( h ) + gapq

              if ( term .eq. abs ( h ) ) then
                t = a(p,q) / h
              else
                theta = 0.5D+00 * h / a(p,q)
                t = 1.0D+00 / 
     &            ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
                if ( theta .lt. 0.0D+00 ) then
                  t = - t
                end if
              end if

              c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
              s = t * c
              tau = s / ( 1.0D+00 + c )
              h = t * a(p,q)
c
c  Accumulate corrections to diagonal elements.
c
              zw(p) = zw(p) - h
              zw(q) = zw(q) + h
              d(p) = d(p) - h
              d(q) = d(q) + h

              a(p,q) = 0.0D+00
c
c  Rotate, using information from the upper triangle of A only.
c
              do j = 1, p - 1
                g = a(j,p)
                h = a(j,q)
                a(j,p) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = p + 1, q - 1
                g = a(p,j)
                h = a(j,q)
                a(p,j) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = q + 1, n
                g = a(p,j)
                h = a(q,j)
                a(p,j) = g - s * ( h + g * tau )
                a(q,j) = h + s * ( g - h * tau )
              end do
c
c  Accumulate information in the eigenvector matrix.
c
              do j = 1, n
                g = v(j,p)
                h = v(j,q)
                v(j,p) = g - s * ( h + g * tau )
                v(j,q) = h + s * ( g - h * tau )
              end do

              rot_num = rot_num + 1

            end if

          end do
        end do

        do i = 1, n
          bw(i) = bw(i) + zw(i)
          d(i) = bw(i)
          zw(i) = 0.0D+00
        end do

        go to 10

      end if

20    continue
c
c  Restore upper triangle of input matrix.
c
      do j = 1, n
        do i = 1, j - 1
          a(i,j) = a(j,i)
        end do
      end do
c
c  Ascending sort the eigenvalues and eigenvectors.
c
      do k = 1, n - 1

        m = k

        do l = k + 1, n
          if ( d(l) .lt. d(m) ) then
            m = l
          end if
        end do

        if ( m .ne. k ) then

          t    = d(m)
          d(m) = d(k)
          d(k) = t

          do i = 1, n
            w(i)   = v(i,m)
            v(i,m) = v(i,k)
            v(i,k) = w(i)
          end do

        end if

      end do

      return
      end
      subroutine r_jacobi ( n, a, b, alpha, beta )

c*********************************************************************72
c
cc R_JACOBI computes recurrence coefficients for monic Jacobi polynomials.
c
c  Discussion:
c
c    This function generates the first N recurrence coefficients for monic 
c    Jacobi polynomials with parameters A and B. 
c
c    These polynomials are orthogonal on [-1,1] relative to the weight
c
c      w(x) = (1.0-x)^A * (1.0+x)^B. 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2013
c
c  Author:
c
c    Original MATLAB version by Dirk Laurie, Walter Gautschi.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Walter Gautschi,
c    Orthogonal Polynomials: Computation and Approximation,
c    Oxford, 2004,
c    ISBN: 0-19-850672-4,
c    LC: QA404.5 G3555.
c
c  Parameters:
c
c    Input, integer N, the number of coefficients desired.
c
c    Input, double precision A, B, the parameters for the Jacobi polynomial.
c    -1.0 < A, -1.0 < B.
c
c    Output, double precision ALPHA(N), BETA(N), the first N recurrence 
c    coefficients.
c
      implicit none

      integer n

      double precision a
      double precision alpha(n)
      double precision b
      double precision beta(n)
      integer i
      double precision i_r8
      double precision mu
      double precision nab
      double precision nu
      double precision r8_gamma

      if ( a .le. -1.0D+00 ) then 
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R_JACOBI - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of A.'
        stop
      end if

      if ( b .le. -1.0D+00 ) then 
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R_JACOBI - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of B.'
        stop
      end if

      nu = ( b - a ) / ( a + b + 2.0D+00 )

      mu = 2.0D+00 ** ( a + b + 1.0D+00 ) 
     &  * r8_gamma ( a + 1.0D+00 ) 
     &  * r8_gamma ( b + 1.0D+00 ) 
     &  / r8_gamma ( a + b + 2.0D+00 )

      alpha(1) = nu
      beta(1) = mu 

      if ( n .eq. 1 ) then
        return
      end if

      do i = 2, n
        i_r8 = dble ( i )
        alpha(i) = ( b - a ) * ( b + a ) 
     &    / ( 2.0D+00 * ( i_r8 - 1.0D+00 ) + a + b ) 
     &    / ( 2.0D+00 * i_r8 + a + b )
      end do

      beta(2) = 4.0D+00 * ( a + 1.0D+00 ) * ( b + 1.0D+00 ) 
     &  / ( a + b + 2.0D+00 ) ** 2 
     &  / ( a + b + 3.0D+00 )

      do i = 3, n
        i_r8 = dble ( i )
        nab = 2.0D+00 * ( i_r8 - 1.0D+00 ) + a + b
        beta(i) = 4.0D+00 
     &    * ( i_r8 - 1.0D+00 + a ) * ( i_r8 - 1.0D+00 + b ) 
     &    * ( i_r8 - 1.0D+00 ) * ( i_r8 - 1.0D+00 + a + b ) 
     &    / nab ** 2 
     &    / ( nab + 1.0D+00 ) 
     &    / ( nab - 1.0D+00 )
      end do

      return
      end
      function r8_acos ( c )

c*********************************************************************72
c
cc R8_ACOS computes the arc cosine function, with argument truncation.
c
c  Discussion:
c
c    If you call your system ACOS routine with an input argument that is
c    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
c    surprise (I did).
c
c    This routine simply truncates arguments outside the range.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 October 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision C, the argument.
c
c    Output, double precision R8_ACOS, an angle whose cosine is C.
c
      implicit none

      double precision c
      double precision c2
      double precision r8_acos

      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )

      r8_acos = acos ( c2 )

      return
      end
      function r8_asin ( s )

c*********************************************************************72
c
cc R8_ASIN computes the arc sine function, with argument truncation.
c
c  Discussion:
c
c    If you call your system ASIN routine with an input argument that is
c    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
c    surprise (I did).
c
c    This routine simply truncates arguments outside the range.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision S, the argument.
c
c    Output, double precision R8_ASIN, an angle whose sine is S.
c
      implicit none

      double precision r8_asin
      double precision s
      double precision s2

      s2 = s
      s2 = max ( s2, -1.0D+00 )
      s2 = min ( s2, +1.0D+00 )

      r8_asin = asin ( s2 )

      return
      end
      function r8_atan ( y, x )

c*********************************************************************72
c
cc R8_ATAN computes the inverse tangent of the ratio Y / X.
c
c  Discussion:
c
c    R8_ATAN returns an angle whose tangent is ( Y / X ), a job which
c    the built in functions ATAN and ATAN2 already do.
c
c    However:
c
c    * R8_ATAN always returns a positive angle, between 0 and 2 PI,
c      while ATAN and ATAN2 return angles in the interval [-PI/2,+PI/2]
c      and [-PI,+PI] respectively;
c
c    * R8_ATAN accounts for the signs of X and Y, (as does ATAN2).  The ATAN
c     function by contrast always returns an angle in the first or fourth
c     quadrants.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision Y, X, two quantities which represent the
c    tangent of an angle.  If Y is not zero, then the tangent is (Y/X).
c
c    Output, double precision R8_ATAN, an angle between 0 and 2 * PI, whose
c    tangent is (Y/X), and which lies in the appropriate quadrant so that
c    the signs of its cosine and sine match those of X and Y.
c
      implicit none

      double precision abs_x
      double precision abs_y
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_atan
      double precision theta
      double precision theta_0
      double precision x
      double precision y
c
c  Special cases:
c
      if ( x .eq. 0.0D+00 ) then

        if ( 0.0D+00 .lt. y ) then
          theta = pi / 2.0D+00
        else if ( y .lt. 0.0D+00 ) then
          theta = 3.0D+00 * pi / 2.0D+00
        else if ( y .eq. 0.0D+00 ) then
          theta = 0.0D+00
        end if

      else if ( y .eq. 0.0D+00 ) then

        if ( 0.0D+00 .lt. x ) then
          theta = 0.0D+00
        else if ( x .lt. 0.0D+00 ) then
          theta = pi
        end if
c
c  We assume that ATAN2 is correct when both arguments are positive.
c
      else

        abs_y = dabs ( y )
        abs_x = dabs ( x )

        theta_0 = atan2 ( abs_y, abs_x )

        if ( 0.0D+00 .lt. x .and. 0.0D+00 .lt. y ) then
          theta = theta_0
        else if ( x .lt. 0.0D+00 .and. 0.0D+00 .lt. y ) then
          theta = pi - theta_0
        else if ( x .lt. 0.0D+00 .and. y .lt. 0.0D+00 ) then
          theta = pi + theta_0
        else if ( 0.0D+00 .lt. x .and. y .lt. 0.0D+00 ) then
          theta = 2.0D+00 * pi - theta_0
        end if

      end if

      r8_atan = theta

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      function r8_gamma ( x )

c*********************************************************************72
c
cc R8_GAMMA evaluates Gamma(X) for a real argument.
c
c  Discussion:
c
c    This routine calculates the gamma function for a real argument X.
c    Computation is based on an algorithm outlined in reference 1.
c    The program uses rational functions that approximate the gamma
c    function to at least 20 significant decimal digits.  Coefficients
c    for the approximation over the interval (1,2) are unpublished.
c    Those for the approximation for 12 <= X are from reference 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 January 2008
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody,
c    An Overview of Software Development for Special Functions,
c    in Numerical Analysis Dundee, 1975,
c    edited by GA Watson,
c    Lecture Notes in Mathematics 506,
c    Springer, 1976.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, 
c    Charles Mesztenyi, John Rice, Henry Thatcher, 
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision R8_GAMMA, the value of the function.
c
      implicit none

      double precision c(7)
      double precision eps
      double precision fact
      integer i
      integer n
      double precision p(8)
      logical parity
      double precision pi
      double precision q(8)
      double precision r8_gamma
      double precision res
      double precision sqrtpi
      double precision sum
      double precision x
      double precision xbig
      double precision xden
      double precision xinf
      double precision xminin
      double precision xnum
      double precision y
      double precision y1
      double precision ysq
      double precision z
c
c  Mathematical constants
c
      data sqrtpi / 0.9189385332046727417803297D+00 /
      data pi / 3.1415926535897932384626434D+00 /
c
c  Machine dependent parameters
c
      data xbig / 171.624D+00 /
      data xminin / 2.23D-308 /
      data eps / 2.22D-16 /
      data xinf /1.79D+308 /
c
c  Numerator and denominator coefficients for rational minimax
c  approximation over (1,2).
c
      data p /
     & -1.71618513886549492533811d+00,
     &  2.47656508055759199108314d+01,
     & -3.79804256470945635097577d+02,
     &  6.29331155312818442661052d+02,
     &  8.66966202790413211295064d+02,
     & -3.14512729688483675254357d+04,
     & -3.61444134186911729807069d+04,
     &  6.64561438202405440627855d+04 /

      data q /
     & -3.08402300119738975254353d+01,
     &  3.15350626979604161529144d+02,
     & -1.01515636749021914166146d+03,
     & -3.10777167157231109440444d+03,
     &  2.25381184209801510330112d+04,
     &  4.75584627752788110767815d+03,
     & -1.34659959864969306392456d+05,
     & -1.15132259675553483497211d+05 /
c
c  Coefficients for minimax approximation over (12, INF).
c
      data c /
     & -1.910444077728D-03,
     &  8.4171387781295D-04,
     & -5.952379913043012D-04,
     &  7.93650793500350248D-04,
     & -2.777777777777681622553D-03,
     &  8.333333333333333331554247D-02,
     &  5.7083835261D-03 /

      parity = .false.
      fact = 1.0D+00
      n = 0
      y = x
c
c  Argument is negative.
c
      if ( y .le. 0.0D+00 ) then

        y = - x
        y1 = aint ( y )
        res = y - y1

        if ( res .ne. 0.0D+00 ) then

          if ( y1 .ne. aint ( y1 * 0.5D+00 ) * 2.0D+00 ) then
            parity = .true.
          end if

          fact = - pi / sin ( pi * res )
          y = y + 1.0D+00

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Argument is positive.
c
      if ( y .lt. eps ) then
c
c  Argument < EPS.
c
        if ( xminin .le. y ) then
          res = 1.0D+00 / y
        else
          res = xinf
          r8_gamma = res
          return
        end if

      else if ( y .lt. 12.0D+00 ) then

        y1 = y
c
c  0.0 < argument < 1.0.
c
        if ( y .lt. 1.0D+00 ) then

          z = y
          y = y + 1.0D+00
c
c  1.0 < argument < 12.0.
c  Reduce argument if necessary.
c
        else

          n = int ( y ) - 1
          y = y - dble ( n )
          z = y - 1.0D+00

        end if
c
c  Evaluate approximation for 1.0 < argument < 2.0.
c
        xnum = 0.0D+00
        xden = 1.0D+00
        do i = 1, 8
          xnum = ( xnum + p(i) ) * z
          xden = xden * z + q(i)
        end do

        res = xnum / xden + 1.0D+00
c
c  Adjust result for case  0.0 < argument < 1.0.
c
        if ( y1 .lt. y ) then

          res = res / y1
c
c  Adjust result for case 2.0 < argument < 12.0.
c
        else if ( y .lt. y1 ) then

          do i = 1, n
            res = res * y
            y = y + 1.0D+00
          end do

        end if

      else
c
c  Evaluate for 12.0 <= argument.
c
        if ( y .le. xbig ) then

          ysq = y * y
          sum = c(7)
          do i = 1, 6
            sum = sum / ysq + c(i)
          end do
          sum = sum / y - y + sqrtpi
          sum = sum + ( y - 0.5D+00 ) * log ( y )
          res = exp ( sum )

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Final adjustments and return.
c
      if ( parity ) then
        res = - res
      end if

      if ( fact .ne. 1.0D+00 ) then
        res = fact / res
      end if

      r8_gamma = res

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
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
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
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

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
      subroutine tridisolve ( n, a, b, c, d, x )

c*********************************************************************72
c
cc TRIDISOLVE solves a tridiagonal system of linear equations.
c
c  Discussion:
c
c    We can describe an NxN tridiagonal matrix by vectors A, B, and C, where
c    A and C are of length N-1.  In that case, a linear system can be
c    represented as
c                        b(1) * x(1) + c(1) * x(2)   = d(1),
c      a(j-1) * x(j-1) + b(j) * x(j) + c(j) * x(j+1) = d(j), j = 2:n-1,
c      a(n-1) * x(n-1) + b(n) * x(n)                 = d(n)
c
c    This function produces the solution vector X.
c
c    This function is derived from Cleve Moler's Matlab suite.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the order of the linear system.
c
c    Input, double precision A(N-1), B(N), C(N-1), the matrix entries.
c
c    Input, double precision D(N), the right hand side.
c
c    Output, double precision X(N), the solution.
c
      implicit none

      integer n

      double precision a(n-1)
      double precision b(n)
      double precision bi(n)
      double precision c(n-1)
      double precision d(n)
      integer j
      double precision mu
      double precision x(n)

      do j = 1, n
        x(j) = d(j)
      end do

      do j = 1, n
        bi(j) = 1.0D+00 / b(j)
      end do

      do j = 1, n - 1
        mu = a(j) * bi(j)
        b(j+1) = b(j+1) - mu * c(j)
        x(j+1) = x(j+1) - mu * x(j)
      end do

      x(n) = x(n) * bi(n)
      do j = n - 1, 1, -1
        x(j) = ( x(j) - c(j) * x(j+1) ) * bi(j)
      end do

      return
      end
