      subroutine blend_101 ( r, x0, x1, x )

c*********************************************************************72
c
cc BLEND_101 extends scalar endpoint data to a line.
c
c  Diagram:
c
c    0-----r-----1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, the coordinate where an interpolated
c    value is desired.
c
c    Input, double precision X0, X1, the data values at the ends of the line.
c
c    Output, double precision X, the interpolated data value at (R).
c
      implicit none

      double precision r
      double precision x
      double precision x0
      double precision x1

      x = ( 1.0D+00 - r ) * x0 + r * x1

      return
      end
      subroutine blend_102 ( r, s, x00, x01, x10, x11, x )

c*********************************************************************72
c
cc BLEND_102 extends scalar point data into a square.
c
c  Diagram:
c
c    01------------11
c     |      .      |
c     |      .      |
c     |.....rs......|
c     |      .      |
c     |      .      |
c    00------------10
c
c  Formula:
c
c    Written in terms of R and S, the map has the form:
c
c      X(R,S) =
c               1     * ( + x00 )
c             + r     * ( - x00 + x10 )
c             + s     * ( - x00       + x01 )
c             + r * s * ( + x00 - x10 - x01 + x11 )
c
c    Written in terms of the coefficients, the map has the form:
c
c      X(R,S) = x00 * ( 1 - r - s + r * s )
c             + x01 * (         s - r * s )
c             + x10 * (     r     - r * s )
c             + x11 * (             r * s )
c
c             = x00 * ( 1 - r ) * ( 1 - s )
c             + x01 * ( 1 - r ) *       s
c             + x10 *       r   * ( 1 - s )
c             + x11 *       r           s
c
c    The nonlinear term ( r * s ) has an important role:
c
c      If ( x01 + x10 - x00 - x11 ) is zero, then the input data lies in
c      a plane, and the mapping is affine.  All the interpolated data
c      will lie on the plane defined by the four corner values.  In
c      particular, on any line through the square, data values at
c      intermediate points will lie between the values at the endpoints.
c
c      If ( x01 + x10 - x00 - x11 ) is not zero, then the input data does
c      not lie in a plane, and the interpolation map is nonlinear.  On
c      any line through the square, data values at intermediate points
c      may lie above or below the data values at the endpoints.  The
c      size of the coefficient of r * s will determine how severe this
c      effect is.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, the coordinates where an
c    interpolated value is desired.
c
c    Input, double precision X00, X01, X10, X11, the data values
c    at the corners.
c
c    Output, double precision X, the interpolated data value at (R,S).
c
      implicit none

      double precision r
      double precision s
      double precision x
      double precision x00
      double precision x01
      double precision x10
      double precision x11

      x =             + x00       
     &    + r *     ( - x00 + x10 )
     &    + s *     ( - x00       + x01 )
     &    + r * s * ( + x00 - x10 - x01 + x11 )

      return
      end
      subroutine blend_103 ( r, s, t, x000, x001, x010, x011, x100, 
     &  x101, x110, x111, x )

c*********************************************************************72
c
cc BLEND_103 extends scalar point data into a cube.
c
c  Diagram:
c
c    011--------------111
c      |               |
c      |               |
c      |               |
c      |               |
c      |               |
c    001--------------101
c
c
c      *---------------*
c      |               |
c      |               |
c      |      rst      |
c      |               |
c      |               |
c      *---------------*
c
c
c    010--------------110
c      |               |
c      |               |
c      |               |
c      |               |
c      |               |
c    000--------------100
c
c
c  Formula:
c
c    Written as a polynomial in R, S and T, the interpolation map has the
c    form:
c
c      X(R,S,T) =
c        1         * ( + x000 )
c      + r         * ( - x000 + x100 )
c      +     s     * ( - x000        + x010 )
c      +         t * ( - x000               + x001 )
c      + r * s     * ( + x000 - x100 - x010                       + x110 )
c      + r     * t * ( + x000 - x100        - x001        + x101 )
c      +     s * t * ( + x000        - x010 - x001 + x011 )
c      + r * s * t * ( - x000 + x100 + x010 + x001 - x011 - x101 - x110 + x111 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, T, the coordinates where an
c    interpolated value is desired.
c
c    Input, double precision X000, X001, X010, X011, X100, X101, X110,
c    X111, the data values at the corners.
c
c    Output, double precision X, the interpolated data value at (R,S,T).
c
      implicit none

      double precision r
      double precision s
      double precision t
      double precision x
      double precision x000
      double precision x001
      double precision x010
      double precision x011
      double precision x100
      double precision x101
      double precision x110
      double precision x111
c
c  Interpolate the interior point.
c
      x = 
     &  1.0D+00     * ( + x000 ) 
     &  + r         * ( - x000 + x100 ) 
     &  +     s     * ( - x000        + x010 ) 
     &  +         t * ( - x000               + x001 ) 
     &  + r * s     * ( + x000 - x100 - x010                      
     &                                              + x110 ) 
     &  + r     * t * ( + x000 - x100        - x001        + x101 ) 
     &  +     s * t * ( + x000        - x010 - x001 + x011 ) 
     &  + r * s * t * ( - x000 + x100 + x010 + x001 - x011 - x101 
     &                                              - x110 + x111 )

      return
      end
      subroutine blend_112 ( r, s, x00, x01, x10, x11, xr0, xr1, x0s, 
     &  x1s, x )

c*********************************************************************72
c
cc BLEND_112 extends scalar line data into a square.
c
c  Diagram:
c
c    01-----r1-----11
c     |      .      |
c     |      .      |
c    0s.....rs.....1s
c     |      .      |
c     |      .      |
c    00-----r0-----10
c
c  Formula:
c
c    Written in terms of R and S, the interpolation map has the form:
c
c      X(R,S) =
c               1     * ( - x00       + x0s                   + xr0 )
c             + r     * (   x00       - x0s - x10       + x1s )
c             + s     * (   x00 - x01                         - xr0 + xr1 )
c             + r * s * ( - x00 + x01       + x10 - x11 )
c
c    Written in terms of the data, the map has the form:
c
c      X(R,S) =
c             - ( 1 - r ) * ( 1 - s ) * x00
c             + ( 1 - r )             * x0s
c             - ( 1 - r ) *       s   * x01
c             +             ( 1 - s ) * xr0
c             +                   s   * xr1
c             -       r   * ( 1 - s ) * x10
c             +       r               * x1s
c             -       r   *       s   * x11
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, the coordinates where an interpolated
c    value is desired.
c
c    Input, double precision X00, X01, X10, X11, the data values
c    at the corners.
c
c    Input, double precision XR0, XR1, X0S, X1S, the data values at
c    points along the edges corresponding to (R,0), (R,1), (0,S) and (1,S).
c
c    Output, double precision X, the interpolated data value at (R,S).
c
      implicit none

      double precision r
      double precision s
      double precision x
      double precision x00
      double precision x01
      double precision x10
      double precision x11
      double precision xr0
      double precision xr1
      double precision x0s
      double precision x1s

      x = - ( 1.0D+00 - r ) * ( 1.0D+00 - s ) * x00 
     &    + ( 1.0D+00 - r )                   * x0s 
     &    - ( 1.0D+00 - r ) *             s   * x01 
     &    +                   ( 1.0D+00 - s ) * xr0 
     &    +                               s   * xr1 
     &    -             r   * ( 1.0D+00 - s ) * x10 
     &    +             r                     * x1s 
     &    -             r   *             s   * x11

      return
      end
      subroutine blend_113 ( r, s, t, x000, x001, x010, x011, x100, 
     &  x101, x110,   x111, xr00, xr01, xr10, xr11, x0s0, x0s1, x1s0, 
     &  x1s1, x00t, x01t, x10t,   x11t, x )

c*********************************************************************72
c
cc BLEND_113 extends scalar line data into a cube.
c
c  Diagram:
c
c     011-----r11-----111
c      |               |
c      |               |
c     0s1             1s1
c      |               |
c      |               |
c     001-----r01-----101
c
c
c     01t-------------11t
c      |               |
c      |               |
c      |      rst      |
c      |               |
c      |               |
c     00t-------------10t
c
c
c     010-----r10-----110
c      |               |
c      |               |
c     0s0             1s0
c      |               |
c      |               |
c     000-----r00-----100
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, T, the coordinates where an interpolated
c    value is desired.
c
c    Input, double precision X000, X001, X010, X011, X100, X101, X110,
c    X111, the data values at the corners.
c
c    Input, double precision XR00, XR01, XR10, XR11, X0S0, X0S1, X1S0,
c    X1S1, X00T, X01T, X10T, X11T, the data values at points along the edges.
c
c    Output, double precision X, the interpolated data value at (R,S,T).
c
      implicit none

      double precision r
      double precision s
      double precision t
      double precision x
      double precision x000
      double precision x001
      double precision x010
      double precision x011
      double precision x100
      double precision x101
      double precision x110
      double precision x111
      double precision xr00
      double precision xr01
      double precision xr0t
      double precision xr10
      double precision xr11
      double precision xr1t
      double precision xrs0
      double precision xrs1
      double precision x0s0
      double precision x0s1
      double precision x0st
      double precision x1s0
      double precision x1s1
      double precision x1st
      double precision x00t
      double precision x01t
      double precision x10t
      double precision x11t
c
c  Interpolate the points in the centers of the faces.
c
      call blend_112 ( s, t, x000, x001, x010, x011, x0s0, x0s1, x00t, 
     &  x01t, x0st )
      call blend_112 ( s, t, x100, x101, x110, x111, x1s0, x1s1, x10t, 
     &  x11t, x1st )
      call blend_112 ( r, t, x000, x001, x100, x101, xr00, xr01, x00t, 
     &  x10t, xr0t )
      call blend_112 ( r, t, x010, x011, x110, x111, xr10, xr11, x01t, 
     &  x11t, xr1t )
      call blend_112 ( r, s, x000, x010, x100, x110, xr00, xr10, x0s0, 
     &  x1s0, xrs0 )
      call blend_112 ( r, s, x001, x011, x101, x111, xr01, xr11, x0s1, 
     &  x1s1, xrs1 )
c
c  Interpolate the I-th coordinate component of the interior point.
c
      call blend_123 ( r, s, t, x000, x001, x010, x011, x100, x101, 
     &  x110, x111, xr00, xr01, xr10, xr11, x0s0, x0s1, x1s0, x1s1, 
     &  x00t, x01t, x10t, x11t, x0st, x1st, xr0t, xr1t, xrs0, xrs1, x )

      return
      end
      subroutine blend_123 ( r, s, t, x000, x001, x010, x011, x100, 
     &  x101, x110, x111, xr00, xr01, xr10, xr11, x0s0, x0s1, x1s0, 
     &  x1s1, x00t, x01t, x10t, x11t, x0st, x1st, xr0t, xr1t, xrs0, 
     &  xrs1, x )
c
c*********************************************************************72
c
cc BLEND_123 extends scalar face data into a cube.
c
c  Diagram:
c
c    010-----r10-----110        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    0s0.....rs0.....1s0        0s1.....rs1.....1s1     S
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        001-----r01-----101     +----R
c           BOTTOM                      TOP
c
c    011-----0s1-----001        111-----1s1-----101
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    01t.....0st.....00t        11t.....1st.....10t          T
c      |       .       |          |       .       |          |
c      |       .       |          |       .       |          |
c    010-----0s0-----000        110-----1s0-----100     S----+
c           LEFT                       RIGHT
c
c    001-----r01-----101        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    00t.....r0t.....100        01t.....r1t.....11t     T
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        010-----r10-----110     +----R
c           FRONT                       BACK
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, T, the coordinates where an interpolated
c    value is desired.
c
c    Input, double precision X000, X001, X010, X011, X100, X101, X110,
c    X111, the data values at the corners.
c
c    Input, double precision XR00, XR01, XR10, XR11, X0S0, X0S1, X1S0,
c    X1S1, X00T, X01T, X10T, X11T, the data values at points along the edges.
c
c    Input, double precision X0ST, X1ST, XR0T, XR1T, XRS0, XRS1, the
c    data values at points on the faces.
c
c    Output, double precision X, the interpolated data value at (R,S,T).
c
      implicit none

      double precision r
      double precision s
      double precision t
      double precision x
      double precision x000
      double precision x001
      double precision x010
      double precision x011
      double precision x100
      double precision x101
      double precision x110
      double precision x111
      double precision xr00
      double precision xr01
      double precision xr10
      double precision xr11
      double precision x0s0
      double precision x0s1
      double precision x1s0
      double precision x1s1
      double precision x00t
      double precision x01t
      double precision x10t
      double precision x11t
      double precision x0st
      double precision x1st
      double precision xr0t
      double precision xr1t
      double precision xrs0
      double precision xrs1
c
c  Interpolate the interior point.
c
      x =    ( 1.0D+00 - r ) * ( 1.0D+00 - s ) * ( 1.0D+00 - t ) * x000 
     &     - ( 1.0D+00 - r ) * ( 1.0D+00 - s )                   * x00t 
     &     + ( 1.0D+00 - r ) * ( 1.0D+00 - s ) *             t   * x001 
     &     - ( 1.0D+00 - r )                   * ( 1.0D+00 - t ) * x0s0 
     &     + ( 1.0D+00 - r )                                     * x0st 
     &     - ( 1.0D+00 - r )                   *             t   * x0s1 
     &     + ( 1.0D+00 - r ) *             s   * ( 1.0D+00 - t ) * x010 
     &     - ( 1.0D+00 - r ) *             s                     * x01t 
     &     + ( 1.0D+00 - r ) *             s   *             t   * x011 
     &     -                   ( 1.0D+00 - s ) * ( 1.0D+00 - t ) * xr00 
     &     +                   ( 1.0D+00 - s )                   * xr0t 
     &     -                   ( 1.0D+00 - s ) *             t   * xr01 
     &     +                                     ( 1.0D+00 - t ) * xrs0 
     &     +                                                 t   * xrs1 
     &     -                               s   * ( 1.0D+00 - t ) * xr10 
     &     +                               s                     * xr1t 
     &     -                               s   *             t   * xr11 
     &     +             r   * ( 1.0D+00 - s ) * ( 1.0D+00 - t ) * x100 
     &     -             r   * ( 1.0D+00 - s )                   * x10t 
     &     +             r   * ( 1.0D+00 - s ) *             t   * x101 
     &     -             r                     * ( 1.0D+00 - t ) * x1s0 
     &     +             r                                       * x1st 
     &     -             r                     *             t   * x1s1 
     &     +             r   *             s   * ( 1.0D+00 - t ) * x110 
     &     -             r   *             s                     * x11t 
     &     +             r   *             s   *             t   * x111

      return
      end
      subroutine blend_i_0d1 ( x, m )

c*********************************************************************72
c
cc BLEND_I_0D1 extends indexed scalar data at endpoints along a line.
c
c  Diagram:
c
c    ( X1, ..., ..., ..., ..., ..., XM )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M).
c    On input, X(1) and X(M) contain scalar values which are to be
c    interpolated through the entries X(2) through X(M).  It is assumed
c    that the dependence of the data is linear in the vector index I.
c    On output, X(2) through X(M-1) have been assigned interpolated
c    values.
c
c    Input, integer M, the number of entries in X.
c
      implicit none

      integer m

      integer i
      double precision r
      double precision x(m)

      do i = 2, m - 1

        r = dble ( i - 1 ) / dble ( m - 1 )

        call blend_101 ( r, x(1), x(m), x(i) )

      end do

      return
      end
      subroutine blend_ij_0d1 ( x, m1, m2 )

c*********************************************************************72
c
cc BLEND_IJ_0D1 extends indexed scalar data at corners into a table.
c
c  Diagram:
c
c    ( X11,  ..., ..., ..., ..., ..., X1M2  )
c    ( ...,  ..., ..., ..., ..., ..., ...   )
c    ( ...,  ..., ..., ..., ..., ..., ...   )
c    ( ...,  ..., ..., ..., ..., ..., ...   )
c    ( XM11, ..., ..., ..., ..., ..., XM1M2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M1,M2).
c    On input, X(1,1), X(1,M2), X(M1,1) and X(M1,M2) contain scalar
c    values which are to be interpolated throughout the table, using
c    the table indices I and J as independent variables.
c    On output, all entries in X have been assigned a value.
c
c    Input, integer M1, M2, the number of rows and columns in X.
c
      implicit none

      integer m1
      integer m2

      integer i
      integer j
      double precision r
      double precision s
      double precision x(m1,m2)
c
c  Interpolate values along the edges.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        call blend_101 ( r, x(1,1), x(m1,1), x(i,1) )

        call blend_101 ( r, x(1,m2), x(m1,m2), x(i,m2) )

      end do

      do j = 2, m2 - 1

        s = dble ( j - 1 ) / dble ( m2 - 1 )

        call blend_101 ( s, x(1,1), x(1,m2), x(1,j) )

        call blend_101 ( s, x(m1,1), x(m1,m2), x(m1,j) )

      end do
c
c  Interpolate values in the interior.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          call blend_112 ( r, s, x(1,1), x(1,m2), x(m1,1), x(m1,m2),    
     &     x(i,1), x(i,m2), x(1,j), x(m1,j), x(i,j) )

        end do

      end do

      return
      end
      subroutine blend_ij_1d1 ( x, m1, m2 )

c*********************************************************************72
c
cc BLEND_IJ_1D1 extends indexed scalar data along edges into a table.
c
c  Diagram:
c
c    ( X11,  X12,  X13,  X14,  X15,  X16,  X1M2  )
c    ( X21,  ...,  ...,  ...,  ...,  ...,  X2M2  )
c    ( X31,  ...,  ...,  ...,  ...,  ...,  X3M2  )
c    ( X41,  ...,  ...,  ...,  ...,  ...,  X4M2  )
c    ( XM11, XM12, XM13, XM14, XM15, XM16, XM1M2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M1,M2).
c    On input, data is contained in the "edge entries" X(1,J), X(I,1),
c    X(M1,J) and X(I,M2), for I = 1 to M1, and J = 1 to M2.
c    On output, all entries in X have been assigned a value.
c
c    Input, integer M1, M2, the number of rows and columns in X.
c
      implicit none

      integer m1
      integer m2

      integer i
      integer j
      double precision x(m1,m2)
      double precision r
      double precision s
c
c  Interpolate values in the interior.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          call blend_112 ( r, s, x(1,1), x(1,m2), x(m1,1), x(m1,m2),    
     &     x(i,1), x(i,m2), x(1,j), x(m1,j), x(i,j) )

        end do

      end do

      return
      end
      subroutine blend_ij_w_1d1 ( x, r, s, m1, m2 )

c*********************************************************************72
c
cc BLEND_IJ_W_1D1 extends weighted indexed scalar data along edges into a table.
c
c  Diagram:
c
c    Instead of assuming that the data in the table is equally spaced,
c    the arrays R and S are supplied, which should behave as
c    "coordinates" for the data.
c
c            S(1)  S(2)  S(3)  S(4)  S(5)  S(6)  S(M2)
c
c    R(1)  ( X11,  X12,  X13,  X14,  X15,  X16,  X1M2  )
c    R(2)  ( X21,  ...,  ...,  ...,  ...,  ...,  X2M2  )
c    R(3)  ( X31,  ...,  ...,  ...,  ...,  ...,  X3M2  )
c    R(4)  ( X41,  ...,  ...,  ...,  ...,  ...,  X4M2  )
c    R(M1) ( XM11, XM12, XM13, XM14, XM15, XM16, XM1M2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M1,M2).
c    On input, data is contained in the "edge entries" X(1,J), X(I,1),
c    X(M1,J) and X(I,M2), for I = 1 to M1, and J = 1 to M2.
c    On output, all entries in X have been assigned a value.
c
c    Input, double precision R(M1), S(M2), are "coordinates" for the rows and
c    columns of the array.  The values in R, and the values in S, should
c    be strictly increasing or decreasing.
c
c    Input, integer M1, M2, the number of rows and columns in X.
c
      implicit none

      integer m1
      integer m2

      integer i
      integer j
      double precision x(m1,m2)
      double precision r(m1)
      double precision rr
      double precision s(m2)
      double precision ss
c
c  Interpolate values in the interior.
c
      do i = 2, m1 - 1

        rr = ( r(i) - r(1) ) / ( r(m1) - r(1) )

        do j = 2, m2 - 1

          ss = ( s(j) - s(1) ) / ( s(m2) - s(1) )

          call blend_112 ( rr, ss, x(1,1), x(1,m2), x(m1,1), x(m1,m2),  
     &       x(i,1), x(i,m2), x(1,j), x(m1,j), x(i,j) )

        end do

      end do

      return
      end
      subroutine blend_ijk_0d1 ( x, m1, m2, m3 )

c*********************************************************************72
c
cc BLEND_IJK_0D1 extends indexed scalar corner data into a cubic table.
c
c  Diagram:
c
c    ( X111,   ...,  ...,  ...,  ...,  ...,  X1M21   )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )   First "layer"
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( XM111,  ...,  ...,  ...,  ...,  ...,  XM1M21  )
c
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )   Middle "layers"
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c
c    ( X11M3,  ...,  ...,  ...,  ...,  ...,  X1M2M3  )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )   Last "layer"
c    ( ....,   ...,  ...,  ...,  ...,  ...,  ...     )
c    ( XM11M3, ...,  ...,  ...,  ...,  ...,  XM1M2M3 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M1,M2,M3).
c    On input, X(1,1,1), X(1,M2,1), X(M1,1,1), X(M1,M2,1), X(1,1,M3),
c    X(1,M2,M3), X(M1,1,M3) and X(M1,M2,M3) contain scalar values
c    which are to be interpolated throughout the table, using the table
c    indices I and J as independent variables.
c    On output, all entries in X have been assigned a value.
c
c    Input, integer M1, M2, M3, the number of rows, columns,
c    and layers in X.
c
      implicit none

      integer m1
      integer m2
      integer m3

      integer i
      integer j
      integer k
      double precision r
      double precision s
      double precision t
      double precision x(m1,m2,m3)
c
c  Interpolate values along the "edges", that is, index triplets (i,j,k)
c  with exactly two of I, J, K an "extreme" value.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        call blend_101 ( r, x( 1, 1, 1), x(m1, 1, 1), x( i, 1, 1) )
        call blend_101 ( r, x( 1,m2, 1), x(m1,m2, 1), x( i,m2, 1) )
        call blend_101 ( r, x( 1, 1,m3), x(m1, 1,m3), x( i, 1,m3) )
        call blend_101 ( r, x( 1,m2,m3), x(m1,m2,m3), x( i,m2,m3) )

      end do

      do j = 2, m2 - 1

        s = dble ( j - 1 ) / dble ( m2 - 1 )

        call blend_101 ( s, x( 1, 1, 1), x( 1,m2, 1), x( 1, j, 1) )
        call blend_101 ( s, x(m1, 1, 1), x(m1,m2, 1), x(m1, j, 1) )
        call blend_101 ( s, x( 1, 1,m3), x( 1,m2,m3), x( 1, j,m3) )
        call blend_101 ( s, x(m1, 1,m3), x(m1,m2,m3), x(m1, j,m3) )

      end do

      do k = 2, m3 - 1

        t = dble ( k - 1 ) / dble ( m3 - 1 )

        call blend_101 ( t, x( 1, 1,1), x( 1, 1,m3), x( 1, 1,k) )
        call blend_101 ( t, x(m1, 1,1), x(m1, 1,m3), x(m1, 1,k) )
        call blend_101 ( t, x( 1,m2,1), x( 1,m2,m3), x( 1,m2,k) )
        call blend_101 ( t, x(m1,m2,1), x(m1,m2,m3), x(m1,m2,k) )
      end do
c
c  Interpolate values along the "faces", that is, index triplets (i,j,k)
c  with exactly one of I, J, K is an "extreme" value.
c
      do j = 2, m2 - 1

        s = dble ( j - 1 ) / dble ( m2 - 1 )

        do k = 2, m3 - 1

          t = dble ( k - 1 ) / dble ( m3 - 1 )

          call blend_112 ( s, t, x(1,1,1), x(1,1,m3), x(1,m2,1), 
     &      x(1,m2,m3), x(1,j,1), x(1,j,m3), x(1,1,k), x(1,m2,k), 
     &      x(1,j,k) )

          call blend_112 ( s, t, x(m1,1,1), x(m1,1,m3), x(m1,m2,1), 
     &      x(m1,m2,m3), x(m1,j,1), x(m1,j,m3), x(m1,1,k), x(m1,m2,k), 
     &      x(m1,j,k) )

        end do
      end do

      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do k = 2, m3 - 1

          t = dble ( k - 1 ) / dble ( m3 - 1 )

          call blend_112 ( r, t, x(1,1,1), x(1,1,m3), x(m1,1,1), 
     &      x(m1,1,m3), x(i,1,1), x(i,1,m3), x(1,1,k), x(m1,1,k), 
     &      x(i,1,k) )

          call blend_112 ( r, t, x(1,m2,1), x(1,m2,m3), x(m1,m2,1), 
     &      x(m1,m2,m3), x(i,m2,1), x(i,m2,m3), x(1,m2,k), x(m1,m2,k), 
     &      x(i,m2,k) )

        end do
      end do

      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          call blend_112 ( r, s, x(1,1,1), x(1,m2,1), x(m1,1,1), 
     &      x(m1,m2,1), x(i,1,1), x(i,m2,1), x(1,j,1), x(m1,j,1), 
     &      x(i,j,1) )

          call blend_112 ( r, s, x(1,1,m3), x(1,m2,m3), x(m1,1,m3), 
     &      x(m1,m2,m3), x(i,1,m3), x(i,m2,m3), x(1,j,m3), x(m1,j,m3), 
     &      x(i,j,m3) )

        end do
      end do
c
c  Interpolate values in the interior.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          do k = 2, m3 - 1

            t = dble ( k - 1 ) / dble ( m3 - 1 )

            call blend_123 ( r, s, t, 
     &        x( 1,1,1), x( 1, 1,m3), x( 1,m2,1), x( 1,m2,m3),
     &        x(m1,1,1), x(m1, 1,m3), x(m1,m2,1), x(m1,m2,m3),
     &        x( i,1,1), x( i, 1,m3), x( i,m2,1), x(i,m2,m3),
     &        x( 1,j,1), x( 1, j,m3), x(m1, j,1), x(m1, j,m3),
     &        x( 1,1,k), x( 1,m2, k), x(m1, 1,k), x(m1,m2, k),    
     &        x( 1,j,k), x(m1, j, k), x( i, 1,k), x( i,m2, k),           
     &        x( i,j,1), x( i, j,m3), x( i, j,k) )

          end do

        end do

      end do

      return
      end
      subroutine blend_ijk_1d1 ( x, m1, m2, m3 )

c*********************************************************************72
c
cc BLEND_IJK_1D1 extends indexed scalar edge data into a cubic table.
c
c  Diagram:
c
c    ( X111,   X121,   X131,   X141,   X151,   X1M21   )
c    ( X211,   ...,    ...,    ...,    ...,    X2M21   )
c    ( X311,   ...,    ...,    ...,    ...,    X3M21   )   Layer 1
c    ( X411,   ...,    ...,    ...,    ...,    X4M21   )
c    ( XM111,  XM121,  XM131,  XM141,  XM151,  XM1M21  )
c
c    ( X11K,   ...,    ...,    ...,    ...,    X1M2K   )
c    ( ....,   ...,    ...,    ...,    ...,    ...     )
c    ( ....,   ...,    ...,    ...,    ...,    ...     )   Layer K
c    ( ....,   ...,    ...,    ...,    ...,    ...     )   1 < K < M3
c    ( XM11K,  ...,    ...,    ...,    ...,    XM1M2K  )
c
c    ( X11M3,  X12M3,  X13M3,  X14M3,  X15M3,  X1M2M3  )
c    ( X21M3,  ...,    ...,    ...,    ...,    X2M2M3  )
c    ( X31M3,  ...,    ...,    ...,    ...,    X3M2M3  )   Layer M3
c    ( X41M3   ...,    ...,    ...,    ...,    X4M2M3  )
c    ( XM11M3, XM12M3, XM13M3, XM14M3, XM15M3, XM1M2M3 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M1,M2,M3).
c    On input, there is already scalar data in the entries X(I,J,K)
c    corresponding to "edges" of the table, that is, entries for which
c    at least two of the three indices I, J and K are equal to their
c    minimum or maximum possible values.
c    On output, all entries in X have been assigned a value, using the
c    table indices as independent variables.
c
c    Input, integer M1, M2, M3, the number of rows, columns, and
c    layers in X.
c
      implicit none

      integer m1
      integer m2
      integer m3

      integer i
      integer j
      integer k
      double precision r
      double precision s
      double precision t
      double precision x(m1,m2,m3)
c
c  Interpolate values along the "faces", that is, index triplets (i,j,k)
c  where exactly one of I, J, K is an "extreme" value.
c
      do j = 2, m2 - 1

        s = dble ( j - 1 ) / dble ( m2 - 1 )

        do k = 2, m3 - 1

          t = dble ( k - 1 ) / dble ( m3 - 1 )

          call blend_112 ( s, t, x(1,1,1), x(1,1,m3), x(1,m2,1),        
     &      x(1,m2,m3), x(1,j,1), x(1,j,m3), x(1,1,k), x(1,m2,k), 
     &      x(1,j,k) )

          call blend_112 ( s, t, x(m1,1,1), x(m1,1,m3), x(m1,m2,1),     
     &      x(m1,m2,m3), x(m1,j,1), x(m1,j,m3), x(m1,1,k), x(m1,m2,k), 
     &      x(m1,j,k) )

        end do
      end do

      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do k = 2, m3 - 1

          t = dble ( k - 1 ) / dble ( m3 - 1 )

          call blend_112 ( r, t, x(1,1,1), x(1,1,m3), x(m1,1,1), 
     &      x(m1,1,m3), x(i,1,1), x(i,1,m3), x(1,1,k), x(m1,1,k), 
     &      x(i,1,k) )

          call blend_112 ( r, t, x(1,m2,1), x(1,m2,m3), x(m1,m2,1), 
     &      x(m1,m2,m3), x(i,m2,1), x(i,m2,m3), x(1,m2,k), x(m1,m2,k), 
     &      x(i,m2,k) )

        end do
      end do

      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          call blend_112 ( r, s, x(1,1,1), x(1,m2,1), x(m1,1,1), 
     &      x(m1,m2,1), x(i,1,1), x(i,m2,1), x(1,j,1), x(m1,j,1), 
     &      x(i,j,1) )

          call blend_112 ( r, s, x(1,1,m3), x(1,m2,m3), x(m1,1,m3), 
     &      x(m1,m2,m3), x(i,1,m3), x(i,m2,m3), x(1,j,m3), x(m1,j,m3), 
     &      x(i,j,m3) )

        end do
      end do
c
c  Interpolate values in the interior.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          do k = 2, m3 - 1

            t = dble ( k - 1 ) / dble ( m3 - 1 )

            call blend_123 ( r, s, t,
     &        x( 1,1,1), x( 1, 1,m3), x( 1,m2,1), x( 1,m2,m3),
     &        x(m1,1,1), x(m1, 1,m3), x(m1,m2,1), x(m1,m2,m3),
     &        x( i,1,1), x( i, 1,m3), x( i,m2,1), x(i,m2,m3),
     &        x( 1,j,1), x( 1, j,m3), x(m1, j,1), x(m1, j,m3),
     &        x( 1,1,k), x( 1,m2, k), x(m1, 1,k), x(m1,m2, k),    
     &        x( 1,j,k), x(m1, j, k), x( i, 1,k), x( i,m2, k),           
     &        x( i,j,1), x( i, j,m3), x( i, j,k) )

          end do

        end do

      end do

      return
      end
      subroutine blend_ijk_2d1 ( x, m1, m2, m3 )

c*********************************************************************72
c
cc BLEND_IJK_2D1 extends indexed scalar face data into a cubic table.
c
c  Diagram:
c
c    ( X111    X121    X131    X141    X151    X1M21   )
c    ( X211    X221    X231    X241    X251    X2M21   )
c    ( X311    X321    X331    X341    X351    X3M21   )   Layer 1
c    ( X411    X421    X431    X441    X451    X4M21   )
c    ( XM111   XM121   XM131   XM141   XM151   XM1M21  )
c
c    ( X11K    X12K    X13K    X14K    X15K    X1M2K   )
c    ( X21K    ...     ....    ....    ....    X2M2K   )
c    ( X31K    ...     ....    ....    ....    X3M2K   )   Layer K
c    ( X41K    ...     ....    ....    ....    X4M2K   )   1 < K < M3
c    ( XM11K   XM12K   XM13K   XM14K   XM15K   XM1M2K  )
c
c    ( X11M3   X12M3   X13M3   X14M3   X15M3   X1M2M3  )
c    ( X21M3   X22M3   X23M3   X24M3   X25M3   X2M2M3  )
c    ( X31M3   X32M3   X33M3   X34M3   X35M3   X3M2M3  )   Layer M3
c    ( X41M3   X42M3   X43M3   X44M3   X45M3   X4M2M3  )
c    ( XM11M3  XM12M3  XM13M3  XM14M3  XM15M3  XM1M2M3 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input/output, double precision X(M1,M2,M3).
c    On input, there is already scalar data in the entries X(I,J,K)
c    corresponding to "faces" of the table, that is, entries for which
c    at least one of the three indices I, J and K is equal to their
c    minimum or maximum possible values.
c    On output, all entries in X have been assigned a value, using the
c    table indices as independent variables.
c
c    Input, integer M1, M2, M3, the number of rows, columns, and
c    layers in X.
c
      implicit none

      integer m1
      integer m2
      integer m3

      integer i
      integer j
      integer k
      double precision r
      double precision s
      double precision t
      double precision x(m1,m2,m3)
c
c  Interpolate values in the interior.
c
      do i = 2, m1 - 1

        r = dble ( i - 1 ) / dble ( m1 - 1 )

        do j = 2, m2 - 1

          s = dble ( j - 1 ) / dble ( m2 - 1 )

          do k = 2, m3 - 1

            t = dble ( k - 1 ) / dble ( m3 - 1 )

            call blend_123 ( r, s, t,
     &        x( 1,1,1), x( 1, 1,m3), x( 1,m2,1), x( 1,m2,m3),
     &        x(m1,1,1), x(m1, 1,m3), x(m1,m2,1), x(m1,m2,m3),
     &        x( i,1,1), x( i, 1,m3), x( i,m2,1), x(i,m2,m3),
     &        x( 1,j,1), x( 1, j,m3), x(m1, j,1), x(m1, j,m3),
     &        x( 1,1,k), x( 1,m2, k), x(m1, 1,k), x(m1,m2, k),    
     &        x( 1,j,k), x(m1, j, k), x( i, 1,k), x( i,m2, k),           
     &        x( i,j,1), x( i, j,m3), x( i, j,k) )

          end do

        end do

      end do

      return
      end
      subroutine blend_r_0dn ( r, x, n, bound_r )

c*********************************************************************72
c
cc BLEND_R_0DN extends vector data at endpoints into a line.
c
c  Diagram:
c
c    0-----r-----1
c
c  Discussion:
c
c    This is simply linear interpolation.  BLEND_R_0DN is provided
c    mainly as a "base routine" which can be compared to its
c    generalizations, such as BLEND_RS_0DN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, the (R) coordinate of the point to
c    be evaluated.
c
c    Output, double precision X(N), the interpolated value at the point (R).
c
c    Input, integer N, the dimension of the vector space.
c
c    External, BOUND_R, is a subroutine which is given (R) coordinates
c    and an component value I, and returns XI, the value of the I-th
c    component of the N-vector at that point.  BOUND_R will only be
c    called for "corners", that is, for values (R) where R is either
c    0.0 or 1.0.  BOUND_R has the form:
c
c      subroutine bound_r ( r, i, xi )
c
      implicit none

      integer n

      external bound_r
      integer i
      double precision r
      double precision x(n)
      double precision x0
      double precision x1

      do i = 1, n
c
c  Get the I-th coordinate component at the two corners.
c
        call bound_r ( 0.0D+00, i, x0 )
        call bound_r ( 1.0D+00, i, x1 )
c
c  Interpolate the I-th coordinate component of the interior point.
c
        call blend_101 ( r, x0, x1, x(i) )

      end do

      return
      end
      subroutine blend_rs_0dn ( r, s, x, n, bound_rs )

c*********************************************************************72
c
cc BLEND_RS_0DN extends vector data at corners into a square.
c
c  Diagram:
c
c    01-----r1-----11
c     |      .      |
c     |      .      |
c    0s.....rs.....1s
c     |      .      |
c     |      .      |
c    00-----r0-----10
c
c  Discussion:
c
c    BLEND_RS_0DN should be equivalent to the use of a bilinear finite
c    element method.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, the (R,S) coordinates of the point to be
c    evaluated.
c
c    Output, double precision X(N), the interpolated value at the point (R,S).
c
c    Input, integer N, the dimension of the vector space.
c
c    External, BOUND_RS, is a subroutine which is given (R,S)
c    coordinates and an component value I, and returns XI, the value
c    of the I-th component of the N-vector at that point.  BOUND_RS
c    will only be called for "corners", that is, for values (R,S) where
c    R and S are either 0.0 or 1.0.  BOUND_RS has the form:
c      subroutine bound_rs ( r, s, i, xi )
c
      implicit none

      integer n

      external bound_rs
      integer i
      double precision r
      double precision s
      double precision x(n)
      double precision x00
      double precision x01
      double precision x10
      double precision x11
      double precision xr0
      double precision xr1
      double precision x0s
      double precision x1s

      do i = 1, n
c
c  Get the I-th coordinate component at the four corners.
c
        call bound_rs ( 0.0D+00, 0.0D+00, i, x00 )
        call bound_rs ( 0.0D+00, 1.0D+00, i, x01 )
        call bound_rs ( 1.0D+00, 0.0D+00, i, x10 )
        call bound_rs ( 1.0D+00, 1.0D+00, i, x11 )
c
c  Interpolate the I-th coordinate component at the sides.
c
        call blend_101 ( r, x00, x10, xr0 )
        call blend_101 ( r, x01, x11, xr1 )
        call blend_101 ( s, x00, x01, x0s )
        call blend_101 ( s, x10, x11, x1s )
c
c  Interpolate the I-th coordinate component of the interior point.
c
        call blend_112 ( r, s, x00, x01, x10, x11, xr0, xr1, x0s, x1s, 
     &    x(i) )

      end do

      return
      end
      subroutine blend_rs_1dn ( r, s, x, n, bound_rs )

c*********************************************************************72
c
cc BLEND_RS_1DN extends vector data along sides into a square.
c
c  Diagram:
c
c    01-----r1-----11
c     |      .      |
c     |      .      |
c    0s.....rs.....1s
c     |      .      |
c     |      .      |
c    00-----r0-----10
c
c  Discussion:
c
c    BLEND_RS_1DN is NOT equivalent to a bilinear finite element method,
c    since the data is sampled everywhere along the boundary lines,
c    rather than at a finite number of nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, the (R,S) coordinates of the point to be
c    evaluated.
c
c    Output, double precision X(N), the interpolated value at the point (R,S).
c
c    Input, integer N, the dimension of the vector space.
c
c    External, BOUND_RS, is a subroutine which is given (R,S)
c    coordinates and an component value I, and returns XI, the value
c    of the I-th component of the N-vector at that point.  BOUND_RS
c    will only be called for "sides", that is, for values (R,S) where
c    at least one of R and S is either 0.0 or 1.0.  BOUND_RS has the
c    form:
c      subroutine bound_rs ( r, s, i, xi )
c
      implicit none

      integer n

      external bound_rs
      integer i
      double precision r
      double precision s
      double precision x(n)
      double precision x00
      double precision x01
      double precision x10
      double precision x11
      double precision xr0
      double precision xr1
      double precision x0s
      double precision x1s

      do i = 1, n
c
c  Get the I-th coordinate component at the four corners.
c
        call bound_rs ( 0.0D+00, 0.0D+00, i, x00 )
        call bound_rs ( 0.0D+00, 1.0D+00, i, x01 )
        call bound_rs ( 1.0D+00, 0.0D+00, i, x10 )
        call bound_rs ( 1.0D+00, 1.0D+00, i, x11 )
c
c  Get the I-th coordinate component at the sides.
c
        call bound_rs ( r, 0.0D+00, i, xr0 )
        call bound_rs ( r, 1.0D+00, i, xr1 )
        call bound_rs ( 0.0D+00, s, i, x0s )
        call bound_rs ( 1.0D+00, s, i, x1s )
c
c  Interpolate the I-th coordinate component of the interior point.
c
        call blend_112 ( r, s, x00, x01, x10, x11, xr0, xr1, x0s, x1s,
     &   x(i) )

      end do

      return
      end
      subroutine blend_rst_0dn ( r, s, t, x, n, bound_rst )

c*********************************************************************72
c
cc BLEND_RST_0DN extends vector data at corners into a cube.
c
c  Diagram:
c
c    010-----r10-----110        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    0s0.....rs0.....1s0        0s1.....rs1.....1s1     S
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        001-----r01-----101     +----R
c           BOTTOM                      TOP
c
c    011-----0s1-----001        111-----1s1-----101
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    01t.....0st.....00t        11t.....1st.....10t          T
c      |       .       |          |       .       |          |
c      |       .       |          |       .       |          |
c    010-----0s0-----000        110-----1s0-----100     S----+
c           LEFT                       RIGHT
c
c    001-----r01-----101        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    00t.....r0t.....100        01t.....r1t.....11t     T
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        010-----r10-----110     +----R
c           FRONT                       BACK
c
c  Discussion:
c
c    BLEND_RST_0DN is equivalent to a trilinear finite element method.
c    Data along the edges, faces, and interior of the cube is
c    interpolated from the data at the corners.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, T, the (R,S,T) coordinates of the
c    point to be evaluated.
c
c    Output, double precision X(N), the interpolated value at the
c    point (R,S,T).
c
c    Input, integer N, the dimension of the vector space.
c
c    External, BOUND_RST, is a subroutine which is given (R,S,T)
c    coordinates and an component value I, and returns XI, the value
c    of the I-th component of the N-vector at that point.  BOUND_RST
c    will only be called for "corners", that is, for values (R,S,T)
c    where R, S and T are either 0.0 or 1.0.  BOUND_RST has the form:
c      subroutine bound_rst ( r, s, t, i, xi )
c
      implicit none

      integer n

      external bound_rst
      integer i
      double precision r
      double precision s
      double precision t
      double precision x(n)
      double precision x000
      double precision x001
      double precision x010
      double precision x011
      double precision x100
      double precision x101
      double precision x110
      double precision x111
      double precision xr00
      double precision xr01
      double precision xr10
      double precision xr11
      double precision x0s0
      double precision x0s1
      double precision x1s0
      double precision x1s1
      double precision x00t
      double precision x01t
      double precision x10t
      double precision x11t
      double precision x0st
      double precision x1st
      double precision xr0t
      double precision xr1t
      double precision xrs0
      double precision xrs1

      do i = 1, n
c
c  Get the I-th coordinate component at the corners.
c
        call bound_rst ( 0.0D+00, 0.0D+00, 0.0D+00, i, x000 )
        call bound_rst ( 0.0D+00, 0.0D+00, 1.0D+00, i, x001 )
        call bound_rst ( 0.0D+00, 1.0D+00, 0.0D+00, i, x010 )
        call bound_rst ( 0.0D+00, 1.0D+00, 1.0D+00, i, x011 )
        call bound_rst ( 1.0D+00, 0.0D+00, 0.0D+00, i, x100 )
        call bound_rst ( 1.0D+00, 0.0D+00, 1.0D+00, i, x101 )
        call bound_rst ( 1.0D+00, 1.0D+00, 0.0D+00, i, x110 )
        call bound_rst ( 1.0D+00, 1.0D+00, 1.0D+00, i, x111 )
c
c  Interpolate the I-th coordinate component at the edges.
c
        call blend_101 ( r, x000, x100, xr00 )
        call blend_101 ( r, x001, x101, xr01 )
        call blend_101 ( r, x010, x110, xr10 )
        call blend_101 ( r, x011, x111, xr11 )

        call blend_101 ( s, x000, x010, x0s0 )
        call blend_101 ( s, x001, x011, x0s1 )
        call blend_101 ( s, x100, x110, x1s0 )
        call blend_101 ( s, x101, x111, x1s1 )

        call blend_101 ( t, x000, x001, x00t )
        call blend_101 ( t, x010, x011, x01t )
        call blend_101 ( t, x100, x101, x10t )
        call blend_101 ( t, x110, x111, x11t )
c
c  Interpolate the I-th component on the faces.
c
        call blend_112 ( s, t, x000, x001, x010, x011, x0s0, x0s1, x00t,
     &    x01t, x0st )

        call blend_112 ( s, t, x100, x101, x110, x111, x1s0, x1s1, x10t,
     &    x11t, x1st )

        call blend_112 ( r, t, x000, x001, x100, x101, xr00, xr01, x00t,
     &    x10t, xr0t )

        call blend_112 ( r, t, x010, x011, x110, x111, xr10, xr11, x01t,
     &    x11t, xr1t )

        call blend_112 ( r, s, x000, x010, x100, x110, xr00, xr10, x0s0,
     &    x1s0, xrs0 )

        call blend_112 ( r, s, x001, x011, x101, x111, xr01, xr11, x0s1,
     &    x1s1, xrs1 )
c
c  Interpolate the I-th coordinate component of the interior point.
c
        call blend_123 ( r, s, t, x000, x001, x010, x011, x100, x101, 
     &    x110, x111, xr00, xr01, xr10, xr11, x0s0, x0s1, x1s0, x1s1, 
     &    x00t, x01t, x10t, x11t, x0st, x1st, xr0t, xr1t, xrs0, xrs1, 
     &    x(i) )

      end do

      return
      end
      subroutine blend_rst_1dn ( r, s, t, x, n, bound_rst )

c*********************************************************************72
c
cc BLEND_RST_1DN extends vector data on edges into a cube.
c
c  Diagram:
c
c    010-----r10-----110        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    0s0.....rs0.....1s0        0s1.....rs1.....1s1     S
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        001-----r01-----101     +----R
c           BOTTOM                      TOP
c
c    011-----0s1-----001        111-----1s1-----101
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    01t.....0st.....00t        11t.....1st.....10t          T
c      |       .       |          |       .       |          |
c      |       .       |          |       .       |          |
c    010-----0s0-----000        110-----1s0-----100     S----+
c           LEFT                       RIGHT
c
c    001-----r01-----101        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    00t.....r0t.....100        01t.....r1t.....11t     T
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        010-----r10-----110     +----R
c           FRONT                       BACK
c
c  Discussion:
c
c    BLEND_RST_1D is NOT equivalent to a trilinear finite element method,
c    since the data is sampled everywhere along the corners and edges,
c    rather than at a finite number of nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, T, the (R,S,T) coordinates of the
c    point to be evaluated.
c
c    Output, double precision X(N), the interpolated value at the
c    point (R,S,T).
c
c    Input, integer N, the dimension of the vector space.
c
c    External, BOUND_RST, is a subroutine which is given (R,S,T)
c    coordinates and an component value I, and returns XI, the value
c    of the I-th component of the N-vector at that point.  BOUND_RST
c    will only be called for "edges", that is, for values (R,S,T)
c    where at least two of R, S and T are either 0.0 or 1.0.
c    BOUND_RST has the form:
c      subroutine bound_rst ( r, s, t, i, xi )
c
      implicit none

      integer n

      external bound_rst
      integer i
      double precision r
      double precision s
      double precision t
      double precision x(n)
      double precision x000
      double precision x001
      double precision x010
      double precision x011
      double precision x100
      double precision x101
      double precision x110
      double precision x111
      double precision xr00
      double precision xr01
      double precision xr10
      double precision xr11
      double precision x0s0
      double precision x0s1
      double precision x1s0
      double precision x1s1
      double precision x00t
      double precision x01t
      double precision x10t
      double precision x11t
      double precision x0st
      double precision x1st
      double precision xr0t
      double precision xr1t
      double precision xrs0
      double precision xrs1

      do i = 1, n
c
c  Get the I-th coordinate component at the corners.
c
        call bound_rst ( 0.0D+00, 0.0D+00, 0.0D+00, i, x000 )
        call bound_rst ( 0.0D+00, 0.0D+00, 1.0D+00, i, x001 )
        call bound_rst ( 0.0D+00, 1.0D+00, 0.0D+00, i, x010 )
        call bound_rst ( 0.0D+00, 1.0D+00, 1.0D+00, i, x011 )
        call bound_rst ( 1.0D+00, 0.0D+00, 0.0D+00, i, x100 )
        call bound_rst ( 1.0D+00, 0.0D+00, 1.0D+00, i, x101 )
        call bound_rst ( 1.0D+00, 1.0D+00, 0.0D+00, i, x110 )
        call bound_rst ( 1.0D+00, 1.0D+00, 1.0D+00, i, x111 )
c
c  Get the I-th coordinate component at the edges.
c
        call bound_rst ( r, 0.0D+00, 0.0D+00, i, xr00 )
        call bound_rst ( r, 0.0D+00, 1.0D+00, i, xr01 )
        call bound_rst ( r, 1.0D+00, 0.0D+00, i, xr10 )
        call bound_rst ( r, 1.0D+00, 1.0D+00, i, xr11 )

        call bound_rst ( 0.0D+00, s, 0.0D+00, i, x0s0 )
        call bound_rst ( 0.0D+00, s, 1.0D+00, i, x0s1 )
        call bound_rst ( 1.0D+00, s, 0.0D+00, i, x1s0 )
        call bound_rst ( 1.0D+00, s, 1.0D+00, i, x1s1 )

        call bound_rst ( 0.0D+00, 0.0D+00, t, i, x00t )
        call bound_rst ( 0.0D+00, 1.0D+00, t, i, x01t )
        call bound_rst ( 1.0D+00, 0.0D+00, t, i, x10t )
        call bound_rst ( 1.0D+00, 1.0D+00, t, i, x11t )
c
c  Interpolate the I-th component on the faces.
c
        call blend_112 ( s, t, x000, x001, x010, x011, x0s0, x0s1, x00t,
     &    x01t, x0st )

        call blend_112 ( s, t, x100, x101, x110, x111, x1s0, x1s1, x10t,
     &    x11t, x1st )

        call blend_112 ( r, t, x000, x001, x100, x101, xr00, xr01, x00t,
     &    x10t, xr0t )

        call blend_112 ( r, t, x010, x011, x110, x111, xr10, xr11, x01t,
     &    x11t, xr1t )

        call blend_112 ( r, s, x000, x010, x100, x110, xr00, xr10, x0s0,
     &    x1s0, xrs0 )

        call blend_112 ( r, s, x001, x011, x101, x111, xr01, xr11, x0s1,
     &    x1s1, xrs1 )
c
c  Interpolate the I-th coordinate component of the interior point.
c
        call blend_123 ( r, s, t, x000, x001, x010, x011, x100, x101, 
     &    x110, x111, xr00, xr01, xr10, xr11, x0s0, x0s1, x1s0, x1s1, 
     &    x00t, x01t, x10t, x11t, x0st, x1st, xr0t, xr1t, xrs0, xrs1, 
     &    x(i) )

      end do

      return
      end
      subroutine blend_rst_2dn ( r, s, t, x, n, bound_rst )

c*********************************************************************72
c
cc BLEND_RST_2DN extends vector data on faces into a cube.
c
c  Diagram:
c
c    010-----r10-----110        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    0s0.....rs0.....1s0        0s1.....rs1.....1s1     S
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        001-----r01-----101     +----R
c           BOTTOM                      TOP
c
c    011-----0s1-----001        111-----1s1-----101
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    01t.....0st.....00t        11t.....1st.....10t          T
c      |       .       |          |       .       |          |
c      |       .       |          |       .       |          |
c    010-----0s0-----000        110-----1s0-----100     S----+
c           LEFT                       RIGHT
c
c    001-----r01-----101        011-----r11-----111
c      |       .       |          |       .       |
c      |       .       |          |       .       |
c    00t.....r0t.....100        01t.....r1t.....11t     T
c      |       .       |          |       .       |     |
c      |       .       |          |       .       |     |
c    000-----r00-----100        010-----r10-----110     +----R
c           FRONT                       BACK
c
c  Discussion:
c
c    BLEND_RST_2DN is NOT equivalent to a trilinear finite element
c    method, since the data is sampled everywhere along the corners,
c    edges, and faces, rather than at a finite number of nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Gordon,
c    Blending-Function Methods of Bivariate and Multivariate Interpolation
c    and Approximation,
c    SIAM Journal on Numerical Analysis,
c    Volume 8, Number 1, March 1971, pages 158-177.
c
c    William Gordon and Charles Hall,
c    Transfinite Element Methods: Blending-Function Interpolation over
c    Arbitrary Curved Element Domains,
c    Numerische Mathematik,
c    Volume 21, Number 1, 1973, pages 109-129.
c
c    William Gordon and Charles Hall,
c    Construction of Curvilinear Coordinate Systems and Application to
c    Mesh Generation,
c    International Journal of Numerical Methods in Engineering,
c    Volume 7, 1973, pages 461-477.
c
c    Joe Thompson, Bharat Soni, Nigel Weatherill,
c    Handbook of Grid Generation,
c    CRC Press, 1999.
c
c  Parameters:
c
c    Input, double precision R, S, T, the (R,S,T) coordinates of the point
c    to be evaluated.
c
c    Output, double precision X(N), the interpolated value at the
c    point (R,S,T).
c
c    Input, integer N, the dimension of the vector space.
c
c    External, BOUND_RST, is a subroutine which is given (R,S,T)
c    coordinates and an component value I, and returns XI, the value
c    of the I-th component of the N-vector at that point.  BOUND_RST
c    will only be called for "faces", that is, for values (R,S,T) where
c    at least one of R, S and T is either 0.0 or 1.0.  BOUND_RST has
c    the form:
c      subroutine bound_rst ( r, s, t, i, xi )
c
      implicit none

      integer n

      external bound_rst
      integer i
      double precision r
      double precision s
      double precision t
      double precision x(n)
      double precision x000
      double precision x001
      double precision x010
      double precision x011
      double precision x100
      double precision x101
      double precision x110
      double precision x111
      double precision xr00
      double precision xr01
      double precision xr10
      double precision xr11
      double precision x0s0
      double precision x0s1
      double precision x1s0
      double precision x1s1
      double precision x00t
      double precision x01t
      double precision x10t
      double precision x11t
      double precision x0st
      double precision x1st
      double precision xr0t
      double precision xr1t
      double precision xrs0
      double precision xrs1

      do i = 1, n
c
c  Get the I-th coordinate component at the corners.
c
        call bound_rst ( 0.0D+00, 0.0D+00, 0.0D+00, i, x000 )
        call bound_rst ( 0.0D+00, 0.0D+00, 1.0D+00, i, x001 )
        call bound_rst ( 0.0D+00, 1.0D+00, 0.0D+00, i, x010 )
        call bound_rst ( 0.0D+00, 1.0D+00, 1.0D+00, i, x011 )
        call bound_rst ( 1.0D+00, 0.0D+00, 0.0D+00, i, x100 )
        call bound_rst ( 1.0D+00, 0.0D+00, 1.0D+00, i, x101 )
        call bound_rst ( 1.0D+00, 1.0D+00, 0.0D+00, i, x110 )
        call bound_rst ( 1.0D+00, 1.0D+00, 1.0D+00, i, x111 )
c
c  Get the I-th coordinate component at the edges.
c
        call bound_rst ( r, 0.0D+00, 0.0D+00, i, xr00 )
        call bound_rst ( r, 0.0D+00, 1.0D+00, i, xr01 )
        call bound_rst ( r, 1.0D+00, 0.0D+00, i, xr10 )
        call bound_rst ( r, 1.0D+00, 1.0D+00, i, xr11 )

        call bound_rst ( 0.0D+00, s, 0.0D+00, i, x0s0 )
        call bound_rst ( 0.0D+00, s, 1.0D+00, i, x0s1 )
        call bound_rst ( 1.0D+00, s, 0.0D+00, i, x1s0 )
        call bound_rst ( 1.0D+00, s, 1.0D+00, i, x1s1 )

        call bound_rst ( 0.0D+00, 0.0D+00, t, i, x00t )
        call bound_rst ( 0.0D+00, 1.0D+00, t, i, x01t )
        call bound_rst ( 1.0D+00, 0.0D+00, t, i, x10t )
        call bound_rst ( 1.0D+00, 1.0D+00, t, i, x11t )
c
c  Get the I-th component on the faces.
c
        call bound_rst ( 0.0D+00, s, t, i, x0st )
        call bound_rst ( 1.0D+00, s, t, i, x1st )
        call bound_rst ( r, 0.0D+00, t, i, xr0t )
        call bound_rst ( r, 1.0D+00, t, i, xr1t )
        call bound_rst ( r, s, 0.0D+00, i, xrs0 )
        call bound_rst ( r, s, 1.0D+00, i, xrs1 )
c
c  Interpolate the I-th coordinate component of the interior point.
c
        call blend_123 ( r, s, t, 
     &    x000, x001, x010, x011, x100, x101, x110, x111,
     &    xr00, xr01, xr10, xr11, x0s0, x0s1, x1s0, x1s1, 
     &    x00t, x01t, x10t, x11t,
     &    x0st, x1st, xr0t, xr1t, xrs0, xrs1, x(i) )

      end do

      return
      end
      subroutine r8block_print ( l, m, n, a, title )

c*********************************************************************72
c
cc R8BLOCK_PRINT prints an R8BLOCK.
c
c  Discussion:
c
c    An R8BLOCK is a 3D array of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 September 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer L, M, N, the dimensions of the block.
c
c    Input, double precision A(L,M,N), the matrix to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer l
      integer m
      integer n

      double precision a(l,m,n)
      integer i
      integer j
      integer jhi
      integer jlo
      integer k
      character * ( * )  title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      do k = 1, n

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  K = ', k
        write ( *, '(a)' ) ' '

        do jlo = 1, m, 5
          jhi = min ( jlo + 4, m )
          write ( *, '(a)' ) ' '
          write ( *, '(10x,5(i8,6x))' ) (j, j = jlo, jhi )
          write ( *, '(a)' ) ' '
          do i = 1, l
            write ( *, '(2x,i8,5g14.6)' ) i, ( a(i,j,k), j = jlo, jhi )
          end do
        end do

      end do

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
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
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
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
c    25 January 2007
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
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
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

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

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

