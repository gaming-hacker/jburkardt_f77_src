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
c    02 December 2000
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
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal errorc'
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
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      function r8vec_norm ( n, a )

c*********************************************************************72
c
cc R8VEC_NORM returns the L2 norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), the vector whose L2 norm is desired.
c
c    Output, double precision R8VEC_NORM, the L2 norm of A.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_norm
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + a(i) * a(i)
      end do
      value = sqrt ( value )

      r8vec_norm = value

      return
      end
      subroutine r8vec_normalize ( n, a )

c*********************************************************************72
c
cc R8VEC_NORMALIZE normalizes an R8VEC in the Euclidean norm.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The euclidean norm is also sometimes called the l2 or
c    least squares norm.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input/output, double precision A(N), the vector to be normalized.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision norm

      norm = 0.0D+00
      do i = 1, n
        norm = norm + a(i) * a(i)
      end do
      norm = sqrt ( norm )

      if ( norm .ne. 0.0D+00 ) then
        do i = 1, n
          a(i) = a(i) / norm
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
      subroutine sphere01_sample ( n, seed, x )

c*********************************************************************72
c
cc SPHERE01_SAMPLE picks random points on the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of samples.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X(3,N), the sample points.
c
      implicit none

      integer n

      integer j
      double precision phi
      double precision r8_acos
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision theta
      double precision vdot
      double precision x(3,n)

      do j = 1, n
c
c  Pick a uniformly random VDOT, which must be between -1 and 1.
c  This represents the dot product of the random vector with the Z unit vector.
c
c  Note: this works because the surface area of the sphere between
c  Z and Z + dZ is independent of Z.  So choosing Z uniformly chooses
c  a patch of area uniformly.
c
        vdot = r8_uniform_01 ( seed )
        vdot = 2.0D+00 * vdot - 1.0D+00

        phi = r8_acos ( vdot )
c
c  Pick a uniformly random rotation between 0 and 2 Pi around the
c  axis of the Z vector.
c
        theta = r8_uniform_01 ( seed )
        theta = 2.0D+00 * r8_pi * theta

        x(1,j) = cos ( theta ) * sin ( phi )
        x(2,j) = sin ( theta ) * sin ( phi )
        x(3,j) = cos ( phi )

      end do

      return
      end
      subroutine sphere01_triangle_angles_to_area ( a, b, c, area )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_ANGLES_TO_AREA: area of a spherical triangle on unit sphere.
c
c  Discussion:
c
c    A unit sphere centered at 0 in 3D satisfies the equation:
c
c      X*X + Y*Y + Z*Z = 1
c
c    A spherical triangle is specified by three points on the surface
c    of the sphere.
c
c    The area formula is known as Girard's formula.
c
c    The area of a spherical triangle on the unit sphere is:
c
c      AREA = ( A + B + C - PI )
c
c    where A, B and C are the (surface) angles of the triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the angles of the triangle.
c
c    Output, double precision AREA, the area of the spherical triangle.
c
      implicit none

      double precision a
      double precision area
      double precision b
      double precision c
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
c
c  Apply Girard's formula.
c
      area = a + b + c - r8_pi

      return
      end
      subroutine sphere01_triangle_sides_to_angles ( as, bs, cs,
     &  a, b, c )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_SIDES_TO_ANGLES; spherical triangle angles on the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision AS, BS, CS, the (geodesic) length of the
c    sides of the triangle.
c
c    Output, double precision A, B, C, the spherical angles of the triangle.
c    Angle A is opposite the side of length AS, and so on.
c
      implicit none

      double precision a
      double precision as
      double precision asu
      double precision b
      double precision bs
      double precision bsu
      double precision c
      double precision cs
      double precision csu
      double precision ssu
      double precision tan_a2
      double precision tan_b2
      double precision tan_c2

      asu = as
      bsu = bs
      csu = cs
      ssu = ( asu + bsu + csu ) / 2.0D+00

      tan_a2 = sqrt ( ( sin ( ssu - bsu ) * sin ( ssu - csu ) ) /
     &                ( sin ( ssu ) * sin ( ssu - asu )     ) )

      a = 2.0D+00 * atan ( tan_a2 )

      tan_b2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - csu ) ) /
     &                ( sin ( ssu ) * sin ( ssu - bsu )     ) )

      b = 2.0D+00 * atan ( tan_b2 )

      tan_c2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - bsu ) ) /
     &                ( sin ( ssu ) * sin ( ssu - csu )     ) )

      c = 2.0D+00 * atan ( tan_c2 )

      return
      end
      subroutine sphere01_triangle_vertices_to_area ( v1, v2, v3, area )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_VERTICES_TO_AREA: area of a spherical triangle on the unit sphere.
c
c  Discussion:
c
c    A unit sphere centered at 0 in 3D satisfies the equation:
c
c      X * X + Y * Y + Z * Z = 1
c
c    A spherical triangle is specified by three points on the surface
c    of the sphere.
c
c    The area formula is known as Girard's formula.
c
c    The area of a spherical triangle on the unit sphere is:
c
c      AREA = ( A + B + C - PI ) * 1
c
c    where A, B and C are the (surface) angles of the triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), V3(3), the vertices of the triangle.
c
c    Output, double precision AREA, the area of the spherical triangle.
c
      implicit none

      double precision a
      double precision area
      double precision as
      double precision b
      double precision bs
      double precision c
      double precision cs
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
c
c  Compute the lengths of the sides of the spherical triangle.
c
      call sphere01_triangle_vertices_to_sides ( v1, v2, v3,
     &  as, bs, cs )
c
c  Get the spherical angles.
c
      call sphere01_triangle_sides_to_angles ( as, bs, cs, a, b, c )
c
c  Get the area.
c
      call sphere01_triangle_angles_to_area ( a, b, c, area )

      return
      end
      subroutine sphere01_triangle_vertices_to_sides ( v1, v2, v3,
     &  as, bs, cs )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_VERTICES_TO_SIDES: spherical triangle sides on unit sphere.
c
c  Discussion:
c
c    We can use the ACOS system call here, but the ARC_COSINE routine
c    will automatically take care of cases where the input argument is
c    (usually slightly) out of bounds.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), V3(3), the vertices of the spherical
c    triangle.
c
c    Output, double precision AS, BS, CS, the (geodesic) length of the sides
c    of the triangle.
c
      implicit none

      double precision as
      double precision bs
      double precision cs
      double precision r8_acos
      double precision r8vec_dot_product
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      as = r8_acos ( r8vec_dot_product ( 3, v2, v3 ) )
      bs = r8_acos ( r8vec_dot_product ( 3, v3, v1 ) )
      cs = r8_acos ( r8vec_dot_product ( 3, v1, v2 ) )

      return
      end
      subroutine sphere01_triangle_sample ( n, v1, v2, v3, seed, x )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_SAMPLE: sample spherical triangle on unit sphere.
c
c  Discussion:
c
c    The unit sphere has center 0 and radius 1.
c
c    A spherical triangle on the surface of the unit sphere contains those 
c    points with radius R = 1, bounded by the vertices V1, V2, V3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    James Arvo,
c    Stratified sampling of spherical triangles,
c    Computer Graphics Proceedings, Annual Conference Series, 
c    ACM SIGGRAPH '95, pages 437-438, 1995.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision V1(3), V2(3), V3(3), the XYZ coordinates of
c    the vertices of the spherical triangle.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X(3,N), the XYZ coordinates of the 
c    sample points.
c
      implicit none

      integer n

      double precision a
      double precision alpha
      double precision area
      double precision area_hat
      double precision b
      double precision beta
      double precision c
      double precision gamma
      integer i
      integer j
      double precision q
      double precision r8_uniform_01
      double precision r8vec_dot_product
      double precision r8vec_norm
      double precision s
      integer seed
      double precision t
      double precision u
      double precision v
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision v31(3)
      double precision v4(3)
      double precision v42(3)
      double precision w
      double precision x(3,n)
      double precision xsi1
      double precision xsi2
      double precision z
c
c  Compute the sides, angles, and area of the spherical triangle;
c
      call sphere01_triangle_vertices_to_sides ( v1, v2, v3, a, b, c )

      call sphere01_triangle_sides_to_angles ( a, b, c, alpha, beta, 
     &  gamma )

      call sphere01_triangle_angles_to_area ( alpha, beta, gamma, area )

      do j = 1, n
c
c  Select the new area.
c
        xsi1 = r8_uniform_01 ( seed )

        area_hat = xsi1 * area
c
c  Compute the sine and cosine of the angle phi.
c
        s = sin ( area_hat - alpha )
        t = cos ( area_hat - alpha )
c
c  Compute the pair that determines beta_hat.
c
        u = t - cos ( alpha )
        v = s + sin ( alpha ) * cos ( c )
c
c  Q is the cosine of the new edge length b_hat.
c
        q = ( ( v * t - u * s ) * cos ( alpha ) - v ) 
     &    / ( ( v * s + u * t ) * sin ( alpha ) )
c
c  We very occasionally get a Q value out of bounds.
c
        q = max ( q, - 1.0D+00 )
        q = min ( q, + 1.0D+00 )
c
c  V31 = normalized ( V3 - ( V3 dot V1 ) * V1 )
c
        w = r8vec_dot_product ( 3, v3, v1 )

        do i = 1, 3
          v31(i) = v3(i) - w * v1(i)
        end do

        t = r8vec_norm ( 3, v31(1:3) )
        if ( 0.0D+00 .lt. t ) then
          do i = 1, 3
            v31(i) = v31(i) / t
          end do
        end if
c
c  V4 is the third vertex of the subtriangle V1, V2, V4.
c
        do i = 1, 3
          v4(i) = q * v1(i) + sqrt ( 1.0D+00 - q * q ) * v31(i)
        end do
c
c  Select cos theta, which will sample along the edge from V2 to V4.
c
        xsi2 = r8_uniform_01 ( seed )

        z = 1.0D+00 - xsi2 
     &    * ( 1.0D+00 - r8vec_dot_product ( 3, v4, v2 ) )
c
c  V42 = normalized ( V4 - ( V4 dot V2 ) * V2 )
c
        w = r8vec_dot_product ( 3, v4, v2 )

        do i = 1, 3
          v42(i) = v4(i) - w * v2(i)
        end do

        t = r8vec_norm ( 3, v42(1:3) )
        if ( 0.0D+00 < t ) then
          do i = 1, 3
            v42(i) = v42(i) / t
          end do
        end if
c
c  Construct the point.
c
        do i = 1, 3
          x(i,j) = z * v2(i) + sqrt ( 1.0D+00 - z * z ) * v42(i)
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
