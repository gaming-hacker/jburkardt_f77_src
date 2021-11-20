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
c    21 April 2014
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
      double precision r8_atan
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision theta
      double precision theta_0
      double precision x
      double precision y
c
c  Special cases:
c
      if ( x .eq. 0.0D+00 ) then

        if ( 0.0D+00 .lt. y ) then
          theta = r8_pi / 2.0D+00
        else if ( y .lt. 0.0D+00 ) then
          theta = 3.0D+00 * r8_pi / 2.0D+00
        else if ( y .eq. 0.0D+00 ) then
          theta = 0.0D+00
        end if

      else if ( y .eq. 0.0D+00 ) then

        if ( 0.0D+00 .lt. x ) then
          theta = 0.0D+00
        else if ( x .lt. 0.0D+00 ) then
          theta = r8_pi
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
          theta = r8_pi - theta_0
        else if ( x .lt. 0.0D+00 .and. y .lt. 0.0D+00 ) then
          theta = r8_pi + theta_0
        else if ( 0.0D+00 .lt. x .and. y .lt. 0.0D+00 ) then
          theta = 2.0D+00 * r8_pi - theta_0
        end if

      end if

      r8_atan = theta

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
      subroutine r8vec_polarize ( n, a, p, a_normal, a_parallel )

c*********************************************************************72
c
cc R8VEC_POLARIZE decomposes an R8VEC into normal and parallel components.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The (nonzero) vector P defines a direction.
c
c    The vector A can be written as the sum
c
c      A = A_normal + A_parallel
c
c    where A_parallel is a linear multiple of P, and A_normal
c    is perpendicular to P.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the vector to be polarized.
c
c    Input, double precision P(N), the polarizing direction.
c
c    Output, double precision A_NORMAL(N), A_PARALLEL(N), the normal
c    and parallel components of A.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_dot_p
      double precision a_normal(n)
      double precision a_parallel(n)
      integer i
      double precision p(n)
      double precision p_norm
      double precision r8vec_dot_product

      p_norm = 0.0D+00
      do i = 1, n
        p_norm = p_norm + p(i) * p(i)
      end do
      p_norm = sqrt ( p_norm )

      if ( p_norm .eq. 0.0D+00 ) then
        do i = 1, n
          a_normal(i) = a(i)
        end do
        do i = 1, n
          a_parallel(i) = 0.0D+00
        end do
        return
      end if

      a_dot_p = r8vec_dot_product ( n, a, p ) / p_norm

      do i = 1, n
        a_parallel(i) = a_dot_p * p(i) / p_norm
      end do

      do i = 1, n
        a_normal(i) = a(i) - a_parallel(i)
      end do

      return
      end
      subroutine r8vec_transpose_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Example:
c
c    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
c    TITLE = 'My vector:  '
c
c    My vector:
c        1.0    2.1    3.2    4.3    5.4
c        6.5    7.6    8.7    9.8   10.9
c       11.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2010
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
      integer ihi
      integer ilo
      character * ( * )  title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      do ilo = 1, n, 5
        ihi = min ( ilo + 5 - 1, n )
        write ( *, '(5g14.6)' ) a(ilo:ihi)
      end do

      return
      end
      subroutine s_cat ( s1, s2, s3 )

c*********************************************************************72
c
cc S_CAT concatenates two strings to make a third string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S1, the "prefix" string.
c
c    Input, character * ( * ) S2, the "postfix" string.
c
c    Output, character * ( * ) S3, the string made by
c    concatenating S1 and S2, ignoring any trailing blanks.
c
      implicit none

      character * ( * ) s1
      character * ( * ) s2
      character * ( * ) s3

      s3 = trim ( s1 ) // trim ( s2 )

      return
      end
      subroutine sphere01_distance_xyz ( xyz1, xyz2, dist )

c*********************************************************************72
c
cc SPHERE01_DISTANCE_XYZ computes great circle distances on a unit sphere.
c
c  Discussion:
c
c    XYZ coordinates are used.
c
c    We assume the points XYZ1 and XYZ2 lie on the unit sphere.
c
c    This computation is a special form of the Vincenty formula.
c    It should be less sensitive to errors associated with very small
c    or very large angular separations.
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
c  Reference:
c
c    "Great-circle distance",
c    Wikipedia.
c
c  Parameters:
c
c    Input, double precision XYZ1(3), the coordinates of the first point.
c
c    Input, double precision XYZ2(3), the coordinates of the second point.
c
c    Output, double precision DIST, the great circle distance between
c    the points.
c
      implicit none

      double precision bot
      double precision dist
      double precision lat1
      double precision lat2
      double precision lon1
      double precision lon2
      double precision r8_asin
      double precision r8_atan
      double precision top
      double precision xyz1(3)
      double precision xyz2(3)

      lat1 = r8_asin ( xyz1(3) )
      lon1 = r8_atan ( xyz1(2), xyz1(1) )

      lat2 = r8_asin ( xyz2(3) )
      lon2 = r8_atan ( xyz2(2), xyz2(1) )

      top = ( cos ( lat2 ) * sin ( lon1 - lon2 ) )**2
     &    + ( cos ( lat1 ) * sin ( lat2 )
     &    -   sin ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 ) )**2

      top = sqrt ( top )

      bot = sin ( lat1 ) * sin ( lat2 )
     &    + cos ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 )

      dist = atan2 ( top, bot )

      return
      end
      subroutine sphere01_sample ( n, seed, x )

c*********************************************************************72
c
cc SPHERE01_SAMPLE picks random points on the unit sphere in 3D.
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
      subroutine sphere01_triangle_project ( a_xyz, b_xyz, c_xyz, f1, 
     &  f2, f3, node_xyz )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_PROJECT projects from plane to spherical triangle.
c
c  Discussion:
c
c    We assume that points A, B and C lie on the unit sphere, and they
c    thus define a spherical triangle.
c
c    They also, of course, define a planar triangle.
c
c    Let (F1,F2,F3) be the barycentric coordinates of a point in this 
c    planar triangle.
c
c    This function determines the coordinates of the point in the planar
c    triangle identified by the barycentric coordinates, and returns the
c    coordinates of the projection of that point onto the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A_XYZ(3), B_XYZ(3), C_XYZ(3), the coordinates
c    of the points A, B, and C.
c
c    Input, integer F1, F2, F3, the barycentric coordinates
c    of a point in the triangle ABC.  Normally, these coordinates would
c    be real numbers, and would sum to 1.  For convenience, we allow these
c    to be integers which must be divided by F1+F2+F3.
c
c    Output, double precision NODE_XYZ(3), the coordinates of the 
c    point on the unit sphere which is the projection of the point on the plane
c    whose barycentric coordinates with respect to A, B, and C is
c    (F1,F2,F3)/(F1+F2+F3).
c
      implicit none

      double precision a_xyz(3)
      double precision b_xyz(3)
      double precision c_xyz(3)
      integer f1
      integer f2
      integer f3
      integer i
      double precision node_norm
      double precision node_xyz(3)
      double precision r8vec_norm

      do i = 1, 3
        node_xyz(i) = 
     &    ( dble ( f1           ) * a_xyz(i)   
     &    + dble (      f2      ) * b_xyz(i)   
     &    + dble (           f3 ) * c_xyz(i) ) 
     &    / dble ( f1 + f2 + f3 )
      end do

      node_norm = r8vec_norm ( 3, node_xyz(1:3) )

      do i = 1, 3
        node_xyz(i) = node_xyz(i) / node_norm
      end do

      return
      end
      subroutine sphere01_triangle_project2 ( a_xyz, b_xyz, c_xyz, f1, 
     &  f2, f3, node_xyz )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_PROJECT2 projects from plane to spherical triangle.
c
c  Discussion:
c
c    We assume that points A, B and C lie on the unit sphere, and they
c    thus define a spherical triangle.
c
c    They also, of course, define a planar triangle.
c
c    Let (F1,F2,F3) be the barycentric coordinates of a point in this 
c    planar triangle.
c
c    This function determines the coordinates of the point in the planar
c    triangle identified by the barycentric coordinates, and returns the
c    coordinates of the projection of that point onto the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A_XYZ(3), B_XYZ(3), C_XYZ(3), the coordinates
c    of the points A, B, and C.
c
c    Input, integer F1, F2, F3, the barycentric coordinates
c    of a point in the triangle ABC.  Normally, these coordinates would
c    be real numbers, and would sum to 1.  For convenience, we allow these
c    to be integers which must be divided by F1+F2+F3.
c
c    Output, double precision) NODE_XYZ(3), the coordinates of the 
c    point on the unit sphere which is the projection of the point on the 
c    plane whose barycentric coordinates with respect to A, B, and C is
c    (F1,F2,F3)/(F1+F2+F3).
c
      implicit none

      double precision a_xyz(3)
      double precision ab(3)
      double precision ac(3)
      double precision acn(3)
      double precision acp(3)
      double precision angle
      double precision b_xyz(3)
      double precision bn(3)
      double precision bp(3)
      double precision c_xyz(3)
      double precision cn(3)
      double precision cp(3)
      integer f1
      integer f2
      integer f3
      integer i
      double precision node_xyz(3)
      double precision r8vec_norm
      double precision t
      double precision theta_ab
      double precision theta_ac
      double precision theta_bc
c
c  This check avoids 0/0 calculations later.
c
      if ( f2 .eq. 0 .and. f3 .eq. 0 ) then
        do i = 1, 3
          node_xyz(i) = a_xyz(i)
        end do
        return
      else if ( f1 .eq. 0 .and. f3 .eq. 0 ) then
        do i = 1, 3
          node_xyz(i) = b_xyz(i)
        end do
        return
      else if ( f1 .eq. 0 .and. f2 .eq. 0 ) then
        do i = 1, 3
          node_xyz(i) = c_xyz(i)
        end do
        return
      end if
c
c  Determine the angular distances (A,B) and (A,C).
c
      call sphere01_distance_xyz ( a_xyz, b_xyz, theta_ab )

      call sphere01_distance_xyz ( a_xyz, c_xyz, theta_ac )
c
c  Polarize B = BP + BN
c  Normalize BN, 
c  Same for C.
c
      call r8vec_polarize ( 3, b_xyz, a_xyz, bn, bp )

      t = r8vec_norm ( 3, bn )
      do i = 1, 3
        bn(i) = bn(i) / t
      end do

      call r8vec_polarize ( 3, c_xyz, a_xyz, cn, cp )

      t = r8vec_norm ( 3, cn )
      do i = 1, 3
        cn(i) = cn(i) / t
      end do
c
c  Determine AB and AC that use cos ( ( F2 + F3 ) / ( F1 + F2 + F3 ) ) of A
c  and cos ( F1 / ( F1 + F2 + F3 ) ) of B or C.
c
      angle = ( dble ( f2 + f3 ) * theta_ab ) 
     &  / dble ( f1 + f2 + f3 )

      do i = 1, 3
        ab(i) = cos ( angle ) * a_xyz(i) + sin ( angle ) * bn(i)
      end do

      angle = ( dble ( f2 + f3 ) * theta_ac ) 
     &  / dble ( f1 + f2 + f3 )

      do i = 1, 3
        ac(i) = cos ( angle ) * a_xyz(i) + sin ( angle ) * cn(i)
      end do
c
c  Determine the angular distance between AB and AC.
c
      call sphere01_distance_xyz ( ab(1:3), ac(1:3), theta_bc )
c
c  Polarize AC = ACP + ACN, normalize ACN.
c
      call r8vec_polarize ( 3, ac, ab, acn, acp )

      t = r8vec_norm ( 3, acn )
      do i = 1, 3
        acn(i) = acn(i) / t
      end do
c
c  The interval between AB and AC is marked by F2+F3+1 vertices 0 through F2+F3.
c
      angle = ( dble ( f3 ) * theta_bc ) / dble ( f2 + f3 )

      do i = 1, 3
        node_xyz(i) = cos ( angle ) * ab(i) + sin ( angle ) * acn(i)
      end do

      return
      end
      subroutine sphere01_triangle_quad_00 ( n, v1, v2, v3, f, seed, 
     &  result )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_QUAD_00: quadrature over a triangle on the unit sphere.
c
c  Discussion:
c
c    This is a Monte Carlo approach.
c
c    The integral is approximated by averaging the values at N random points,
c    multiplied by the area.
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
c    Input, integer N, the number of sample points.
c
c    Input, double precision V1(3), V2(3), V3(3), the XYZ coordinates of
c    the vertices of the triangle.
c
c    Input, external, double precision F, evaluates the integrand at X.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision RESULT, the approximate integral.
c
      implicit none

      integer n

      double precision area
      double precision f
      external f
      integer j
      double precision quad
      double precision result
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision vc(3,n)

      call sphere01_triangle_vertices_to_area ( v1, v2, v3, area )

      call sphere01_triangle_sample ( n, v1, v2, v3, seed, vc )

      quad = 0.0D+00
      do j = 1, n
        quad = quad + f ( vc(1:3,j) )
      end do

      result = quad * area / dble ( n )

      return
      end
      subroutine sphere01_triangle_quad_01 ( v1, v2, v3, f, result )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_QUAD_01: quadrature over a triangle on the unit sphere.
c
c  Discussion:
c
c    The integral is approximated by the value at the centroid,
c    multiplied by the area.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), V3(3), the XYZ coordinates of
c    the vertices of the triangle.
c
c    Input, external, double precision F, evaluates the integrand at X.
c
c    Output, double precision RESULT, the approximate integral.
c
      implicit none

      double precision area
      double precision f
      external f
      double precision quad
      double precision result
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision vc(3)

      call sphere01_triangle_vertices_to_area ( v1, v2, v3, area )

      call sphere01_triangle_vertices_to_centroid ( v1, v2, v3, vc )

      quad = f ( vc )
      result = quad * area

      return
      end
      subroutine sphere01_triangle_quad_02 ( v1, v2, v3, f, result )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_QUAD_02: quadrature over a triangle on the unit sphere.
c
c  Discussion:
c
c    The integral is approximated by the average of the vertex values,
c    multiplied by the area.
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
c    Input, double precision V1(3), V2(3), V3(3), the XYZ coordinates of
c    the vertices of the triangle.
c
c    Input, external, double precision F, evaluates the integrand at X.
c
c    Output, double precision RESULT, the approximate integral.
c
      implicit none

      double precision area
      double precision f
      external f
      double precision quad
      double precision result
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      call sphere01_triangle_vertices_to_area ( v1, v2, v3, area )

      quad = ( f ( v1 ) + f ( v2 ) + f ( v3 ) ) / 3.0D+00

      result = quad * area

      return
      end
      subroutine sphere01_triangle_quad_03 ( v1, v2, v3, f, result )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_QUAD_03: quadrature over a triangle on the unit sphere.
c
c  Discussion:
c
c    The integral is approximated by the average of the midside values,
c    multiplied by the area.
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
c    Input, double precision V1(3), V2(3), V3(3), the XYZ coordinates of
c    the vertices of the triangle.
c
c    Input, external, double precision F, evaluates the integrand at X.
c
c    Output, double precision RESULT, the approximate integral.
c
      implicit none

      double precision area
      double precision f
      external f
      double precision quad
      double precision result
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision v4(3)
      double precision v5(3)
      double precision v6(3)

      call sphere01_triangle_vertices_to_area ( v1, v2, v3, area )

      call sphere01_triangle_vertices_to_midpoints ( v1, v2, v3, v4, 
     &  v5, v6 )

      quad = ( f ( v4 ) + f ( v5 ) + f ( v6 ) ) / 3.0D+00

      result = quad * area

      return
      end
      subroutine sphere01_triangle_quad_icos1c ( a_xyz, b_xyz, c_xyz, 
     &  factor, fun, node_num, result )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_QUAD_ICOS1C: centroid rule, subdivide then project.
c
c  Discussion:
c
c    This function estimates an integral over a spherical triangle on the
c    unit sphere.
c
c    This function sets up an icosahedral grid, and subdivides each
c    edge of the icosahedron into FACTOR subedges.  These edges define a grid
c    within each triangular icosahedral face.  The centroids of these
c    triangles can be determined.  All of these calculations are done,
c    essentially, on the FLAT faces of the icosahedron.  Only then are
c    the triangle vertices and centroids projected to the sphere.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A_XYZ(3), B_XYZ(3), C_XYZ(3), the vertices
c    of the spherical triangle.
c
c    Input, integer FACTOR, the subdivision factor, which must
c    be at least 1.
c
c    Input, external, double precision FUN, evaluates the integrand at X.
c
c    Output, integer NODE_NUM, the number of evaluation points.
c
c    Output, double precision RESULT, the estimated integral.
c
      implicit none

      double precision a_xyz(3)
      double precision a2_xyz(3)
      double precision area
      double precision area_total
      double precision b_xyz(3)
      double precision b2_xyz(3)
      double precision c_xyz(3)
      double precision c2_xyz(3)
      integer f1
      integer f2
      integer f3
      integer factor
      double precision fun
      external fun
      integer node_num
      double precision node_xyz(3)
      double precision result
      double precision v
c
c  Initialize the integral data.
c
      result = 0.0D+00
      area_total = 0.0D+00
      node_num = 0
c
c  Some subtriangles will have the same direction as the triangle.
c  Generate each in turn, by determining the barycentric coordinates
c  of the centroid (F1,F2,F3), from which we can also work out the barycentric
c  coordinates of the vertices of the subtriangle.
c
      do f3 = 1, 3 * factor - 2, 3
        do f2 = 1, 3 * factor - f3 - 1, 3

          f1 = 3 * factor - f3 - f2

          call sphere01_triangle_project ( a_xyz, b_xyz, c_xyz, f1, 
     &      f2, f3, node_xyz )

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 + 2, f2 - 1, f3 - 1, a2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 - 1, f2 + 2, f3 - 1, b2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 - 1, f2 - 1, f3 + 2, c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          v = fun ( node_xyz )    

          node_num = node_num + 1
          result = result + area * v
          area_total = area_total + area

        end do
      end do
c
c  The other subtriangles have the opposite direction from the triangle.
c  Generate each in turn, by determining the barycentric coordinates
c  of the centroid (F1,F2,F3), from which we can also work out the barycentric
c  coordinates of the vertices of the subtriangle.
c
      do f3 = 2, 3 * factor - 4, 3
        do f2 = 2, 3 * factor - f3 - 2, 3

          f1 = 3 * factor - f3 - f2

          call sphere01_triangle_project ( a_xyz, b_xyz, c_xyz, f1, 
     &      f2, f3, node_xyz )

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 - 2, f2 + 1, f3 + 1, a2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 + 1, f2 - 2, f3 + 1, b2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 + 1, f2 + 1, f3 - 2, c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          v = fun ( node_xyz )  

          node_num = node_num + 1  
          result = result + area * v
          area_total = area_total + area

        end do
      end do

      return
      end
      subroutine sphere01_triangle_quad_icos1m ( a_xyz, b_xyz, c_xyz, 
     &  factor, fun, node_num, result )

c*****************************************************************************80
c
cc SPHERE01_TRIANGLE_QUAD_ICOS1M: midpoint rule, subdivide then project.
c
c  Discussion:
c
c    This function estimates an integral over a spherical triangle on the
c    unit sphere.
c
c    This function sets up an icosahedral grid, and subdivides each
c    edge of the icosahedron into FACTOR subedges.  These edges define a grid
c    within each triangular icosahedral face.  The midpoints of the edges
c    of these triangles can be determined.  All of these calculations are done,
c    essentially, on the FLAT faces of the icosahedron.  Only then are
c    the triangle vertices and midpoints projected to the sphere.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A_XYZ(3), B_XYZ(3), C_XYZ(3), the vertices
c    of the spherical triangle.
c
c    Input, integer FACTOR, the subdivision factor, which must
c    be at least 1.
c
c    Input, external, double precision FUN, evaluates the integrand at X.
c
c    Output, integer NODE_NUM, the number of evaluation points.
c
c    Output, double precision RESULT, the estimated integral.
c
      implicit none

      double precision a_xyz(3)
      double precision a2_xyz(3)
      double precision a3_xyz(3)
      double precision area
      double precision area_total
      double precision b_xyz(3)
      double precision b2_xyz(3)
      double precision b3_xyz(3)
      double precision c_xyz(3)
      double precision c2_xyz(3)
      double precision c3_xyz(3)
      integer f1
      integer f2
      integer f3
      integer factor
      double precision fun
      external fun
      integer node_num
      double precision result
      double precision va
      double precision vb
      double precision vc
c
c  Initialize the integral data.
c
      result = 0.0D+00
      area_total = 0.0D+00
      node_num = 0
c
c  Deal with subtriangles that have same orientation as face.
c
      do f1 = 0, factor - 1
        do f2 = 0, factor - f1 - 1
          f3 = factor - f1 - f2

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 + 1, f2,     f3 - 1, a2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2 + 1, f3 - 1, b2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2,     f3,     c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, 2 * f1 + 1, 2 * f2 + 1, 2 * f3 - 2, 
     &      a3_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, 2 * f1,     2 * f2 + 1, 2 * f3 - 1, 
     &      b3_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, 2 * f1 + 1, 2 * f2,     2 * f3 - 1, 
     &      c3_xyz )

          node_num = node_num + 3
          va = fun ( a3_xyz )
          vb = fun ( b3_xyz )   
          vc = fun ( c3_xyz )   
          result = result + area * ( va + vb + vc ) / 3.0D+00
          area_total = area_total + area

          end do
        end do
c
c  Deal with subtriangles that have opposite orientation as face.
c
      do f3 = 0, factor - 2
        do f2 = 1, factor - f3 - 1
          f1 = factor - f2 - f3

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 - 1, f2,     f3 + 1, a2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2 - 1, f3 + 1, b2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2,     f3,     c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, 2 * f1 - 1, 2 * f2 - 1, 2 * f3 + 2, 
     &      a3_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, 2 * f1,     2 * f2 - 1, 2 * f3 + 1, 
     &      b3_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, 2 * f1 - 1, 2 * f2,     2 * f3 + 1, 
     &      c3_xyz )

          node_num = node_num + 3
          va = fun ( a3_xyz )
          vb = fun ( b3_xyz )   
          vc = fun ( c3_xyz )  
          result = result + area * ( va + vb + vc ) / 3.0D+00
          area_total = area_total + area

        end do
      end do

      return
      end
      subroutine sphere01_triangle_quad_icos1v ( a_xyz, b_xyz, c_xyz, 
     &  factor, fun, node_num, result )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_QUAD_ICOS1V: vertex rule, subdivide then project.
c
c  Discussion:
c
c    This function estimates an integral over a spherical triangle on the
c    unit sphere.
c
c    This function sets up an icosahedral grid, and subdivides each
c    edge of the icosahedron into FACTOR subedges.  These edges define a grid
c    within each triangular icosahedral face.   All of these calculations are 
c    done, essentially, on the FLAT faces of the icosahedron.  Only then are
c    the triangle vertices projected to the sphere.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A_XYZ(3), B_XYZ(3), C_XYZ(3), the vertices
c    of the spherical triangle.
c
c    Input, integer FACTOR, the subdivision factor, which must
c    be at least 1.
c
c    Input, external, double precision FUN, evaluates the integrand at X.
c
c    Output, integer NODE_NUM, the number of evaluation points.
c
c    Output, double precision RESULT, the estimated integral.
c
      implicit none

      double precision a_xyz(3)
      double precision a2_xyz(3)
      double precision area
      double precision area_total
      double precision b_xyz(3)
      double precision b2_xyz(3)
      double precision c_xyz(3)
      double precision c2_xyz(3)
      integer f1
      integer f2
      integer f3
      integer factor
      double precision fun
      external fun
      integer node_num
      double precision result
      double precision va
      double precision vb
      double precision vc
c
c  Initialize the integral data.
c
      result = 0.0D+00
      area_total = 0.0D+00
      node_num = 0
c
c  Deal with subtriangles that have same orientation as face.
c
      do f1 = 0, factor - 1
        do f2 = 0, factor - f1 - 1
          f3 = factor - f1 - f2

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 + 1, f2,     f3 - 1, a2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2 + 1, f3 - 1, b2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2,     f3,     c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          node_num = node_num + 3
          va = fun ( a2_xyz )
          vb = fun ( b2_xyz )   
          vc = fun ( c2_xyz )   
          result = result + area * ( va + vb + vc ) / 3.0D+00
          area_total = area_total + area

          end do
        end do
c
c  Deal with subtriangles that have opposite orientation as face.
c
      do f3 = 0, factor - 2
        do f2 = 1, factor - f3 - 1
          f1 = factor - f2 - f3

          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1 - 1, f2,     f3 + 1, a2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2 - 1, f3 + 1, b2_xyz )
          call sphere01_triangle_project ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2,     f3,     c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          node_num = node_num + 3
          va = fun ( a2_xyz )
          vb = fun ( b2_xyz )   
          vc = fun ( c2_xyz )  
          result = result + area * ( va + vb + vc ) / 3.0D+00
          area_total = area_total + area

        end do
      end do

      return
      end
      subroutine sphere01_triangle_quad_icos2v ( a_xyz, b_xyz, c_xyz, 
     &  factor, fun, node_num, result )

c*****************************************************************************80
c
cc SPHERE01_TRIANGLE_QUAD_ICOS2V: vertex rule, subdivide then project.
c
c  Discussion:
c
c    This function estimates an integral over a spherical triangle on the
c    unit sphere.
c
c    This function sets up an icosahedral grid, and subdivides each
c    edge of the icosahedron into FACTOR subedges.  These edges define a grid
c    within each triangular icosahedral face.   All of these calculations are 
c    done, essentially, on the FLAT faces of the icosahedron.  Only then are
c    the triangle vertices projected to the sphere.  
c
c    This function uses a more sophisticated projection scheme than
c    SPHERE01_TRIANGLE_QUAD_ICOS1V, but this does not seem to improve
c    the results significantly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A_XYZ(3), B_XYZ(3), C_XYZ(3), the vertices
c    of the spherical triangle.
c
c    Input, integer FACTOR, the subdivision factor, which must
c    be at least 1.
c
c    Input, external, double precision FUN, evaluates the integrand at X.
c
c    Output, integer NODE_NUM, the number of evaluation points.
c
c    Output, double precision RESULT, the estimated integral.
c
      implicit none

      double precision a_xyz(3)
      double precision a2_xyz(3)
      double precision area
      double precision area_total
      double precision b_xyz(3)
      double precision b2_xyz(3)
      double precision c_xyz(3)
      double precision c2_xyz(3)
      integer f1
      integer f2
      integer f3
      integer factor
      double precision fun
      external fun
      integer node_num
      double precision result
      double precision va
      double precision vb
      double precision vc
c
c  Initialize the integral data.
c
      result = 0.0D+00
      area_total = 0.0D+00
      node_num = 0
c
c  Deal with subtriangles that have same orientation as face.
c
      do f1 = 0, factor - 1
        do f2 = 0, factor - f1 - 1
          f3 = factor - f1 - f2

          call sphere01_triangle_project2 ( 
     &      a_xyz, b_xyz, c_xyz, f1 + 1, f2,     f3 - 1, a2_xyz )
          call sphere01_triangle_project2 ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2 + 1, f3 - 1, b2_xyz )
          call sphere01_triangle_project2 ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2,     f3,     c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          node_num = node_num + 3
          va = fun ( a2_xyz )
          vb = fun ( b2_xyz )   
          vc = fun ( c2_xyz )   
          result = result + area * ( va + vb + vc ) / 3.0D+00
          area_total = area_total + area

        end do
      end do
c
c  Deal with subtriangles that have opposite orientation as face.
c
      do f3 = 0, factor - 2
        do f2 = 1, factor - f3 - 1
          f1 = factor - f2 - f3

          call sphere01_triangle_project2 ( 
     &      a_xyz, b_xyz, c_xyz, f1 - 1, f2,     f3 + 1, a2_xyz )
          call sphere01_triangle_project2 ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2 - 1, f3 + 1, b2_xyz )
          call sphere01_triangle_project2 ( 
     &      a_xyz, b_xyz, c_xyz, f1,     f2,     f3,     c2_xyz )

          call sphere01_triangle_vertices_to_area ( a2_xyz, b2_xyz, 
     &      c2_xyz, area )

          node_num = node_num + 3
          va = fun ( a2_xyz )
          vb = fun ( b2_xyz )   
          vc = fun ( c2_xyz )  
          result = result + area * ( va + vb + vc ) / 3.0D+00
          area_total = area_total + area

        end do
      end do

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
        if ( 0.0D+00 .lt. t ) then
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
      subroutine sphere01_triangle_vertices_to_centroid ( v1, v2, v3,
     &  vs )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_VERTICES_TO_CENTROID: spherical triangle centroid on unit sphere.
c
c  Discussion:
c
c    A unit sphere centered at 0 in 3D satisfies the equation:
c
c      X*X + Y*Y + Z*Z = 1
c
c    A spherical triangle is specified by three points on the sphere.
c
c    The (true) centroid of a spherical triangle is the point
c
c      VT = (XT,YT,ZT) = Integral ( X, Y, Z ) dArea / Integral 1 dArea
c
c    Note that the true centroid does NOT, in general, lie on the sphere.
c
c    The "flat" centroid VF is the centroid of the planar triangle defined by
c    the vertices of the spherical triangle.
c
c    The "spherical" centroid VS of a spherical triangle is computed by
c    the intersection of the geodesic bisectors of the triangle angles.
c    The spherical centroid lies on the sphere.
c
c    VF, VT and VS lie on a line through the center of the sphere.  We can
c    easily calculate VF by averaging the vertices, and from this determine
c    VS by normalizing.
c
c    Of course, we still will not have actually computed VT, which lies
c    somewhere between VF and VSc
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
c    Output, double precision VS(3), the coordinates of the "spherical
c    centroid" of the spherical triangle.
c
      implicit none

      integer i
      double precision norm
      double precision r8vec_norm
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision vs(3)

      do i = 1, 3
        vs(i) = ( v1(i) + v2(i) + v3(i) ) / 3.0D+00
      end do

      norm = r8vec_norm ( 3, vs )

      do i = 1, 3
        vs(i) = vs(i) / norm
      end do

      return
      end
      subroutine sphere01_triangle_vertices_to_midpoints ( v1, v2, v3,
     &  m1, m2, m3 )

c*********************************************************************72
c
cc SPHERE01_TRIANGLE_VERTICES_TO_MIDPOINTS gets the midsides of a spherical triangle.
c
c  Discussion:
c
c    The points are assumed to lie on the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), V3(3), the vertices of the triangle.
c
c    Output, double precision M1(3), M2(3), M3(3), the coordinates of
c    the midpoints of the sides of the spherical triangle.
c
      implicit none

      integer i
      double precision m1(3)
      double precision m2(3)
      double precision m3(3)
      double precision norm
      double precision r8vec_norm
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      do i = 1, 3
        m1(i) = ( v1(i) + v2(i) ) / 2.0D+00
      end do
      norm = r8vec_norm ( 3, m1 )
      do i = 1, 3
        m1(i) = m1(i) / norm
      end do

      do i = 1, 3
        m2(i) = ( v2(i) + v3(i) ) / 2.0D+00
      end do
      norm = r8vec_norm ( 3, m2 )
      do i = 1, 3
        m2(i) = m2(i) / norm
      end do

      do i = 1, 3
        m3(i) = ( v3(i) + v1(i) ) / 2.0D+00
      end do
      norm = r8vec_norm ( 3, m3 )
      do i = 1, 3
        m3(i) = m3(i) / norm
      end do

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
