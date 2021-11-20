      subroutine cartesian_to_hypersphere ( m, n, c, x, r, theta )

c*********************************************************************72
c
cc CARTESIAN_TO_HYPERSPHERE: Cartesian to hypersphere coordinate transform.
c
c  Discussion:
c
c    We allow the trivial case M = 1; in that case alone, the value R
c    must be assumed to have a sign.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 <= M.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision C(M), the center of the hypersphere.
c
c    Input, double precision X(M,N), the Cartesian coordinates of the points.
c
c    Output, double precision R(N), the radius of the points on the 
c    hypersphere.  Except for the trivial case M = 1, R is assumed nonnegative.
c
c    Output, double precision THETA(M-1,N), the coordinate angles of the 
c    points, measured in radians.
c
      implicit none

      integer m
      integer n

      double precision c(m)
      integer i
      integer i1
      integer j
      double precision r(n)
      double precision t
      double precision theta(m-1,n)
      double precision top(n)
      double precision x(m,n)
      double precision x2(m,n)
c
c  Handle special case of M = 1.
c
      if ( m .eq. 1 ) then
        do j = 1, n
          r(j) = x(1,j) - c(1)
        end do
        return
      end if
c
c  Subtract the center.
c
      do j = 1, n
        do i = 1, m
          x2(i,j) = x(i,j) - c(i)
        end do
      end do
c
c  Compute R.
c
      do j = 1, n
        t = 0.0D+00
        do i = 1, m
          t = t + x2(i,j) ** 2
        end do
        r(j) = sqrt ( t )
      end do
c
c  Compute M-2 components of THETA.
c
      do j = 1, n
        do i = 1, m - 1
          theta(i,j) = 0.0D+00
        end do
      end do

      do i = 2, m - 1
        do i1 = 1, i - 1
          theta(i1,1:n) = theta(i1,1:n) + x2(i,1:n) ** 2
        end do
      end do

      do j = 1, n
        do i = 1, m - 2
          theta(i,j) = theta(i,j) + x2(m,j) ** 2
        end do
      end do

      do i = 1, m - 2
        do j = 1, n
          theta(i,j) = atan2 ( sqrt ( theta(i,j) ), x2(i,j) )
        end do
      end do
c
c  Compute last component of THETA.
c
      do j = 1, n
        top(j) = ( sqrt ( x2(m,j) ** 2 + x2(m-1,j) ** 2 ) + x2(m-1,j) )
      end do

      do j = 1, n
        theta(m-1,j) = 2.0 * atan2 ( x2(m,j), top(j) )
      end do

      return
      end
      function hypersphere_01_area ( m )

c*********************************************************************72
c
cc HYPERSPHERE_01_AREA computes the surface area of a unit hypersphere.
c
c  Discussion:
c
c    The unit sphere in ND satisfies:
c
c      sum ( 1 <= I <= M ) X(I) * X(I) = 1
c
c    Results for the first few values are:
c
c     M       Area
c
c     2    2        * PI
c     3    4        * PI
c     4  ( 2 /   1) * PI^2
c     5  ( 8 /   3) * PI^2
c     6  ( 1 /   1) * PI^3
c     7  (16 /  15) * PI^3
c     8  ( 1 /   3) * PI^4
c     9  (32 / 105) * PI^4
c    10  ( 1 /  12) * PI^5
c
c    For the unit sphere, Area(M) = M * Volume(M)
c
c    Sphere_Unit_Area ( M ) = 2 * PI^(M/2) / Gamma ( M / 2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 October 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Output, double precision HYPERSPHERE_01_AREA, the area of the sphere.
c
      implicit none

      double precision area
      double precision hypersphere_01_area
      integer i
      integer m
      integer m2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      if ( mod ( m, 2 ) .eq. 0 ) then
        m2 = m / 2
        area = 2.0D+00 * ( pi ** m2 )
        do i = 1, m2 - 1
          area = area / dble ( i )
        end do
      else
        m2 = ( m - 1 ) / 2
        area = ( pi ** m2 ) * ( 2.0D+00 ** m )
        do i = m2 + 1, 2 * m2
          area = area / dble ( i )
        end do
      end if

      hypersphere_01_area = area

      return
      end
      subroutine hypersphere_01_area_values ( n_data, n, area )

c*********************************************************************72
c
cc HYPERSPHERE_01_AREA_VALUES returns some areas of the unit hypersphere.
c
c  Discussion:
c
c    The formula for the surface area of the unit sphere in N dimensions is:
c
c      Sphere_Unit_Area ( N ) = 2 * PI**(N/2) / Gamma ( N / 2 )
c
c    Some values of the function include:
c
c       N   Area
c
c       2    2        * PI
c       3  ( 4 /    ) * PI
c       4  ( 2 /   1) * PI^2
c       5  ( 8 /   3) * PI^2
c       6  ( 1 /   1) * PI^3
c       7  (16 /  15) * PI^3
c       8  ( 1 /   3) * PI^4
c       9  (32 / 105) * PI^4
c      10  ( 1 /  12) * PI^5
c
c    For the unit sphere, Area(N) = N * Volume(N)
c
c    In Mathematica, the function can be evaluated by:
c
c      2 * Pi^(n/2) / Gamma[n/2]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.
c    On input, if N_DATA is 0, the first test data is returned, and
c    N_DATA is set to the index of the test data.  On each subsequent
c    call, N_DATA is incremented and that test data is returned.  When
c    there is no more test data, N_DATA is set to 0.
c
c    Output, integer N, the spatial dimension.
c
c    Output, double precision AREA, the area of the unit sphere
c    in that dimension.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision area
      double precision area_vec(n_max)
      integer n_data
      integer n
      integer n_vec(n_max)

      save area_vec
      save n_vec

      data area_vec /
     &  0.2000000000000000D+01,
     &  0.6283185307179586D+01,
     &  0.1256637061435917D+02,
     &  0.1973920880217872D+02,
     &  0.2631894506957162D+02,
     &  0.3100627668029982D+02,
     &  0.3307336179231981D+02,
     &  0.3246969701133415D+02,
     &  0.2968658012464836D+02,
     &  0.2550164039877345D+02,
     &  0.2072514267328890D+02,
     &  0.1602315322625507D+02,
     &  0.1183817381218268D+02,
     &  0.8389703410491089D+01,
     &  0.5721649212349567D+01,
     &  0.3765290085742291D+01,
     &  0.2396678817591364D+01,
     &  0.1478625959000308D+01,
     &  0.8858104195716824D+00,
     &  0.5161378278002812D+00 /
      data n_vec /
     &   1,
     &   2,
     &   3,
     &   4,
     &   5,
     &   6,
     &   7,
     &   8,
     &   9,
     &  10,
     &  11,
     &  12,
     &  13,
     &  14,
     &  15,
     &  16,
     &  17,
     &  18,
     &  19,
     &  20 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        area = 0.0D+00
      else
        n = n_vec(n_data)
        area = area_vec(n_data)
      end if

      return
      end
      subroutine hypersphere_01_interior_uniform ( m, n, seed, x )

c*********************************************************************72
c
cc HYPERSPHERE_01_INTERIOR_UNIFORM: uniform points inside unit hypersphere.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    This routine is valid for any spatial dimension.
c
c    We first generate a point ON the sphere, and then distribute it
c    IN the sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2004
c
c  Author:
c
c    John Burkardt & Sahara Moses
c
c  Reference:
c
c    Russell Cheng,
c    Random Variate Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998, pages 168.
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(M,N), the points.
c
      implicit none

      integer m
      integer n

      double precision exponent
      integer i
      integer j
      double precision norm
      double precision r
      double precision r8_uniform_01
      integer seed
      double precision x(m,n)

      exponent = 1.0D+00 / dble ( m )

      do j = 1, n
c
c  Fill a vector with normally distributed values.
c
        call r8vec_normal_01 ( m, seed, x(1:m,j) )
c
c  Compute the length of the vector.
c
        norm = 0.0D+00
        do i = 1, m
          norm = norm + x(i,j)**2
        end do
        norm = sqrt ( norm )
c
c  Normalize the vector.
c
        do i = 1, m
          x(i,j) = x(i,j) / norm
        end do
c
c  Now compute a value to map the point ON the sphere INTO the sphere.
c
        r = r8_uniform_01 ( seed )

        do i = 1, m
          x(i,j) = r ** exponent * x(i,j)
        end do

      end do

      return
      end
      subroutine hypersphere_01_surface_uniform ( m, n, seed, x )

c*********************************************************************72
c
cc HYPERSPHERE_01_SURFACE_UNIFORM: uniform points on unit hypersphere surface.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    This procedure is valid for any spatial dimension DIM_NUM.
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
c  Reference:
c
c    Russell Cheng,
c    Random Variate Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998, pages 168.
c
c    George Marsaglia,
c    Choosing a point from the surface of a sphere,
c    Annals of Mathematical Statistics,
c    Volume 43, Number 2, April 1972, pages 645-646.
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(M,N), the points.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision norm
      integer seed
      double precision x(m,n)
c
c  Fill a matrix with normally distributed values.
c
      call r8mat_normal_01 ( m, n, seed, x )
c
c  Normalize each column.
c
      do j = 1, n
c
c  Compute the length of the vector.
c
        norm = 0.0D+00
        do i = 1, m
          norm = norm + x(i,j)**2
        end do
        norm = sqrt ( norm )
c
c  Normalize the vector.
c
        do i = 1, m
          x(i,j) = x(i,j) / norm
        end do

      end do

      return
      end
      function hypersphere_01_volume ( m )

c*********************************************************************72
c
cc HYPERSPHERE_01_VOLUME computes the volume of a unit hypersphere.
c
c  Discussion:
c
c    The unit sphere in ND satisfies:
c
c      sum ( 1 <= I <= M ) X(I) * X(I) = 1
c
c    Results for the first few values of M are:
c
c     M   Volume
c
c     1    2
c     2    1        * PI
c     3  ( 4 /   3) * PI
c     4  ( 1 /   2) * PI^2
c     5  ( 8 /  15) * PI^2
c     6  ( 1 /   6) * PI^3
c     7  (16 / 105) * PI^3
c     8  ( 1 /  24) * PI^4
c     9  (32 / 945) * PI^4
c    10  ( 1 / 120) * PI^5
c
c    For the unit sphere, Volume(M) = 2 * PI * Volume(M-2)/ M
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 May 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision HYPERSPHERE_01_VOLUME, the volume of the sphere.
c
      implicit none

      double precision hypersphere_01_volume
      integer i
      integer m
      integer m2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision volume

      if ( mod ( m, 2 ) .eq. 0 ) then
        m2 = m / 2
        volume = ( pi ** m2 )
        do i = 1, m2
          volume = volume / dble ( i )
        end do
      else
        m2 = ( m - 1 ) / 2
        volume = ( pi ** m2 ) * ( 2.0D+00 ** m )
        do i = m2 + 1, 2 * m2 + 1
          volume = volume / dble ( i )
        end do
      end if

      hypersphere_01_volume = volume

      return
      end
      subroutine hypersphere_01_volume_values ( n_data, n, volume )

c*********************************************************************72
c
cc HYPERSPHERE_01_VOLUME_VALUES returns some volumes of the unit hypersphere.
c
c  Discussion:
c
c    The formula for the volume of the unit sphere in N dimensions is
c
c      Volume(N) = 2 * PI^(N/2) / ( N * Gamma ( N / 2 ) )
c
c    This function satisfies the relationships:
c
c      Volume(N) = 2 * PI * Volume(N-2) / N
c      Volume(N) = Area(N) / N
c
c    Some values of the function include:
c
c       N  Volume
c
c       1    1
c       2    1        * PI
c       3  ( 4 /   3) * PI
c       4  ( 1 /   2) * PI^2
c       5  ( 8 /  15) * PI^2
c       6  ( 1 /   6) * PI^3
c       7  (16 / 105) * PI^3
c       8  ( 1 /  24) * PI^4
c       9  (32 / 945) * PI^4
c      10  ( 1 / 120) * PI^5
c
c    In Mathematica, the function can be evaluated by:
c
c      2 * Pi^(n/2) / ( n * Gamma[n/2] )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.
c    On input, if N_DATA is 0, the first test data is returned, and
c    N_DATA is set to the index of the test data.  On each subsequent
c    call, N_DATA is incremented and that test data is returned.  When
c    there is no more test data, N_DATA is set to 0.
c
c    Output, integer N, the spatial dimension.
c
c    Output, double precision VOLUME, the volume of the unit
c    sphere in that dimension.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      integer n_data
      integer n
      integer n_vec(n_max)
      double precision volume
      double precision volume_vec(n_max)

      save n_vec
      save volume_vec

      data n_vec /
     &   1,  2,
     &   3,  4,
     &   5,  6,
     &   7,  8,
     &   9, 10,
     &  11, 12,
     &  13, 14,
     &  15, 16,
     &  17, 18,
     &  19, 20 /
      data volume_vec /
     &  0.2000000000000000D+01,
     &  0.3141592653589793D+01,
     &  0.4188790204786391D+01,
     &  0.4934802200544679D+01,
     &  0.5263789013914325D+01,
     &  0.5167712780049970D+01,
     &  0.4724765970331401D+01,
     &  0.4058712126416768D+01,
     &  0.3298508902738707D+01,
     &  0.2550164039877345D+01,
     &  0.1884103879389900D+01,
     &  0.1335262768854589D+01,
     &  0.9106287547832831D+00,
     &  0.5992645293207921D+00,
     &  0.3814432808233045D+00,
     &  0.2353306303588932D+00,
     &  0.1409811069171390D+00,
     &  0.8214588661112823D-01,
     &  0.4662160103008855D-01,
     &  0.2580689139001406D-01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        volume = 0.0D+00
      else
        n = n_vec(n_data)
        volume = volume_vec(n_data)
      end if

      return
      end
      function hypersphere_area ( m, r )

c*********************************************************************72
c
cc HYPERSPHERE_AREA computes the surface area of an implicit sphere in ND.
c
c  Discussion:
c
c    An implicit sphere in ND satisfies the equation:
c
c      sum ( ( P(1:M) - C(1:M) )^2 ) = R^2
c
c    M   Area
c
c    2      2       * PI   * R
c    3      4       * PI   * R^2
c    4      2       * PI^2 * R^3
c    5      (8/3)   * PI^2 * R^4
c    6                PI^3 * R^5
c    7      (16/15) * PI^3 * R^6
c
c    Sphere_Area ( M, R ) = 2 * PI^(M/2) * R^(M-1) / Gamma ( M / 2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 September 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, double precision R, the radius of the sphere.
c
c    Output, double precision HYPERSPHERE_AREA, the area of the sphere.
c
      implicit none

      double precision hypersphere_01_area
      double precision hypersphere_area
      integer m
      double precision r

      hypersphere_area = r ** ( m - 1  ) * hypersphere_01_area ( m )

      return
      end
      subroutine hypersphere_stereograph ( m, n, x, x2 )

c*********************************************************************72
c
cc HYPERSPHERE_STEREOGRAPH: stereographic mapping of points on a hypersphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    M must be at least 2.
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(M,N), the points to be mapped.
c
c    Output, double precision X2(M-1,N), the stereographically mapped points.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision x(m,n)
      double precision x2(m-1,n)

      do j = 1, n
        do i = 1, m - 1
          x2(i,j) = x(i,j)
        end do
      end do

      do j = 1, n
        do i = 1, m - 1
          x2(i,j) = x2(i,j) / ( 1.0D+00 - x(m,j) )
        end do
      end do
      
      return
      end
      subroutine hypersphere_stereograph_inverse ( m, n, x2, x )

c*********************************************************************72
c
cc HYPERSPHERE_STEREOGRAPH_INVERSE inverts a stereographic map.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    M must be at least 2.
c
c    Input, integer N, the number of points.
c
c    Input, double precision X2(M-1,N), points in the plane.
c
c    Input, double precision X(M,N), points mapped back to the hypersphere.
c
      implicit none

      integer m
      integer n

      double precision d(n)
      integer i
      integer j
      double precision t
      double precision x(m,n)
      double precision x2(m-1,n)

      do j = 1, n
        do i = 1, m - 1
          x(1:m-1,1:n) = 2.0D+00 * x2(1:m-1,1:n)
        end do
      end do

      do j = 1, n
        t = 0.0D+00
        do i = 1, m - 1
          t = t + x2(i,j) ** 2
        end do
        d(j) = t
      end do

      do j = 1, n
        x(m,j) = d(j) - 1.0D+00
      end do
     
      do j = 1, n
        do i = 1, m
          x(i,j) = x(i,j) / ( d(j) + 1.0D+00 )
        end do
      end do

      return
      end
      subroutine hypersphere_surface_uniform ( m, n, r, c, seed, x )

c*********************************************************************72
c
cc HYPERSPHERE_SURFACE_UNIFORM: uniform hypersphere surface samples
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Russell Cheng,
c    Random Variate Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998, pages 168.
c
c    George Marsaglia,
c    Choosing a point from the surface of a sphere,
c    Annals of Mathematical Statistics,
c    Volume 43, Number 2, April 1972, pages 645-646.
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Wiley, 1986, page 234.
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input, real R, the radius of the sphere.
c
c    Input, real C(M), the center of the sphere.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, real X(M,N), the points.
c
      implicit none

      integer m
      integer n

      double precision c(m)
      integer i
      integer j
      double precision r
      double precision seed
      double precision x(m,n)

      call hypersphere_01_surface_uniform ( m, n, seed, x )
c
c  Scale by the sphere radius.
c
      do j = 1, n
        do i = 1, m
          x(i,j) = r * x(i,j)
        end do
      end do
c
c  Shift to the sphere center.
c
      do j = 1, n
        do i = 1, m
          x(i,j) = x(i,j) + c(i)
        end do
      end do

      return
      end
      subroutine hypersphere_to_cartesian ( m, n, c, r, theta, x )

c*********************************************************************72
c
cc HYPERSPHERE_TO_CARTESIAN: hypersphere to Cartesian coordinate transform.
c
c  Discussion:
c
c    We allow the trivial case M = 1; in that case alone, the value R
c    must be assumed to have a sign.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 <= M.
c
c    Input, integer N, the number of points to transform.
c
c    Input, real C(M), the center of the hypersphere.
c
c    Input, real R(N), the radius of the points on the hypersphere.
c    Except for the trivial case M = 1, R is assumed nonnegative.
c
c    Input, real THETA(M-1,N), the coordinate angles of the points,
c    measured in radians.
c
c    Output, real X(M,N), the Cartesian coordinates of the points.
c
      implicit none

      integer m
      integer n

      double precision c(m)
      integer i
      integer i1
      integer i2
      integer j
      double precision r(n)
      double precision theta(m-1,n)
      double precision x(m,n)

      if ( m .eq. 1 ) then

        do j = 1, n
          x(1,j) = r(j)
        end do

      else

        do j = 1, n
          do i = 1, m
            x(i,j) = r(j)
          end do
        end do

        do j = 1, n
          do i1 = 1, m - 1
            x(i1,j) = x(i1,j) * cos ( theta(i1,j) )
            do i2 = i1 + 1, m
              x(i2,j) = x(i2,j) * sin ( theta(i1,j) )
            end do
          end do
        end do

      end if
c
c  Add the center.
c
      do j = 1, n
        do i = 1, m
          x(i,j) = x(i,j) + c(i)
        end do
      end do

      return
      end
      function hypersphere_volume ( m, r )

c*********************************************************************72
c
cc HYPERSPHERE_VOLUME computes the volume of a hypersphere.
c
c  Discussion:
c
c    An implicit sphere in ND satisfies the equation:
c
c      sum ( ( X(1:N) - PC(1:N) )^2 ) = R^2
c
c    where R is the radius and PC is the center.
c
c    Results for the first few values of N are:
c
c    DIM_NUM  Volume
c    -     -----------------------
c    2                PI   * R^2
c    3     (4/3)    * PI   * R^3
c    4     (1/2)    * PI^2 * R^4
c    5     (8/15)   * PI^2 * R^5
c    6     (1/6)    * PI^3 * R^6
c    7     (16/105) * PI^3 * R^7
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 September 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, double precision R, the radius of the sphere.
c
c    Output, double precision HYPERSPHERE_VOLUME, the volume of the sphere.
c
      implicit none

      double precision hypersphere_01_volume
      double precision hypersphere_volume
      integer m
      double precision r

      hypersphere_volume = ( r ** m ) * hypersphere_01_volume ( m )

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
      function r8mat_norm_fro_affine ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_NORM_FRO_AFFINE returns the Frobenius norm of an R8MAT difference.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    The Frobenius norm is defined as
c
c      R8MAT_NORM_FRO = sqrt (
c        sum ( 1 .le. I .le. M ) sum ( 1 .le. j .le. N ) A(I,J)^2 )
c
c    The matrix Frobenius norm is not derived from a vector norm, but
c    is compatible with the vector L2 norm, so that:
c
c      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, double precision A1(M,N), A2(M,N), the matrices for which the
c    Frobenius norm of the difference is desired.
c
c    Output, double precision R8MAT_NORM_FRO_AFFINE, the Frobenius norm 
C    of A1 - A2.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j
      double precision r8mat_norm_fro_affine
      double precision value

      value = 0.0D+00
      do j = 1, n
        do i = 1, m
          value = value + ( a1(i,j) - a2(i,j) )**2 
        end do
      end do
      value = sqrt ( value )

      r8mat_norm_fro_affine = value

      return
      end
      subroutine r8mat_normal_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
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
c    Output, double precision R(M,N), the array of pseudonormal values.
c
      implicit none

      integer m
      integer n

      integer seed
      double precision r(m,n)

      call r8vec_normal_01 ( m * n, seed, r )

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
        stop 1
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
      subroutine r8vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.  If N is negative,
c    then the code will flush its internal memory; in particular,
c    if there is a saved value to be used on the next call, it is
c    instead discarded.  This is useful if the user has reset the
c    random number seed, for instance.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
      implicit none

      integer n

      integer i
      integer m
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(2)
      double precision r8_uniform_01
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  Maybe we don't need any more values.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop 1
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        do i = x_lo_index, x_hi_index, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        do i = x_lo_index, x_hi_index - 1, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do

        call r8vec_uniform_01 ( 2, seed, r )

        x(n) = sqrt ( -2.0D+00 * log ( r(1) ) )
     &    * cos ( 2.0D+00 * pi * r(1) )

      end if

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
      subroutine sphere_stereograph ( m, n, p, q )

c*********************************************************************72
c
cc SPHERE_STEREOGRAPH computes the stereographic image of points on a sphere.
c
c  Discussion:
c
c    We start with a sphere of radius 1 and center (0,0,0).
c
c    The north pole N = (0,0,1) is the point of tangency to the sphere
c    of a plane, and the south pole S = (0,0,-1) is the focus for the
c    stereographic projection.
c
c    For any point P on the sphere, the stereographic projection Q of the
c    point is defined by drawing the line from S through P, and computing
c    Q as the intersection of this line with the plane.
c
c    Actually, we allow the spatial dimension M to be arbitrary.  Values
c    of M make sense starting with 2.  The north and south poles are
c    selected as the points (0,0,...,+1) and (0,0,...,-1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    C F Marcus,
c    The stereographic projection in vector notation,
c    Mathematics Magazine,
c    Volume 39, Number 2, March 1966, pages 100-102.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision P(M,N), a set of points on the unit sphere.
c
c    Output, double precision Q(M,N), the coordinates of the
c    image points.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision p(m,n)
      double precision q(m,n)

      do j = 1, n
        do i = 1, m - 1
          q(i,j) = 2.0D+00 * p(i,j) / ( 1.0D+00 + p(m,j) )
        end do
        q(m,j) = 1.0D+00
      end do

      return
      end
      subroutine sphere_stereograph_inverse ( m, n, q, p )

c*********************************************************************72
c
cc SPHERE_STEREOGRAPH_INVERSE computes stereographic preimages of points.
c
c  Discussion:
c
c    We start with a sphere of radius 1 and center (0,0,0).
c
c    The north pole N = (0,0,1) is the point of tangency to the sphere
c    of a plane, and the south pole S = (0,0,-1) is the focus for the
c    stereographic projection.
c
c    For any point Q on the plane, the stereographic inverse projection
c    P of the point is defined by drawing the line from S through Q, and
c    computing P as the intersection of this line with the sphere.
c
c    Actually, we allow the spatial dimension M to be arbitrary.  Values
c    of M make sense starting with 2.  The north and south poles are
c    selected as the points (0,0,...,+1) and (0,0,...,-1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    C F Marcus,
c    The stereographic projection in vector notation,
c    Mathematics Magazine,
c    Volume 39, Number 2, March 1966, pages 100-102.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision Q(M,N), the points, which are presumed to lie
c    on the plane Z = 1.
c
c    Output, double precision P(M,N), the stereographic
c    inverse projections of the points.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision p(m,n)
      double precision q(m,n)
      double precision qn

      do j = 1, n

        qn = 0.0D+00
        do i = 1, m - 1
          qn = qn + q(i,j)**2
        end do

        do i = 1, m - 1
          p(i,j) = 4.0D+00 * q(i,j) / ( 4.0D+00 + qn )
        end do

        p(m,j) = ( 4.0D+00 - qn ) / ( 4.0D+00 + qn )

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
