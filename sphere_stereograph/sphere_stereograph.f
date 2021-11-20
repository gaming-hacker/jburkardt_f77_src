      subroutine plane_normal_basis_3d ( pp, normal, pq, pr )

c*********************************************************************72
c
cc PLANE_NORMAL_BASIS_3D finds two perpendicular vectors in a plane in 3D.
c
c  Discussion:
c
c    The normal form of a plane in 3D is:
c
c      PP is a point on the plane,
c      N is a normal vector to the plane.
c
c    The two vectors to be computed, PQ and PR, can be regarded as
c    the basis of a Cartesian coordinate system for points in the plane.
c    Any point in the plane can be described in terms of the "origin"
c    point PP plus a weighted sum of the two vectors PQ and PR:
c
c      P = PP + a * PQ + b * PR.
c
c    The vectors PQ and PR have unit length, and are perpendicular to N
c    and to each other.
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
c    Input, double precision PP(3), a point on the plane.  (Actually,
c    we never need to know these values to do the calculationc)
c
c    Input, double precision NORMAL(3), a normal vector N to the plane.  The
c    vector must not have zero length, but it is not necessary for N
c    to have unit length.
c
c    Output, double precision PQ(3), a vector of unit length,
c    perpendicular to the vector N and the vector PR.
c
c    Output, double precision PR(3), a vector of unit length,
c    perpendicular to the vector N and the vector PQ.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )

      integer i
      double precision r8vec_norm
      double precision normal(dim_num)
      double precision normal_norm
      double precision pp(dim_num)
      double precision pq(dim_num)
      double precision pr(dim_num)
      double precision pr_norm
c
c  Compute the length of NORMAL.
c
      normal_norm = r8vec_norm ( dim_num, normal )

      if ( normal_norm .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PLANE_NORMAL_BASIS_3D - Fatal error!'
        write ( *, '(a)' ) '  The normal vector is 0.'
        stop
      end if
c
c  Find a vector PQ that is normal to NORMAL and has unit length.
c
      call r8vec_any_normal ( dim_num, normal, pq )
c
c  Now just take the cross product NORMAL x PQ to get the PR vector.
c
      call r8vec_cross_product_3d ( normal, pq, pr )

      pr_norm = r8vec_norm ( dim_num, pr )

      do i = 1, dim_num
        pr(i) = pr(i) / pr_norm
      end do

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
      subroutine r8vec_any_normal ( dim_num, v1, v2 )

c*********************************************************************72
c
cc R8VEC_ANY_NORMAL returns some normal vector to V1.
c
c  Discussion:
c
c    If DIM_NUM < 2, then no normal vector can be returned.
c
c    If V1 is the zero vector, then any unit vector will do.
c
c    No doubt, there are better, more robust algorithms.  But I will take
c    just about ANY reasonable unit vector that is normal to V1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 May 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, double precision V1(DIM_NUM), the vector.
c
c    Output, double precision V2(DIM_NUM), a vector that is
c    normal to V2, and has unit Euclidean length.
c
      implicit none

      integer dim_num

      integer i
      integer j
      integer k
      double precision r8vec_norm
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision vj
      double precision vk

      if ( dim_num .lt. 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_ANY_NORMAL - Fatal error!'
        write ( *, '(a)' ) '  Called with DIM_NUM < 2.'
        stop 1
      end if

      if ( r8vec_norm ( dim_num, v1 ) .eq. 0.0D+00 ) then
        v2(1) = 1.0D+00
        do i = 2, dim_num
          v2(i) = 0.0D+00
        end do
        return
      end if
c
c  Seek the largest entry in V1, VJ = V1(J), and the
c  second largest, VK = V1(K).
c
c  Since V1 does not have zero norm, we are guaranteed that
c  VJ, at least, is not zero.
c
      j = - 1
      vj = 0.0D+00

      k = - 1
      vk = 0.0D+00

      do i = 1, dim_num

        if ( abs ( vk ) .lt. abs ( v1(i) ) .or. k .lt. 1 ) then

          if ( abs ( vj ) .lt. abs ( v1(i) ) .or. j .lt. 1 ) then
            k = j
            vk = vj
            j = i
            vj = v1(i)
          else
            k = i
            vk = v1(i)
          end if

        end if

      end do
c
c  Setting V2 to zero, except that V2(J) = -VK, and V2(K) = VJ,
c  will just about do the trick.
c
      do i = 1, dim_num
        v2(i) = 0.0D+00
      end do

      v2(j) = - vk / sqrt ( vk * vk + vj * vj )
      v2(k) =   vj / sqrt ( vk * vk + vj * vj )

      return
      end
      subroutine r8vec_cross_product_3d ( v1, v2, v3 )

c*********************************************************************72
c
cc R8VEC_CROSS_PRODUCT_3D computes the cross product of two R8VEC's in 3D.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The cross product in 3D can be regarded as the determinant of the
c    symbolic matrix:
c
c          |  i  j  k |
c      det | x1 y1 z1 |
c          | x2 y2 z2 |
c
c      = ( y1 * z2 - z1 * y2 ) * i
c      + ( z1 * x2 - x1 * z2 ) * j
c      + ( x1 * y2 - y1 * x2 ) * k
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), the two vectors.
c
c    Output, double precision V3(3), the cross product vector.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )

      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)

      v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
      v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
      v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

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
      function r8vec_norm_affine ( n, v0, v1 )

c*********************************************************************72
c
cc R8VEC_NORM_AFFINE returns the affine norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The affine vector L2 norm is defined as:
c
c      R8VEC_NORM_AFFINE(V0,V1)
c        = sqrt ( sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the vectors.
c
c    Input, double precision V0(N), the base vector.
c
c    Input, double precision V1(N), the vector whose affine norm is desired.
c
c    Output, double precision R8VEC_NORM_AFFINE, the L2 norm of V1-V0.
c
      implicit none

      integer n

      integer i
      double precision r8vec_norm_affine
      double precision v0(n)
      double precision v1(n)

      r8vec_norm_affine = 0.0D+00
      do i = 1, n
        r8vec_norm_affine = r8vec_norm_affine
     &    + ( v0(i) - v1(i) )**2
      end do
      r8vec_norm_affine = sqrt ( r8vec_norm_affine )

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
          qn = qn + q(i,j) ** 2
        end do

        do i = 1, m - 1
          p(i,j) = 4.0D+00 * q(i,j) / ( 4.0D+00 + qn )
        end do

        p(m,j) = ( 4.0D+00 - qn ) / ( 4.0D+00 + qn )

      end do

      return
      end
      subroutine sphere_stereograph2 ( m, n, p, focus, center, q )

c*********************************************************************72
c
cc SPHERE_STEREOGRAPH2 computes the stereographic image of points on a sphere.
c
c  Discussion:
c
c    We start with a sphere of center C.
c
c    F is a point on the sphere which is the focus of the mapping,
c    and the antipodal point 2*C-F is the point of tangency
c    to the sphere of a plane.
c
c    For any point P on the sphere, the stereographic projection Q of the
c    point is defined by drawing the line from F through P, and computing
c    Q as the intersection of this line with the plane.
c
c    The spatial dimension M is arbitrary, but should be at least 2.
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
c    Input, integer the number of points.
c
c    Input, double precision P[M*N], a set of points on the unit sphere.
c
c    Input, double precision FOCUS[M], the coordinates of the focus point.
c
c    Input, double precision CENTER[M], the coordinates of the center of
c    the sphere.
c
c    Output, double precision Q[M*N], the coordinates of the
c    image points,
c
      implicit none

      integer m
      integer n

      double precision center(m)
      double precision cf_dot_pf
      double precision cf_normsq
      double precision focus(m)
      integer i
      integer j
      double precision p(m,n)
      double precision q(m,n)
      double precision s

      do j = 1, n
        cf_normsq = 0.0D+00
        cf_dot_pf = 0.0D+00
        do i = 1, m
          cf_normsq = cf_normsq + ( center(i) - focus(i) ) ** 2
          cf_dot_pf = cf_dot_pf + ( center(i) - focus(i) ) 
     &      * ( p(i,j) - focus(i) )
        end do
        s = 2.0D+00 * cf_normsq / cf_dot_pf
        do i = 1, m
          q(i,j) = s * p(i,j) + ( 1.0D+00 - s ) * focus(i)
        end do
      end do
      return
      end
      subroutine sphere_stereograph2_inverse ( m, n, q, focus, center, 
     &  p )

c*********************************************************************72
c
cc SPHERE_STEREOGRAPH2_INVERSE computes stereographic preimages of points.
c
c  Discussion:
c
c    We start with a sphere of center C.
c
c    F is a point on the sphere which is the focus of the mapping,
c    and the antipodal point 2*C-F is the point of tangency
c    to the sphere of a plane.
c
c    For any point Q on the plane, the stereographic inverse projection
c    P of the point is defined by drawing the line from F through Q, and
c    computing P as the intersection of this line with the sphere.
c
c    The spatial dimension M is arbitrary, but should be at least 2.
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
c    on the plane.
c
c    Input, double precision FOCUS(M), the coordinates of the focus point.
c
c    Input, double precision CENTER(M), the coordinates of the center
c    of the sphere.
c
c    Output, double precision P(M,N), the stereographic
c    inverse projections of the points.
c
      implicit none

      integer m
      integer n

      double precision center(m)
      double precision cf_dot_qf
      double precision focus(m)
      integer i
      integer j
      double precision p(m,n)
      double precision q(m,n)
      double precision qf_normsq
      double precision s

      do j = 1, n

        cf_dot_qf = 0.0D+00
        qf_normsq = 0.0D+00
        do i = 1, m
          cf_dot_qf = cf_dot_qf + ( center(i) - focus(i) ) 
     &      * ( q(i,j) - focus(i) )
          qf_normsq = qf_normsq + ( q(i,j) - focus(i) ) ** 2
        end do

        s = 2.0D+00 * cf_dot_qf / qf_normsq
        do i = 1, m
          p(i,j) = s * q(i,j) + ( 1.0D+00 - s ) * focus(i)
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
      subroutine uniform_on_sphere01_map ( dim_num, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_SPHERE01_MAP maps uniform points onto the unit sphere.
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
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,N), the points.
c
      implicit none

      integer dim_num
      integer n

      integer i
      integer j
      double precision norm
      integer seed
      double precision x(dim_num,n)
c
c  Fill a matrix with normally distributed values.
c
      call r8mat_normal_01 ( dim_num, n, seed, x )
c
c  Normalize each column.
c
      do j = 1, n
c
c  Compute the length of the vector.
c
        norm = 0.0D+00
        do i = 1, dim_num
          norm = norm + x(i,j) ** 2
        end do
        norm = sqrt ( norm )
c
c  Normalize the vector.
c
        do i = 1, dim_num
          x(i,j) = x(i,j) / norm
        end do

      end do

      return
      end
