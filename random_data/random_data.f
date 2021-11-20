      function arc_cosine ( c )

c*********************************************************************72
c
cc ARC_COSINE computes the arc cosine function, with argument truncation.
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
c    Output, double precision ARC_COSINE, an angle whose cosine is C.
c
      implicit none

      double precision arc_cosine
      double precision c
      double precision c2

      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )

      arc_cosine = acos ( c2 )

      return
      end
      subroutine bad_in_simplex01 ( dim_num, point_num, seed, x )

c*********************************************************************72
c
cc BAD_IN_SIMPLEX01 is a "bad" (nonuniform) sampling of the unit simplex.
c
c  Discussion:
c
c    The interior of the unit DIM_NUM-dimensional simplex is the set of
c    points X(1:DIM_NUM) such that each X(I) is nonnegative, and
c    sum(X(1:DIM_NUM)) .le. 1.
c
c    Any point in the unit simplex CAN be chosen by this algorithm.
c
c    However, the points that are chosen tend to be clustered near
c    the centroid.
c
c    This routine is supplied as an example of "bad" sampling.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer POINT_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,POINT_NUM), the points.
c
      implicit none

      integer dim_num
      integer point_num

      double precision e(dim_num+1)
      double precision e_sum
      integer i
      integer j
      double precision r8vec_sum
      integer seed
      double precision x(dim_num,point_num)

      do j = 1, point_num

        call r8vec_uniform_01 ( dim_num + 1, seed, e )

        e_sum = r8vec_sum ( dim_num + 1, e )

        do i = 1, dim_num + 1
          e(i) = e(i) / e_sum
        end do
c
c  We may take the values E(1:DIM_NUM+1) as being the barycentric
c  coordinates of the point.
c
        do i = 1, dim_num
          x(i,j) = e(i)
        end do

      end do

      return
      end
      subroutine brownian ( dim_num, n, seed, x )

c*********************************************************************72
c
cc BROWNIAN creates Brownian motion points.
c
c  Discussion:
c
c    A starting point is generated at the origin.  The next point
c    is generated at a uniformly random angle and a (0,1) normally
c    distributed distance from the previous point.
c
c    It is up to the user to rescale the data, if desired.
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
c    John Burkardt
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

      double precision direction(dim_num)
      integer i
      integer j
      double precision r
      double precision r8_normal_01
      integer seed
      double precision x(dim_num,n)
c
c  Initial point.
c
      do i = 1, dim_num
        x(i,1) = 0.0D+00
      end do
c
c  Generate angles and steps.
c
      do j = 2, n

        r = r8_normal_01 ( seed )
        r = abs ( r )

        call direction_uniform_nd ( dim_num, seed, direction )

        do i = 1, dim_num
          x(i,j) = x(i,j-1) + r * direction(i)
        end do

      end do

      return
      end
      subroutine direction_uniform_nd ( dim_num, seed, w )

c*********************************************************************72
c
cc DIRECTION_UNIFORM_ND generates a random direction vector.
c
c  Discussion:
c
c    This is actually simply a random point on the unit sphere.
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision W(DIM_NUM), a random direction vector,
c    with unit norm.
c
      implicit none

      integer dim_num

      integer i
      double precision norm
      double precision r8vec_norm
      integer seed
      double precision w(dim_num)
c
c  Sample the standard normal distribution.
c
      call r8vec_normal_01 ( dim_num, seed, w )
c
c  Compute the length of the vector.
c
      norm = r8vec_norm ( dim_num, w )
c
c  Normalize the vector.
c
      do i = 1, dim_num
        w(i) = w(i) / norm
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
      subroutine grid_in_cube01 ( dim_num, n, center, seed, r )

c*********************************************************************72
c
cc GRID_IN_CUBE01 generates grid points in the unit hypercube.
c
c  Discussion:
c
c    N points are needed in an DIM_NUM dimensional space.
c
c    The points are to lie on a uniform grid of side N_SIDE.
c
c    Unless the N = N_SIDE**DIM_NUM for some N_SIDE, we can't use all the
c    points on a grid.  What we do is find the smallest N_SIDE
c    that's big enough, and randomly omit some points.
c
c    If N_SIDE is 4, then the choices in 1D are:
c
c    A: 0,   1/3, 2/3, 1
c    B: 1/5, 2/5, 3/5, 4/5
c    C: 0,   1/4, 2/4, 3/4
c    D: 1/4, 2/4, 3/4, 1
c    E: 1/8, 3/8, 5/8, 7/8
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 May 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, integer CENTER, specifies the 1D grid centering:
c    1: first point is 0.0, last point is 1.0;
c    2: first point is 1/(N+1), last point is N/(N+1);
c    3: first point is 0, last point is (N-1)/N;
c    4: first point is 1/N, last point is 1;
c    5: first point is 1/(2*N), last point is (2*N-1)/(2*N);
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision R(DIM_NUM,N), the points.
c
      implicit none

      integer dim_num
      integer n

      integer base(dim_num)
      integer center
      integer i
      integer j
      integer n_grid
      integer n_side
      double precision r(dim_num,n)
      integer rank
      integer rank_list(n)
      integer seed
      integer tuple(dim_num)
c
c  Find the dimension of the smallest grid with N points.
c
      call grid_side ( dim_num, n, n_side )
c
c  We need to select N points out of N_SIDE**DIM_NUM set.
c
      n_grid = n_side ** dim_num
c
c  Generate a random subset of N items from a set of size N_GRID.
c
      call ksub_random2 ( n_grid, n, seed, rank_list )
c
c  Must make one dummy call to TUPLE_NEXT_FAST with RANK = -1.
c
      rank = -1
      call tuple_next_fast ( n_side, dim_num, rank, base, tuple )
c
c  Now generate the appropriate indices, and "center" them.
c
      do j = 1, n

        rank = rank_list(j) - 1

        call tuple_next_fast ( n_side, dim_num, rank, base, tuple )

        do i = 1, dim_num

          if ( center .eq. 1 ) then
            r(i,j) = dble ( tuple(i) - 1 ) / dble ( n_side - 1 )
          else if ( center .eq. 2 ) then
            r(i,j) = dble ( tuple(i) ) / dble ( n_side + 1 )
          else if ( center .eq. 3 ) then
            r(i,j) = dble ( tuple(i) - 1 ) / dble ( n_side )
          else if ( center .eq. 4 ) then
            r(i,j) = dble ( tuple(i) ) / dble ( n_side )
          else if ( center .eq. 5 ) then
            r(i,j) = dble ( 2 * tuple(i) - 1 ) / dble ( 2 * n_side )
          end if

        end do
      end do

      return
      end
      subroutine grid_side ( dim_num, n, n_side )

c*********************************************************************72
c
cc GRID_SIDE finds the smallest grid containing at least N points.
c
c  Discussion:
c
c    Each coordinate of the grid will have N_SIDE distinct values.
c    Thus the total number of points in the grid is N_SIDE**DIM_NUM.
c    This routine seeks the smallest N_SIDE such that N .le. N_SIDE**M.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Output, integer N_SIDE, the length of one side of the
c    smallest grid in M dimensions that contains at least N points.
c
      implicit none

      integer dim_num
      double precision exponent
      integer n
      integer n_side

      if ( n .le. 0 ) then
        n_side = 0
        return
      end if

      if ( dim_num .le. 0 ) then
        n_side = -1
        return
      end if

      exponent = 1.0D+00 / dble ( dim_num )

      n_side = int ( ( dble ( n ) ) ** exponent )

      if ( n_side ** dim_num .lt. n ) then
        n_side = n_side + 1
      end if

      return
      end
      function halham_leap_check ( dim_num, leap )

c*********************************************************************72
c
cc HALHAM_LEAP_CHECK checks LEAP for a Halton or Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEAP(DIM_NUM), the leap vector.
c
c    Output, logical, HALHAM_LEAP_CHECK, is true if LEAP is legal.
c
      implicit none

      integer dim_num

      logical halham_leap_check
      integer i
      integer leap(dim_num)

      halham_leap_check = .true.

      do i = 1, dim_num
        if ( leap(i) .lt. 1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'HALHAM_LEAP_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Some entry of LEAP .lt. 1!'
          write ( *, '(a)' ) ' '
          call i4vec_transpose_print ( dim_num, leap, 'LEAP:  ' )
          halham_leap_check = .false.
        end if
      end do

      return
      end
      function halham_n_check ( n )

c*********************************************************************72
c
cc HALHAM_N_CHECK checks N for a Halton or Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the spatial dimension.
c
c    Output, logical HALHAM_N_CHECK, is true if N is legal.
c
      implicit none

      logical halham_n_check
      integer n

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HALHAM_N_CHECK - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        write ( *, '(a,i12)' ) '  N = ', n
        halham_n_check = .false.
      else
        halham_n_check = .true.
      end if

      return
      end
      function halham_dim_num_check ( dim_num )

c*********************************************************************72
c
cc HALHAM_DIM_NUM_CHECK checks DIM_NUM for a Halton or Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Output, logical HALHAM_DIM_NUM_CHECK, is true if DIM_NUM is legal.
c
      implicit none

      logical halham_dim_num_check
      integer dim_num

      if ( dim_num .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HALHAM_DIM_NUM_CHECK - Fatal error!'
        write ( *, '(a)' ) '  DIM_NUM .lt. 1.'
        write ( *, '(a,i12)' ) '  DIM_NUM = ', dim_num
        halham_dim_num_check = .false.
      else
        halham_dim_num_check = .true.
      end if

      return
      end
      function halham_seed_check ( dim_num, seed )

c*********************************************************************72
c
cc HALHAM_SEED_CHECK checks SEED for a Halton or Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer SEED(DIM_NUM), the seed vector.
c
c    Output, logical, HALHAM_SEED_CHECK, is true if SEED is legal.
c
      implicit none

      integer dim_num

      logical halham_seed_check
      integer i
      integer seed(dim_num)

      halham_seed_check = .true.

      do i = 1, dim_num
        if ( seed(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'HALHAM_SEED_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Some entry of SEED .lt. 0!'
          write ( *, '(a)' ) ' '
          call i4vec_transpose_print ( dim_num, seed, 'SEED:  ' )
          halham_seed_check = .false.
        end if
      end do

      return
      end
      function halham_step_check ( step )

c*********************************************************************72
c
cc HALHAM_STEP_CHECK checks STEP for a Halton or Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer STEP, the index of the subsequence element.
c
c    Output, logical HALHAM_STEP_CHECK, is true if STEP is legal.
c
      implicit none

      logical halham_step_check
      integer step

      if ( step .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HALHAM_STEP_CHECK - Fatal error!'
        write ( *, '(a)' ) '  STEP .lt. 0.'
        write ( *, '(a,i12)' ) '  STEP = ', step
        halham_step_check = .false.
      else
        halham_step_check = .true.
      end if

      return
      end
      function halton_base_check ( dim_num, base )

c*********************************************************************72
c
cc HALTON_BASE_CHECK checks BASE for a Halton sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer BASE(DIM_NUM), the bases.
c
c    Output, logical, HALTON_BASE_CHECK, is true if BASE is legal.
c
      implicit none

      integer dim_num

      integer base(dim_num)
      logical halton_base_check
      integer i

      halton_base_check = .true.

      do i = 1, dim_num
        if ( base(i) .le. 1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'HALTON_BASE_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Some entry of BASE is .le. 1!'
          write ( *, '(a)' ) ' '
          call i4vec_transpose_print ( dim_num, base, 'BASE:  ' )
          halton_base_check = .false.
        end if
      end do

      return
      end
      subroutine halton_in_circle01_accept ( dim_num, n, seed, x )

c*********************************************************************72
c
cc HALTON_IN_CIRCLE01_ACCEPT accepts Halton points in the unit circle.
c
c  Discussion:
c
c    The acceptance/rejection method is used.
c
c    The unit circle has center at the origin, and radius 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space
c    (which is 2).
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

      integer base(dim_num)
      integer have
      integer i
      integer leap(dim_num)
      integer prime
      double precision r8vec_norm
      integer seed
      integer seed_vec(dim_num)
      integer step
      double precision u(dim_num)
      double precision x(dim_num,n)

      have = 0

      do i = 1, dim_num
        seed_vec(i) = 0
      end do

      do i = 1, dim_num
        leap(i) = 1
      end do

      do i = 1, dim_num
        base(i) = prime ( i )
      end do

10    continue

      if ( have .lt. n ) then

        step = seed

        call i4_to_halton ( dim_num, step, seed_vec, leap, base, u )

        seed = seed + 1

        do i = 1, dim_num
          u(i) = 2.0D+00 * u(i) - 1.0D+00
        end do

        if ( r8vec_norm ( dim_num, u ) .le. 1.0D+00 ) then
          have = have + 1
          do i = 1, dim_num
            x(i,have) = u(i)
          end do
        end if

        go to 10

      end if

      return
      end
      subroutine halton_in_circle01_map ( dim_num, n, seed, x )

c*********************************************************************72
c
cc HALTON_IN_CIRCLE01_MAP maps Halton points into the unit circle.
c
c  Discussion:
c
c    The unit circle has center at the origin, and radius 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space
c    (which is 2).
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

      integer base(1)
      integer i
      integer leap(1)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(n)
      integer seed
      integer seed_vec(1)
      integer step
      double precision t(n)
      double precision x(dim_num,n)

      step = seed
      seed_vec(1) = 0
      leap(1) = 1
      base(1) = 2

      call i4_to_halton_sequence ( 1, n, step, seed_vec, leap, base, r )

      do i = 1, n
        r(i) = sqrt ( r(i) )
      end do

      step = seed
      seed_vec(1) = 0
      leap(1) = 1
      base(1) = 3

      call i4_to_halton_sequence ( 1, n, step, seed_vec, leap, base, t )

      do i = 1, n
        t(i) = 2.0D+00 * pi * t(i)
      end do

      do i = 1, n
        x(1,i) = r(i) * cos ( t(i) )
        x(2,i) = r(i) * sin ( t(i) )
      end do

      seed = seed + n

      return
      end
      subroutine halton_in_cube01 ( dim_num, n, seed, x )

c*********************************************************************72
c
cc HALTON_IN_CUBE01 generates Halton points in the unit hypercube.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2004
c
c  Author:
c
c    John Burkardt
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
c    Output, double precision X(DIM_NUM,N), a Halton sequence of length N.
c
      implicit none

      integer dim_num
      integer n

      integer base(dim_num)
      integer i
      integer leap(dim_num)
      integer prime
      integer seed
      integer seed_vec(dim_num)
      integer step
      double precision x(dim_num,n)

      step = seed

      do i = 1, dim_num
        seed_vec(i) = 0
      end do

      do i = 1, dim_num
        leap(i) = 1
      end do

      do i = 1, dim_num
        base(i) = prime(i)
      end do

      call i4_to_halton_sequence ( dim_num, n, step, seed_vec, leap, 
     &  base, x )

      seed = seed + n

      return
      end
      function hammersley_base_check ( dim_num, base )

c*********************************************************************72
c
cc HAMMERSLEY_BASE_CHECK is TRUE if BASE is legal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer BASE(DIM_NUM), the bases.
c
c    Output, logical, HAMMERSLEY_BASE_CHECK.
c
      implicit none

      integer dim_num

      integer base(dim_num)
      logical hammersley_base_check
      integer i

      hammersley_base_check = .true.

      do i = 1, dim_num
        if ( base(i) .eq. 0 .or. base(i) .eq. 1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'HAMMERSLEY_BASE_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Some entry of BASE is 0 or 1!'
          write ( *, '(a)' ) ' '
          call i4vec_transpose_print ( dim_num, base, 'BASE:  ' )
          hammersley_base_check = .false.
        end if
      end do

      return
      end
      subroutine hammersley_in_cube01 ( dim_num, n, seed, x )

c*********************************************************************72
c
cc HAMMERSLEY_IN_CUBE01 generates Hammersley points in the unit hypercube.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the element.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,N), the elements of the Hammersley
c    sequence.
c
      implicit none

      integer dim_num
      integer n

      integer arg
      integer base(dim_num)
      integer i
      integer leap(dim_num)
      integer prime
      integer seed
      integer seed_vec(dim_num)
      integer step
      double precision x(dim_num,n)

      step = seed
      do i = 1, dim_num
        seed_vec(i) = 0
      end do

      do i = 1, dim_num
        leap(i) = 1
      end do

      base(1) = -n
      do i = 2, dim_num
        arg = i - 1
        base(i) = prime ( arg )
      end do

      call i4_to_hammersley_sequence ( dim_num, n, step, seed_vec, 
     &  leap, base, x )

      seed = seed + n

      return
      end
      function i4_factorial ( n )

c*********************************************************************72
c
cc I4_FACTORIAL computes the factorial of N.
c
c  Discussion:
c
c    factorial ( N ) = product ( 1 .le. I .le. N ) I
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the factorial function.
c    If N is less than 1, the function value is returned as 1.
c    0 .le. N .le. 13 is required.
c
c    Output, integer I4_FACTORIAL, the factorial of N.
c
      implicit none

      integer i
      integer i4_factorial
      integer n

      i4_factorial = 1

      if ( 13 .lt. n ) then
        i4_factorial = - 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_FACTORIAL - Fatal error!'
        write ( *, '(a)' )
     &  '  I4_FACTORIAL(N) cannot be computed as an integer'
        write ( *, '(a)' ) '  for 13 .lt. N.'
        write ( *, '(a,i8)' ) '  Input value N = ', n
        stop
      end if

      do i = 1, n
        i4_factorial = i4_factorial * i
      end do

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
      subroutine i4_to_halton ( dim_num, step, seed, leap, base, r )

c*********************************************************************72
c
cc I4_TO_HALTON computes one element of a leaped Halton subsequence.
c
c  Discussion:
c
c    The DIM_NUM-dimensional Halton sequence is really DIM_NUM separate
c    sequences, each generated by a particular base.
c
c    This routine selects elements of a "leaped" subsequence of the
c    Halton sequence.  The subsequence elements are indexed by a
c    quantity called STEP, which starts at 0.  The STEP-th subsequence
c    element is simply element
c
c      SEED(1:DIM_NUM) + STEP * LEAP(1:DIM_NUM)
c
c    of the original Halton sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    John Halton,
c    On the efficiency of certain quasi-random sequences of points
c    in evaluating multi-dimensional integrals,
c    Numerische Mathematik,
c    Volume 2, Number 1, December 1960, pages 84-90.
c
c    John Halton, G B Smith,
c    Algorithm 247: Radical-Inverse Quasi-Random Point Sequence,
c    Communications of the ACM,
c    Volume 7, Number 12, December 1964, pages 701-702
c
c    Ladislav Kocis, William Whiten,
c    Computational Investigations of Low-Discrepancy Sequences,
c    ACM Transactions on Mathematical Software,
c    Volume 23, Number 2, June 1997, pages 266-294.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c    1 .le. DIM_NUM is required.
c
c    Input, integer STEP, the index of the subsequence element.
c    0 .le. STEP is required.
c
c    Input, integer SEED(DIM_NUM), the Halton sequence index
c    corresponding to STEP = 0.
c    0 .le. SEED(1:DIM_NUM) is required.
c
c    Input, integer LEAP(DIM_NUM), the successive jumps in
c    the Halton sequence. 1 .le. LEAP(1:DIM_NUM) is required.
c
c    Input, integer BASE(DIM_NUM), the Halton bases.
c    1 .lt. BASE(1:DIM_NUM) is required.
c
c    Output, double precision R(DIM_NUM), the STEP-th element of the leaped
c    Halton subsequence.
c
      implicit none

      integer dim_num

      integer base(dim_num)
      double precision base_inv
      integer digit
      logical halham_leap_check
      logical halham_dim_num_check
      logical halham_seed_check
      logical halham_step_check
      logical halton_base_check
      integer i
      integer leap(dim_num)
      double precision r(dim_num)
      integer seed(dim_num)
      integer seed2
      integer step
c
c  Check the input.
c
      if ( .not. halham_dim_num_check ( dim_num ) ) then
        stop 1
      end if

      if ( .not. halham_step_check ( step ) ) then
        stop 1
      end if

      if ( .not. halham_seed_check ( dim_num, seed ) ) then
        stop 1
      end if

      if ( .not. halham_leap_check ( dim_num, leap ) ) then
        stop 1
      end if

      if ( .not. halton_base_check ( dim_num, base ) ) then
        stop 1
      end if
c
c  Calculate the data.
c
      do i = 1, dim_num

        seed2 = seed(i) + step * leap(i)

        r(i) = 0.0D+00

        base_inv = dble ( 1.0D+00 ) / dble ( base(i) )

10      continue

        if ( seed2 .ne. 0 ) then
          digit = mod ( seed2, base(i) )
          r(i) = r(i) + dble ( digit ) * base_inv
          base_inv = base_inv / dble ( base(i) )
          seed2 = seed2 / base(i)
          go to 10
        end if

      end do

      return
      end
      subroutine i4_to_halton_sequence ( dim_num, n, step, seed, leap, 
     &  base, r )

c*********************************************************************72
c
cc I4_TO_HALTON_SEQUENCE computes N elements of a leaped Halton subsequence.
c
c  Discussion:
c
c    The DIM_NUM-dimensional Halton sequence is really DIM_NUM separate
c    sequences, each generated by a particular base.
c
c    This routine selects elements of a "leaped" subsequence of the
c    Halton sequence.  The subsequence elements are indexed by a
c    quantity called STEP, which starts at 0.  The STEP-th subsequence
c    element is simply element
c
c      SEED(1:DIM_NUM) + STEP * LEAP(1:DIM_NUM)
c
c    of the original Halton sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 July 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    John Halton,
c    On the efficiency of certain quasi-random sequences of points
c    in evaluating multi-dimensional integrals,
c    Numerische Mathematik,
c    Volume 2, Number 1, December 1960, pages 84-90.
c
c    John Halton, G B Smith,
c    Algorithm 247: Radical-Inverse Quasi-Random Point Sequence,
c    Communications of the ACM,
c    Volume 7, Number 12, December 1964, pages 701-702
c
c    Ladislav Kocis, William Whiten,
c    Computational Investigations of Low-Discrepancy Sequences,
c    ACM Transactions on Mathematical Software,
c    Volume 23, Number 2, June 1997, pages 266-294.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c    1 .le. DIM_NUM is required.
c
c    Input, integer N, the number of elements of the sequence.
c
c    Input, integer STEP, the index of the subsequence element.
c    0 .le. STEP is required.
c
c    Input, integer SEED(DIM_NUM), the Halton sequence index
c    corresponding to STEP = 0.
c
c    Input, integer LEAP(DIM_NUM), the succesive jumps in
c    the Halton sequence.
c
c    Input, integer BASE(DIM_NUM), the Halton bases.
c
c    Output, double precision R(DIM_NUM,N), the next N elements of the
c    leaped Halton subsequence, beginning with element STEP.
c
      implicit none

      integer n
      integer dim_num

      integer base(dim_num)
      double precision base_inv
      integer digit(n)
      logical halham_leap_check
      logical halham_n_check
      logical halham_dim_num_check
      logical halham_seed_check
      logical halham_step_check
      logical halton_base_check
      integer i
      integer j
      integer leap(dim_num)
      double precision r(dim_num,n)
      integer seed(dim_num)
      integer seed2(n)
      logical skip
      integer step
c
c  Check the input.
c
      if ( .not. halham_dim_num_check ( dim_num ) ) then
        stop 1
      end if

      if ( .not. halham_n_check ( n ) ) then
        stop 1
      end if

      if ( .not. halham_step_check ( step ) ) then
        stop 1
      end if

      if ( .not. halham_seed_check ( dim_num, seed ) ) then
        stop 1
      end if

      if ( .not. halham_leap_check ( dim_num, leap ) ) then
        stop 1
      end if

      if ( .not. halton_base_check ( dim_num, base ) ) then
        stop 1
      end if
c
c  Calculate the data.
c
      do j = 1, n
        do i = 1, dim_num
          r(i,j) = 0.0D+00
        end do
      end do

      do i = 1, dim_num

        do j = 1, n
          seed2(j) = seed(i) + ( step + j - 1 ) * leap(i)
        end do

        base_inv = dble ( 1.0D+00 ) / dble ( base(i) )

10      continue
        
        skip = .true.

        do j = 1, n
          if ( seed2(j) .ne. 0 ) then
            skip = .false.
          end if
        end do

        if ( .not. skip ) then
          do j = 1, n
            digit(j) = mod ( seed2(j), base(i) )
          end do
          do j = 1, n
            r(i,j) = r(i,j) + dble ( digit(j) ) * base_inv
          end do
          base_inv = base_inv / dble ( base(i) )
          do j = 1, n
            seed2(j) = seed2(j) / base(i)
          end do

          go to 10

        end if

      end do

      return
      end
      subroutine i4_to_hammersley ( dim_num, step, seed, leap, base, r )

c*********************************************************************72
c
cc I4_TO_HAMMERSLEY computes one element of a leaped Hammersley subsequence.
c
c  Discussion:
c
c    The DIM_NUM-dimensional Hammersley sequence is really DIM_NUM separate
c    sequences, each generated by a particular base.  If the base is
c    greater than 1, a standard 1-dimensional
c    van der Corput sequence is generated.  But if the base is
c    negative, this is a signal that the much simpler sequence J/(-BASE)
c    is to be generated.  For the standard Hammersley sequence, the
c    first spatial coordinate uses a base of (-N), and subsequent
c    coordinates use bases of successive primes (2, 3, 5, 7, 11, ...).
c    This program allows the user to specify any combination of bases,
c    included nonprimes and repeated values.
c
c    This routine selects elements of a "leaped" subsequence of the
c    Hammersley sequence.  The subsequence elements are indexed by a
c    quantity called STEP, which starts at 0.  The STEP-th subsequence
c    element is simply element
c
c      SEED(1:DIM_NUM) + STEP * LEAP(1:DIM_NUM)
c
c    of the original Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    John Hammersley,
c    Monte Carlo methods for solving multivariable problems,
c    Proceedings of the New York Academy of Science,
c    Volume 86, 1960, pages 844-874.
c
c    Ladislav Kocis, William Whiten,
c    Computational Investigations of Low-Discrepancy Sequences,
c    ACM Transactions on Mathematical Software,
c    Volume 23, Number 2, June 1997, pages 266-294.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c    1 .le. DIM_NUM is required.
c
c    Input, integer STEP, the index of the subsequence element.
c    0 .le. STEP is required.
c
c    Input, integer SEED(DIM_NUM), the sequence index corresponding
c    to STEP = 0.
c    0 .le. SEED(1:DIM_NUM) is required.
c
c    Input, integer LEAP(DIM_NUM), the successive jumps in
c    the sequence.
c    1 .le. LEAP(1:DIM_NUM) is required.
c
c    Input, integer BASE(DIM_NUM), the bases.
c
c    Output, double precision R(DIM_NUM), the STEP-th element of the leaped
c    Hammersley subsequence.
c
      implicit none

      integer dim_num

      integer base(dim_num)
      double precision base_inv
      integer digit
      double precision fiddle
      parameter ( fiddle = 1.0D+00 )
      logical halham_leap_check
      logical halham_dim_num_check
      logical halham_seed_check
      logical halham_step_check
      logical hammersley_base_check
      integer i
      integer leap(dim_num)
      double precision r(dim_num)
      integer seed(dim_num)
      integer seed2
      integer step
c
c  Check the input.
c
      if ( .not. halham_dim_num_check ( dim_num ) ) then
        stop 1
      end if

      if ( .not. halham_step_check ( step ) ) then
        stop 1
      end if

      if ( .not. halham_seed_check ( dim_num, seed ) ) then
        stop 1
      end if

      if ( .not. halham_leap_check ( dim_num, leap ) ) then
        stop 1
      end if

      if ( .not. hammersley_base_check ( dim_num, base ) ) then
        stop 1
      end if
c
c  Calculate the data.
c
      do i = 1, dim_num

        if ( 1 .lt. base(i) ) then

          seed2 = seed(i) + step * leap(i)

          r(i) = 0.0D+00

          base_inv = dble ( 1.0D+00 ) / dble ( base(i) )

10        continue

          if ( seed2 .ne. 0 ) then
            digit = mod ( seed2, base(i) )
            r(i) = r(i) + dble ( digit ) * base_inv
            base_inv = base_inv / dble ( base(i) )
            seed2 = seed2 / base(i)
            go to 10
          end if
c
c  In the following computation, the value of FIDDLE can be:
c
c    0,   for the sequence 0/N, 1/N, ..., N-1/N
c    1,   for the sequence 1/N, 2/N, ..., N/N
c    1/2, for the sequence 1/(2N), 3/(2N), ..., (2*N-1)/(2N)
c
        else if ( base(i) .le. -1 ) then

          seed2 = seed(i) + step * leap(i)

          seed2 = mod ( seed2, abs ( base(i) ) )

          r(i) = ( dble ( seed2 ) + fiddle ) / dble ( -base(i) )

        end if

      end do

      return
      end
      subroutine i4_to_hammersley_sequence ( dim_num, n, step, seed, 
     &  leap, base, r )

c*********************************************************************72
c
cc I4_TO_HAMMERSLEY_SEQUENCE: N elements of a leaped Hammersley subsequence.
c
c  Discussion:
c
c    The DIM_NUM-dimensional Hammersley sequence is really DIM_NUM separate
c    sequences, each generated by a particular base.  If the base is
c    greater than 1, a standard 1-dimensional
c    van der Corput sequence is generated.  But if the base is
c    negative, this is a signal that the much simpler sequence J/(-BASE)
c    is to be generated.  For the standard Hammersley sequence, the
c    first spatial coordinate uses a base of (-N), and subsequent
c    coordinates use bases of successive primes (2, 3, 5, 7, 11, ...).
c    This program allows the user to specify any combination of bases,
c    included nonprimes and repeated values.
c
c    This routine selects elements of a "leaped" subsequence of the
c    Hammersley sequence.  The subsequence elements are indexed by a
c    quantity called STEP, which starts at 0.  The STEP-th subsequence
c    element is simply element
c
c      SEED(1:DIM_NUM) + STEP * LEAP(1:DIM_NUM)
c
c    of the original Hammersley sequence.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    John Hammersley,
c    Monte Carlo methods for solving multivariable problems,
c    Proceedings of the New York Academy of Science,
c    Volume 86, 1960, pages 844-874.
c
c    Ladislav Kocis, William Whiten,
c    Computational Investigations of Low-Discrepancy Sequences,
c    ACM Transactions on Mathematical Software,
c    Volume 23, Number 2, June 1997, pages 266-294.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c    1 .le. DIM_NUM is required.
c
c    Input, integer N, the number of elements of the sequence.
c
c    Input, integer STEP, the index of the subsequence element.
c    0 .le. STEP is required.
c
c    Input, integer SEED(DIM_NUM), the sequence index corresponding
c    to STEP = 0.
c
c    Input, integer LEAP(DIM_NUM), the succesive jumps in the
c    sequence.
c
c    Input, integer BASE(DIM_NUM), the bases.
c
c    Output, double precision R(DIM_NUM,N), the next N elements of the
c    leaped Hammersley subsequence, beginning with element STEP.
c
      implicit none

      integer n
      integer dim_num

      integer base(dim_num)
      double precision base_inv
      integer digit(n)
      double precision fiddle
      parameter ( fiddle = 1.0D+00 )
      logical halham_leap_check
      logical halham_dim_num_check
      logical halham_seed_check
      logical halham_step_check
      logical hammersley_base_check
      integer i
      integer j
      integer leap(dim_num)
      double precision r(dim_num,n)
      integer seed(dim_num)
      integer seed2(n)
      logical skip
      integer step
c
c  Check the input.
c
      if ( .not. halham_dim_num_check ( dim_num ) ) then
        stop 1
      end if

      if ( .not. halham_step_check ( step ) ) then
        stop 1
      end if

      if ( .not. halham_seed_check ( dim_num, seed ) ) then
        stop 1
      end if

      if ( .not. halham_leap_check ( dim_num, leap ) ) then
        stop 1
      end if

      if ( .not. hammersley_base_check ( dim_num, base ) ) then
        stop 1
      end if
c
c  Calculate the data.
c
      do i = 1, dim_num

        if ( 1 .lt. base(i) ) then

          do j = 1, n
            seed2(j) = seed(i) + ( step + j - 1 ) * leap(i)
          end do

          do j = 1, n
            r(i,j) = 0.0D+00
          end do

          base_inv = dble ( 1.0D+00 ) / dble ( base(i) )

10        continue

          skip = .true.
          do j = 1, n
            if ( seed2(j) .ne. 0 ) then
              skip = .false.
            end if
          end do

          if ( .not. skip ) then
            do j = 1, n
              digit(j) = mod ( seed2(j), base(i) )
            end do
            do j = 1, n
              r(i,j) = r(i,j) + dble ( digit(j) ) * base_inv
            end do
            base_inv = base_inv / dble ( base(i) )
            do j = 1, n
              seed2(j) = seed2(j) / base(i)
            end do
            go to 10
          end if
c
c  In the following computation, the value of FIDDLE can be:
c
c    0,   for the sequence 0/N, 1/N, ..., N-1/N
c    1,   for the sequence 1/N, 2/N, ..., N/N
c    1/2, for the sequence 1/(2N), 3/(2N), ..., (2*N-1)/(2N)
c
        else if ( base(i) .le. -1 ) then

          do j = 1, n
            seed2(j) = seed(i) + ( step + j - 1 ) * leap(i)
          end do

          do j = 1, n
            seed2(j) = mod ( seed2(j), abs ( base(i) ) )
          end do

          do j = 1, n
            r(i,j) = ( dble ( seed2(j) ) + fiddle ) / dble ( -base(i) )
          end do

        end if

      end do

      return
      end
      function i4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM_AB, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_uniform_ab
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r = dble ( seed ) * 4.656612875E-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0E+00 - r ) * ( dble ( min ( a, b ) ) - 0.5E+00 )
     &  +             r   * ( dble ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform_ab = value

      return
      end
      subroutine i4vec_transpose_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_TRANSPOSE_PRINT prints an I4VEC "transposed".
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Example:
c
c    A = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
c    TITLE = 'My vector:  '
c
c    My vector:
c
c        1    2    3    4    5
c        6    7    8    9   10
c       11
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer ihi
      integer ilo
      character ( len = 11 ) string
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do ilo = 1, n, 5
        ihi = min ( ilo + 5 - 1, n )
        write ( *, '(5i12)' ) ( a(i), i = ilo, ihi)
      end do

      return
      end
      subroutine ksub_random2 ( n, k, seed, a )

c*********************************************************************72
c
cc KSUB_RANDOM2 selects a random subset of size K from a set of size N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 2003
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer N, the size of the set.
c
c    Input, integer K, the size of the subset, between 0 and N.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer A(K), the indices of the selected elements.
c
      implicit none

      integer k

      integer a(k)
      integer available
      integer candidate
      integer have
      integer n
      integer need
      double precision r
      double precision r8_uniform_01
      integer seed

      if ( k .lt. 0 .or. n .lt. k ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KSUB_RANDOM2 - Fatal error!'
        write ( *, '(a,i8)' ) '  N = ', n
        write ( *, '(a,i8)' ) '  K = ', k
        write ( *, '(a)' ) '  but 0 .le. K .le. N is required!'
        stop
      end if

      if ( k .eq. 0 ) then
        return
      end if

      need = k
      have = 0

      available = n
      candidate = 0

10    continue

        candidate = candidate + 1

        r = r8_uniform_01 ( seed )

        if ( dble ( available ) * r .le. dble ( need ) ) then

          need = need - 1
          have = have + 1
          a(have) = candidate

          if ( need .le. 0 ) then
            go to 20
          end if

        end if

        available = available - 1

      go to 10

20    continue

      return
      end
      subroutine normal ( dim_num, n, r, mu, seed, x )

c*********************************************************************72
c
cc NORMAL creates normally distributed points.
c
c  Discussion:
c
c    The multivariate normal distribution for the DIM_NUM dimensional vector X
c    has the form:
c
c      pdf(X) = (2*pi*det(V))**(-DIM_NUM/2)
c        * exp(-0.5*(X-MU)'*inverse(V)*(X-MU))
c
c    where MU is the mean vector, and V is a positive definite symmetric
c    matrix called the variance-covariance matrix.
c
c    This routine requires that the user supply the upper triangular
c    Cholesky factor R, which has the property that
c
c      V = R' * R
c
c    This factorization always exists if V is actually symmetric and
c    positive definite.  This factorization can be computed by the
c    routine DPO_FA.
c
c    The user also supplies the mean vector MU.
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input, double precision R(DIM_NUM,DIM_NUM), the upper triangular
c    Cholesky factor of the variance-covariance matrix.
c
c    Input, double precision MU(DIM_NUM), the mean vector.
c
c    Output, double precision X(DIM_NUM,N), the points.
c
      implicit none

      integer dim_num
      integer n

      integer i
      integer j
      double precision mu(dim_num)
      double precision r(dim_num,dim_num)
      double precision rt(dim_num)
      integer seed
      double precision t(dim_num)
      double precision x(dim_num,n)
c
c  Compute X = MU + R' * T.
c
      do j = 1, n
        call r8vec_normal_01 ( dim_num, seed, t )
        call r8mat_mtv ( dim_num, dim_num, r, t, rt )
        do i = 1, dim_num
          x(i,j) = mu(i) + rt(i)
        end do
      end do

      return
      end
      subroutine normal_circular ( dim_num, n, seed, x )

c*********************************************************************72
c
cc NORMAL_CIRCULAR creates circularly normal points.
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
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    US Department of Commerce, 1964, page 936.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space,
c    which must be 2.
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
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(n)
      integer seed
      double precision t(n)
      double precision x(dim_num,n)
c
c  The angle varies uniformly from 0 to 2 pi.
c
      call r8vec_uniform_01 ( n, seed, t )

      do i = 1, n
        t(i) = 2.0D+00 * pi * t(i)
      end do
c
c  The radius is normally distributed.
c
      call r8vec_normal_01 ( n, seed, r )

      do i = 1, n
        x(1,i) = r(i) * cos ( t(i) )
        x(2,i) = r(i) * sin ( t(i) )
      end do

      return
      end
      subroutine normal_multivariate ( m, n, r, mu, seed, x )

c*********************************************************************72
c
cc NORMAL_MULTIVARIATE samples a multivariate normal distribution.
c
c  Discussion:
c
c    The multivariate normal distribution for the DIM_NUM dimensional vector X
c    has the form:
c
c      pdf(X) = (2*pi*det(V))^(-M/2) * exp(-0.5*(X-MU)'*inverse(V)*(X-MU))
c
c    where MU is the mean vector, and V is a positive definite symmetric
c    matrix called the variance-covariance matrix.
c
c    This routine samples points associated with the M-dimensional
c    normal distribution with mean MU and covariance matrix V.
c
c    This routine requires that the user supply the upper triangular
c    Cholesky factor R of V, which has the property that
c
c      V = R' * R
c
c    This factorization always exists if V is actually symmetric and
c    positive definite.  This factorization can be computed by the
c    routine DPO_FA.
c
c    The user also supplies the mean vector MU.
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
c    John Burkardt
c
c  Reference:
c
c    Russell Cheng,
c    Random Variate Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998, pages 167-168.
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input, double precision R(M,M), the upper triangular
c    Cholesky factor of the variance-covariance matrix.
c
c    Input, double precision MU(M), the mean vector.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(M,N), corresponding points associated
c    with the multivariate normal distribution.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision mu(m)
      double precision r(m,m)
      double precision rtu(m)
      integer seed
      double precision u(m)
      double precision x(m,n)
c
c  Create an M by N array U of samples of the standard normal distribution.
c
      call r8vec_normal_01 ( m * n, seed, u )
c
c  Compute X = MU + R' * U.
c
      do j = 1, n
        call r8vec_normal_01 ( m, seed, u )
        call r8mat_mtv ( m, m, r, u, rtu )
        do i = 1, m
          x(i,j) = mu(i) + rtu(i)
        end do
      end do

      return
      end
      subroutine normal_simple ( dim_num, n, seed, x )

c*********************************************************************72
c
cc NORMAL_SIMPLE creates normally distributed points.
c
c  Discussion:
c
c    The multivariate normal distribution has the form:
c
c      f(x) = (2*pi*det(V))**(-DIM_NUM/2) * exp(-0.5*(x-mu)'*inverse(V)*(x-mu))
c
c    where mu is the mean vector, and V is a positive definite symmetric
c    matrix called the variance-covariance matrix.
c
c    This routine implements the simplest version of a multivariate
c    normal distribution.  The variance-covariance matrix is the identity,
c    and the mean vector is entirely zero.  Thus, a sample on N points
c    is simply DIM_NUM*N scalar values generated under the univariate
c    normal distribution with zero mean and unit variance.
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
c    John Burkardt
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

      integer seed
      double precision x(dim_num,n)

      call r8vec_normal_01 ( dim_num*n, seed, x )

      return
      end
      subroutine polygon_centroid_2d ( n, v, centroid )

c*********************************************************************72
c
cc POLYGON_CENTROID_2D computes the centroid of a polygon in 2D.
c
c  Formula:
c
c    Denoting the centroid coordinates by CENTROID, then
c
c      CENTROID(1) = Integral ( Polygon interior ) x dx dy / Area ( Polygon )
c      CENTROID(2) = Integral ( Polygon interior ) y dx dy / Area ( Polygon ).
c
c    Green's theorem states that
c
c      Integral ( Polygon boundary ) ( M dx + N dy ) =
c      Integral ( Polygon interior ) ( dN/dx - dM/dy ) dx dy.
c
c    Using M = 0 and N = x * x / 2, we get:
c
c      CENTROID(1) = 0.5 * Integral ( Polygon boundary ) x * x dy,
c
c    which becomes
c
c      CENTROID(1) = 1/6 Sum ( 1 .le. I .le. N )
c        ( X(I+1) + X(I) ) * ( X(I) * Y(I+1) - X(I+1) * Y(I))
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
c    Graphics Gems IV, edited by Paul Heckbert,
c    AP Professional, 1994.
c
c  Parameters:
c
c    Input, integer N, the number of sides of the polygonal shape.
c
c    Input, double precision V(2,N), the coordinates of the vertices
c    of the shape.
c
c    Output, double precision CENTROID(2), the coordinates of the
c    centroid of the shape.
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
      centroid(1) = 0.0D+00
      centroid(2) = 0.0D+00

      do i = 1, n

        if ( i .lt. n ) then
          ip1 = i + 1
        else
          ip1 = 1
        end if

        temp = ( v(1,i) * v(2,ip1) - v(1,ip1) * v(2,i) )

        area = area + temp

        centroid(1) = centroid(1) + ( v(1,ip1) + v(1,i) ) * temp
        centroid(2) = centroid(2) + ( v(2,ip1) + v(2,i) ) * temp

      end do

      area = area / 2.0D+00
      centroid(1) = centroid(1) / ( 6.0D+00 * area )
      centroid(2) = centroid(2) / ( 6.0D+00 * area )

      return
      end
      function prime ( n )

c*********************************************************************72
c
cc PRIME returns any of the first PRIME_MAX prime numbers.
c
c  Discussion:
c
c    PRIME_MAX is 1600, and the largest prime stored is 13499.
c
c    Thanks to Bart Vandewoestyne for pointing out a typo, 18 February 2005.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Daniel Zwillinger,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996, pages 95-98.
c
c  Parameters:
c
c    Input, integer N, the index of the desired prime number.
c    In general, is should be true that 0 <= N <= PRIME_MAX.
c    N = -1 returns PRIME_MAX, the index of the largest prime available.
c    N = 0 is legal, returning PRIME = 1.
c
c    Output, integer PRIME, the N-th prime.  If N is out of range,
c    PRIME is returned as -1.
c
      implicit none

      integer prime_max
      parameter ( prime_max = 1600 )

      integer i
      integer n
      integer npvec(prime_max)
      integer prime

      save npvec

      data ( npvec(i), i = 1, 100 ) /
     &      2,    3,    5,    7,   11,   13,   17,   19,   23,   29,
     &     31,   37,   41,   43,   47,   53,   59,   61,   67,   71,
     &     73,   79,   83,   89,   97,  101,  103,  107,  109,  113,
     &    127,  131,  137,  139,  149,  151,  157,  163,  167,  173,
     &    179,  181,  191,  193,  197,  199,  211,  223,  227,  229,
     &    233,  239,  241,  251,  257,  263,  269,  271,  277,  281,
     &    283,  293,  307,  311,  313,  317,  331,  337,  347,  349,
     &    353,  359,  367,  373,  379,  383,  389,  397,  401,  409,
     &    419,  421,  431,  433,  439,  443,  449,  457,  461,  463,
     &    467,  479,  487,  491,  499,  503,  509,  521,  523,  541 /

      data ( npvec(i), i = 101, 200 ) /
     &    547,  557,  563,  569,  571,  577,  587,  593,  599,  601,
     &    607,  613,  617,  619,  631,  641,  643,  647,  653,  659,
     &    661,  673,  677,  683,  691,  701,  709,  719,  727,  733,
     &    739,  743,  751,  757,  761,  769,  773,  787,  797,  809,
     &    811,  821,  823,  827,  829,  839,  853,  857,  859,  863,
     &    877,  881,  883,  887,  907,  911,  919,  929,  937,  941,
     &    947,  953,  967,  971,  977,  983,  991,  997, 1009, 1013,
     &   1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069,
     &   1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151,
     &   1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223 /

      data ( npvec(i), i = 201, 300 ) /
     &   1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291,
     &   1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373,
     &   1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451,
     &   1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511,
     &   1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583,
     &   1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657,
     &   1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733,
     &   1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811,
     &   1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889,
     &   1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987 /

      data ( npvec(i), i = 301, 400 ) /
     &   1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053,
     &   2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129,
     &   2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213,
     &   2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287,
     &   2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357,
     &   2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423,
     &   2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531,
     &   2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617,
     &   2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687,
     &   2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741 /

      data ( npvec(i), i = 401, 500 ) /
     &   2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819,
     &   2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903,
     &   2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999,
     &   3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079,
     &   3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181,
     &   3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257,
     &   3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331,
     &   3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413,
     &   3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511,
     &   3517, 3527, 3529, 3533, 3539, 3541, 3547, 3557, 3559, 3571 /

      data ( npvec(i), i = 501, 600 ) /
     &   3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643,
     &   3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727,
     &   3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821,
     &   3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907,
     &   3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989,
     &   4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057,
     &   4073, 4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139,
     &   4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231,
     &   4241, 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297,
     &   4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409 /

      data ( npvec(i), i = 601, 700 ) /
     &   4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493,
     &   4507, 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583,
     &   4591, 4597, 4603, 4621, 4637, 4639, 4643, 4649, 4651, 4657,
     &   4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 4751,
     &   4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831,
     &   4861, 4871, 4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937,
     &   4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003,
     &   5009, 5011, 5021, 5023, 5039, 5051, 5059, 5077, 5081, 5087,
     &   5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179,
     &   5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279 /

      data ( npvec(i), i = 701, 800 ) /
     &   5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387,
     &   5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443,
     &   5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521,
     &   5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, 5639,
     &   5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693,
     &   5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791,
     &   5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849, 5851, 5857,
     &   5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939,
     &   5953, 5981, 5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053,
     &   6067, 6073, 6079, 6089, 6091, 6101, 6113, 6121, 6131, 6133 /

      data ( npvec(i), i = 801, 900 ) /
     &   6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221,
     &   6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301,
     &   6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367,
     &   6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473,
     &   6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571,
     &   6577, 6581, 6599, 6607, 6619, 6637, 6653, 6659, 6661, 6673,
     &   6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761,
     &   6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833,
     &   6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917,
     &   6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997 /

      data ( npvec(i), i = 901, 1000 ) /
     &   7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103,
     &   7109, 7121, 7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207,
     &   7211, 7213, 7219, 7229, 7237, 7243, 7247, 7253, 7283, 7297,
     &   7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411,
     &   7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499,
     &   7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561,
     &   7573, 7577, 7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643,
     &   7649, 7669, 7673, 7681, 7687, 7691, 7699, 7703, 7717, 7723,
     &   7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829,
     &   7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919 /

      data ( npvec(i), i = 1001, 1100 ) /
     &   7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017,
     &   8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111,
     &   8117, 8123, 8147, 8161, 8167, 8171, 8179, 8191, 8209, 8219,
     &   8221, 8231, 8233, 8237, 8243, 8263, 8269, 8273, 8287, 8291,
     &   8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387,
     &   8389, 8419, 8423, 8429, 8431, 8443, 8447, 8461, 8467, 8501,
     &   8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597,
     &   8599, 8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677,
     &   8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731, 8737, 8741,
     &   8747, 8753, 8761, 8779, 8783, 8803, 8807, 8819, 8821, 8831 /

      data ( npvec(i), i = 1101, 1200 ) /
     &   8837, 8839, 8849, 8861, 8863, 8867, 8887, 8893, 8923, 8929,
     &   8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011,
     &   9013, 9029, 9041, 9043, 9049, 9059, 9067, 9091, 9103, 9109,
     &   9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, 9187, 9199,
     &   9203, 9209, 9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283,
     &   9293, 9311, 9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377,
     &   9391, 9397, 9403, 9413, 9419, 9421, 9431, 9433, 9437, 9439,
     &   9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 9533,
     &   9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631,
     &   9643, 9649, 9661, 9677, 9679, 9689, 9697, 9719, 9721, 9733 /

      data ( npvec(i), i = 1201, 1300 ) /
     &   9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811,
     &   9817, 9829, 9833, 9839, 9851, 9857, 9859, 9871, 9883, 9887,
     &   9901, 9907, 9923, 9929, 9931, 9941, 9949, 9967, 9973,10007,
     &  10009,10037,10039,10061,10067,10069,10079,10091,10093,10099,
     &  10103,10111,10133,10139,10141,10151,10159,10163,10169,10177,
     &  10181,10193,10211,10223,10243,10247,10253,10259,10267,10271,
     &  10273,10289,10301,10303,10313,10321,10331,10333,10337,10343,
     &  10357,10369,10391,10399,10427,10429,10433,10453,10457,10459,
     &  10463,10477,10487,10499,10501,10513,10529,10531,10559,10567,
     &  10589,10597,10601,10607,10613,10627,10631,10639,10651,10657 /

      data ( npvec(i), i = 1301, 1400 ) /
     &  10663,10667,10687,10691,10709,10711,10723,10729,10733,10739,
     &  10753,10771,10781,10789,10799,10831,10837,10847,10853,10859,
     &  10861,10867,10883,10889,10891,10903,10909,10937,10939,10949,
     &  10957,10973,10979,10987,10993,11003,11027,11047,11057,11059,
     &  11069,11071,11083,11087,11093,11113,11117,11119,11131,11149,
     &  11159,11161,11171,11173,11177,11197,11213,11239,11243,11251,
     &  11257,11261,11273,11279,11287,11299,11311,11317,11321,11329,
     &  11351,11353,11369,11383,11393,11399,11411,11423,11437,11443,
     &  11447,11467,11471,11483,11489,11491,11497,11503,11519,11527,
     &  11549,11551,11579,11587,11593,11597,11617,11621,11633,11657 /

      data ( npvec(i), i = 1401, 1500 ) /
     &  11677,11681,11689,11699,11701,11717,11719,11731,11743,11777,
     &  11779,11783,11789,11801,11807,11813,11821,11827,11831,11833,
     &  11839,11863,11867,11887,11897,11903,11909,11923,11927,11933,
     &  11939,11941,11953,11959,11969,11971,11981,11987,12007,12011,
     &  12037,12041,12043,12049,12071,12073,12097,12101,12107,12109,
     &  12113,12119,12143,12149,12157,12161,12163,12197,12203,12211,
     &  12227,12239,12241,12251,12253,12263,12269,12277,12281,12289,
     &  12301,12323,12329,12343,12347,12373,12377,12379,12391,12401,
     &  12409,12413,12421,12433,12437,12451,12457,12473,12479,12487,
     &  12491,12497,12503,12511,12517,12527,12539,12541,12547,12553 /

      data ( npvec(i), i = 1501, 1600 ) /
     &  12569,12577,12583,12589,12601,12611,12613,12619,12637,12641,
     &  12647,12653,12659,12671,12689,12697,12703,12713,12721,12739,
     &  12743,12757,12763,12781,12791,12799,12809,12821,12823,12829,
     &  12841,12853,12889,12893,12899,12907,12911,12917,12919,12923,
     &  12941,12953,12959,12967,12973,12979,12983,13001,13003,13007,
     &  13009,13033,13037,13043,13049,13063,13093,13099,13103,13109,
     &  13121,13127,13147,13151,13159,13163,13171,13177,13183,13187,
     &  13217,13219,13229,13241,13249,13259,13267,13291,13297,13309,
     &  13313,13327,13331,13337,13339,13367,13381,13397,13399,13411,
     &  13417,13421,13441,13451,13457,13463,13469,13477,13487,13499 /

      if ( n .eq. -1 ) then
        prime = prime_max
      else if ( n .eq. 0 ) then
        prime = 1
      else if ( n .le. prime_max ) then
        prime = npvec(n)
      else
        prime = -1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PRIME - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal prime index N = ', n
        write ( *, '(a,i8)' )
     &    '  N should be between 1 and PRIME_MAX =', prime_max
        stop
      end if

      return
      end
      function r8_normal_01 ( seed )

c*********************************************************************72
c
cc R8_NORMAL_01 returns a unit pseudonormal R8.
c
c  Discussion:
c
c    This routine uses the Box Muller method.
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
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision R8_NORMAL_01, a sample of the standard normal PDF.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r1
      double precision r2
      double precision r8_normal_01
      double precision r8_uniform_01
      integer seed
      double precision x

      r1 = r8_uniform_01 ( seed )
      r2 = r8_uniform_01 ( seed )
      x = sqrt ( -2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )

      r8_normal_01 = x

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
      subroutine r8mat_mtv ( m, n, a, x, y )

c*****************************************************************************80
c
cc R8MAT_MTV multiplies a transposed matrix times a vector
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of
c    the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(M), the vector to be multiplied by A.
c
c    Output, double precision Y(N), the product A'*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(m)
      double precision y(n)
      double precision y1(n)

      do i = 1, n
        y1(i) = 0.0D+00
        do j = 1, m
          y1(i) = y1(i) + a(j,i) * x(j)
        end do
      end do

      do i = 1, n
        y(i) = y1(i)
      end do

      return
      end
      subroutine r8mat_mv ( m, n, a, x, y )

c*********************************************************************72
c
cc R8MAT_MV multiplies a matrix times a vector.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    In FORTRAN90, this operation can be more efficiently carried
c    out by the command
c
c      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision Y(M), the product A*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(n)
      double precision y(m)
      double precision y1(m)

      do i = 1, m
        y1(i) = 0.0D+00
        do j = 1, n
          y1(i) = y1(i) + a(i,j) * x(j)
        end do
      end do

      do i = 1, m
        y(i) = y1(i)
      end do

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
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
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
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine r8po_fa ( n, a, info )

c*********************************************************************72
c
cc R8PO_FA factors an R8PO matrix.
c
c  Discussion:
c
c    The R8PO storage format is used for a symmetric positive definite 
c    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
c    upper triangular matrix, so it will be in R8GE storage format.)
c
c    Only the diagonal and upper triangle of the square array are used.
c    This same storage scheme is used when the matrix is factored by
c    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
c    is set to zero.
c
c    R8PO storage is used by LINPACK and LAPACK.
c
c    The positive definite symmetric matrix A has a Cholesky factorization
c    of the form:
c
c      A = R' * R
c
c    where R is an upper triangular matrix with positive elements on
c    its diagonal.  This routine overwrites the matrix A with its
c    factor R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2003
c
c  Author:
c
c    Original FORTRAN77 version by Dongarra, Bunch, Moler, Stewart.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input/output, double precision A(N,N).
c    On input, the matrix in R8PO storage.
c    On output, the Cholesky factor R in R8GE storage.
c
c    Output, integer INFO, error flag.
c    0, normal return.
c    K, error condition.  The principal minor of order K is not
c    positive definite, and the factorization was not completed.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer info
      integer j
      integer k
      double precision r8vec_dot_product
      double precision s
      double precision t

      do j = 1, n

        do k = 1, j - 1
          t = r8vec_dot_product ( k - 1, a(1,k), a(1,j) )
          a(k,j) = ( a(k,j) - t ) / a(k,k)
        end do

        t = 0.0D+00
        do i = 1, j - 1
          t = t + a(i,j)**2
        end do

        s = a(j,j) - t

        if ( s .le. 0.0D+00 ) then
          info = j
          return
        end if

        a(j,j) = sqrt ( s )

      end do

      info = 0
c
c  Since the Cholesky factor is stored in R8GE format, be sure to
c  zero out the lower triangle.
c
      do i = 1, n
        do j = 1, i-1
          a(i,j) = 0.0D+00
        end do
      end do

      return
      end
      subroutine r8po_sl ( n, a_lu, b )

c*********************************************************************72
c
cc R8PO_SL solves an R8PO system factored by R8PO_FA.
c
c  Discussion:
c
c    The R8PO storage format is used for a symmetric positive definite 
c    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
c    upper triangular matrix, so it will be in R8GE storage format.)
c
c    Only the diagonal and upper triangle of the square array are used.
c    This same storage scheme is used when the matrix is factored by
c    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
c    is set to zero.
c
c    R8PO storage is used by LINPACK and LAPACK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    04 March 1999
c
c  Author:
c
c    Original FORTRAN77 version by Dongarra, Bunch, Moler, Stewart.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A_LU(N,N), the Cholesky factor from R8PO_FA.
c
c    Input/output, double precision B(N).
c    On input, the right hand side.
c    On output, the solution vector.
c
      implicit none

      integer n

      double precision a_lu(n,n)
      double precision b(n)
      integer i
      integer k
      double precision r8vec_dot_product
      double precision t
c
c  Solve R' * y = b.
c
      do k = 1, n
        b(k) = ( b(k) - r8vec_dot_product ( k - 1, b, a_lu(1,k) ) ) 
     &    / a_lu(k,k)
      end do
c
c  Solve R * x = y.
c
      do k = n, 1, -1
        b(k) = b(k) / a_lu(k,k)
        do i = 1, k - 1
          b(i) = b(i) - a_lu(i,k) * b(k)
        end do
      end do

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
c      R8VEC_NORM = sqrt ( sum ( 1 .le. I .le. N ) A(I)^2 ).
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
      character ( len = * ) title

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
      subroutine scale_from_simplex01 ( dim_num, n, t, x )

c*********************************************************************72
c
cc SCALE_FROM_SIMPLEX01 rescales data from a unit to non-unit simplex.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
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
c    Input, double precision T(DIM_NUM,0:DIM_NUM), the coordinates of the
c    DIM_NUM+1 points that define the simplex.  T(1:DIM_NUM,0) corresponds
c    to the origin, and T(1:DIM_NUM,J) will be the image of the J-th unit
c    coordinate vector.
c
c    Input/output, double precision X(DIM_NUM,N), the data to be modified.
c
      implicit none

      integer dim_num
      integer n

      double precision a(dim_num,dim_num)
      double precision ax(dim_num)
      integer i
      integer j
      double precision t(dim_num,0:dim_num)
      double precision x(dim_num,n)

      do j = 1, dim_num
        do i = 1, dim_num
          a(i,j) = t(i,j)
        end do
      end do

      do j = 1, dim_num
        do i = 1, dim_num
          a(i,j) = a(i,j) - t(i,0)
        end do
      end do

      do j = 1, n
        call r8mat_mv ( dim_num, dim_num, a, x(1,j), ax )
        do i = 1, dim_num
          x(i,j) = t(i,0) + ax(i)
        end do
      end do

      return
      end
      subroutine scale_to_ball01 ( dim_num, n, x )

c*********************************************************************72
c
cc SCALE_TO_BALL01 translates and rescales data to fit within the unit ball.
c
c  Discussion:
c
c    Completely arbitrary input data is given.
c
c    The average of the data is computed, and taken as the coordinates
c    of the center C of a sphere.  The radius R of that sphere is the
c    distance from the center to the furthest point in the data set.
c
c    Then each point is transformed to the ball of center 0 and radius
c    1 by subtracting C and dividing by R:
c
c      X(1:DIM_NUM,J) -> ( X(1:DIM_NUM,J) - C(1:DIM_NUM) ) / R
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, double precision X(DIM_NUM,N), the data to be modified.
c
      implicit none

      integer dim_num
      integer n

      integer i
      integer j
      double precision r(n)
      double precision r8vec_sum
      double precision scale
      double precision t
      double precision x(dim_num,n)
      double precision xave(dim_num)
c
c  Determine the center.
c
      do i = 1, dim_num
        t = 0.0D+00
        do j = 1, n
          t = t + x(i,j)
        end do
        xave(i) = t / dble ( n )
      end do
c
c  Determine SCALE, the maximum distance of any point X from the center.
c
      do j = 1, n
        t = 0.0D+00
        do i = 1, dim_num
          t = t + ( x(i,j) - xave(i) )**2
        end do
        r(j) = t
      end do

      scale = 0.0D+00
      do i = 1, n
        scale = max ( scale, r(i) )
      end do
      scale = sqrt ( scale )
c
c  Dividing all values by SCALE will guarantee that every point is
c  inside the unit sphere, and one point at least is ON the sphere.
c
      if ( 0.0D+00 .lt. scale ) then
        do i = 1, dim_num
          do j = 1, n
            x(i,j) = ( x(i,j) - xave(i) ) / scale
          end do
        end do
      else
        do j = 1, n
          do i = 1, dim_num
            x(i,j) = 0.0D+00
          end do
        end do
      end if

      return
      end
      subroutine scale_to_block01 ( dim_num, n, x )

c*********************************************************************72
c
cc SCALE_TO_BLOCK01 translates and rescales data to fit in the unit block.
c
c  Discussion:
c
c    The minimum and maximum coordinate values M1(I) and M2(I) are
c    determined, and the maximum of M2(I) - M1(I) is used to scale
c    all the coordinates by the same factor.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, double precision X(DIM_NUM,N), the data to be modified.
c
      implicit none

      integer dim_num
      integer n

      integer i
      integer j
      double precision x(dim_num,n)
      double precision xmax(dim_num)
      double precision xmin(dim_num)
      double precision xrange
      double precision xrange2
c
c  Determine the extremes in each dimension.
c
      xrange = 0.0D+00
      do i = 1, dim_num
        xmin(i) = x(i,1)
        xmax(i) = x(i,1)
        do j = 2, n
          xmin(i) = min ( xmin(i), x(i,j) )
          xmax(i) = max ( xmax(i), x(i,j) )
        end do
        xrange = max ( xrange, xmax(i) - xmin(i) )
      end do
c
c  Extend all the extremes so that the range is the same in each dimension.
c
      do i = 1, dim_num
        xrange2 = xrange - ( xmax(i) - xmin(i) )
        xmax(i) = xmax(i) + 0.5D+00 * xrange2
        xmin(i) = xmin(i) - 0.5D+00 * xrange2
      end do
c
c  Now map the data to [0,1], using a single dilation factor for all dimensions.
c
      if ( 0.0D+00 .eq. xrange ) then

        do j = 1, n
          do i = 1, dim_num
            x(i,j) = 0.5D+00
          end do
        end do

      else

        do i = 1, dim_num
          do j = 1, n
            x(i,j) = ( x(i,j) - xmin(i) ) / xrange
          end do
        end do

      end if

      return
      end
      subroutine scale_to_cube01 ( dim_num, n, x )

c*********************************************************************72
c
cc SCALE_TO_CUBE01 translates and rescales data to the unit hypercube.
c
c  Discussion:
c
c    In each coordinate dimension I, the minimum and maximum coordinate
c    values M1(I) and M2(I) are determined.
c
c    Then, in each coordinate, the points are rescaled as
c
c      X(I) -> ( X(I) - M1(I) ) / ( M2(I) - M1(I) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, double precision X(DIM_NUM,N), the data to be modified.
c
      implicit none

      integer dim_num
      integer n

      integer i
      integer j
      double precision x(dim_num,n)
      double precision xmax(dim_num)
      double precision xmin(dim_num)

      do i = 1, dim_num
        xmin(i) = x(i,1)
        xmax(i) = x(i,1)
        do j = 2, n
          xmin(i) = min ( xmin(i), x(i,j) )
          xmax(i) = max ( xmax(i), x(i,j) )
        end do
      end do

      do i = 1, dim_num
        if ( 0.0D+00 .lt. xmax(i) - xmin(i) ) then
          do j = 1, n
            x(i,j) = ( x(i,j) - xmin(i) ) / ( xmax(i) - xmin(i) )
          end do
        else
          do j = 1, n
            x(i,j) = 0.5D+00
          end do
        end if
      end do

      return
      end
      subroutine stri_angles_to_area ( r, a, b, c, area )

c*********************************************************************72
c
cc STRI_ANGLES_TO_AREA computes the area of a spherical triangle.
c
c  Discussion:
c
c    A sphere centered at 0 in 3D satisfies the equation:
c
c      X*X + Y*Y + Z*Z = R*R
c
c    A spherical triangle is specified by three points on the surface
c    of the sphere.
c
c    The area formula is known as Girard's formula.
c
c    The area of a spherical triangle is:
c
c      AREA = ( A + B + C - PI ) * R*R
c
c    where A, B and C are the (surface) angles of the triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the sphere.
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
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
c
c  Apply Girard's formula.
c
      area = r * r * ( a + b + c - pi )

      return
      end
      subroutine stri_sides_to_angles ( r, as, bs, cs, a, b, c )

c*********************************************************************72
c
cc STRI_SIDES_TO_ANGLES computes spherical triangle angles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the sphere.
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
      double precision r
      double precision ssu
      double precision tan_a2
      double precision tan_b2
      double precision tan_c2

      asu = as / r
      bsu = bs / r
      csu = cs / r
      ssu = ( asu + bsu + csu ) / 2.0D+00

      tan_a2 = sqrt ( ( sin ( ssu - bsu ) * sin ( ssu - csu ) ) / 
     &                ( sin ( ssu ) * sin ( ssu - asu )     ) )

      a = 2.0D+00 * atan ( tan_a2 )

      tan_b2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - csu ) ) / 
     &                ( sin ( ssu ) * sin ( ssu - bsu )     ) )

      b = 2.0D+00 * atan ( tan_b2 )

      tan_c2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - bsu ) ) / 
     ^                ( sin ( ssu ) * sin ( ssu - csu )     ) )

      c = 2.0D+00 * atan ( tan_c2 )

      return
      end
      subroutine stri_vertices_to_sides ( r, v1, v2, v3, as, bs, cs )

c*********************************************************************72
c
cc STRI_VERTICES_TO_SIDES_3D computes spherical triangle sides.
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
c    21 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the sphere.
c
c    Input, double precision V1(3), V2(3), V3(3), the vertices of the spherical
c    triangle.
c
c    Output, double precision AS, BS, CS, the (geodesic) length of the sides
c    of the triangle.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )

      double precision arc_cosine
      double precision as
      double precision bs
      double precision cs
      double precision r
      double precision r8vec_dot_product
      double precision t
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)

      t = r8vec_dot_product ( dim_num, v2, v3 ) / r**2
      as = r * arc_cosine ( t )
      t = r8vec_dot_product ( dim_num, v3, v1 ) / r**2
      bs = r * arc_cosine ( t )
      t = r8vec_dot_product ( dim_num, v1, v2 ) / r**2
      cs = r * arc_cosine ( t )

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
      subroutine triangle_area_2d ( t, area )

c*********************************************************************72
c
cc TRIANGLE_AREA_2D computes the area of a triangle in 2D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision AREA, the absolute area of the triangle.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )

      double precision area
      double precision t(dim_num,3)

      area = 0.5D+00 * abs ( 
     &    t(1,1) * ( t(2,2) - t(2,3) ) 
     &  + t(1,2) * ( t(2,3) - t(2,1) ) 
     &  + t(1,3) * ( t(2,1) - t(2,2) ) )

      return
      end
      subroutine tuple_next_fast ( m, n, rank, base, x )

c*********************************************************************72
c
cc TUPLE_NEXT_FAST computes the next element of a tuple space, "fast".
c
c  Discussion:
c
c    The elements are N vectors.  Each entry is constrained to lie
c    between 1 and M.  The elements are produced one at a time.
c    The first element is
c      (1,1,...,1)
c    and the last element is
c      (M,M,...,M)
c    Intermediate elements are produced in lexicographic order.
c
c    This code was written as a possibly faster version of TUPLE_NEXT.
c
c  Example:
c
c    N = 2,
c    M = 3
c
c    INPUT        OUTPUT
c    -------      -------
c    Rank          X
c    ----          ----
c   -1            -1 -1
c
c    0             1  1
c    1             1  2
c    2             1  3
c    3             2  1
c    4             2  2
c    5             2  3
c    6             3  1
c    7             3  2
c    8             3  3
c    9             1  1
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
c  Parameters:
c
c    Input, integer M, the maximum entry in any component.
c    M must be greater than 0.
c
c    Input, integer N, the number of components.
c    N must be greater than 0.
c
c    Input, integer RANK, indicates the rank of the tuple.
c    Typically, 0 .le. RANK .lt. N**M.  Values of RANK greater than
c    N**M are legal and meaningful; they are equivalent to the
c    corresponding value mod (N**M).  If RANK .lt. 0, this indicates
c    that this is the first call for the given values of (M,N).
c    Initialization is done, and X is set to a dummy value.
c
c    Workspace, integer BASE(N).
c
c    Output, integer X(N), the next tuple, or a dummy value if
c    initialization has just been done.
c
      implicit none

      integer n

      integer base(n)
      integer i
      integer m
      integer rank
      integer x(n)

      if ( rank .lt. 0 ) then

        if ( m .le. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TUPLE_NEXT_FAST - Fatal error!'
          write ( *, '(a)' ) '  The value M .le. 0 is not allowed.'
          write ( *, '(a,i8)' ) '  M = ', m
          stop
        end if

        if ( n .le. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TUPLE_NEXT_FAST - Fatal error!'
          write ( *, '(a)' ) '  The value N .le. 0 is not allowed.'
          write ( *, '(a,i8)' ) '  N = ', n
          stop
        end if

        base(n) = 1
        do i = n-1, 1, -1
          base(i) = base(i+1) * m
        end do

        do i = 1, n
          x(i) = -1
        end do

      else

        do i = 1, n
          x(i) = mod ( rank / base(i), m ) + 1
        end do

      end if

      return
      end
      subroutine uniform_in_annulus ( pc, r1, r2, n, seed, p )

c*********************************************************************72
c
cc UNIFORM_IN_ANNULUS samples a circular annulus.
c
c  Discussion:
c
c    A circular annulus with center PC, inner radius R1 and
c    outer radius R2, is the set of points P so that
c
c      R1^2 .le. (P(1)-PC(1))^2 + (P(2)-PC(2))^2 .le. R2^2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Peter Shirley,
c    Nonuniform Random Point Sets Via Warping,
c    Graphics Gems, Volume III,
c    edited by David Kirk,
c    AP Professional, 1992,
c    ISBN: 0122861663,
c    LC: T385.G6973.
c
c  Parameters:
c
c    Input, double precision PC(2), the center.
c
c    Input, double precision R1, R2, the inner and outer radii.
c
c    Input, integer N, the number of points to generate.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(2,N), sample points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      integer i
      integer j
      double precision p(dim_num,n)
      double precision pc(dim_num)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(n)
      double precision r1
      double precision r2
      double precision r8_uniform_01
      integer seed
      double precision theta(n)
      double precision theta1
      double precision theta2
      double precision u(n)
      double precision v(n)

      call r8vec_uniform_01 ( n, seed, u )

      do i = 1, n
        theta(i) = u(i) * 2.0D+00 * pi
      end do

      call r8vec_uniform_01 ( n, seed, v )

      do i = 1, n
        r(i) = sqrt ( ( 1.0D+00 - v(i) ) * r1 * r1 
     &           +                v(i)   * r2 * r2 )
      end do

      do i = 1, n
        p(1,i) = pc(1) + r(i) * cos ( theta(i) )
        p(2,i) = pc(2) + r(i) * sin ( theta(i) )
      end do

      return
      end
      subroutine uniform_in_annulus_accept ( pc, r1, r2, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_ANNULUS_ACCEPT accepts points in an annulus.
c
c  Discussion:
c
c    A circular annulus with center PC, inner radius R1 and
c    outer radius R2, is the set of points P so that
c
c      R1**2 .le. (P(1)-PC(1))**2 + (P(2)-PC(2))**2 .le. R2**2
c
c    The acceptance/rejection method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision PC(2), the center.
c
c    Input, double precision R1, R2, the inner and outer radii.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,N), the points.
c
      implicit none

      integer n
      integer dim_num
      parameter ( dim_num = 2 )

      integer i
      integer j
      double precision pc(dim_num)
      double precision r1
      double precision r2
      double precision r8vec_norm
      integer seed
      double precision u(dim_num)
      double precision x(dim_num,n)

      if ( r2 .le. r1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_IN_ANNULUS_ACCEPT - Fatal error!'
        write ( *, '(a)' ) '  R2 .le. R1.'
        return
      end if
c
c  Generate points in a square of "radius" R2.
c  Accept those points which lie inside the circle of radius R2, and outside
c  the circle of radius R1.
c
      do j = 1, n

10      continue

          call r8vec_uniform_01 ( dim_num, seed, u )

          do i = 1, dim_num
            u(i) = ( 2.0D+00 * u(i) - 1.0D+00 ) * r2
          end do

          if ( r1 .le. ( r8vec_norm ( dim_num, u ) ) .and. 
     &      r8vec_norm ( dim_num, u ) .le. r2 ) then
            go to 20
          end if

        go to 10

20      continue

        do i = 1, dim_num
          x(i,j) = pc(i) + u(i)
        end do

      end do

      return
      end
      subroutine uniform_in_annulus_sector ( pc, r1, r2, theta1, 
     &  theta2, n, seed, p )

c*********************************************************************72
c
cc UNIFORM_IN_ANNULUS_SECTOR samples an annular sector in 2D.
c
c  Discussion:
c
c    An annular sector with center PC, inner radius R1 and
c    outer radius R2, and angles THETA1, THETA2, is the set of points
c    P so that
c
c      R1**2 .le. (P(1)-PC(1))**2 + (P(2)-PC(2))**2 .le. R2**2
c
c    and
c
c      THETA1 .le. THETA ( P - PC ) .le. THETA2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Peter Shirley,
c    Nonuniform Random Point Sets Via Warping,
c    Graphics Gems, Volume III,
c    edited by David Kirk,
c    AP Professional, 1992,
c    ISBN: 0122861663,
c    LC: T385.G6973.
c
c  Parameters:
c
c    Input, double precision PC(2), the center.
c
c    Input, double precision R1, R2, the inner and outer radii.
c
c    Input, double precision THETA1, THETA2, the angles.
c
c    Input, integer N, the number of points to generate.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(2,N), sample points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      integer i
      double precision p(2,n)
      double precision pc(dim_num)
      double precision r(1:n)
      double precision r1
      double precision r2
      double precision r8_uniform_01
      integer seed
      double precision theta(1:n)
      double precision theta1
      double precision theta2
      double precision u(1:n)
      double precision v(1:n)

      call r8vec_uniform_01 ( n, seed, u )

      do i = 1, n
        theta(i) = ( 1.0D+00 - u(i) ) * theta1 
     &              +          u(i)   * theta2
      end do

      call r8vec_uniform_01 ( n, seed, v )

      do i = 1, n
        r(i) = sqrt ( ( 1.0D+00 - v(i) ) * r1 * r1 
     &           +                v(i)   * r2 * r2 )
      end do

      do i = 1, n
        p(1,i) = pc(1) + r(i) * cos ( theta(i) )
        p(2,i) = pc(2) + r(i) * sin ( theta(i) )
      end do

      return
      end
      subroutine uniform_in_circle01_map ( n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_CIRCLE01_MAP maps uniform points into the unit circle.
c
c  Discussion:
c
c    The unit circle has center at the origin, and radius 1.
c
c    This routine is valid for spatial dimension DIM_NUM = 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space
c    (which must be 2).
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
      parameter ( dim_num = 2 )
      integer n

      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(n)
      integer seed
      double precision t(n)
      double precision x(dim_num,n)

      call r8vec_uniform_01 ( n, seed, r )
      do i = 1, n
        r(i) = sqrt ( r(i) )
      end do

      call r8vec_uniform_01 ( n, seed, t )
      do i = 1, n
        t(i) = 2.0D+00 * pi * t(i)
      end do

      do i = 1, n
        x(1,i) = r(i) * cos ( t(i) )
        x(2,i) = r(i) * sin ( t(i) )
      end do

      return
      end
      subroutine uniform_in_cube01 ( dim_num, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_CUBE01 creates uniform points in the unit hypercube.
c
c  Discussion:
c
c    The unit hypercube is defined as points whose components are between
c    0 and 1.
c
c    This routine is valid for any spatial dimension DIM_NUM.
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
c    John Burkardt
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

      integer seed
      double precision x(dim_num,n)

      call r8vec_uniform_01 ( dim_num*n, seed, x )

      return
      end
      subroutine uniform_in_ellipsoid_map ( dim_num, n, a, r, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_ELLIPSOID_MAP maps uniform points into an ellipsoid.
c
c  Discussion:
c
c    The points X in the ellipsoid are described by a DIM_NUM by DIM_NUM
c    positive definite symmetric matrix A, and a "radius" R, such that
c
c      X' * A * X .le. R * R
c
c    The algorithm computes the Cholesky factorization of A:
c
c      A = U' * U.
c
c    A set of uniformly random points Y is generated, satisfying:
c
c      Y' * Y .le. R * R.
c
c    The appropriate points in the ellipsoid are found by solving
c
c      U * X = Y
c
c    Thanks to Dr Karl-Heinz Keil for pointing out that the original
c    coding was actually correct only if A was replaced by its inverse.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
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
c    Input, double precision A(DIM_NUM,DIM_NUM), the matrix that describes
c    the ellipsoid.
c
c    Input, double precision R, the right hand side of the ellipsoid equation.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,N), the points.
c
      implicit none

      integer dim_num
      integer n

      double precision a(dim_num,dim_num)
      integer i
      integer info
      integer j
      double precision r
      integer seed
      double precision u(dim_num,dim_num)
      double precision x(dim_num,n)
c
c  Get the Cholesky factor U.
c
      do j = 1, dim_num
        do i = 1, dim_num
          u(i,j) = a(i,j)
        end do
      end do

      call r8po_fa ( dim_num, u, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_IN_ELLIPSOID_MAP - Fatal error!'
        write ( *, '(a)' ) '  R8PO_FA reports that the matrix A '
        write ( *, '(a)' ) '  is not positive definite symmetric.'
        stop
      end if
c
c  Get the points Y that satisfy Y' * Y .le. R * R.
c
      call uniform_in_sphere01_map ( dim_num, n, seed, x )

      do j = 1, n
        do i = 1, dim_num
          x(i,j) = r * x(i,j)
        end do
      end do
c
c  Solve U * X = Y.
c
      do j = 1, n
        call r8po_sl ( dim_num, u, x(1,j) )
      end do

      return
      end
      subroutine uniform_in_parallelogram_map ( v1, v2, v3, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_PARALLELOGRAM_MAP maps uniform points into a parallelogram.
c
c  Discussion:
c
c    The parallelogram is defined by three vertices, V1, V2 and V3.
c    The missing vertex V4 is equal to V2+V3-V1.
c
c    This routine is valid for spatial dimension DIM_NUM = 2.
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
c    John Burkardt
c
c  Reference:
c
c    Greg Turk,
c    Generating Random Points in a Triangle,
c    in Graphics Gems,
c    edited by Andrew Glassner,
c    AP Professional, 1990, pages 24-28.
c
c  Parameters:
c
c    Input, double precision V1(2), V2(2), V3(2), the vertices.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      integer i
      integer j
      double precision r(2)
      integer seed
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)
      double precision x(dim_num,n)

      do j = 1, n

        call r8vec_uniform_01 ( 2, seed, r )

        do i = 1, dim_num
          x(i,j) = ( 1.0D+00 - r(1) - r(2) ) * v1(i) 
     &                       + r(1)          * v2(i) 
     &                              + r(2)   * v3(i)
        end do

      end do

      return
      end
      subroutine uniform_in_polygon_map ( nv, v, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_POLYGON_MAP maps uniform points into a polygon.
c
c  Discussion:
c
c    If the polygon is regular, or convex, or at least star-shaped,
c    this routine will work.
c
c    This routine assumes that all points between the centroid and
c    any point on the boundary lie within the polygon.
c
c    This routine is valid for spatial dimension DIM_NUM = 2.
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
c    clockwise or counterclockwise order.
c
c    Input, integer N, the number of points to create.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      integer nv

      double precision area(nv)
      double precision area_percent
      double precision centroid(dim_num)
      integer i
      integer i2
      integer ip1
      integer j
      integer k
      double precision r(2)
      double precision r8_uniform_01
      double precision r8vec_sum
      integer seed
      double precision t(dim_num,3)
      double precision x(dim_num,n)
      double precision v(dim_num,nv)
c
c  Find the centroid.
c
      call polygon_centroid_2d ( nv, v, centroid )
c
c  Determine the areas of each triangle.
c
      do i = 1, nv

        if ( i .lt. nv ) then
          ip1 = i + 1
        else
          ip1 = 1
        end if

        t(1,1) = v(1,i)
        t(2,1) = v(2,i)
        t(1,2) = v(1,ip1)
        t(2,2) = v(2,ip1)
        t(1,3) = centroid(1)
        t(2,3) = centroid(2)

        call triangle_area_2d ( t, area(i) )

      end do
c
c  Normalize the areas.
c
      do i = 1, nv
        area(i) = area(i) / sum ( area(1:nv) )
      end do
c
c  Replace each area by the sum of itself and all previous ones.
c
      do i = 2, nv
        area(i) = area(i) + area(i-1)
      end do

      do j = 1, n
c
c  Choose a triangle at random, based on areas.
c
        area_percent = r8_uniform_01 ( seed )

        do k = 1, nv

          if ( area_percent .le. area(k) ) then
            i = k
            go to 10
          end if

        end do

10      continue
c
c  Now choose a point at random in the triangle.
c
        if ( i .lt. nv ) then
          ip1 = i + 1
        else
          ip1 = 1
        end if

        call r8vec_uniform_01 ( dim_num, seed, r )

        if ( 1.0D+00 .lt. r8vec_sum ( dim_num, r ) ) then
          do i2 = 1, dim_num
            r(i2) = 1.0D+00 - r(i2)
          end do
        end if

        do i2 = 1, dim_num
          x(i2,j) = ( 1.0D+00 - r(1) - r(2) ) * v(i2,i) 
     &                        + r(1)          * v(i2,ip1) 
     &                               + r(2)   * centroid(i2)
        end do

      end do

      return
      end
      subroutine uniform_in_sector_map ( r1, r2, t1, t2, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_SECTOR_MAP maps uniform points into a circular sector.
c
c  Discussion:
c
c    The sector lies between circles with center at 0 and radius R1 and R2,
c    and between rays from the center at the angles T1 and T2.
c
c    This routine is valid for spatial dimension DIM_NUM = 2.
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
c    John Burkardt
c
c  Reference:
c
c    Peter Shirley,
c    Nonuniform Random Point Sets Via Warping,
c    Graphics Gems, Volume III,
c    edited by David Kirk,
c    AP Professional, 1992,
c    ISBN: 0122861663,
c    LC: T385.G6973.
c
c  Parameters:
c
c    Input, double precision R1, R2, the two radii.
c
c    Input, double precision T1, T2, the two angles, which should
c    be measured in radians, with T1 .lt. T2.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      integer i
      double precision r(n)
      double precision r1
      double precision r2
      integer seed
      double precision t(n)
      double precision t1
      double precision t2
      double precision u(n)
      double precision v(n)
      double precision x(dim_num,n)

      call r8vec_uniform_01 ( n, seed, u )
      call r8vec_uniform_01 ( n, seed, v )

      do i = 1, n
        t(i) =        ( 1.0D+00 - u(i) ) * t1    + u(i) * t2
      end do

      do i = 1, n
        r(i) = sqrt ( ( 1.0D+00 - v(i) ) * r1**2 + v(i) * r2**2 )
      end do

      do i = 1, n
        x(1,i) = r(i) * cos ( t(i) )
        x(2,i) = r(i) * sin ( t(i) )
      end do

      return
      end
      subroutine uniform_in_simplex01_map ( dim_num, point_num, seed, 
     &  x )

c*********************************************************************72
c
cc UNIFORM_IN_SIMPLEX01_MAP maps uniform points into the unit simplex.
c
c  Discussion:
c
c    The interior of the unit DIM_NUM-dimensional simplex is the set of
c    points X(1:DIM_NUM) such that each X(I) is nonnegative, and
c    sum(X(1:DIM_NUM)) .le. 1.
c
c    This routine is valid for any spatial dimension DIM_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
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
c    Input, integer POINT_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,POINT_NUM), the points.
c
      implicit none

      integer dim_num
      integer point_num

      double precision e(dim_num+1)
      integer i
      integer j
      double precision r8vec_sum
      integer seed
      double precision x(dim_num,point_num)
c
c  The construction begins by sampling DIM_NUM+1 points from the
c  exponential distribution with parameter 1.
c
      do j = 1, point_num

        call r8vec_uniform_01 ( dim_num + 1, seed, e )

        do i = 1, dim_num + 1
          e(i) = -log ( e(i) )
        end do

        do i = 1, dim_num
          x(i,j) = e(i) / r8vec_sum ( dim_num + 1, e )
        end do

      end do

      return
      end
      subroutine uniform_in_sphere01_map ( dim_num, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_SPHERE01_MAP maps uniform points into the unit sphere.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    This routine is valid for any spatial dimension DIM_NUM.
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

      double precision exponent
      integer i
      integer j
      double precision norm
      double precision r
      double precision r8_uniform_01
      double precision r8vec_norm
      integer seed
      double precision x(dim_num,n)

      exponent = 1.0D+00 / dble ( dim_num )

      do j = 1, n
c
c  Fill a vector with normally distributed values.
c
        call r8vec_normal_01 ( dim_num, seed, x(1,j) )
c
c  Compute the length of the vector.
c
        norm = r8vec_norm ( dim_num, x(1,j) )
c
c  Normalize the vector.
c
        do i = 1, dim_num
          x(i,j) = x(i,j) / norm
        end do
c
c  Now compute a value to map the point ON the sphere INTO the sphere.
c
        r = r8_uniform_01 ( seed )

        do i = 1, dim_num
          x(i,j) = r ** exponent * x(i,j)
        end do

      end do

      return
      end
      subroutine uniform_in_tetrahedron ( v, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_TETRAHEDRON returns uniform points in a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Claudio Rocchini, Paolo Cignoni,
c    Generating Random Points in a Tetrahedron,
c    Journal of Graphics Tools,
c    Volume 5, Number 5, 2000, pages 9-12.
c
c  Parameters:
c
c    Input, double precision V(3,4), the vertices of the tetrahedron.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(3,N), the points.
c
      implicit none

      integer n

      double precision c(4)
      integer j
      integer seed
      double precision t
      double precision v(3,4)
      double precision x(3,n)

      do j = 1, n

        call r8vec_uniform_01 ( 3, seed, c )

        if ( 1.0D+00 .lt. c(1) + c(2) ) then
          c(1) = 1.0D+00 - c(1)
          c(2) = 1.0D+00 - c(2)
        end if

        if ( 1.0D+00 .lt. c(2) + c(3) ) then
          t = c(3)
          c(3) = 1.0D+00 - c(1) - c(2)
          c(2) = 1.0D+00 - t
        else if ( 1.0D+00 .lt. c(1) + c(2) + c(3) ) then
           t = c(3)
           c(3) = c(1) + c(2) + c(3) - 1.0D+00
           c(1) = 1.0D+00 - c(2) - t
        end if

        c(4) = 1.0D+00 - c(1) - c(2) - c(3)
c
c  C(1:4) are the barycentric coordinates of the point.
c
        call r8mat_mv ( 3, 4, v, c, x(1,j) )

      end do

      return
      end
      subroutine uniform_in_triangle_map1 ( v1, v2, v3, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_TRIANGLE_MAP1 maps uniform points into a triangle.
c
c  Discussion:
c
c    The triangle is defined by three vertices.  This routine
c    uses Turk's rule #1.
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
c    John Burkardt
c
c  Reference:
c
c    Greg Turk,
c    Generating Random Points in a Triangle,
c    in Graphics Gems,
c    edited by Andrew Glassner,
c    AP Professional, 1990, pages 24-28.
c
c  Parameters:
c
c    Input, double precision V1(2), V2(2), V3(2), the vertices.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      double precision a
      double precision b
      double precision c
      integer i
      integer j
      double precision r(3)
      integer seed
      double precision x(dim_num,n)
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)
c
c  Generate the points using Turk's rule 1.
c
      do j = 1, n

        call r8vec_uniform_01 ( 2, seed, r )

        a = 1.0D+00            - sqrt ( r(2) )
        b = ( 1.0D+00 - r(1) ) * sqrt ( r(2) )
        c =             r(1)   * sqrt ( r(2) )

        do i = 1, dim_num
          x(i,j) = a * v1(i) + b * v2(i) + c * v3(i)
        end do

      end do

      return
      end
      subroutine uniform_in_triangle_map2 ( v1, v2, v3, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_TRIANGLE_MAP2 maps uniform points into a triangle.
c
c  Discussion:
c
c    The triangle is defined by three vertices.  This routine
c    uses Turk's rule #2.
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
c    John Burkardt
c
c  Reference:
c
c    Greg Turk,
c    Generating Random Points in a Triangle,
c    in Graphics Gems,
c    edited by Andrew Glassner,
c    AP Professional, 1990, pages 24-28.
c
c  Parameters:
c
c    Input, double precision V1(2), V2(2), V3(2), the vertices.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      integer i
      integer j
      double precision r(3)
      integer seed
      double precision x(dim_num,n)
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)

      do j = 1, n

        call r8vec_uniform_01 ( 2, seed, r )

        if ( 1.0D+00 .lt. r(1) + r(2) ) then
          r(1) = 1.0D+00 - r(1)
          r(2) = 1.0D+00 - r(2)
        end if

        do i = 1, dim_num
          x(i,j) = ( 1.0D+00 - r(1) - r(2) ) * v1(i) 
     &                       + r(1)          * v2(i) 
     &                              + r(2)   * v3(i)
        end do

      end do

      return
      end
      subroutine uniform_in_triangle01_map ( n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_TRIANGLE01_MAP maps uniform points into the unit triangle.
c
c  Discussion:
c
c    The triangle is defined by the three vertices (1,0), (0,1) and (0,0).
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n

      integer i
      integer j
      double precision r(dim_num)
      double precision r8vec_sum
      integer seed
      double precision x(dim_num,n)
c
c  Generate the points using barycentric coordinates.
c
      do j = 1, n

        call r8vec_uniform_01 ( dim_num, seed, r )

        if ( 1.0D+00 .lt. r8vec_sum ( dim_num, r ) ) then
          do i = 1, dim_num
            r(i) = 1.0D+00 - r(i)
          end do
        end if

        do i = 1, dim_num
          x(i,j) = r(i)
        end do

      end do

      return
      end
      subroutine uniform_on_cube ( m, n, c, r, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_CUBE returns random points on the surface of a cube.
c
c  Discussion:
c
c    The cube is assumed to be aligned with the coordinate axes.
c
c    The cube has center C and radius R.  Any point on the surface of
c    the cube is described by
c
c      X = C + R * PM
c
c    where PM is an M-dimensional vector whose entries are between
c    -1 and +1, and for which at least one value has norm 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 .le. M.
c
c    Input, integer N, the number of points.
c
c    Input, double precision C(M), the coordinates of the center.
c
c    Input, double precision R, the radius.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(M,N), the coordinates of N points, chosen
c    uniformly at random from the surface of the M-cube of center C and 
c    radius R.
c
      implicit none

      integer m
      integer n

      double precision c(m)
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      double precision r
      integer seed
      double precision x(m,n)
c
c  Choose random points within the cube of radius 1.
c
      call r8mat_uniform_01 ( m, n, seed, x )

      do j = 1, n
        do i = 1, m
          x(i,j) = 2.0D+00 * x(i,j) - 1.0D+00
        end do
      end do
c
c  For each point, select a coordinate at random, and set it to +1 or -1.
c
      do j = 1, n 
        i = i4_uniform_ab ( 1, m, seed )
        k = i4_uniform_ab ( 0, 1, seed )
        if ( k .eq. 0 ) then
          x(i,j) = 1.0D+00
        else
          x(i,j) = -1.0D+00
        end if
      end do
c
c  Shift by C and scale by R.
c
      do j = 1, n
        do i = 1, m
          x(i,j) = c(i) + r * x(i,j)
        end do
      end do

      return
      end
      subroutine uniform_on_cube01 ( m, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_CUBE01 returns random points on the surface of the unit cube.
c
c  Discussion:
c
c    The cube is assumed to be aligned with the coordinate axes.
c
c    The cube has center at the origin and radius 1. Any point on the 
c    surface of is described by
c
c      X = PM
c
c    where PM is an M-dimensional vector whose entries are between
c    -1 and +1, and for which at least one value has norm 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 .le. M.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(M,N), the coordinates of N points, chosen
c    uniformly at random from the surface of the unit M-cube.
c
      implicit none

      integer m
      integer n

      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer seed
      double precision x(m,n)
c
c  Choose random points within the cube of radius 1.
c
      call r8mat_uniform_01 ( m, n, seed, x )

      do j = 1, n
        do i = 1, m
          x(i,j) = 2.0D+00 * x(i,j) - 1.0D+00
        end do
      end do
c
c  For each point, select a coordinate at random, and set it to +1 or -1.
c
      do j = 1, n 
        i = i4_uniform_ab ( 1, m, seed )
        k = i4_uniform_ab ( 0, 1, seed )
        if ( k .eq. 0 ) then
          x(i,j) = 1.0D+00
        else
          x(i,j) = -1.0D+00
        end if
      end do

      return
      end
      subroutine uniform_on_ellipsoid_map ( dim_num, n, a, r, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_ELLIPSOID_MAP maps uniform points onto an ellipsoid.
c
c  Discussion:
c
c    The points X on the ellipsoid are described by an M by M positive
c    definite symmetric matrix A, and a "radius" R, such that
c
c      X' * A * X = R * R
c
c    The algorithm computes the Cholesky factorization of A:
c
c      A = U' * U.
c
c    A set of uniformly random points Y is generated, satisfying:
c
c      Y' * Y = R * R.
c
c    The appropriate points in the ellipsoid are found by solving
c
c      U * X = Y
c
c    Thanks to Dr Karl-Heinz Keil for pointing out that the original
c    coding was actually correct only if A was replaced by its inverse.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
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
c    Input, double precision A(DIM_NUM,DIM_NUM), the matrix that describes
c    the ellipsoid.
c
c    Input, double precision R, the right hand side of the ellipsoid equation.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,N), the points.
c
      implicit none

      integer dim_num
      integer n

      double precision a(dim_num,dim_num)
      integer i
      integer info
      integer j
      double precision r
      integer seed
      double precision u(dim_num,dim_num)
      double precision x(dim_num,n)
c
c  Get the factor U.
c
      do j = 1, dim_num
        do i = 1, dim_num
          u(i,j) = a(i,j)
        end do
      end do

      call r8po_fa ( dim_num, u, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_ON_ELLIPSOID_MAP - Fatal error!'
        write ( *, '(a)' ) '  R8PO_FA reports that the matrix A '
        write ( *, '(a)' ) '  is not positive definite symmetric.'
        stop
      end if
c
c  Get the points Y that satisfy Y' * Y = R * R.
c
      call uniform_on_sphere01_map ( dim_num, n, seed, x )

      do j = 1, n
        do i = 1, dim_num
          x(i,j) = r * x(i,j)
        end do
      end do
c
c  Solve U * X = Y.
c
      do j = 1, n
        call r8po_sl ( dim_num, u, x(1,j) )
      end do

      return
      end
      subroutine uniform_on_hemisphere01_phong ( n, m, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_HEMISPHERE01_PHONG maps uniform points onto the unit hemisphere.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    The Phong density is used, with exponent M:
c
c    rho ( theta, phi; m ) = ( m + 1 ) * cos ( phi )**M / ( 2 * pi )
c
c    This routine is valid for spatial dimension DIM_NUM = 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Peter Shirley,
c    Nonuniform Random Point Sets Via Warping,
c    Graphics Gems, Volume III,
c    edited by David Kirk,
c    AP Professional, 1992,
c    ISBN: 0122861663,
c    LC: T385.G6973.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, integer M, the Phong exponent.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(3,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer n

      integer i
      integer j
      integer m
      double precision phi(n)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision power
      integer seed
      double precision theta(n)
      double precision x(dim_num,n)

      power = 1.0D+00 / dble ( m + 1 )
      call r8vec_uniform_01 ( n, seed, phi )

      do i = 1, n
        phi(i) = acos ( ( 1.0D+00 - phi(i) ) ** power )
      end do

      call r8vec_uniform_01 ( n, seed, theta )
      do i = 1, n
        theta(i) = 2.0D+00 * pi * theta(i)
      end do

      do i = 1, n
        x(1,i) = cos ( theta(i) ) * sin ( phi(i) )
        x(2,i) = sin ( theta(i) ) * sin ( phi(i) )
        x(3,i) = cos ( phi(i) )
      end do

      return
      end
      subroutine uniform_on_simplex01_map ( dim_num, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_SIMPLEX01_MAP maps uniform points onto the unit simplex.
c
c  Discussion:
c
c    The surface of the unit DIM_NUM-dimensional simplex is the set of points
c    X(1:DIM_NUM) such that each X(I) is nonnegative,
c    every X(I) is no greater than 1, and
c
c    ( X(I) = 0 for some I, or sum ( X(1:M) ) = 1. )
c
c    In DIM_NUM dimensions, there are DIM_NUM sides, and one main face.
c    This code picks a point uniformly with respect to "area".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
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

      double precision area1
      double precision area2
      double precision e(dim_num)
      integer i
      integer i4_uniform_ab
      integer j
      double precision r
      double precision r8_uniform_01
      double precision r8vec_sum
      integer seed
      double precision x(dim_num,n)
c
c  The construction begins by sampling DIM_NUM points from the
c  exponential distribution with parameter 1.
c
      do j = 1, n

        call r8vec_uniform_01 ( dim_num, seed, e )

        do i = 1, dim_num
          e(i) = - log ( e(i) )
        end do
c
c  Based on their relative areas, choose a side of the simplex,
c  or the main face.
c
        do i = 1, dim_num
          x(i,j) = e(i) / r8vec_sum ( dim_num, e )
        end do

        area1 = sqrt ( dble ( dim_num ) )

        area2 = dble ( dim_num )

        r = r8_uniform_01 ( seed )

        if ( area1 / ( area1 + area2 ) .lt. r ) then
          i = i4_uniform_ab ( 1, dim_num, seed )
          x(i,j) = 0.0D+00
        end if

      end do

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
      double precision r8vec_norm
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
        norm = r8vec_norm ( dim_num, x(1,j) )
c
c  Normalize the vector.
c
        do i = 1, dim_num
          x(i,j) = x(i,j) / norm
        end do

      end do

      return
      end
      subroutine uniform_on_sphere01_patch_tp ( n, phi1, phi2, theta1, 
     &  theta2, seed, tp )

c*********************************************************************72
c
cc UNIFORM_ON_SPHERE01_PATCH_TP maps uniform points onto a spherical TP patch.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    A spherical TP patch on the surface of the unit sphere contains those
c    points with radius R = 1 and angles (THETA,PHI) such that
c
c      0.0 .le. THETA1 .le. THETA .le. THETA2 .le. 2 * PI
c      0.0 .le. PHI1   .le. PHI   .le. PHI2   .le.     PI
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Peter Shirley,
c    Nonuniform Random Point Sets Via Warping,
c    Graphics Gems, Volume III,
c    edited by David Kirk,
c    AP Professional, 1992,
c    ISBN: 0122861663,
c    LC: T385.G6973.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision PHI1, PHI2, the latitudinal angle range.
c
c    Input, double precision THETA1, THETA2, the longitudinal angle range.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision TP(2,N), the THETA, PHI coordinates of
c    the points.
c
      implicit none

      integer n

      integer j
      double precision phi1
      double precision phi2
      integer seed
      double precision theta1
      double precision theta2
      double precision tp(2,n)

      call r8mat_uniform_01 ( 2, n, seed, tp )

      do j = 1, n
        tp(1,j) = ( 1.0D+00 - tp(1,j) ) * theta1 
     &           +            tp(1,j)   * theta2

        tp(2,j) = acos ( ( 1.0D+00 - tp(2,j) ) * cos ( phi1 ) 
     &                +              tp(2,j)   * cos ( phi2 ) )
      end do

      return
      end
      subroutine uniform_on_sphere01_patch_xyz ( n, phi1, phi2, theta1, 
     &  theta2, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_SPHERE01_PATCH_XYZ maps uniform points to a spherical XYZ patch.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    A sphere XYZ patch on the surface of the unit sphere contains those
c    points with radius R = 1 and angles (THETA,PHI) such that
c
c      0.0 .le. THETA1 .le. THETA .le. THETA2 .le. 2 * PI
c      0.0 .le. PHI1   .le. PHI   .le. PHI2   .le.     PI
c
c    transformed into Cartesian XYZ coordinates:
c
c      X = cos ( THETA ) * sin ( PHI )
c      Y = sin ( THETA ) * sin ( PHI )
c      Z =                 cos ( PHI )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Peter Shirley,
c    Nonuniform Random Point Sets Via Warping,
c    Graphics Gems, Volume III,
c    edited by David Kirk,
c    AP Professional, 1992,
c    ISBN: 0122861663,
c    LC: T385.G6973.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision PHI1, PHI2, the latitudinal angle range.
c
c    Input, double precision THETA1, THETA2, the longitudinal angle range.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(3,N), the points.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer n

      integer i
      double precision phi(n)
      double precision phi1
      double precision phi2
      integer seed
      double precision theta(n)
      double precision theta1
      double precision theta2
      double precision x(dim_num,n)

      call r8vec_uniform_01 ( n, seed, phi )

      do i = 1, n
        phi(i) = acos ( ( 1.0D+00 - phi(i) ) * cos ( phi1 ) 
     &                +             phi(i)   * cos ( phi2 ) )
      end do

      call r8vec_uniform_01 ( n, seed, theta )

      do i = 1, n
        theta(i) = ( 1.0D+00 - theta(i) ) * theta1 
     &           +             theta(i)   * theta2
      end do

      do i = 1, n
        x(1,i) = cos ( theta(i) ) * sin ( phi(i) )
        x(2,i) = sin ( theta(i) ) * sin ( phi(i) )
        x(3,i) = cos ( phi(i) )
      end do

      return
      end
      subroutine uniform_on_sphere01_triangle_xyz ( n, v1, v2, v3, 
     &  seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_SPHERE01_TRIANGLE_XYZ: sample spherical triangle, XYZ coordinates.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
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
      double precision r
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
c  for now, we assume R = 1.
c
      r = 1.0D+00

      call stri_vertices_to_sides ( r, v1, v2, v3, a, b, c )

      call stri_sides_to_angles ( r, a, b, c, alpha, beta, gamma )

      call stri_angles_to_area ( r, alpha, beta, gamma, area )

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
c  V31 = normalized ( V3 - ( V3 dot V1 ) * V1 )
c
        w = r8vec_dot_product ( 3, v3, v1 )
        do i = 1, 3
          v31(i) = v3(i) - w * v1(i)
        end do
        w = r8vec_norm ( 3, v31 )
        do i = 1, 3
          v31(i) = v31(i) / w
        end do
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
        z = 1.0D+00 
     &    - xsi2 * ( 1.0D+00 - r8vec_dot_product ( 3, v4, v2 ) )
c
c  V42 = normalized ( V4 - ( V4 dot V2 ) * V2 )
c
        w = r8vec_dot_product ( 3, v4, v2 )
        do i = 1, 3
          v42(i) = v4(i) - w * v2(i)
        end do
        w = r8vec_norm ( 3, v42 )
        do i = 1, 3
          v42(i) = v42(i) / w
        end do
c
c  Construct the point.
c
        do i = 1, 3
          x(i,j) = z * v2(i) + sqrt ( 1.0D+00 - z * z ) * v42(i)
        end do

      end do

      return
      end
      subroutine uniform_on_triangle ( n, v, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_TRIANGLE maps uniform points onto the boundary of a triangle.
c
c  Discussion:
c
c    The triangle is defined by the three vertices V1, V2, V3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision V(2,3), the vertices of the triangle.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n

      integer j
      double precision l1
      double precision l2
      double precision l3
      double precision r
      double precision r8_uniform_01
      double precision s
      integer seed
      double precision t
      double precision v(2,3)
      double precision x(m,n)

      l1 = sqrt ( ( v(1,2) - v(1,1) ) ** 2 
     &          + ( v(2,2) - v(2,1) ) ** 2 )

      l2 = sqrt ( ( v(1,3) - v(1,2) ) ** 2 
     &          + ( v(2,3) - v(2,2) ) ** 2 )

      l3 = sqrt ( ( v(1,1) - v(1,3) ) ** 2 
     &          + ( v(2,1) - v(2,3) ) ** 2 )
      
      do j = 1, n
c
c  R can be regarded as the distance of the point on the perimeter,
c  as measured from the origin, along the perimeter.
c
        r = ( l1 + l2 + l3 ) * r8_uniform_01 ( seed )
c
c  Case 1: between V1 and V2.
c
        if ( r .le. l1 ) then

          s = ( l1 - r ) / l1
          t =        r   / l1
          x(1,j) = s * v(1,1) + t * v(1,2)
          x(2,j) = s * v(2,1) + t * v(2,2)
c
c  Case 2: between V2 and V3.
c
        else if ( r .le. l1 + l2 ) then

          s = ( l2 - r + l1 ) / l2
          t = (      r - l1 ) / l2
          x(1,j) = s * v(1,2) + t * v(1,3)
          x(2,j) = s * v(2,2) + t * v(2,3)
c
c  Case 3: between V3 and V1.
c
        else

          s = ( l3 - r + l1 + l2 ) / l3
          t = (      r - l1 - l2 ) / l3
          x(1,j) = s * v(1,3) + t * v(1,1)
          x(2,j) = s * v(2,3) + t * v(2,1)

        end if

      end do

      return
      end
      subroutine uniform_on_triangle01 ( n, seed, x )

c*********************************************************************72
c
cc UNIFORM_ON_TRIANGLE01: uniform points on the boundary of the unit triangle.
c
c  Discussion:
c
c    The unit triangle is defined by the three vertices (1,0), (0,1) and (0,0).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2,N), the points.
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n

      double precision a
      double precision b
      integer j
      double precision r
      double precision r8_uniform_01
      double precision s
      integer seed
      double precision x(m,n)

      s = sqrt ( 2.0D+00 )

      a =   1.0D+00       / ( 2.0D+00 + s )
      b = ( 1.0D+00 + s ) / ( 2.0D+00 + s )
      
      do j = 1, n
c
c  R can be regarded as the distance of the point on the perimeter,
c  as measured from the origin, along the perimeter.
c
        r = ( 2.0D+00 + s ) * r8_uniform_01 ( seed )
c
c  Case 1: between (0,0) and (1,0).
c
        if ( r .le. a ) then

          x(1,j) = 0.0D+00
          x(2,j) = r
c
c  Case 2: between (1,0) and (0,1).
c
        else if ( r .le. b ) then

          x(1,j) = 1.0D+00 - ( r - a ) * sqrt ( 2.0D+00 ) / 2.0D+00
          x(2,j) = 0.0D+00 + ( r - a ) * sqrt ( 2.0D+00 ) / 2.0D+00
c
c  Case 3: between (0,1) and (0,0).
c
        else

          x(1,j) = 0.0D+00
          x(2,j) = 1.0D+00 - ( r - b )

        end if

      end do

      return
      end
      subroutine uniform_walk ( dim_num, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_WALK generates points on a uniform random walk.
c
c  Discussion:
c
c    The first point is at the origin.  Uniform random numbers are
c    generated to determine the direction of the next step, which
c    is always of length 1, and in a coordinate direction.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM the dimension of the space.
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

      double precision dir(n-1)
      integer i
      integer j
      integer seed
      double precision x(dim_num,n)

      do i = 1, dim_num
        x(i,1) = 0.0D+00
      end do

      call r8vec_uniform_01 ( n - 1, seed, dir )

      do j = 1, n - 1
        dir(j) = dble ( 2 * dim_num ) * ( dir(j) - 0.5D+00 )
      end do

      do j = 2, n

        do i = 1, dim_num
          x(i,j) = x(i,j-1)
        end do

        i = nint ( abs ( dir(j-1) ) + 0.5D+00 )
        i = min ( i, dim_num )
        i = max ( i, 1 )

        if ( dir(j-1) .lt. 0.0D+00 ) then
          x(i,j) = x(i,j) - 1.0D+00
        else
          x(i,j) = x(i,j) + 1.0D+00
        end if

      end do

      return
      end
