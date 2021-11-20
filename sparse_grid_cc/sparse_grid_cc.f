      subroutine abscissa_level_closed_nd ( level_max, dim_num, 
     &  test_num, test_val, test_level )

c*********************************************************************72
c
cc ABSCISSA_LEVEL_CLOSED_ND: first level at which given abscissa is generated.
c
c  Discussion:
c
c    We assume an underlying product grid.  In each dimension, this product
c    grid has order 2^LEVEL_MAX + 1.
c
c    We will say a sparse grid has total level LEVEL if each point in the
c    grid has a total level of LEVEL or less.
c
c    The "level" of a point is determined as the sum of the levels of the
c    point in each spatial dimension.
c
c    The level of a point in a single spatial dimension I is determined as
c    the level, between 0 and LEVEL_MAX, at which the point's I'th index
c    would have been generated.
c
c
c    This description is terse and perhaps unenlightening.  Keep in mind
c    that the product grid is the product of 1D grids,
c    that the 1D grids are built up by levels, having
c    orders (total number of points ) 1, 3, 5, 9, 17, 33 and so on,
c    and that these 1D grids are nested, so that each point in a 1D grid
c    has a first level at which it appears.
c
c    Our procedure for generating the points of a sparse grid, then, is
c    to choose a value LEVEL_MAX, to generate the full product grid,
c    but then only to keep those points on the full product grid whose
c    LEVEL is less than or equal to LEVEL_MAX.
c
c
c    Note that this routine is really just testing out the idea of
c    determining the level.  Our true desire is to be able to start
c    with a value LEVEL, and determine, in a straightforward manner,
c    all the points that are generated exactly at that level, or
c    all the points that are generated up to and including that level.
c
c    This allows us to generate the new points to be added to one sparse
c    grid to get the next, or to generate a particular sparse grid at once.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer LEVEL_MAX, controls the size of the 
c    final sparse grid.
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer TEST_NUM, the number of points to be tested.
c
c    Input, integer TEST_VAL(DIM_NUM,TEST_NUM), the indices of 
c    the points to be tested.  Normally, each index would be between 0 and 
c    2^LEVEL_MAX.
c
c    Output, integer TEST_LEVEL(TEST_NUM), the value of LEVEL 
c    at which the point would first be generated, assuming that a standard 
c    sequence of nested grids is used.
c
      implicit none

      integer dim_num
      integer test_num

      integer index_to_level_closed
      integer j
      integer level_max
      integer order
      integer t
      integer test_level(test_num)
      integer test_val(dim_num,test_num)
c
c  Special case: LEVEL_MAX = 0.
c
      if ( level_max .le. 0 ) then

        do j = 1, test_num
          test_level(j) = 0
        end do
 
      else

        order = 2 ** level_max + 1

        do j = 1, test_num

          test_level(j) = index_to_level_closed ( dim_num, 
     &      test_val(1,j), order, level_max )

        end do

      end if

      return
      end
      function cc_abscissa ( order, i )

c*********************************************************************72
c
cc CC_ABSCISSA returns the I-th abscissa for the Clenshaw Curtis rule.
c
c  Discussion:
c
c    Our convention is that the abscissas are numbered from left to
c    right.
c
c    This rule is defined on [-1,1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ORDER, the order of the Clenshaw Curtis rule.
c    1 .le. ORDER.
c
c    Input, integer I, the index of the desired abscissa.  
c    1 .le. I .le. ORDER.
c
c    Output, double precision CC_ABSCISSA, the value of the I-th 
c    abscissa in the Clenshaw Curtis rule of order ORDER.
c
      implicit none

      double precision cc_abscissa
      integer i
      integer order
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_huge
      double precision value

      if ( order .lt. 1 ) then

        value = - r8_huge ( )

      else if ( i .lt. 1 .or. order .lt. i ) then

        value = - r8_huge ( )

      else if ( order .eq. 1 ) then

        value = 0.0D+00

      else if ( 2 * i - 1 .eq. order ) then

        value = 0.0D+00

      else

        value = cos ( dble ( order - i ) * pi 
     &              / dble ( order - 1 ) )

      end if

      cc_abscissa = value

      return
      end
      subroutine cc_weights ( n, w )

c*********************************************************************72
c
cc CC_WEIGHTS computes Clenshaw Curtis weights.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Charles Clenshaw, Alan Curtis,
c    A Method for Numerical Integration on an Automatic Computer,
c    Numerische Mathematik,
c    Volume 2, Number 1, December 1960, pages 197-205.
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision W(N), the weights of the rule.
c
      implicit none

      integer n

      double precision b
      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision w(n)

      if ( n .eq. 1 ) then
        w(1) = 2.0D+00
        return
      end if

      do i = 1, n
        theta(i) = dble ( i - 1 ) * pi 
     &           / dble ( n - 1 )
      end do

      do i = 1, n

        w(i) = 1.0D+00

        do j = 1, ( n - 1 ) / 2

          if ( 2 * j .eq. ( n - 1 ) ) then
            b = 1.0D+00
          else
            b = 2.0D+00
          end if

          w(i) = w(i) - b * cos ( 2.0D+00 * dble ( j ) * theta(i) ) 
     &         / dble ( 4 * j * j - 1 )

        end do

      end do

      w(1) = w(1) / dble ( n - 1 )
      do i = 2, n - 1
        w(i) = 2.0D+00 * w(i) / dble ( n - 1 )
      end do
      w(n) = w(n) / dble ( n - 1 )

      return
      end
      subroutine comp_next ( n, k, a, more, h, t )

c*********************************************************************72
c
cc COMP_NEXT computes the compositions of the integer N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to N.  The compositions (1,2,1)
c    and (1,1,2) are considered to be distinct.
c
c    The routine computes one composition on each call until there are no more.
c    For instance, one composition of 6 into 3 parts is
c    3+2+1, another would be 6+0+0.
c
c    On the first call to this routine, set MORE = FALSE.  The routine
c    will compute the first element in the sequence of compositions, and
c    return it, as well as setting MORE = TRUE.  If more compositions
c    are desired, call again, and again.  Each time, the routine will
c    return with a new composition.
c
c    However, when the LAST composition in the sequence is computed 
c    and returned, the routine will reset MORE to FALSE, signaling that
c    the end of the sequence has been reached.
c
c    This routine originally used a SAVE statement to maintain the
c    variables H and T.  I have decided (based on an wasting an
c    entire morning trying to track down a problem) that it is safer
c    to pass these variables as arguments, even though the user should
c    never alter them.  This allows this routine to safely shuffle
c    between several ongoing calculations.
c
c
c    There are 28 compositions of 6 into three parts.  This routine will
c    produce those compositions in the following order:
c
c     I         A
c     -     ---------
c     1     6   0   0
c     2     5   1   0
c     3     4   2   0
c     4     3   3   0
c     5     2   4   0
c     6     1   5   0
c     7     0   6   0
c     8     5   0   1
c     9     4   1   1
c    10     3   2   1
c    11     2   3   1
c    12     1   4   1
c    13     0   5   1
c    14     4   0   2
c    15     3   1   2
c    16     2   2   2
c    17     1   3   2
c    18     0   4   2
c    19     3   0   3
c    20     2   1   3
c    21     1   2   3
c    22     0   3   3
c    23     2   0   4
c    24     1   1   4
c    25     0   2   4
c    26     1   0   5
c    27     0   1   5
c    28     0   0   6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2008
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the integer whose compositions are desired.
c
c    Input, integer K, the number of parts in the composition.
c
c    Input/output, integer A(K), the parts of the composition.
c
c    Input/output, logical MORE, set by the user to start the computation,
c    and by the routine to terminate it.
c
c    Input/output, integer H, T, two internal parameters needed for the
c    computation.  The user should allocate space for these in the calling
c    program, include them in the calling sequence, but never alter them!
c
      implicit none

      integer k

      integer a(k)
      integer h
      integer i
      logical more
      integer n
      integer t
c
c  The first computation.
c
      if ( .not. more ) then

        t = n
        h = 0
        a(1) = n
        do i = 2, k
          a(i) = 0
        end do
c
c  The next computation.
c
      else

        if ( 1 .lt. t ) then
          h = 0
        end if

        h = h + 1
        t = a(h)
        a(h) = 0
        a(1) = t - 1
        a(h+1) = a(h+1) + 1

      end if
c
c  This is the last element of the sequence if all the
c  items are in the last slot.
c
      more = ( a(k) .ne. n )

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
      function i4_choose ( n, k )

c*********************************************************************72
c
cc I4_CHOOSE computes the binomial coefficient C(N,K).
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in integer arithmetic.
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
c    02 June 2007
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
c    Output, integer I4_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer i4_choose
      integer k
      integer mn
      integer mx
      integer n
      integer value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0

      else if ( mn .eq. 0 ) then

        value = 1

      else

        mx = max ( k, n - k )
        value = mx + 1

        do i = 2, mn
          value = ( value * ( mx + i ) ) / i
        end do

      end if

      i4_choose = value

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
      function i4_mop ( i )

c*********************************************************************72
c
cc I4_MOP returns the I-th power of -1 as an I4 value.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the power of -1.
c
c    Output, integer I4_MOP, the I-th power of -1.
c
      implicit none

      integer i
      integer i4_mop

      if ( mod ( i, 2 ) .eq. 0 ) then
        i4_mop = 1
      else
        i4_mop = -1
      end if

      return
      end
      function i4vec_eq ( n, a1, a2 )

c*********************************************************************72
c
cc I4VEC_EQ is true if every pair of entries in two I4VECs is equal.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 May 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vectors.
c
c    Input, integer A1(N), A2(N), two vectors to compare.
c
c    Output, logical I4VEC_EQ.
c    I4VEC_EQ is .TRUE. if every pair of elements A1(I) and A2(I) are equal,
c    and .FALSE. otherwise.
c
      implicit none

      integer n

      integer a1(n)
      integer a2(n)
      integer i
      logical i4vec_eq

      i4vec_eq = .false.

      do i = 1, n
        if ( a1(i) .ne. a2(i) ) then
          return
        end if
      end do

      i4vec_eq = .true.

      return
      end
      function i4vec_product ( n, a )

c*********************************************************************72
c
cc I4VEC_PRODUCT returns the product of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    PRODUCT function:
c
c      I4VEC_PRODUCT ( N, A ) = PRODUCT ( A(1:N) )
c
c    In MATLAB, this facility is offered by the built in
c    PROD function:
c
c      I4VEC_PRODUCT ( N, A ) = PROD ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 January 2007
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
c    Output, integer I4VEC_PRODUCT, the product of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_product

      i4vec_product = 1
      do i = 1, n
        i4vec_product = i4vec_product * a(i)
      end do

      return
      end
      function index_to_level_closed ( dim_num, t, order, level_max )

c*********************************************************************72
c
cc INDEX_TO_LEVEL_CLOSED determines the level of a point given its index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer T(DIM_NUM), the grid indices of a point 
c    in a 1D closed rule.  0 .le. T(I) .le. ORDER.
c
c    Input, integer ORDER, the order of the rule.
c
c    Input, integer LEVEL_MAX, the level with respect to which the
c    index applies.
c
c    Output, integer INDEX_TO_LEVEL_CLOSED, the first level on 
c    which the point associated with the given index will appear.
c
      implicit none

      integer dim_num

      integer dim
      integer i4_modp
      integer index_to_level_closed
      integer level
      integer level_max
      integer order
      integer s
      integer t(dim_num)

      index_to_level_closed = 0
  
      do dim = 1, dim_num

        s = i4_modp ( t(dim), order )

        if ( s .eq. 0 ) then

          level = 0

        else

          level = level_max

10        continue

          if ( mod ( s, 2 ) .eq. 0 ) then
            s = s / 2
            level = level - 1
            go to 10
          end if

        end if

        if ( level .eq. 0 ) then
          level = 1
        else if ( level .eq. 1 ) then
          level = 0
        end if

        index_to_level_closed = index_to_level_closed + level

      end do

      return
      end
      subroutine level_to_order_ccs ( dim_num, level, order )

c*********************************************************************72
c
cc LEVEL_TO_ORDER_CCS: level to order for CCS rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Knut Petras,
c    Smolyak Cubature of Given Polynomial Degree with Few Nodes
c    for Increasing Dimension,
c    Numerische Mathematik,
c    Volume 93, Number 4, February 2003, pages 729-753.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL(DIM_NUM), the 1D levels.
c
c    Output, integer ORDER(DIM_NUM), the 1D orders 
c    (number of points).
c
      implicit none

      integer dim_num

      integer dim
      integer level(dim_num)
      integer o
      integer order(dim_num)

      do dim = 1, dim_num
        if ( level(dim) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'LEVEL_TO_ORDER_CCS - Fatal error!'
          write ( *, '(a)' ) '  Some entry of LEVEL is negative.'
          stop
        end if
      end do

      do dim = 1, dim_num

        if ( level(dim) .eq. 0 ) then
          o = 1
        else
          o = 2
10        continue
          if ( o .lt. 2 * level(dim) + 1 ) then
            o = 2 * ( o - 1 ) + 1
            go to 10
          end if
        end if

        order(dim) = o

      end do

      return
      end
      subroutine level_to_order_closed ( dim_num, level, order )

c*********************************************************************72
c
cc LEVEL_TO_ORDER_CLOSED converts a level to an order for closed rules.
c
c  Discussion:
c
c    Sparse grids can naturally be nested.  A natural scheme is to use
c    a series of one-dimensional rules arranged in a series of "levels"
c    whose order roughly doubles with each step.
c
c    The arrangement described here works naturally for the Clenshaw Curtis
c    and Newton Cotes closed rules.  
c
c    The idea is that we start with LEVEL = 0, ORDER = 1 indicating the single 
c    point at the center, and for all values afterwards, we use the 
c    relationship
c
c      ORDER = 2^LEVEL + 1
c
c    The following table shows how the growth will occur:
c
c    Level    Order
c
c    0          1
c    1          3 =  2 + 1
c    2          5 =  4 + 1
c    3          9 =  8 + 1
c    4         17 = 16 + 1
c    5         33 = 32 + 1
c
c    For the Clenshaw Curtis and Newton Cotes Closed rules, the point growth
c    is nested.  If we have ORDER points on a particular LEVEL, the next
c    level includes all these old points, plus ORDER-1 new points, formed
c    in the gaps between successive pairs of old points.
c
c    Level    Order = New + Old
c
c    0          1   =  1  +  0
c    1          3   =  2  +  1
c    2          5   =  2  +  3
c    3          9   =  4  +  5
c    4         17   =  8  +  9
c    5         33   = 16  + 17
c
c    In this routine, we assume that a vector of levels is given,
c    and the corresponding orders are desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL(DIM_NUM), the nesting levels of the
c    1D rules.
c
c    Output, integer ORDER(DIM_NUM), the order (number of points) 
c    of the 1D rules.
c
      implicit none

      integer dim_num

      integer dim
      integer level(dim_num)
      integer order(dim_num)

      do dim = 1, dim_num

        if ( level(dim) .lt. 0 ) then
          order(dim) = -1
        else if ( level(dim) .eq. 0 ) then
          order(dim) = 1
        else
          order(dim) = ( 2 ** level(dim) ) + 1
        end if

      end do

      return
      end
      subroutine levels_closed_index ( dim_num, level_max, point_num, 
     &  grid_index )

c*********************************************************************72
c
cc LEVELS_CLOSED_INDEX computes closed grids with 0 .le. LEVEL .le. LEVEL_MAX.
c
c  Discussion:
c
c    The necessary dimensions of GRID_INDEX can be determined by 
c    calling LEVELS_CLOSED_INDEX_SIZE first.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the maximum value of LEVEL.
c
c    Input, integer POINT_NUM, the total number of points 
c    in the grids.
c
c    Output, integer GRID_INDEX(DIM_NUM,POINT_NUM), a list of 
c    point indices, representing a subset of the product grid of level 
c    LEVEL_MAX, representing (exactly once) each point that will show up in a
c    sparse grid of level LEVEL_MAX.
c
      implicit none

      integer dim_num
      integer order_max
      parameter ( order_max = 1025 )
      integer point_num

      integer dim
      integer factor
      integer grid_index(dim_num,point_num)
      integer grid_index2(dim_num,order_max)
      integer grid_level(order_max)
      integer h
      integer i4vec_product
      integer j
      integer level
      integer level_1d(dim_num)
      integer level_max
      logical more
      integer order_1d(dim_num)
      integer order_nd
      integer point
      integer point_num2
      integer t
c
c  The outer loop generates LEVELs from 0 to LEVEL_MAX.
c
      point_num2 = 0

      do level = 0, level_max
c
c  The middle loop generates the next partition LEVEL_1D(1:DIM_NUM)
c  that adds up to LEVEL.
c
        more = .false.
        h = 0
        t = 0

10      continue

          call comp_next ( level, dim_num, level_1d, more, h, t )
c
c  Transform each 1D level to a corresponding 1D order.
c
          call level_to_order_closed ( dim_num, level_1d, order_1d )
c
c  The product of the 1D orders gives us the number of points in this grid.
c
          order_nd = i4vec_product ( dim_num, order_1d )
c
c  The inner (hidden) loop generates all points corresponding to given grid.
c
          call multigrid_index0 ( dim_num, order_1d, order_nd,
     &      grid_index2 )
c
c  Adjust these grid indices to reflect LEVEL_MAX.
c
          call multigrid_scale_closed ( dim_num, order_nd, level_max,
     &      level_1d, grid_index2 )
c
c  Determine the first level of appearance of each of the points.
c
          call abscissa_level_closed_nd ( level_max, dim_num, order_nd, 
     &      grid_index2, grid_level )
c
c  Only keep those points which first appear on this level.
c
          do point = 1, order_nd

            if ( grid_level(point) .eq. level ) then

              point_num2 = point_num2 + 1

              do dim = 1, dim_num
                grid_index(dim,point_num2) = grid_index2(dim,point)
              end do

            end if

          end do

          if ( .not. more ) then
            go to 20
          end if

        go to 10

20      continue

      end do

      return
      end
      subroutine monomial_int01 ( dim_num, expon, value )

c*********************************************************************72
c
cc MONOMIAL_INT01 returns the integral of a monomial over the [0,1] hypercube.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 .le. dim .le. dim_num ) x(dim)^expon(dim)
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
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer EXPON(DIM_NUM), the exponents.
c
c    Output, double precision VALUE, the value of the integral of the
c    monomial.
c
      implicit none

      integer dim_num

      integer dim
      integer expon(dim_num)
      double precision p
      double precision value

      p = 1
      do dim = 1, dim_num
       p = p * ( expon(dim) + 1 )
      end do

      value = 1.0D+00 / dble ( p )

      return
      end
      subroutine monomial_quadrature ( dim_num, expon, point_num, 
     &  weight, x, quad_error )

c*********************************************************************72
c
cc MONOMIAL_QUADRATURE applies a quadrature rule to a monomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer EXPON(DIM_NUM), the exponents.
c
c    Input, integer POINT_NUM, the number of points in the rule.
c
c    Input, double precision WEIGHT(POINT_NUM), the quadrature weights.
c
c    Input, double precision X(DIM_NUM,POINT_NUM), the quadrature points.
c
c    Output, double precision QUAD_ERROR, the quadrature error.
c
      implicit none

      integer dim_num

      double precision exact
      integer expon(dim_num)
      integer point_num
      double precision quad
      double precision quad_error
      double precision r8vec_dot_product
      double precision scale
      double precision value(point_num)
      double precision weight(point_num)
      double precision x(dim_num,point_num)
c
c  Get the exact value of the integral of the unscaled monomial.
c
      call monomial_int01 ( dim_num, expon, scale )
c
c  Evaluate the monomial at the quadrature points.
c
      call monomial_value ( dim_num, point_num, x, expon, value )
c
c  Compute the weighted sum and divide by the exact value.
c
      quad = r8vec_dot_product ( point_num, weight, value ) / scale
c
c  Error:
c
      exact = 1.0D+00
      quad_error = abs ( quad - exact )

      return
      end
      subroutine monomial_value ( dim_num, point_num, x, expon, value )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 .le. dim .le. dim_num ) x(dim)^expon(dim)
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
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer POINT_NUM, the number of points at which the
c    monomial is to be evaluated.
c
c    Input, double precision X(DIM_NUM,POINT_NUM), the point coordinates.
c
c    Input, integer EXPON(DIM_NUM), the exponents.
c
c    Output, double precision VALUE(POINT_NUM), the value of the monomial.
c
      implicit none

      integer dim_num
      integer point_num

      integer dim
      integer expon(dim_num)
      integer i
      double precision value(point_num)
      double precision x(dim_num,point_num)

      do i = 1, point_num
        value(i) = 1.0D+00
      end do

      do dim = 1, dim_num
        if ( 0 .ne. expon(dim) ) then
          do i = 1, point_num
            value(i) = value(i) * x(dim,i) ** expon(dim)
          end do
        end if
      end do

      return
      end
      subroutine multigrid_index0 ( dim_num, order_1d, order_nd, indx )

c*********************************************************************72
c
cc MULTIGRID_INDEX0 returns an indexed multidimensional grid.
c
c  Discussion:
c
c    For dimension DIM, the second index of INDX may vary from 
c    0 to ORDER_1D(DIM)-1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension of the points.
c
c    Input, integer ORDER_1D(DIM_NUM), the order of the
c    rule in each dimension.
c
c    Input, integer ORDER_ND, the product of the entries 
c    of ORDER_1D.
c
c    Output, integer INDX(DIM_NUM,ORDER_ND), the indices of the 
c    points in the grid.  The second dimension of this array is equal to the
c    product of the entries of ORDER_1D.
c
      implicit none

      integer dim_num
      integer order_nd

      integer a(dim_num)
      integer change
      integer dim
      logical more
      integer order_1d(dim_num)
      integer p
      integer indx(dim_num,order_nd)

      more = .false.
      p = 0

10    continue

        call vec_colex_next2 ( dim_num, order_1d, a, more )

        if ( .not. more ) then
          go to 20
        end if

        p = p + 1

        do dim = 1, dim_num
          indx(dim,p) = a(dim)
        end do

      go to 10

20    continue

      return
      end
      subroutine multigrid_scale_closed ( dim_num, order_nd, level_max, 
     &  level_1d, grid_index )

c*********************************************************************72
c
cc MULTIGRID_SCALE_CLOSED renumbers a grid as a subgrid on a higher level.
c
c  Discussion:
c
c    This routine takes a grid associated with a given value of
c    LEVEL, and multiplies all the indices by a power of 2, so that
c    the indices reflect the position of the same points, but in
c    a grid of level LEVEL_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer ORDER_ND, the number of points in the grid.
c
c    Input, integer LEVEL_MAX, the maximum value of LEVEL.
c
c    Input, integer LEVEL_1D(DIM_NUM), the level in each dimension.
c
c    Input/output, integer GRID_INDEX(DIM_NUM,POINT_NUM), the index
c    values for each grid point.  On input, these indices are based in
c    the level for which the grid was generated; on output, the
c    indices are appropriate for the grid as a subgrid of a grid
c    of level LEVEL_MAX.
c
      implicit none

      integer dim_num
      integer order_nd

      integer dim
      integer factor
      integer grid_index(dim_num,order_nd)
      integer j
      integer level_1d(dim_num)
      integer level_max
      integer order_max

      do dim = 1, dim_num

        if ( level_1d(dim) .eq. 0 ) then

          if ( 0 .eq. level_max ) then
            order_max = 1
          else
            order_max = 2 ** level_max + 1
          end if

          do j = 1, order_nd
            grid_index(dim,j) = ( order_max - 1 ) / 2
          end do

        else

          factor = 2 ** ( level_max - level_1d(dim) )

          do j = 1, order_nd
            grid_index(dim,j) = grid_index(dim,j) * factor
          end do

        end if

      end do

      return
      end
      subroutine product_weights_cc ( dim_num, order_1d, order_nd, 
     &  w_nd )

c*********************************************************************72
c
cc PRODUCT_WEIGHTS_CC: Clenshaw Curtis product rule weights.
c
c  Discussion:
c
c    This routine computes the weights for a quadrature rule which is
c    a product of 1D closed rules of varying order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer ORDER_1D(DIM_NUM), the order of the 1D rules.
c
c    Input, integer ORDER_ND, the order of the product rule.
c
c    Output, double precision W_ND(DIM_NUM,ORDER_ND), the product rule weights.
c
      implicit none

      integer dim_num
      integer order_max
      parameter ( order_max = 1025 )
      integer order_nd

      integer dim
      integer j
      integer order_1d(dim_num)
      double precision w_1d(order_max)
      double precision w_nd(order_nd)

      do j = 1, order_nd
        w_nd(j) = 1.0D+00
      end do

      do dim = 1, dim_num
	
        call cc_weights ( order_1d(dim), w_1d )

        call r8vec_direct_product2 ( dim, order_1d(dim), w_1d, 
     &    dim_num, order_nd, w_nd )
 
      end do

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
      subroutine r8vec_direct_product2 ( factor_index, factor_order,
     &  factor_value, factor_num, point_num, w )

c*********************************************************************72
c
cc R8VEC_DIRECT_PRODUCT2 creates a direct product of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    To explain what is going on here, suppose we had to construct
c    a multidimensional quadrature rule as the product of K rules
c    for 1D quadrature.
c
c    The product rule will be represented as a list of points and weights.
c
c    The J-th item in the product rule will be associated with
c      item J1 of 1D rule 1,
c      item J2 of 1D rule 2,
c      ...,
c      item JK of 1D rule K.
c
c    In particular,
c      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
c    and
c      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
c
c    So we can construct the quadrature rule if we can properly
c    distribute the information in the 1D quadrature rules.
c
c    This routine carries out the task involving the weights W.
c
c    Another way to do this would be to compute, one by one, the
c    set of all possible indices (J1,J2,...,JK), and then index
c    the appropriate information.  An advantage of the method shown
c    here is that you can process the K-th set of information and
c    then discard it.
c
c  Example:
c
c    Rule 1:
c      Order = 4
c      W(1:4) = ( 2, 3, 5, 7 )
c
c    Rule 2:
c      Order = 3
c      W(1:3) = ( 11, 13, 17 )
c
c    Rule 3:
c      Order = 2
c      W(1:2) = ( 19, 23 )
c
c    Product Rule:
c      Order = 24
c      W(1:24) =
c        ( 2 * 11 * 19 )
c        ( 3 * 11 * 19 )
c        ( 4 * 11 * 19 )
c        ( 7 * 11 * 19 )
c        ( 2 * 13 * 19 )
c        ( 3 * 13 * 19 )
c        ( 5 * 13 * 19 )
c        ( 7 * 13 * 19 )
c        ( 2 * 17 * 19 )
c        ( 3 * 17 * 19 )
c        ( 5 * 17 * 19 )
c        ( 7 * 17 * 19 )
c        ( 2 * 11 * 23 )
c        ( 3 * 11 * 23 )
c        ( 5 * 11 * 23 )
c        ( 7 * 11 * 23 )
c        ( 2 * 13 * 23 )
c        ( 3 * 13 * 23 )
c        ( 5 * 13 * 23 )
c        ( 7 * 13 * 23 )
c        ( 2 * 17 * 23 )
c        ( 3 * 17 * 23 )
c        ( 5 * 17 * 23 )
c        ( 7 * 17 * 23 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FACTOR_INDEX, the index of the factor being processed.
c    The first factor processed must be factor 1!
c
c    Input, integer FACTOR_ORDER, the order of the factor.
c
c    Input, double precision FACTOR_VALUE(FACTOR_ORDER), the factor values
c    for factor FACTOR_INDEX.
c
c    Input, integer FACTOR_NUM, the number of factors.
c
c    Input, integer POINT_NUM, the number of elements in the direct product.
c
c    Input/output, double precision W(POINT_NUM), the elements of the
c    direct product, which are built up gradually.  Before the first call,
c    W should be set to 1.
c
c  Local Parameters:
c
c    Local, integer START, the first location of a block of values to set.
c
c    Local, integer CONTIG, the number of consecutive values to set.
c
c    Local, integer SKIP, the distance from the current value of START
c    to the next location of a block of values to set.
c
c    Local, integer REP, the number of blocks of values to set.
c
      implicit none

      integer factor_num
      integer factor_order
      integer point_num

      integer contig
      integer factor_index
      double precision factor_value(factor_order)
      integer i
      integer j
      integer k
      integer rep
      integer skip
      integer start
      double precision w(point_num)

      save contig
      save rep
      save skip

      data contig / 0 /
      data rep / 0 /
      data skip / 0 /

      if ( factor_index .eq. 1 ) then
        contig = 1
        skip = 1
        rep = point_num
      end if

      rep = rep / factor_order
      skip = skip * factor_order

      do j = 1, factor_order

        start = 1 + ( j - 1 ) * contig

        do k = 1, rep
          do i = start, start+contig-1
            w(i) = w(i) * factor_value(j)
          end do
          start = start + skip
        end do

      end do

      contig = contig * factor_order

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
      subroutine s_blank_delete ( s )

c*********************************************************************72
c
cc S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
c
c  Discussion:
c
c    All TAB characters are also removed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character*(*) S, the string to be transformed.
c
      implicit none

      character ch
      integer get
      integer put
      character*(*) s
      integer s_len_trim
      integer s_length
      character tab

      tab = char ( 9 )

      put = 0
      s_length = s_len_trim ( s )

      do get = 1, s_length

        ch = s(get:get)

        if ( ch .ne. ' ' .and. ch .ne. tab ) then
          put = put + 1
          s(put:put) = ch
        end if

      end do

      s(put+1:s_length) = ' '

      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

      return
      end
      subroutine sparse_grid_cc ( dim_num, level_max, point_num,
     &  grid_weight, grid_point )
  
c********************************************************************72 
c 
cc SPARSE_GRID_CC computes a sparse grid of Clenshaw Curtis points. 
c 
c  Discussion: 
c 
c    This program computes a quadrature rule and writes it to a file. 
c 
c    The quadrature rule is associated with a sparse grid derived from 
c    a Smolyak construction using a closed 1D quadrature rule.  
c 
c    The user specifies: 
c    * the spatial dimension of the quadrature region, 
c    * the level that defines the Smolyak grid. 
c    * the closed 1D quadrature rule (Clenshaw-Curtis or Newton-Cotes Closed). 
c 
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c 
c  Author: 
c 
c    John Burkardt 
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters: 
c 
c    Input, integer DIM_NUM, the spatial dimension. 
c 
c    Input, integer LEVEL_MAX, controls the size of the final 
c    sparse grid. 
c 
c    Input, integer POINT_NUM, the number of points in the grid, 
c    as determined by SPARSE_GRID_CC_SIZE. 
c 
c    Output, double precision GRID_WEIGHT(POINT_NUM), the weights. 
c 
c    Output, double precision GRID_POINT(DIM_NUM,POINT_NUM), the points. 
c 
      implicit none

      integer dim_num
      integer point_num

      double precision cc_abscissa
      integer dim
      integer grid_index(dim_num,point_num)
      double precision grid_point(dim_num,point_num)
      double precision grid_weight(point_num)
      integer level_max
      integer order_max
      integer point
c 
c  Determine the index vector, relative to the full product grid, 
c  that identifies the points in the sparse grid. 
c
      call sparse_grid_cc_index ( dim_num, level_max, point_num,
     &  grid_index ) 
c 
c  Compute the physical coordinates of the abscissas. 
c 
      if ( 0 .eq. level_max ) then
        order_max = 1
      else 
        order_max = 2 ** level_max + 1
      end if

      do point = 1, point_num
        do dim = 1, dim_num
          grid_point(dim,point) = 
     &      cc_abscissa ( order_max, grid_index(dim,point) + 1 )
        end do
      end do
c 
c  Gather the weights. 
c 
      call sparse_grid_cc_weights ( dim_num, level_max, point_num,
     &  grid_index, grid_weight )

      return
      end
      subroutine sparse_grid_cc_index ( dim_num, level_max, point_num, 
     &  grid_index )

c*********************************************************************72
c
cc SPARSE_GRID_CC_INDEX indexes the points forming a sparse grid.
c
c  Discussion:
c
c    The points forming the sparse grid are guaranteed to be a subset
c    of a certain product grid.  The product grid is formed by DIM_NUM
c    copies of a 1D rule of fixed order.  The orders of the 1D rule,
c    (called ORDER_1D) and the order of the product grid, (called ORDER)
c    are determined from the value LEVEL_MAX.
c
c    Thus, any point in the product grid can be identified by its grid index,
c    a set of DIM_NUM indices, each between 1 and ORDER_1D.
c
c    This routine creates the GRID_INDEX array, listing (uniquely) the
c    points of the sparse grid.  
c
c    An assumption has been made that the 1D rule is closed (includes
c    the interval endpoints) and nested (points that are part of a rule
c    of a given level will be part of every rule of higher level).
c
c    The necessary dimensions of GRID_INDEX can be determined by 
c    calling SPARSE_GRID_CC_SIZE first.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the maximum value of LEVEL.
c
c    Input, integer POINT_NUM, the total number of points in 
c    the grids.
c
c    Output, integer GRID_INDEX(DIM_NUM,POINT_NUM), a list of 
c    point indices, representing a subset of the product grid of level 
c    LEVEL_MAX, representing (exactly once) each point that will show up in a
c    sparse grid of level LEVEL_MAX.
c
      implicit none

      integer dim_num
      integer order_max
      parameter ( order_max = 1025 )
      integer point_num

      integer dim
      integer factor
      integer grid_index(dim_num,point_num)
      integer grid_index2(dim_num,order_max)
      integer grid_level(order_max)
      integer h
      integer i4vec_product
      integer j
      integer level
      integer level_1d(dim_num)
      integer level_max
      logical more
      integer order_1d(dim_num)
      integer order_nd
      integer point
      integer point_num2
      integer t
c
c  The outer loop generates LEVELs from 0 to LEVEL_MAX.
c
      point_num2 = 0

      do level = 0, level_max
c
c  The middle loop generates the next partition LEVEL_1D(1:DIM_NUM)
c  that adds up to LEVEL.
c
        more = .false.

10      continue

          call comp_next ( level, dim_num, level_1d, more, h, t )
c
c  Transform each 1D level to a corresponding 1D order.
c
          call level_to_order_closed ( dim_num, level_1d, order_1d )
c
c  The product of the 1D orders gives us the number of points in this grid.
c
          order_nd = i4vec_product ( dim_num, order_1d )
c
c  The inner (hidden) loop generates all points corresponding to given grid.
c
          call multigrid_index0 ( dim_num, order_1d, order_nd,
     &      grid_index2 )
c
c  Adjust these grid indices to reflect LEVEL_MAX.
c
          call multigrid_scale_closed ( dim_num, order_nd, level_max,
     &      level_1d, grid_index2 )
c
c  Determine the first level of appearance of each of the points.
c
          call abscissa_level_closed_nd ( level_max, dim_num, order_nd, 
     &      grid_index2, grid_level )
c
c  Only keep those points which first appear on this level.
c
          do point = 1, order_nd

            if ( grid_level(point) .eq. level ) then

              point_num2 = point_num2 + 1
              do dim = 1, dim_num
                grid_index(dim,point_num2) = grid_index2(dim,point)
              end do

            end if

          end do

          if ( .not. more ) then
            go to 20
          end if

        go to 10

20      continue

      end do

      return
      end
      subroutine sparse_grid_cc_weights ( dim_num, level_max, point_num, 
     &  grid_index, grid_weight )

c*********************************************************************72
c
cc SPARSE_GRID_CC_WEIGHTS gathers the weights.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the maximum value of LEVEL.
c
c    Input, integer POINT_NUM, the total number of points in 
c    the grids.
c
c    Input, integer GRID_INDEX(DIM_NUM,POINT_NUM), a list of
c    point indices, representing a subset of the product grid of level 
c    LEVEL_MAX, representing (exactly once) each point that will show up in a
c    sparse grid of level LEVEL_MAX.
c
c    Output, double precision GRID_WEIGHT(POINT_NUM), the weights
c    associated with the sparse grid points.
c
      implicit none

      integer dim_num
      integer order_max
      parameter ( order_max = 1025 )
      integer point_num

      integer coeff
      integer dim
      integer grid_index(dim_num,point_num)
      integer grid_index2(dim_num,order_max)
      double precision grid_weight(point_num)
      double precision grid_weight2(order_max)
      integer h
      integer i4_choose
      integer i4_mop
      logical i4vec_eq
      integer i4vec_product
      integer j
      integer level
      integer level_1d(dim_num)
      integer level_max
      integer level_min
      logical more
      integer order_nd
      integer order_1d(dim_num)
      integer point
      integer point2
      integer t

      if ( level_max .eq. 0 ) then
        do j = 1, point_num
          grid_weight(j) = 2.0D+00 ** dim_num
        end do
        return
      end if

      do j = 1, point_num
        grid_weight(j) = 0.0D+00
      end do

      level_min = max ( 0, level_max + 1 - dim_num )

      do level = level_min, level_max
c
c  The middle loop generates the next partition LEVEL_1D(1:DIM_NUM)
c  that adds up to LEVEL.
c
        more = .false.

20      continue

          call comp_next ( level, dim_num, level_1d, more, h, t )
c
c  Transform each 1D level to a corresponding 1D order.
c
          call level_to_order_closed ( dim_num, level_1d, order_1d )
c
c  The product of the 1D orders gives us the number of points in this grid.
c
          order_nd = i4vec_product ( dim_num, order_1d )
c
c  Generate the indices of the points corresponding to the grid.
c
          call multigrid_index0 ( dim_num, order_1d, order_nd,
     &      grid_index2 )
c
c  Compute the weights for this grid.
c
          call product_weights_cc ( dim_num, order_1d, order_nd,
     &      grid_weight2 )
c
c  Adjust the grid indices to reflect LEVEL_MAX.
c
          call multigrid_scale_closed ( dim_num, order_nd, level_max,
     &      level_1d, grid_index2 )
c
c  Now determine the coefficient.
c
          coeff = i4_mop ( level_max - level ) 
     &      * i4_choose ( dim_num - 1, level_max - level )

          do point2 = 1, order_nd

            do point = 1, point_num

              if ( i4vec_eq ( dim_num, grid_index2(1,point2), 
     &          grid_index(1,point) ) ) then
                grid_weight(point) = grid_weight(point) 
     &            + dble ( coeff ) * grid_weight2(point2)
                go to 10
              end if

            end do

            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'SPARSE_GRID_CC_WEIGHTS - Fatal error!'
            write ( *, '(a)' ) '  No match found.'
            stop

10          continue

          end do

          if ( .not. more ) then
            go to 30
          end if

        go to 20

30      continue

      end do

      return
      end
      subroutine sparse_grid_ccs_size ( dim_num, level_max, point_num )

c*********************************************************************72
c
cc SPARSE_GRID_CCS_SIZE sizes a sparse grid using Clenshaw Curtis Slow rules.
c
c  Discussion:
c
c    The grid is defined as the sum of the product rules whose LEVEL
c    satisfies:
c
c      0 .le. LEVEL .le. LEVEL_MAX.
c
c    This calculation is much faster than a previous method.  It simply
c    computes the number of new points that are added at each level in the
c    1D rule, and then counts the new points at a given DIM_NUM dimensional
c    level vector as the product of the new points added in each dimension.
c
c    This approach will work for nested families, and may be extensible
c    to other families, and to mixed rules.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the maximum value of LEVEL.
c
c    Output, integer POINT_NUM, the total number of unique 
c    points in the grids.
c
      implicit none

      integer dim_max
      parameter ( dim_max = 100 )
      integer level_max_max
      parameter ( level_max_max = 10 )

      integer dim
      integer dim_num
      integer h
      integer l
      integer level
      integer level_1d(dim_max)
      integer level_max
      logical more
      integer new_1d(0:level_max_max)
      integer o
      integer p
      integer point_num
      integer pr
      integer t
c
c  Special case.
c
      if ( level_max .lt. 0 ) then
        point_num = 0
        return
      end if

      if ( level_max .eq. 0 ) then
        point_num = 1
        return
      end if
c
c  Construct the vector that counts the new points in the 1D rule.
c
      new_1d(0) = 1
      new_1d(1) = 2

      p = 3
      o = 3

      do l = 2, level_max
        p = 2 * l + 1
        if ( o .lt. p ) then
          new_1d(l) = o - 1
          o = 2 * o - 1
        else
          new_1d(l) = 0
        end if
      end do
c
c  Count the number of points by counting the number of new points 
c  associated with each level vector.
c
      point_num = 0

      do level = 0, level_max

        more = .false.
        h = 0
        t = 0

10      continue

          call comp_next ( level, dim_num, level_1d, more, h, t )

          pr = 1
          do dim = 1, dim_num
            pr = pr * new_1d(level_1d(dim))
          end do

          point_num = point_num + pr

          if ( .not. more ) then
            go to 20
          end if

        go to 10

20      continue

      end do

      return
      end

      subroutine sparse_grid_cfn_size ( dim_num, level_max, point_num )

c*********************************************************************72
c
cc SPARSE_GRID_CC_SIZE sizes a sparse grid using Closed Fully Nested rules.
c
c  Discussion:
c
c    The grid is defined as the sum of the product rules whose LEVEL
c    satisfies:
c
c      0 .le. LEVEL .le. LEVEL_MAX.
c
c    This calculation is much faster than a previous method.  It simply
c    computes the number of new points that are added at each level in the
c    1D rule, and then counts the new points at a given DIM_NUM dimensional
c    level vector as the product of the new points added in each dimension.
c
c    This approach will work for nested families, and may be extensible
c    to other families, and to mixed rules.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the maximum value of LEVEL.
c
c    Output, integer POINT_NUM, the total number of unique 
c    points in the grids.
c
      implicit none

      integer dim_max
      parameter ( dim_max = 100 )
      integer level_max_max
      parameter ( level_max_max = 10 )

      integer dim
      integer dim_num
      integer h
      integer j
      integer l
      integer level
      integer level_1d(dim_max)
      integer level_max
      logical more
      integer new_1d(0:level_max_max)
      integer p
      integer point_num
      integer t
c
c  Special case.
c
      if ( level_max .lt. 0 ) then
        point_num = 0
        return
      end if

      if ( level_max .eq. 0 ) then
        point_num = 1
        return
      end if
c
c  Construct the vector that counts the new points in the 1D rule.
c
      new_1d(0) = 1
      new_1d(1) = 2

      j = 1
      do l = 2, level_max
        j = j * 2
        new_1d(l) = j
      end do
c
c  Count the number of points by counting the number of new points 
c  associated with each level vector.
c
      point_num = 0

      do level = 0, level_max

        more = .false.
        h = 0
        t = 0

10      continue

          call comp_next ( level, dim_num, level_1d, more, h, t )

          p = 1
          do dim = 1, dim_num
            p = p * new_1d(level_1d(dim))
          end do

          point_num = point_num + p

          if ( .not. more ) then
            go to 20
          end if

        go to 10

20      continue

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
      subroutine vec_colex_next2 ( dim_num, base, a, more )

c*********************************************************************72
c
cc VEC_COLEX_NEXT2 generates vectors in colex order.
c
c  Discussion:
c
c    The vectors are produced in colexical order, starting with
c
c    (0,        0,        ...,0),
c    (1,        0,        ...,0),
c     ...
c    (BASE(1)-1,0,        ...,0)
c
c    (0,        1,        ...,0)
c    (1,        1,        ...,0)
c    ...
c    (BASE(1)-1,1,        ...,0)
c
c    (0,        2,        ...,0)
c    (1,        2,        ...,0)
c    ...
c    (BASE(1)-1,BASE(2)-1,...,BASE(DIM_NUM)-1).
c
c  Examples:
c
c    DIM_NUM = 2,
c    BASE = ( 3, 3 )
c
c    0   0
c    1   0
c    2   0
c    0   1
c    1   1
c    2   1
c    0   2
c    1   2
c    2   2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer BASE(DIM_NUM), the bases to be used in each 
c    dimension.  In dimension I, entries will range from 0 to BASE(I)-1.
c
c    Input/output, integer A(DIM_NUM).  On each return, A
c    will contain entries in the range 0 to N-1.
c
c    Input/output, logical MORE.  Set this variable FALSE before
c    the first call.  On return, MORE is TRUE if another vector has
c    been computed.  If MORE is returned FALSE, ignore the output 
c    vector and stop calling the routine.
c
      implicit none

      integer dim_num

      integer a(dim_num)
      integer base(dim_num)
      integer i
      logical more

      if ( .not. more ) then

        do i = 1, dim_num
          a(i) = 0
        end do

        more = .true.

      else

        do i = 1, dim_num

          a(i) = a(i) + 1

          if ( a(i) .lt. base(i) ) then
            return
          end if

          a(i) = 0

        end do

        more = .false.

      end if

      return
      end
