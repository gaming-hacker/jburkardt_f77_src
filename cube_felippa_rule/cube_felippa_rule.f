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
      subroutine cube_monomial ( a, b, expon, value )

c*********************************************************************72
c
cc CUBE_MONOMIAL integrates a monomial over a cube in 3D.
c
c  Discussion:
c
c    This routine integrates a monomial of the form
c
c      product ( 1 <= dim <= 3 ) x(dim)^expon(dim)
c
c    The combination 0^0 should be treated as 1.
c
c    The integration region is:
c      A(1) <= X <= B(1)
c      A(2) <= Y <= B(2)
c      A(3) <= Z <= B(3)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(3), B(3), the lower and upper limits.
c
c    Input, integer EXPON(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      double precision a(3)
      double precision b(3)
      integer ep1
      integer expon(3)
      integer i
      double precision value

      do i = 1, 3

        if ( expon(i) .eq. -1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'CUBE_MONOMIAL - Fatal error!'
          write ( *, '(a)' ) '  Exponent of -1 encountered.'
          stop 1
        end if

      end do

      value = 1.0D+00

      do i = 1, 3

        ep1 = expon(i) + 1

        value = value * ( b(i) ** ep1 - a(i) ** ep1 ) / dble ( ep1 )
 
      end do

      return
      end
      subroutine cube_monomial_test ( degree_max )

c*********************************************************************72
c
cc CUBE_MONOMIAL_TEST tests CUBE_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DEGREE_MAX, the maximum total degree of the
c    monomials to check.
c
      implicit none

      double precision a(3)
      integer alpha
      double precision b(3)
      integer beta
      double precision cube_volume
      integer degree_max
      integer expon(3)
      integer gamma
      integer i
      double precision value

      do i = 1, 3
        a(i) = -1.0D+00
        b(i) = +1.0D+00
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For a cube in 3D,'
      write ( *, '(a)' ) 
     &  '  CUBE_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) '  over the cube [A1,B1]x[A2,B2]x[A3,B3]'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', cube_volume ( a, b )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call cube_monomial ( a, b, expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine cube_quad_test ( degree_max )

c*********************************************************************72
c
cc CUBE_QUAD_TEST tests the rules for a cube in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DEGREE_MAX, the maximum total degree of the
c    monomials to check.
c
      implicit none

      integer order_max
      parameter ( order_max = 125 )

      double precision a(3)
      double precision b(3)
      double precision cube_volume
      integer degree_max
      integer expon(3)
      integer h
      integer i
      integer i4vec_product
      integer k
      logical more
      integer order
      integer order_1d(3)
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(3,order_max)

      do i = 1, 3
        a(i) = -1.0D+00
        b(i) = +1.0D+00
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_QUAD_TEST'
      write ( *, '(a)' ) '  For a cube in 3D,'
      write ( *, '(a)' ) '  we approximate monomial integrals with'
      write ( *, '(a)' ) 
     &  '  CUBE_RULE, which returns N1 by N2 by N3 point rules.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, 3, expon, more, h, t )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents: ', expon(1:3)
        write ( *, '(a)' ) ' '

        do k = 1, 5

          order_1d(1) = k
          order_1d(2) = k
          order_1d(3) = k
          order = i4vec_product ( 3, order_1d )
          call cube_rule ( a, b, order_1d, w, xyz )
          call monomial_value ( 3, order, expon, xyz, v )
          quad = r8vec_dot_product ( order, w, v )
          write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' ) 
     &      order_1d(1:3), quad

        end do
c
c  Try a rule of mixed orders.
c
        order_1d(1) = 3
        order_1d(2) = 5
        order_1d(3) = 2
        order = i4vec_product ( 3, order_1d )
        call cube_rule ( a, b, order_1d, w, xyz )
        call monomial_value ( 3, order, expon, xyz, v )
        quad = r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' )
     &    order_1d(1:3), quad

        write ( *, '(a)' ) ' '
        call cube_monomial ( a, b, expon, quad )
        write ( *, '(2x,a,2x,6x,2x,6x,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine cube_rule ( a, b, order_1d, w, xyz )

c*********************************************************************72
c
cc CUBE_RULE returns a quadrature rule for a cube in 3D.
c
c  Discussion:
c
c    The integration region is:
c      A(1) <= X <= B(1)
c      A(2) <= Y <= B(2)
c      A(3) <= Z <= B(3)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Input, double precision A(3), B(3), the lower and upper limits.
c
c    Input, integer ORDER_1D(3), the order of the rule in each
c    dimension.  1 <= ORDER_1D(I) <= 5.
c
c    Output, double precision W(ORDER_1D(1)*ORDER_1D(2)*ORDER_1D(3)),
c    the weights.
c
c    Output, double precision XYZ(3,ORDER_1D(1)*ORDER_1D(2)*ORDER_1D(3)),
c    the abscissas.
c
      implicit none

      integer order_1d_max
      parameter ( order_1d_max = 5 )

      double precision a(3)
      double precision b(3)
      integer i
      integer i4vec_product
      integer j
      integer order
      integer order_1d(3)
      double precision w(order_1d(1)*order_1d(2)*order_1d(3))
      double precision w_1d(order_1d_max)
      double precision x_1d(order_1d_max)
      double precision xyz(3,order_1d(1)*order_1d(2)*order_1d(3))

      order = i4vec_product ( 3, order_1d )

      do i = 1, 3

        if ( order_1d(i) .eq. 1 ) then
          call line_unit_o01 ( w_1d, x_1d )
        else if ( order_1d(i) .eq. 2 ) then
          call line_unit_o02 ( w_1d, x_1d )
        else if ( order_1d(i) .eq. 3 ) then
          call line_unit_o03 ( w_1d, x_1d )
        else if ( order_1d(i) .eq. 4 ) then
          call line_unit_o04 ( w_1d, x_1d )
        else if ( order_1d(i) .eq. 5 ) then
          call line_unit_o05 ( w_1d, x_1d )
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'CUBE_RULE - Fatal error!'
          write ( *, '(a)' ) '  Illegal value of ORDER_1D(*).'
          stop 1
        end if
c
c  Transform from [-1,+1] to [Ai,Bi]
c
        do j = 1, order_1d(i)
          w_1d(j) = w_1d(j) * ( b(i) - a(i) ) / 2.0D+00
          x_1d(j) = ( ( 1.0D+00 - x_1d(j) ) * a(i)   
     &              + ( 1.0D+00 + x_1d(j) ) * b(i) ) 
     &              /   2.0D+00
        end do
c
c  Add this information to the rule.
c
        call r8vec_direct_product ( i, order_1d(i), x_1d,
     &    3, order, xyz )

        call r8vec_direct_product2 ( i, order_1d(i), w_1d,
     &    3, order, w )

      end do

      return
      end
      function cube_volume ( a, b )

c*********************************************************************72
c
cc CUBE_VOLUME: volume of a cube in 3D.
c
c  Discussion:
c
c    The integration region is:
c      A(1) <= X <= B(1)
c      A(2) <= Y <= B(2)
c      A(3) <= Z <= B(3)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(3), B(3), the lower and upper limits.
c
c    Output, double precision CUBE_VOLUME, the volume.
c
      implicit none

      double precision a(3)
      double precision b(3)
      double precision cube_volume
      integer i

      cube_volume = 1.0D+00
      do i = 1, 3
        cube_volume = cube_volume * ( b(i) - a(i) )
      end do

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
      subroutine line_unit_o01 ( w, x )

c*********************************************************************72
c
cc LINE_UNIT_O01 returns a 1 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(1), the weights.
c
c    Output, double precision X(1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      integer i
      double precision w(order)
      double precision w_save(1)
      double precision x(order)
      double precision x_save(1)

      save w_save
      save x_save

      data w_save /
     &  2.0D+00 /
      data x_save /
     &  0.0D+00 /

      do i = 1, order
        w(i) = w_save(i)
        x(i) = x_save(i)
      end do

      return
      end
      subroutine line_unit_o02 ( w, x )

c*********************************************************************72
c
cc LINE_UNIT_O02 returns a 2 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(2), the weights.
c
c    Output, double precision X(2), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 2 )

      integer i
      double precision w(order)
      double precision w_save(2)
      double precision x(order)
      double precision x_save(2)

      save w_save
      save x_save

      data w_save /
     &  1.0000000000000000000D+00,
     &  1.0000000000000000000D+00 /
      data x_save /
     &  -0.57735026918962576451D+00,
     &   0.57735026918962576451D+00 /

      do i = 1, order
        w(i) = w_save(i)
        x(i) = x_save(i)
      end do

      return
      end
      subroutine line_unit_o03 ( w, x )

c*********************************************************************72
c
cc LINE_UNIT_O03 returns a 3 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(3), the weights.
c
c    Output, double precision X(3), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 3 )

      integer i
      double precision w(order)
      double precision w_save(3)
      double precision x(order)
      double precision x_save(3)

      save w_save
      save x_save

      data w_save /
     &  0.55555555555555555556D+00, 
     &  0.88888888888888888889D+00, 
     &  0.55555555555555555556D+00 /
      data x_save /
     & -0.77459666924148337704D+00, 
     &  0.00000000000000000000D+00, 
     &  0.77459666924148337704D+00 /

      do i = 1, order
        w(i) = w_save(i)
        x(i) = x_save(i)
      end do

      return
      end
      subroutine line_unit_o04 ( w, x )

c*********************************************************************72
c
cc LINE_UNIT_O04 returns a 4 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(4), the weights.
c
c    Output, double precision X(4), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 4 )

      integer i
      double precision w(order)
      double precision w_save(4)
      double precision x(order)
      double precision x_save(4)

      save w_save
      save x_save

      data w_save /
     &  0.34785484513745385737D+00, 
     &  0.65214515486254614263D+00, 
     &  0.65214515486254614263D+00, 
     &  0.34785484513745385737D+00 /
      data x_save /
     &  -0.86113631159405257522D+00, 
     &  -0.33998104358485626480D+00, 
     &   0.33998104358485626480D+00, 
     &   0.86113631159405257522D+00 /

      do i = 1, order
        w(i) = w_save(i)
        x(i) = x_save(i)
      end do

      return
      end
      subroutine line_unit_o05 ( w, x )

c*********************************************************************72
c
cc LINE_UNIT_O05 returns a 5 point quadrature rule for the unit line.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
c
c    Output, double precision W(5), the weights.
c
c    Output, double precision X(5), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 5 )

      integer i
      double precision w(order)
      double precision w_save(5)
      double precision x(order)
      double precision x_save(5)

      save w_save
      save x_save

      data w_save /
     &  0.23692688505618908751D+00, 
     &  0.47862867049936646804D+00, 
     &  0.56888888888888888889D+00, 
     &  0.47862867049936646804D+00, 
     &  0.23692688505618908751D+00 /
      data x_save /
     &  -0.90617984593866399280D+00, 
     &  -0.53846931010568309104D+00, 
     &   0.00000000000000000000D+00, 
     &   0.53846931010568309104D+00, 
     &   0.90617984593866399280D+00 /

      do i = 1, order
        w(i) = w_save(i)
        x(i) = x_save(i)
      end do

      return
      end
      subroutine monomial_value ( m, n, e, x, v )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    F(X) = product ( 1 <= DIM <= M ) X(I)^EXPON(I)
c
c    with the convention that 0^0 = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the evaluation points.
c
c    Output, double precision V(N), the monomial values.
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
        if ( e(i) .ne. 0.0D+00 ) then
          do j = 1, n
            v(j) = v(j) * x(i,j) ** e(i)
          end do
        end if
      end do

      return
      end
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
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
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      subroutine r8vec_direct_product ( factor_index, factor_order,
     &  factor_value, factor_num, point_num, x )

c*********************************************************************72
c
cc R8VEC_DIRECT_PRODUCT creates a direct product of R8VEC's.
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
c    This routine carries out that task for the abscissas X.
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
c      X(1:4) = ( 1, 2, 3, 4 )
c
c    Rule 2:
c      Order = 3
c      X(1:3) = ( 10, 20, 30 )
c
c    Rule 3:
c      Order = 2
c      X(1:2) = ( 100, 200 )
c
c    Product Rule:
c      Order = 24
c      X(1:24) =
c        ( 1, 10, 100 )
c        ( 2, 10, 100 )
c        ( 3, 10, 100 )
c        ( 4, 10, 100 )
c        ( 1, 20, 100 )
c        ( 2, 20, 100 )
c        ( 3, 20, 100 )
c        ( 4, 20, 100 )
c        ( 1, 30, 100 )
c        ( 2, 30, 100 )
c        ( 3, 30, 100 )
c        ( 4, 30, 100 )
c        ( 1, 10, 200 )
c        ( 2, 10, 200 )
c        ( 3, 10, 200 )
c        ( 4, 10, 200 )
c        ( 1, 20, 200 )
c        ( 2, 20, 200 )
c        ( 3, 20, 200 )
c        ( 4, 20, 200 )
c        ( 1, 30, 200 )
c        ( 2, 30, 200 )
c        ( 3, 30, 200 )
c        ( 4, 30, 200 )
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
c    Input/output, double precision X(FACTOR_NUM,POINT_NUM), the elements of the
c    direct product, which are built up gradually.  Before the first call,
c    X might be set to 0.  After each factor has been input, X should
c    have the correct value.
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
      double precision x(factor_num,point_num)

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
            x(factor_index,i) = factor_value(j)
          end do
          start = start + skip
        end do

      end do

      contig = contig * factor_order

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
        do j = 1, point_num
          w(j) = 1.0D+00
        end do
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
      function r8vec_product ( n, v1 )

c*********************************************************************72
c
cc R8VEC_PRODUCT multiplies the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine PRODUCT should be called
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
c    Output, double precision R8VEC_PRODUCT, the product of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_product
      double precision v1(n)
      double precision value

      value = 1.0D+00
      do i = 1, n
        value = value * v1(i)
      end do

      r8vec_product = value

      return
      end
      subroutine subcomp_next ( n, k, a, more, h, t )

c*********************************************************************72
c
cc SUBCOMP_NEXT computes the next subcomposition of N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to a value of N.
c
c    A subcomposition of the integer N into K parts is a composition
c    of M into K parts, where 0 .le. M .le. N.
c
c    A subcomposition of the integer N into K parts is also a lattice
c    point in the simplex whose vertices are the origin, and the K direction
c    vectors N*E(I) for I = 1 to K.
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the integer whose subcompositions are desired.
c
c    Input, integer K, the number of parts in the subcomposition.
c
c    Input/output, integer A(K), the parts of the subcomposition.
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
      logical more2
      integer n
      integer n2
      integer t

      save more2
      save n2

      data more2 / .false. /
      data n2 / 0 /
c
c  The first computation.
c
      if ( .not. more ) then

        more = .true.

        do i = 1, k
          a(i) = 0
        end do

        n2 = 0
        more2 = .false.
c
c  Do the next element at the current value of N.
c    
      else if ( more2 ) then

        call comp_next ( n2, k, a, more2, h, t )

      else

        more2 = .false.
        n2 = n2 + 1

        call comp_next ( n2, k, a, more2, h, t )
        
      end if
c
c  Termination occurs if MORE2 = FALSE and N2 = N.
c
      if ( .not. more2 .and. n2 .eq. n ) then
        more = .false.
      end if

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
