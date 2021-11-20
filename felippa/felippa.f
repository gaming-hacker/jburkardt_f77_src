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
      subroutine hexa_unit_monomial ( expon, value )

c*********************************************************************72
c
cc HEXA_UNIT_MONOMIAL integrates a monomial over the unit hexahedron.
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
c
c    - 1.0 <= X <= 1.0
c    - 1.0 <= Y <= 1.0
c    - 1.0 <= Z <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(3)
      integer i
      double precision value

      value = 1.0D+00

      do i = 1, 3

        if ( mod ( expon(i), 2 ) .eq. 1 ) then
          value = 0.0D+00
        else if ( expon(i) .eq. -1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'HEXA_UNIT_MONOMIAL - Fatal error!'
          write ( *, '(a)' ) '  Exponent of -1 encountered.'
          stop 1
        else
          value = value * 2.0D+00 / dble ( expon(i) + 1 )
        end if

      end do

      return
      end
      subroutine hexa_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc HEXA_UNIT_MONOMIAL_TEST tests HEXA_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer beta
      integer degree_max
      integer expon(3)
      integer gamma
      double precision hexa_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HEXA_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit hexahedron,'
      write ( *, '(a)' ) 
     &  '  HEXA_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', hexa_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call hexa_unit_monomial ( expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine hexa_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc HEXA_UNIT_QUAD_TEST tests the rules for the unit hexahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2014
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

      integer dim_num
      parameter ( dim_num = 3 )
      integer order_max
      parameter ( order_max = 125 )

      integer degree_max
      integer expon(dim_num)
      integer h
      double precision hexa_unit_volume
      integer i
      integer i4vec_product
      integer k
      logical more
      integer order
      integer order_1d(dim_num)
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HEXA_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit hexahedron,'
      write ( *, '(a)' ) '  we approximate monomial integrals with'
      write ( *, '(a)' ) 
     &  '  HEXA_UNIT_RULE, which returns N1 by N2 by N3 point rules.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        do i = 1, dim_num
          if ( mod ( expon(i), 2 ) .eq. 1 ) then
            if ( .not. more ) then
              go to 20
            else
              go to 10
            end if
          end if
        end do

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        do k = 1, 5

          order_1d(1) = k
          order_1d(2) = k
          order_1d(3) = k
          order = i4vec_product ( dim_num, order_1d )
          call hexa_unit_rule ( order_1d, w, xyz )
          call monomial_value ( dim_num, order, expon, xyz, v )
          quad = hexa_unit_volume ( ) 
     &      * r8vec_dot_product ( order, w, v )
          write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' ) 
     &      order_1d(1:dim_num), quad

        end do
c
c  Try a rule of mixed orders.
c
        order_1d(1) = 3
        order_1d(2) = 5
        order_1d(3) = 2
        order = i4vec_product ( dim_num, order_1d )
        call hexa_unit_rule ( order_1d, w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = hexa_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' )
     &    order_1d(1:dim_num), quad

        write ( *, '(a)' ) ' '
        call hexa_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,6x,2x,6x,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine hexa_unit_rule ( order_1d, w, xyz )

c*********************************************************************72
c
cc HEXA_UNIT_RULE returns a quadrature rule for the unit hexahedron.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c    - 1.0 <= Y <= 1.0
c    - 1.0 <= Z <= 1.0
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
c  Reference:
c
c    Carlos Felippa,
c    A compendium of FEM integration formulas for symbolic work,
c    Engineering Computation,
c    Volume 21, Number 8, 2004, pages 867-890.
c
c  Parameters:
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

      integer dim_num
      parameter ( dim_num = 3 )
      integer order_1d_max
      parameter ( order_1d_max = 5 )

      integer dim
      integer i4vec_product
      integer order
      integer order_1d(dim_num)
      double precision w(order_1d(1)*order_1d(2)*order_1d(3))
      double precision w_1d(order_1d_max)
      double precision x_1d(order_1d_max)
      double precision xyz(3,order_1d(1)*order_1d(2)*order_1d(3))

      order = i4vec_product ( dim_num, order_1d )

      do dim = 1, dim_num

        if ( order_1d(dim) .eq. 1 ) then
          call line_unit_o01 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 2 ) then
          call line_unit_o02 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 3 ) then
          call line_unit_o03 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 4 ) then
          call line_unit_o04 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 5 ) then
          call line_unit_o05 ( w_1d, x_1d )
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'HEXA_UNIT_RULE - Fatal error!'
          write ( *, '(a)' ) '  Illegal value of ORDER_1D(*).'
          stop 1
        end if

        call r8vec_direct_product ( dim, order_1d(dim), x_1d,
     &    dim_num, order, xyz )

        call r8vec_direct_product2 ( dim, order_1d(dim), w_1d,
     &    dim_num, order, w )

      end do

      return
      end
      function hexa_unit_volume ( )

c*********************************************************************72
c
cc HEXA_UNIT_VOLUME: volume of a unit hexahedron.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c    - 1.0 <= Y <= 1.0
c    - 1.0 <= Z <= 1.0
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
c  Parameters:
c
c    Output, double precision HEXA_UNIT_VOLUME, the volume.
c
      implicit none

      double precision hexa_unit_volume

      hexa_unit_volume = 8.0D+00

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
      subroutine line_unit_monomial ( alpha, value )

c*********************************************************************72
c
cc LINE_UNIT_MONOMIAL: monomial integral in a unit line.
c
c  Discussion:
c
c    This function returns the integral of X^ALPHA over the unit line.
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
c    12 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ALPHA, the exponent of X.
c    ALPHA must not be -1.
c
c    Output, double precision value, the integral of the monomial.
c
      implicit none

      integer alpha
      double precision value

      if ( alpha .eq. - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LINE_UNIT_MONOMIAL - Fatal error!'
        write ( *, '(a)' ) '  ALPHA = -1 is not a legal input.'
        stop 1
      else if ( mod ( alpha, 2 ) .eq. 1 ) then
        value = 0.0D+00
      else
        value = 2.0D+00 / dble ( alpha + 1 )
      end if

      return
      end
      subroutine line_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc LINE_UNIT_MONOMIAL_TEST tests LINE_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer degree_max
      double precision line_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit line,'
      write ( *, '(a)' ) 
     &  '  LINE_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', line_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        call line_unit_monomial ( alpha, value )
        write ( *, '(2x,i8,2x,g14.6)' ) alpha, value
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
      double precision line_unit_volume
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
        w(i) = w_save(i) / line_unit_volume ( )
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
      double precision line_unit_volume
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
        w(i) = w_save(i) / line_unit_volume ( )
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
      double precision line_unit_volume
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
        w(i) = w_save(i) / line_unit_volume ( )
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
      double precision line_unit_volume
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
        w(i) = w_save(i) / line_unit_volume ( )
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
      double precision line_unit_volume
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
        w(i) = w_save(i) / line_unit_volume ( )
        x(i) = x_save(i)
      end do

      return
      end
      subroutine line_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc LINE_UNIT_QUAD_TEST tests the rules for the unit line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2008
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

      integer dim_num
      parameter ( dim_num = 1 )
      integer order_max
      parameter ( order_max = 5 )

      integer degree_max
      integer expon(dim_num)
      integer h
      integer i
      double precision line_unit_volume
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision v(order_max)
      double precision w(order_max)
      double precision x(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit line,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  LINE_UNIT_O01, a 1 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O02, a 2 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O03, a 3 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O04, a 4 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O05, a 5 point rule.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        do i = 1, dim_num
          if ( mod ( expon(i), 2 ) .eq. 1 ) then
            if ( .not. more ) then
              go to 20
            else
              go to 10
            end if
          end if
        end do

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2)' ) 
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        order = 1
        call line_unit_o01 ( w, x )
        call monomial_value ( dim_num, order, expon, x, v )
        quad = line_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 2
        call line_unit_o02 ( w, x )
        call monomial_value ( dim_num, order, expon, x, v )
        quad = line_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 3
        call line_unit_o03 ( w, x )
        call monomial_value ( dim_num, order, expon, x, v )
        quad = line_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 4
        call line_unit_o04 ( w, x )
        call monomial_value ( dim_num, order, expon, x, v )
        quad = line_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 5
        call line_unit_o05 ( w, x )
        call monomial_value ( dim_num, order, expon, x, v )
        quad = line_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        write ( *, '(a)' ) ' '
        call line_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      function line_unit_volume ( )

c*********************************************************************72
c
cc LINE_UNIT_VOLUME: volume of a unit line.
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
c  Parameters:
c
c    Output, double precision LINE_UNIT_VOLUME, the volume.
c
      implicit none

      double precision line_unit_volume

      line_unit_volume = 2.0D+00

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
      subroutine pyra_unit_monomial ( expon, value )

c*********************************************************************72
c
cc PYRA_UNIT_MONOMIAL: monomial integral in a unit pyramid.
c
c  Discussion:
c
c    This routine returns the integral of
c
c      product ( 1 <= I <= 3 ) X(I)^EXPON(I)
c
c    over the unit pyramid.
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c  Parameters:
c
c    Input, integer EXPON(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(3)
      integer i
      integer i_hi
      double precision r8_choose
      double precision r8_mop
      double precision value

      value = 0.0D+00

      if ( mod ( expon(1), 2 ) .eq. 0 .and. 
     &     mod ( expon(2), 2 ) .eq. 0 ) then

        i_hi = 2 + expon(1) + expon(2)

        do i = 0, i_hi
          value = value + r8_mop ( i ) * r8_choose ( i_hi, i )
     &      / dble ( i + expon(3) + 1 )
        end do

        value = value
     &    * 2.0D+00 / dble ( expon(1) + 1 )
     &    * 2.0D+00 / dble ( expon(2) + 1 )

      end if

      return
      end
      subroutine pyra_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc PYRA_UNIT_MONOMIAL_TEST tests PYRA_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer beta
      integer degree_max
      integer expon(3)
      integer gamma
      double precision pyra_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRA_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit pyramid,'
      write ( *, '(a)' ) 
     &  '  PYRA_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', pyra_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call pyra_unit_monomial ( expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine pyra_unit_o01 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O01 returns a 1 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
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
c    Output, double precision XYZ(3,1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision xyz(3,order)
      double precision xyz_save(3,1)

      save w_save
      save xyz_save

      data w_save /
     &  1.0D+00 /
      data xyz_save /
     &  0.0D+00, 0.0D+00, 0.25D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o05 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O05 returns a 5 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision XYZ(3,5), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 5 )

      double precision w(order)
      double precision w_save(5)
      double precision xyz(3,order)
      double precision xyz_save(3,5)

      save w_save
      save xyz_save

      data w_save /
     & 0.21093750000000000000D+00, 
     & 0.21093750000000000000D+00, 
     & 0.21093750000000000000D+00, 
     & 0.21093750000000000000D+00, 
     & 0.15625000000000000000D+00 /
      data xyz_save /
     &  -0.48686449556014765641D+00, 
     &  -0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &   0.48686449556014765641D+00, 
     &  -0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &   0.48686449556014765641D+00, 
     &   0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &  -0.48686449556014765641D+00, 
     &   0.48686449556014765641D+00, 
     &   0.16666666666666666667D+00, 
     &   0.00000000000000000000D+00, 
     &   0.00000000000000000000D+00, 
     &   0.70000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o06 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O06 returns a 6 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(6), the weights.
c
c    Output, double precision XYZ(3,6), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 6 )

      double precision w(order)
      double precision w_save(6)
      double precision xyz(3,order)
      double precision xyz_save(3,6)

      save w_save
      save xyz_save

      data w_save /
     & 0.21000000000000000000D+00, 
     & 0.21000000000000000000D+00, 
     & 0.21000000000000000000D+00, 
     & 0.21000000000000000000D+00, 
     & 0.06000000000000000000D+00, 
     & 0.10000000000000000000D+00 /
      data xyz_save /
     &-0.48795003647426658968D+00, 
     &-0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     & 0.48795003647426658968D+00, 
     &-0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     & 0.48795003647426658968D+00, 
     & 0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     &-0.48795003647426658968D+00, 
     & 0.48795003647426658968D+00, 
     & 0.16666666666666666667D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.58333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.75000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o08 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O08 returns an 8 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 8 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.075589411559869072938D+00, 
     & 0.075589411559869072938D+00, 
     & 0.075589411559869072938D+00, 
     & 0.075589411559869072938D+00, 
     & 0.17441058844013092706D+00, 
     & 0.17441058844013092706D+00, 
     & 0.17441058844013092706D+00, 
     & 0.17441058844013092706D+00 /
      data xyz_save /
     &-0.26318405556971359557D+00, 
     &-0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     & 0.26318405556971359557D+00, 
     &-0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     & 0.26318405556971359557D+00, 
     & 0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     &-0.26318405556971359557D+00, 
     & 0.26318405556971359557D+00, 
     & 0.54415184401122528880D+00, 
     &-0.50661630334978742377D+00, 
     &-0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00, 
     & 0.50661630334978742377D+00, 
     &-0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00, 
     & 0.50661630334978742377D+00, 
     & 0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00, 
     &-0.50661630334978742377D+00, 
     & 0.50661630334978742377D+00, 
     & 0.12251482265544137787D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o08b ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O08B returns an 8 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.16438287736328777572D+00, 
     & 0.16438287736328777572D+00, 
     & 0.16438287736328777572D+00, 
     & 0.16438287736328777572D+00, 
     & 0.085617122636712224276D+00, 
     & 0.085617122636712224276D+00, 
     & 0.085617122636712224276D+00, 
     & 0.085617122636712224276D+00 /
      data xyz_save /
     &-0.51197009372656270107D+00, 
     &-0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     & 0.51197009372656270107D+00, 
     &-0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     & 0.51197009372656270107D+00, 
     & 0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     &-0.51197009372656270107D+00, 
     & 0.51197009372656270107D+00, 
     & 0.11024490204163285720D+00, 
     &-0.28415447557052037456D+00, 
     &-0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00, 
     & 0.28415447557052037456D+00, 
     &-0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00, 
     & 0.28415447557052037456D+00, 
     & 0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00, 
     &-0.28415447557052037456D+00, 
     & 0.28415447557052037456D+00, 
     & 0.518326526529795714229D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o09 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O09 returns a 9 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(9), the weights.
c
c    Output, double precision XYZ(3,9), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 9 )

      double precision w(order)
      double precision w_save(9)
      double precision xyz(3,order)
      double precision xyz_save(3,9)

      save w_save
      save xyz_save

      data w_save /
     & 0.13073389672275944791D+00, 
     & 0.13073389672275944791D+00, 
     & 0.13073389672275944791D+00, 
     & 0.13073389672275944791D+00, 
     & 0.10989110327724055209D+00, 
     & 0.10989110327724055209D+00, 
     & 0.10989110327724055209D+00, 
     & 0.10989110327724055209D+00, 
     & 0.03750000000000000000D+00 /
      data xyz_save /
     &-0.52966422253852215131D+00, 
     &-0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     & 0.52966422253852215131D+00, 
     &-0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     & 0.52966422253852215131D+00, 
     & 0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     &-0.52966422253852215131D+00, 
     & 0.52966422253852215131D+00, 
     & 0.08176876558246862335D+00, 
     &-0.34819753825720418039D+00, 
     &-0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     & 0.34819753825720418039D+00, 
     &-0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     & 0.34819753825720418039D+00, 
     & 0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     &-0.34819753825720418039D+00, 
     & 0.34819753825720418039D+00, 
     & 0.400374091560388519511D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.83333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o13 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O13 returns a 13 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(13), the weights.
c
c    Output, double precision XYZ(3,13), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 13 )

      double precision w(order)
      double precision w_save(13)
      double precision xyz(3,order)
      double precision xyz_save(3,13)

      save w_save
      save xyz_save

      data w_save /
     & 0.063061594202898550725D+00, 
     & 0.063061594202898550725D+00, 
     & 0.063061594202898550725D+00, 
     & 0.063061594202898550725D+00, 
     & 0.042101946815575556199D+00, 
     & 0.042101946815575556199D+00, 
     & 0.042101946815575556199D+00, 
     & 0.042101946815575556199D+00, 
     & 0.13172030707666776585D+00, 
     & 0.13172030707666776585D+00, 
     & 0.13172030707666776585D+00, 
     & 0.13172030707666776585D+00, 
     & 0.05246460761943250889D+00 /
      data xyz_save /
     &-0.38510399211870384331D+00, 
     &-0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     & 0.38510399211870384331D+00, 
     &-0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     & 0.38510399211870384331D+00, 
     & 0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     &-0.38510399211870384331D+00, 
     & 0.38510399211870384331D+00, 
     &0.428571428571428571429D+00, 
     &-0.40345831960728204766D+00, 
     & 0.00000000000000000000D+00, 
     &0.33928571428571428571D+00,
     & 0.40345831960728204766D+00, 
     & 0.00000000000000000000D+00, 
     &0.33928571428571428571D+00,
     & 0.00000000000000000000D+00, 
     &-0.40345831960728204766D+00, 
     &0.33928571428571428571D+00,
     & 0.00000000000000000000D+00, 
     & 0.40345831960728204766D+00, 
     &0.33928571428571428571D+00,
     &-0.53157877436961973359D+00, 
     &-0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     & 0.53157877436961973359D+00, 
     &-0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     & 0.53157877436961973359D+00, 
     & 0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     &-0.53157877436961973359D+00, 
     & 0.53157877436961973359D+00, 
     &0.08496732026143790850D+00,
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.76219701803768503595D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o18 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O18 returns an 18 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(18), the weights.
c
c    Output, double precision XYZ(3,18), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 18 )

      double precision w(order)
      double precision w_save(18)
      double precision xyz(3,order)
      double precision xyz_save(3,18)

      save w_save
      save xyz_save

      data w_save /
     & 0.023330065296255886709D+00, 
     & 0.037328104474009418735D+00, 
     & 0.023330065296255886709D+00, 
     & 0.037328104474009418735D+00, 
     & 0.059724967158415069975D+00, 
     & 0.037328104474009418735D+00, 
     & 0.023330065296255886709D+00, 
     & 0.037328104474009418735D+00, 
     & 0.023330065296255886709D+00, 
     & 0.05383042853090460712D+00, 
     & 0.08612868564944737139D+00, 
     & 0.05383042853090460712D+00, 
     & 0.08612868564944737139D+00, 
     & 0.13780589703911579422D+00, 
     & 0.08612868564944737139D+00, 
     & 0.05383042853090460712D+00, 
     & 0.08612868564944737139D+00, 
     & 0.05383042853090460712D+00 /
      data xyz_save /
     &-0.35309846330877704481D+00, 
     &-0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.00000000000000000000D+00, 
     &-0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.35309846330877704481D+00, 
     &-0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     &-0.35309846330877704481D+00, 
     & 0.00000000000000000000D+00, 
     &0.544151844011225288800D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.544151844011225288800D+00, 
     & 0.35309846330877704481D+00, 
     & 0.00000000000000000000D+00, 
     &0.544151844011225288800D+00, 
     &-0.35309846330877704481D+00, 
     & 0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.00000000000000000000D+00, 
     & 0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     & 0.35309846330877704481D+00, 
     & 0.35309846330877704481D+00, 
     &0.544151844011225288800D+00, 
     &-0.67969709567986745790D+00, 
     &-0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.00000000000000000000D+00, 
     &-0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.67969709567986745790D+00, 
     &-0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     &-0.67969709567986745790D+00, 
     & 0.00000000000000000000D+00, 
     &0.12251482265544137787D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.12251482265544137787D+00, 
     & 0.67969709567986745790D+00, 
     & 0.00000000000000000000D+00, 
     &0.12251482265544137787D+00, 
     &-0.67969709567986745790D+00, 
     & 0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.00000000000000000000D+00, 
     & 0.67969709567986745790D+00, 
     &0.12251482265544137787D+00, 
     & 0.67969709567986745790D+00, 
     & 0.67969709567986745790D+00, 
     &0.12251482265544137787D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o27 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O27 returns a 27 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(27), the weights.
c
c    Output, double precision XYZ(3,27), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 27 )

      double precision w(order)
      double precision w_save(27)
      double precision xyz(3,order)
      double precision xyz_save(3,27)

      save w_save
      save xyz_save

      data w_save /
     & 0.036374157653908938268D+00, 
     & 0.05819865224625430123D+00, 
     & 0.036374157653908938268D+00, 
     & 0.05819865224625430123D+00, 
     & 0.09311784359400688197D+00, 
     & 0.05819865224625430123D+00, 
     & 0.036374157653908938268D+00, 
     & 0.05819865224625430123D+00, 
     & 0.036374157653908938268D+00, 
     & 0.033853303069413431019D+00, 
     & 0.054165284911061489631D+00, 
     & 0.033853303069413431019D+00, 
     & 0.054165284911061489631D+00, 
     & 0.08666445585769838341D+00, 
     & 0.054165284911061489631D+00, 
     & 0.033853303069413431019D+00, 
     & 0.054165284911061489631D+00, 
     & 0.033853303069413431019D+00, 
     & 0.006933033103838124540D+00, 
     & 0.011092852966140999264D+00, 
     & 0.006933033103838124540D+00, 
     & 0.011092852966140999264D+00, 
     & 0.017748564745825598822D+00, 
     & 0.011092852966140999264D+00, 
     & 0.006933033103838124540D+00, 
     & 0.011092852966140999264D+00, 
     & 0.006933033103838124540D+00 /
      data xyz_save /
     &-0.7180557413198889387D+00, 
     & -0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.00000000000000000000D+00, 
     &-0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.7180557413198889387D+00, 
     & -0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     &-0.7180557413198889387D+00, 
     & 0.00000000000000000000D+00, 
     &0.07299402407314973216D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.07299402407314973216D+00, 
     & 0.7180557413198889387D+00, 
     & 0.00000000000000000000D+00, 
     &0.07299402407314973216D+00, 
     &-0.7180557413198889387D+00, 
     & 0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.00000000000000000000D+00, 
     & 0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     & 0.7180557413198889387D+00, 
     & 0.7180557413198889387D+00, 
     & 0.07299402407314973216D+00, 
     &-0.50580870785392503961D+00, 
     &-0.50580870785392503961D+00, 
     & 0.34700376603835188472D+00, 
     & 0.00000000000000000000D+00, 
     &-0.50580870785392503961D+00, 
     & 0.34700376603835188472D+00, 
     & 0.50580870785392503961D+00, 
     &-0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     &-0.50580870785392503961D+00, 
     & 0.00000000000000000000D+00, 
     &0.34700376603835188472D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     &0.34700376603835188472D+00, 
     & 0.50580870785392503961D+00, 
     & 0.00000000000000000000D+00, 
     &0.34700376603835188472D+00, 
     &-0.50580870785392503961D+00, 
     & 0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     & 0.50580870785392503961D+00, 
     & 0.50580870785392503961D+00, 
     &0.34700376603835188472D+00, 
     &-0.22850430565396735360D+00, 
     &-0.22850430565396735360D+00, 
     &0.70500220988849838312D+00, 
     & 0.00000000000000000000D+00, 
     &-0.22850430565396735360D+00, 
     &0.70500220988849838312D+00, 
     & 0.22850430565396735360D+00, 
     &-0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00, 
     &-0.22850430565396735360D+00, 
     & 0.00000000000000000000D+00, 
     & 0.70500220988849838312D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.70500220988849838312D+00, 
     & 0.22850430565396735360D+00, 
     & 0.00000000000000000000D+00, 
     & 0.70500220988849838312D+00, 
     &-0.22850430565396735360D+00, 
     & 0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00, 
     & 0.00000000000000000000D+00, 
     & 0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00, 
     & 0.22850430565396735360D+00, 
     & 0.22850430565396735360D+00, 
     & 0.70500220988849838312D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_o48 ( w, xyz )

c*********************************************************************72
c
cc PYRA_UNIT_O48 returns a 48 point quadrature rule for the unit pyramid.
c
c  Discussion:
c
c    The integration region is:
c
c    - ( 1 - Z ) <= X <= 1 - Z
c    - ( 1 - Z ) <= Y <= 1 - Z
c              0 <= Z <= 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c  Parameters:
c
c    Output, double precision W(48), the weights.
c
c    Output, double precision XYZ(3,48), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 48 )

      double precision w(order)
      double precision w_save(48)
      double precision xyz(3,order)
      double precision xyz_save(3,48)

      save w_save
      save xyz_save

      data w_save /
     &2.01241939442682455D-002, 
     &2.01241939442682455D-002, 
     &2.01241939442682455D-002, 
     &2.01241939442682455D-002, 
     &2.60351137043010779D-002, 
     &2.60351137043010779D-002, 
     &2.60351137043010779D-002, 
     &2.60351137043010779D-002, 
     &1.24557795239745531D-002, 
     &1.24557795239745531D-002, 
     &1.24557795239745531D-002, 
     &1.24557795239745531D-002, 
     &1.87873998794808156D-003, 
     &1.87873998794808156D-003, 
     &1.87873998794808156D-003, 
     &1.87873998794808156D-003, 
     &4.32957927807745280D-002, 
     &4.32957927807745280D-002, 
     &4.32957927807745280D-002, 
     &4.32957927807745280D-002, 
     &1.97463249834127288D-002, 
     &1.97463249834127288D-002, 
     &1.97463249834127288D-002, 
     &1.97463249834127288D-002, 
     &5.60127223523590526D-002, 
     &5.60127223523590526D-002, 
     &5.60127223523590526D-002, 
     &5.60127223523590526D-002, 
     &2.55462562927473852D-002, 
     &2.55462562927473852D-002, 
     &2.55462562927473852D-002, 
     &2.55462562927473852D-002, 
     &2.67977366291788643D-002, 
     &2.67977366291788643D-002, 
     &2.67977366291788643D-002, 
     &2.67977366291788643D-002, 
     &1.22218992265373354D-002, 
     &1.22218992265373354D-002, 
     &1.22218992265373354D-002, 
     &1.22218992265373354D-002, 
     &4.04197740453215038D-003, 
     &4.04197740453215038D-003, 
     &4.04197740453215038D-003, 
     &4.04197740453215038D-003, 
     &1.84346316995826843D-003, 
     &1.84346316995826843D-003, 
     &1.84346316995826843D-003, 
     &1.84346316995826843D-003 /
      data xyz_save /
     &0.88091731624450909D+00, 
     & 0.0000000000000000D+00, 
     & 4.85005494469969989D-02, 
     & -0.88091731624450909D+00, 
     & 0.0000000000000000D+00, 
     & 4.85005494469969989D-02, 
     & 0.0000000000000000D+00, 
     & 0.88091731624450909D+00, 
     &4.85005494469969989D-02, 
     & 0.0000000000000000D+00, 
     &-0.88091731624450909D+00, 
     &4.85005494469969989D-02, 
     &0.70491874112648223D+00, 
     & 0.0000000000000000D+00, 
     & 0.23860073755186201D+00, 
     & -0.70491874112648223D+00, 
     & 0.0000000000000000D+00, 
     & 0.23860073755186201D+00, 
     & 0.0000000000000000D+00, 
     & 0.70491874112648223D+00, 
     &0.23860073755186201D+00, 
     & 0.0000000000000000D+00, 
     &-0.70491874112648223D+00, 
     &0.23860073755186201D+00, 
     &0.44712732143189760D+00, 
     & 0.0000000000000000D+00, 
     & 0.51704729510436798D+00, 
     & -0.44712732143189760D+00, 
     & 0.0000000000000000D+00, 
     & 0.51704729510436798D+00, 
     & 0.0000000000000000D+00, 
     & 0.44712732143189760D+00, 
     &0.51704729510436798D+00, 
     & 0.0000000000000000D+00, 
     &-0.44712732143189760D+00, 
     &0.51704729510436798D+00, 
     &0.18900486065123448D+00, 
     & 0.0000000000000000D+00, 
     & 0.79585141789677305D+00, 
     & -0.18900486065123448D+00, 
     & 0.0000000000000000D+00, 
     & 0.79585141789677305D+00, 
     & 0.0000000000000000D+00, 
     & 0.18900486065123448D+00, 
     &0.79585141789677305D+00, 
     & 0.0000000000000000D+00, 
     &-0.18900486065123448D+00, 
     &0.79585141789677305D+00, 
     &0.36209733410322176D+00, 
     & 0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     & -0.36209733410322176D+00, 
     & 0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     & -0.36209733410322176D+00, 
     &-0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     &0.36209733410322176D+00, 
     &-0.36209733410322176D+00, 
     &4.85005494469969989D-02, 
     &0.76688932060387538D+00, 
     & 0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     & -0.76688932060387538D+00, 
     & 0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     & -0.76688932060387538D+00, 
     &-0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     &0.76688932060387538D+00, 
     &-0.76688932060387538D+00, 
     &4.85005494469969989D-02, 
     &0.28975386476618070D+00, 
     & 0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     & -0.28975386476618070D+00, 
     & 0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     & -0.28975386476618070D+00, 
     &-0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     &0.28975386476618070D+00, 
     &-0.28975386476618070D+00, 
     &0.23860073755186201D+00, 
     &0.61367241226233160D+00, 
     & 0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     & -0.61367241226233160D+00, 
     & 0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     & -0.61367241226233160D+00, 
     &-0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     &0.61367241226233160D+00, 
     &-0.61367241226233160D+00, 
     &0.23860073755186201D+00, 
     &0.18378979287798017D+00, 
     & 0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     & -0.18378979287798017D+00, 
     & 0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     & -0.18378979287798017D+00, 
     &-0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     &0.18378979287798017D+00, 
     &-0.18378979287798017D+00, 
     &0.51704729510436798D+00, 
     &0.38925011625173161D+00, 
     & 0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     & -0.38925011625173161D+00, 
     & 0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     & -0.38925011625173161D+00, 
     &-0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     &0.38925011625173161D+00, 
     &-0.38925011625173161D+00, 
     &0.51704729510436798D+00, 
     &7.76896479525748113D-02, 
     & 7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     & -7.76896479525748113D-02, 
     & 7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     & -7.76896479525748113D-02, 
     &-7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     &7.76896479525748113D-02, 
     &-7.76896479525748113D-02, 
     &0.79585141789677305D+00, 
     &0.16453962988669860D+00, 
     & 0.16453962988669860D+00, 
     &0.79585141789677305D+00, 
     & -0.16453962988669860D+00, 
     & 0.16453962988669860D+00, 
     &0.79585141789677305D+00, 
     & -0.16453962988669860D+00, 
     &-0.16453962988669860D+00, 
     &0.79585141789677305D+00, 
     &0.16453962988669860D+00, 
     &-0.16453962988669860D+00, 
     &0.79585141789677305D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine pyra_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc PYRA_UNIT_QUAD_TEST tests the rules for the unit pyramid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2008
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

      integer dim_num
      parameter ( dim_num = 3 )
      integer order_max
      parameter ( order_max = 48 )

      integer degree_max
      integer expon(dim_num)
      integer h
      logical more
      integer order
      double precision quad
      integer t
      double precision pyra_unit_volume
      double precision r8vec_dot_product
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRA_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit pyramid,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  PYRA_UNIT_O01,'
      write ( *, '(a)' ) '  PYRA_UNIT_O05,'
      write ( *, '(a)' ) '  PYRA_UNIT_O06,'
      write ( *, '(a)' ) '  PYRA_UNIT_O08,'
      write ( *, '(a)' ) '  PYRA_UNIT_O08b,'
      write ( *, '(a)' ) '  PYRA_UNIT_O09,'
      write ( *, '(a)' ) '  PYRA_UNIT_O13,'
      write ( *, '(a)' ) '  PYRA_UNIT_O18,'
      write ( *, '(a)' ) '  PYRA_UNIT_O27,'
      write ( *, '(a)' ) '  PYRA_UNIT_O48.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        if ( mod ( expon(1), 2 ) .eq. 1 .or.
     &       mod ( expon(2), 2 ) .eq. 1 ) then
          go to 10
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        order = 1
        call pyra_unit_o01 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 5
        call pyra_unit_o05 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 6
        call pyra_unit_o06 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 8
        call pyra_unit_o08 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 8
        call pyra_unit_o08b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 9
        call pyra_unit_o09 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 13
        call pyra_unit_o13 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 18
        call pyra_unit_o18 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 27
        call pyra_unit_o27 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 48
        call pyra_unit_o48 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = pyra_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        write ( *, '(a)' ) ' '
        call pyra_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      function pyra_unit_volume ( )

c*********************************************************************72
c
cc PYRA_UNIT_VOLUME: volume of a unit pyramid with square base.
c
c  Discussion:
c
c    The volume of this unit pyramid is 4/3.
c
c    The integration region is:
c
c      - ( 1 - Z ) <= X <= 1 - Z
c      - ( 1 - Z ) <= Y <= 1 - Z
c                0 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision PYRA_UNIT_VOLUME, the volume.
c
      implicit none

      double precision pyra_unit_volume

      pyra_unit_volume = 4.0D+00 / 3.0D+00

      return
      end
      subroutine quad_unit_monomial ( expon, value )

c*********************************************************************72
c
cc QUAD_UNIT_MONOMIAL integrates a monomial over the unit quadrilateral.
c
c  Discussion:
c
c    This routine integrates a monomial of the form
c
c      product ( 1 <= dim <= 2 ) x(dim)^expon(dim)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c    - 1.0 <= Y <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON(2), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(2)
      integer i
      double precision value

      value = 1.0D+00

      do i = 1, 2

        if ( mod ( expon(i), 2 ) .eq. 1 ) then
          value = 0.0D+00
        else if ( expon(i) .eq. -1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD_UNIT_MONOMIAL - Fatal error!'
          write ( *, '(a)' ) '  Exponent of -1 encountered.'
          stop 1
        else
          value = value * 2.0D+00 / dble ( expon(i) + 1 )
        end if

      end do

      return
      end
      subroutine quad_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc QUAD_UNIT_MONOMIAL_TEST tests QUAD_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer beta
      integer degree_max
      integer expon(2)
      double precision quad_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUAD_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit quadrilateral,'
      write ( *, '(a)' ) 
     &  '  QUAD_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', quad_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          call quad_unit_monomial ( expon, value )
          write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) expon(1:2), value
        end do
      end do

      return
      end
      subroutine quad_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc QUAD_UNIT_QUAD_TEST tests the rules for the unit quadrilateral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 April 2008
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

      integer dim_num
      parameter ( dim_num = 2 )
      integer order_max
      parameter ( order_max = 25 )

      integer degree_max
      integer expon(dim_num)
      integer h
      integer i
      integer i4vec_product
      integer k
      logical more
      integer order
      integer order_1d(dim_num)
      double precision quad
      double precision quad_unit_volume
      double precision r8vec_dot_product
      integer t
      double precision v(order_max)
      double precision w(order_max)
      double precision xy(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUAD_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit quadrilateral,'
      write ( *, '(a)' ) '  we approximate monomial integrals with'
      write ( *, '(a)' ) 
     &  '  QUAD_UNIT_RULE, which returns M by N point rules.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        do i = 1, dim_num
          if ( mod ( expon(i), 2 ) .eq. 1 ) then
            if ( .not. more ) then
              go to 20
            else
              go to 10
            end if
          end if
        end do

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2)' ) 
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        do k = 1, 5

          order_1d(1:dim_num) = k
          order = i4vec_product ( dim_num, order_1d )
          call quad_unit_rule ( order_1d, w, xy )
          call monomial_value ( dim_num, order, expon, xy, v )
          quad = quad_unit_volume ( ) * 
     &      r8vec_dot_product ( order, w, v )
          write ( *, '(2x,i6,2x,i6,2x,g14.6)' ) 
     &      order_1d(1:dim_num), quad

        end do
c
c  Try a rule of mixed orders.
c
        order_1d(1) = 3
        order_1d(2) = 5
        order = i4vec_product ( dim_num, order_1d )
        call quad_unit_rule ( order_1d, w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = quad_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,i6,2x,g14.6)' ) order_1d(1:dim_num), quad

        write ( *, '(a)' ) ' '
        call quad_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,6x,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine quad_unit_rule ( order_1d, w, xy )

c*********************************************************************72
c
cc QUAD_UNIT_RULE returns a quadrature rule for the unit quadrilateral.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c    - 1.0 <= Y <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 April 2009
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
c    Input, integer ORDER_1D(2), the order of the rule in
c    each dimension.  1 <= ORDER_1D(I) <= 5.
c
c    Output, double precision W(ORDER_1D(1)*ORDER_1D(2)), the weights.
c
c    Output, double precision XY(2,ORDER_1D(1)*ORDER_1D(2)), the abscissas.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer order_1d_max
      parameter ( order_1d_max = 5 )

      integer dim
      integer i4vec_product
      integer order
      integer order_1d(2)
      double precision w(order_1d(1)*order_1d(2))
      double precision w_1d(order_1d_max)
      double precision x_1d(order_1d_max)
      double precision xy(2,order_1d(1)*order_1d(2))

      order = i4vec_product ( dim_num, order_1d )
      
      do dim = 1, dim_num

        if ( order_1d(dim) .eq. 1 ) then
          call line_unit_o01 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 2 ) then
          call line_unit_o02 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 3 ) then
          call line_unit_o03 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 4 ) then
          call line_unit_o04 ( w_1d, x_1d )
        else if ( order_1d(dim) .eq. 5 ) then
          call line_unit_o05 ( w_1d, x_1d )
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD_UNIT_RULE - Fatal error!'
          write ( *, '(a)' ) '  Illegal value of ORDER_1D(*).'
          stop 1
        end if

        call r8vec_direct_product ( dim, order_1d(dim), x_1d,
     &    dim_num, order, xy )

        call r8vec_direct_product2 ( dim, order_1d(dim), w_1d,
     &    dim_num, order, w )

      end do

      return
      end
      function quad_unit_volume ( )

c*********************************************************************72
c
cc QUAD_UNIT_VOLUME: volume of a unit quadrilateral.
c
c  Discussion:
c
c    The integration region is:
c
c    - 1.0 <= X <= 1.0
c    - 1.0 <= Y <= 1.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision QUAD_UNIT_VOLUME, the volume.
c
      implicit none

      double precision quad_unit_volume

      quad_unit_volume = 4.0D+00

      return
      end
      function r8_choose ( n, k )

c*********************************************************************72
c
cc R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in R8 arithmetic.
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
c    07 June 2008
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
c    Output, double precision R8_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer k
      integer mn
      integer mx
      integer n
      double precision r8_choose
      double precision value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0.0D+00

      else if ( mn .eq. 0 ) then

        value = 1.0D+00

      else

        mx = max ( k, n - k )
        value = dble ( mx + 1 )

        do i = 2, mn
          value = ( value * dble ( mx + i ) ) / dble ( i )
        end do

      end if

      r8_choose = value

      return
      end
      function r8_mop ( i )

c*********************************************************************72
c
cc R8_MOP returns the I-th power of -1 as an R8.
c
c  Discussion:
c
c    An R8 is a double precision real value.
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
c    Input, integer I, the power of -1.
c
c    Output, double precision R8_MOP, the I-th power of -1.
c
      implicit none

      integer i
      double precision r8_mop
      double precision value

      if ( mod ( i, 2 ) .eq. 0 ) then
        value = + 1.0D+00
      else
        value = - 1.0D+00
      end if

      r8_mop = value

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
      function r8mat_det_4d ( a )

c*********************************************************************72
c
cc R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
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
c    31 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(4,4), the matrix whose determinant is desired.
c
c    Output, double precision R8MAT_DET_4D, the determinant of the matrix.
c
      implicit none

      double precision a(4,4)
      double precision r8mat_det_4d

      r8mat_det_4d =
     &       a(1,1) * (
     &           a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) )
     &     - a(1,2) * (
     &           a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) )
     &     + a(1,3) * (
     &           a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )
     &     - a(1,4) * (
     &           a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) )
     &         + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( * ) title

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
      subroutine tetr_unit_monomial ( expon, value )

c*********************************************************************72
c
cc TETR_UNIT_MONOMIAL integrates a monomial over the unit tetrahedron.
c
c  Discussion:
c
c    This routine integrates a monomial of the form
c
c      product ( 1 <= dim <= 3 ) x(dim)^expon(dim)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c    Integral ( over unit tetrahedron ) x^l y^m z^n dx dy =
c    l! * m! * n! / ( m + n + 3 )!
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      0 <= X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(3)
      integer i
      integer k
      double precision value
c
c  The first computation ends with VALUE = 1.0;
c
      value = 1.0D+00
c
c  The first loop simply calculates 1, so we short circuit it.
c
      k = expon(1)
      do i = 1, expon(2)
        k = k + 1
        value = value * dble ( i ) / real ( k )
      end do

      do i = 1, expon(3)
        k = k + 1
        value = value * dble ( i ) / real ( k )
      end do

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )

      return
      end
      subroutine tetr_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc TETR_UNIT_MONOMIAL_TEST tests TETR_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer beta
      integer degree_max
      integer expon(3)
      integer gamma
      double precision tetr_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETR_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit tetrahedron,'
      write ( *, '(a)' ) 
     &  '  TETR_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', tetr_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call tetr_unit_monomial ( expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine tetr_unit_o01 ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O01 returns a 1 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
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
c    Output, double precision XYZ(3,1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision xyz(3,order)
      double precision xyz_save(3,1)

      data w_save /
     &  1.0000000000000000000D+00 /
      data xyz_save /
     &  0.25000000000000000000D+00,  
     &  0.25000000000000000000D+00,  
     &  0.25000000000000000000D+00 /

      save w_save
      save xyz_save

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o04 ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O04 returns a 4 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
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
c    Output, double precision XYZ(3,4), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 4 )

      double precision w(order)
      double precision w_save(4)
      double precision xyz(3,order)
      double precision xyz_save(3,4)

      save w_save
      save xyz_save

      data w_save /
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00 /
      data xyz_save /
     & 0.58541019662496845446D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.58541019662496845446D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.58541019662496845446D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00, 
     & 0.13819660112501051518D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o08 ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O08 returns an 8 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
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
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 8 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.13852796651186214232D+00, 
     & 0.13852796651186214232D+00, 
     & 0.13852796651186214232D+00, 
     & 0.13852796651186214232D+00, 
     & 0.11147203348813785768D+00, 
     & 0.11147203348813785768D+00, 
     & 0.11147203348813785768D+00, 
     & 0.11147203348813785768D+00 /
      data xyz_save /
     & 0.015835909865720057993D+00, 
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.015835909865720057993D+00, 
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.015835909865720057993D+00, 
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.32805469671142664734D+00,
     & 0.67914317820120795168D+00,
     & 0.10695227393293068277D+00, 
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.67914317820120795168D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.67914317820120795168D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00,
     & 0.10695227393293068277D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o08b ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O08B returns an 8 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
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
c    Output, double precision W(8), the weights.
c
c    Output, double precision XYZ(3,8), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 8 )

      double precision w(order)
      double precision w_save(8)
      double precision xyz(3,order)
      double precision xyz_save(3,8)

      save w_save
      save xyz_save

      data w_save /
     & 0.025000000000000000000D+00, 
     & 0.025000000000000000000D+00, 
     & 0.025000000000000000000D+00, 
     & 0.025000000000000000000D+00, 
     & 0.22500000000000000000D+00, 
     & 0.22500000000000000000D+00, 
     & 0.22500000000000000000D+00, 
     & 0.22500000000000000000D+00 /
      data xyz_save /
     & 1.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 1.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 1.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o14 ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O14 returns a 14 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
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
c    Output, double precision W(ORDER), the weights.
c
c    Output, double precision XYZ(3,ORDER), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 14 )

      double precision w(order)
      double precision w_save(14)
      double precision xyz(3,order)
      double precision xyz_save(3,14)

      save w_save
      save xyz_save

      data w_save /
     & 0.073493043116361949544D+00, 
     & 0.073493043116361949544D+00, 
     & 0.073493043116361949544D+00, 
     & 0.073493043116361949544D+00, 
     & 0.11268792571801585080D+00, 
     & 0.11268792571801585080D+00, 
     & 0.11268792571801585080D+00, 
     & 0.11268792571801585080D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00, 
     & 0.042546020777081466438D+00 /
      data xyz_save /
     & 0.72179424906732632079D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.72179424906732632079D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.72179424906732632079D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.092735250310891226402D+00, 
     & 0.067342242210098170608D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.067342242210098170608D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.067342242210098170608D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.31088591926330060980D+00, 
     & 0.045503704125649649492D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.45449629587435035051D+00, 
     & 0.045503704125649649492D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o14b ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O14B returns a 14 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
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
c    Output, double precision W(14), the weights.
c
c    Output, double precision XYZ(3,14), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 14 )

      double precision w(order)
      double precision w_save(14)
      double precision xyz(3,order)
      double precision xyz_save(3,14)

      save w_save
      save xyz_save

      data w_save /
     & 0.13283874668559071814D+00, 
     & 0.13283874668559071814D+00, 
     & 0.13283874668559071814D+00, 
     & 0.13283874668559071814D+00, 
     & 0.088589824742980710434D+00, 
     & 0.088589824742980710434D+00, 
     & 0.088589824742980710434D+00, 
     & 0.088589824742980710434D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00, 
     & 0.019047619047619047619D+00 /
      data xyz_save /
     & 0.056881379520423421748D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.056881379520423421748D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.056881379520423421748D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.31437287349319219275D+00, 
     & 0.69841970432438656092D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.69841970432438656092D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.69841970432438656092D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.10052676522520447969D+00, 
     & 0.50000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.00000000000000000000D+00, 
     & 0.50000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o15 ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O15 returns a 15 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
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
c    Output, double precision W(15), the weights.
c
c    Output, double precision XYZ(3,15), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 15 )

      double precision w(order)
      double precision w_save(15)
      double precision xyz(3,order)
      double precision xyz_save(3,15)

      save w_save
      save xyz_save

      data w_save /
     & 0.071937083779018620010D+00, 
     & 0.071937083779018620010D+00, 
     & 0.071937083779018620010D+00, 
     & 0.071937083779018620010D+00, 
     & 0.069068207226272385281D+00, 
     & 0.069068207226272385281D+00, 
     & 0.069068207226272385281D+00, 
     & 0.069068207226272385281D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.052910052910052910053D+00, 
     & 0.11851851851851851852D+00 /
      data xyz_save /
     & 0.72408676584183090163D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.72408676584183090163D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.72408676584183090163D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.091971078052723032789D+00, 
     & 0.040619116511110274837D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.040619116511110274837D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.040619116511110274837D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.31979362782962990839D+00, 
     & 0.44364916731037084426D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.056350832689629155741D+00, 
     & 0.44364916731037084426D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o15b ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O15B returns a 15 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2009
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
c    Output, double precision W(15), the weights.
c
c    Output, double precision XYZ(3,15), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 15 )

      double precision w(order)
      double precision w_save(15)
      double precision xyz(3,order)
      double precision xyz_save(3,15)

      save w_save
      save xyz_save

      data w_save /
     & 0.036160714285714285714D+00, 
     & 0.036160714285714285714D+00, 
     & 0.036160714285714285714D+00, 
     & 0.036160714285714285714D+00, 
     & 0.069871494516173816465D+00, 
     & 0.069871494516173816465D+00, 
     & 0.069871494516173816465D+00, 
     & 0.069871494516173816465D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.065694849368318756074D+00, 
     & 0.18170206858253505484D+00 /
      data xyz_save /
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.00000000000000000000D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.33333333333333333333D+00, 
     & 0.72727272727272727273D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.72727272727272727273D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.72727272727272727273D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.090909090909090909091D+00, 
     & 0.43344984642633570176D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.066550153573664298240D+00, 
     & 0.43344984642633570176D+00, 
     & 0.25000000000000000000D+00, 
     & 0.25000000000000000000D+00, 
     & 0.250000000000000000D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_o24 ( w, xyz )

c*********************************************************************72
c
cc TETR_UNIT_O24 returns a 24 point quadrature rule for the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      0 <= Z
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2009
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
c    Output, double precision W(24), the weights.
c
c    Output, double precision XYZ(3,24), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 24 )

      double precision w(order)
      double precision w_save(24)
      double precision xyz(3,order)
      double precision xyz_save(3,24)

      save w_save
      save xyz_save

      data w_save /
     & 0.039922750257869636194D+00, 
     & 0.039922750257869636194D+00, 
     & 0.039922750257869636194D+00, 
     & 0.039922750257869636194D+00, 
     & 0.010077211055345822612D+00, 
     & 0.010077211055345822612D+00, 
     & 0.010077211055345822612D+00, 
     & 0.010077211055345822612D+00, 
     & 0.055357181543927398338D+00, 
     & 0.055357181543927398338D+00, 
     & 0.055357181543927398338D+00, 
     & 0.055357181543927398338D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00, 
     & 0.048214285714285714286D+00 /
      data xyz_save /
     & 0.35619138622025439121D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.35619138622025439121D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.35619138622025439121D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.21460287125991520293D+00, 
     & 0.87797812439616594065D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.87797812439616594065D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.87797812439616594065D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.040673958534611353116D+00, 
     & 0.032986329573173468968D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.032986329573173468968D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.032986329573173468968D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.32233789014227551034D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.60300566479164914137D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.60300566479164914137D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.063661001875017525299D+00, 
     & 0.26967233145831580803D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 3, order, xyz_save, xyz )

      return
      end
      subroutine tetr_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc TETR_UNIT_QUAD_TEST tests the rules for the unit tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2008
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

      integer dim_num
      parameter ( dim_num = 3 )
      integer order_max
      parameter ( order_max = 24 )

      integer degree_max
      integer expon(dim_num)
      integer h
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision tetr_unit_volume
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETR_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit tetrahedron,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  TETR_UNIT_O01,'
      write ( *, '(a)' ) '  TETR_UNIT_O04,'
      write ( *, '(a)' ) '  TETR_UNIT_O08,'
      write ( *, '(a)' ) '  TETR_UNIT_O08b,'
      write ( *, '(a)' ) '  TETR_UNIT_O14,'
      write ( *, '(a)' ) '  TETR_UNIT_O14b,'
      write ( *, '(a)' ) '  TETR_UNIT_O15,'
      write ( *, '(a)' ) '  TETR_UNIT_O15b,'
      write ( *, '(a)' ) '  TETR_UNIT_O24.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        order = 1
        call tetr_unit_o01 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 4
        call tetr_unit_o04 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 8
        call tetr_unit_o08 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 8
        call tetr_unit_o08b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 14
        call tetr_unit_o14 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 14
        call tetr_unit_o14b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 15
        call tetr_unit_o15 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 15
        call tetr_unit_o15b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 24
        call tetr_unit_o24 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetr_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        write ( *, '(a)' ) ' '
        call tetr_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      function tetr_unit_volume ( )

c*********************************************************************72
c
cc TETR_UNIT_VOLUME returns the volume of the unit tetrahedron.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X,
c      0 <= Y,
c      0 <= Z,
c      X + Y + Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision TETR_UNIT_VOLUME, the volume.
c
      implicit none

      double precision tetr_unit_volume

      tetr_unit_volume = 1.0D+00 / 6.0D+00

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
      subroutine trig_unit_monomial ( expon, value )

c*********************************************************************72
c
cc TRIG_UNIT_MONOMIAL integrates a monomial over the unit triangle.
c
c  Discussion:
c
c    This routine integrates a monomial of the form
c
c      product ( 1 <= dim <= 2 ) x(dim)^expon(dim)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c    Integral ( over unit triangle ) x^m y^n dx dy = m! * n! / ( m + n + 2 )!
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON(2), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(2)
      integer i
      integer k
      double precision value
c
c  The first computation ends with VALUE = 1.0;
c
      value = 1.0D+00

      k = expon(1)

      do i = 1, expon(2)
        k = k + 1
        value = value * dble ( i ) / dble ( k )
      end do

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )

      return
      end
      subroutine trig_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc TRIG_UNIT_MONOMIAL_TEST tests TRIG_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer beta
      integer degree_max
      integer expon(2)
      double precision trig_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIG_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit triangle,'
      write ( *, '(a)' ) 
     &  '  TRIG_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', trig_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          call trig_unit_monomial ( expon, value )
          write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) expon(1:2), value
        end do
      end do

      return
      end
      subroutine trig_unit_o01 ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O01 returns a 1 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 1.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision XY(2,1), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 1 )

      double precision w(order)
      double precision w_save(1)
      double precision xy(2,order)
      double precision xy_save(2,1)

      save w_save
      save xy_save

      data w_save /
     &   1.0D+00 /
      data xy_save /
     &   0.33333333333333333333D+00, 
     &   0.33333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_o03 ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O03 returns a 3 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 2.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision XY(2,3), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 3 )

      double precision w(order)
      double precision w_save(3)
      double precision xy(2,order)
      double precision xy_save(2,3)

      save w_save
      save xy_save

      data w_save /
     &   0.33333333333333333333D+00, 
     &   0.33333333333333333333D+00, 
     &   0.33333333333333333333D+00 /
      data xy_save /
     &   0.66666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.66666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.16666666666666666667D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_o03b ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O03B returns a 3 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 2.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision XY(2,3), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 3 )

      double precision w(order)
      double precision w_save(3)
      double precision xy(2,order)
      double precision xy_save(2,3)

      save w_save
      save xy_save

      data w_save / 
     &   0.33333333333333333333D+00, 
     &   0.33333333333333333333D+00, 
     &   0.33333333333333333333D+00 /
      data xy_save /
     &   0.0D+00, 
     &   0.5D+00, 
     &   0.5D+00, 
     &   0.0D+00, 
     &   0.5D+00, 
     &   0.5D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_o06 ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O06 returns a 6 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 4.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(6), the weights.
c
c    Output, double precision XY(2,6), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 6 )

      double precision w(order)
      double precision w_save(6)
      double precision xy(2,order)
      double precision xy_save(2,6)

      save w_save
      save xy_save

      data w_save /
     &   0.22338158967801146570D+00, 
     &   0.22338158967801146570D+00, 
     &   0.22338158967801146570D+00, 
     &   0.10995174365532186764D+00, 
     &   0.10995174365532186764D+00, 
     &   0.10995174365532186764D+00 /
      data xy_save /
     &   0.10810301816807022736D+00, 
     &   0.44594849091596488632D+00, 
     &   0.44594849091596488632D+00, 
     &   0.10810301816807022736D+00, 
     &   0.44594849091596488632D+00, 
     &   0.44594849091596488632D+00, 
     &   0.81684757298045851308D+00, 
     &   0.091576213509770743460D+00, 
     &   0.091576213509770743460D+00, 
     &   0.81684757298045851308D+00, 
     &   0.091576213509770743460D+00, 
     &   0.091576213509770743460D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_o06b ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O06B returns a 6 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 3.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(6), the weights.
c
c    Output, double precision XY(2,6), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 6 )

      double precision w(order)
      double precision w_save(6)
      double precision xy(2,order)
      double precision xy_save(2,6)

      save w_save
      save xy_save

      data w_save /
     &   0.30000000000000000000D+00, 
     &   0.30000000000000000000D+00, 
     &   0.30000000000000000000D+00, 
     &   0.033333333333333333333D+00, 
     &   0.033333333333333333333D+00, 
     &   0.033333333333333333333D+00 /
      data xy_save /
     &   0.66666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.66666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.16666666666666666667D+00, 
     &   0.0D+00, 
     &   0.5D+00, 
     &   0.5D+00, 
     &   0.0D+00, 
     &   0.5D+00, 
     &   0.5D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_o07 ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O07 returns a 7 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 5.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2009
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
c    Output, double precision W(7), the weights.
c
c    Output, double precision XY(2,7), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 7 )

      double precision w(order)
      double precision w_save(7)
      double precision xy(2,order)
      double precision xy_save(2,7)

      save w_save
      save xy_save

      data w_save /
     &   0.12593918054482715260D+00, 
     &   0.12593918054482715260D+00, 
     &   0.12593918054482715260D+00, 
     &   0.13239415278850618074D+00, 
     &   0.13239415278850618074D+00, 
     &   0.13239415278850618074D+00, 
     &   0.22500000000000000000D+00 /
      data xy_save /
     &   0.79742698535308732240D+00, 
     &   0.10128650732345633880D+00, 
     &   0.10128650732345633880D+00, 
     &   0.79742698535308732240D+00, 
     &   0.10128650732345633880D+00, 
     &   0.10128650732345633880D+00, 
     &   0.059715871789769820459D+00, 
     &   0.47014206410511508977D+00, 
     &   0.47014206410511508977D+00, 
     &   0.059715871789769820459D+00, 
     &   0.47014206410511508977D+00, 
     &   0.47014206410511508977D+00, 
     &   0.33333333333333333333D+00, 
     &   0.33333333333333333333D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_o12 ( w, xy )

c*********************************************************************72
c
cc TRIG_UNIT_O12 returns a 12 point quadrature rule for the unit triangle.
c
c  Discussion:
c
c    This rule is precise for monomials through degree 6.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2009
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
c    Output, double precision W(12), the weights.
c
c    Output, double precision XY(2,12), the abscissas.
c
      implicit none

      integer order
      parameter ( order = 12 )

      double precision w(order)
      double precision w_save(12)
      double precision xy(2,order)
      double precision xy_save(2,12)

      save w_save
      save xy_save

      data w_save /
     &    0.050844906370206816921D+00, 
     &    0.050844906370206816921D+00, 
     &    0.050844906370206816921D+00, 
     &    0.11678627572637936603D+00, 
     &    0.11678627572637936603D+00, 
     &    0.11678627572637936603D+00, 
     &    0.082851075618373575194D+00, 
     &    0.082851075618373575194D+00, 
     &    0.082851075618373575194D+00, 
     &    0.082851075618373575194D+00, 
     &    0.082851075618373575194D+00, 
     &    0.082851075618373575194D+00 /
      data xy_save /
     &   0.87382197101699554332D+00, 
     &   0.063089014491502228340D+00, 
     &   0.063089014491502228340D+00, 
     &   0.87382197101699554332D+00, 
     &   0.063089014491502228340D+00, 
     &   0.063089014491502228340D+00, 
     &   0.50142650965817915742D+00, 
     &   0.24928674517091042129D+00, 
     &   0.24928674517091042129D+00, 
     &   0.50142650965817915742D+00, 
     &   0.24928674517091042129D+00, 
     &   0.24928674517091042129D+00, 
     &   0.053145049844816947353D+00, 
     &   0.31035245103378440542D+00, 
     &   0.31035245103378440542D+00, 
     &   0.053145049844816947353D+00, 
     &   0.053145049844816947353D+00, 
     &   0.63650249912139864723D+00, 
     &   0.31035245103378440542D+00, 
     &   0.63650249912139864723D+00, 
     &   0.63650249912139864723D+00, 
     &   0.053145049844816947353D+00, 
     &   0.63650249912139864723D+00, 
     &   0.31035245103378440542D+00 /

      call r8vec_copy ( order, w_save, w )
      call r8mat_copy ( 2, order, xy_save, xy )

      return
      end
      subroutine trig_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc TRIG_UNIT_QUAD_TEST tests the rules for the unit triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2008
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

      integer dim_num
      parameter ( dim_num = 2 )
      integer order_max
      parameter ( order_max = 12 )

      integer degree_max
      integer expon(dim_num)
      integer h
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision trig_unit_volume
      double precision v(order_max)
      double precision w(order_max)
      double precision xy(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIG_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit triangle,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  TRIG_UNIT_O01,'
      write ( *, '(a)' ) '  TRIG_UNIT_O03,'
      write ( *, '(a)' ) '  TRIG_UNIT_O03b,'
      write ( *, '(a)' ) '  TRIG_UNIT_O06,'
      write ( *, '(a)' ) '  TRIG_UNIT_O06b,'
      write ( *, '(a)' ) '  TRIG_UNIT_O07,'
      write ( *, '(a)' ) '  TRIG_UNIT_O012,'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2)' ) 
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        order = 1
        call trig_unit_o01 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 3
        call trig_unit_o03 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 3
        call trig_unit_o03b ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 6
        call trig_unit_o06 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 6
        call trig_unit_o06b ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 7
        call trig_unit_o07 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 12
        call trig_unit_o12 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = trig_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        write ( *, '(a)' ) ' '
        call trig_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      function trig_unit_volume ( )

c*********************************************************************72
c
cc TRIG_UNIT_VOLUME: volume of a unit triangle.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision TRIG_UNIT_VOLUME, the volume.
c
      implicit none

      double precision trig_unit_volume

      trig_unit_volume = 0.5D+00

      return
      end
      subroutine wedg_unit_monomial ( expon, value )

c*********************************************************************72
c
cc WEDG_UNIT_MONOMIAL: monomial integral in a unit wedge.
c
c  Discussion:
c
c    This routine returns the integral of
c
c      product ( 1 <= I <= 3 ) X(I)^EXPON(I)
c
c    over the unit wedge.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c  Parameters:
c
c    Input, integer EXPON(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer expon(3)
      integer i
      integer k
      double precision value
c
c  The first computation ends with VALUE = 1.0;
c
      value = 1.0D+00

      k = expon(1)

      do i = 1, expon(2)
        k = k + 1
        value = value * dble ( i ) / dble ( k )
      end do

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )
c
c  Now account for integration in Z.
c
      if ( expon(3) .eq. - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDG_UNIT_MONOMIAL - Fatal error!'
        write ( *, '(a)' ) '  EXPON(3) = -1 is not a legal input.'
        stop 1
      else if ( mod ( expon(3), 2 ) .eq. 1 ) then
        value = 0.0D+00
      else
        value = value * 2.0D+00 / dble ( expon(3) + 1 )
      end if

      return
      end
      subroutine wedg_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc WEDG_UNIT_MONOMIAL_TEST tests WEDG_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
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

      integer alpha
      integer beta
      integer degree_max
      integer expon(3)
      integer gamma
      double precision value
      double precision wedg_unit_volume

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDG_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit wedge,'
      write ( *, '(a)' ) 
     &  '  WEDG_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', wedg_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call wedg_unit_monomial ( expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine wedg_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc WEDG_UNIT_QUAD_TEST tests the rules for the unit wedge.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2009
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

      integer dim_num
      parameter ( dim_num = 3 )
      integer order_max
      parameter ( order_max = 48 )
      integer test_num
      parameter ( test_num = 7 )

      integer degree_max
      integer expon(dim_num)
      integer h
      integer line_order
      integer line_order_array(test_num)
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      integer test
      integer trig_order
      integer trig_order_index
      integer trig_order_array(test_num)
      double precision wedg_unit_volume
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(dim_num,order_max)

      save line_order_array
      save trig_order_array

      data line_order_array /
     &  1, 2, 2, 3, 2, 3, 4 /
      data trig_order_array/
     &  1, 3, -3, 6, -6, 7, 12 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDG_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit wedge,'
      write ( *, '(a)' ) 
     &  '  we approximate monomial integrals with WEDG_UNIT_RULE.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        if ( mod ( expon(3), 2 ) .eq. 1 ) then
          if ( .not. more ) then
            go to 20
          else
            go to 10
          end if
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        do test = 1, test_num

          line_order = line_order_array(test)
          trig_order = trig_order_array(test)

          order = line_order * abs ( trig_order )

          call wedg_unit_rule ( line_order, trig_order, w, xyz )
          call monomial_value ( dim_num, order, expon, xyz, v )
          quad = wedg_unit_volume ( ) 
     &      * r8vec_dot_product ( order, w, v )
          write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' )
     &      trig_order, line_order, order, quad

        end do

        write ( *, '(a)' ) ' '
        call wedg_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,6x,2x,6x,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine wedg_unit_rule ( line_order, trig_order, w, xyz )

c*********************************************************************72
c
cc WEDG_UNIT_RULE returns a quadrature rule for the unit wedge.
c
c  Discussion:
c
c    It is usually sensible to take LINE_ORDER and TRIG_ORDER so that
c    the line and triangle rules are roughly the same precision.  For that
c    criterion, we recommend the following combinations:
c
c      TRIG_ORDER  LINE_ORDER  Precision
c      ----------  ----------  ---------
c          1           1       1 x 1
c          3           2       2 x 3
c         -3           2       2 x 3
c          6           3       4 x 5
c         -6           2       3 x 3
c          7           3       5 x 5
c         12           4       6 x 7
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2009
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
c    Input, integer LINE_ORDER, the index of the line rule.
c    The index of the rule is equal to the order of the rule.
c    1 <= LINE_ORDER <= 5.
c
c    Input, integer TRIG_ORDER, the indes of the triangle rule.
c    The index of the rule is 1, 3, -3, 6, -6, 7 or 12.
c
c    Output, double precision W(LINE_ORDER*abs(TRIG_ORDER)), the weights.
c
c    Output, double precision XYZ(3,LINE_ORDER*abs(TRIG_ORDER)), the abscissas.
c
      implicit none

      integer line_order
      integer trig_order

      integer i
      integer j
      integer k
      double precision line_w(line_order)
      double precision line_x(line_order)
      double precision trig_w(abs(trig_order))
      double precision trig_xy(2,abs(trig_order))
      double precision w(line_order*abs(trig_order))
      double precision xyz(3,line_order*abs(trig_order))

      if ( line_order .eq. 1 ) then
        call line_unit_o01 ( line_w, line_x )
      else if ( line_order .eq. 2 ) then
        call line_unit_o02 ( line_w, line_x )
      else if ( line_order .eq. 3 ) then
        call line_unit_o03 ( line_w, line_x )
      else if ( line_order .eq. 4 ) then
        call line_unit_o04 ( line_w, line_x )
      else if ( line_order .eq. 5 ) then
        call line_unit_o05 ( line_w, line_x )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDG_UNIT_RULE - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of LINE_ORDER.'
        stop 1
      end if

      if ( trig_order .eq. 1 ) then
        call trig_unit_o01 ( trig_w, trig_xy )
      else if ( trig_order .eq. 3 ) then
        call trig_unit_o03 ( trig_w, trig_xy )
      else if ( trig_order .eq. - 3 ) then
        call trig_unit_o03b ( trig_w, trig_xy )
      else if ( trig_order .eq. 6 ) then
        call trig_unit_o06 ( trig_w, trig_xy )
      else if ( trig_order .eq. - 6 ) then
        call trig_unit_o06b ( trig_w, trig_xy )
      else if ( trig_order .eq. 7 ) then
        call trig_unit_o07 ( trig_w, trig_xy )
      else if ( trig_order .eq. 12 ) then
        call trig_unit_o12 ( trig_w, trig_xy )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDG_UNIT_RULE - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of TRIG_ORDER.'
        stop 1
      end if

      k = 0
      do i = 1, line_order
        do j = 1, abs ( trig_order )
          k = k + 1
          w(k) = line_w(i) * trig_w(j)
          xyz(1:2,k) = trig_xy(1:2,j)
          xyz(3,k) = line_x(i)
        end do
      end do

      return
      end
      function wedg_unit_volume ( )

c*********************************************************************72
c
cc WEDG_UNIT_VOLUME: volume of a unit wedge.
c
c  Discussion:
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision WEDG_UNIT_VOLUME, the volume.
c
      implicit none

      double precision wedg_unit_volume

      wedg_unit_volume = 1.0D+00

      return
      end
      subroutine wedg_unit_write_test ( )

c*********************************************************************72
c
cc WEDG_UNIT_WRITE_TEST writes out some rules for the unit wedge.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer order_max
      parameter ( order_max = 48 )

      integer rule_num
      parameter ( rule_num = 7 )

      integer line_order
      integer line_order_array(rule_num)
      integer order
      integer rule
      integer trig_order
      integer trig_order_array(rule_num)
      double precision w(order_max)
      character * ( 255 ) w_filename
      double precision x(dim_num,order_max)
      character * ( 255 ) x_filename

      save line_order_array
      save trig_order_array

      data line_order_array /
     &  1, 2, 2, 3, 2, 3, 4 /
      data trig_order_array /
     &  1, 3, -3, 6, -6, 7, 12 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDG_UNIT_WRITE_TEST'
      write ( *, '(a)' ) '  For the unit wedge,'
      write ( *, '(a)' ) '  write some rules to a file'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Rule  Trig    Line   Total  W_File X_File'
      write ( *, '(a)' ) '         Order   Order  Order'
      write ( *, '(a)' ) ' '

      do rule = 1, rule_num

        if ( rule .eq. 1 ) then
          w_filename = 'wedge_felippa_1x1_w.txt'
          x_filename = 'wedge_felippa_1x1_x.txt'
        else if ( rule .eq. 2 ) then
          w_filename = 'wedge_felippa_3x2_w.txt'
          x_filename = 'wedge_felippa_3x2_x.txt'
        else if ( rule .eq. 3 ) then
          w_filename = 'wedge_felippa_3bx2_w.txt'
          x_filename = 'wedge_felippa_3bx2_x.txt'
        else if ( rule .eq. 4 ) then
          w_filename = 'wedge_felippa_6x3_w.txt'
          x_filename = 'wedge_felippa_6x3_x.txt'
        else if ( rule .eq. 5 ) then
          w_filename = 'wedge_felippa_6bx2_w.txt'
          x_filename = 'wedge_felippa_6bx2_x.txt'
        else if ( rule .eq. 6 ) then
          w_filename = 'wedge_felippa_7x3_w.txt'
          x_filename = 'wedge_felippa_7x3_x.txt'
        else if ( rule .eq. 7 ) then
          w_filename = 'wedge_felippa_12x4_w.txt'
          x_filename = 'wedge_felippa_12x4_x.txt'
        end if

        line_order = line_order_array(rule)
        trig_order = trig_order_array(rule)

        order = line_order * abs ( trig_order )

        call wedg_unit_rule ( line_order, trig_order, w, x )
        call r8mat_write ( w_filename, 1, order, w )
        call r8mat_write ( x_filename, dim_num, order, x )
        write ( *, '(2x,i6,2x,i6,2x,i6,2x,i6,2x,a25,2x,a25)' )
     &    rule, trig_order, line_order, order, w_filename, x_filename

      end do

      return
      end
