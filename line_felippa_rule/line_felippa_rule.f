      subroutine line_monomial ( a, b, alpha, value )

c*********************************************************************72
c
cc LINE_MONOMIAL: monomial integral over a line segment in 1D.
c
c  Discussion:
c
c    This function returns the integral of X^ALPHA.
c
c    The integration region is:
c    A <= X <= B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input, integer ALPHA, the exponent of X.
c    ALPHA must not be -1.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      double precision a
      integer alpha
      double precision b
      double precision value

      if ( alpha .eq. - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LINE_MONOMIAL - Fatal error!'
        write ( *, '(a)' ) '  ALPHA = -1 is not a legal input.'
        stop 1
      end if


      value = ( b ** ( alpha + 1 ) - a ** ( alpha + 1 ) ) 
     &  / dble ( alpha + 1 )

      return
      end
      subroutine line_monomial_test ( degree_max )

c*********************************************************************72
c
cc LINE_MONOMIAL_TEST tests LINE_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
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

      double precision a
      integer alpha
      double precision b
      integer degree_max
      double precision line_volume
      double precision value

      a = 0.0D+00
      b = 1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For a line segment in 1D,'
      write ( *, '(a)' ) 
     &  '  LINE_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', line_volume ( a, b )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        call line_monomial ( a, b, alpha, value )
        write ( *, '(2x,i8,2x,g14.6)' ) alpha, value
      end do

      return
      end
      subroutine line_quad_test ( degree_max )

c*********************************************************************72
c
cc LINE_QUAD_TEST tests the rules for a line segment in 1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
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
      parameter ( order_max = 5 )

      double precision a
      double precision b
      integer degree_max
      integer expon
      integer j
      integer order
      double precision quad
      double precision r8vec_dot_product
      double precision v(order_max)
      double precision w(order_max)
      double precision x(order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_QUAD_TEST'
      write ( *, '(a)' ) '  For a line segment in 1D,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  LINE_UNIT_O01, a 1 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O02, a 2 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O03, a 3 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O04, a 4 point rule.'
      write ( *, '(a)' ) '  LINE_UNIT_O05, a 5 point rule.'

      do expon = 0, degree_max

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2)' ) '  Monomial exponent:  ', expon
        write ( *, '(a)' ) ' '

        do order = 1, 5

          call line_rule ( a, b, order, w, x )
          do j = 1, order
            v(j) = x(j) ** expon
          end do
          quad = r8vec_dot_product ( order, w, v )
          write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        end do

        write ( *, '(a)' ) ' '
        call line_monomial ( a, b, expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

      end do

      return
      end
      subroutine line_rule ( a, b, order, w, x )

c*********************************************************************72
c
cc LINE_RULE returns a quadrature rule for a line segment in 1D.
c
c  Discussion:
c
c    The integration region is:
c      A <= X <= B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
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
c    Input, double precision A, B, the lower and upper limits.
c
c    Input, integer ORDER, the order of the rule.
c
c    Output, double precision W(ORDER), the weights.
c
c    Output, double precision X(ORDER), the abscissas.
c
      implicit none

      integer order

      double precision a
      double precision b
      integer j
      double precision w(order)
      double precision x(order)

      if ( order .eq. 1 ) then
        call line_unit_o01 ( w, x )
      else if ( order .eq. 2 ) then
        call line_unit_o02 ( w, x )
      else if ( order .eq. 3 ) then
        call line_unit_o03 ( w, x )
      else if ( order .eq. 4 ) then
        call line_unit_o04 ( w, x )
      else if ( order .eq. 5 ) then
        call line_unit_o05 ( w, x )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LINE_RULE - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of ORDER.'
        stop 1
      end if
c
c  Transform from [-1,+1] to [A,B]
c
      do j = 1, order
        w(j) = w(j) * ( b - a ) / 2.0D+00
        x(j) = ( ( 1.0D+00 - x(j) ) * a   
     &         + ( 1.0D+00 + x(j) ) * b ) 
     &         /   2.0D+00
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
c    -1 <= X <= +1
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
      function line_volume ( a, b )

c*********************************************************************72
c
cc LINE_VOLUME: volume of a line segment in 1D.
c
c  Discussion:
c
c    The integration region is:
c    A <= X <= B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Output, double precision LINE_VOLUME, the volume.
c
      implicit none

      double precision a
      double precision b
      double precision line_volume

      line_volume = b - a

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

