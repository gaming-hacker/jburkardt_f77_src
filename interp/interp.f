      subroutine cc_abscissas ( n, x )

c*********************************************************************72
c
cc CC_ABSCISSAS computes the Clenshaw Curtis abscissas.
c
c  Discussion:
c
c    The interval is [ -1, 1 ].
c
c    The abscissas are the cosines of equally spaced angles between
c    180 and 0 degrees, including the endpoints.
c
c      X(I) = cos ( ( ORDER - I ) * PI / ( ORDER - 1 ) )
c
c    except for the basic case ORDER = 1, when
c
c      X(1) = 0.
c
c    If the value of ORDER is increased in a sensible way, then
c    the new set of abscissas will include the old ones.  One such
c    sequence would be ORDER(K) = 2*K+1 for K = 0, 1, 2, ...
c
c    When doing interpolation with Lagrange polynomials, the Clenshaw Curtis
c    abscissas can be a better choice than regularly spaced abscissas.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 2007
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
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c    Joerg Waldvogel,
c    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
c    BIT Numerical Mathematics,
c    Volume 43, Number 1, 2003, pages 1-18.
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CC_ABSCISSA - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.0D+00
        return
      end if

      do i = 1, n
        theta(i) = dble ( n - i ) * pi / dble ( n - 1 )
      end do

      do i = 1, n
        x(i) = cos ( theta(i) )
      end do

      return
      end
      subroutine cc_abscissas_ab ( a, b, n, x )

c*********************************************************************72
c
cc CC_ABSCISSAS_AB computes Clenshaw Curtis abscissas for the interval [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CC_ABSCISSAS_AB - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.5D+00 * ( b + a )
        return
      end if

      do i = 1, n
        theta(i) = dble ( n - i ) * pi / dble ( n - 1 )
      end do

      do i = 1, n
        x(i) = 0.5D+00 * ( ( b + a ) + ( b - a ) * cos ( theta(i) ) )
      end do

      return
      end
      subroutine f1_abscissas ( n, x )

c*********************************************************************72
c
cc F1_ABSCISSAS computes Fejer type 1 abscissas.
c
c  Discussion:
c
c    The interval is [ -1, +1 ].
c
c    The abscissas are the cosines of equally spaced angles, which
c    are the midpoints of N equal intervals between 0 and PI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c    Walter Gautschi,
c    Numerical Quadrature in the Presence of a Singularity,
c    SIAM Journal on Numerical Analysis,
c    Volume 4, Number 3, 1967, pages 357-362.
c
c    Joerg Waldvogel,
c    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
c    BIT Numerical Mathematics,
c    Volume 43, Number 1, 2003, pages 1-18.
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F1_ABSCISSAS - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.0D+00
        return
      end if

      do i = 1, n
        theta(i) = dble ( 2 * n - 2 * i + 1 ) * pi / dble ( 2 * n )
      end do

      do i = 1, n
        x(i) = cos ( theta(i) )
      end do

      return
      end
      subroutine f1_abscissas_ab ( a, b, n, x )

c*********************************************************************72
c
cc F1_ABSCISSAS_AB computes Fejer type 1 abscissas for the interval [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c    Walter Gautschi,
c    Numerical Quadrature in the Presence of a Singularity,
c    SIAM Journal on Numerical Analysis,
c    Volume 4, Number 3, 1967, pages 357-362.
c
c    Joerg Waldvogel,
c    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
c    BIT Numerical Mathematics,
c    Volume 43, Number 1, 2003, pages 1-18.
c
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F1_ABSCISSAS_AB - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.5D+00 * ( b + a )
        return
      end if

      do i = 1, n
        theta(i) = dble ( 2 * n - 2 * i + 1 ) * pi / dble ( 2 * n )
      end do

      do i = 1, n
        x(i) = 0.5D+00 * ( ( b + a ) + ( b - a ) * cos ( theta(i) ) )
      end do

      return
      end
      subroutine f2_abscissas ( n, x )

c*********************************************************************72
c
cc F2_ABSCISSAS computes Fejer Type 2 abscissas.
c
c  Discussion:
c
c    The interval is [-1,+1].
c
c    The abscissas are the cosines of equally spaced angles.
c    The angles are computed as N+2 equally spaced values between 0 and PI,
c    but with the first and last angle omitted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c    Walter Gautschi,
c    Numerical Quadrature in the Presence of a Singularity,
c    SIAM Journal on Numerical Analysis,
c    Volume 4, Number 3, 1967, pages 357-362.
c
c    Joerg Waldvogel,
c    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
c    BIT Numerical Mathematics,
c    Volume 43, Number 1, 2003, pages 1-18.
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F2_ABSCISSAS - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.0D+00
        return
      else if ( n .eq. 2 ) then
        x(1) = -0.5D+00
        x(2) =  0.5D+00
        return
      end if

      do i = 1, n
        theta(i) = dble ( n + 1 - i ) * pi / dble ( n + 1 )
      end do

      do i = 1, n
        x(i) = cos ( theta(i) )
      end do

      return
      end
      subroutine f2_abscissas_ab ( a, b, n, x )

c*********************************************************************72
c
cc F2_ABSCISSAS_AB computes Fejer Type 2 abscissas for the interval [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F2_ABSCISSAS_AB - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      do i = 1, n
        theta(i) = dble ( n + 1 - i ) * pi / dble ( n + 1 )
      end do

      do i = 1, n
        x(i) = 0.5D+00 * ( ( b + a ) + ( b - a ) * cos ( theta(i) ) )
      end do

      return
      end
      subroutine interp_lagrange ( m, data_num, t_data, p_data, 
     &  interp_num, t_interp, p_interp )

c*********************************************************************72
c
cc INTERP_LAGRANGE: Lagrange polynomial interpolant to a curve in M dimensions.
c
c  Discussion:
c
c    From a space of M dimensions, we are given a sequence of
c    DATA_NUM points, which are presumed to be successive samples
c    from a curve of points P.
c
c    We are also given a parameterization of this data, that is,
c    an associated sequence of DATA_NUM values of a variable T.
c
c    Thus, we have a sequence of values P(T), where T is a scalar,
c    and each value of P is of dimension M.
c
c    We are then given INTERP_NUM values of T, for which values P
c    are to be produced, by linear interpolation of the data we are given.
c
c    The user may request extrapolation.  This occurs whenever
c    a T_INTERP value is less than the minimum T_DATA or greater than the
c    maximum T_DATA.  In that case, extrapolation is used.
c
c    For each spatial component, a polynomial of degree
c    ( DATA_NUM - 1 ) is generated for the interpolation.  In most cases,
c    such a polynomial interpolant begins to oscillate as DATA_NUM
c    increases, even if the original data seems well behaved.  Typically,
c    values of DATA_NUM should be no greater than 10!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer DATA_NUM, the number of data points.
c
c    Input, double precision T_DATA(DATA_NUM), the value of the
c    independent variable at the sample points.
c
c    Input, double precision P_DATA(M,DATA_NUM), the value of the
c    dependent variables at the sample points.
c
c    Input, integer INTERP_NUM, the number of points
c    at which interpolation is to be done.
c
c    Input, double precision T_INTERP(INTERP_NUM), the value of the
c    independent variable at the interpolation points.
c
c    Output, double precision P_INTERP(M,DATA_NUM), the interpolated
c    values of the dependent variables at the interpolation points.
c
      implicit none

      integer data_num
      integer m
      integer interp_num

      double precision l_interp(data_num,interp_num)
      double precision p_data(m,data_num)
      double precision p_interp(m,interp_num)
      double precision t_data(data_num)
      double precision t_interp(interp_num)
c
c  Evaluate the DATA_NUM Lagrange polynomials associated with T_DATA(1:DATA_NUM)
c  for the interpolation points T_INTERP(1:INTERP_NUM).
c
      call lagrange_value ( data_num, t_data, interp_num, t_interp, 
     &  l_interp )
c
c  Multiply P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
c  to get P_INTERP(1:M,1:INTERP_NUM).
c
      call r8mat_mm ( m, data_num, interp_num, p_data, l_interp,
     &  p_interp )

      return
      end
      subroutine interp_linear ( m, data_num, t_data, p_data, 
     &  interp_num, t_interp, p_interp )

c*********************************************************************72
c
cc INTERP_LINEAR: piecewise linear interpolation to a curve in M dimensions.
c
c  Discussion:
c
c    From a space of M dimensions, we are given a sequence of
c    DATA_NUM points, which are presumed to be successive samples
c    from a curve of points P.
c
c    We are also given a parameterization of this data, that is,
c    an associated sequence of DATA_NUM values of a variable T.
c    The values of T are assumed to be strictly increasing.
c
c    Thus, we have a sequence of values P(T), where T is a scalar,
c    and each value of P is of dimension M.
c
c    We are then given INTERP_NUM values of T, for which values P
c    are to be produced, by linear interpolation of the data we are given.
c
c    Note that the user may request extrapolation.  This occurs whenever
c    a T_INTERP value is less than the minimum T_DATA or greater than the
c    maximum T_DATA.  In that case, linear extrapolation is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer DATA_NUM, the number of data points.
c
c    Input, double precision T_DATA(DATA_NUM), the value of the
c    independent variable at the sample points.  The values of T_DATA
c    must be strictly increasing.
c
c    Input, double precision P_DATA(M,DATA_NUM), the value of the
c    dependent variables at the sample points.
c
c    Input, integer INTERP_NUM, the number of points
c    at which interpolation is to be done.
c
c    Input, double precision T_INTERP(INTERP_NUM), the value of the
c    independent variable at the interpolation points.
c
c    Output, double precision P_INTERP(M,DATA_NUM), the interpolated
c    values of the dependent variables at the interpolation points.
c
      implicit none

      integer data_num
      integer m
      integer interp_num

      integer i
      integer interp
      integer left
      double precision p_data(m,data_num)
      double precision p_interp(m,interp_num)
      logical r8vec_ascends_strictly
      integer right
      double precision t
      double precision t_data(data_num)
      double precision t_interp(interp_num)

      if ( .not. r8vec_ascends_strictly ( data_num, t_data ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'INTERP_LINEAR - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Independent variable array T_DATA not strictly increasing.'
        stop 1
      end if

      do interp = 1, interp_num

        t = t_interp(interp)
c
c  Find the interval [ TDATA(LEFT), TDATA(RIGHT) ] that contains, or is
c  nearest to, TVAL.
c
        call r8vec_bracket ( data_num, t_data, t, left, right )

        do i = 1, m
          p_interp(i,interp) =
     &    ( ( t_data(right) - t               )   * p_data(i,left)  
     &    + (                 t - t_data(left ) ) * p_data(i,right) )       
     &    / ( t_data(right)     - t_data(left) )
        end do

      end do

      return
      end
      subroutine interp_nearest ( m, data_num, t_data, p_data, 
     &  interp_num, t_interp, p_interp )

c*********************************************************************72
c
cc INTERP_NEAREST: Nearest neighbor interpolation to a curve in M dimensions.
c
c  Discussion:
c
c    From a space of M dimensions, we are given a sequence of
c    DATA_NUM points, which are presumed to be successive samples
c    from a curve of points P.
c
c    We are also given a parameterization of this data, that is,
c    an associated sequence of DATA_NUM values of a variable T.
c
c    Thus, we have a sequence of values P(T), where T is a scalar,
c    and each value of P is of dimension M.
c
c    We are then given INTERP_NUM values of T, for which values P
c    are to be produced, by nearest neighbor interpolation.
c
c    The user may request extrapolation.  This occurs whenever
c    a T_INTERP value is less than the minimum T_DATA or greater than the
c    maximum T_DATA.  In that case, extrapolation is used.
c
c    The resulting interpolant is piecewise constant.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer DATA_NUM, the number of data points.
c
c    Input, double precision T_DATA(DATA_NUM), the value of the
c    independent variable at the sample points.
c
c    Input, double precision P_DATA(M,DATA_NUM), the value of the
c    dependent variables at the sample points.
c
c    Input, integer INTERP_NUM, the number of points
c    at which interpolation is to be done.
c
c    Input, double precision T_INTERP(INTERP_NUM), the value of the
c    independent variable at the interpolation points.
c
c    Output, double precision P_INTERP(M,DATA_NUM), the interpolated
c    values of the dependent variables at the interpolation points.
c
      implicit none

      integer data_num
      integer m
      integer interp_num

      integer i
      integer jd
      integer ji
      double precision p_data(m,data_num)
      double precision p_interp(m,interp_num)
      integer r8vec_sorted_nearest
      double precision t_data(data_num)
      double precision t_interp(interp_num)
c
c  For each interpolation point, find the index of the nearest data point.
c
      do ji = 1, interp_num
        jd = r8vec_sorted_nearest ( data_num, t_data, t_interp(ji) )
        do i = 1, m
          p_interp(i,ji) = p_data(i,jd)
        end do
      end do

      return
      end
      subroutine lagrange_value ( data_num, t_data, interp_num, 
     &  t_interp, l_interp )

c*********************************************************************72
c
cc LAGRANGE_VALUE evaluates the Lagrange polynomials.
c
c  Discussion:
c
c    Given DATA_NUM distinct abscissas, T_DATA(1:DATA_NUM),
c    the I-th Lagrange polynomial L(I)(T) is defined as the polynomial of
c    degree DATA_NUM - 1 which is 1 at T_DATA(I) and 0 at the DATA_NUM - 1
c    other abscissas.
c
c    A formal representation is:
c
c      L(I)(T) = Product ( 1 <= J <= DATA_NUM, I /= J )
c       ( T - T(J) ) / ( T(I) - T(J) )
c
c    This routine accepts a set of INTERP_NUM values at which all the Lagrange
c    polynomials should be evaluated.
c
c    Given data values P_DATA at each of the abscissas, the value of the
c    Lagrange interpolating polynomial at each of the interpolation points
c    is then simple to compute by matrix multiplication:
c
c      P_INTERP(1:INTERP_NUM) =
c        P_DATA(1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
c
c    or, in the case where P is multidimensional:
c
c      P_INTERP(1:M,1:INTERP_NUM) =
c        P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DATA_NUM, the number of data points.
c    DATA_NUM must be at least 1.
c
c    Input, double precision T_DATA(DATA_NUM), the data points.
c
c    Input, integer INTERP_NUM, the number of
c    interpolation points.
c
c    Input, double precision T_INTERP(INTERP_NUM), the
c    interpolation points.
c
c    Output, double precision L_INTERP(DATA_NUM,INTERP_NUM), the values
c    of the Lagrange polynomials at the interpolation points.
c
      implicit none

      integer data_num
      integer interp_num

      integer i
      integer i2
      integer j
      double precision l_interp(data_num,interp_num)
      double precision t_data(data_num)
      double precision t_interp(interp_num)
c
c  Evaluate the polynomial.
c
      do j = 1, interp_num
        do i = 1, data_num
          l_interp(i,j) = 1.0D+00
        end do
      end do

      do i = 1, data_num

        do i2 = 1, data_num

          if ( i .ne. i2 ) then

            do j = 1, interp_num
              l_interp(i,j) = l_interp(i,j) 
     &          * ( t_interp(j) - t_data(i2) ) 
     &          / ( t_data(i)   - t_data(i2) )
            end do

          end if

        end do

      end do

      return
      end
      subroutine ncc_abscissas ( n, x )

c*********************************************************************72
c
cc NCC_ABSCISSAS computes the Newton Cotes Closed abscissas.
c
c  Discussion:
c
c    The interval is [ -1, 1 ].
c
c    The abscissas are the equally spaced points between -1 and 1,
c    including the endpoints.
c
c    If N is 1, however, the single abscissas is X = 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      integer i
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NCC_ABSCISSAS - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.0D+00
        return
      end if

      do i = 1, n
        x(i) = ( dble ( n - i     ) * ( -1.0D+00 )            
     &         + dble (     i - 1 ) * ( +1.0D+00 ) )
     &         / dble ( n     - 1 )
      end do

      return
      end
      subroutine ncc_abscissas_ab ( a, b, n, x )

c*********************************************************************72
c
cc NCC_ABSCISSAS_AB computes the Newton Cotes Closed abscissas for [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NCC_ABSCISSAS_AB - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      if ( n .eq. 1 ) then
        x(1) = 0.5D+00 * ( b + a )
        return
      end if

      do i = 1, n
        x(i) = ( dble ( n - i     ) * a
     &         + dble (     i - 1 ) * b )
     &         / dble ( n     - 1 )
      end do

      return
      end
      subroutine nco_abscissas ( n, x )

c*********************************************************************72
c
cc NCO_ABSCISSAS computes the Newton Cotes Open abscissas.
c
c  Discussion:
c
c    The interval is [ -1, 1 ].
c
c    The abscissas are the equally spaced points between -1 and 1,
c    not including the endpoints.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      integer i
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NCO_ABSCISSAS - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      do i = 1, n
        x(i) = ( dble ( n - i + 1 ) * ( -1.0D+00 )            
     &         + dble (     i     ) * ( +1.0D+00 ) )
     &         / dble ( n     + 1 )
      end do

      return
      end
      subroutine nco_abscissas_ab ( a, b, n, x )

c*********************************************************************72
c
cc NCO_ABSCISSAS_AB computes the Newton Cotes Open abscissas for [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the abscissas.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NCO_ABSCISSAS_AB - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        stop 1
      end if

      do i = 1, n
        x(i) = ( dble ( n - i + 1 ) * a
     &         + dble (     i     ) * b )
     &         / dble ( n     + 1 )
      end do

      return
      end
      subroutine parameterize_arc_length ( m, data_num, p_data, t_data )

c*********************************************************************72
c
cc PARAMETERIZE_ARC_LENGTH parameterizes data by pseudo-arclength.
c
c  Discussion:
c
c    A parameterization is required for the interpolation.
c
c    This routine provides a parameterization by computing the
c    pseudo-arclength of the data, that is, the Euclidean distance
c    between successive points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer DATA_NUM, the number of data points.
c
c    Input, double precision P_DATA(M,DATA_NUM), the data values.
c
c    Output, double precision T_DATA(DATA_NUM), parameter values
c    assigned to the data.
c
      implicit none

      integer data_num
      integer m

      integer i
      integer j
      double precision p_data(m,data_num)
      double precision s
      double precision t_data(data_num)

      t_data(1) = 0.0D+00
      do j = 2, data_num
        s = 0.0D+00
        do i = 1, m
          s = s + ( p_data(i,j) - p_data(i,j-1) ) ** 2
        end do
        s = sqrt ( s )
        t_data(j) = t_data(j-1) + s
      end do

      return
      end
      subroutine parameterize_index ( m, data_num, p_data, t_data )

c*********************************************************************72
c
cc PARAMETERIZE_INDEX parameterizes data by its index.
c
c  Discussion:
c
c    A parameterization is required for the interpolation.
c
c    This routine provides a naive parameterization by vector index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer DATA_NUM, the number of data points.
c
c    Input, double precision P_DATA(M,DATA_NUM), the data values.
c
c    Output, double precision T_DATA(DATA_NUM), parameter values
c    assigned to the data.
c
      implicit none

      integer data_num
      integer m

      integer j
      double precision p_data(m,data_num)
      double precision t_data(data_num)

      t_data(1) = 0.0D+00
      do j = 2, data_num
        t_data(j) = dble ( j - 1 )
      end do

      return
      end
      subroutine r8mat_expand_linear2 ( m, n, a, m2, n2, a2 )

c*********************************************************************72
c
cc R8MAT_EXPAND_LINEAR2 expands an R8MAT by linear interpolation.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In this version of the routine, the expansion is indicated
c    by specifying the dimensions of the expanded array.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in A.
c
c    Input, double precision A(M,N), a "small" M by N array.
c
c    Input, integer M2, N2, the number of rows and columns in A2.
c
c    Output, double precision A2(M2,N2), the expanded array, which
c    contains an interpolated version of the data in A.
c
      implicit none

      integer m
      integer m2
      integer n
      integer n2

      double precision a(m,n)
      double precision a2(m2,n2)
      integer i
      integer i1
      integer i2
      integer j
      integer j1
      integer j2
      double precision r
      double precision r1
      double precision r2
      double precision s
      double precision s1
      double precision s2

      do i = 1, m2

        if ( m2 .eq. 1 ) then
          r = 0.5D+00
        else
          r = dble ( i - 1 ) / dble ( m2 - 1 )
        end if

        i1 = 1 + int ( r * dble ( m - 1 ) )
        i2 = i1 + 1

        if ( m .lt. i2 ) then
          i1 = m - 1
          i2 = m
        end if

        r1 = dble ( i1 - 1 ) / dble ( m - 1 )

        r2 = dble ( i2 - 1 ) / dble ( m - 1 )

        do j = 1, n2

          if ( n2 .eq. 1 ) then
            s = 0.5D+00
          else
            s = dble ( j - 1 ) / dble ( n2 - 1 )
          end if

          j1 = 1 + int ( s * dble ( n - 1 ) )
          j2 = j1 + 1

          if ( n .lt. j2 ) then
            j1 = n - 1
            j2 = n
          end if

          s1 = dble ( j1 - 1 ) / dble ( n - 1 )

          s2 = dble ( j2 - 1 ) / dble ( n - 1 )

          a2(i,j) = 
     &      ( ( r2 - r ) * ( s2 - s ) * a(i1,j1) 
     &      + ( r - r1 ) * ( s2 - s ) * a(i2,j1) 
     &      + ( r2 - r ) * ( s - s1 ) * a(i1,j2) 
     &      + ( r - r1 ) * ( s - s1 ) * a(i2,j2) ) 
     &      / ( ( r2 - r1 ) * ( s2 - s1 ) )

        end do

      end do

      return
      end
      subroutine r8mat_mm ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MM multiplies two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N2,N3), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n2,n3)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do

      do j = 1, n3
        do i = 1, n1
          c(i,j) = c1(i,j)
        end do
      end do

      return
      end
      function r8vec_ascends_strictly ( n, x )

c*********************************************************************72
c
cc R8VEC_ASCENDS_STRICTLY determines if an R8VEC is strictly ascending.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    Notice the effect of entry number 6 in the following results:
c
c      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.4, 9.8 )
c      Y = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
c      Z = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.6, 9.8 )
c
c      R8VEC_ASCENDS_STRICTLY ( X ) = FALSE
c      R8VEC_ASCENDS_STRICTLY ( Y ) = FALSE
c      R8VEC_ASCENDS_STRICTLY ( Z ) = TRUE
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the array.
c
c    Input, double precision X(N), the array to be examined.
c
c    Output, logical R8VEC_ASCENDS_STRICTLY, is TRUE if the
c    entries of X strictly ascend.
c
      implicit none

      integer n

      integer i
      logical r8vec_ascends_strictly
      double precision x(n)

      do i = 1, n - 1
        if ( x(i+1) .le. x(i) ) then
          r8vec_ascends_strictly = .false.
          return
        end if
      end do

      r8vec_ascends_strictly = .true.

      return
      end
      subroutine r8vec_bracket ( n, x, xval, left, right )

c*********************************************************************72
c
cc R8VEC_BRACKET searches a sorted array for successive brackets of a value.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    If the values in the vector are thought of as defining intervals
c    on the real line, then this routine searches for the interval
c    nearest to or containing the given value.
c
c  Modified:
c
c    24 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, length of input array.
c
c    Input, double precision X(N), an array that has been sorted into
c    ascending order.
c
c    Input, double precision XVAL, a value to be bracketed.
c
c    Output, integer LEFT, RIGHT, the results of the search.
c    Either:
c      XVAL < X(1), when LEFT = 1, RIGHT = 2;
c      X(N) < XVAL, when LEFT = N-1, RIGHT = N;
c    or
c      X(LEFT) <= XVAL <= X(RIGHT).
c
      implicit none

      integer n

      integer i
      integer left
      integer right
      double precision x(n)
      double precision xval

      do i = 2, n - 1

        if ( xval .lt. x(i) ) then
          left = i - 1
          right = i
          return
        end if

       end do

      left = n - 1
      right = n

      return
      end
      subroutine r8vec_expand_linear ( n, x, fat, xfat )

c*********************************************************************72
c
cc R8VEC_EXPAND_LINEAR linearly interpolates new data into an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    This routine copies the old data, and inserts NFAT new values
c    between each pair of old data values.  This would be one way to
c    determine places to evenly sample a curve, given the (unevenly
c    spaced) points at which it was interpolated.
c
c  Example:
c
c    N = 3
c    NFAT = 2
c
c    X(1:N)        = (/ 0.0,           6.0,             7.0 /)
c    XFAT(1:2*3+1) = (/ 0.0, 2.0, 4.0, 6.0, 6.33, 6.66, 7.0 /)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of input data values.
c
c    Input, double precision X(N), the original data.
c
c    Input, integer FAT, the number of data values to interpolate
c    between each pair of original data values.
c
c    Output, double precision XFAT((N-1)*(FAT+1)+1), the "fattened" data.
c
      implicit none

      integer fat
      integer n

      integer i
      integer j
      integer k
      double precision x(n)
      double precision xfat((n-1)*(fat+1)+1)

      k = 0

      do i = 1, n - 1

        k = k + 1
        xfat(k) = x(i)

        do j = 1, fat
          k = k + 1
          xfat(k) = ( dble ( fat - j + 1 ) * x(i)
     &              + dble (       j     ) * x(i+1) )
     &              / dble ( fat     + 1 )
        end do

      end do

      k = k + 1
      xfat(k) = x(n)

      return
      end
      subroutine r8vec_expand_linear2 ( n, x, before, fat, after, xfat )

c*********************************************************************72
c
cc R8VEC_EXPAND_LINEAR2 linearly interpolates new data into an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    This routine starts with a vector of data.
c
c    The intent is to "fatten" the data, that is, to insert more points
c    between successive values of the original data.
c
c    There will also be extra points placed BEFORE the first original
c    value and AFTER that last original value.
c
c    The "fattened" data is equally spaced between the original points.
c
c    The BEFORE data uses the spacing of the first original interval,
c    and the AFTER data uses the spacing of the last original interval.
c
c  Example:
c
c    N = 3
c    BEFORE = 3
c    FAT = 2
c    AFTER = 1
c
c    X    = (/                   0.0,           6.0,             7.0       /)
c    XFAT = (/ -6.0, -4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 6.33, 6.66, 7.0, 7.66 /)
c            3 "BEFORE's"        Old  2 "FATS"  Old    2 "FATS"  Old  1 "AFTER"
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of input data values.
c    N must be at least 2.
c
c    Input, double precision X(N), the original data.
c
c    Input, integer BEFORE, the number of "before" values.
c
c    Input, integer FAT, the number of data values to interpolate
c    between each pair of original data values.
c
c    Input, integer AFTER, the number of "after" values.
c
c    Output, double precision XFAT(BEFORE+(N-1)*(FAT+1)+1+AFTER), the
c    "fattened" data.
c
      implicit none

      integer after
      integer before
      integer fat
      integer n

      integer i
      integer j
      integer k
      double precision x(n)
      double precision xfat(before+(n-1)*(fat+1)+1+after)

      k = 0
c
c  Points BEFORE.
c
      do j = 1 - before + fat, fat
        k = k + 1
        xfat(k) = ( dble ( fat - j + 1 ) * ( x(1) - ( x(2) - x(1) ) )
     &            + dble (       j     ) *   x(1)          )
     &            / dble ( fat     + 1 )
      end do
c
c  Original points and FAT points.
c
      do i = 1, n - 1

        k = k + 1
        xfat(k) = x(i)

        do j = 1, fat
          k = k + 1
          xfat(k) = ( dble ( fat - j + 1 ) * x(i)
     &              + dble (       j     ) * x(i+1) )
     &              / dble ( fat     + 1 )
        end do

      end do

      k = k + 1
      xfat(k) = x(n)
c
c  Points AFTER.
c
      do j = 1, after
        k = k + 1
        xfat(k) =
     &    ( dble ( fat - j + 1 ) * x(n)
     &    + dble (       j     ) * ( x(n) + ( x(n) - x(n-1) ) ) )
     &    / dble ( fat     + 1 )
      end do

      return
      end
      function r8vec_sorted_nearest ( n, a, value )

c*********************************************************************72
c
cc R8VEC_SORTED_NEAREST returns the nearest element in a sorted R8VEC.
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
c    25 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements of A.
c
c    Input, double precision A(N), a sorted vector.
c
c    Input, double precision VALUE, the value whose nearest vector
c    entry is sought.
c
c    Output, integer R8VEC_SORTED_NEAREST, the index of the nearest
c    entry in the vector.
c
      implicit none

      integer n

      double precision a(n)
      integer r8vec_sorted_nearest
      integer hi
      integer lo
      integer mid
      double precision value

      if ( n .lt. 1 ) then
        r8vec_sorted_nearest = -1
        return
      end if

      if ( n .eq. 1 ) then
        r8vec_sorted_nearest = 1
        return
      end if

      if ( a(1) .lt. a(n) ) then

        if ( value .lt. a(1) ) then
          r8vec_sorted_nearest = 1
          return
        else if ( a(n) .lt. value ) then
          r8vec_sorted_nearest = n
          return
        end if
c
c  Seek an interval containing the value.
c
        lo = 1
        hi = n

10      continue

        if ( lo .lt. hi - 1 ) then

          mid = ( lo + hi ) / 2

          if ( value .eq. a(mid) ) then
            r8vec_sorted_nearest = mid
            return
          else if ( value .lt. a(mid) ) then
            hi = mid
          else
            lo = mid
          end if

          go to 10

        end if
c
c  Take the nearest.
c
        if ( abs ( value - a(lo) ) .lt. abs ( value - a(hi) ) ) then
          r8vec_sorted_nearest = lo
        else
          r8vec_sorted_nearest = hi
        end if

        return
c
c  A descending sorted vector A.
c
      else

        if ( value .lt. a(n) ) then
          r8vec_sorted_nearest = n
          return
        else if ( a(1) .lt. value ) then
          r8vec_sorted_nearest = 1
          return
        end if
c
c  Seek an interval containing the value.
c
        lo = n
        hi = 1

20      continue

        if ( lo .lt. hi - 1 ) then

          mid = ( lo + hi ) / 2

          if ( value .eq. a(mid) ) then
            r8vec_sorted_nearest = mid
            return
          else if ( value .lt. a(mid) ) then
            hi = mid
          else
            lo = mid
          end if

          go to 20

        end if
c
c  Take the nearest.
c
        if ( abs ( value - a(lo) ) .lt. abs ( value - a(hi) ) ) then
          r8vec_sorted_nearest = lo
        else
          r8vec_sorted_nearest = hi
        end if

        return

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
