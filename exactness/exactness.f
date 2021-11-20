      subroutine chebyshev1_exactness ( n, x, w, p_max )

c*********************************************************************72
c
cc CHEBYSHEV1_EXACTNESS: monomial exactness for the Chebyshev1 integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points in the rule.
c
c    Input, double precision X(N), the quadrature points.
c
c    Input, double precision W(N), the quadrature weights.
c
c    Input, integer P_MAX, the maximum exponent.
c    0 <= P_MAX.
c
      implicit none

      integer n

      double precision e
      integer i
      integer p
      integer p_max
      double precision q
      double precision r8vec_dot_product
      double precision s
      double precision v(n)
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule for the Chebyshev1 integral'
      write ( *, '(a,i3)' ) '  Rule of order N = ', n
      write ( *, '(a)' ) '  Degree          Relative Error'
      write ( *, '(a)' ) ''

      do p = 0, p_max

        call chebyshev1_integral ( p, s )

        do i = 1, n
          v(i) = x(i) ** p
        end do

        q = r8vec_dot_product ( n, w, v )

        if ( s .eq. 0.0D+00 ) then
          e = abs ( q )
        else
          e = abs ( q - s ) / abs ( s )
        end if

        write ( *, '(2x,i6,2x,f24.16)' ) p, e

      end do

      return
      end
      subroutine chebyshev1_integral ( expon, exact )

c*********************************************************************72
c
cc CHEBYSHEV1_INTEGRAL evaluates a monomial Chebyshev type 1 integral.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x <= +1 ) x^n / sqrt ( 1 - x * x ) dx
c
c    This routine is given the value of the exponent, and returns the
c    exact value of the integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON, the exponent.
c
c    Output, double precision EXACT, the value of the exact integral.
c
      implicit none

      double precision bot
      double precision exact
      integer expon
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision top
c
c  Get the exact value of the integral.
c
      if ( mod ( expon, 2 ) .eq. 0 ) then

        top = 1
        bot = 1
        do i = 2, expon, 2
          top = top * ( i - 1 )
          bot = bot *   i
        end do
	
        exact = r8_pi * dble ( top ) / dble ( bot )

      else

        exact = 0.0D+00
	
      end if

      return
      end
      subroutine chebyshev2_exactness ( n, x, w, p_max )

c*********************************************************************72
c
cc CHEBYSHEV2_EXACTNESS: monomial exactness for the Chebyshev2 integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points in the rule.
c
c    Input, double precision X(N), the quadrature points.
c
c    Input, double precision W(N), the quadrature weights.
c
c    Input, integer P_MAX, the maximum exponent.
c    0 <= P_MAX.
c
      implicit none

      integer n

      double precision e
      integer i
      integer p
      integer p_max
      double precision q
      double precision r8vec_dot_product
      double precision s
      double precision v(n)
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule for the Chebyshev2 integral'
      write ( *, '(a,i3)' ) '  Rule of order N = ', n
      write ( *, '(a)' ) '  Degree          Relative Error'
      write ( *, '(a)' ) ''

      do p = 0, p_max

        call chebyshev2_integral ( p, s )

        do i = 1, n
          v(i) = x(i) ** p
        end do

        q = r8vec_dot_product ( n, w, v )

        if ( s .eq. 0.0D+00 ) then
          e = abs ( q )
        else
          e = abs ( q - s ) / abs ( s )
        end if

        write ( *, '(2x,i6,2x,f24.16)' ) p, e

      end do

      return
      end
      subroutine chebyshev2_integral ( expon, exact )

c*********************************************************************72
c
cc CHEBYSHEV2_INTEGRAL evaluates a monomial Chebyshev type 2 integral.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x <= +1 ) x^n * sqrt ( 1 - x * x ) dx
c
c    This routine is given the value of the exponent, and returns the
c    exact value of the integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON, the exponent.
c
c    Output, real ( kind = 8 ) EXACT, the value of the exact integral.
c
      implicit none

      double precision bot
      double precision exact
      integer expon
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision top
c
c  Get the exact value of the integral.
c
      if ( mod ( expon, 2 ) .eq. 0 ) then

        top = 1
        bot = 1
        do i = 2, expon, 2
          top = top * ( i - 1 )
          bot = bot *   i
        end do

        bot = bot * dble ( expon + 2 )

        exact = r8_pi * dble ( top ) / dble ( bot )

      else

        exact = 0.0D+00
	
      end if

      return
      end
      subroutine hermite_exactness ( n, x, w, p_max )

c*********************************************************************72
c
cc HERMITE_EXACTNESS: monomial exactness for the Hermite integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points in the rule.
c
c    Input, double precision X(N), the quadrature points.
c
c    Input, double precision W(N), the quadrature weights.
c
c    Input, integer P_MAX, the maximum exponent.
c    0 <= P_MAX.
c
      implicit none

      integer n

      double precision e
      integer i
      integer p
      integer p_max
      double precision q
      double precision r8vec_dot_product
      double precision s
      double precision v(n)
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule for the Hermite integral'
      write ( *, '(a,i3)' ) '  Rule of order N = ', n
      write ( *, '(a)' ) '  Degree          Relative Error'
      write ( *, '(a)' ) ''

      do p = 0, p_max

        call hermite_integral ( p, s )

        do i = 1, n
          v(i) = x(i) ** p
        end do

        q = r8vec_dot_product ( n, w, v )

        if ( s .eq. 0.0D+00 ) then
          e = abs ( q )
        else
          e = abs ( q - s ) / abs ( s )
        end if

        write ( *, '(2x,i6,2x,f24.16)' ) p, e

      end do

      return
      end
      subroutine hermite_integral ( p, s )

c*********************************************************************72
c
cc HERMITE_INTEGRAL evaluates a monomial Hermite integral.
c
c  Discussion:
c
c    Integral ( -oo < x < +oo ) x^p exp(-x^2) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the exponent.
c    0 <= P.
c
c    Output, double precision S, the value of the integral.
c
      implicit none

      integer p
      double precision r8_factorial2
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision s

      if ( mod ( p, 2 ) .eq. 0 ) then
        s = r8_factorial2 ( p - 1 ) * sqrt ( r8_pi ) 
     &    / 2.0D+00 ** ( p / 2 )
      else
        s = 0.0D+00
      end if

      return
      end
      subroutine laguerre_exactness ( n, x, w, p_max )

c*********************************************************************72
c
cc LAGUERRE_EXACTNESS: monomial exactness for the Laguerre integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points in the rule.
c
c    Input, double precision X(N), the quadrature points.
c
c    Input, double precision W(N), the quadrature weights.
c
c    Input, integer P_MAX, the maximum exponent.
c    0 <= P_MAX.
c
      implicit none

      integer n

      double precision e
      integer i
      integer p
      integer p_max
      double precision q
      double precision r8vec_dot_product
      double precision s
      double precision v(n)
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule for the Laguerre integral'
      write ( *, '(a,i3)' ) '  Rule of order N = ', n
      write ( *, '(a)' ) '  Degree          Relative Error'
      write ( *, '(a)' ) ''

      do p = 0, p_max

        call laguerre_integral ( p, s )

        do i = 1, n
          v(i) = x(i) ** p
        end do

        q = r8vec_dot_product ( n, w, v )

        if ( s .eq. 0.0D+00 ) then
          e = abs ( q )
        else
          e = abs ( q - s ) / abs ( s )
        end if

        write ( *, '(2x,i6,2x,f24.16)' ) p, e

      end do

      return
      end
      subroutine laguerre_integral ( p, s )

c*********************************************************************72
c
cc LAGUERRE_INTEGRAL evaluates a monomial integral associated with L(n,x).
c
c  Discussion:
c
c    The integral:
c
c      integral ( 0 <= x < +oo ) x^p * exp ( -x ) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the exponent.
c    0 <= P.
c
c    Output, double precision S, the value of the integral.
c
      implicit none

      integer p
      double precision r8_factorial
      double precision s

      s = r8_factorial ( p )

      return
      end
      subroutine legendre_exactness ( n, x, w, p_max )

c*********************************************************************72
c
cc LEGENDRE_EXACTNESS: monomial exactness for the Legendre integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points in the rule.
c
c    Input, double precision X(N), the quadrature points.
c
c    Input, double precision W(N), the quadrature weights.
c
c    Input, integer P_MAX, the maximum exponent.
c    0 <= P_MAX.
c
      implicit none

      integer n

      double precision e
      integer i
      integer p
      integer p_max
      double precision q
      double precision r8vec_dot_product
      double precision s
      double precision v(n)
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Quadrature rule for the Legendre integral'
      write ( *, '(a,i3)' ) '  Rule of order N = ', n
      write ( *, '(a)' ) '  Degree          Relative Error'
      write ( *, '(a)' ) ''

      do p = 0, p_max

        call legendre_integral ( p, s )

        do i = 1, n
          v(i) = x(i) ** p
        end do

        q = r8vec_dot_product ( n, w, v )

        if ( s .eq. 0.0D+00 ) then
          e = abs ( q )
        else
          e = abs ( q - s ) / abs ( s )
        end if

        write ( *, '(2x,i6,2x,f24.16)' ) p, e

      end do

      return
      end
      subroutine legendre_integral ( p, s )

c*********************************************************************72
c
cc LEGENDRE_INTEGRAL evaluates a monomial Legendre integral.
c
c  Discussion:
c
c    To test a Legendre quadrature rule, we use it to approximate the
c    integral of a monomial:
c
c      integral ( -1 <= x <= +1 ) x^p dx
c
c    This routine is given the value of the exponent, and returns the
c    exact value of the integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the power.
c
c    Output, double precision S, the value of the exact integral.
c
      implicit none

      integer p
      double precision s
c
c  Get the exact value of the integral.
c
      if ( mod ( p, 2 ) .eq. 0 ) then

        s = 2.0D+00 / dble ( p + 1 )
     
      else

        s = 0.0D+00
     
      end if

      return
      end
      function r8_factorial ( n )

c*********************************************************************72
c
cc R8_FACTORIAL computes the factorial of N.
c
c  Discussion:
c
c    factorial ( N ) = product ( 1 <= I <= N ) I
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the factorial function.
c    If N is less than 1, the function value is returned as 1.
c
c    Output, double precision R8_FACTORIAL, the factorial of N.
c
      implicit none

      double precision r8_factorial
      integer i
      integer n

      r8_factorial = 1.0D+00

      do i = 1, n
        r8_factorial = r8_factorial * dble ( i )
      end do

      return
      end
      function r8_factorial2 ( n )

c*********************************************************************72
c
cc R8_FACTORIAL2 computes the double factorial function.
c
c  Discussion:
c
c    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
c                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
c
c  Example:
c
c     N    N!!
c
c     0     1
c     1     1
c     2     2
c     3     3
c     4     8
c     5    15
c     6    48
c     7   105
c     8   384
c     9   945
c    10  3840
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the double factorial
c    function.  If N is less than 1, R8_FACTORIAL2 is returned as 1.0.
c
c    Output, double precision R8_FACTORIAL2, the value of N!!.
c
      implicit none

      integer n
      double precision r8_factorial2
      double precision r8_n
      double precision value

      if ( n .lt. 1 ) then

        value = 1.0D+00

      else

        r8_n = dble ( n )
        value = 1.0D+00

10      continue

        if ( 1.0D+00 .lt. r8_n ) then
          value = value * r8_n
          r8_n = r8_n - 2.0D+00
          go to 10
        end if

      end if

      r8_factorial2 = value

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
