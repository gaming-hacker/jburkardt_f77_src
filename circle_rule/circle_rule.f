      subroutine circle_rule ( nt, w, t )

c*********************************************************************72
c
cc CIRCLE_RULE computes a quadrature rule for the unit circle.
c
c  Discussion:
c
c    The unit circle is the region:
c
c      x * x + y * y = 1.
c
c    The integral I(f) is then approximated by
c
c      Q(f) = 2 * pi * sum ( 1 <= i <= NT ) W(i) * F ( cos(T(i)), sin(T(i)) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NT, the number of angles to use.
c
c    Output, double precision W(NT), the weights for the rule.
c
c    Output, double precision T(NT), the angles for the rule.
c
      implicit none

      integer nt

      integer it
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision w(nt)

      do it = 1, nt
        w(it) = 1.0D+00 / dble ( nt )
        t(it) = 2.0D+00 * r8_pi * dble ( it - 1 ) / dble ( nt )
      end do

      return
      end
      subroutine circle01_monomial_integral ( e, integral )

c*********************************************************************72
c
cc CIRCLE01_MONOMIAL_INTEGRAL: integrals on circumference of unit circle in 2D.
c
c  Discussion:
c
c    The integration region is 
c
c      X^2 + Y^2 = 1.
c
c    The monomial is F(X,Y) = X^E(1) * Y^E(2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 January 2014
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
c    Academic Press, 1984, page 263.
c
c  Parameters:
c
c    Input, integer E(2), the exponents of X and Y in the 
c    monomial.  Each exponent must be nonnegative.
c
c    Output, double precision INTEGRAL, the integral.
c
      implicit none

      double precision arg
      integer e(2)
      integer i
      double precision integral
      double precision r8_gamma

      do i = 1, 2
        if ( e(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'CIRCLE01_MONOMIAL_INTEGRAL - Fatal error!'
          write ( *, '(a)' ) '  All exponents must be nonnegative.'
          stop 1
        end if
      end do

      if ( mod ( e(1), 2 ) == 1 .or.
     &     mod ( e(2), 2 ) == 1 ) then

        integral = 0.0D+00

      else

        integral = 2.0D+00

        do i = 1, 2
          arg = 0.5D+00 * dble ( e(i) + 1 )
          integral = integral * r8_gamma ( arg )
        end do

        arg = 0.5D+00 * dble ( e(1) + e(2) + 2 )
        integral = integral / r8_gamma ( arg )

      end if

      return
      end
      function r8_gamma ( x )

c*********************************************************************72
c
cc R8_GAMMA evaluates Gamma(X) for a real argument.
c
c  Discussion:
c
c    This routine calculates the gamma function for a real argument X.
c    Computation is based on an algorithm outlined in reference 1.
c    The program uses rational functions that approximate the gamma
c    function to at least 20 significant decimal digits.  Coefficients
c    for the approximation over the interval (1,2) are unpublished.
c    Those for the approximation for 12 <= X are from reference 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 January 2008
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody,
c    An Overview of Software Development for Special Functions,
c    in Numerical Analysis Dundee, 1975,
c    edited by GA Watson,
c    Lecture Notes in Mathematics 506,
c    Springer, 1976.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, 
c    Charles Mesztenyi, John Rice, Henry Thatcher, 
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision R8_GAMMA, the value of the function.
c
      implicit none

      double precision c(7)
      double precision eps
      double precision fact
      integer i
      integer n
      double precision p(8)
      logical parity
      double precision pi
      double precision q(8)
      double precision r8_gamma
      double precision res
      double precision sqrtpi
      double precision sum
      double precision x
      double precision xbig
      double precision xden
      double precision xinf
      double precision xminin
      double precision xnum
      double precision y
      double precision y1
      double precision ysq
      double precision z
c
c  Mathematical constants
c
      data sqrtpi / 0.9189385332046727417803297D+00 /
      data pi / 3.1415926535897932384626434D+00 /
c
c  Machine dependent parameters
c
      data xbig / 171.624D+00 /
      data xminin / 2.23D-308 /
      data eps / 2.22D-16 /
      data xinf /1.79D+308 /
c
c  Numerator and denominator coefficients for rational minimax
c  approximation over (1,2).
c
      data p /
     & -1.71618513886549492533811d+00,
     &  2.47656508055759199108314d+01,
     & -3.79804256470945635097577d+02,
     &  6.29331155312818442661052d+02,
     &  8.66966202790413211295064d+02,
     & -3.14512729688483675254357d+04,
     & -3.61444134186911729807069d+04,
     &  6.64561438202405440627855d+04 /

      data q /
     & -3.08402300119738975254353D+01,
     &  3.15350626979604161529144D+02,
     & -1.01515636749021914166146D+03,
     & -3.10777167157231109440444D+03,
     &  2.25381184209801510330112D+04,
     &  4.75584627752788110767815D+03,
     & -1.34659959864969306392456D+05,
     & -1.15132259675553483497211D+05 /
c
c  Coefficients for minimax approximation over (12, INF).
c
      data c /
     & -1.910444077728D-03,
     &  8.4171387781295D-04,
     & -5.952379913043012D-04,
     &  7.93650793500350248D-04,
     & -2.777777777777681622553D-03,
     &  8.333333333333333331554247D-02,
     &  5.7083835261D-03 /

      parity = .false.
      fact = 1.0D+00
      n = 0
      y = x
c
c  Argument is negative.
c
      if ( y .le. 0.0D+00 ) then

        y = - x
        y1 = aint ( y )
        res = y - y1

        if ( res .ne. 0.0D+00 ) then

          if ( y1 .ne. aint ( y1 * 0.5D+00 ) * 2.0D+00 ) then
            parity = .true.
          end if

          fact = - pi / sin ( pi * res )
          y = y + 1.0D+00

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Argument is positive.
c
      if ( y .lt. eps ) then
c
c  Argument < EPS.
c
        if ( xminin .le. y ) then
          res = 1.0D+00 / y
        else
          res = xinf
          r8_gamma = res
          return
        end if

      else if ( y .lt. 12.0D+00 ) then

        y1 = y
c
c  0.0 < argument < 1.0.
c
        if ( y .lt. 1.0D+00 ) then

          z = y
          y = y + 1.0D+00
c
c  1.0 < argument < 12.0.
c  Reduce argument if necessary.
c
        else

          n = int ( y ) - 1
          y = y - dble ( n )
          z = y - 1.0D+00

        end if
c
c  Evaluate approximation for 1.0 < argument < 2.0.
c
        xnum = 0.0D+00
        xden = 1.0D+00
        do i = 1, 8
          xnum = ( xnum + p(i) ) * z
          xden = xden * z + q(i)
        end do

        res = xnum / xden + 1.0D+00
c
c  Adjust result for case  0.0 < argument < 1.0.
c
        if ( y1 .lt. y ) then

          res = res / y1
c
c  Adjust result for case 2.0 < argument < 12.0.
c
        else if ( y .lt. y1 ) then

          do i = 1, n
            res = res * y
            y = y + 1.0D+00
          end do

        end if

      else
c
c  Evaluate for 12.0 <= argument.
c
        if ( y .le. xbig ) then

          ysq = y * y
          sum = c(7)
          do i = 1, 6
            sum = sum / ysq + c(i)
          end do
          sum = sum / y - y + sqrtpi
          sum = sum + ( y - 0.5D+00 ) * log ( y )
          res = exp ( sum )

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Final adjustments and return.
c
      if ( parity ) then
        res = - res
      end if

      if ( fact .ne. 1.0D+00 ) then
        res = fact / res
      end if

      r8_gamma = res

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
