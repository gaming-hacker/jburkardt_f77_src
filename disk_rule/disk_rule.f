      subroutine disk_rule ( nr, nt, w, r, t )

c*********************************************************************72
c
cc DISK_RULE computes a quadrature rule for the unit disk.
c
c  Discussion:
c
c    The unit disk is the region:
c
c      x * x + y * y <= 1.
c
c    The integral I(f) is then approximated by
c
c      Q(f) = pi * sum ( 1 <= j <= NT ) sum ( 1 <= i <= NR ) 
c        W(i) * F ( R(i) * cos(T(j)), R(i) * sin(T(j)) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NR, the number of points in the radial rule.
c
c    Input, integer NT, the number of angles to use.
c
c    Output, double precision W(NR), the weights for the disk rule.
c
c    Output, double precision R(NR), T(NT), the (R,Theta) points for the rule.
c
      implicit none

      integer nr
      integer nt

      integer ir
      integer it
      double precision r(nr)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision w(nr)
      double precision wr(nr)
      double precision xr(nr)
c
c  Request a Legendre rule for [-1,+1].
c
      call legendre_ek_compute ( nr, xr, wr )
c
c  Shift the rule to [0,1].
c
      do ir = 1, nr
        xr(ir) = ( xr(ir) + 1.0D+00 ) / 2.0D+00
      end do

      do ir = 1, nr
        wr(ir) = wr(ir) / 2.0D+00
      end do
c
c  Compute the disk rule.
c
      do it = 1, nt
        t(it) = 2.0D+00 * r8_pi * dble ( it - 1 ) / dble ( nt )
      end do

      do ir = 1, nr
        w(ir) = wr(ir) / dble ( nt )
        r(ir) = sqrt ( xr(ir) )
      end do

      return
      end
      subroutine disk01_monomial_integral ( e, integral )

c*********************************************************************72
c
cc DISK01_MONOMIAL_INTEGRAL returns monomial integrals in the unit disk in 2D.
c
c  Discussion:
c
c    The integration region is 
c
c      X^2 + Y^2 <= 1.
c
c    The monomial is F(X,Y) = X^E(1) * Y^E(2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 January 2014
c
c  Author:
c
c    John Burkardt
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
      double precision r
      parameter ( r = 1.0D+00 )
      double precision r8_gamma
      integer s

      if ( e(1) .lt. 0 .or. e(2) .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISK01_MONOMIAL_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  All exponents must be nonnegative.'
        stop 1
      end if

      if ( mod ( e(1), 2 ) .eq. 1 .or. mod ( e(2), 2 ) .eq. 1 ) then

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
c
c  The surface integral is now adjusted to give the volume integral.
c
      s = e(1) + e(2) + 2

      integral = integral * r ** s / dble ( s )

      return
      end
      subroutine imtqlx ( n, d, e, z )

c*********************************************************************72
c
cc IMTQLX diagonalizes a symmetric tridiagonal matrix.
c
c  Discussion:
c
c    This routine is a slightly modified version of the EISPACK routine to
c    perform the implicit QL algorithm on a symmetric tridiagonal matrix.
c
c    The authors thank the authors of EISPACK for permission to use this
c    routine.
c
c    It has been modified to produce the product Q' * Z, where Z is an input
c    vector and Q is the orthogonal matrix diagonalizing the input matrix.
c    The changes consist (essentially) of applying the orthogonal
c    transformations directly to Z as they are generated.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 April 2011
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Sylvan Elhay, Jaroslav Kautsky,
c    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
c    Interpolatory Quadrature,
c    ACM Transactions on Mathematical Software,
c    Volume 13, Number 4, December 1987, pages 399-415.
c
c    Roger Martin, James Wilkinson,
c    The Implicit QL Algorithm,
c    Numerische Mathematik,
c    Volume 12, Number 5, December 1968, pages 377-383.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input/output, double precision D(N), the diagonal entries of the matrix.
c    On output, the information in D has been overwritten.
c
c    Input/output, double precision E(N), the subdiagonal entries of the
c    matrix, in entries E(1) through E(N-1).  On output, the information in
c    E has been overwritten.
c
c    Input/output, double precision Z(N).  On input, a vector.  On output,
c    the value of Q' * Z, where Q is the matrix that diagonalizes the
c    input symmetric tridiagonal matrix.
c
      implicit none

      integer n

      double precision b
      double precision c
      double precision d(n)
      double precision e(n)
      double precision f
      double precision g
      integer i
      integer ii
      integer itn
      parameter ( itn = 30 )
      integer j
      integer k
      integer l
      integer m
      integer mml
      double precision p
      double precision prec
      double precision r
      double precision r8_epsilon
      double precision s
      double precision test
      double precision z(n)

      prec = r8_epsilon ( prec )

      if ( n .eq. 1 ) then
        return
      end if

      e(n) = 0.0D+00

      do l = 1, n

        j = 0

10      continue

          do m = l, n

            if ( m == n ) then
              go to 20
            end if

            test = prec * ( abs ( d(m) ) + abs ( d(m+1) ) )

            if ( abs ( e(m) ) .le. test ) then
              go to 20
            end if

          end do

20        continue

          p = d(l)

          if ( m .eq. l ) then
            go to 30
          end if

          if ( itn .le. j ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'IMTQLX - Fatal error!'
            write ( *, '(a)' ) '  Iteration limit exceeded.'
            write ( *, '(a,i8)' ) '  J = ', j
            write ( *, '(a,i8)' ) '  L = ', l
            write ( *, '(a,i8)' ) '  M = ', m
            write ( *, '(a,i8)' ) '  N = ', n
            stop
          end if

          j = j + 1
          g = ( d(l+1) - p ) / ( 2.0D+00 * e(l) )
          r =  sqrt ( g * g + 1.0D+00 )
          g = d(m) - p + e(l) / ( g + sign ( r, g ) )
          s = 1.0D+00
          c = 1.0D+00
          p = 0.0D+00
          mml = m - l

          do ii = 1, mml

            i = m - ii
            f = s * e(i)
            b = c * e(i)

            if ( abs ( g ) .le. abs ( f ) ) then
              c = g / f
              r =  sqrt ( c * c + 1.0D+00 )
              e(i+1) = f * r
              s = 1.0D+00 / r
              c = c * s
            else
              s = f / g
              r =  sqrt ( s * s + 1.0D+00 )
              e(i+1) = g * r
              c = 1.0D+00 / r
              s = s * c
            end if

            g = d(i+1) - p
            r = ( d(i) - g ) * s + 2.0D+00 * c * b
            p = s * r
            d(i+1) = g + p
            g = c * r - b
            f = z(i+1)
            z(i+1) = s * z(i) + c * f
            z(i) = c * z(i) - s * f

          end do

          d(l) = d(l) - p
          e(l) = g
          e(m) = 0.0D+00

        go to 10

30      continue

      end do
c
c  Sorting.
c
      do ii = 2, n

        i = ii - 1
        k = i
        p = d(i)

        do j = ii, n
          if ( d(j) .lt. p ) then
            k = j
            p = d(j)
          end if
        end do

        if ( k .ne. i ) then
          d(k) = d(i)
          d(i) = p
          p = z(i)
          z(i) = z(k)
          z(k) = p
        end if

      end do

      return
      end
      subroutine legendre_ek_compute ( n, x, w )

c*********************************************************************72
c
cc LEGENDRE_EK_COMPUTE: Legendre quadrature rule by the Elhay-Kautsky method.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2011
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Sylvan Elhay, Jaroslav Kautsky,
c    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
c    Interpolatory Quadrature,
c    ACM Transactions on Mathematical Software,
c    Volume 13, Number 4, December 1987, pages 399-415.
c
c  Parameters:
c
c    Input, integer N, the order.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = 2.0D+00
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        bj(i) = sqrt ( dble ( i * i ) / dble ( 4 * i * i - 1 ) )
      end do

      do i = 1, n
        x(i) = 0.0D+00
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      do i = 1, n
        w(i) = w(i)**2
      end do

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

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
