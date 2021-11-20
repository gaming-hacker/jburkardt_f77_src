      function glomin ( a, b, c, m, machep, e, t, f, x, calls )

c*********************************************************************72
c
cc glomin() seeks a global minimum of a function F(X) in an interval [A,B].
c
c  Discussion:
c
c    This function assumes: 
c    * F(X) is twice continuously differentiable over [A,B];
c    * F''(X) <= M for all X in [A,B];
c    * the user can supply the value of this upper bound M.
c
c    Thanks to Hans Bieshaar for supplying several corrections to the code,
c    28 May 2021
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2021
c
c  Author:
c
c    Richard Brent
c    Modifications by John Burkardt
c
c  Reference:
c
c    Richard Brent,
c    Algorithms for Minimization Without Derivatives,
c    Dover, 2002,
c    ISBN: 0-486-41998-3,
c    LC: QA402.5.B74.
c
c  Input:
c
c    double precision A, B, the endpoints of the interval.
c    It must be the case that A < B.
c
c    double precision C, an initial guess for the global
c    minimizer.  If no good guess is known, C = A or B is acceptable.
c
c    double precision M, the bound on the second derivative.
c
c    double precision MACHEP, an estimate for the relative machine
c    precision.
c
c    double precision E, a positive tolerance, a bound for the
c    absolute error in the evaluation of F(X) for any X in [A,B].
c
c    double precision T, a positive error tolerance.
c
c    external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose global minimum is being sought.
c
c  Output:
c
c    double precision X, the estimated value of the abscissa
c    for which F attains its global minimum value in [A,B].
c
c    integer CALLS: the number of function calls.
c
c    double precision GLOMIN, the value F(X).
c
      implicit none

      double precision a
      double precision a0
      double precision a2
      double precision a3
      double precision b
      double precision c
      integer calls
      double precision d0
      double precision d1
      double precision d2
      double precision e
      double precision f
      logical force_first
      double precision glomin
      double precision h
      integer k
      double precision m
      double precision m2
      double precision machep
      double precision p
      double precision q
      double precision qs
      double precision r
      double precision s
      double precision t
      double precision x
      double precision y
      double precision y0
      double precision y1
      double precision y2
      double precision y3
      double precision yb
      double precision z0
      double precision z1
      double precision z2

      calls = 0
      a0 = b
      x = a0
      a2 = a
      y0 = f ( b )
      calls = calls + 1
      yb = y0
      y2 = f ( a )
      calls = calls + 1
      y = y2

      if ( y0 < y ) then
        y = y0
      else
        x = a
      end if

      if ( m <= 0.0D+00 .or. b <= a ) then
        glomin = y
        return
      end if

      m2 = 0.5D+00 * ( 1.0D+00 + 16.0D+00 * machep ) * m

      if ( c <= a .or. b <= c ) then
        c = 0.5D+00 * ( a + b )
      end if

      y1 = f ( c )
      calls = calls + 1
      k = 3
      d0 = a2 - c
      h = 9.0D+00 / 11.0D+00

      if ( y1 < y ) then
        x = c
        y = y1
      end if

      do

        d1 = a2 - a0
        d2 = c - a0
        z2 = b - a2
        z0 = y2 - y1
        z1 = y2 - y0
        r = d1 * d1 * z0 - d0 * d0 * z1
        p = r
        qs = 2.0D+00 * ( d0 * z1 - d1 * z0 )
        q = qs
c
c  Loop control corrected by Hans Bieshaar, 28 May 2021.
c
        force_first = .true.

        if ( 100000 < k .and. y < y2 ) then
          k = mod ( 1611 * k, 1048576 )
          q = 1.0D+00
          r = ( b - a ) * 0.00001D+00 * dble ( k )
          force_first = .false.
        end if

        do while ( r < z2 .or. force_first )

          force_first = .false.

          if ( q * ( r * ( yb - y2 ) + z2 * q * ( ( y2 - y ) + t ) ) < 
     &      z2 * m2 * r * ( z2 * q - r ) ) then

            a3 = a2 + r / q
            y3 = f ( a3 )
            calls = calls + 1

            if ( y3 < y ) then
              x = a3
              y = y3
            end if

          end if

          k = mod ( 1611 * k, 1048576 )
          q = 1.0D+00
          r = ( b - a ) * 0.00001D+00 * dble ( k )

        end do

        r = m2 * d0 * d1 * d2
        s = sqrt ( ( ( y2 - y ) + t ) / m2 )
        h = 0.5D+00 * ( 1.0D+00 + h )
        p = h * ( p + 2.0D+00 * r * s )
c
c  Correction supplied by Hans Bieshaar, 27 May 2021.
c
        q = r + 0.5D+00 * qs
        r = - 0.5D+00 * ( d0 + ( z0 + 2.01D+00 * e ) / ( d0 * m2 ) )

        if ( r < s .or. d0 < 0.0D+00 ) then
          r = a2 + s
        else
          r = a2 + r
        end if

        if ( 0.0D+00 < p * q ) then
          a3 = a2 + p / q
        else
          a3 = r
        end if

90      continue

        if ( a3 .lt. r ) a3 = r

        if ( a3 .lt. b ) go to 100
        a3 = b
        y3 = yb
        go to 110
100     continue
        y3 = f ( a3 )
        calls = calls + 1
110     continue

        if ( y3 .lt. y ) then
          x = a3
          y = y3
        end if

        d0 = a3 - a2

        if ( a3 .le. r ) go to 130

        p = 2.0D+00 * ( y2 - y3 ) / ( m * d0 )

        if ( abs ( p ) .ge. ( 1.0D+00 + 9.0D+00 * machep ) * d0 ) 
     &    go to 130
        if ( 0.5D+00 * m2 * ( d0 * d0 + p * p ) .le.
     &    ( y2 - y ) + ( y3 - y ) + 2.0D+00 * t ) go to 130

        a3 = 0.5D+00 * ( a2 + a3 )
        h = 0.9D+00 * h
        go to 90

130     continue

        if ( b <= a3 ) then
          exit
        end if

        a0 = c
        c = a2
        a2 = a3
        y0 = y1
        y1 = y2
        y2 = y3

      end do

      glomin = y

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc r8_epsilon() returns the R8 roundoff unit.
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
c  Output:
c
c    double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end

