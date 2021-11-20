      function zero ( a, b, t, f, calls )

c*********************************************************************72
c
cc zero() seeks the root of a function F(X) in an interval [A,B].
c
c  Discussion:
c
c    The interval [A,B] must be a change of sign interval for F.
c    That is, F(A) and F(B) must be of opposite signs.  Then
c    assuming that F is continuous implies the existence of at least
c    one value C between A and B for which F(C) = 0.
c
c    The location of the zero is determined to within an accuracy
c    of 4 * EPSILON * abs ( C ) + 2 * T.
c
c    Thanks to Thomas Secretin for pointing out a transcription error in the
c    setting of the value of P, 11 February 2013.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 July 2021
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
c    double precision A, B, the endpoints of the change of sign interval.
c
c    double precision T, a positive error tolerance.
c
c    external double precision F, the name of a user-supplied
c    function, of the form "FUNCTION F ( X )", which evaluates the
c    function whose zero is being sought.
c
c  Output:
c
c    integer CALLS: the number of times the function F was called.
c
c    double precision ZERO, the estimated zero of the function F.
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer calls
      double precision d
      double precision e
      double precision f
      double precision fa
      double precision fb
      double precision fc
      double precision m
      double precision machep
      parameter ( machep = 2.220446049250313D-016 )
      double precision p
      double precision q
      double precision r
      double precision s
      double precision sa
      double precision sb
      double precision t
      double precision tol
      double precision zero

      calls = 0
c
c  Make local copies of A and B.
c
      sa = a
      fa = f ( sa )
      calls = calls + 1

      sb = b
      fb = f ( sb )
      calls = calls + 1

10    continue

      c = sa
      fc = fa
      e = sb - sa
      d = e

20    continue

      if ( abs ( fc ) .lt. abs ( fb ) ) then
        sa = sb
        sb = c
        c = sa
        fa = fb
        fb = fc
        fc = fa
      end if

c30    continue

      tol = 2.0D+00 * machep * abs ( sb ) + t
      m = 0.5D+00 * ( c - sb )
      if ( abs ( m ) .le. tol .or. fb .eq. 0.0D+00 ) go to 140
      if ( abs ( e ) .ge. tol .and. abs ( fa ) .gt. abs ( fb ) ) 
     &  go to 40

      e = m
      d = e
      go to 100

40    continue

      s = fb / fa
      if ( sa .ne. c ) go to 50

      p = 2.0D+00 * m * s
      q = 1.0D+00 - s
      go to 60

50    continue

      q = fa / fc
      r = fb / fc
      p = s * 
     &  ( 2.0D+00 * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0D+00 ) )
      q = ( q - 1.0D+00 ) * ( r - 1.0D+00 ) * ( s - 1.0D+00 )

60    continue

      if ( p .le. 0.0D+00 ) go to 70

      q = - q
      go to 80

70    continue

      p = - p

80    continue

      s = e
      e = d
      if ( 2.0D+00 * p .ge. 3.0D+00 * m * q - abs ( tol * q ) .or.
     &  p .ge. abs ( 0.5D+00 * s * q ) ) go to 90

      d = p / q
      go to 100

90    continue

      e = m
      d = e

100   continue

      sa = sb
      fa = fb
      if ( abs ( d ) .le. tol ) go to 110
      sb = sb + d
      go to 130

110   continue

      if ( m .le. 0.0D+00 ) go to 120
      sb = sb + tol
      go to 130

120   continue

      sb = sb - tol

130   continue

      fb = f ( sb )
      calls = calls + 1

      if ( fb .gt. 0.0D+00 .and. fc .gt. 0.0D+00 ) go to 10
      if ( fb .le. 0.0D+00 .and. fc .le. 0.0D+00 ) go to 10
      go to 20

140   continue

      zero = sb

      return
      end

