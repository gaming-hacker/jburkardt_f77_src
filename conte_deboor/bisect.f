      subroutine bisect ( f, a, b, xtol, iflag )

c*********************************************************************72
c
cc bisect uses bisection to locate a zero of a function.
c
c  Discussion:
c
c    The bisection method is used, in which the interval known to 
c    contain a zero is repeatedly halved.
c
c  Input:
c
c    F, name of function whose zero is sought.  Name must appear in an
c    external statement in the calling program.
c
c    A, B, endpoints of the interval wherein a zero is sought.
c
c    XTOL, the desired length of the output interval.
c
c  Output:
c
c    A, B, endpoints of the interval known to contain a zero of F.
c
c    IFLAG, an integer:
c    -1, failure since F has same sign at input points A and B.
c     0, termination since abs(A-B)/2 <= XTOL.
c     1, termination since abs(A-B)/2 is so small that addition to
c        (A+B)/2 makes no difference.
c
c  Reference:
c
c    Samuel Conte, Carl de Boor,
c    Elementary Numerical Analysis,
c    Third Edition,
c    SIAM, 2017,
c    ISBN: 978-1-611975-19-2.
c
      real a
      real b
      real error
      real f
      real fa
      real fm
      integer iflag
      real xm
      real xtol

      fa = f(a)

      if ( fa * f(b) .gt. 0.0 ) then
        iflag = -1
        write ( *, '(a)' ) '  F(X) is of same sign at A and B.'
        return
      end if

      error = abs ( b - a )

      do while ( xtol < error )

        error = error / 2.0
        if ( error <= xtol ) then
          iflag = 0
          return
        end if

        xm = ( a + b ) / 2.0
        if ( xm + error == xm ) then
          iflag = 1
          return
        end if

        fm = f(xm)

        if ( 0.0 < fa * fm ) then
          a = xm
          fa = fm
        else
          b = xm
        end if

      end do

      return
      end

