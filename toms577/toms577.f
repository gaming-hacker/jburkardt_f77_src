      function rc ( x, y, errtol, ierr )

c*********************************************************************72
c
cc RC computes the elementary integral RC(X,Y).
c
c  Discussion:
c
c    This function computes the elementary integral
c
c      RC(X,Y) = Integral ( 0 <= T < oo )
c
c                              -1/2     -1
c                    (1/2)(T+X)    (T+Y)  DT,
c
c    where X is nonnegative and Y is positive.  The duplication
c    theorem is iterated until the variables are nearly equal,
c    and the function is then expanded in Taylor series to fifth
c    order.  
c
c    Logarithmic, inverse circular, and inverse hyperbolic 
c    functions can be expressed in terms of RC.  
c
c    Check by addition theorem: 
c
c      RC(X,X+Z) + RC(Y,Y+Z) = RC(0,Z),
c      where X, Y, and Z are positive and X * Y = Z * Z.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    Relative error due to truncation is less than
c      16 * ERRTOL ^ 6 / (1 - 2 * ERRTOL).
c    Sample choices:  
c      ERRTOL   Relative truncation error less than
c      1.D-3    2.D-17
c      3.D-3    2.D-14
c      1.D-2    2.D-11
c      3.D-2    2.D-8
c      1.D-1    2.D-5
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision c1
      double precision c2
      double precision errtol
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision rc
      double precision s
      double precision sn
      double precision uplim
      double precision x
      double precision xn
      double precision y
      double precision yn
C
C          LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C          LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
C          UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
C
      save lolim
      save uplim

      data lolim /3.D-78/
      data uplim /1.D+75/

      if ( 
     &  x .lt. 0.0d0 .or.  
     &  y .le. 0.0d0 .or.
     &  ( x + y ) .lt. lolim .or.
     &  uplim .lt. x .or.
     &  uplim .lt. y ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RC - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a)' ) ''
        ierr = 1
        rc = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y

      do

        mu = ( xn + yn + yn ) / 3.0d0
        sn = ( yn + mu ) / mu - 2.0d0

        if ( abs ( sn ) .lt. errtol ) then
          c1 = 1.0d0 / 7.0d0
          c2 = 9.0d0 / 22.0d0
          s = sn * sn * ( 0.3d0 
     &      + sn * ( c1 + sn * ( 0.375d0 + sn * c2 ) ) )
          rc = ( 1.0d0 + s ) / sqrt ( mu )
          return
        end if

        lamda = 2.0d0 * sqrt ( xn ) * sqrt ( yn ) + yn
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0

      end do

      end
      function rd ( x, y, z, errtol, ierr )

c*********************************************************************72
c
cc RD computes an incomplete elliptic integral of the second kind, RD(X,Y,Z).
c
c  Discussion:
c
c    This function computes an incomplete elliptic integral of the second kind.
c
C    RD(X,Y,Z) = Integral ( 0 <= T < oo )
C
C                                -1/2     -1/2     -3/2
C                      (3/2)(T+X)    (T+Y)    (T+Z)    DT,
C
C    where X and Y are nonnegative, X + Y is positive, and Z is positive.
c
c    If X or Y is zero, the integral is complete.
c
C    The duplication theorem is iterated until the variables are
c    nearly equal, and the function is then expanded in Taylor
c    series to fifth order.  
C
C    Check: 
c
c      RD(X,Y,Z) + RD(Y,Z,X) + RD(Z,X,Y) = 3 / sqrt ( X * Y * Z ), 
c      where X, Y, and Z are positive.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    The relative error due to truncation is less than
c      3 * ERRTOL ^ 6 / (1-ERRTOL) ^ 3/2.
c    Sample choices:
c      ERRTOL   Relative truncation error less than
c      1.D-3    4.D-18
c      3.D-3    3.D-15
c      1.D-2    4.D-12
c      3.D-2    3.D-9
c      1.D-1    4.D-6
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision ea
      double precision eb
      double precision ec
      double precision ed
      double precision ef
      double precision epslon
      double precision errtol
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision power4
      double precision rd
      double precision sigma
      double precision s1
      double precision s2
      double precision uplim
      double precision x
      double precision xn
      double precision xndev
      double precision xnroot
      double precision y
      double precision yn
      double precision yndev
      double precision ynroot
      double precision z
      double precision zn
      double precision zndev
      double precision znroot
C
C          LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C          LOLIM IS NOT LESS THAN 2 / (MACHINE MAXIMUM) ^ (2/3).
C          UPLIM IS NOT GREATER THAN (0.1 * ERRTOL / MACHINE
C          MINIMUM) ^ (2/3), WHERE ERRTOL IS DESCRIBED BELOW.
C          IN THE FOLLOWING TABLE IT IS ASSUMED THAT ERRTOL WILL
C          NEVER BE CHOSEN SMALLER THAN 1.D-5.
C
      save lolim
      save uplim

      data lolim /6.D-51/
      data uplim /1.D+48/

      if ( 
     &  x .lt. 0.0D+00 .or.
     &  y .lt. 0.0D+00 .or.
     &  x + y .lt. lolim .or.
     &  z .lt. lolim .or.
     &  x .gt. uplim .or.
     &  y .gt. uplim .or.
     &  z .gt. uplim ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RD - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a,d23.16)' ) '  Z = ', z
        write ( *, '(a)' ) ''
        ierr = 1
        rd = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y
      zn = z
      sigma = 0.0d0
      power4 = 1.0d0

      do

        mu = ( xn + yn + 3.0d0 * zn ) * 0.2d0
        xndev = ( mu - xn ) / mu
        yndev = ( mu - yn ) / mu
        zndev = ( mu - zn ) / mu
        epslon = max ( abs ( xndev ), abs ( yndev ), abs ( zndev ) )

        if ( epslon .lt. errtol ) then
          c1 = 3.0d0 / 14.0d0
          c2 = 1.0d0 / 6.0d0
          c3 = 9.0d0 / 22.0d0
          c4 = 3.0d0 / 26.0d0
          ea = xndev * yndev
          eb = zndev * zndev
          ec = ea - eb
          ed = ea - 6.0d0 * eb
          ef = ed + ec + ec
          s1 = ed 
     &      * ( - c1 + 0.25d0 * c3 * ed - 1.5d0 * c4 * zndev * ef )
          s2 = zndev 
     &      * ( c2 * ef + zndev * ( - c3 * ec + zndev * c4 * ea ) )
          rd = 3.0d0 * sigma 
     &      + power4 * ( 1.0d0 + s1 + s2 ) / ( mu * sqrt ( mu ) )

          return
        end if

        xnroot = sqrt ( xn )
        ynroot = sqrt ( yn )
        znroot = sqrt ( zn )
        lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot
        sigma = sigma + power4 / ( znroot * ( zn + lamda ) )
        power4 = power4 * 0.25d0
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0
        zn = ( zn + lamda ) * 0.25d0

      end do

      end
      function rf ( x, y, z, errtol, ierr )

c*********************************************************************72
C
cc RF computes an incomplete elliptic integral of the first kind, RF(X,Y,Z).
c
c  Discussion:
c
c    This function computes the incomplete elliptic integral of the first kind.
c
c    RF(X,Y,Z) = Integral ( 0 <= T < oo )
c
c                                -1/2     -1/2     -1/2
c                      (1/2)(T+X)    (T+Y)    (T+Z)    DT,
c
c    where X, Y, and Z are nonnegative and at most one of them is zero.
c
c    If X or Y or Z is zero, the integral is complete.
c
C    The duplication theorem is iterated until the variables are
c    nearly equal, and the function is then expanded in Taylor
c    series to fifth order.  
c
c    Check by addition theorem: 
c
c      RF(X,X+Z,X+W) + RF(Y,Y+Z,Y+W) = RF(0,Z,W), 
c      where X, Y, Z, W are positive and X * Y = Z * W.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
C    Relative error due to truncation is less than
C      ERRTOL ^ 6 / (4 * (1 - ERRTOL)).
C    Sample choices:
c      ERRTOL   Relative truncation error less than
C      1.D-3    3.D-19
C      3.D-3    2.D-16
C      1.D-2    3.D-13
C      3.D-2    2.D-10
C      1.D-1    3.D-7
C
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision e2
      double precision e3
      double precision epslon
      double precision errtol
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision rf
      double precision s
      double precision uplim
      double precision x
      double precision xn
      double precision xndev
      double precision xnroot
      double precision y
      double precision yn
      double precision yndev
      double precision ynroot
      double precision z
      double precision zn
      double precision zndev
      double precision znroot
C
C          LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C          LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
C          UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
C
      save lolim
      save uplim

      data lolim /3.D-78/
      data uplim /1.D+75/

      if (
     &  x .lt. 0.0D+00 .or.
     &  y .lt. 0.0D+00 .or.
     &  z .lt. 0.0D+00 .or.
     &  x + y .lt. lolim .or.
     &  x + z .lt. lolim .or.
     &  y + z .lt. lolim .or.
     &  uplim .le. x .or.
     &  uplim .le. y .or.
     &  uplim .le. z ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RF - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a,d23.16)' ) '  Z = ', z
        write ( *, '(a)' ) ''
        ierr = 1
        rf = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y
      zn = z

      do

        mu = ( xn + yn + zn ) / 3.0d0
        xndev = 2.0d0 - ( mu + xn ) / mu
        yndev = 2.0d0 - ( mu + yn ) / mu
        zndev = 2.0d0 - ( mu + zn ) / mu
        epslon = max ( abs ( xndev ), abs ( yndev ), abs ( zndev ) )

        if ( epslon .lt. errtol ) then
          c1 = 1.0d0 / 24.0d0
          c2 = 3.0d0 / 44.0d0
          c3 = 1.0d0 / 14.0d0
          e2 = xndev * yndev - zndev * zndev
          e3 = xndev * yndev * zndev
          s = 1.0d0 + ( c1 * e2 - 0.1d0 - c2 * e3 ) * e2 + c3 * e3
          rf = s / sqrt ( mu ) 
          return
        end if

        xnroot = sqrt ( xn )
        ynroot = sqrt ( yn )
        znroot = sqrt ( zn )
        lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0
        zn = ( zn + lamda ) * 0.25d0

      end do

      end
      function rj ( x, y, z, p, errtol, ierr )

c*********************************************************************72
c
cc RJ computes an incomplete elliptic integral of the third kind, RJ(X,Y,Z,P).
c
c  Discussion:
c
c    This function computes an incomplete elliptic integral of the third kind.
c
C    RJ(X,Y,Z,P) = Integral ( 0 <= T < oo )
C
C                                  -1/2     -1/2     -1/2     -1
C                        (3/2)(T+X)    (T+Y)    (T+Z)    (T+P)  DT,
C
C    where X, Y, and Z are nonnegative, at most one of them is
c    zero, and P is positive.
c
c    If X or Y or Z is zero, then the integral is complete.
c
c    The duplication theorem is iterated until the variables are nearly equal, 
c    and the function is then expanded in Taylor series to fifth order.  
C
C    Check by addition theorem: 
c
c      RJ(X,X+Z,X+W,X+P)
C      + RJ(Y,Y+Z,Y+W,Y+P) + (A-B) * RJ(A,B,B,A) + 3 / sqrt ( A)
C      = RJ(0,Z,W,P), where X,Y,Z,W,P are positive and X * Y
C      = Z * W,  A = P * P * (X+Y+Z+W),  B = P * (P+X) * (P+Y),
C      and B - A = P * (P-Z) * (P-W).  
c
c      The sum of the third and fourth terms on the left side is 3 * RC(A,B).
C
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, P, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    Relative error due to truncation of the series for rj
c    is less than 3 * ERRTOL ^ 6 / (1 - ERRTOL) ^ 3/2.
c    An error tolerance (ETOLRC) will be passed to the subroutine
c    for RC to make the truncation error for RC less than for RJ.
c    Sample choices:
c      ERRTOL   Relative truncation error less than
c      1.D-3    4.D-18
c      3.D-3    3.D-15
c      1.D-2    4.D-12
c      3.D-2    3.D-9
c      1.D-1    4.D-6
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision alfa
      double precision beta
      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision ea
      double precision eb
      double precision ec
      double precision e2
      double precision e3
      double precision epslon
      double precision errtol
      double precision etolrc
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision p
      double precision pn
      double precision pndev
      double precision power4
      double precision rc
      double precision rj
      double precision sigma
      double precision s1
      double precision s2
      double precision s3
      double precision uplim
      double precision x
      double precision xn
      double precision xndev
      double precision xnroot
      double precision y
      double precision yn
      double precision yndev
      double precision ynroot
      double precision z
      double precision zn
      double precision zndev
      double precision znroot

C          LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C          LOLIM IS NOT LESS THAN THE CUBE ROOT OF THE VALUE
C          OF LOLIM USED IN THE SUBROUTINE FOR RC.
C          UPLIM IS NOT GREATER THAN 0.3 TIMES THE CUBE ROOT OF
C          THE VALUE OF UPLIM USED IN THE SUBROUTINE FOR RC.
C
      save lolim
      save uplim

      data lolim /2.D-26/
      data uplim /3.D+24/

      if ( 
     &  x .lt. 0.0D+00 .or.
     &  y .lt. 0.0D+00 .or.
     &  z .lt. 0.0D+00 .or.
     &  x + y .lt. lolim .or.
     &  x + z .lt. lolim .or.
     &  y + z .lt. lolim .or.
     &  p .lt. lolim .or.
     &  uplim .lt. x .or.
     &  uplim .lt. y .or.
     &  uplim .lt. z .or.
     &  uplim .lt. p ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RJ - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a,d23.16)' ) '  Z = ', z
        write ( *, '(a,d23.16)' ) '  P = ', p
        write ( *, '(a)' ) ''
        ierr = 1
        rj = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y
      zn = z
      pn = p
      sigma = 0.0d0
      power4 = 1.0d0
      etolrc = 0.5d0 * errtol

      do

        mu = ( xn + yn + zn + pn + pn ) * 0.2d0
        xndev = ( mu - xn ) / mu
        yndev = ( mu - yn ) / mu
        zndev = ( mu - zn ) / mu
        pndev = ( mu - pn ) / mu
        epslon = max ( abs ( xndev ), abs ( yndev ), abs ( zndev ), 
     &    abs ( pndev ) )

        if ( epslon .lt. errtol ) then
          c1 = 3.0d0 / 14.0d0
          c2 = 1.0d0 / 3.0d0
          c3 = 3.0d0 / 22.0d0
          c4 = 3.0d0 / 26.0d0
          ea = xndev * ( yndev + zndev ) + yndev * zndev
          eb = xndev * yndev * zndev
          ec = pndev * pndev
          e2 = ea - 3.0d0 * ec
          e3 = eb + 2.0d0 * pndev * ( ea - ec )
          s1 = 1.0d0 
     &      + e2 * ( - c1 + 0.75d0 * c3 * e2 - 1.5d0 * c4 * e3 )
          s2 = eb * ( 0.5d0 * c2 + pndev * ( - c3 - c3 + pndev * c4 ) )
          s3 = pndev * ea * ( c2 - pndev * c3 ) - c2 * pndev * ec
          rj = 3.0d0 * sigma 
     &      + power4 * ( s1 + s2 + s3 ) / ( mu * sqrt ( mu ) )
          return
        end if

        xnroot = sqrt ( xn )
        ynroot = sqrt ( yn )
        znroot = sqrt ( zn )
        lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot
        alfa = pn * ( xnroot + ynroot + znroot ) 
     &    + xnroot * ynroot * znroot
        alfa = alfa * alfa
        beta = pn * ( pn + lamda ) * ( pn + lamda )
        sigma = sigma + power4 * rc ( alfa, beta, etolrc, ierr )

        if ( ierr .ne. 0 ) then
          rj = 0.0D+00
          return
        end if

        power4 = power4 * 0.25d0
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0
        zn = ( zn + lamda ) * 0.25d0
        pn = ( pn + lamda ) * 0.25d0

      end do

      end

