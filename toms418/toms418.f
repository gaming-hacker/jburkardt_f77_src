      subroutine fspl2 ( f, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &  lc, ls, c, s )

c*********************************************************************72
c
cc FSPL2 estimates a Fourier integral using Filon's method.
c
c  Discussion:
c
c    This routine computes the Fourier integrals
c      C = integral ( a <= x <= b ) f(x) cos(wx) dx
c      S = integral ( a <= x <= b ) f(x) sin(wx) dx
c    with a spline procedure.  Repeated Richardson extrapolation is used.
c
c  Modified:
c
c    20 May 2014
c
c  Author:
c
c    Original Fortran77 version by Bo Einarsson.
c    This Fortran77 version by John Burkardt.
c
c  References:
c
c    Stephen Chase, Lloyd Fosdick,
c    An Algorithm for Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 453-457.
c
c    Stephen Chase, Lloyd Fosdick,
c    Algorithm 353:
c    Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 457-458.
c
c    Bo Einarsson,
c    Numerical calculation of Fourier integrals with cubic splines,
c    Communications of the ACM,
c    Volume 8, pages 457-458, 1969.
c
c  Parameters:
c
C    F(X) = THE FUNCTION TO BE INTEGRATED, SUPPLIED BY THE USER
C    AND DECLARED 'EXTERNAL' IN THE CALLING PROGRAM.
c
c    Input, double precision A, B, the lower and upper quadrature limits.
c    If B .le. A, the computation is not carried out, and the signs
c    of LC, LS and EPS are reversed to indicate an error.
c
C    FPA AND FPB ARE THE VALUES OF THE DERIVATIVE OF F(X).
C    FBA AND FBB ARE THE CORRESPONDING VALUES OF THE SECOND
C    DERIVATIVE AT THE POINTS A AND B.
c
c    Input, double precision W, the angular frequency.
c
C    EPS = REQUIRED ACCURACY, DEFINED BY
C         |ERROR| < =  EPS*(1.+|C|)
C    AND
C         |ERROR| < =  EPS*(1.+|S|)
C    IF CONVERGENCE IS NOT OBTAINED, THE VALUE
C    OF EPS IS RETURNED WITH NEGATIVE SIGN.
C
C    MAXH = THE MAXIMUM NUMBER OF PARTITIONS (INTERVAL HALVINGS).
C    IN THIS ROUTINE THE INTERNAL VARIABLE MXN DEFINED BELOW
C    IS USED INSTEAD OF MAXH.
c
C    LC POSITIVE ON ENTRY INDICATES THAT C IS WANTED.
C    LS POSITIVE ON ENTRY INDICATES THAT S IS WANTED.
c
C    ON EXIT LC AND LS GIVE THE NUMBER OF PARTITIONS USED
C    FOR THE COMPUTATION OF C AND S.
C
c    Output, double precision C, the value of the cosine integral,
c    if requested.
c
c    Output, double precision S, the value of the sine integral,
c    if requested.
c
      implicit none

      double precision a
      double precision alfa
      double precision alpha
      double precision b
      double precision beta
      double precision c
      double precision cosa
      double precision cosb
      double precision delta
      double precision eps
      double precision epsil
      double precision f
      external f
      double precision fa
      double precision fb
      double precision fba
      double precision fbb
      double precision fpa
      double precision fpb
      double precision fx
      double precision h
      double precision h2
      integer i
      integer lc
      integer llc
      integer lls
      integer ls
      integer m
      integer maxh
      integer mxn
      integer n
      integer nst
      integer nstop
      double precision pi
      double precision pvtc(7)
      double precision pvts(7)
      double precision s
      double precision sina
      double precision sinb
      double precision sumcos
      double precision sumsin
      double precision t2
      double precision t3
      double precision tc
      double precision temp
      double precision temp1
      double precision temp2
      double precision temp3
      double precision teta
      double precision tmax
      double precision tmaxb
      double precision ts
      double precision w
      double precision w1
      double precision wx
      double precision x

      save pi

      data pi / 3.141592653589793D+00 /

      if ( eps .lt. 0.0D+00 ) then
        lc = -lc
        ls = -ls
        return
      end if

      if ( b .le. a ) then
        eps = -eps
        lc = -lc
        ls = -ls
        return
      end if

      n = 1
      w1 = abs ( w )
      temp = 2.0D+00 * ( b - a ) * w1 / pi

      if ( 2.0D+00 .lt. temp ) then
        n = int ( log ( temp ) / 0.693D+00 )
      end if
c
c  0.693 = alog(2.) rounded downwards.
c
      mxn = max ( maxh, n + 1 )
      fa = f ( a )
      fb = f ( b )
      cosa = cos ( w1 * a )
      sina = sin ( w1 * a )
      cosb = cos ( w1 * b )
      sinb = sin ( w1 * b )
      h = ( b - a ) / dble ( 2 ** n )
      nstop = 2 ** n - 1
      nst = 1
c
c  TMAX is the switch-over point for TETA.
c  Analysis shows that with a 56 bit floating point mantissa,
c    tmax = 0.2D+00
c  is suitable, while with a 24 bit mantissa,
c    tmax = 1.0E+00
c  is preferred.
c
      tmax = 0.2D+00
c
c  tmaxb is the switch-over point in beta, where the
c  cancellation is strongest.
c
      tmaxb = 5.0D+00 * tmax
c
c  Save information from input LS and LC, because they will
c  also be used for output.
c
      if ( 0 < ls ) then
        lls = 1
      else
        lls = 2
      end if

      if ( 0 < lc ) then
        llc = 1
      else
        llc = 2
      end if

      sumcos = 0.5D+00 * ( fa * cosa + fb * cosb )
      sumsin = 0.5D+00 * ( fa * sina + fb * sinb )
c
c  All of the above is executed only once per call.
c
c  Now the iteration begins.
c  The constant 'M' is used in the Richardson extrapolation.
c  M-1 is the number of times the original step length 'H'
c  has been divided by two.
c
      m = 1

10    continue

      h2 = h * h
      teta = w1 * h

      do i = 1, nstop, nst

        x = a + h * dble ( i )
        wx = w1 * x
        fx = f ( x )

        if ( lls .eq. 1 ) then
          sumsin = sumsin + fx * sin ( wx )
        end if

        if ( llc .eq. 1 ) then
          sumcos = sumcos + fx * cos ( wx )
        end if

      end do

      t2 = teta * teta
      temp = 1.0D+00 - sin ( 0.5D+00 * teta ) ** 2 / 1.5D+00
c
c  Power series for small TETA.
c
      if ( teta .le. tmax ) then

        alfa = teta * (1.0D+00
     &    - t2 * ( 2.0D+00 / 15.0D+00
     &    - t2 * ( 19.0D+00 / 1680.0D+00
     &    - t2 * ( 13.0D+00 / 25200.0D+00
     &    - t2 * ( 293.0D+00 / 19958400.0D+00
     &    - t2 * 181.0D+00 / 619164000.0D+00 ))))) / 12.0D+00

        delta = -1.0D+00 / 12.0D+00
     &    + t2 * ( 1.0D+00 / 90.0D+00
     &    - t2 * ( 5.0D+00 / 12096.0D+00
     &    - t2 * ( 1.0D+00 / 129600.0D+00
     &    - t2 / 11404800.0D+00 )))

        epsil = 1.0D+00
     &    - t2 * ( 1.0D+00 / 6.0D+00
     &    - t2 * ( 0.0125D+00
     &    - t2 * ( 17.0D+00 / 30240.0D+00
     &    - t2 * ( 31.0D+00 / 1814400.0D+00
     &    - t2 / 2661120.0D+00 ))))

        t3 = t2

        beta = teta * h2 * ( 1.0D+00
     &    - t2 / 21.0D+00 * ( 1.0D+00
     &    - t2 * ( 1.0D+00 / 48.0D+00
     &    - t2 * ( 1.0D+00 / 3960.0D+00
     &    - t3 / 494208.0D+00 )))) / 180.0D+00
c
c  Closed form for larger TETA.
c
      else

        temp1 = ( 0.5D+00 * teta ) ** 2
        temp2 = sin ( 0.5D+00 * teta ) ** 2 / temp1
        temp3 = sin ( teta ) / teta
        alfa = ( temp - temp2 * temp3 ) / teta
        delta = ( temp - temp2 ) / t2
        epsil = temp2 * temp2

        if ( teta .le. tmaxb ) then

          t3 = t2 * ( 1.0D+00
     &      - t2 * ( 1.0D+00 / 175.0D+00
     &      - t2 * ( 1.0D+00 / 40800.0D+00
     &      - t2 / 12209400.0D+00 )))

          beta = teta * h2 * ( 1.0D+00
     &      - t2 / 21.0D+00 * ( 1.0D+00
     &      - t2 * ( 1.0D+00 / 48.0D+00
     &      - t2 * ( 1.0D+00 / 3960.0D+00
     &      - t3 / 494208.0D+00 )))) / 180.0D+00

        else

          beta = ( temp - temp3 ) / ( teta * w1 * w1 )

        end if

      end if
c
c  Integration formulas.
c
      if ( lls .eq. 1 ) then

        ts = h * ( 
     &      ( beta * fbb - alfa * fb ) * cosb
     &    + ( alfa * fa - beta * fba ) * cosa
     &    + delta * h * ( fpb * sinb - fpa * sina )
     &    + epsil * sumsin ) / temp

        call endt2 ( pvts, ts, eps, s, lls, m )

        ls = n

      end if

      if ( llc .eq. 1 ) then
        tc = h * (
     &      ( alfa * fb - beta * fbb ) * sinb
     &    + ( beta * fba - alfa * fa ) * sina
     &    + delta * h * ( fpb * cosb - fpa * cosa )
     &    + epsil * sumcos ) / temp

        call endt2 ( pvtc, tc, eps, c, llc, m )

        lc = n

      end if
c
c  Now test to see if done.
c
      if ( llc + lls .le. 3 ) then
 
        n = n + 1
c
c  This is the beginning of the iteration.
c
        if ( n .le. mxn ) then
          h = 0.5D+00 * h
          nst = 2
          nstop = 2 ** n
          m = m + 1
          go to 10
        end if

        eps = -eps

      end if

      if ( 0 .lt. ls .and. w .lt. 0.0D+00 ) then
        s = -s
      end if

      return
      end
      subroutine endt2 ( prevot, quant, eps, value, l, m )

c*********************************************************************72
c
cc ENDT2 performs Richardson extrapolation.
c
c  Discussion:
c
c    ENDT2 performs Richardson extra-
c    polation of the values 'quant' which are introduced into
c    the routine each time it is called, each time with
c    increasing value of 'm', starting with m = 1.  
c
c    the current values are stored in the array 'prevot', where 'prevot(1)'
c    at exit is equal to 'quant'.  the best value for the moment
c    is given in 'value'.  endt2 requires the present value to
c    agree with the previous value to within eps2, where 
c    eps2 = eps*(1.0 + abs(present value)).
c
c    eps is supplied by the user.
c
c    the error expansion is of the form
c    error = c4*h**4 + c6*h**6 + c8*h**8 + ... + cn*h**n + ...
c
      implicit none

      double precision diff
      double precision eps
      integer k
      integer l
      integer m
      double precision prevot(7)
      double precision quant
      double precision reps
      double precision rich(7)
      double precision temp1
      double precision temp2
      double precision value

      save rich

      data rich /    
     &  0.0D+00, 15.0D+00, 63.0D+00, 255.0D+00,
     &  1023.0D+00, 4095.0D+00, 16383.0D+00 /
c
c  rich(1) is not used.
c  rich(k) = 2**(2*k) - 1, k = 2,3,4,5,6,7.
c
      temp2 = prevot(1)
      prevot(1) = quant
      temp1 = quant

      if ( m .eq. 1 ) then
        value = temp1
        return
      end if

      reps = eps * ( 1.0D+00 + abs ( quant ) )

      do k = 2, m

        diff = temp1 - temp2

        if ( abs ( diff ) .le. reps ) then
          l = 2
          value = temp1
          return
        end if

        if ( k .eq. 8 ) then
          value = temp1
          return
        end if

        temp1 = temp1 + diff / rich(k)
        temp2 = prevot(k)
        prevot(k) = temp1

      end do

      value = temp1

      return
      end
