      function alnorm ( x, upper )

c*********************************************************************72
c
cc ALNORM computes the cumulative density of the standard normal distribution.
c
c  Modified:
c
c    28 March 1999
c
c  Reference:
c
c    David Hill,
c    The Normal Integral,
c    Algorithm AS 66,
c    Applied Statistics,
c    Volume 22, Number 3, pages 424-427, 1973.
c
c  Parameters:
c
c    Input, double precision X, is one endpoint of the semi-infinite interval
c    over which the integration takes place.
c
c    Input, logical UPPER, determines whether the upper or lower
c    interval is to be integrated:
c    .TRUE.  => integrate from X to + Infinity;
c    .FALSE. => integrate from - Infinity to X.
c
c    Output, double precision ALNORM, the integral of the standard normal
c    distribution over the desired interval.
c
      double precision a1
      double precision a2
      double precision a3
      double precision b1
      double precision b2
      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision c5
      double precision c6
      double precision con
      double precision d1
      double precision d2
      double precision d3
      double precision d4
      double precision d5
      double precision ltone
      double precision p
      double precision q
      double precision r
      double precision utzero

      parameter ( a1 = 5.75885480458D+00 )
      parameter ( a2 = 2.62433121679D+00 )
      parameter ( a3 = 5.92885724438D+00 )
      parameter ( b1 = -29.8213557807D+00 )
      parameter ( b2 = 48.6959930692D+00 )
      parameter ( c1 = -0.000000038052D+00 )
      parameter ( c2 = 0.000398064794D+00 )
      parameter ( c3 = -0.151679116635D+00 )
      parameter ( c4 = 4.8385912808D+00 )
      parameter ( c5 = 0.742380924027D+00 )
      parameter ( c6 = 3.99019417011D+00 )
      parameter ( con = 1.28D+00 )
      parameter ( d1 = 1.00000615302D+00 )
      parameter ( d2 = 1.98615381364D+00 )
      parameter ( d3 = 5.29330324926D+00 )
      parameter ( d4 = -15.1508972451D+00 )
      parameter ( d5 = 30.789933034D+00 )
      parameter ( ltone = 7.0D+00 )
      parameter ( p = 0.398942280444D+00 )
      parameter ( q = 0.39990348504D+00 )
      parameter ( r = 0.398942280385D+00 )
      parameter ( utzero = 18.66D+00 )

      double precision alnorm
      logical up
      logical upper
      double precision x
      double precision y
      double precision z

      up = upper
      z = x

      if ( z .lt. 0.0D+00 ) then
        up = .not. up
        z = - z
      end if

      if ( z .gt. ltone .and. 
     &  ( ( .not. up ) .or. z .gt. utzero ) ) then

        if ( up ) then
          alnorm = 0.0D+00
        else
          alnorm = 1.0D+00
        end if

        return

      end if

      y = 0.5D+00 * z**2

      if ( z .le. con ) then

        alnorm = 0.5D+00 - z * ( p - q * y
     &    / ( y + a1 + b1 
     &    / ( y + a2 + b2 
     &    / ( y + a3 ))))

      else

        alnorm = r * dexp ( - y )
     &    / ( z + c1 + d1
     &    / ( z + c2 + d2
     &    / ( z + c3 + d3
     &    / ( z + c4 + d4
     &    / ( z + c5 + d5
     &    / ( z + c6 ))))))

      end if

      if ( .not. up ) then
        alnorm = 1.0D+00 - alnorm
      end if

      return
      end
      function alogam ( x, ifault )

c*********************************************************************72
c
cc ALOGAM computes the logarithm of the Gamma function.
c
c  Modified:
c
c    28 March 1999
c
c  Author:
c
c    Malcolm Pike,
c    David Hill.
c
c  Reference:
c
c    Malcolm Pike, David Hill,
c    Algorithm 291: 
c    Logarithm of Gamma Function,
c    Communications of the ACM,
c    Volume 9, Number 9, September 1966, page 684.
c
c  Parameters:
c
c    Input, double precision X, the argument of the Gamma function.
c    X should be greater than 0.
c
c    Output, integer IFAULT, error flag.
c    0, no error.
c    1, X <= 0.
c
c    Output, double precision ALOGAM, the logarithm of the Gamma function of X.
c
      implicit none

      double precision alogam
      double precision f
      integer ifault
      double precision x
      double precision y
      double precision z

      if ( x .le. 0.0D+00 ) then
        ifault = 1
        alogam = 0.0D+00
        return
      end if

      ifault = 0
      y = x

      if ( x .lt. 7.0D+00 ) then

        f = 1.0D+00
        z = y

10      continue

        if ( z .lt. 7.0D+00 ) then
          f = f * z
          z = z + 1.0D+00
          go to 10
        end if

        y = z
        f = - dlog ( f )

      else

        f = 0.0D+00

      end if

      z = 1.0D+00 / y / y
        
      alogam = f + ( y - 0.5D+00 ) * dlog ( y ) - y
     &  + 0.918938533204673D+00 +
     &  ((( 
     &  - 0.000595238095238D+00   * z 
     &  + 0.000793650793651D+00 ) * z
     &  - 0.002777777777778D+00 ) * z 
     &  + 0.083333333333333D+00 ) / y

      return
      end
      function digama ( x, ifault )

c*********************************************************************72
c
cc DIGAMA calculates DIGAMMA ( X ) = d ( LOG ( GAMMA ( X ) ) ) / dX
c
c  Modified:
c
c    28 March 1999
c
c  Author:
c
c    Jose Bernardo
c
c  Reference:
c
c    Jose Bernardo,
c    Algorithm AS 103:
c    Psi ( Digamma ) Function,
c    Applied Statistics,
c    Volume 25, Number 3, 1976, pages 315-317.
c
c  Parameters:
c
c    Input, double precision X, the argument of the digamma function.
c    0 < X.
c
c    Output, integer IFAULT, error flag.
c    0, no error.
c    1, X <= 0.
c
c    Output, double precision DIGAMA, the value of the digamma function at X.
c
      implicit none

      double precision c
      double precision d1
      double precision s
      double precision s3
      double precision s4
      double precision s5

      parameter ( c = 8.5D+00 )
      parameter ( d1 = -0.5772156649D+00 )
      parameter ( s = 0.00001D+00 )
      parameter ( s3 = 0.08333333333D+00 )
      parameter ( s4 = 0.0083333333333D+00 )
      parameter ( s5 = 0.003968253968D+00 )

      double precision digama
      integer ifault
      double precision r
      double precision x
      double precision y
c
c  Check the input.
c
      if ( x .le. 0.0D+00 ) then
        digama = 0.0D+00
        ifault = 1
        return
      end if
c
c  Initialize.
c
      ifault = 0
      y = x
      digama = 0.0D+00
c
c  Use approximation if argument <= S.
c
      if ( y .le. s ) then
        digama = d1 - 1.0D+00 / y
        return
      end if
c
c  Reduce to DIGAMA(X + N) where (X + N) >= C.
c
10    continue

      if ( y .lt. c ) then
        digama = digama - 1.0D+00 / y
        y = y + 1.0D+00
        go to 10
      end if
c
c  Use Stirling's (actually de Moivre's) expansion if argument > C.
c
      r = 1.0D+00 / y
      digama = digama + log ( y ) - 0.5D+00 * r
      r = r * r
      digama = digama - r * ( s3 - r * ( s4 - r * s5 ) )

      return
      end
      subroutine dirich ( k, n, x, ix, init, alpha, rlogl, v, g, niter, 
     &  s, eps, work, ifault )

c*********************************************************************72
c
cc DIRICH estimates the parameters of a Dirichlet distribution.
c
c  Auxilliary routines:
c
c    ALOGAM (CACM algorithm 291 or AS 245),
c    DIGAMA (AS 103), 
c    GAMMAD (AS 239), 
c    PPCHI2 (AS 91), 
c    TRIGAM (AS 121).
c
c  Modified:
c
c    26 March 1999
c
c  Reference:
c
c    A. Naryanan,
c    Algorithm AS 266:
c    Maximum Likelihood Estimation of the Parameters of the
c    Dirichlet Distribution,
c    Applied Statistics,
c    Volume 40, Number 2, 1991, pages 365-374.
c
c  Parameters:
c
c    Input, integer K, the number of parameters.
c    2 <= K.
c
c    Input, integer N, the number of observations.
c    K < N.
c
c    Input, double precision X(IX,K), contains the N by K array of samples
c    from the distribution.  X(I,J) is the J-th component of
c    the I-th sample.
c
c    Input, integer IX, the leading dimension of the array X.
c    N <= IX.
c
c    Input, integer INIT, specifies how the parameter estimates
c    are to be initialized:
c    1, use the method of moments;
c    2, initialize each ALPHA to the minimum of X;
c    otherwise, the input values of ALPHA already contain estimates.
c
c    Input/output, double precision ALPHA(K).
c    On input, if INIT is not 1 or 2, then ALPHA must contain
c    initial estimates for the parameters.
c    On output, with IFAULT = 0, ALPHA contains the computed
c    estimates for the parameters.
c
c    Output, double precision RLOGL, the value of the log-likelihood function
c    at the solution point.
c
c    Output, double precision V(K*(K+1)/2); V(J*(J-1)/2+I) contains the covariance 
c    between ALPHA(I) and ALPHA(J), for I = 1 to J, J = 1 to K.
c
c    Output, double precision G(K), contains an estimate of the derivative of
c    the log-likelihood with respect to each component of ALPHA.
c
c    Output, integer NITER, contains the number of Newton-Raphson
c    iterations performed.
c
c    Output, double precision S, the value of the chi-squared statistic.
c
c    Output, double precision EPS, contains the probability that the chi-squared
c    statistic is less than S.
c
c    Workspace, double precision WORK(2*K).
c
c    Output, integer IFAULT, error indicator.
c    0, no error, the results were computed successfully;
c    1, K < 2;
c    2, N <= K;
c    3, IX < N;
c    4, if X(I,J) <= 0 for any I or J, or if
c       ABS ( Sum ( 1 <= J <= K ) X(I,J) - 1 ) >= GAMMA = 0.001;
c    5, if IFAULT is returned nonzero from the chi-square 
c       routine PPCHI2;
c    6, if ALPHA(J) <= 0 for any J during any step of the iteration;
c    7, if MAXIT iterations were carried out but convergence
c       was not achieved.
c
      implicit none

      integer ix
      integer k

      double precision alogam
      double precision alpha(k)
      double precision an
      double precision beta
      double precision chi2
      double precision digama
      double precision eps
      double precision g(k)
      double precision gamma
      parameter ( gamma = 0.0001D+00 )
      double precision gammad
      double precision gg
      integer i
      integer i2
      integer if1
      integer ifault
      integer init
      integer itn
      integer j
      integer kk
      integer maxit
      parameter ( maxit = 100 )
      integer n
      integer niter
      double precision ppchi2
      double precision rk
      double precision rlogl
      double precision s
      double precision sum
      double precision sum1
      double precision temp
      double precision trigam
      double precision v(k*(k+1)/2)
      double precision varp1
      double precision work(2*k)
      double precision x(ix,k)
      double precision x11
      double precision x12

      if1 = 0
c
c  Check the input arguments.
c
      if ( k .lt. 2 ) then
        ifault = 1
        return
      end if

      if ( n .le. k ) then
        ifault = 2
        return
      end if

      if ( ix .lt. n ) then
        ifault = 3
        return
      end if

      do i = 1, n

        do j = 1, k
          if ( x(i,j) .le. 0.0D+00 ) then
            niter = i
            ifault = 4
            return
          end if
        end do

        sum = 0.0D+00
        do j = 1, k
          sum = sum + x(i,j)
        end do

        if ( abs ( sum - 1.0D+00 ) .ge. gamma ) then
          ifault = 4
          niter = i
          return
        end if

      end do

      ifault = 0

      an = dble ( n )
      rk = dble ( k )
      niter = 0
c
c  Calculate initial estimates using the method of moments.
c
      if ( init .eq. 1 ) then

        sum = 0.0D+00
        do j = 1, k - 1
          x12 = 0.0D+00
          do i = 1, n
            x12 = x12 + x(i,j)
          end do
          alpha(j) = x12 / an
          sum = sum + alpha(j)
        end do

        x12 = 0.0D+00
        do i = 1, n
          x12 = x12 + x(i,1)**2
        end do

        alpha(k) = 1.0D+00 - sum
        x12 = x12 / an
        varp1 = x12 - alpha(1)**2

        x11 = ( alpha(1) - x12 ) / varp1
        do j = 1, k
          alpha(j) = x11 * alpha(j)
        end do
c
c  Calculate initial estimates using Ronning's suggestion.
c
      else if ( init .eq. 2 ) then

        sum = x(1,1)
        do j = 1, k
          do i = 1, n
            sum = min ( sum, x(i,j) )
          end do
        end do

        do j = 1, k
          alpha(j) = sum
        end do

      end if
c
c  Check whether any ALPHA's are negative or zero.
c
      do j = 1, k
        if ( alpha(j) .le. 0.0D+00 ) then
          ifault = 6
          return
        end if
      end do
c
c  Calculate n * log(G(j)) for j = 1,2,...,k and store in WORK array.
c
      do j = 1, k
        sum = 0.0D+00
        do i = 1, n
          sum = sum + log ( x(i,j) )
        end do
        work(j) = sum
      end do
c
c  GG = log ( gamma ( k/2 ) ).
c  Note that this call to ALOGAM cannot fail.
c
      gg = alogam ( rk / 2.0D+00, ifault )
c
c  Call Algorithm AS 91 to compute CHI2, the chi-squared value.
c
      chi2 = ppchi2 ( gamma, rk, gg, ifault )

      if ( ifault .gt. 0 ) then
        ifault = 5
        return
      end if
c
c  Carry out the Newton iteration.
c
      do itn = 1, maxit

        sum = 0.0D+00
        do j = 1, k
          sum = sum + alpha(j)
        end do
c
c  Note that TRIGAM and DIGAMA cannot fail if the first argument
c  is positive.
c
        sum1 = 0.0D+00
        do j = 1, k
          work(k+j) = trigam ( alpha(j), ifault )
          sum1 = sum1 + 1.0D+00 / work(k+j)
        end do

        beta = trigam ( sum, ifault )
        beta = an * beta / ( 1.0D+00 - beta * sum1 )

        temp = digama ( sum, ifault )

        do j = 1, k
          g(j) = an * ( temp - digama ( alpha(j), ifault ) ) + work(j)
        end do
c
c  Calculate the lower triangle of the Variance-Covariance matrix V.
c
        sum = beta / an**2
        do i = 1, k
          do j = 1, i
            kk = i * ( i - 1 ) / 2 + j
            v(kk) = sum / ( work(k+i) * work(k+j) )
            if ( i .eq. j ) then
              v(kk) = v(kk) + 1.0D+00 / ( an * work(k+j) )
            end if
          end do
        end do
c
c  Postmultiply the Variance-Covariance matrix V by G and store
c  in the last k elements of WORK.
c
        do i = 1, k

          sum = 0.0D+00
          i2 = i * ( i - 1 ) / 2
          do j = 1, i - 1
            sum = sum + v(i2+j) * g(j)
          end do
          do j = i + 1, k
            sum = sum + v(j*(j-1)/2+i) * g(j)
          end do

          work(k+i) = sum + v(i*(i+1)/2) * g(i)

        end do
c
c  Update the ALPHA's.
c
        niter = itn

        do j = 1, k
          alpha(j) = alpha(j) + work(k+j)
        end do

        do j = 1, k
          if ( alpha(j) .le. 0.0D+00 ) then
            ifault = 6
            return
          end if
        end do
c
c  Test for convergence.
c
        s = 0.0D+00
        do j = 1, k
          s = s + g(j) * work(k+j)
        end do

        if ( s .lt. chi2 ) then
          go to 230
        end if

      end do

      ifault = 7
c
c  Note that GAMMAD cannot fail.
c
  230 continue

      eps = gammad ( s / 2.0D+00, rk / 2.0D+00, if1 )

      sum = 0.0D+00
      do j = 1, k
        sum = sum + alpha(j)
      end do

      rlogl = 0.0D+00
      do j = 1, k
        rlogl = rlogl + ( alpha(j) - 1.0D+00 ) * work(j) -
     &    an * alogam ( alpha(j), if1 )
      end do

      rlogl = rlogl + an * alogam ( sum, if1 )

      return
      end
      subroutine dirichlet_check ( n, a )

c*********************************************************************72
c
cc DIRICHLET_CHECK checks the parameters of the Dirichlet PDF.
c
c  Modified:
c
c    12 January 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components.
c
c    Input, double precision A(N), the probabilities for each component.
c    Each A(I) should be nonnegative, and at least one should be positive.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      logical positive

      positive = .false.

      do i = 1, n

        if ( a(i) < 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIRICHLET_CHECK - Fatal error!'
          write ( *, '(a)' ) '  A(I) < 0.'
          write ( *, '(a,i8)' ) '  For I = ', i
          write ( *, '(a,g14.6)' ) '  A(I) = ', a(i)
          stop
        else if ( 0.0D+00 < a(i) ) then
          positive = .true.
        end if

      end do

      if ( .not. positive ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_CHECK - Fatal error!'
        write ( *, '(a)' ) '  All parameters are zero!'
        stop
      end if

      return
      end
      subroutine dirichlet_mean ( n, a, mean )

c*********************************************************************72
c
cc DIRICHLET_MEAN returns the means of the Dirichlet PDF.
c
c  Modified:
c
c    12 January 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components.
c
c    Input, double precision A(N), the probabilities for each component.
c    Each A(I) should be nonnegative, and at least one should be positive.
c
c    Output, real MEAN(N), the means of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_sum
      integer i
      double precision mean(n)

      call dirichlet_check ( n, a )

      a_sum = 0.0D+00
      do i = 1, n
        a_sum = a_sum + a(i)
      end do

      do i = 1, n
        mean(i) = a(i) / a_sum
      end do

      return
      end
      subroutine dirichlet_variance ( n, a, variance )

c*********************************************************************72
c
cc DIRICHLET_VARIANCE returns the variances of the Dirichlet PDF.
c
c  Modified:
c
c    12 January 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components.
c
c    Input, double precision A(N), the probabilities for each component.
c    Each A(I) should be nonnegative, and at least one should be positive.
c
c    Output, double precision VARIANCE(N), the variances of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_sum
      integer i
      double precision variance(n)

      call dirichlet_check ( n, a )

      a_sum = 0.0D+00
      do i = 1, n
        a_sum = a_sum + a(i)
      end do

      do i = 1, n
        variance(i) = a(i) * ( a_sum - a(i) ) 
     &  / ( a_sum * a_sum * ( a_sum + 1.0D+00 ) )
      end do

      return
      end
      function gammad ( x, p, ifault )

c*********************************************************************72
c
cc GAMMAD computes the Incomplete Gamma Integral
c
c  Auxiliary functions:
c
c    ALOGAM = logarithm of the gamma function, 
c    ALNORM = algorithm AS66
c
c  Modified:
c
c    31 March 1999
c
c  Reference:
c
c    B Shea,
c    Chi-squared and Incomplete Gamma Integral,
c    Algorithm AS 239,
c    Applied Statistics,
c    Volume 37, Number 3, pages 466-473, 1988.
c
c  Parameters:
c
c    Input, double precision X, P, the parameters of the incomplete gamma ratio.
c    0 <= X, and 0 < P.
c
c    Output, integer IFAULT, error flag.
c    0, no error.
c    1, X < 0 or P <= 0.
c
c    Output, double precision GAMMAD, the value of the incomplete Gamma integral.
c
      implicit none

      double precision elimit
      double precision oflo
      double precision plimit
      double precision tol
      double precision xbig

      parameter ( elimit = - 88.0D+00 )
      parameter ( oflo = 1.0D+37 )
      parameter ( plimit = 1000.0D+00 )
      parameter ( tol = 1.0D-14 )
      parameter ( xbig = 1.0D+08 )

      double precision a
      double precision alnorm
      double precision alogam
      double precision an
      double precision arg
      double precision b
      double precision c
      double precision gammad
      integer ifault
      double precision p
      double precision pn1
      double precision pn2
      double precision pn3
      double precision pn4
      double precision pn5
      double precision pn6
      double precision rn
      logical upper
      double precision x

      gammad = 0.0D+00
c
c  Check the input.
c
      if ( x .lt. 0.0D+00 ) then
        ifault = 1
        return
      end if

      if ( p .le. 0.0D+00 ) then
        ifault = 1
        return
      end if

      ifault = 0

      if ( x .eq. 0.0D+00 ) then
        gammad = 0.0D+00
        return
      end if
c
c  If P is large, use a normal approximation.
c
      if ( p .gt. plimit ) then

        pn1 = 3.0D+00 * dsqrt ( p ) * ( ( x / p )**( 1.0D+00 / 3.0D+00 ) 
     &  + 1.0D+00 / ( 9.0D+00 * p ) - 1.0D+00 )

        upper = .false.
        gammad = alnorm ( pn1, upper )
        return

      end if
c
c  If X is large set GAMMAD = 1.
c
      if ( x .gt. xbig ) then
        gammad = 1.0D+00
        return
      end if
c
c  Use Pearson's series expansion.
c  (Note that P is not large enough to force overflow in ALOGAM).
c  No need to test IFAULT on exit since P > 0.
c
      if ( x .le. 1.0D+00 .or. x .lt. p ) then

        arg = p * dlog ( x ) - x - alogam ( p + 1.0D+00, ifault )
        c = 1.0D+00
        gammad = 1.0D+00
        a = p

   40   continue

        a = a + 1.0D+00
        c = c * x / a
        gammad = gammad + c

        if ( c .gt. tol ) then
          go to 40
        end if

        arg = arg + dlog ( gammad )

        if ( arg .ge. elimit ) then
          gammad = dexp ( arg )
        else
          gammad = 0.0D+00
        end if
c
c  Use a continued fraction expansion.
c
      else 

        arg = p * dlog ( x ) - x - alogam ( p, ifault )
        a = 1.0D+00 - p
        b = a + x + 1.0D+00
        c = 0.0D+00
        pn1 = 1.0D+00
        pn2 = x
        pn3 = x + 1.0D+00
        pn4 = x * b
        gammad = pn3 / pn4

   60   continue

        a = a + 1.0D+00
        b = b + 2.0D+00
        c = c + 1.0D+00
        an = a * c
        pn5 = b * pn3 - an * pn1
        pn6 = b * pn4 - an * pn2

        if ( pn6 .ne. 0.0D+00 ) then

          rn = pn5 / pn6

          if ( abs ( gammad - rn ) .le. min ( tol, tol * rn ) ) then
            go to 80
          end if

          gammad = rn

        end if

        pn1 = pn3
        pn2 = pn4
        pn3 = pn5
        pn4 = pn6
c
c  Re-scale terms in continued fraction if terms are large.
c
        if ( abs ( pn5 ) .ge. oflo ) then
          pn1 = pn1 / oflo
          pn2 = pn2 / oflo
          pn3 = pn3 / oflo
          pn4 = pn4 / oflo
        end if

        go to 60

   80   continue

        arg = arg + dlog ( gammad )

        if ( arg .ge. elimit ) then
          gammad = 1.0D+00 - dexp ( arg )
        else
          gammad = 1.0D+00
        end if

      end if

      return
      end
      function ppchi2 ( p, v, ifault )

c*********************************************************************72
c
cc PPCHI2 evaluates the percentage points of the chi-squared PDF.
c
c  Auxiliary routines:
c
c     PPND = AS 111 or AS 241;
c     GAMMAD = AS 239.
c
c  Modified:
c
c    30 March 1999
c
c  Reference:
c
c    Best, Roberts,
c    The Percentage Points of the Chi-Squared Distribution,
c    Algorithm AS 91,  
c    Applied Statistics,
c    Volume 24, Number ?, pages 385-390, 1975.
c
c  Parameters:
c
c    Input, double precision P, a value of the chi-squared cumulative probability
c    density function.
c    0.000002 <= P <= 0.999998.
c
c    Input, double precision V, the parameter of the chi-squared probability density
c    function.  V > 0.
c
c    Output, integer IFAULT, error flag.
c    0, no error detected.
c    1, P < PMIN or P > PMAX.
c    2, V <= 0.0.
c    3, an error occurred in the incomplete Gamma function routine.
c    4, the maximum number of iterations were taken without convergence.
c    5, an error occurred in the log Gamma routine.
c
c    Output, double precision PPCHI2, the value of the chi-squared random deviate
c    with the property that the probability that a chi-squared random
c    deviate with parameter V is less than or equal to PPCHI2 is P.
c
      implicit none

      double precision aa
      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision c5
      double precision c6
      double precision c7
      double precision c8
      double precision c9
      double precision c10
      double precision c11
      double precision c12
      double precision c13
      double precision c14
      double precision c15
      double precision c16
      double precision c17
      double precision c18
      double precision c19
      double precision c20
      double precision c21
      double precision c22
      double precision c23
      double precision c24
      double precision c25
      double precision c26
      double precision c27
      double precision c28
      double precision c29
      double precision c30
      double precision c31
      double precision c32
      double precision c33
      double precision c34
      double precision c35
      double precision c36
      double precision c37
      double precision c38
      double precision e
      integer maxit
      double precision pmax
      double precision pmin

      parameter ( aa = 0.6931471806D+00 )
      parameter ( c1 = 0.01D+00 )
      parameter ( c2 = 0.222222D+00 )
      parameter ( c3 = 0.32D+00 )
      parameter ( c4 = 0.4D+00 )
      parameter ( c5 = 1.24D+00 )
      parameter ( c6 = 2.2D+00 )
      parameter ( c7 = 4.67D+00 )
      parameter ( c8 = 6.66D+00 )
      parameter ( c9 = 6.73D+00 )
      parameter ( c10 = 13.32D+00 )
      parameter ( c11 = 60.0D+00 )
      parameter ( c12 = 70.0D+00 )
      parameter ( c13 = 84.0D+00 )
      parameter ( c14 = 105.0D+00 )
      parameter ( c15 = 120.0D+00 )
      parameter ( c16 = 127.0D+00 )
      parameter ( c17 = 140.0D+00 )
      parameter ( c18 = 175.0D+00 )
      parameter ( c19 = 210.0D+00 )
      parameter ( c20 = 252.0D+00 )
      parameter ( c21 = 264.0D+00 )
      parameter ( c22 = 294.0D+00 )
      parameter ( c23 = 346.0D+00 )
      parameter ( c24 = 420.0D+00 )
      parameter ( c25 = 462.0D+00 )
      parameter ( c26 = 606.0D+00 )
      parameter ( c27 = 672.0D+00 )
      parameter ( c28 = 707.0D+00 )
      parameter ( c29 = 735.0D+00 )
      parameter ( c30 = 889.0D+00 )
      parameter ( c31 = 932.0D+00 )
      parameter ( c32 = 966.0D+00 )
      parameter ( c33 = 1141.0D+00 )
      parameter ( c34 = 1182.0D+00 )
      parameter ( c35 = 1278.0D+00 )
      parameter ( c36 = 1740.0D+00 )
      parameter ( c37 = 2520.0D+00 )
      parameter ( c38 = 5040.0D+00 )
      parameter ( e = 0.0000005D+00 )
      parameter ( maxit = 20 )
      parameter ( pmax = 0.999998D+00 )
      parameter ( pmin = 0.000002D+00 )

      double precision a
      double precision alogam
      double precision b
      double precision c
      double precision ch
      double precision g
      double precision gammad
      integer i
      integer ifault
      integer ifault2
      double precision p
      double precision p1
      double precision p2
      double precision ppchi2
      double precision ppnd
      double precision q
      double precision s1
      double precision s2
      double precision s3
      double precision s4
      double precision s5
      double precision s6
      double precision t
      double precision v
      double precision x
      double precision xx

      ifault2 = 0
c
c  Check the input.
c
      if ( p .lt. pmin .or. p .gt. pmax ) then
        ifault = 1
        ppchi2 = - 1.0D+00
        return
      end if

      if ( v .le. 0.0D+00 ) then
        ifault = 2
        ppchi2 = - 1.0D+00
        return
      end if

      ifault = 0
      xx = 0.5D+00 * v
      c = xx - 1.0D+00
c
c  Compute Log ( Gamma ( V/2 ) ).
c
      g = alogam ( v / 2.0D+00, ifault )

      if ( ifault .ne. 0 ) then
        ifault = 5
        return
      end if
c
c  Starting approximation for small chi-squared.
c
      if ( v .lt. - c5 * dlog ( p ) ) then

        ch = ( p * xx * dexp ( g + xx * aa ) )**( 1.0D+00 / xx )

        if ( ch .lt. e ) then
          ifault = 0
          ppchi2 = ch
          return
        end if
c
c  Starting approximation for V less than or equal to 0.32.
c
      else if ( v .le. c3 ) then

        ch = c4
        a = log ( 1.0D+00 - p )

10      continue

        q = ch
        p1 = 1.0D+00 + ch * ( c7 + ch )
        p2 = ch * ( c9 + ch * ( c8 + ch ) )

        t = - 0.5D+00 + ( c7 + 2.0 * ch ) / p1 - ( c9 + ch * ( c10 +
     &    3.0D+00 * ch ) ) / p2

        ch = ch - ( 1.0D+00 - exp ( a + g + 0.5D+00 * ch + c * aa ) *
     &    p2 / p1 ) / t

        if ( dabs ( q / ch - 1.0D+00 ) .gt. c1 ) then
          go to 10
        end if
c
c  Call to algorithm AS 111.
c  Note that P has been tested above.
c  AS 241 could be used as an alternative.
c
      else

        x = ppnd ( p, ifault2 )
c
c  Starting approximation using Wilson and Hilferty estimate.
c
        p1 = c2 / v
        ch = v * ( x * dsqrt ( p1 ) + 1.0D+00 - p1 )**3
c
c  Starting approximation for P tending to 1.
c
        if ( ch .gt. c6 * v + 6.0 ) then
          ch = - 2.0D+00 * ( log ( 1.0 - p ) 
     &    - c * dlog ( 0.5D+00 * ch ) + g )
        end if

      end if
c
c  Call to algorithm AS 239 and calculation of seven term Taylor series.
c
      do i = 1, maxit

        q = ch
        p1 = 0.5D+00 * ch
        p2 = p - gammad ( p1, xx, ifault2 )

        if ( ifault2 .ne. 0 ) then
          ppchi2 = - 1.0D+00
          ifault = 3
          return
        end if

        t = p2 * dexp ( xx * aa + g + p1 - c * dlog ( ch ) )
        b = t / ch
        a = 0.5D+00 * t - b * c

        s1 = 
     &      ( c19 + a 
     &    * ( c17 + a 
     &    * ( c14 + a 
     &    * ( c13 + a 
     &    * ( c12 + a 
     &    *   c11 ) ) ) ) ) / c24

        s2 = 
     &      ( c24 + a 
     &    * ( c29 + a 
     &    * ( c32 + a 
     &    * ( c33 + a
     &    *   c35 ) ) ) ) / c37

        s3 = 
     &      ( c19 + a 
     &    * ( c25 + a 
     &    * ( c28 + a * 
     &        c31 ) ) ) / c37

        s4 = 
     &      ( c20 + a 
     &    * ( c27 + a 
     &    *   c34 ) + c 
     &    * ( c22 + a 
     &    * ( c30 + a 
     &    *   c36 ) ) ) / c38

        s5 = ( c13 + c21 * a + c * ( c18 + c26 * a ) ) / c37

        s6 = ( c15 + c * ( c23 + c16 * c ) ) / c38

        ch = ch + t * ( 1.0D+00 + 0.5D+00 * t * s1 - b * c 
     &    * ( s1 - b 
     &    * ( s2 - b 
     &    * ( s3 - b 
     &    * ( s4 - b 
     &    * ( s5 - b 
     &    *   s6 ) ) ) ) ) )

        if ( dabs ( q / ch - 1.0D+00 ) .gt. e ) then
          ifault = 0
          ppchi2 = ch
          return
        end if

      end do

      ifault = 4
      ppchi2 = ch

      return
      end
      function ppnd ( p, ifault )

!*********************************************************************72
!
!! PPND produces the normal deviate value corresponding to lower tail area = P.
!
!  Modified:
!
!    28 March 1999
!
!  Reference:
!
!    J Beasley, S Springer,
!    Algorithm AS 111:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 26, Number 1, 1977, pages 118-121.
!
!  Parameters:
!
!    Input, double precision P, the value of the cumulative probability densitity function.
!    0 < P < 1.
!
!    Output, integer IFAULT, error flag.
!    0, no error.
!    1, P <= 0 or P >= 1.  PPND is returned as 0.
!
!    Output, double precision PPND, the normal deviate value with the property that
!    the probability of a standard normal deviate being less than or
!    equal to PPND is P.
!
      implicit none

      double precision a0
      double precision a1
      double precision a2
      double precision a3
      double precision b1
      double precision b2
      double precision b3
      double precision b4
      double precision c0
      double precision c1
      double precision c2
      double precision c3
      double precision d1
      double precision d2
      double precision split

      parameter ( a0 = 2.50662823884D+00 )
      parameter ( a1 = -18.61500062529D+00 )
      parameter ( a2 = 41.39119773534D+00 )
      parameter ( a3 = -25.44106049637D+00 )
      parameter ( b1 = -8.47351093090D+00 )
      parameter ( b2 = 23.08336743743D+00 )
      parameter ( b3 = -21.06224101826D+00 )
      parameter ( b4 = 3.13082909833D+00 )
      parameter ( c0 = -2.78718931138D+00 )
      parameter ( c1 = -2.29796479134D+00 )
      parameter ( c2 = 4.85014127135D+00 )
      parameter ( c3 = 2.32121276858D+00 )
      parameter ( d1 = 3.54388924762D+00 )
      parameter ( d2 = 1.63706781897D+00 )
      parameter ( split = 0.42D+00 )

      integer ifault
      double precision p
      double precision ppnd
      double precision r

      ifault = 0
!
!  0.08 < P < 0.92
!
      if ( dabs ( p - 0.5D+00 ) .le. split ) then

        r = ( p - 0.5D+00 )**2

        ppnd = ( p - 0.5D+00 ) * ( ( ( 
     &      a3   * r 
     &    + a2 ) * r 
     &    + a1 ) * r 
     &    + a0 ) / ( ( ( (
     &      b4   * r 
     &    + b3 ) * r 
     &    + b2 ) * r
     &    + b1 ) * r 
     &    + 1.0D+00 )
!
!  P < 0.08 or P > 0.92, 
!  R = min ( P, 1-P )
!
      else if ( p .gt. 0.0D+00 .and. p .lt. 1.0D+00 ) then

        if ( p .gt. 0.5D+00 ) then
          r = dsqrt ( - dlog ( 1.0D+00 - p ) )
        else
          r = dsqrt ( - dlog ( p ) )
        end if

        ppnd = ( ( (
     &      c3   * r 
     &    + c2 ) * r 
     &    + c1 ) * r 
     &    + c0 ) / ( ( 
     &      d2   * r 
     &    + d1 ) * r 
     &    + 1.0D+00 )

        if ( p .lt. 0.5D+00 ) then
          ppnd = - ppnd
        end if
!
!  P <= 0.0 or P >= 1.0
!
      else

        ifault = 1
        ppnd = 0.0D+00

      end if

      return
      end
      subroutine r8col_mean ( m, n, a, mean )

c*********************************************************************72
c
cc R8COL_MEAN returns the column means of an R8COL.
c
c  Discussion:
c
c    An R8COL is an M by N array of R8 values, regarded
c    as an array of N columns of length M.
c
c  Example:
c
c    A =
c      1  2  3
c      2  6  7
c
c    MEAN =
c      1.5  4.0  5.0
c
c  Modified:
c
c    13 January 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), the array to be examined.
c
c    Output, double precision MEAN(N), the means, or averages, of the columns.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision a_sum
      integer i
      integer j
      double precision mean(n)

      do j = 1, n

        a_sum = 0.0D+00
        do i = 1, m
          a_sum = a_sum + a(i,j)
        end do

        mean(j) = a_sum / dble ( m )

      end do

      return
      end
      subroutine r8col_variance ( m, n, a, variance )

c*********************************************************************72
c
cc R8COL_VARIANCE returns the variances of an R8COL.
c
c  Discussion:
c
c    An R8COL is an M by N array of R8 values, regarded
c    as an array of N columns of length M.
c
c  Modified:
c
c    13 January 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in
c    the array.
c
c    Input, double precision A(M,N), the array whose variances are desired.
c
c    Output, double precision VARIANCE(N), the variances of the rows.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision a_sum
      integer i
      integer j
      double precision mean
      double precision variance(n)

      do j = 1, n

        a_sum = 0.0D+00
        do i = 1, m
          a_sum = a_sum + a(i,j)
        end do
        mean = a_sum / dble ( m )

        variance(j) = 0.0D+00
        do i = 1, m
          variance(j) = variance(j) + ( a(i,j) - mean )**2
        end do

        if ( 1 < m ) then
          variance(j) = variance(j) / dble ( m - 1 )
        else
          variance(j) = 0.0D+00
        end if

      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
      function trigam ( x, ifault )

c*********************************************************************72
c
cc TRIGAM calculates trigamma(x) = d**2 log(gamma(x)) / dx**2
c
c  Modified:
c
c    28 March 1999
c
c  Author:
c
c    BE Schneider
c
c  Reference:
c
c    BE Schneider,
c    Algorithm AS 121:
c    Trigamma Function,
c    Applied Statistics, 
c    Volume 27, Number 1, pages 97-99, 1978.
c
c  Parameters:
c
c    Input, double precision X, the argument of the trigamma function.
c    0 < X.
c
c    Output, integer IFAULT, error flag.
c    0, no error.
c    1, X <= 0.
c
c    Output, double precision TRIGAM, the value of the trigamma function at X.
c
      implicit none

      double precision a
      double precision b
      double precision b2
      double precision b4
      double precision b6
      double precision b8

      parameter ( a = 0.0001D+00 )
      parameter ( b = 5.0D+00 )
      parameter ( b2 =  0.1666666667D+00 )
      parameter ( b4 = -0.03333333333D+00 )
      parameter ( b6 =  0.02380952381D+00 )
      parameter ( b8 = -0.03333333333D+00 )

      integer ifault
      double precision trigam
      double precision x
      double precision y
      double precision z
c
c  Check the input.
c
      if ( x .le. 0.0D+00 ) then
        ifault = 1
        trigam = 0.0D+00
        return
      end if

      ifault = 0
      z = x
c
c  Use small value approximation if X <= A.
c
      if ( x .le. a ) then
        trigam = 1.0D+00 / x**2
        return
      end if
c
c  Increase argument to ( X + I ) >= B.
c
      trigam = 0.0D+00

10    continue

      if ( z .lt. b ) then
        trigam = trigam + 1.0D+00 / z**2
        z = z + 1.0D+00
        go to 10
      end if
c
c  Apply asymptotic formula if argument .ge. B.
c
      y = 1.0D+00 / z**2

      trigam = trigam + 0.5D+00 * 
     &    y + ( 1.0D+00
     &  + y * ( b2 
     &  + y * ( b4 
     &  + y * ( b6 
     &  + y *   b8 )))) / z

      return
      end
