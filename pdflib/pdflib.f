      function i4_binomial_pdf ( n, p, k )

c*********************************************************************72
c
cc I4_BINOMIAL_PDF evaluates the binomial PDF.
c
c  Discussion:
c
c    pdf(n,p,k) = C(n,k) p^k (1-p)^(n-k)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2018
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of binomial trials.
c    0 .lt. N.
c
c    Input, double precision P, the probability of a success in one trial.
c
c    Input, integer K, the number of successes.
c
c    Output, double precision I4_BINOMIAL_PDF, the probability of K successes
c    in N trials with a per-trial success probability of P.
c
      implicit none

      double precision i4_binomial_pdf
      integer k
      integer n
      double precision p
      double precision r8_choose
      double precision value

      if ( k .lt. 0 ) then
        value = 0.0D+00
      else if ( k .le. n ) then
        value = r8_choose ( n, k ) 
     &    * p ** k * ( 1.0D+00 - p ) ** ( n - k )
      else
        value = 0.0D+00
      end if

      i4_binomial_pdf = value

      return
      end
      function i4_binomial_sample ( n, pp )

c*********************************************************************72
c
cc I4_BINOMIAL_SAMPLE generates a binomial random deviate.
c
c  Discussion:
c
c    This procedure generates a single random deviate from a binomial
c    distribution whose number of trials is N and whose
c    probability of an event in each trial is P.
c
c    The previous version of this program relied on the assumption that
c    local memory would be preserved between calls.  It set up data
c    one time to be preserved for use over multiple calls.  In the
c    interests of portability, this assumption has been removed, and
c    the "setup" data is recomputed on every call.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    This version by John Burkardt.
c
c  Reference:
c
c    Voratas Kachitvichyanukul, Bruce Schmeiser,
c    Binomial Random Variate Generation,
c    Communications of the ACM,
c    Volume 31, Number 2, February 1988, pages 216-222.
c
c  Parameters:
c
c    Input, integer N, the number of binomial trials, from which a
c    random deviate will be generated.
c    0 .lt. N.
c
c    Input, double precision PP, the probability of an event in each trial of
c    the binomial distribution from which a random deviate is to be generated.
c    0.0 .lt. PP .lt. 1.0.
c
c    Output, integer I4_BINOMIAL_SAMPLE, a random deviate from the
c    distribution.
c
      implicit none

      double precision al
      double precision alv
      double precision amaxp
      double precision c
      double precision f
      double precision f1
      double precision f2
      double precision ffm
      double precision fm
      double precision g
      integer i
      integer i4_binomial_sample
      integer ix
      integer ix1
      integer k
      integer m
      integer mp
      double precision pp
      integer n
      double precision p
      double precision p1
      double precision p2
      double precision p3
      double precision p4
      double precision q
      double precision qn
      double precision r
      double precision r8_uniform_01_sample
      double precision t
      double precision u
      double precision v
      double precision w
      double precision w2
      double precision x
      double precision x1
      double precision x2
      double precision xl
      double precision xll
      double precision xlr
      double precision xm
      double precision xnp
      double precision xnpq
      double precision xr
      double precision ynorm
      double precision z
      double precision z2

      if ( pp .le. 0.0D+00 .or. 1.0D+00 .le. pp ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_BINOMIAL_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  PP is out of range.'
        stop
      end if

      p = min ( pp, 1.0D+00 - pp )
      q = 1.0D+00 - p
      xnp = dble ( n ) * p

      if ( xnp .lt. 30.0D+00 ) then

        qn = q ** n
        r = p / q
        g = r * dble ( n + 1 )

10      continue

          ix = 0
          f = qn
          u = r8_uniform_01_sample ( )

20        continue

            if ( u .lt. f ) then
              if ( 0.5D+00 .lt. pp ) then
                ix = n - ix
              end if
              i4_binomial_sample = ix
              return
            end if

            if ( 110 .lt. ix ) then
              go to 30
            end if

            u = u - f
            ix = ix + 1
            f = f * ( g / dble ( ix ) - r )

          go to 20

30      continue

        go to 10

      end if

      ffm = xnp + p
      m = ffm
      fm = m
      xnpq = xnp * q
      p1 = int ( 2.195D+00 * sqrt ( xnpq ) - 4.6D+00 * q ) + 0.5D+00
      xm = fm + 0.5D+00
      xl = xm - p1
      xr = xm + p1
      c = 0.134D+00 + 20.5D+00 / ( 15.3D+00 + fm )
      al = ( ffm - xl ) / ( ffm - xl * p )
      xll = al * ( 1.0D+00 + 0.5D+00 * al )
      al = ( xr - ffm ) / ( xr * q )
      xlr = al * ( 1.0D+00 + 0.5D+00 * al )
      p2 = p1 * ( 1.0D+00 + c + c )
      p3 = p2 + c / xll
      p4 = p3 + c / xlr
c
c  Generate a variate.
c
40    continue

        u = r8_uniform_01_sample ( ) * p4
        v = r8_uniform_01_sample ( )
c
c  Triangle
c
        if ( u .lt. p1 ) then
          ix = xm - p1 * v + u
          if ( 0.5D+00 .lt. pp ) then
            ix = n - ix
          end if
          i4_binomial_sample = ix
          return
        end if
c
c  Parallelogram
c
        if ( u .le. p2 ) then

          x = xl + ( u - p1 ) / c
          v = v * c + 1.0D+00 - abs ( xm - x ) / p1

          if ( v .le. 0.0D+00 .or. 1.0D+00 .lt. v ) then
            go to 40
          end if

          ix = x

        else if ( u .le. p3 ) then

          ix = xl + log ( v ) / xll
          if ( ix .lt. 0 ) then
            go to 40
          end if
          v = v * ( u - p2 ) * xll

        else

          ix = xr - log ( v ) / xlr
          if ( n .lt. ix ) then
            go to 40
          end if
          v = v * ( u - p3 ) * xlr

        end if

        k = abs ( ix - m )

        if ( k .le. 20 .or. xnpq / 2.0D+00 - 1.0D+00 .le. k ) then

          f = 1.0D+00
          r = p / q
          g = dble ( n + 1 ) * r

          if ( m .lt. ix ) then
            mp = m + 1
            do i = m + 1, ix
              f = f * ( g / i - r )
            end do
          else if ( ix .lt. m ) then
            ix1 = ix + 1
            do i = ix + 1, m
              f = f / ( g / dble ( i ) - r )
            end do
          end if

          if ( v .le. f ) then
            if ( 0.5D+00 .lt. pp ) then
              ix = n - ix
            end if
            i4_binomial_sample = ix
            return
          end if

        else

          amaxp = ( k / xnpq ) * ( ( k * ( k / 3.0D+00 
     &      + 0.625D+00 ) + 0.1666666666666D+00 ) / xnpq + 0.5D+00 )
          ynorm = - dble ( k * k ) / ( 2.0D+00 * xnpq )
          alv = log ( v )

          if ( alv .lt. ynorm - amaxp ) then
            if ( 0.5D+00 .lt. pp ) then
              ix = n - ix
            end if
            i4_binomial_sample = ix
            return
          end if

          if ( ynorm + amaxp .lt. alv ) then
            go to 40
          end if

          x1 = dble ( ix + 1 )
          f1 = fm + 1.0D+00
          z = dble ( n + 1 ) - fm
          w = dble ( n - ix + 1 )
          z2 = z * z
          x2 = x1 * x1
          f2 = f1 * f1
          w2 = w * w

          t = xm * log ( f1 / x1 ) + ( n - m + 0.5D+00 ) * log ( z / w )
     &      + dble ( ix - m ) * log ( w * p / ( x1 * q )) 
     &      + ( 13860.0D+00 - ( 462.0D+00 
     &      - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 / f2 ) / f2 ) / f2 ) 
     &      / f2 ) / f1 / 166320.0D+00 + ( 13860.0D+00 - ( 462.0D+00 - 
     &      ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 / z2 ) / z2 ) / z2 ) 
     &      / z2 ) / z / 166320.0D+00 + ( 13860.0D+00 - ( 462.0D+00 
     &      - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 / x2 ) / x2 ) / x2 ) 
     &      / x2 ) / x1 / 166320.0D+00 + ( 13860.0D+00 - ( 462.0D+00 
     &      - ( 132.0D+00 - ( 99.0D+00 - 140.0D+00 
     &      / w2 ) / w2 ) / w2 ) / w2 ) / w / 166320.0D+00

          if ( alv .le. t ) then
            if ( 0.5D+00 .lt. pp ) then
              ix = n - ix
            end if
            i4_binomial_sample = ix
            return
          end if

        end if

      go to 40

      return
      end
      function i4vec_multinomial_pdf ( n, p, m, x )

c*********************************************************************72
c
cc I4VEC_MULTINOMIAL_PDF evaluates the multinomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer N, the number of trials.
c
c    Input, double precision P(M), the probability of each outcome
c    on any single trial.
c
c    Input, integer M, the number of possible outcomes
c    of a single trial.
c
c    Input, integer X(M), the results of N trials,
c    with X(I) the number of times outcome I occurred.
c
c    Output, double precision I4VEC_MULTINOMIAL_PDF, the probability
c    density function evaluated at X.
c
      implicit none

      integer m
      integer n

      integer bot
      integer c
      integer i
      double precision i4vec_multinomial_pdf
      integer j
      double precision p(m)
      double precision pdf
      integer top
      integer x(m)
c
c  The combinatorial coefficient is an integer.
c
      c = 1
      top = n
      do i = 1, m
        bot = 1
        do j = 1, x(i)
          c = ( c * top ) / bot
          top = top - 1
          bot = bot + 1
        end do
      end do

      pdf = dble ( c )
      do i = 1, m
        pdf = pdf * p(i) ** x(i)
      end do

      i4vec_multinomial_pdf = pdf

      return
      end
      subroutine i4vec_multinomial_sample ( n, p, ncat, ix )

c*********************************************************************72
c
cc I4VEC_MULTINOMIAL_SAMPLE generates a multinomial random deviate.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    This version by John Burkardt.
c
c  Reference:
c
c    Luc Devroye,
c    Non-Uniform Random Variate Generation,
c    Springer, 1986,
c    ISBN: 0387963057,
c    LC: QA274.D48.
c
c  Parameters:
c
c    Input, integer N, the number of trials.
c
c    Input, double precision P(NCAT).  P(I) is the probability that an event
c    will be classified into category I.  Thus, each P(I) must be between
c    0.0 and 1.0, and the P's must sum to 1.
c
c    Input, integer NCAT, the number of possible outcomes
c    of a single trial.
c
c    Output, integer IX(NCAT), a random observation from
c    the multinomial distribution.  All IX(i) will be nonnegative and their
c    sum will be N.
c
      implicit none

      integer n
      integer ncat

      integer i
      integer i4_binomial_sample
      integer icat
      integer ix(ncat)
      integer ntot
      double precision p(ncat)
      double precision prob
      double precision ptot

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0'
        stop
      end if

      if ( ncat .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  NCAT .le. 1'
        stop
      end if

      do i = 1, ncat

        if ( p(i) .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
          write ( *, '(a)' ) '  Some P(i) .lt. 0.'
          stop
        end if

        if ( 1.0D+00 .lt. p(i) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
          write ( *, '(a)' ) '  Some 1 .lt. P(i).'
          stop
        end if

      end do
c
c  Initialize variables.
c
      ntot = n
      ptot = 1.0D+00
      do i = 1, ncat
        ix(i) = 0
      end do
c
c  Generate the observation.
c
      do icat = 1, ncat - 1
        prob = p(icat) / ptot
        ix(icat) = i4_binomial_sample ( ntot, prob )
        ntot = ntot - ix(icat)
        if ( ntot .le. 0 ) then
          return
        end if
        ptot = ptot - p(icat)
      end do

      ix(ncat) = ntot

      return
      end
      function r8_beta_pdf ( alpha, beta, rval )

c*********************************************************************72
c
cc R8_BETA_PDF evaluates the PDF of a beta distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2015
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision ALPHA, BETA, shape parameters.
c    0.0 .lt. ALPHA, BETA.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_BETA_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision alpha
      double precision beta
      double precision r8_beta_pdf
      double precision r8_gamma_log
      double precision rval
      double precision temp

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_BETA_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
        stop
      end if

      if ( beta .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_BETA_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter BETA is not positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 .or. 1.0D+00 .le. rval ) then

        r8_beta_pdf = 0.0D+00

      else

        temp = r8_gamma_log ( alpha + beta ) - r8_gamma_log ( alpha )
     &      - r8_gamma_log ( beta )

        r8_beta_pdf = exp ( temp ) * rval ** ( alpha - 1.0D+00 )
     &      * ( 1.0D+00 - rval ) ** ( beta - 1.0D+00 )

      end if

      return
      end
      function r8_beta_sample ( aa, bb )

c*********************************************************************72
c
cc R8_BETA_SAMPLE generates a beta random deviate.
c
c  Discussion:
c
c    This procedure returns a single random deviate from the beta distribution
c    with parameters A and B.  The density is
c
c      x^(a-1) * (1-x)^(b-1) / Beta(a,b) for 0 .lt. x .lt. 1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    This version by John Burkardt.
c
c  Reference:
c
c    Russell Cheng,
c    Generating Beta Variates with Nonintegral Shape Parameters,
c    Communications of the ACM,
c    Volume 21, Number 4, April 1978, pages 317-322.
c
c  Parameters:
c
c    Input, double precision AA, the first parameter of the beta distribution.
c    0.0 .lt. AA.
c
c    Input, double precision BB, the second parameter of the beta distribution.
c    0.0 .lt. BB.
c
c    Output, double precision R8_BETA_SAMPLE, a beta random variate.
c
      implicit none

      double precision a
      double precision aa
      double precision alpha
      double precision b
      double precision bb
      double precision beta
      double precision delta
      double precision gamma
      double precision k1
      double precision k2
      double precision log4
      parameter ( log4 = 1.3862943611198906188D+00 )
      double precision log5
      parameter ( log5 = 1.6094379124341003746D+00 )
      double precision r
      double precision r8_beta_sample
      double precision r8_uniform_01_sample
      double precision s
      double precision t
      double precision u1
      double precision u2
      double precision v
      double precision w
      double precision y
      double precision z

      if ( aa .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_BETA_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  AA .le. 0.0'
        stop
      end if

      if ( bb .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_BETA_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  BB .le. 0.0'
        stop
      end if
c
c  Algorithm BB
c
      if ( 1.0D+00 .lt. aa .and. 1.0D+00 .lt. bb ) then

        a = min ( aa, bb )
        b = max ( aa, bb )
        alpha = a + b
        beta = 
     &    sqrt ( ( alpha - 2.0D+00 ) / ( 2.0D+00 * a * b - alpha ) )
        gamma = a + 1.0D+00 / beta

10      continue

          u1 = r8_uniform_01_sample ( )
          u2 = r8_uniform_01_sample ( )
          v = beta * log ( u1 / ( 1.0D+00 - u1 ) )
          w = a * exp ( v )

          z = u1 ** 2 * u2
          r = gamma * v - log4
          s = a + r - w

          if ( 5.0D+00 * z .le. s + 1.0D+00 + log5 ) then
            go to 20
          end if

          t = log ( z )
          if ( t .le. s ) then
            go to 20
          end if

          if ( t .le. ( r + alpha * log ( alpha / ( b + w ) ) ) ) then
            go to 20
          end if

        go to 10

20      continue
c
c  Algorithm BC
c
      else

        a = max ( aa, bb )
        b = min ( aa, bb )
        alpha = a + b
        beta = 1.0D+00 / b
        delta = 1.0D+00 + a - b
        k1 = delta * ( 1.0D+00 / 72.0D+00 + b / 24.0D+00 )
     &    / ( a / b - 7.0D+00 / 9.0D+00 )
        k2 = 0.25D+00 + ( 0.5D+00 + 0.25D+00 / delta ) * b

30      continue

          u1 = r8_uniform_01_sample ( )
          u2 = r8_uniform_01_sample ( )

          if ( u1 .lt. 0.5D+00 ) then

            y = u1 * u2
            z = u1 * y

            if ( k1 .le. 0.25D+00 * u2 + z - y ) then
              go to 30
            end if

          else

            z = u1 ** 2 * u2

            if ( z .le. 0.25D+00 ) then

              v = beta * log ( u1 / ( 1.0D+00 - u1 ) )
              w = a * exp ( v )

              if ( aa .eq. a ) then
                r8_beta_sample = w / ( b + w )
              else
                r8_beta_sample = b / ( b + w )
              end if

              return

            end if

            if ( k2 .lt. z ) then
              go to 30
            end if

          end if

          v = beta * log ( u1 / ( 1.0D+00 - u1 ) )
          w = a * exp ( v )

          if ( log ( z ) .le. alpha * ( log ( alpha / ( b + w ) ) + v ) 
     &      - log4 ) then
            go to 40
          end if

        go to 30

40      continue

      end if

      if ( aa .eq. a ) then
        r8_beta_sample = w / ( b + w )
      else
        r8_beta_sample = b / ( b + w )
      end if

      return
      end
      function r8_chi_pdf ( df, rval )

c*********************************************************************72
c
cc R8_CHI_PDF evaluates the PDF of a chi-squared distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision DF, the degrees of freedom.
c    0.0 .lt. DF.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_CHI_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision df
      double precision r8_chi_pdf
      double precision r8_gamma_log
      double precision rval
      double precision temp1
      double precision temp2

      if ( df .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_CHI_PDF - Fatal error!'
        write ( *, '(a)' ) '  Degrees of freedom must be positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 ) then

        r8_chi_pdf = 0.0D+00

      else

        temp2 = df * 0.5D+00

        temp1 = ( temp2 - 1.0D+00 ) * log ( rval ) - 0.5D+00 * rval
     &      - temp2 * log ( 2.0D+00 ) - r8_gamma_log ( temp2 )

        r8_chi_pdf = exp ( temp1 )

      end if

      return
      end
      function r8_chi_sample ( df )

c*********************************************************************72
c
cc R8_CHI_SAMPLE generates a Chi-Square random deviate.
c
c  Discussion:
c
c    This procedure generates a random deviate from the chi square distribution
c    with DF degrees of freedom random variable.
c
c    The algorithm exploits the relation between chisquare and gamma.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    This version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision DF, the degrees of freedom.
c    0.0 .lt. DF.
c
c    Output, double precision R8_CHI_SAMPLE, a random deviate
c    from the distribution.
c
      implicit none

      double precision arg1
      double precision arg2
      double precision df
      double precision r8_chi_sample
      double precision r8_gamma_sample

      if ( df .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_CHI_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  DF .le. 0.'
        write ( *, '(a,g14.6)' ) '  Value of DF: ', df
        stop
      end if

      arg1 = 1.0D+00
      arg2 = df / 2.0D+00

      r8_chi_sample = 2.0D+00 * r8_gamma_sample ( arg1, arg2 )

      return
      end
      function r8_choose ( n, k )

c*********************************************************************72
c
cc R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in R8 arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, double precision R8_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer k
      integer mn
      integer mx
      integer n
      double precision r8_choose
      double precision value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0.0D+00

      else if ( mn .eq. 0 ) then

        value = 1.0D+00

      else

        mx = max ( k, n - k )
        value = dble ( mx + 1 )

        do i = 2, mn
          value = ( value * dble ( mx + i ) ) / dble ( i )
        end do

      end if

      r8_choose = value

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
      function r8_exponential_pdf ( beta, rval )

c*********************************************************************72
c
cc R8_EXPONENTIAL_PDF evaluates the PDF of an exponential distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision BETA, the scale value.
c    0.0 .lt. BETA.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_EXPONENTIAL_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision beta
      double precision r8_exponential_pdf
      double precision rval

      if ( beta .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_EXPONENTIAL_PDF - Fatal error!'
        write ( *, '(a)' ) '  BETA parameter must be positive.'
        stop
      end if

      if ( rval .lt. 0.0D+00 ) then
        r8_exponential_pdf = 0.0D+00
      else
        r8_exponential_pdf = exp ( - rval / beta ) / beta
      end if

      return
      end
      function r8_exponential_sample ( lambda )

c*********************************************************************72
c
cc R8_EXPONENTIAL_SAMPLE samples the exponential PDF.
c
c  Discussion:
c
c    Note that the parameter LAMBDA is a multiplier.  In some formulations,
c    it is used as a divisor instead.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision LAMBDA, the parameter of the PDF.
c
c    Output, double precision R8_EXPONENTIAL_SAMPLE, a sample of the PDF.
c
      implicit none

      double precision lambda
      double precision r
      double precision r8_exponential_sample
      double precision r8_uniform_01_sample

      r = r8_uniform_01_sample ( )

      r8_exponential_sample = - log ( r ) * lambda

      return
      end
      function r8_exponential_01_pdf ( rval )

c*********************************************************************72
c
cc R8_EXPONENTIAL_01_PDF: PDF of a standard exponential distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_EXPONENTIAL_01_PDF, the value of the PDF.
c
      implicit none

      double precision r8_exponential_01_pdf
      double precision rval

      if ( rval .lt. 0.0D+00 ) then
        r8_exponential_01_pdf = 0.0D+00
      else
        r8_exponential_01_pdf = exp ( - rval )
      end if

      return
      end
      function r8_exponential_01_sample ( )

c*********************************************************************72
c
cc R8_EXPONENTIAL_01_SAMPLE samples the standard exponential PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EXPONENTIAL_01_SAMPLE, a sample of the PDF.
c
      implicit none

      double precision r
      double precision r8_exponential_01_sample
      double precision r8_uniform_01_sample

      r = r8_uniform_01_sample ( )

      r8_exponential_01_sample = - log ( r )

      return
      end
      function r8_gamma_log ( x )

c*********************************************************************72
c
cc R8_GAMMA_LOG evaluates the logarithm of the gamma function.
c
c  Discussion:
c
c    This routine calculates the LOG(GAMMA) function for a positive real
c    argument X.  Computation is based on an algorithm outlined in
c    references 1 and 2.  The program uses rational functions that
c    theoretically approximate LOG(GAMMA) to at least 18 significant
c    decimal digits.  The approximation for X > 12 is from reference
c    3, while approximations for X < 12.0 are similar to those in
c    reference 1, but are unpublished.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody, Kenneth Hillstrom,
c    Chebyshev Approximations for the Natural Logarithm of the
c    Gamma Function,
c    Mathematics of Computation,
c    Volume 21, Number 98, April 1967, pages 198-203.
c
c    Kenneth Hillstrom,
c    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
c    May 1969.
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
c    Output, double precision R8_GAMMA_LOG, the value of the function.
c
      implicit none

      double precision c(7)
      double precision corr
      double precision d1 
      parameter ( d1 = -5.772156649015328605195174D-01 )
      double precision d2
      parameter ( d2 = 4.227843350984671393993777D-01 )
      double precision d4
      parameter ( d4 = 1.791759469228055000094023D+00 )
      double precision frtbig
      parameter ( frtbig = 2.25D+76 )
      integer i
      double precision p1(8)
      double precision p2(8)
      double precision p4(8)
      double precision q1(8)
      double precision q2(8)
      double precision q4(8)
      double precision r8_epsilon
      double precision r8_gamma_log
      double precision res
      double precision sqrtpi 
      parameter ( sqrtpi = 0.9189385332046727417803297D+00 )
      double precision x
      double precision xbig
      parameter ( xbig = 2.55D+305 )
      double precision xden
      double precision xinf
      parameter ( xinf = 1.79D+308 )
      double precision xm1
      double precision xm2
      double precision xm4
      double precision xnum
      double precision y
      double precision ysq

      save c
      save p1
      save p2
      save p4
      save q1
      save q2
      save q4

      data c /
     &  -1.910444077728D-03, 
     &   8.4171387781295D-04, 
     &  -5.952379913043012D-04, 
     &   7.93650793500350248D-04, 
     &  -2.777777777777681622553D-03, 
     &   8.333333333333333331554247D-02, 
     &   5.7083835261D-03 /
      data p1 /
     &  4.945235359296727046734888D+00, 
     &  2.018112620856775083915565D+02, 
     &  2.290838373831346393026739D+03, 
     &  1.131967205903380828685045D+04, 
     &  2.855724635671635335736389D+04, 
     &  3.848496228443793359990269D+04, 
     &  2.637748787624195437963534D+04, 
     &  7.225813979700288197698961D+03 /
      data p2 /
     &  4.974607845568932035012064D+00, 
     &  5.424138599891070494101986D+02, 
     &  1.550693864978364947665077D+04, 
     &  1.847932904445632425417223D+05, 
     &  1.088204769468828767498470D+06, 
     &  3.338152967987029735917223D+06, 
     &  5.106661678927352456275255D+06, 
     &  3.074109054850539556250927D+06 /
      data p4 /
     &  1.474502166059939948905062D+04, 
     &  2.426813369486704502836312D+06, 
     &  1.214755574045093227939592D+08, 
     &  2.663432449630976949898078D+09, 
     &  2.940378956634553899906876D+10, 
     &  1.702665737765398868392998D+11, 
     &  4.926125793377430887588120D+11, 
     &  5.606251856223951465078242D+11 /
      data q1 /
     &  6.748212550303777196073036D+01, 
     &  1.113332393857199323513008D+03, 
     &  7.738757056935398733233834D+03, 
     &  2.763987074403340708898585D+04, 
     &  5.499310206226157329794414D+04, 
     &  6.161122180066002127833352D+04, 
     &  3.635127591501940507276287D+04, 
     &  8.785536302431013170870835D+03 /
      data q2 /
     &  1.830328399370592604055942D+02, 
     &  7.765049321445005871323047D+03, 
     &  1.331903827966074194402448D+05, 
     &  1.136705821321969608938755D+06, 
     &  5.267964117437946917577538D+06, 
     &  1.346701454311101692290052D+07, 
     &  1.782736530353274213975932D+07, 
     &  9.533095591844353613395747D+06 /
      data q4 /
     &  2.690530175870899333379843D+03, 
     &  6.393885654300092398984238D+05, 
     &  4.135599930241388052042842D+07, 
     &  1.120872109616147941376570D+09, 
     &  1.488613728678813811542398D+10, 
     &  1.016803586272438228077304D+11, 
     &  3.417476345507377132798597D+11, 
     &  4.463158187419713286462081D+11 /

      y = x

      if ( 0.0D+00 .lt. y .and. y .le. xbig ) then

        if ( y .le. r8_epsilon ( ) ) then

          res = - log ( y )
c
c  EPS < X <= 1.5.
c
        else if ( y .le. 1.5D+00 ) then

          if ( y .lt. 0.6796875D+00 ) then
            corr = -log ( y )
            xm1 = y
          else
            corr = 0.0D+00
            xm1 = ( y - 0.5D+00 ) - 0.5D+00
          end if

          if ( y .le. 0.5D+00 .or. 0.6796875D+00 .le. y ) then

            xden = 1.0D+00
            xnum = 0.0D+00
            do i = 1, 8
              xnum = xnum * xm1 + p1(i)
              xden = xden * xm1 + q1(i)
            end do

            res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

          else

            xm2 = ( y - 0.5D+00 ) - 0.5D+00
            xden = 1.0D+00
            xnum = 0.0D+00
            do i = 1, 8
              xnum = xnum * xm2 + p2(i)
              xden = xden * xm2 + q2(i)
            end do

            res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

          end if
c
c  1.5 < X <= 4.0.
c
        else if ( y .le. 4.0D+00 ) then

          xm2 = y - 2.0D+00
          xden = 1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm2 + p2(i)
            xden = xden * xm2 + q2(i)
          end do

          res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
c
c  4.0 < X <= 12.0.
c
        else if ( y .le. 12.0D+00 ) then

          xm4 = y - 4.0D+00
          xden = -1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm4 + p4(i)
            xden = xden * xm4 + q4(i)
          end do

          res = d4 + xm4 * ( xnum / xden )
c
c  Evaluate for 12 <= argument.
c
        else

          res = 0.0D+00

          if ( y .le. frtbig ) then

            res = c(7)
            ysq = y * y

            do i = 1, 6
              res = res / ysq + c(i)
            end do

          end if

          res = res / y
          corr = log ( y )
          res = res + sqrtpi - 0.5D+00 * corr
          res = res + y * ( corr - 1.0D+00 )

        end if
c
c  Return for bad arguments.
c
      else

        res = xinf

      end if
c
c  Final adjustments and return.
c
      r8_gamma_log = res

      return
      end
      function r8_gamma_pdf ( beta, alpha, rval )

c*********************************************************************72
c
cc R8_GAMMA_PDF evaluates the PDF of a gamma distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision BETA, the rate parameter.
c    0.0 .lt. BETA.
c
c    Input, double precision ALPHA, the shape parameter.
c    0.0 .lt. ALPHA.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_GAMMA_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision alpha
      double precision beta
      double precision r8_gamma_log
      double precision r8_gamma_pdf
      double precision rval
      double precision temp

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_GAMMA_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
        stop
      end if

      if ( beta .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_GAMMA_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter BETA is not positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 ) then

        r8_gamma_pdf = 0.0D+00

      else

        temp = alpha * log ( beta ) + ( alpha - 1.0D+00 ) * log ( rval )
     &    - beta * rval - r8_gamma_log ( alpha )

        r8_gamma_pdf = exp ( temp )

      end if

      return
      end
      function r8_gamma_sample ( a, r )

c*********************************************************************72
c
cc R8_GAMMA_SAMPLE generates a Gamma random deviate.
c
c  Discussion:
c
c    This procedure generates random deviates from the gamma distribution whose
c    density is (A^R)/Gamma(R) * X^(R-1) * Exp(-A*X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joachim Ahrens, Ulrich Dieter,
c    Generating Gamma Variates by a Modified Rejection Technique,
c    Communications of the ACM,
c    Volume 25, Number 1, January 1982, pages 47-54.
c
c    Joachim Ahrens, Ulrich Dieter,
c    Computer Methods for Sampling from Gamma, Beta, Poisson and
c    Binomial Distributions,
c    Computing,
c    Volume 12, Number 3, September 1974, pages 223-246.
c
c  Parameters:
c
c    Input, double precision A, the rate parameter.
c
c    Input, double precision R, the shape parameter.
c
c    Output, double precision R8_GAMMA_SAMPLE, a random deviate
c    from the distribution.
c
      implicit none

      double precision a
      double precision r
      double precision r8_gamma_sample
      double precision r8_gamma_01_sample

      r8_gamma_sample = r8_gamma_01_sample ( r ) / a

      return
      end
      function r8_gamma_01_pdf ( alpha, rval )

c*********************************************************************72
c
cc R8_GAMMA_01_PDF evaluates the PDF of a standard gamma distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision ALPHA, the shape parameter.
c    0.0 .lt. ALPHA.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_GAMMA_01_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision alpha
      double precision r8_gamma_log
      double precision r8_gamma_01_pdf
      double precision rval
      double precision temp

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_GAMMA_01_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 ) then

        r8_gamma_01_pdf = 0.0D+00

      else

        temp = ( alpha - 1.0D+00 ) * log ( rval ) - rval - r8_gamma_log 
     &( alpha )

        r8_gamma_01_pdf = exp ( temp )

      end if

      return
      end
      function r8_gamma_01_sample ( a )

c*********************************************************************72
c
cc R8_GAMMA_01_SAMPLE samples the standard Gamma distribution.
c
c  Discussion:
c
c    This procedure corresponds to algorithm GD in the reference.
c
c    pdf ( a; x ) = 1/gamma(a) * x^(a-1) * exp ( - x )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joachim Ahrens, Ulrich Dieter,
c    Generating Gamma Variates by a Modified Rejection Technique,
c    Communications of the ACM,
c    Volume 25, Number 1, January 1982, pages 47-54.
c
c  Parameters:
c
c    Input, double precision A, the shape parameter of the standard gamma
c    distribution. 0 .lt. A.
c
c    Output, double precision R8_GAMMA_01_SAMPLE, a random deviate
c    from the distribution.
c
      implicit none

      double precision a
      double precision a1
      parameter ( a1 =  0.3333333D+00 )
      double precision a2
      parameter ( a2 = -0.2500030D+00 )
      double precision a3
      parameter ( a3 =  0.2000062D+00 )
      double precision a4
      parameter ( a4 = -0.1662921D+00 )
      double precision a5
      parameter ( a5 =  0.1423657D+00 )
      double precision a6
      parameter ( a6 = -0.1367177D+00 )
      double precision a7
      parameter ( a7 =  0.1233795D+00 )
      double precision b
      double precision c
      double precision d
      double precision e
      double precision, parameter :: e1 = 1.0D+00
      double precision, parameter :: e2 = 0.4999897D+00
      double precision, parameter :: e3 = 0.1668290D+00
      double precision, parameter :: e4 = 0.0407753D+00
      double precision, parameter :: e5 = 0.0102930D+00
      double precision p
      double precision q
      double precision q0
      double precision, parameter :: q1 =  0.04166669D+00
      double precision, parameter :: q2 =  0.02083148D+00
      double precision, parameter :: q3 =  0.00801191D+00
      double precision, parameter :: q4 =  0.00144121D+00
      double precision, parameter :: q5 = -0.00007388D+00
      double precision, parameter :: q6 =  0.00024511D+00
      double precision, parameter :: q7 =  0.00024240D+00
      double precision r
      double precision r8_exponential_01_sample
      double precision r8_gamma_01_sample
      double precision r8_normal_01_sample
      double precision r8_uniform_01_sample
      double precision s
      double precision s2
      double precision si
      double precision, parameter :: sqrt32 = 5.656854D+00
      double precision t
      double precision u
      double precision v
      double precision w
      double precision x

      if ( 1.0D+00 .le. a ) then

        s2 = a - 0.5D+00
        s = sqrt ( s2 )
        d = sqrt32 - 12.0D+00 * s
c
c  Immediate acceptance.
c
        t = r8_normal_01_sample ( )
        x = s + 0.5D+00 * t
        r8_gamma_01_sample = x * x

        if ( 0.0D+00 .le. t ) then
          return
        end if
c
c  Squeeze acceptance.
c
        u = r8_uniform_01_sample ( )
        if ( d * u .le. t * t * t ) then
          return
        end if

        r = 1.0D+00 / a
        q0 = (((((( q7 
     &    * r + q6 ) 
     &    * r + q5 ) 
     &    * r + q4 ) 
     &    * r + q3 ) 
     &    * r + q2 ) 
     &    * r + q1 ) 
     &    * r
c
c  Approximation depending on size of parameter A.
c
        if ( 13.022D+00 .lt. a ) then
          b = 1.77D+00
          si = 0.75D+00
          c = 0.1515D+00 / s
        else if ( 3.686D+00 .lt. a ) then
          b = 1.654D+00 + 0.0076D+00 * s2
          si = 1.68D+00 / s + 0.275D+00
          c = 0.062D+00 / s + 0.024D+00
        else
          b = 0.463D+00 + s + 0.178D+00 * s2
          si = 1.235D+00
          c = 0.195D+00 / s - 0.079D+00 + 0.16D+00 * s
        end if
c
c  Quotient test.
c
        if ( 0.0D+00 .lt. x ) then

          v = 0.5D+00 * t / s

          if ( 0.25D+00 .lt. abs ( v ) ) then
            q = q0 - s * t + 0.25D+00 * t * t 
     &        + 2.0D+00 * s2 * log ( 1.0D+00 + v )
          else
            q = q0 + 0.5D+00 * t * t * (((((( a7 
     &        * v + a6 ) 
     &        * v + a5 ) 
     &        * v + a4 ) 
     &        * v + a3 ) 
     &        * v + a2 ) 
     &        * v + a1 ) 
     &        * v
          end if

          if ( log ( 1.0D+00 - u ) .le. q ) then
            return
          end if

        end if

10      continue

          e = r8_exponential_01_sample ( )
          u = 2.0D+00 * r8_uniform_01_sample ( ) - 1.0D+00

          if ( 0.0D+00 .le. u ) then
            t = b + abs ( si * e )
          else
            t = b - abs ( si * e )
          end if
c
c  Possible rejection.
c
          if ( t .lt. -0.7187449D+00 ) then
            go to 10
          end if
c
c  Calculate V and quotient Q.
c
          v = 0.5D+00 * t / s

          if ( 0.25D+00 .lt. abs ( v ) ) then
            q = q0 - s * t + 0.25D+00 * t * t 
     &        + 2.0D+00 * s2 * log ( 1.0D+00 + v )
          else
            q = q0 + 0.5D+00 * t * t * (((((( a7 
     &        * v + a6 ) 
     &        * v + a5 ) 
     &        * v + a4 ) 
     &        * v + a3 ) 
     &        * v + a2 ) 
     &        * v + a1 ) 
     &        * v
          end if
c
c  Hat acceptance.
c
          if ( q .le. 0.0D+00 ) then
            go to 10
          end if

          if ( 0.5D+00 .lt. q ) then
            w = exp ( q ) - 1.0D+00
          else
            w = (((( e5 * q + e4 ) * q + e3 ) * q + e2 ) * q + e1 ) * q
          end if
c
c  May have to sample again.
c
          if ( c * abs ( u ) .le. w * exp ( e - 0.5D+00 * t * t ) ) then
            go to 20
          end if

        go to 10

20      continue

        x = s + 0.5D+00 * t
        r8_gamma_01_sample = x * x

        return
c
c  Method for A .lt. 1.
c
      else

        b = 1.0D+00 + 0.3678794D+00 * a

30      continue

          p = b * r8_uniform_01_sample ( )

          if ( p .lt. 1.0D+00 ) then

            r8_gamma_01_sample = exp ( log ( p ) / a )

            if ( r8_gamma_01_sample .le. 
     &        r8_exponential_01_sample ( ) ) then
              return
            end if

            go to 30

          end if

          r8_gamma_01_sample = - log ( ( b - p ) / a )

          if ( ( 1.0D+00 - a ) * log ( r8_gamma_01_sample ) .le. 
     &      r8_exponential_01_sample ( ) ) then
            go to 40
          end if

        go to 30

40      continue

      end if

      return
      end
      function r8_invchi_pdf ( df, rval )

c*********************************************************************72
c
cc R8_INVCHI_PDF evaluates the PDF of an inverse chi-squared distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision DF, the degrees of freedom.
c    0.0 .lt. DF.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_INVCHI_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision df
      double precision r8_gamma_log
      double precision r8_invchi_pdf
      double precision rval
      double precision temp1
      double precision temp2

      if ( df .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_INVCHI_PDF - Fatal error!'
        write ( *, '(a)' ) '  Degrees of freedom must be positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 ) then

        r8_invchi_pdf = 0.0D+00

      else

        temp2 = df * 0.5D+00
        temp1 = - temp2 * log ( 2.0D+00 ) 
     &    - ( temp2 + 1.0D+00 ) * log ( rval ) 
     &    - 0.5D+00 / rval - r8_gamma_log ( temp2 )

        r8_invchi_pdf = exp ( temp1 )

      end if

      return
      end
      function r8_invchi_sample ( df )

c*********************************************************************72
c
cc R8_INVCHI_SAMPLE samples an inverse chi-squared distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 May 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision DF, the degrees of freedom.
c    0.0 .lt. DF.
c
c    Output, double precision R8_INVCHI_SAMPLE, a sample value.
c
      implicit none

      double precision a
      double precision b
      double precision df
      double precision r8_gamma_sample
      double precision r8_invchi_sample
      double precision value

      a = 0.5D+00
      b = 0.5D+00 * df

      value = r8_gamma_sample ( a, b )

      if ( value /= 0.0D+00 ) then
        value = 1.0D+00 / value
      end if

      r8_invchi_sample = value

      return
      end
      function r8_invgam_pdf ( beta, alpha, rval )

c*********************************************************************72
c
cc R8_INVGAM_PDF evaluates the PDF of an inverse gamma distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision BETA, the rate parameter.
c    0.0 .lt. BETA.
c
c    Input, double precision ALPHA, the shape parameter.
c    0.0 .lt. ALPHA.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_INVGAM_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision alpha
      double precision beta
      double precision r8_gamma_log
      double precision r8_invgam_pdf
      double precision rval
      double precision temp

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_INVGAM_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter ALPHA is not positive.'
        stop
      end if

      if ( beta .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_INVGAM_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter BETA is not positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 ) then

        r8_invgam_pdf = 0.0D+00

      else

        temp = alpha * log ( beta ) - ( alpha + 1.0D+00 ) * log ( rval )
     &    - beta / rval - r8_gamma_log ( alpha )

        r8_invgam_pdf = exp ( temp )

      end if

      return
      end
      function r8_invgam_sample ( beta, alpha )

c*********************************************************************72
c
cc R8_INVGAM_SAMPLE samples an inverse gamma distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 May 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision BETA, the rate parameter.
c    0.0 .lt. BETA.
c
c    Input, double precision ALPHA, the shape parameter.
c    0.0 .lt. ALPHA.
c
c    Output, double precision R8_INVGAM_SAMPLE, a sample value.
c
      implicit none

      double precision alpha
      double precision beta
      double precision r8_gamma_sample
      double precision r8_invgam_sample
      double precision value

      value = r8_gamma_sample ( beta, alpha )

      if ( value .ne. 0.0D+00 ) then
        value = 1.0D+00 / value
      end if

      r8_invgam_sample = value

      return
      end
      function r8_normal_pdf ( av, sd, rval )

c*********************************************************************72
c
cc R8_NORMAL_PDF evaluates the PDF of a normal distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision AV, the mean value.
c
c    Input, double precision SD, the standard deviation.
c    0.0 .lt. SD.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_NORMAL_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision av
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_normal_pdf
      double precision rtemp
      double precision rval
      double precision sd

      if ( sd .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_NORMAL_PDF - Fatal error!'
        write ( *, '(a)' ) '  Standard deviation must be positive.'
        stop
      end if

      rtemp = ( rval - av ) * ( rval - av ) * 0.5D+00 / ( sd * sd )

      r8_normal_pdf = exp ( - rtemp ) / sd / sqrt ( 2.0D+00 * pi )

      return
      end
      function r8_normal_sample ( av, sd )

c*********************************************************************72
c
cc R8_NORMAL_SAMPLE generates a normal random deviate.
c
c  Discussion:
c
c    This procedure generates a single random deviate from a normal distribution
c    with mean AV, and standard deviation SD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joachim Ahrens, Ulrich Dieter,
c    Extensions of Forsythe's Method for Random
c    Sampling from the Normal Distribution,
c    Mathematics of Computation,
c    Volume 27, Number 124, October 1973, page 927-937.
c
c  Parameters:
c
c    Input, double precision AV, the mean.
c
c    Input, double precision SD, the standard deviation.
c
c    Output, double precision R8_NORMAL_SAMPLE, a random deviate
c    from the distribution.
c
      implicit none

      double precision av
      double precision r8_normal_sample
      double precision r8_normal_01_sample
      double precision sd

      r8_normal_sample = sd * r8_normal_01_sample ( ) + av

      return
      end
      function r8_normal_01_pdf ( rval )

c*********************************************************************72
c
cc R8_NORMAL_01_PDF evaluates the PDF of a standard normal distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_NORMAL_01_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_normal_01_pdf
      double precision rval

      r8_normal_01_pdf = exp ( - 0.5D+00 * rval ** 2 ) 
     &  / sqrt ( 2.0D+00 * pi )

      return
      end
      function r8_normal_01_sample ( )

c*********************************************************************72
c
cc R8_NORMAL_01_SAMPLE returns a unit pseudonormal R8.
c
c  Discussion:
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    The Box-Muller method is used, which is efficient, but
c    generates two values at a time.
c
c    Typically, we would use one value and save the other for the next call.
c    However, the fact that this function has saved memory makes it difficult
c    to correctly handle cases where we want to re-initialize the code,
c    or to run in parallel.  Therefore, we will instead use the first value
c    and DISCARD the second.
c
c    EFFICIENCY must defer to SIMPLICITY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_NORMAL_01_SAMPLE, a sample of the standard
c    normal PDF.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r1
      double precision r2
      double precision r8_normal_01_sample
      double precision r8_uniform_01_sample
      double precision x

      r1 = r8_uniform_01_sample ( )
      r2 = r8_uniform_01_sample ( )

      x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )

      r8_normal_01_sample = x

      return
      end
      function r8_scinvchi_pdf ( df, s, rval )

c*********************************************************************72
c
cc R8_SCINVCHI_PDF: PDF for a scaled inverse chi-squared distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision DF, the degrees of freedom.
c    0.0 .lt. DF.
c
c    Input, double precision S, the scale factor.
c    0.0 .lt. S.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_SCINVCHI_PDF, the value of the PDF at RVAL.
c    inverse-chi-square distribution.
c
      implicit none

      double precision df
      double precision r8_gamma_log
      double precision r8_scinvchi_pdf
      double precision rval
      double precision s
      double precision temp1
      double precision temp2

      if ( df .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_SCINVCHI_PDF - Fatal error!'
        write ( *, '(a)' ) '  Degrees of freedom must be positive.'
        stop
      end if

      if ( s .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_SCINVCHI_PDF - Fatal error!'
        write ( *, '(a)' ) '  Scale parameter must be positive.'
        stop
      end if

      if ( rval .le. 0.0D+00 ) then

        r8_scinvchi_pdf = 0.0D+00

      else

        temp2 = df * 0.5D+00
        temp1 = temp2 * log ( temp2 ) + temp2 * log ( s ) 
     &    - ( temp2 * s / rval ) 
     &    - ( temp2 + 1.0D+00 ) * log ( rval ) - r8_gamma_log ( temp2 )

        r8_scinvchi_pdf = exp ( temp1 )

      end if

      return
      end
      function r8_scinvchi_sample ( df, s )

c*********************************************************************72
c
cc R8_SCINVCHI_SAMPLE: sample a scaled inverse chi-squared distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision DF, the degrees of freedom.
c    0.0 .lt. DF.
c
c    Input, double precision S, the scale factor.
c    0.0 .lt. S.
c
c    Input, double precision R8_SCINVCHI_SAMPLE, a sample value.
c
      implicit none

      double precision a
      double precision b
      double precision df
      double precision r8_gamma_sample
      double precision r8_scinvchi_sample
      double precision s
      double precision value

      a = 0.5D+00 * df * s
      b = 0.5D+00 * df

      value = r8_gamma_sample ( a, b )

      if ( value .ne. 0.0D+00 ) then
        value = 1.0D+00 / value
      end if

      r8_scinvchi_sample = value

      return
      end
      function r8_uniform_pdf ( lower, upper, rval )

c*********************************************************************72
c
cc R8_UNIFORM_PDF evaluates the PDF of a uniform distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2013
c
c  Author:
c
c    FORTRAN90 version by Guannan Zhang.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision LOWER, UPPER, the lower and upper range limits.
c    LOWER .lt. UPPER.
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_UNIFORM_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision lower
      double precision r8_uniform_pdf
      double precision rval
      double precision upper

      if ( upper .le. lower ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_PDF - Fatal error!'
        write ( *, '(a)' ) '  For uniform PDF, the lower limit must be '
        write ( *, '(a)' ) '  less than the upper limitc'
        stop
      end if

      if ( rval .lt. lower ) then
        r8_uniform_pdf = 0.0D+00
      else if ( rval .le. upper ) then
        r8_uniform_pdf = 1.0D+00 / ( upper - lower )
      else
        r8_uniform_pdf = 0.0D+00
      end if

      return
      end
      function r8_uniform_sample ( low, high )

c*********************************************************************72
c
cc R8_UNIFORM_SAMPLE generates a uniform random deviate.
c
c  Discussion:
c
c    This procedure generates a real deviate uniformly distributed between
c    LOW and HIGH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision LOW, HIGH, the lower and upper bounds.
c
c    Output, double precision R8_UNIFORM_SAMPLE, a random deviate
c    from the distribution.
c
      implicit none

      double precision high
      double precision low
      double precision r8_uniform_01_sample
      double precision r8_uniform_sample

      r8_uniform_sample = low + ( high - low ) 
     &  * r8_uniform_01_sample ( )

      return
      end
      function r8_uniform_01_pdf ( rval )

c*********************************************************************72
c
cc R8_UNIFORM_01_PDF evaluates the PDF of a standard uniform distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision RVAL, the point where the PDF is evaluated.
c
c    Output, double precision R8_UNIFORM_01_PDF, the value of the PDF at RVAL.
c
      implicit none

      double precision r8_uniform_01_pdf
      double precision rval

      if ( rval .lt. 0.0D+00 ) then
        r8_uniform_01_pdf = 0.0D+00
      else if ( rval .le. 1.0D+00 ) then
        r8_uniform_01_pdf = 1.0D+00
      else
        r8_uniform_01_pdf = 0.0D+00
      end if

      return
      end
      function r8_uniform_01_sample ( )

c*********************************************************************72
c
cc R8_UNIFORM_01_SAMPLE generates a uniform random deviate from [0,1].
c
c  Discussion:
c
c    This function should be the only way that the package accesses random
c    numbers.
c
c    Setting OPTION to 0 accesses the R8_UNI_01() function in RNGLIB,
c    for which there are versions in various languages, which should result
c    in the same values being returned.  This should be the only function
c    that calls a function in RNGLIB.
c
c    Setting OPTION to 1 in the FORTRAN90 version calls the system
c    RNG "random_number()".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Output, double precision R8_UNIFORM_01_SAMPLE, a random deviate
c    from the distribution.
c
      implicit none

      integer option
      parameter ( option = 0 )
      double precision r8_uni_01
      double precision r8_uniform_01_sample
      double precision value

      if ( option == 0 ) then
        value = r8_uni_01 ( )
      else
        call random_number ( harvest = value )
      end if

      r8_uniform_01_sample = value

      return
      end
      subroutine r8mat_podet ( n, r, det )

c*********************************************************************72
c
cc R8MAT_PODET computes the determinant of a factored positive definite matrix.
c
c  Discussion:
c
c    This routine expects to receive R, the upper triangular factor of A,
c    computed by R8MAT_POFAC, with the property that A = R' * R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision R(N,N), the Cholesky factor of A.
c
c    Output, double precision DET, the determinant of A.
c
      implicit none

      integer n

      double precision det
      integer i
      double precision r(n,n)

      det = 1.0D+00
      do i = 1, n
        det = det * r(i,i) * r(i,i)
      end do

      return
      end
      subroutine r8mat_pofac ( n, a, r )

c*********************************************************************72
c
cc R8MAT_POFAC factors a real symmetric positive definite matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the symmetric matrix
c    to be factored.  Only the diagonal and upper triangle are used.
c
c    Output, double precision R(N,N), an upper triangular matrix such that
c    A = R'*R.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer j
      integer k
      double precision r(n,n)
      double precision r8vec_dot_product
      double precision s
      double precision t

      do j = 1, n
        do i = 1, j
          r(i,j) = a(i,j)
        end do
        do i = j + 1, n
          r(i,j) = 0.0D+00
        end do
      end do

      do j = 1, n

        s = 0.0D+00

        do k = 1, j - 1
          t = r(k,j) 
     &      - r8vec_dot_product ( k - 1, r(1:k-1,k), r(1:k-1,j) )
          t = t / r(k,k)
          r(k,j) = t
          s = s + t * t
        end do

        s = r(j,j) - s

        if ( s .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8MAT_POFAC - Fatal error!'
          write ( *, '(a)' ) '  The matrix is not positive definite.'
          stop
        end if

        if ( s .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8MAT_POFAC - Warning!'
          write ( *, '(a)' ) 
     &      '  The matrix is not strictly positive definite.'
        end if

        r(j,j) = sqrt ( s )

      end do

      return
      end
      subroutine r8mat_poinv ( n, r, b )

c*********************************************************************72
c
cc R8MAT_POINV computes the inverse of a factored positive definite matrix.
c
c  Discussion:
c
c    This routine expects to receive R, the upper triangular factor of A,
c    computed by R8MAT_POFAC, with the property that A = R' * R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix A.
c
c    Input, double precision R(N,N), the Cholesky factor of A.
c
c    Input, double precision B(N,N), the inverse of A.
c
      implicit none

      integer n

      double precision b(n,n)
      integer i
      integer j
      integer k
      double precision r(n,n)
      double precision t

      do j = 1, n
        do i = 1, n
          b(i,j) = r(i,j)
        end do
      end do

      do k = 1, n

        b(k,k) = 1.0D+00 / b(k,k)
        do i = 1, k - 1
          b(i,k) = - b(i,k) * b(k,k)
        end do

        do j = k + 1, n
          t = b(k,j)
          b(k,j) = 0.0D+00
          do i = 1, k
            b(i,j) = b(i,j) + t * b(i,k)
          end do
        end do

      end do
c
c  Form inverse(R) * (inverse(R))'.
c
      do j = 1, n
        do k = 1, j - 1
          t = b(k,j)
          do i = 1, k
            b(i,k) = b(i,k) + t * b(i,j)
          end do
        end do
        t = b(j,j)
        do i = 1, j
          b(i,j) = b(i,j) * t
        end do
      end do

      return
      end
      subroutine r8mat_upsol ( n, r, b, x )

c*********************************************************************72
c
cc R8MAT_UPSOL solves R * X = B for an upper triangular matrix R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision R(N,N), the upper triangular matrix.
c
c    Input, double precision B(N), the right hand side.
c
c    Output, double precision X(N), the solution.
c
      implicit none

      integer n

      double precision b(n)
      integer i
      integer j
      double precision r(n,n)
      double precision x(n)

      do i = 1, n
        x(i) = b(i)
      end do

      x(n) = x(n) / r(n,n)

      do j = n - 1, 1, -1
        do i = 1, j
          x(i) = x(i) - r(i,j+1) * x(j+1)
        end do
        x(j) = x(j) / r(j,j)
      end do

      return
      end
      subroutine r8mat_utsol ( n, r, b, x )

c*********************************************************************72
c
cc R8MAT_UTSOL solves R' * X = B for an upper triangular matrix R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 June 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision R(N,N), the upper triangular matrix.
c
c    Input, double precision B(N), the right hand side.
c
c    Output, double precision X(N), the solution.
c
      implicit none

      integer n

      double precision b(n)
      integer i
      integer j
      double precision r(n,n)
      double precision r8vec_dot_product
      double precision x(n)

      do i = 1, n
        x(i) = b(i)
      end do

      x(1) = x(1) / r(1,1)

      do j = 2, n
        x(j) = ( x(j) 
     &    - r8vec_dot_product ( j - 1, r(1:j-1,j), x(1:j-1) ) ) / r(j,j)
      end do

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
      function r8vec_multinormal_pdf ( n, mu, r, c_det, x )

c*********************************************************************72
c
cc R8VEC_MULTINORMAL_PDF evaluates a multivariate normal PDF.
c
c  Discussion:
c
c    PDF ( MU(1:N), C(1:N,1:N); X(1:N) ) =
c      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / sqrt ( det ( C ) )
c      * exp ( - ( X - MU )' * inverse ( C ) * ( X - MU ) / 2 )
c
c    Here,
c
c      X is the argument vector of length N,
c      MU is the mean vector of length N,
c      C is an N by N positive definite symmetric covariance matrix.
c
c    The properties of C guarantee that it has an upper triangular
c    matrix R, the Cholesky factor, such that C = R' * R.  It is the
c    matrix R that is required by this routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the spatial dimension.
c
c    Input, double precision MU(N), the mean vector.
c
c    Input, double precision R(N,N), the upper triangular Cholesky
c    factor of the covariance matrix C.
c
c    Input, double precision C_DET, the determinant of the
c    covariance matrix C.
c
c    Input, double precision X(N), a sample of the distribution.
c
c    Output, double precision R8VEC_MULTINORMAL_PDF, the PDF evaluated
c    at X.
c
      implicit none

      integer n

      double precision b(n)
      double precision c_det
      integer i
      double precision mu(n)
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(n,n)
      double precision r8vec_multinormal_pdf
      double precision r8vec_dot_product
      double precision x(n)
      double precision xcx
      double precision y(n)
c
c  Compute:
c    inverse(R')*(x-mu) = y
c  by solving:
c    R'*y = x-mu
c
      do i = 1, n
        b(i) = x(i) - mu(i)
      end do

      call r8mat_utsol ( n, r, b, y )
c
c  Compute:
c    (x-mu)' * inv(C)          * (x-mu)
c  = (x-mu)' * inv(R'*R)       * (x-mu)
c  = (x-mu)' * inv(R) * inv(R) * (x-mu)
c  = y' * y.
c
      xcx = r8vec_dot_product ( n, y, y )

      pdf = 1.0D+00 / sqrt ( ( 2.0D+00 * pi ) ** n )
     &    * 1.0D+00 / sqrt ( c_det )
     &    * exp ( -0.5D+00 * xcx )

      r8vec_multinormal_pdf = pdf

      return
      end
      subroutine r8vec_multinormal_sample ( n, mu, r, x )

c*********************************************************************72
c
cc R8VEC_MULTINORMAL_SAMPLE samples a multivariate normal PDF.
c
c  Discussion:
c
c    PDF ( MU(1:N), C(1:N,1:N); X(1:N) ) =
c      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / det ( C )
c      * exp ( - ( X - MU )' * inverse ( C ) * ( X - MU ) / 2 )
c
c    Here,
c
c      X is the argument vector of length N,
c      MU is the mean vector of length N,
c      C is an N by N positive definite symmetric covariance matrix.
c
c    The properties of C guarantee that it has an upper triangular
c    matrix R, the Cholesky factor, such that C = R' * R.  It is the
c    matrix R that is required by this routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the spatial dimension.
c
c    Input, double precision MU(N), the mean vector.
c
c    Input, double precision R(N,N), the upper triangular Cholesky
c    factor of the covariance matrix C.
c
c    Output, double precision X(N), a sample of the distribution.
c
      implicit none

      integer n

      integer i
      integer j
      double precision mu(n)
      double precision r(n,n)
      double precision r8_normal_01_sample
      double precision x(n)
      double precision z(n)
c
c  Compute X = MU + R' * Z
c  where Z is a vector of standard normal variates.
c
      do j = 1, n
        z(j) = r8_normal_01_sample ( )
      end do

      do i = 1, n
        x(i) = mu(i)
        do j = 1, i
          x(i) = x(i) + r(j,i) * z(j)
        end do
      end do

      return
      end
