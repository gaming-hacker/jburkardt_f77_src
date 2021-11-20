      function genbet ( aa, bb )

c*********************************************************************72
c
cc GENBET generates a beta random deviate.
c
c  Discussion:
c
c    This procedure returns a single random deviate from the beta distribution 
c    with parameters A and B.  The density is
c
c      x^(a-1) * (1-x)^(b-1) / Beta(a,b) for 0 < x < 1
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
c    Modifications by John Burkardt.
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
c    Input, real AA, the first parameter of the beta distribution.
c    0.0 < AA.
c
c    Input, real BB, the second parameter of the beta distribution.
c    0.0 < BB.
c
c    Output, real GENBET, a beta random variate.
c
      implicit none

      real a
      real aa
      real alpha
      real b
      real bb
      real beta
      real delta
      real gamma
      real genbet
      real k1
      real k2
      real log4 
      parameter ( log4 = 1.3862943611198906188E+00 )
      real log5
      parameter ( log5 = 1.6094379124341003746E+00 )
      real r
      real r4_exp
      real r4_uni_01
      real s
      real t
      real u1
      real u2
      real v
      real w
      real y
      real z

      if ( aa .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENBET - Fatal error!'
        write ( *, '(a)' ) '  AA <= 0.0'
        stop 1
      end if

      if ( bb .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENBET - Fatal error!'
        write ( *, '(a)' ) '  BB <= 0.0'
        stop 1
      end if
c
c  Algorithm BB
c
      if ( 1.0E+00 .lt. aa .and. 1.0E+00 .lt. bb ) then

        a = min ( aa, bb )
        b = max ( aa, bb )
        alpha = a + b
        beta = sqrt ( ( alpha - 2.0E+00 ) 
     &    / ( 2.0E+00 * a * b - alpha ) )
        gamma = a + 1.0E+00 / beta

10      continue

        u1 = r4_uni_01 ( )
        u2 = r4_uni_01 ( )
        v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
c
c  exp(v) replaced by r4_exp(v).
c
        w = a * r4_exp ( v )

        z = u1 ** 2 * u2
        r = gamma * v - log4
        s = a + r - w

        if ( s + 1.0E+00 + log5 .lt. 5.0E+00 * z ) then
          t = log ( z )
          if ( s .lt. t ) then
            if ( ( r + alpha * log ( alpha / ( b + w ) ) ) .lt. t ) then
              go to 10
            end if
          end if
        end if
c
c  Algorithm BC
c
      else

        a = max ( aa, bb )
        b = min ( aa, bb )
        alpha = a + b
        beta = 1.0E+00 / b
        delta = 1.0E+00 + a - b
        k1 = delta * ( 1.0E+00 / 72.0E+00 + b / 24.0E+00 ) 
     &    / ( a / b - 7.0E+00 / 9.0E+00 )
        k2 = 0.25E+00 + ( 0.5E+00 + 0.25E+00 / delta ) * b

20      continue

        u1 = r4_uni_01 ( )
        u2 = r4_uni_01 ( )

        if ( u1 .lt. 0.5E+00 ) then

          y = u1 * u2
          z = u1 * y

          if ( k1 .le. 0.25E+00 * u2 + z - y ) then
            go to 20
          end if

        else

          z = u1 ** 2 * u2

          if ( z .le. 0.25E+00 ) then

            v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
            w = a * exp ( v )

            if ( aa .eq. a ) then
              genbet = w / ( b + w )
            else
              genbet = b / ( b + w )
            end if

            return

          end if

          if ( k2 .lt. z ) then
            go to 20
          end if

        end if

        v = beta * log ( u1 / ( 1.0E+00 - u1 ) )
        w = a * exp ( v )

        if ( ( alpha * ( log ( alpha / ( b + w ) ) + v ) 
     &    - log4 ) .lt. log ( z ) ) then
          go to 20
        end if

      end if

      if ( aa .eq. a ) then
        genbet = w / ( b + w )
      else
        genbet = b / ( b + w )
      end if

      return
      end
      function genchi ( df )

c*********************************************************************72
c
cc GENCHI generates a Chi-Square random deviate.
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
c    20 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real DF, the degrees of freedom.
c    0.0 < DF.
c
c    Output, real GENCHI, a random deviate from the distribution.
c
      implicit none

      real arg1
      real arg2
      real df
      real genchi
      real gengam

      if ( df .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENCHI - Fatal error!'
        write ( *, '(a)' ) '  DF <= 0.'
        write ( *, '(a,g14.6)' ) '  Value of DF: ', df
        stop 1
      end if

      arg1 = 1.0E+00
      arg2 = df / 2.0E+00

      genchi = 2.0E+00 * gengam ( arg1, arg2 )

      return
      end
      function genexp ( av )

c*********************************************************************72
c
cc GENEXP generates an exponential random deviate.
c
c  Discussion:
c
c    This procedure generates a single random deviate from an exponential
c    distribution with mean AV.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Reference:
c
c    Joachim Ahrens, Ulrich Dieter,
c    Computer Methods for Sampling From the
c    Exponential and Normal Distributions,
c    Communications of the ACM,
c    Volume 15, Number 10, October 1972, pages 873-882.
c
c  Parameters:
c
c    Input, real AV, the mean of the exponential distribution from which
c    a random deviate is to be generated.
c
c    Output, real GENEXP, a random deviate from the distribution.
c
      implicit none

      real av
      real genexp
      real sexpo

      genexp = sexpo ( ) * av

      return
      end
      function genf ( dfn, dfd )

c*********************************************************************72
c
cc GENF generates an F random deviate.
c
c  Discussion:
c
c    This procedure generates a random deviate from the F (variance ratio)
c    distribution with DFN degrees of freedom in the numerator
c    and DFD degrees of freedom in the denominator.
c
c    It directly generates the ratio of chisquare variates
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real DFN, the numerator degrees of freedom.
c    0.0 < DFN.
c
c    Input, real DFD, the denominator degrees of freedom.
c    0.0 < DFD.
c
c    Output, real GENF, a random deviate from the distribution.
c
      implicit none

      real dfd
      real dfn
      real genchi
      real genf
      real xden
      real xnum

      if ( dfn .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENF - Fatal error!'
        write ( *, '(a)' ) '  DFN <= 0.0'
        stop 1
      end if

      if ( dfd .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENF - Fatal error!'
        write ( *, '(a)' ) '  DFD <= 0.0'
        stop 1
      end if

      xnum = genchi ( dfn ) / dfn
      xden = genchi ( dfd ) / dfd
      genf = xnum / xden

      return
      end
      function gengam ( a, r )

c*********************************************************************72
c
cc GENGAM generates a Gamma random deviate.
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
c    20 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    Input, real A, the location parameter.
c
c    Input, real R, the shape parameter.
c
c    Output, real GENGAM, a random deviate from the distribution.
c
      implicit none

      real a
      real gengam
      real r
      real sgamma

      gengam = sgamma ( r ) / a

      return
      end
      subroutine genmn ( parm, x, work )

c*********************************************************************72
c
cc GENMN generates a multivariate normal deviate.
c
c  Discussion:
c
c    The method is:
c    1) Generate P independent standard normal deviates - Ei ~ N(0,1)
c    2) Using Cholesky decomposition find A so that A'*A = COVM
c    3) A' * E + MEANV ~ N(MEANV,COVM)
c
c    Note that PARM contains information needed to generate the
c    deviates, and is set up by SETGMN.
c
c    PARM(1) contains the size of the deviates, P
c    PARM(2:P+1) contains the mean vector.
c    PARM(P+2:P*(P+3)/2+1) contains the upper half of the Cholesky
c    decomposition of the covariance matrix.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real PARM(P*(P+3)/2+1), parameters set by SETGMN.
c
c    Output, real X(P), a random deviate from the distribution.
c
c    Workspace, real WORK(P).
c
      implicit none

      real ae
      integer i
      integer icount
      integer j
      integer p
      real parm(*)
      real snorm
      real work(*)
      real x(*)

      p = int ( parm(1) )
c
c  Generate P independent normal deviates.
c
      do i = 1, p
        work(i) = snorm ( )
      end do
c
c  Compute X = MEANV + A' * WORK
c
      do i = 1, p
        icount = 0
        ae = 0.0E+00
        do j = 1, i
          icount = icount + j - 1
          ae = ae + parm(i+(j-1)*p-icount+p+1) * work(j)
        end do

        x(i) = ae + parm(i+1)

      end do

      return
      end
      subroutine genmul ( n, p, ncat, ix )

c*********************************************************************72
c
cc GENMUL generates a multinomial random deviate.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    Input, integer N, the number of events, which will be classified
c    into one of the NCAT categories.
c
c    Input, real P(NCAT-1).  P(I) is the probability that an event will be 
c    classified into category I.  Thus, each P(I) must be between 0.0 and 1.0.
c    Only the first NCAT-1 values of P must be defined since P(NCAT) would be
c    1.0 minus the sum of the first NCAT-1 P's.
c
c    Input, integer NCAT, the number of categories.
c
c    Output, integer IX(NCAT), a random observation from the multinomial
c    distribution.  All IX(i) will be nonnegative and their sum will be N.
c
      implicit none

      integer n
      integer ncat

      integer i
      integer icat
      integer ignbin
      integer ix(ncat)
      integer ntot
      real p(ncat-1)
      real prob
      real ptot

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENMUL - Fatal error!'
        write ( *, '(a)' ) '  N < 0'
        stop 1
      end if

      if ( ncat .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENMUL - Fatal error!'
        write ( *, '(a)' ) '  NCAT <= 1'
        stop 1
      end if

      do i = 1, ncat - 1

        if ( p(i) .lt. 0.0E+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'GENMUL - Fatal error!'
          write ( *, '(a)' ) '  Some P(i) < 0.'
          stop 1
        end if

        if ( 1.0E+00 .lt. p(i) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'GENMUL - Fatal error!'
          write ( *, '(a)' ) '  Some 1 < P(i).'
          stop 1
        end if

      end do

      ptot = 0.0E+00
      do i = 1, ncat - 1
        ptot = ptot + p(i)
      end do

      if ( 0.99999E+00 .lt. ptot ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENMUL - Fatal error!'
        write ( *, '(a)' ) '  1 < Sum of P().'
        stop 1
      end if
c
c  Initialize variables.
c
      ntot = n
      ptot = 1.0E+00
      do i = 1, ncat
        ix(i) = 0
      end do
c
c  Generate the observation.
c
      do icat = 1, ncat - 1
        prob = p(icat) / ptot
        ix(icat) = ignbin ( ntot, prob )
        ntot = ntot - ix(icat)
        if ( ntot .le. 0 ) then
          return
        end if
        ptot = ptot - p(icat)
      end do

      ix(ncat) = ntot

      return
      end
      function gennch ( df, xnonc )

c*********************************************************************72
c
cc GENNCH generates a noncentral Chi-Square random deviate.
c
c  Discussion:
c
c    This procedure generates a random deviate from the  distribution of a 
c    noncentral chisquare with DF degrees of freedom and noncentrality parameter
c    XNONC.
c
c    It uses the fact that the noncentral chisquare is the sum of a chisquare
c    deviate with DF-1 degrees of freedom plus the square of a normal
c    deviate with mean XNONC and standard deviation 1.
c
c    A subtle ambiguity arises in the original formulation:
c
c      gennch = genchi ( arg1 ) + ( gennor ( arg2, arg3 ) ) ** 2
c
c    because the compiler is free to invoke either genchi or gennor
c    first, both of which alter the random number generator state,
c    resulting in two distinct possible results.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real DF, the degrees of freedom.
c    1.0 < DF.
c
c    Input, real XNONC, the noncentrality parameter.
c    0.0 <= XNONC.
c
c    Output, real GENNCH, a random deviate from the distribution.
c 
      implicit none

      real arg1
      real arg2
      real arg3
      real df
      real genchi
      real gennch
      real gennor
      real t1
      real t2
      real xnonc

      if ( df .le. 1.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENNCH - Fatal error!'
        write ( *, '(a)' ) '  DF <= 1.'
        stop 1
      end if

      if ( xnonc .lt. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENNCH - Fatal error!'
        write ( *, '(a)' ) '  XNONC < 0.0.'
        stop 1
      end if

      arg1 = df - 1.0E+00
      arg2 = sqrt ( xnonc )
      arg3 = 1.0E+00

      t1 = genchi ( arg1 )
      t2 = gennor ( arg2, arg3 )

      gennch = t1 + t2 * t2

      return
      end
      function gennf ( dfn, dfd, xnonc )

c*********************************************************************72
c
cc GENNF generates a noncentral F random deviate.
c
c  Discussion:
c
c    This procedure generates a random deviate from the noncentral F 
c    (variance ratio) distribution with DFN degrees of freedom in the 
c    numerator, and DFD degrees of freedom in the denominator, and 
c    noncentrality parameter XNONC.
c
c    It directly generates the ratio of noncentral numerator chisquare variate
c    to central denominator chisquare variate.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real DFN, the numerator degrees of freedom.
c    1.0 < DFN.
c
c    Input, real DFD, the denominator degrees of freedom.
c    0.0 < DFD.
c
c    Input, real XNONC, the noncentrality parameter.
c    0.0 <= XNONC.
c
c    Output, real GENNF, a random deviate from the distribution.
c
      implicit none

      real dfd
      real dfn
      real genchi
      real gennch
      real gennf
      real xden
      real xnonc
      real xnum

      if ( dfn .le. 1.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENNF - Fatal error!'
        write ( *, '(a)' ) '  DFN <= 1.0'
        stop 1
      end if

      if ( dfd .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENNF - Fatal error!'
        write ( *, '(a)' ) '  DFD <= 0.0'
        stop 1
      end if

      if ( xnonc .lt. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENNF - Fatal error!'
        write ( *, '(a)' ) '  XNONC < 0.0'
        stop 1
      end if

      xnum = gennch ( dfn, xnonc ) / dfn
      xden = genchi ( dfd ) / dfd

      gennf = xnum / xden

      return
      end
      function gennor ( av, sd )

c*********************************************************************72
c
cc GENNOR generates a normal random deviate.
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
c    20 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    Input, real AV, the mean of the normal distribution.
c
c    Input, real SD, the standard deviation of the normal distribution.
c
c    Output, real GENNOR, a random deviate from the distribution.
c 
      implicit none

      real av
      real gennor
      real sd
      real snorm

      gennor = sd * snorm ( ) + av

      return
      end
      subroutine genprm ( iarray, n )

c*********************************************************************72
c
cc GENPRM generates and applies a random permutation to an array.
c
c  Discussion:
c
c    To see the permutation explicitly, let the input array be
c    1, 2, ..., N.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input/output, integer IARRAY(N), an array to be permuted.
c
c    Input, integer N, the number of entries in the array.
c
      implicit none

      integer n

      integer i
      integer iarray(n)
      integer ignuin
      integer itmp
      integer iwhich

      do i = 1, n
        iwhich = ignuin ( i, n )
        itmp = iarray(iwhich)
        iarray(iwhich) = iarray(i)
        iarray(i) = itmp
      end do

      return
      end
      function genunf ( low, high )

c*********************************************************************72
c
cc GENUNF generates a uniform random deviate.
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
c    20 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real LOW, HIGH, the lower and upper bounds.
c
c    Output, real GENUNF, a random deviate from the distribution.
c 
      implicit none

      real genunf
      real high
      real low
      real r4_uni_01

      genunf = low + ( high - low ) * r4_uni_01 ( )

      return
      end
      function ignbin ( n, pp )

c*********************************************************************72
c
cc IGNBIN generates a binomial random deviate.
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
c    29 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    0 < N.
c
c    Input, real PP, the probability of an event in each trial of the
c    binomial distribution from which a random deviate is to be generated.
c    0.0 < PP < 1.0.
c
c    Output, integer IGNBIN, a random deviate from the distribution.
c
      implicit none

      real al
      real alv
      real amaxp
      real c
      real f
      real f1
      real f2
      real ffm
      real fm
      real g
      integer i
      integer ignbin
      integer ix
      integer ix1
      integer k
      integer m
      integer mp
      real pp
      integer n
      real p
      real p1
      real p2
      real p3
      real p4
      real q
      real qn
      real r
      real r4_uni_01
      real t
      real u
      real v
      real w
      real w2
      real x
      real x1
      real x2
      real xl
      real xll
      real xlr
      real xm
      real xnp
      real xnpq
      real xr
      real ynorm
      real z
      real z2

      if ( pp <= 0.0E+00 .or. 1.0E+00 <= pp ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IGNBIN - Fatal error!'
        write ( *, '(a)' ) '  PP is out of range.'
        stop 1
      end if

      p = min ( pp, 1.0E+00 - pp )
      q = 1.0E+00 - p
      xnp = real ( n ) * p

      if ( xnp .lt. 30.0E+00 ) then
        qn = q ** n
        r = p / q
        g = r * real ( n + 1 )
        go to 20
      end if
c
c  The calculation of this data was originally intended to be
c  done once, then saved for later calls.  
c
      ffm = xnp + p
      m = ffm
      fm = m
      xnpq = xnp * q
      p1 = int ( 2.195E+00 * sqrt ( xnpq ) - 4.6E+00 * q ) + 0.5E+00
      xm = fm + 0.5E+00
      xl = xm - p1
      xr = xm + p1
      c = 0.134E+00 + 20.5E+00 / ( 15.3E+00 + fm )
      al = ( ffm - xl ) / ( ffm - xl * p )
      xll = al * ( 1.0E+00 + 0.5E+00 * al )
      al = ( xr - ffm ) / ( xr * q )
      xlr = al * ( 1.0E+00 + 0.5E+00 * al )
      p2 = p1 * ( 1.0E+00 + c + c )
      p3 = p2 + c / xll
      p4 = p3 + c / xlr
c
c  Generate a variate.
c
10    continue

      u = r4_uni_01 ( ) * p4
      v = r4_uni_01 ( )
c
c  Triangle
c
      if ( u .lt. p1 ) then
        ix = xm - p1 * v + u
        if ( 0.5E+00 .lt. pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if
c
c  Parallelogram
c
      if ( u .le. p2 ) then

        x = xl + ( u - p1 ) / c
        v = v * c + 1.0E+00 - abs ( xm - x ) / p1

        if ( v .le. 0.0E+00 .or. 1.0E+00 .lt. v ) then
          go to 10
        end if

        ix = x

      else if ( u .le. p3 ) then

        ix = xl + alog ( v ) / xll
        if ( ix .lt. 0 ) then
          go to 10
        end if
        v = v * ( u - p2 ) * xll

      else

        ix = xr - alog ( v ) / xlr
        if ( n .lt. ix ) then
          go to 10
        end if
        v = v * ( u - p3 ) * xlr

      end if

      k = abs ( ix - m )

      if ( k .le. 20 .or. xnpq / 2.0 - 1.0 .le. k ) then

        f = 1.0E+00
        r = p / q
        g = ( n + 1 ) * r

        if ( m .lt. ix ) then
          mp = m + 1
          do i = mp, ix
            f = f * ( g / i - r )
          end do
        else if ( ix .lt. m ) then
          ix1 = ix + 1
          do i = ix1, m
            f = f / ( g / i - r )
          end do
        end if

        if ( v .le. f ) then
          if ( 0.5E+00 .lt. pp ) then
            ix = n - ix
          end if
          ignbin = ix
          return
        end if

      else

        amaxp = ( k / xnpq ) * ( ( k * ( k / 3.0E+00
     &    + 0.625E+00 ) + 0.1666666666666E+00 ) / xnpq + 0.5E+00 )
        ynorm = - real ( k * k ) / ( 2.0E+00 * xnpq )
        alv = alog ( v )

        if ( alv .lt. ynorm - amaxp ) then
          if ( 0.5E+00 .lt. pp ) then
            ix = n - ix
          end if
          ignbin = ix
        return
        end if

        if ( ynorm + amaxp .lt. alv ) then
          go to 10
        end if

        x1 = real ( ix + 1 )
        f1 = fm + 1.0E+00
        z = real ( n + 1 ) - fm
        w = real ( n - ix + 1 )
        z2 = z * z
        x2 = x1 * x1
        f2 = f1 * f1
        w2 = w * w

        t = xm * alog ( f1 / x1 ) + ( n - m + 0.5E+00 ) * alog ( z / w ) 
     &    + real ( ix - m ) * alog ( w * p / ( x1 * q ))
     &    + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 
     &    - ( 99.0E+00 - 140.0E+00
     &    / f2 ) / f2 ) / f2 ) / f2 ) / f1 / 166320.0E+00
     &    + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 
     &    - ( 99.0E+00 - 140.0E+00
     &    / z2 ) / z2 ) / z2 ) / z2 ) / z / 166320.0E+00
     &    + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 
     &    - ( 99.0E+00 - 140.0E+00
     &    / x2 ) / x2 ) / x2 ) / x2 ) / x1 / 166320.0E+00
     &    + ( 13860.0E+00 - ( 462.0E+00 - ( 132.0E+00 
     &    - ( 99.0E+00 - 140.0E+00 
     &    / w2 ) / w2 ) / w2 ) / w2 ) / w / 166320.0E+00

        if ( alv .le. t ) then
          if ( 0.5E+00 .lt. pp ) then
            ix = n - ix
          end if
          ignbin = ix
          return
        end if

      end if

      go to 10
c
c  Mean less than 30.
c
20    continue

      ix = 0
      f = qn
      u = r4_uni_01 ( )

30    continue

      if ( u .lt. f ) then
        if ( 0.5E+00 .lt. pp ) then
          ix = n - ix
        end if
        ignbin = ix
        return
      end if

      if ( ix .le. 110 ) then
        u = u - f
        ix = ix + 1
        f = f * ( g / real ( ix ) - r )
        go to 30
      end if

      go to 20

      return
      end
      function ignnbn ( n, p )

c*********************************************************************72
c
cc IGNNBN generates a negative binomial random deviate.
c
c  Discussion:
c
c    This procedure generates a single random deviate from a negative binomial
c    distribution.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    Input, integer N, the required number of events.
c    0 <= N.
c
c    Input, real P, the probability of an event during a Bernoulli trial.
c    0.0 < P < 1.0.
c
c    Output, integer IGNNBN, a random deviate from the distribution.
c
      implicit none

      real a
      real gengam
      integer ignnbn
      integer ignpoi
      integer n
      real p
      real r
      real y

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IGNNBN - Fatal error!'
        write ( *, '(a)' ) '  N < 0.'
        stop 1
      end if

      if ( p .le. 0.0E+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IGNNBN - Fatal error!'
        write ( *, '(a)' ) '  P <= 0.0'
        stop 1
      end if

      if ( 1.0E+00 .le. p ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IGNNBN - Fatal error!'
        write ( *, '(a)' ) '  1.0 <= P'
        stop 1
      end if
c
c  Generate Y, a random gamma (n,(1-p)/p) variable.
c
      r = real ( n )
      a = p / ( 1.0E+00 - p )
      y = gengam ( a, r )
c
c  Generate a random Poisson ( y ) variable.
c
      ignnbn = ignpoi ( y )

      return
      end
      function ignpoi ( mu )

c*********************************************************************72
c
cc IGNPOI generates a Poisson random deviate.
c
c  Discussion:
c
c    This procedure generates a single random deviate from a Poisson
c    distribution with given mean.
c                                  
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Reference:
c
c    Joachim Ahrens, Ulrich Dieter,
c    Computer Generation of Poisson Deviates
c    From Modified Normal Distributions,
c    ACM Transactions on Mathematical Software,
c    Volume 8, Number 2, June 1982, pages 163-179.
c
c  Parameters:
c
c    Input, real MU, the mean of the Poisson distribution from which
c    a random deviate is to be generated.
c
c    Output, integer IGNPOI, a random deviate from the distribution.
c
      implicit none

      real a0
      real a1
      real a2
      real a3
      real a4
      real a5
      real a6
      real a7
      real b1
      real b2
      real c
      real c0
      real c1
      real c2
      real c3
      real d
      real del
      real difmuk
      real e
      real fact(10)
      real fk
      real fx
      real fy
      real g
      integer ignpoi
      integer j
      integer k
      integer kflag
      integer l
      integer m
      real mu
      real muold
      real muprev
      real omega
      real p
      real p0
      real px
      real py
      real q
      real r4_uni_01
      real s
      real sexpo
      real snorm
      real t
      real u
      real v
      real x
      real xx

      save a0
      save a1
      save a2
      save a3
      save a4
      save a5
      save a6
      save a7
      save fact

      data a0 / -0.5E+00 /
      data a1 /  0.3333333E+00 /
      data a2 / -0.2500068E+00 /
      data a3 /  0.2000118E+00 /
      data a4 / -0.1661269E+00 /
      data a5 /  0.1421878E+00 /
      data a6 / -0.1384794E+00 /
      data a7 /  0.1250060E+00 /
      data fact / 1.0E+00, 1.0E+00, 2.0E+00, 6.0E+00, 24.0E+00, 
     &  120.0E+00, 720.0E+00, 5040.0E+00, 40320.0E+00, 362880.0E+00 /
c
c  Start new table and calculate P0.
c
      if ( mu .lt. 10.0E+00 ) then
 
        m = max ( 1, int ( mu ) )
        p = exp ( - mu )
        q = p
        p0 = p
c
c  Uniform sample for inversion method.
c
10      continue

        u = r4_uni_01 ( )
        ignpoi = 0

        if ( u .le. p0 ) then
          return
        end if
c
c  Creation of new Poisson probabilities.
c
        do k = 1, 35
          p = p * mu / real ( k )
          q = q + p
          if ( u .le. q ) then
            ignpoi = k
            return
          end if
        end do

        go to 10

      else

        s = sqrt ( mu )
        d = 6.0E+00 * mu * mu
        l = int ( mu - 1.1484E+00 )
c
c  Normal sample.
c
        g = mu + s * snorm ( )

        if ( 0.0E+00 .le. g ) then

          ignpoi = int ( g )
c
c  Immediate acceptance if large enough.
c
          if ( l .le. ignpoi ) then
            return
          end if
c
c  Squeeze acceptance.
c
          fk = real ( ignpoi )
          difmuk = mu - fk
          u = r4_uni_01 ( ) 

          if ( difmuk * difmuk * difmuk .le. d * u ) then
            return
          end if

        end if
c
c  Preparation for steps P and Q.
c
        omega = 0.3989423E+00 / s
        b1 = 0.04166667E+00 / mu
        b2 = 0.3E+00 * b1 * b1
        c3 = 0.1428571E+00 * b1 * b2
        c2 = b2 - 15.0E+00 * c3
        c1 = b1 - 6.0E+00 * b2 + 45.0E+00 * c3
        c0 = 1.0E+00 - b1 + 3.0E+00 * b2 - 15.0E+00 * c3
        c = 0.1069E+00 / mu

        if ( 0.0E+00 .le. g ) then

          kflag = 0

          if ( ignpoi .lt. 10 ) then

            px = -mu
            py = mu ** ignpoi / fact(ignpoi+1)

          else

            del = 0.8333333E-01 / fk
            del = del - 4.8E+00 * del * del * del
            v = difmuk / fk

            if ( 0.25E+00 .lt. abs ( v ) ) then
              px = fk * alog ( 1.0E+00 + v ) - difmuk - del
            else
              px = fk * v * v * ((((((( a7
     &          * v + a6 )
     &          * v + a5 )
     &          * v + a4 )
     &          * v + a3 )
     &          * v + a2 )
     &          * v + a1 )
     &          * v + a0 ) - del
            end if

            py = 0.3989423E+00 / sqrt ( fk )

          end if

          x = ( 0.5E+00 - difmuk ) / s
          xx = x * x
          fx = -0.5E+00 * xx
          fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

          if ( kflag .le. 0 ) then

            if ( fy - u * fy .le. py * exp ( px - fx ) ) then
              return
            end if

          else

            if ( c * abs ( u ) .le. 
     &        py * exp ( px + e ) - fy * exp ( fx + e ) ) then
              return
            end if

          end if

        end if
c
c  Exponential sample.
c
20      continue

        e = sexpo ( )
        u = 2.0E+00 * r4_uni_01 ( ) - 1.0E+00
        if ( u < 0.0E+00 ) then
          t = 1.8E+00 - abs ( e )
        else
          t = 1.8E+00 + abs ( e )
        end if

        if ( t .le. -0.6744E+00 ) then
          go to 20
        end if

        ignpoi = int ( mu + s * t )
        fk = real ( ignpoi )
        difmuk = mu - fk

        kflag = 1
c
c  Calculation of PX, PY, FX, FY.
c
        if ( ignpoi .lt. 10 ) then

          px = -mu
          py = mu ** ignpoi / fact(ignpoi+1)

        else

          del = 0.8333333E-01 / fk
          del = del - 4.8E+00 * del * del * del
          v = difmuk / fk

          if ( 0.25E+00 .lt. abs ( v ) ) then
            px = fk * alog ( 1.0E+00 + v ) - difmuk - del
          else
            px = fk * v * v * ((((((( a7
     &        * v + a6 )
     &        * v + a5 )
     &        * v + a4 )
     &        * v + a3 )
     &        * v + a2 )
     &        * v + a1 )
     &        * v + a0 ) - del
          end if

          py = 0.3989423E+00 / sqrt ( fk )

        end if

        x = ( 0.5E+00 - difmuk ) / s
        xx = x * x
        fx = -0.5E+00 * xx
        fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

        if ( kflag .le. 0 ) then

          if ( fy - u * fy .le. py * exp ( px - fx ) ) then
            return
          end if

        else

          if ( c * abs ( u ) .le. 
     &      py * exp ( px + e ) - fy * exp ( fx + e ) ) then
            return
          end if

        end if

        go to 20

      end if

      end
      function ignuin ( low, high )

c*********************************************************************72
c
cc IGNUIN generates a random integer in a given range.
c
c  Discussion:
c
c    Each deviate K satisfies LOW <= K <= HIGH.
c
c    If (HIGH-LOW) > 2,147,483,561, this procedure prints an error message 
c    and stops the program.
c
c    IGNLGI generates integers between 1 and 2147483562.
c
c    MAXNUM is 1 less than the maximum generatable value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, integer LOW, HIGH, the lower and upper bounds.
c
c    Output, integer IGNUIN, a random deviate from the distribution.
c
      implicit none

      integer err
      integer high
      integer i4_uni
      integer ign
      integer ignuin
      integer low
      integer maxnow
      integer maxnum
      parameter ( maxnum = 2147483561 )
      integer ranp1
      integer width

      if ( high .lt. low ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IGNUIN - Fatal error!'
        write ( *, '(a)' ) '  HIGH < LOW.'
        stop 1
      end if

      width = high - low

      if ( maxnum .lt. width ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IGNUIN - Fatal error!'
        write ( *, '(a)' ) '  Range HIGH-LOW is too large.'
        stop 1
      end if

      if ( low .eq. high ) then
        ignuin = low
        return
      end if

      ranp1 = width + 1
      maxnow = ( maxnum / ranp1 ) * ranp1

10    continue

      ign = i4_uni ( ) - 1

      if ( maxnow .lt. ign ) then
        go to 10
      end if

      ignuin = low + mod ( ign, ranp1 )

      return
      end
      function lennob ( s )

c*********************************************************************72
c
cc LENNOB counts the length of a string, ignoring trailing blanks.
c
c  Discussion:
c
c    This procedure returns the length of a string up to and including 
c    the last non-blank character.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, character * ( * ) S, the string.
c
c    Output, integer LENNOB, the length of the string to the last
c    nonblank.
c
      implicit none

      integer i
      integer lennob
      character * ( * ) s
      integer s_max

      s_max = len ( s )

      do i = s_max, 1, -1
        if ( s(i:i) .ne. ' ' ) then
          lennob = i
          return
        end if
      end do

      lennob = 0

      return
      end
      subroutine phrtsd ( phrase, seed1, seed2 )

c*********************************************************************72
c
cc PHRTST converts a phrase to a pair of random number generator seeds.
c
c  Discussion:
c
c    This procedure uses a character string to generate two seeds for the RGN
c    random number generator.
c
c    Trailing blanks are eliminated before the seeds are generated.
c
c    Generated seed values will fall in the range 1 to 2^30 = 1,073,741,824.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, character * ( * ) PHRASE, a phrase to be used for the
c    random number generation.
c
c    Output, integer SEED1, SEED2, the two seeds for the random number generator,
c    based on PHRASE.
c
      implicit none

      integer i
      integer ichr
      integer j
      integer lennob
      integer lphr
      character * ( * ) phrase
      integer seed1
      integer seed2
      integer shift(0:4)
      character * ( 86 ) table
      parameter ( table = 
     &  'abcdefghijklmnopqrstuvwxyz'//
     &  'ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     &  '0123456789'//
     &  '!@#$%^&*()_+[];:''"<>?,./' )
      integer twop30
      parameter ( twop30 = 1073741824 )
      integer values(5)

      save shift

      data shift / 1, 64, 4096, 262144, 16777216 /

      seed1 = 1234567890
      seed2 = 123456789

      lphr = lennob ( phrase )

      do i = 1, lphr

        ichr = index ( table, phrase(i:i) )
c
c  If the character does not occur, ICHR is returned as 0.
c
        ichr = mod ( ichr, 64 )

        if ( ichr .eq. 0 ) then
          ichr = 63
        end if

        do j = 1, 5
          values(j) = ichr - j
          if ( values(j) .lt. 1 ) then
            values(j) = values(j) + 63
          end if
        end do

        do j = 1, 5
          seed1 = mod ( seed1 + shift(j-1) * values(j), twop30 )
          seed2 = mod ( seed2 + shift(j-1) * values(6-j), twop30 )
        end do

      end do

      return
      end
      subroutine prcomp ( maxobs, p, mean, xcovar, answer )

c*********************************************************************72
c
cc PRCOMP prints covariance information.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, integer P, the number of variables.
c
c    Input, real MEAN(P), the mean for each column.
c
c    Input, real XCOVAR(P,P), the variance/covariance matrix.
c
c    Input, real ANSWER(MAXOBS,P), the observed values.
c
      implicit none

      integer p
      integer maxobs

      real answer(maxobs,p)
      real dum1
      real dum2
      integer i
      integer j
      real mean(p)
      real r4vec_covar
      real rcovar(p,p)
      real rmean(p)
      real rvar(p)
      real xcovar(p,p)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PRCOMP:'
      write ( *, '(a)' ) '  Print and compare covariance information'
      write ( *, '(a)' ) ' '

      do j = 1, p
        call stats ( answer(1,j), maxobs, rmean(j), rvar(j), 
     &    dum1, dum2 )
        write ( *, '(a,i4)' ) '  Variable Number ', j
        write ( *, '(a,g14.6,a,g14.6)' ) 
     &    '  Mean ', mean(j), ' Generated ', rmean(j)
        write ( *, '(a,g14.6,a,g14.6)' ) 
     &    '  Variance ', xcovar(j,j), ' Generated ', rvar(j)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Covariances:'
      write ( *, '(a)' ) ' '

      do i = 1, p
        do j = 1, i - 1
          write ( *, '(a,i4,a,i4)' ) '  I = ', i, ' J = ', j
          rcovar(i,j) = r4vec_covar ( maxobs, answer(1,i), answer(1,j) )
          write ( *, '(a,g14.6,a,g14.6)' ) 
     &      '  Covariance ', xcovar(i,j), ' Generated ', rcovar(i,j)
        end do
      end do

      return
      end
      function r4_exp ( x )

c*********************************************************************72
c
cc R4_EXP computes the exponential function, avoiding overflow and underflow.
c
c  Discussion:
c
c    For arguments of very large magnitude, the evaluation of the
c    exponential function can cause computational problems.  Some languages
c    and compilers may return an infinite value or a "Not-a-Number".  
c    An alternative, when dealing with a wide range of inputs, is simply
c    to truncate the calculation for arguments whose magnitude is too large.
c    Whether this is the right or convenient approach depends on the problem
c    you are dealing with, and whether or not you really need accurate
c    results for large magnitude inputs, or you just want your code to
c    stop crashing.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X, the argument of the exponential function.
c
c    Output, real R4_EXP, the value of exp ( X ).
c
      implicit none

      real r4_huge
      parameter ( r4_huge = 1.0E+30 )
      real r4_log_max
      parameter ( r4_log_max = +69.0776E+00 )
      real r4_log_min
      parameter ( r4_log_min = -69.0776E+00 )
      real r4_exp
      real x

      if ( x .le. r4_log_min ) then
        r4_exp = 0.0E+00
      else if ( x .lt. r4_log_max ) then
        r4_exp = exp ( x )
      else
        r4_exp = r4_huge
      end if

      return
      end
      function r4_exponential_sample ( lambda )

c*********************************************************************72
c
cc R4_EXPONENTIAL_SAMPLE samples the exponential PDF.
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
c    20 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real LAMBDA, the parameter of the PDF.
c
c    Output, real R4_EXPONENTIAL_SAMPLE, a sample of the PDF.
c
      implicit none

      real lambda
      real r
      real r4_exponential_sample
      real r4_uni_01

      r = r4_uni_01 ( )

      r4_exponential_sample = - log ( r ) * lambda

      return
      end
      function r4vec_covar ( n, x, y )

c*********************************************************************72
c
cc R4VEC_COVAR computes the covariance of two vectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2013
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, real X(N), Y(N), the two vectors.
c
c    Input, integer N, the dimension of the two vectors.
c
c    Output, real R4VEC_COVAR, the covariance of the two vectors.
c
      implicit none

      integer n

      integer i
      real r4vec_covar
      real value
      real x(n)
      real x_average
      real y(n)
      real y_average

      x_average = 0.0E+00
      do i = 1, n
        x_average = x_average + x(i)
      end do
      x_average = x_average / real ( n )

      y_average = 0.0E+00
      do i = 1, n
        y_average = y_average + y(i)
      end do
      y_average = y_average / real ( n )

      value = 0.0E+00
      do i = 1, n
        value = value + ( x(i) - x_average ) * ( y(i) - y_average )
      end do

      r4vec_covar = value / real ( n - 1 )

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
      double precision r8_uni_01

      r = r8_uni_01 ( )

      r8_exponential_sample = - log ( r ) * lambda

      return
      end
      function r8vec_covar ( n, x, y )

c*********************************************************************72
c
cc R8VEC_COVAR computes the covariance of two vectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision X(N), Y(N), the two vectors.
c
c    Input, integer N, the dimension of the two vectors.
c
c    Output, double precision R8VEC_COVAR, the covariance of the two vectors.
c
      implicit none

      integer n

      integer i
      double precision r8vec_covar
      double precision value
      double precision x(n)
      double precision x_average
      double precision y(n)
      double precision y_average

      x_average = 0.0D+00
      do i = 1, n
        x_average = x_average + x(i)
      end do
      x_average = x_average / dble ( n )

      y_average = 0.0D+00
      do i = 1, n
        y_average = y_average + y(i)
      end do
      y_average = y_average / dble ( n )

      value = 0.0D+00
      do i = 1, n
        value = value + ( x(i) - x_average ) * ( y(i) - y_average )
      end do

      r8vec_covar = value / dble ( n - 1 )

      return
      end
      function sdot ( n, sx, incx, sy, incy )

c*********************************************************************72
c
cc SDOT forms the dot product of two vectors.
c
c  Discussion:
c
c    This routine uses single precision real arithmetic.
c
c    This routine uses unrolled loops for increments equal to one.
c
c  Modified:
c
c    07 July 2007
c
c  Author:
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
c    Basic Linear Algebra Subprograms for FORTRAN usage,
c    ACM Transactions on Mathematical Software,
c    Volume 5, Number 3, pages 308-323, 1979.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vectors.
c
c    Input, real X(*), one of the vectors to be multiplied.
c
c    Input, integer INCX, the increment between successive entries of X.
c
c    Input, real Y(*), one of the vectors to be multiplied.
c
c    Input, integer INCY, the increment between successive elements of Y.
c
c    Output, real SDOT, the dot product of X and Y.
c
      implicit none

      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
      real sdot
      real stemp
      real sx(*)
      real sy(*)

      sdot = 0.0E+00

      if ( n .le. 0 ) then
        return
      end if

      stemp = 0.0E+00
c
c  Code for unequal increments or equal increments not equal to 1.
c
      if ( incx .ne. 1 .or. incy .ne. 1 ) then

        if ( incx .lt. 0 ) then
          ix = ( - n + 1 ) * incx + 1
        else
          ix = 1
        end if

        if ( incy .lt. 0 ) then
          iy = ( - n + 1 ) * incy + 1
        else
          iy = 1
        end if

        do i = 1, n
          stemp = stemp + sx(ix) * sy(iy)
          ix = ix + incx
          iy = iy + incy
        end do
c
c  Code for both increments equal to 1.
c
      else

        m = mod ( n, 5 )

        do i = 1, m
          stemp = stemp + sx(i) * sy(i)
        end do

        do i = m + 1, n, 5
          stemp = stemp 
     &      + sx(i)     * sy(i) 
     &      + sx(i + 1) * sy(i + 1) 
     &      + sx(i + 2) * sy(i + 2)
     &      + sx(i + 3) * sy(i + 3) 
     &      + sx(i + 4) * sy(i + 4)
        end do

      end if

      sdot = stemp

      return
      end
      subroutine setcov ( p, var, corr, covar )

c*********************************************************************72
c
cc SETCOV sets a covariance matrix from variance and common correlation.
c
c  Discussion:
c
c    This procedure sets the covariance matrix from the variance and 
c    common correlation.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, integer P, the number of variables.
c
c    Input, real VAR(P), the variances.
c
c    Input, real CORR, the common correlaton.
c
c    Output, real COVAR(P,P), the covariance matrix.
c
      implicit none

      integer p

      real corr
      real covar(p,p)
      integer i
      integer j
      real var(p)

      do i = 1, p
        do  j = 1, p
          if ( i .eq. j ) then
            covar(i,j) = var(i)
          else
            covar(i,j) = corr * sqrt ( var(i) * var(j) )
          end if
        end do
      end do

      return
      end
      subroutine setgmn ( meanv, covm, p, parm )

c*********************************************************************72
c
cc SETGMN sets data for the generation of multivariate normal deviates.
c
c  Discussion:
c
c    This procedure places P, MEANV, and the Cholesky factorization of 
c    COVM in GENMN.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real MEANV(P), the means of the multivariate normal distribution.
c
c    Input/output, real COVM(P,P).  On input, the covariance matrix of the
c    multivariate distribution.  On output, the information in COVM has been
c    overwritten.
c
c    Input, integer P, the number of dimensions.
c
c    Output, real PARM(P*(P+3)/2+1), parameters needed to generate 
c    multivariate normal deviates.
c
      implicit none

      integer p

      real covm(p,p)
      integer i
      integer icount
      integer info
      integer j
      real meanv(p)
      real parm(p*(p+3)/2+1)

      if ( p .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SETGMN - Fatal error!'
        write ( *, '(a)' ) '  P was not positive.'
        stop 1
      end if
c
c  Store P.
c
      parm(1) = p
c
c  Store MEANV.
c
      do i = 2, p + 1
        parm(i) = meanv(i-1)
      end do
c
c  Compute the Cholesky decomposition.
c
      call spofa ( covm, p, p, info )

      if ( info .ne. 0) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SETGMN - Fatal error!'
        write ( *, '(a)' ) '  SPOFA finds COVM not positive definite.'
        stop 1
      end if
c
c  Store the upper half of the Cholesky factor.
c
      icount = p + 1

      do i = 1, p
        do j = i, p
          icount = icount + 1
          parm(icount) = covm(i,j)
        end do
      end do

      return
      end
      function sexpo ( )

c*********************************************************************72
c
cc SEXPO samples the standard exponential distribution.
c                                                                      
c  Discussion:
c
c   This procedure corresponds to algorithm SA in the reference.   
c 
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c       
c  Modified:
c
c    21 March 2013
c        
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c      
c  Reference:
c                                                
c    Joachim Ahrens, Ulrich Dieter,
c    Computer Methods for Sampling From the
c    Exponential and Normal Distributions,
c    Communications of the ACM,
c    Volume 15, Number 10, October 1972, pages 873-882.           
c    
c  Parameters:
c
c    Output, real SEXPO, a random deviate from the standard exponential 
c    distribution.
c
      implicit none

      real a
      integer i
      real q(8)
      real r4_uni_01
      real sexpo
      real u
      real umin
      real ustar

      save q

      data q /
     &  0.6931472E+00,
     &  0.9333737E+00,
     &  0.9888778E+00,
     &  0.9984959E+00,
     &  0.9998293E+00,
     &  0.9999833E+00,
     &  0.9999986E+00,
     &  0.9999999E+00 /

      a = 0.0E+00
      u = r4_uni_01 ( )

10    continue

      u = u + u

      if ( u .le. 1.0E+00 ) then
        a = a + q(1)
        go to 10
      end if

      u = u - 1.0E+00

      if ( u .le. q(1) ) then
        sexpo = a + u
        return
      end if

      i = 1
      ustar = r4_uni_01 ( )
      umin = ustar

20    continue

      ustar = r4_uni_01 ( )
      umin = min ( umin, ustar )
      i = i + 1

      if ( q(i) .lt. u ) then 
        go to 20
      end if

      sexpo = a + umin * q(1)

      return
      end
      function sgamma ( a )

c*********************************************************************72
c
cc SGAMMA samples the standard Gamma distribution.
c
c  Discussion:
c
c    This procedure corresponds to algorithm GD in the reference.     
c                                                                      
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    Input, real A, the parameter of the standard gamma distribution.
c    0.0 < A < 1.0.
c
c    Output, real SGAMMA, a random deviate from the distribution.
c
      implicit none

      real a
      real a1
      real a2
      real a3
      real a4
      real a5
      real a6
      real a7
      real b
      real c
      real d
      real e
      real e1
      real e2
      real e3
      real e4
      real e5
      real p
      real q
      real q0
      real q1
      real q2
      real q3
      real q4
      real q5
      real q6
      real q7
      real r
      real r4_uni_01
      real s
      real s2
      real sexpo
      real si
      real sgamma
      real snorm
      real sqrt32
      real t
      real u
      real v
      real w
      real x

      save a1
      save a2
      save a3
      save a4
      save a5
      save a6
      save a7
      save e1
      save e2
      save e3
      save e4
      save e5
      save q1
      save q2
      save q3
      save q4
      save q5
      save q6
      save q7
      save sqrt32

      data a1 /  0.3333333E+00 /
      data a2 / -0.2500030E+00 /
      data a3 /  0.2000062E+00 /
      data a4 / -0.1662921E+00 /
      data a5 /  0.1423657E+00 /
      data a6 / -0.1367177E+00 /
      data a7 /  0.1233795E+00 /
      data e1 / 1.0E+00 /
      data e2 / 0.4999897E+00 /
      data e3 / 0.1668290E+00 /
      data e4 / 0.0407753E+00 /
      data e5 / 0.0102930E+00 /
      data q1 /  0.04166669E+00 /
      data q2 /  0.02083148E+00 /
      data q3 /  0.00801191E+00 /
      data q4 /  0.00144121E+00 /
      data q5 / -0.00007388E+00 /
      data q6 /  0.00024511E+00 /
      data q7 /  0.00024240E+00 /
      data sqrt32 / 5.656854E+00 /
c
c  Recalculations if A has changed.
c
      if ( 1.0E+00 .le. a ) then

        s2 = a - 0.5E+00
        s = sqrt ( s2 )
        d = sqrt32 - 12.0E+00 * s
c
c  Immediate acceptance.
c
        t = snorm ( )
        x = s + 0.5E+00 * t
        sgamma = x * x

        if ( 0.0E+00 .le. t ) then
          return
        end if
c
c  Squeeze acceptance.
c
        u = r4_uni_01 ( )
        if ( d * u .le. t * t * t ) then
          return
        end if
c
c  Possible recalculations.
c
        r = 1.0E+00 / a
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
        if ( 13.022E+00 .lt. a ) then
          b = 1.77E+00
          si = 0.75E+00
          c = 0.1515E+00 / s
        else if ( 3.686E+00 .lt. a ) then
          b = 1.654E+00 + 0.0076E+00 * s2
          si = 1.68E+00 / s + 0.275E+00
          c = 0.062E+00 / s + 0.024E+00
        else
          b = 0.463E+00 + s + 0.178E+00 * s2
          si = 1.235E+00
          c = 0.195E+00 / s - 0.079E+00 + 0.16E+00 * s
        end if
c
c  Quotient test.
c
        if ( 0.0E+00 .lt. x ) then

          v = 0.5E+00 * t / s

          if ( 0.25E+00 .lt. abs ( v ) ) then
            q = q0 - s * t + 0.25E+00 * t * t 
     &        + 2.0E+00 * s2 * alog ( 1.0E+00 + v )
          else
            q = q0 + 0.5E+00 * t * t * (((((( a7
     &        * v + a6 ) 
     &        * v + a5 )
     &        * v + a4 )
     &        * v + a3 )
     &        * v + a2 )
     &        * v + a1 ) 
     &        * v
          end if

          if ( alog ( 1.0E+00 - u ) .le. q ) then
            return
          end if

        end if

10      continue

        e = sexpo ( )
        u = 2.0E+00 * r4_uni_01 ( ) - 1.0E+00

        if ( 0.0E+00 .le. u ) then
          t = b + abs ( si * e )
        else
          t = b - abs ( si * e )
        end if
c
c  Possible rejection.
c
        if ( t .lt. -0.7187449E+00 ) then
          go to 10
        end if
c
c  Calculate V and quotient Q.
c
        v = 0.5E+00 * t / s

        if ( 0.25E+00 .lt. abs ( v ) ) then
          q = q0 - s * t + 0.25E+00 * t * t 
     &      + 2.0E+00 * s2 * alog ( 1.0E+00 + v )
        else
          q = q0 + 0.5E+00 * t * t * (((((( a7
     &      * v + a6 )
     &      * v + a5 ) 
     &      * v + a4 ) 
     &      * v + a3 )
     &      * v + a2 )
     &      * v + a1 )
     &      * v
        end if
c
c  Hat acceptance.
c
        if ( q .le. 0.0E+00 ) then
          go to 10
        end if

        if ( 0.5E+00 .lt. q ) then
          w = exp ( q ) - 1.0E+00
        else
          w = (((( e5 * q + e4 ) * q + e3 ) * q + e2 ) * q + e1 ) * q
        end if
c
c  May have to sample again.
c
        if ( w * exp ( e - 0.5E+00 * t * t ) .lt. c * abs ( u ) ) then
          go to 10
        end if

        x = s + 0.5E+00 * t
        sgamma = x * x
        return
c
c  Method for A < 1.
c
      else

        b = 1.0E+00 + 0.3678794E+00 * a

20      continue

        p = b * r4_uni_01 ( )

        if ( p .lt. 1.0E+00 ) then

          sgamma = exp ( alog ( p ) / a )

          if ( sgamma .le. sexpo ( ) ) then
            return
          end if

          go to 20

        end if

        sgamma = - alog ( ( b - p ) / a )

        if ( sexpo ( ) .lt. ( 1.0E+00 - a ) * alog ( sgamma ) ) then
          go to 20
        end if

      end if

      return
      end
      function snorm ( )

c*********************************************************************72
c        
cc SNORM samples the standard normal distribution.
c
c  Discussion:
c
c    This procedure corresponds to algorithm FL, with M = 5, in the reference.
c 
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c         
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
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
c    Output, real SNORM, a random deviate from the distribution.
c                                                    
      implicit none
                                                      
      real a(32)
      real aa
      real d(31)
      real h(31)
      integer i
      real r4_uni_01
      real s
      real snorm
      real t(31)
      real tt
      real u
      real ustar
      real w
      real y

      save a
      save d
      save h
      save t

      data a /
     &  0.0000000E+00, 0.3917609E-01, 0.7841241E-01, 0.1177699E+00,
     &  0.1573107E+00, 0.1970991E+00, 0.2372021E+00, 0.2776904E+00, 
     &  0.3186394E+00, 0.3601299E+00, 0.4022501E+00, 0.4450965E+00,
     &  0.4887764E+00, 0.5334097E+00, 0.5791322E+00, 0.6260990E+00,
     &  0.6744898E+00, 0.7245144E+00, 0.7764218E+00, 0.8305109E+00,
     &  0.8871466E+00, 0.9467818E+00, 1.009990E+00,  1.077516E+00,
     &  1.150349E+00,  1.229859E+00,  1.318011E+00,  1.417797E+00,
     &  1.534121E+00,  1.675940E+00,  1.862732E+00,  2.153875E+00 /

      data d /
     &  0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 
     &  0.0000000E+00, 0.2636843E+00, 0.2425085E+00, 0.2255674E+00, 
     &  0.2116342E+00, 0.1999243E+00, 0.1899108E+00, 0.1812252E+00, 
     &  0.1736014E+00, 0.1668419E+00, 0.1607967E+00, 0.1553497E+00,
     &  0.1504094E+00, 0.1459026E+00, 0.1417700E+00, 0.1379632E+00, 
     &  0.1344418E+00, 0.1311722E+00, 0.1281260E+00, 0.1252791E+00, 
     &  0.1226109E+00, 0.1201036E+00, 0.1177417E+00, 0.1155119E+00,
     &  0.1134023E+00, 0.1114027E+00, 0.1095039E+00 /

      data h /
     &  0.3920617E-01, 0.3932705E-01, 0.3950999E-01, 0.3975703E-01,
     &  0.4007093E-01, 0.4045533E-01, 0.4091481E-01, 0.4145507E-01, 
     &  0.4208311E-01, 0.4280748E-01, 0.4363863E-01, 0.4458932E-01,
     &  0.4567523E-01, 0.4691571E-01, 0.4833487E-01, 0.4996298E-01, 
     &  0.5183859E-01, 0.5401138E-01, 0.5654656E-01, 0.5953130E-01, 
     &  0.6308489E-01, 0.6737503E-01, 0.7264544E-01, 0.7926471E-01,
     &  0.8781922E-01, 0.9930398E-01, 0.1155599E+00, 0.1404344E+00, 
     &  0.1836142E+00, 0.2790016E+00, 0.7010474E+00 /

      data t /
     &  0.7673828E-03, 0.2306870E-02, 0.3860618E-02, 0.5438454E-02,
     &  0.7050699E-02, 0.8708396E-02, 0.1042357E-01, 0.1220953E-01,
     &  0.1408125E-01, 0.1605579E-01, 0.1815290E-01, 0.2039573E-01, 
     &  0.2281177E-01, 0.2543407E-01, 0.2830296E-01, 0.3146822E-01,
     &  0.3499233E-01, 0.3895483E-01, 0.4345878E-01, 0.4864035E-01, 
     &  0.5468334E-01, 0.6184222E-01, 0.7047983E-01, 0.8113195E-01,
     &  0.9462444E-01, 0.1123001E+00, 0.1364980E+00, 0.1716886E+00,
     &  0.2276241E+00, 0.3304980E+00, 0.5847031E+00 /

      u = r4_uni_01 ( )
      if ( u .le. 0.5E+00 ) then
        s = 0.0E+00
      else
        s = 1.0E+00
      end if
      u = 2.0E+00 * u - s
      u = 32.0E+00 * u
      i = int ( u )
      if ( i .eq. 32 ) then
        i = 31
      end if
c
c  Center
c
      if ( i .ne. 0 ) then

        ustar = u - real ( i )
        aa = a(i)

10      continue

        if ( t(i) .lt. ustar ) then

          w = ( ustar - t(i) ) * h(i)

          y = aa + w
 
          if ( s .ne. 1.0E+00 ) then
            snorm = y
          else
            snorm = -y
          end if

          return

        end if

        u = r4_uni_01 ( )
        w = u * ( a(i+1) - aa )
        tt = ( 0.5E+00 * w + aa ) * w

20      continue

        if ( tt .lt. ustar ) then
          y = aa + w
          if ( s .ne. 1.0E+00 ) then
            snorm = y
          else
            snorm = -y
          end if
          return
        end if

        u = r4_uni_01 ( )
        
        if ( u .le. ustar ) then
          tt = u
          ustar = r4_uni_01 ( )
          go to 20
        end if

        ustar = r4_uni_01 ( )
        go to 10
c
c  Tail
c
      else

        i = 6
        aa = a(32)

30      continue

        u = u + u

        if ( u .lt. 1.0E+00 ) then
          aa = aa + d(i)
          i = i + 1
          go to 30
        end if

        u = u - 1.0E+00
        w = u * d(i)
        tt = ( 0.5E+00 * w + aa ) * w

40      continue

        ustar = r4_uni_01 ( )

        if ( tt .lt. ustar ) then
          y = aa + w
          if ( s .ne. 1.0E+00 ) then
            snorm = y
          else
            snorm = -y
          end if
          return
        end if

        u = r4_uni_01 ( )

        if ( u .le. ustar ) then
          tt = u
        else
          u = r4_uni_01 ( )
          w = u * d(i)
          tt = ( 0.5E+00 * w + aa ) * w
        end if

        go to 40

      end if

      end
      subroutine spofa ( a, lda, n, info )

c*********************************************************************72
c
cc SPOFA factors a real symmetric positive definite matrix.
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
c    Cleve Moler
c
c  Parameters:
c
c    Input/output, real A(LDA,N).  On input, the symmetric matrix to be factored.
c    Only the diagonal and upper triangle are accessed.  On output, the strict
c    lower triangle has not been changed.  The diagonal and upper triangle contain
c    an upper triangular matrix R such that A = R' * R.  If INFO is nonzero,
c    the factorization was not completed.
c
c    Input, integer LDA, the leading dimension of the array A.
c    N <= LDA.
c
c    Input, integer N, the order of the matrix.
c
c    Output, integer INFO, error flag.
c    0, no error was detected.
c    K, the leading minor of order K is not positive definite.
c
      implicit none

      integer lda
      integer n

      real a(lda,n)
      integer info
      integer j
      integer jm1
      integer k
      real s
      real sdot
      real t

      info = 0

      do j = 1, n
        info = j
        s = 0.0E+00
        jm1 = j - 1
        do k = 1, jm1
          t = a(k,j) - sdot ( k-1, a(1,k), 1, a(1,j), 1 )
          t = t / a(k,k)
          a(k,j) = t
          s = s + t * t
        end do
        s = a(j,j) - s
        if ( s .le. 0.0E+00 ) then
          info = j
          return
        end if
        a(j,j) = sqrt ( s )
      end do

      return
      end
      subroutine stats ( x, n, av, var, xmin, xmax )

c*********************************************************************72
c
cc STATS computes statistics for a given array.
c
c  Discussion:
c
c    This procedure computes the average and variance of an array.
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
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, real X(N), the array to be analyzed.
c
c    Input, integer N, the dimension of the array.
c
c    Output, real AV, the average value.
c
c    Output, real VAR, the variance.
c
c    Output, real XMIN, XMAX, the minimum and maximum entries.
c
      implicit none

      integer n

      real av
      integer i
      real total
      real var
      real x(n)
      real xmax
      real xmin

      xmin = x(1)
      xmax = x(1)
      total = 0.0E+00
      do i = 1, n
        total = total + x(i)
        xmin = min ( xmin, x(i) )
        xmax = max ( xmax, x(i) )
      end do

      av = total / real ( n )

      total = 0.0E+00
      do i = 1, n
        total = total + ( x(i) - av ) ** 2
      end do
      var = total / real ( n - 1 )

      return
      end
      subroutine trstat ( pdf, parin, av, var )

c*********************************************************************72
c
cc TRSTAT returns the mean and variance for distributions.
c
c  Discussion:
c
c    This procedure returns the mean and variance for a number of statistical 
c    distributions as a function of their parameters.
c
c    The input vector PARIN is used to pass in the parameters necessary
c    to specify the distribution.  The number of these parameters varies
c    per distribution, and it is necessary to specify an ordering for the
c    parameters used to a given distribution.  The ordering chosen here
c    is as follows:
c
c    bet
c      PARIN(1) is A
c      PARIN(2) is B
c    bin
c      PARIN(1) is Number of trials
c      PARIN(2) is Prob Event at Each Trial
c    chi
c      PARIN(1) = df
c    exp
c      PARIN(1) = mu
c    f
c      PARIN(1) is df numerator
c      PARIN(2) is df denominator
c    gam
c      PARIN(1) is A
c      PARIN(2) is R
c    nbn
c      PARIN(1) is N
c      PARIN(2) is P
c    nch
c      PARIN(1) is df
c      PARIN(2) is noncentrality parameter
c    nf
c      PARIN(1) is df numerator
c      PARIN(2) is df denominator
c      PARIN(3) is noncentrality parameter
c    nor
c      PARIN(1) is mean
c      PARIN(2) is standard deviation
c    poi
c      PARIN(1) is Mean
c    unf
c      PARIN(1) is LOW bound
c      PARIN(2) is HIGH bound
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt.
c
c  Parameters:
c
c    Input, character * ( 4 ) PDF, indicates the distribution:
c    'bet'  beta distribution
c    'bin'  binomial
c    'chi'  chisquare
c    'exp'  exponential
c    'f'    F (variance ratio)
c    'gam'  gamma
c    'nbn'  negative binomial
c    'nch'  noncentral chisquare
c    'nf'   noncentral f
c    'nor'  normal
c    'poi'  Poisson
c    'unf'  uniform
c
c    Input, real PARIN(*), the parameters of the distribution.
c
c    Output, real AV, the mean of the specified distribution.
c
c    Output, real VAR, the variance of the specified distribuion.
c
      implicit none

      real a
      real av
      real b
      integer n
      real p
      real parin(*)
      character * ( 4 ) pdf
      real r
      real var
      real width

      if ( pdf .eq. 'bet' ) then

        av = parin(1) / ( parin(1) + parin(2) )
        var = ( av * parin(2) ) / ( ( parin(1) + parin(2) ) *
     &    ( parin(1) + parin(2) + 1.0E+00 ) )

      else if ( pdf .eq. 'bin' ) then

        n = int ( parin(1) )
        p = parin(2)
        av = real ( n ) * p
        var = real ( n ) * p * ( 1.0E+00 - p )

      else if ( pdf .eq. 'chi' ) then

        av = parin(1)
        var = 2.0E+00 * parin(1)

      else if ( pdf .eq. 'exp' ) then

        av = parin(1)
        var = av ** 2

      else if ( pdf .eq. 'f' ) then

        if ( parin(2) .le. 2.0001E+00 ) then
          av = -1.0E+00
        else
          av = parin(2) / ( parin(2) - 2.0E+00 )
        end if

        if ( parin(2) .le. 4.0001E+00 ) then
          var = -1.0E+00
        else
          var = ( 2.0E+00 * parin(2) ** 2 
     &      * ( parin(1) + parin(2) - 2.0E+00 ) ) /
     &      ( parin(1) * ( parin(2) - 2.0E+00 ) ** 2 
     &      * ( parin(2) - 4.0E+00 ) )
        end if

      else if ( pdf .eq. 'gam' ) then
  
        a = parin(1)
        r = parin(2)
        av = r / a
        var = r / a ** 2

      else if ( pdf .eq. 'nbn' ) then

        n = int ( parin(1) )
        p = parin(2)
        av = n * ( 1.0E+00 - p ) / p
        var = n * ( 1.0E+00 - p ) / p ** 2

      else if ( pdf .eq. 'nch' ) then
  
        a = parin(1) + parin(2)
        b = parin(2) / a
        av = a
        var = 2.0E+00 * a * ( 1.0E+00 + b )

      else if ( pdf .eq. 'nf' ) then

        if ( parin(2) .le. 2.0001E+00 ) then
          av = -1.0E+00
        else
          av = ( parin(2) * ( parin(1) + parin(3) ) ) 
     &      / ( ( parin(2) - 2.0E+00 ) * parin(1) )
        end if

        if ( parin(2) .le. 4.0001E+00 ) then
          var = -1.0E+00
        else
          a = ( parin(1) + parin(3) ) ** 2 
     &      + ( parin(1) + 2.0E+00 * parin(3) ) * ( parin(2) - 2.0E+00 )
          b = ( parin(2) - 2.0E+00 ) ** 2 * ( parin(2) - 4.0E+00 )
          var = 2.0E+00 * ( parin(2) / parin(1) ) ** 2 * ( a / b )
        end if

      else if ( pdf .eq. 'nor' ) then

        av = parin(1)
        var = parin(2) ** 2

      else if ( pdf .eq. 'poi' ) then

        av = parin(1)
        var = parin(1)

      else if ( pdf .eq. 'unf' ) then

        width = parin(2) - parin(1)
        av = parin(1) + width / 2.0E+00
        var = width ** 2 / 12.0E+00

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRSTAT - Fatal error!'
        write ( *, '(a)' ) '  Illegal input value for PDF.'
        stop 1

      end if

      return
      end
