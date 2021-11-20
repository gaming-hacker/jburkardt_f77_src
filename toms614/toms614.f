!     algorithm 614 collected algorithms from acm.
!     algorithm appeared in acm-trans. math. software, vol.10, no. 2,
!     jun., 1984, p. 152-160.
!
      subroutine inthp ( a, b, d, f, m, p, eps, inf, quadr )
!
!***********************************************************************
!
!! INTHP computes the integral of functions which
!     may have singularities at one or both end-points of an
!     interval (a,b), see [1, 2]. 
!
!  Four quadrature routines are included: 
!
!  * one over a finite interval (a,b),
!  * one over (a,+infinity) for nonoscillatory integrands; 
!  * one over (a,+infinity) for oscillatory integrands; 
!  * one over (-infinity,+infinity).
!
!  the user supplies the integrand function, the interval and the 
!  relative error to which the integral is to be evaluated.
!
!  The formulas are optimal in certain hardy spaces h(p,dd),
!  see [1, 2].  Here dd is an open domain in the complex plane,
!  a and b belong to the boundary of dd and h(p,dd), p.gt.1, is
!  the set of all analytic functons in dd whose p-th norm defi-
!     ned as in [2] is finite.
!
!        if the user is unable to specify the parameters p and d
!     of the space h(p,dd) to which his integrand belongs, the
!     algorithm terminates according to a heuristic criterion, see
!     [2] and comments to eps.
!
!        if the user can specify the parameters p and d of the
!     space h(p,dd) to which his integrand belongs, the algorithm
!     terminates with an answer having a guaranteed accuracy ( de-
!     teministic criterion, see [1, 2] and comments to eps).
!
!     input parameters
!
!    Input, real A, the lower limit of integration.  See comments to INF.
!
!    Input, real B, the upper limit of integration.  See comments to INF.
!
!    Input, real D, a parameter of the class H(P,DD) (see comments to inf).
!
!         user sets d:
!
!         heuristic termination
!       = any real number
!
!         deterministic termination
!       = a number in the range 0.lt.d.le.pi/2.
!
!     f = a name of an external integrand function to be
!         supplied by the user. f(x) computes the value of
!         a function f at a point x. the statement
!         ...external f... must appear in the main program.
!
!     m = maximal number of function evaluations allowed in
!         the computations, m.ge.3.( altered on exit ).
!
!     p = 0, 1, .gt.1  a parameter of the class h(p,dd).
!
!         user sets p:
!       = 0 - heuristic termination.
!       = 1 - deterministic termination with the infinity
!             norm.
!      .gt.1 -deterministic termination with the p-th norm.
!
!   eps = a real number - the relative error bound - see
!         remarks below. ( altered on exit ).
!
!   inf = 1, 2, 3, 4 - information parameter. ( altered on exit ).
!
!       = 1 signifies an infinite interval (a,b)=real line,
!           a and b any numbers.
!           deterministic termination -
!           dd=strip(z:abs(im(z)).lt.d).
!
!       = 2 signifies a semi-infinite interval (a, +infinity)
!           user supplies a, b any number.
!           quadrature suited to non-oscillatory integrands.
!           deterministic termination -
!           dd=sector(z:abs(arg(z-a)).lt.d).
!
!       = 3 signifies a semi infinite interval (a,+infinity)
!           user supplies a, b any number.
!           quadrature suited to oscillatory integrands.
!           deterministic termination -
!           dd=region(z:abs(arg(sinh(z-a))).lt.d).
!
!       = 4 signifies a finite interval (a,b).
!           user supplies a and b.
!           deterministic termination -
!           dd=lens region(z:abs(arg((z-a)/(b-z))).lt.d).
!
!
!     output parameters
!
!
!     m = the number of function evaluations used in the
!         quadrature.
!
!   eps = the relative error bound (see remarks below).
!
!         deterministic termination
!
!       = the relative error rexa bound, i.e.,
!                 rexa(f,m(output)) .le. eps.
!
!         heuristic termination
!
!       = max(eps(input),machep).
!
!   inf = 0, 1 - deterministic termination
!
!       = 0 computed quadrature qcom(f,m(eps)), see remarks below.
!
!       = 1 computed quadrature qcom(f,m1), see remarks below.
!
!   inf = 2, 3, 4 - heuristic termination.
!
!       = 2 integration completed with eps=max(eps(input),
!           machep). we can expect the relative error
!           rexa to be of the order of eps (for some p.ge.1).
!
!       = 3 integration not completed. attempt to exceed the
!           maximal allowed number of function evaluations m.
!           truncation conditions (see [2]) satisfied. quadr
!           set to be equal to the last trapezoidal appro-
!           ximation. it is likely that quadr approximates the
!           integral if m is large.
!
!       = 4 integration not completed. attempt to exceed the
!           maximal allowed number of function evaluations m.
!           truncation conditions (see [2]) not satisfied.
!           quadr set to be equal to the computed trapezoidal
!           approximation. it is unlikely that quadr approximates
!           the integral.
!
!   inf = 10, 11, 12, 13 - incorrect input
!
!       = 10  m.lt.3.
!
!       = 11  p does not satisfy p=0, p=1 or p.gt.1 or in the
!             case of deterministic termination d does not
!             satisfy 0.lt.d.le.pi/2.
!
!       = 12  a.ge.b in case of a finite interval.
!
!       = 13  inf not equal to 1, 2, 3, or 4.
!
!
!   quadr = the computed value of quadrature.
!
!     remarks:
!
!         let  qexa(f,m)  ( qcom(f,m) ) be the exact (computed)
!         value of the quadrature with m function evaluations,
!         and let  rexa(f,m) ( rcom(f,m) ) be the relative error
!         of qexa (qcom) ,i.e.,
!            rexa(f,m)=abs(integral(f)-qexa(f,m))/norm(f),
!            rcom(f,m)=abs(integral(f)-qcom(f,m))/norm(f),
!         with the notation 0/0=0.
!             due to the roundoff one cannot expect the error
!         rcom to be less than the relative machine precision
!         machep. therefore the input value of eps is changed
!         according to the formula
!                   eps=max(eps,machep).
!
!         deterministic termination case
!
!             the number of functon evaluations m(eps) is computed
!         so that the error rexa is no greater than eps,i.e.,
!
!         (*)     rexa(f,m(eps)) .le. eps .
!
!         if m(eps).le.m then the quadrature qcom(f,m(eps)) is com-
!         puted. otherwise, which means that eps is too small with
!         respect to m, the quadrature qcom(f,m1) is computed, where
!         m1=2*int((m-1)/2)+1. in this case eps is changed to the
!         smallest number for which the estimate (*) holds with
!         m(eps)=m1 function evaluations.
!
!         heuristic termination case
!
!             we can expect the relative error rexa to be of the
!         order of eps, see [2]. if eps is too small with respect
!         to m then the quadrature qcom(f,m) is computed.
!
!         roundoff errors
!
!             in both deterministic and heuristic cases the round-
!         off error
!                    roff=abs(qexa(f,m)-qcom(f,m))
!         can be estimated by
!
!         (**)       roff .le. 3*c1*r*machep,
!
!         where  r=qcom(abs(f),m)+(1+2*c2)/3*sum(w(i),i=1,2,...m)
!         and c1 is of the order of unity, c1=1/(1-3*machep), w(i)
!         are the weights of the quadrature, see [2], and c2 is
!         a constant estimating the accuracy of computing function
!         values, i.e.,
!               abs(exact(f(x))-computed(f(x))).le.c2*machep.
!         if the integrand values are computed inaccurately, i.e.,
!         c2 is large, then the estimate (**) is large and one can
!         expect the actual error roff to be large. numerical tests
!         indicate that this happens especially when the integrand
!         is evaluated inaccurately near a singularity. the ways of
!         circumventing such pitfalls are explained in [2].
!
!     references:
!
!     [1] sikorski,k., optimal quadrature algorithms in hp
!            spaces, num. math., 39, 405-410 (1982).
!
!     [2] sikorski,k., stenger,f., optimal quadratures in
!            hp spaces, acm toms.
!
      integer i, i1, inf, k, l, l1, m, m1, m2, n, n1
      real a, alfa, b, ba, c, c0, cor, d, e1, eps, eps3, exph, exph0,
     & f, h, h0
      real h1, p, pi, quadr, s, s1, sr, sq2, sum, sum1, sum2, u, t, v,
     & v0, v1
      real v2, w, w1, w2, w3, w4
      logical inf1, inf2
!
      external f
!
      pi = 4.*atan(1.0)
!
!  Check the input data
!
      if (inf.ne.1 .and. inf.ne.2 .and. inf.ne.3 .and. inf.ne.4) go to
     & 300
      if (m.lt.3) go to 270
      if (p.lt.1. .and. p.ne.0.) go to 280
      if (p.ge.1. .and. (d.le.0. .or. d.gt.pi/2.)) go to 280
      if (inf.eq.4 .and. a.ge.b) go to 290
!
      sq2 = sqrt(2.0)
      i1 = inf - 2
      ba = b - a
      n1 = 0
!
!     compute the relative machine precision and check
!     the value of eps.  caution...this loop may not work on a
!     machine that has an accurated arithmetic process compared
!     to the storage precision.  the value of u may need to be
!     simply defined as the relative accuracy of storage precision.
!
      u = 1.
   10 u = u/10.
      t = 1. + u
      if (1..ne.t) go to 10
      u = u*10.
      if (eps.lt.u) eps = u

      if (p.eq.0.) go to 40
!
!     set up data for the deterministic termination
!
      if (p.eq.1.) alfa = 1.
      if (p.gt.1.) alfa = (p-1.)/p
      c = 2.*pi/(1.-1./exp(pi*sqrt(alfa))) + 4.**alfa/alfa
      w = alog(c/eps)
      w1 = 1./(pi*pi*alfa)*w*w
      n = int(w1)
      if (w1.gt.float(n)) n = n + 1
      if (w1.eq.0.) n = 1
      n1 = 2*n + 1
      sr = sqrt(alfa*float(n))
      if (n1.le.m) go to 20
!
!     eps too small with respect to m. compute the new eps
!     guaranteed by the value of m.
!
      n1 = 1
      n = int(float((m-1)/2))
      sr = sqrt(alfa*float(n))
      m = 2*n + 1
      eps = c/exp(pi*sr)
      go to 30
!
   20 m = n1
      n1 = 0
   30 h = 2.*d/sr
      sum2 = 0.
      l1 = n
      k = n
      inf1 = .false.
      inf2 = .false.
      h0 = h
      go to 50
!
!  Set up data for the heuristic termination.
!
   40 h = 1.
      h0 = 1.
      eps3 = eps/3.
      sr = sqrt(eps)
      v1 = eps*10.
      v2 = v1
      m1 = m - 1
      n = int(float(m1/2))
      m2 = n
      l1 = 0
      inf1 = .true.
      inf2 = .false.
!
!  Initialize the quadrature.
!
   50 i = 0
      if (inf.eq.1) sum = f(0.0)
      if (inf.eq.2) sum = f(a+1.)
      if (inf.eq.3) sum = f(a+alog(1.+sq2))/sq2
      if (inf.eq.4) sum = f((a+b)/2.)/4.*ba
!
!  Compute weights, nodes and function values.
!
   60 exph = exp(h)
      exph0 = exp(h0)
      h1 = h0
      e1 = exph0
      u = 0.
      cor = 0.

   70 if (i1) 80, 90, 100

   80 v = f(h1)
      h1 = h1 + h
      go to 150

   90 v = e1*f(a+e1)
      e1 = e1*exph
      go to 150

  100 if (inf.eq.4) go to 140
      w1 = sqrt(e1+1./e1)
      w2 = sqrt(e1)
      if (e1.lt.0.1) go to 110
      s = alog(e1+w1*w2)
      go to 130
  110 w3 = e1
      w4 = e1*e1
      c0 = 1.
      s = e1
      s1 = e1
      t = 0.
  120 c0 = -c0*(0.5+t)*(2.*t+1.)/(2.*t+3.)/(t+1.)
      t = t + 1.
      w3 = w3*w4
      s = s + c0*w3
      if (s.eq.s1) go to 130
      s1 = s
      go to 120
  130 v = w2/w1*f(a+s)
      e1 = e1*exph
      go to 150
!
  140 w1 = e1 + 1.0
      v = e1/w1/w1*f((a+b*e1)/w1)*ba
      e1 = e1*exph
!
!  Summation algorithm.
!
  150 i = i + 1
      sum1 = u + v
      if (abs(u).lt.abs(v)) go to 160
      cor = v - (sum1-u) + cor
      go to 170
  160 cor = u - (sum1-v) + cor
  170 u = sum1
      if (i.lt.l1) go to 70
!
!     switch to check truncation condition ( heuristic
!     termination)
!
      if (inf1) go to 190
!
!     switch to compute the midordinate approximation
!     ( heuristic termination ) or to stop ( determinis-
!     tic termination)
!
      if (inf2) go to 210
!
!  Set up parameters to continue summation.
!
      l1 = k
  180 inf2 = .true.
      i = 0.
      exph = 1./exph
      h0 = -h0
      e1 = 1./exph0
      h1 = h0
      h = -h
      go to 70
!
!  Truncation condition.
!
  190 v0 = v1
      v1 = v2
      v2 = abs(v)
      if (v0+v1+v2.le.eps3) go to 200
      if (i.lt.m2) go to 70
      n1 = 5
  200 if (inf2) k = i
      if (.not.inf2) l = i
      v1 = 10.*eps
      v2 = v1
      m2 = m1 - l
      if (.not.inf2) go to 180
!
!     n1=5 - truncation condition not satisfied
!
      if (n1.eq.5) go to 260
!
!     truncation condition satisfied, sum2=trapezoidal
!     approximation
!
      sum2 = sum1 + cor + sum
      m2 = 2*(k+l)
!
!     check the number of function evaluations
!
      if (m2.gt.m1) go to 240
!
!     initialize iteration
!
      inf1 = .false.
      inf2 = .false.
      l1 = l
      i = 0
      h = -h
      h0 = h/2.
      go to 60
!
!     p.ge.1 = deterministic termination
!
  210 if (p.ge.1.) go to 220
!
!     compute the midordinate approximation sum1
!
      h = -h
      sum1 = (sum1+cor)*h
      w1 = (sum1+sum2)/2.
!
!     termination condition
!
      if (abs(sum1-sum2).le.sr) go to 230
!
!     set up data for the next iteration
!
      m2 = 2*m2
      if (m2.gt.m1) go to 250
      i = 0
      k = 2*k
      l = 2*l
      l1 = l
      h = h/2.
      h0 = h/2.
      sum2 = w1
      inf2 = .false.
      go to 60
!
!     final results
!
  220 quadr = -h*(sum1+cor+sum)
      inf = n1
      return

  230 quadr = w1
      inf = 2
      m = m2 + 1
      return

  240 quadr = sum2
      inf = 3
      m = k + l + 1
      return

  250 quadr = w1
      inf = 3
      m = m2/2 + 1
      return

  260 quadr = u + cor + sum
      inf = 4
      m = k + l + 1
      return

  270 inf = 10
      return

  280 inf = 11
      return

  290 inf = 12
      return

  300 inf = 13
      return
      end
