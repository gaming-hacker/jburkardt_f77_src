      program main

c*********************************************************************72
c
cc MAIN is the main program for QUADPACK_PRB.
c
c  Discussion:
c
c    QUADPACK_PRB tests the QUADPACK library.
c
c  Modified:
c
c    12 September 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUADPACK_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the QUADPACK library.'

      call d1mach_test ( )
      call i1mach_test ( )

      call dqag_test ( )
      call dqagi_test ( )
      call dqagp_test ( )
      call dqags_test ( )
      call dqawc_test ( )
      call dqawf_test ( )
      call dqawo_test ( )
      call dqaws_test ( )
      call dqk15_test ( )
      call dqk21_test ( )
      call dqk31_test ( )
      call dqk41_test ( )
      call dqk51_test ( )
      call dqk61_test ( )
      call dqng_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUADPACK_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )
     
      stop
      end
      subroutine d1mach_test ( )

c*********************************************************************72
c
cc D1MACH_TEST reports the constants returned by D1MACH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 March 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision d1mach

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'D1MACH_TEST'
      write ( *, '(a)' ) '  D1MACH returns constants associated with'
      write ( *, '(a)' ) '  real double precision computer arithmetic.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Assume that double precision numbers are'
      write ( *, '(a)' ) 
     &  '  stored with a mantissa of T digits in base B,'
      write ( *, '(a)' ) '  with an exponent whose value is between '
      write ( *, '(a)' ) '  EMIN and EMAX.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For input arguments of 1 <= I <= 5,'
      write ( *, '(a)' ) '  D1MACH will return the following values:'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  D1MACH(1) = B^(EMIN-1), the smallest positive magnitude.'
      write ( *, * ) d1mach(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  D1MACH(2) = B^EMAX*(1-B^(-T)), the largest magnitude.'
      write ( *, * ) d1mach(2)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  D1MACH(3) = B^(-T), the smallest relative spacing.'
      write ( *, * ) d1mach(3)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  D1MACH(4) = B^(1-T), the largest relative spacing.'
      write ( *, * ) d1mach(4)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  D1MACH(5) = log10(B).'
      write ( *, * ) d1mach(5)

      return
      end
      subroutine i1mach_test ( )

c*********************************************************************72
c
cc I1MACH_TEST reports the constants returned by I1MACH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 March 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i1mach

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I1MACH_TEST'
      write ( *, '(a)' ) '  I1MACH returns constants associated with'
      write ( *, '(a)' ) '  integer computer arithmetic, as well as'
      write ( *, '(a)' ) '  integers associated with real or double'
      write ( *, '(a)' ) '  precision calculations, and input/output.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Numbers associated with input/output units:'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(1) = the standard input unit.'
      write ( *,     * ) i1mach(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(2) = the standard output unit.'
      write ( *,     * ) i1mach(2)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(3) = the standard punch unit.'
      write ( *,     * ) i1mach(3)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  I1MACH(4) = the standard error message unit.'
      write ( *,     * ) i1mach(4)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Numbers associated with words:'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(5) = the number of bits per integer.'
      write ( *,     * ) i1mach(5)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  I1MACH(6) = the number of characters per integer.'
      write ( *,     * ) i1mach(6)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Numbers associated with integer values:'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Assume integers are represented in the S digit '
      write ( *, '(a)' ) '  base A form:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Sign * (X(S-1)*A^(S-1) + ... + X(1)*A + X(0))'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  where the digits X satisfy 0 <= X(1:S-1) < A.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(7) = A, the base.'
      write ( *,     * ) i1mach(7)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(8) = S, the number of base A digits.'
      write ( *,     * ) i1mach(8)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(9) = A^S-1, the largest integer.'
      write ( *,     * ) i1mach(9)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Numbers associated with floating point values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Assume floating point numbers are represented '
      write ( *, '(a)' ) '  in the T digit base B form:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Sign * (B^E) * ((X(1)/B) + ... + (X(T)/B^T) )'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  where '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    0 <= X(1:T) < B,'
      write ( *, '(a)' ) 
     &  '    0 < X(1) (unless the value being represented is 0),'
      write ( *, '(a)' ) '    EMIN <= E <= EMAX.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(10) = B, the base.'
      write ( *,     * ) i1mach(10)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Numbers associated with single precision values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  I1MACH(11) = T, the number of base B digits.'
      write ( *,     * ) i1mach(11)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(12) = EMIN, the smallest exponent E.'
      write ( *,     * ) i1mach(12)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(13) = EMAX, the largest exponent E.'
      write ( *,     * ) i1mach(13)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Numbers associated with double precision values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  I1MACH(14) = T, the number of base B digits.'
      write ( *,     * ) i1mach(14)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(15) = EMIN, the smallest exponent E.'
      write ( *,     * ) i1mach(15)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I1MACH(16) = EMAX, the largest exponent E.'
      write ( *,     * ) i1mach(16)

      return
      end
      subroutine dqag_test ( )

c*********************************************************************72
c
cc DQAG_TEST tests DQAG.
c
c  Discussion:
c
c    DQAG is an adaptive automatic integrator using a Gauss-Kronrod rule.
c
c    integrate cos(100*sin(x)) from 0 to pi.
c
c    The exact answer is pi * j0(100), or roughly 0.06278740.
c
c    KEY chooses the order of the integration rule, from 1 to 6.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer lenw
      parameter ( lenw = limit * 4 )

      double precision a
      double precision abserr
      double precision b
      double precision epsabs
      double precision epsrel
      double precision f02
      external f02
      integer ier
      integer iwork(limit)
      integer key
      integer last
      integer neval
      double precision pi
      double precision result
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00
      key = 6
      pi = 3.141592653589793D+00
      true = 0.06278740D+00

      b = pi

      call dqag ( f02, a, b, epsabs, epsrel, key, result, abserr, 
     &  neval, ier, limit, lenw, last, iwork, work )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAG_TEST'
      write ( *, '(a)' ) '  Test DQAG'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is COS(100*SIN(X))'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqagi_test ( )

c*********************************************************************72
c
cc DQAGI_TEST tests QAGI.
c
c  Discussion:
c
c    DQAGI is an adaptive quadrature routine for infinite intervals.
c
c    integrate log(x)/(1+100*x*x) from 0 to infinity.
c
c    The exact answer is -pi*log(10)/20 = -0.3616892
c
c    give the type of infinity
c
c    inf=1 means a to infinity
c       -1      -infinity to a
c        2      -infinity to infinity
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer lenw
      parameter ( lenw = limit*4 )

      double precision a
      double precision abserr
      double precision epsabs
      double precision epsrel
      double precision f05
      external f05
      integer ier
      integer inf
      integer iwork(limit)
      integer last
      integer neval
      double precision pi
      double precision result
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00
      inf = 1
      pi = 3.141592653589793D+00

      call dqagi ( f05, a, inf, epsabs, epsrel, result, abserr, 
     &  neval, ier, limit, lenw, last, iwork, work )

      true = - pi * log ( 10.0D+00 ) / 20.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAGI_TEST'
      write ( *, '(a)' ) '  Test DQAGI'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is log(x)/(1+100*x*x)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) 
     &  '  Integral right endpoint B =    Infinity'
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqagp_test ( )

c*********************************************************************72
c
cc DQAGP_TEST tests QAGP.
c
c  Discussion:
c
c    DQAGP is an adaptive integrator that can handle singularities
c    of the integrand at user specified points,
c
c    Integrate 
c
c      x**3 * log(abs( (x*x-1)*(x*x-2) )) 
c
c    from 0 to 3.
c
c    The exact answer is 61*log(2)+77*log(7)/4 - 27.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer npts
      parameter ( npts = 2 )

      integer npts2
      parameter ( npts2 = 2 * npts )

      integer leniw
      parameter ( leniw = 2 * limit + npts2 )

      integer lenw
      parameter ( lenw = 2 * leniw - npts2 )

      double precision a
      double precision abserr
      double precision b
      double precision epsabs
      double precision epsrel
      double precision f04
      external f04
      integer ier
      integer iwork(leniw)
      integer last
      integer neval
      double precision points(npts2)
      double precision result
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      b = 3.0D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00
c
c  Singularity points:
c
      points(1) = 1.0D+00
      points(2) = sqrt ( 2.0D+00 )

      call dqagp ( f04, a, b, npts2, points, epsabs, epsrel, 
     &  result, abserr, neval, ier, leniw, lenw, last, iwork, work )

      true = 61.0D+00 * log ( 2.0D+00 ) 
     &  + 77.0D+00 * log ( 7.0D+00 ) / 4.0D+00 - 27.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAGP_TEST'
      write ( *, '(a)' ) '  Test DQAGP'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Integrand is x**3 * log(abs((x*x-1)*(x*x-2)))'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqags_test ( )

c*********************************************************************72
c
cc DQAGS_TEST tests QAGS.
c
c  Discussion:
c
c    QAGS is an adaptive integrator for endpoint singularities.
c
c    integrate log(x)/sqrt(x) from 0 to 1.
c
c    The exact answer is -4.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer lenw
      parameter ( lenw = 4 * limit )

      double precision a
      double precision abserr
      double precision b
      double precision epsabs
      double precision epsrel
      double precision f03
      external f03
      integer ier
      integer iwork(limit)
      integer last
      integer neval
      double precision result
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      b = 1.0D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00
      true = -4.0D+00

      call dqags ( f03, a, b, epsabs, epsrel, result, abserr, 
     &  neval, ier, limit, lenw, last, iwork, work )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAGS_TEST'
      write ( *, '(a)' ) '  Test DQAGS'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is LOG(X)/SQRT(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqawc_test ( )

c*********************************************************************72
c
cc DQAWC_TEST tests QAWC.
c
c  Discussion:
c
c    QAWC is an adaptive integrator for finding the Cauchy
c    principal value of the integral of f(x)*w(x) over (a,b)
c    where w(x)=1/(x-c), c between a and b.
c
c    Integrate 1/(x*(5*x*x*x+6)) from -1 to 5
c
c    The exact answer is log(125/631) / 18 = -0.08994401
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer lenw
      parameter ( lenw = 4 * limit )

      double precision a
      double precision abserr
      double precision b
      double precision c
      double precision epsabs
      double precision epsrel
      double precision f09
      external f09
      integer ier
      integer iwork(limit)
      integer last
      integer neval
      double precision result
      double precision true
      double precision work(lenw)

      a = -1.0D+00
      b = 5.0D+00
      c = 0.0D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00

      call dqawc ( f09, a, b, c, epsabs, epsrel, result, 
     &  abserr, neval, ier, limit, lenw, last, iwork, work )

      true = log ( 125.0D+00 / 631.0D+00 ) / 18.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAWC_TEST'
      write ( *, '(a)' ) '  Test DQAWC'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is 1/(x*(5*x**3+6)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) '  Point of singularity c =      ', c
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqawf_test ( )

c*********************************************************************72
c
cc DQAWF_TEST tests QAWF.
c
c  Discussion:
c
c    DQAWF handles fourier integration of f(x)*w(x) from
c    a to infinity, with w(x)=cos(omega*x) or sine(omega*x)
c
c    integrate cos(pi*x/2) /sqrt(x) from 0 to infinity.
c
c    The exact answer is 1.0
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 3000 )

      integer limlst
      parameter ( limlst = 7 )

      integer leniw
      parameter ( leniw = 2 * limit + limlst )

      integer maxp1
      parameter ( maxp1 = 15 )

      integer lenw
      parameter ( lenw = 2 * leniw + 25 * maxp1 )

      double precision a
      double precision abserr
      double precision epsabs
      double precision f07
      external f07
      integer ier
      integer integr
      integer iwork(leniw)
      integer lst
      integer neval
      double precision omega
      double precision pi
      double precision result
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      epsabs = 0.001D+00
      integr = 1
      pi = 3.141592653589793D+00
      true = 1.0D+00
c
c  set argument of sine or cosine
c  set integr=1 for cosine, 2 for sine
c
      omega = 0.5D+00 * pi

      call dqawf ( f07, a, omega, integr, epsabs, result, abserr,
     &  neval, ier, limlst, lst, leniw, maxp1, lenw, iwork, work )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAWF_TEST'
      write ( *, '(a)' ) '  Test QAWF'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is cos(pi*x/2)/sqrt(x)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqawo_test ( )

c*********************************************************************72
c
cc DQAWO_TEST tests QAWO.
c
c  Discussion:
c
c    DQAWO integrates functions of the form 
c      f(x) * sin(omega*x)
c    or 
c      f(x) * cos(omega*x)
c
c    Here, we estimate 
c
c      Integral ( 0 <= x <= 1 ) log(x) sin(10*pi*x) dx
c
c    The exact answer is
c
c      exact = - ( gamma + log(10*pi) - ci(10*pi) ) / (10*pi)
c            = - 0.1281316...
c
c    Here Gamma is Euler's constant.
c
c    ci is the cosine integral:
c
c      ci(x) = integral ( x <= v < +oo ) - cos ( v ) / v dv.
c
c    We specify 
c      * INTEGR=1 for integrands with a cosine factor;
c      * INTEGR=2 for integrands with a sine factor.
c
c    Thanks to William Gandler for pointing out errors in the documentation
c    and text of this example, 29 October 2010.
c
c  Modified:
c
c    29 October 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer leniw 
      parameter ( leniw = 2 * limit )

      integer maxp1
      parameter ( maxp1 = 25 )

      integer lenw
      parameter ( lenw = 2 * leniw + 25 * maxp1 )

      double precision a
      double precision abserr
      double precision b
      double precision ci
      double precision epsabs
      double precision epsrel
      double precision f06
      external f06
      double precision gamma
      integer ier
      integer integr
      integer iwork(leniw)
      integer last
      integer neval
      double precision omega
      double precision pi
      double precision result
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      b = 1.0D+00
      ci = -0.001007D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00
      gamma = 0.5772156649D+00
      integr = 2
      pi = 3.141592653589793D+00
c
c  Set the argument of the sine or cosine function.
c
      omega = 10.0D+00 * pi

      call dqawo ( f06, a, b, omega, integr, epsabs, epsrel, 
     &  result, abserr, neval, ier, leniw, maxp1, lenw, last,
     &  iwork, work )

      true = - ( gamma + log ( 10.0D+00 * pi ) - ci ) 
     &  / ( 10.0D+00 * pi )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAWO_TEST'
      write ( *, '(a)' ) '  Test DQAWO'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is log(x)*sin(10*pi*x)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) '  Error return code IER = ', ier

      return
      end
      subroutine dqaws_test ( )

c*********************************************************************72
c
cc DQAWS_TEST tests QAWS.
c
c  Discussion:
c
c    DQAWS is an adaptive integrator for integrands with
c    algebraic or logarithmic singularities at the endpoints.
c
c    Here we estimate:
c
c      Integral ( 0 <= x <= 1 ) log(x) / ( 1 + log(x)^2 )^2 dx
c
c    The exact answer is 
c
c      exact = 0.5 * ( ci(1.0) * sin(1.0) - si(G&R)(1.0) * cos(1.0) - 1.0 )
c
c    Numerically:
c
c      exact = -0.18927518788209332118...
c
c    ci is the cosine integral:
c
c      ci(x) = integral ( x <= v < oo ) - cos(v) / v dv
c
c    si (according to Mathematica, for instance) is the sine integral:
c
c      si(x) = integral ( 0 <= v <= x )    sin(v) / v dv
c
c    Note that when Gradshteyn and Rizhik refer to si(x), they
c    mean, instead the complementary form:
c
c      si(G&R)(x) = integral ( x <= v < oo ) sin(v) / v dv
c                 = ( pi / 2 ) - si(x)
c
c    ci(1.0)      = 0.33740392290096813466
c    si(1.0)      = 0.94608307036718301494
c    si(G&R)(1.0) = 0.62471325642771360429
c
c    Thanks to William Gandler for questioning a previous version of
c    the documentation and text of this example, which led to the 
c    clarification of the difference between the Gradshteyn & Rizhik
c    convention versus the Mathematica convention for the sine integral
c    function, 02 November 2010.
c
c    Note that the original QUADPACK documentation lists the answer as
c      (Ci(1)sin(1)+(pi/2-Si(1))*cos(1))/pi
c    which has an incorrect final divisor of pi (it should be 2) and
c    which uses the Mathematica convention for Si.
c
c  Modified:
c
c    02 November 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer limit
      parameter ( limit = 1000 )

      integer lenw
      parameter ( lenw = 4 * limit )

      double precision a
      double precision abserr
      double precision alfa
      double precision b
      double precision beta
      double precision ci
      double precision epsabs
      double precision epsrel
      double precision f08
      external f08
      integer ier
      integer integr
      integer iwork(limit)
      integer last
      integer neval
      double precision pi
      double precision result
      double precision si
      double precision true
      double precision work(lenw)

      a = 0.0D+00
      alfa = 0.0D+00
      b = 1.0D+00
      beta = 0.0D+00
      ci = 0.33740392290096813466D+00
      epsabs = 0.0D+00
      epsrel = 0.0001D+00
      pi = 3.141592653589793D+00
      si = 0.94608307036718301494D+00
c
c  INTEGR = 2 means the weight function is
c
c    (x-a)**alfa * (b-x)**beta * log(x-a)
c
      integr = 2

      call dqaws ( f08, a, b, alfa, beta, integr, epsabs, epsrel,
     &  result, abserr, neval, ier, limit, lenw, last, iwork, work )

      true = 0.5D+00 * ( ci * sin ( 1.0D+00 ) 
     &  + ( pi / 2.0D+00 - si ) * cos ( 1.0D+00 ) - 1.0D+00 ) 

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQAWS_TEST'
      write ( *, '(a)' ) '  Test DQAWS'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is log(x)/(1+(log(x))**2)**2'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) 
     &  '  Error return code IER = ', ier

      return
      end
      subroutine dqk15_test ( )

c*********************************************************************72
c
cc DQK15_TEST tests QK15.
c
c  Discussion:
c
c    DQK15 is a Gauss-Kronrod quadrature rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision f01
      external f01
      double precision resabs
      double precision resasc
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      true = - 4.0D+00 / 9.0D+00

      call dqk15 ( f01, a, b, result, abserr, resabs, resasc )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQK15_TEST'
      write ( *, '(a)' ) '  Test QK15'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,g14.6)' ) 
     &  '  RESABS =                      ', resabs
      write ( *, '(a,g14.6)' ) 
     &  '  RESASC =                      ', resasc

      return
      end
      subroutine dqk21_test ( )

c*********************************************************************72
c
cc DQK21_TEST tests DQK21.
c
c  Discussion:
c
c    DQK21 is a Gauss-Kronrod quadrature rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision f01
      external f01
      double precision resabs
      double precision resasc
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      true = - 4.0D+00 / 9.0D+00

      call dqk21 ( f01, a, b, result, abserr, resabs, resasc )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQK21_TEST'
      write ( *, '(a)' ) '  Test DQK21'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,g14.6)' ) 
     &  '  RESABS =                      ', resabs
      write ( *, '(a,g14.6)' ) 
     &  '  RESASC =                      ', resasc

      return
      end
      subroutine dqk31_test ( )

c*********************************************************************72
c
cc DQK31_TEST tests QK31.
c
c  Discussion:
c
c    QK31 is a Gauss-Kronrod quadrature rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision f01
      external f01
      double precision resabs
      double precision resasc
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      true = - 4.0D+00 / 9.0D+00

      call dqk31 ( f01, a, b, result, abserr, resabs, resasc )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQK31_TEST'
      write ( *, '(a)' ) '  Test QK31'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,g14.6)' ) 
     &  '  RESABS =                      ', resabs
      write ( *, '(a,g14.6)' ) 
     &  '  RESASC =                      ', resasc

      return
      end
      subroutine dqk41_test ( )

c*********************************************************************72
c
cc DQK41_TEST tests QK41.
c
c  Discussion:
c
c    DQK41 is a Gauss-Kronrod quadrature rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision f01
      external f01
      double precision resabs
      double precision resasc
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      true = - 4.0D+00 / 9.0D+00

      call dqk41 ( f01, a, b, result, abserr, resabs, resasc )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQK41_TEST'
      write ( *, '(a)' ) '  Test DQK41'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,g14.6)' ) 
     &  '  RESABS =                      ', resabs
      write ( *, '(a,g14.6)' ) 
     &  '  RESASC =                      ', resasc

      return
      end
      subroutine dqk51_test ( )

c*********************************************************************72
c
cc DQK51_TEST tests DQK51.
c
c  Discussion:
c
c    QK51 is a Gauss-Kronrod quadrature rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision f01
      external f01
      double precision resabs
      double precision resasc
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      true = - 4.0D+00 / 9.0D+00

      call dqk51 ( f01, a, b, result, abserr, resabs, resasc )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQK51_TEST'
      write ( *, '(a)' ) '  Test DQK51'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,g14.6)' ) 
     &  '  RESABS =                      ', resabs
      write ( *, '(a,g14.6)' ) 
     &  '  RESASC =                      ', resasc

      return
      end
      subroutine dqk61_test ( )

c*********************************************************************72
c
cc DQK61_TEST tests DQK61.
c
c  Discussion:
c
c    DQK61 is a Gauss-Kronrod quadrature rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision f01
      external f01
      double precision resabs
      double precision resasc
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      true = - 4.0D+00 / 9.0D+00

      call dqk61 ( f01, a, b, result, abserr, resabs, resasc )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQK61_TEST'
      write ( *, '(a)' ) '  Test DQK61'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,g14.6)' ) 
     &  '  RESABS =                      ', resabs
      write ( *, '(a,g14.6)' ) 
     &  '  RESASC =                      ', resasc

      return
      end
      subroutine dqng_test ( )

c*********************************************************************72
c
cc DQNG_TEST tests DQNG.
c
c  Discussion:
c
c    DQNG is a nonadaptive automatic integrator using a Gauss-Kronrod or 
c    Patterson rule.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision abserr
      double precision b
      double precision epsabs
      double precision epsrel
      double precision f01
      external f01
      integer ier
      integer neval
      double precision result
      double precision true

      a = 0.0D+00
      b = 1.0D+00
      epsabs = 0.0D+00
      epsrel = 0.001D+00
      true = - 4.0D+00 / 9.0D+00

      call dqng ( f01, a, b, epsabs, epsrel, result, abserr, 
     &  neval, ier )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DQNG_TEST'
      write ( *, '(a)' ) '  Test DQNG'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Integrand is SQRT(X)*LOG(X)'
      write ( *, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write ( *, '(a,g14.6)' ) '  Integral right endpoint B =   ', b
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral is             ', true
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral is         ', result
      write ( *, '(a,g14.6)' ) 
     &  '  Estimated integral error =    ', abserr
      write ( *, '(a,g14.6)' ) 
     &  '  Exact integral error =        ', true - result
      write ( *, '(a,i8)' ) 
     &  '  Number of function evaluations, NEVAL = ', neval
      write ( *, '(a,i8)' ) '  Error return code IER = ', ier

      return
      end
      function f01 ( x )

c*********************************************************************72
c
cc F01 is the integrand function SQRT(X) * LOG(X).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f01
      double precision x

      if ( x <= 0.0D+00 )then
        f01 = 0.0D+00
      else
        f01 = sqrt ( x ) * log ( x )
      end if

      return
      end
      function f02 ( x )

c*********************************************************************72
c
cc F02 is the integrand function COS(100*SIN(X)).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f02
      double precision x

      f02 = cos ( 100.0D+00 * sin ( x ) )

      return
      end
      function f03 ( x )

c*********************************************************************72
c
cc F03 is the integrand function LOG(X)/SQRT(X).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f03
      double precision x

      if ( x <= 0.0D+00 ) then
        f03 = 0.0D+00
      else
        f03 = log ( x ) / sqrt ( x )
      end if

      return
      end
      function f04 ( x )

c*********************************************************************72
c
cc F04 is the integrand function X^3 LOG((X^2-1)*(X^2-2))
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f04
      double precision x

      f04 = x**3 * log ( abs ( ( x**2 - 1.0D+00 ) 
     &  * ( x**2 - 2.0D+00 ) ) )

      return
      end
      function f05 ( x )

c*********************************************************************72
c
cc F05 is the integrand function LOG(X)/(1+100X^2).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f05
      double precision x

      f05 = log ( x ) / ( 1.0D+00 + 100.0D+00 * x**2 )

      return
      end
      function f06 ( x )

c*********************************************************************72
c
cc F06 is the integrand function LOG(X).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f06
      double precision x

      if ( x <= 0.0D+00 ) then
        f06 = 0.0D+00
      else
        f06 = log ( x )
      end if

      return
      end
      function f07 ( x )

c*********************************************************************72
c
cc F07 is the integrand function 1/SQRT(X).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f07
      double precision x

      if ( x <= 0.0D+00 ) then
        f07 = 0.0D+00
      else
        f07 = 1.0D+00 / sqrt ( x )
      end if

      return
      end
      function f08 ( x )

c*********************************************************************72
c
cc F08 is the integrand function 1/(1+LOG(X)**2)**2
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f08
      double precision x

      if ( 0.0D+00 < x ) then
        f08 = 1.0D+00 / ( 1.0D+00 + log ( x )**2 )**2
      else
        f08 = 0.0D+00
      end if

      return
      end
      function f09 ( x )

c*********************************************************************72
c
cc F09 is the integrand function 1 / ( 5 X^3 + 6 ).
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f09
      double precision x

      f09 = 1.0D+00 / ( 5.0D+00 * x**3 + 6.0D+00 )

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
