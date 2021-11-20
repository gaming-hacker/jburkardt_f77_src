      program main

c*********************************************************************72
c
cc MAIN is the main program for QUADMOM_PRB.
c
c  Discussion:
c
c    QUADMOM_PRB tests the QUADMOM library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the QUADMOM library.'

      call quadmom_prb01 ( )
      call quadmom_prb02 ( )
      call quadmom_prb03 ( )
      call quadmom_prb04 ( )
      call quadmom_prb05 ( )
      call quadmom_prb06 ( )
      call quadmom_prb07 ( )
      call quadmom_prb08 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine quadmom_prb01 ( )

c*********************************************************************72
c
cc QUADMOM_PRB01 tests the QUADMOM procedure for the Legendre weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer m
      parameter ( m = 2 * n + 1 )

      double precision a
      double precision alpha2
      double precision b
      double precision beta2
      integer kind
      integer lu
      double precision moment(0:m-1)
      double precision w1(n)
      double precision w2(n)
      double precision x1(n)
      double precision x2(n)

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) 'QUADMOM_PRB01:'
      write (  *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule'
      write (  *, '(a)' ) 
     &  '  for the Legendre weight, rho(x)=1, over [-1,+1],'
      write (  *, '(a)' ) '  using Golub and Welsch''s moment method.'
      write (  *, '(a)' ) '  Compare with a standard calculation.'
c
c  N is the order of the rule we want to compute.
c
c  Compute M = 2*N+1 moments for the Legendre weight on [-1,+1].
c
      a = -1.0D+00
      b = 1.0D+00

      call moments_legendre ( m, a, b, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Compute the points and weights the usual way.
c
      kind = 1
      alpha2 = 0.0D+00
      beta2 = 0.0D+00
      a = -1.0D+00
      b = +1.0D+00
      lu = 0

      call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
c
c  Compare the results.
c
      call r8vec2_print ( n, x1, x2,
     &  '  Points from GW moment and orthogonal polynomial methods:' )

      call r8vec2_print ( n, w1, w2,
     &  '  Weights from GW moment and orthogonal polynomial methods:' )

      return
      end
      subroutine quadmom_prb02 ( )

c*********************************************************************72
c
cc QUADMOM_PRB02 tests the QUADMOM procedure for the standard Gaussian weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer m
      parameter ( m = 2 * n + 1 )

      double precision a
      double precision alpha2
      double precision b
      double precision beta2
      integer i
      integer kind
      integer lu
      double precision moment(0:m-1)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision sigma
      double precision w1(n)
      double precision w2(n)
      double precision x1(n)
      double precision x2(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUADMOM_PRB02:'
      write ( *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule for'
      write ( *, '(a)' ) 
     &  '  the standard Gaussian weight, rho(x)=exp(-x^2/2)/sqrt(2pi),'
      write ( *, '(a)' ) 
     &  '  over (-oo,+oo), using Golub and Welsch''s moment method.'
      write ( *, '(a)' ) '  Compare with a standard calculation.'
c
c  N is the order of the quadrature rule.
c
c  Compute the M = 2 * N + 1 moments for the standard Gaussian weight on (-oo,+oo).
c
      call moments_normal_01 ( m, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Compute the points and weights the usual way.
c
      kind = 6
      alpha2 = 0.0D+00
      beta2 = 0.0D+00
      a = 0.0D+00
      b = +0.5D+00
      lu = 0

      call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
c
c  The CGQF weights need to be normalized by sigma * sqrt ( 2 * pi )
c  because they don't divide the Gaussian PDF by that factor.
c
      sigma = 1.0D+00
      do i = 1, n
        w2(i) = w2(i) / sigma / sqrt ( 2.0D+00 * pi )
      end do
c
c  Compare the results.
c
      call r8vec2_print ( n, x1, x2,
     &  '  Points from GW moment and orthogonal polynomial methods:' )

      call r8vec2_print ( n, w1, w2,
     &  '  Weights from GW moment and orthogonal polynomial methods:' )

      return
      end
      subroutine quadmom_prb03 ( )

c*********************************************************************72
c
cc QUADMOM_PRB03 tests the QUADMOM procedure for the general Gaussian weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer m
      parameter ( m = 2 * n + 1 )

      double precision a
      double precision alpha2
      double precision b
      double precision beta2
      integer i
      integer kind
      integer lu
      double precision moment(0:m-1)
      double precision mu
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision sigma
      double precision w1(n)
      double precision w2(n)
      double precision x1(n)
      double precision x2(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB03:'
      write ( *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule for'
      write ( *, '(a)' ) '  a general Gaussian weight,'
      write ( *, '(a)' ) 
     &  '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
      write ( *, '(a)' ) 
     &  '  over (-oo,+oo), using Golub and Welsch''s moment method.'
      write ( *, '(a)' ) '  Compare with a standard calculation.'
c
c  N is the order of the quadrature rule.
c
c  Compute the M = 2 * N + 1 moments for a general Gaussian weight on (-oo,+oo).
c
      mu = 1.0D+00
      sigma = 2.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma

      call moments_normal ( m, mu, sigma, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Compute the points and weights the usual way.
c
      kind = 6
      alpha2 = 0.0D+00
      beta2 = 0.0D+00
      a = 1.0D+00
      b = 0.5D+00 / sigma ** 2
      lu = 0

      call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
c
c  The CGQF weights need to be normalized by sigma * sqrt ( 2 * pi )
c  because they don't divide the Gaussian PDF by that factor.
c
      do i = 1, n
        w2(i) = w2(i) / sigma / sqrt ( 2.0D+00 * pi )
      end do
c
c  Compare the results.
c
      call r8vec2_print ( n, x1, x2,
     &  '  Points from GW moment and orthogonal polynomial methods:' )

      call r8vec2_print ( n, w1, w2,
     &  '  Weights from GW moment and orthogonal polynomial methods:' )

      return
      end
      subroutine quadmom_prb04 ( )

c*********************************************************************72
c
cc QUADMOM_PRB04 tests the QUADMOM procedure for the Laguerre weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer m
      parameter ( m = 2 * n + 1 )

      double precision a
      double precision alpha2
      double precision b
      double precision beta2
      integer kind
      integer lu
      double precision moment(0:m-1)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w1(n)
      double precision w2(n)
      double precision x1(n)
      double precision x2(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB04:'
      write ( *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule for'
      write ( *, '(a)' ) '  the Laguerre weight, rho(x)=exp(-x),'
      write ( *, '(a)' ) 
     &  '  over [0,+oo), using Golub and Welsch''s moment method.'
      write ( *, '(a)' ) '  Compare with a standard calculation.'
c
c  N is the order of the quadrature rule.
c
c  Compute the M = 2 * N + 1 moments for the Laguerre weight on [0,+oo).
c
      call moments_laguerre ( m, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Compute the points and weights the usual way.
c
      kind = 5
      alpha2 = 0.0D+00
      beta2 = 0.0D+00
      a = 0.0D+00
      b = +1.0D+00
      lu = 0

      call cgqf ( n, kind, alpha2, beta2, a, b, lu, x2, w2 )
c
c  Compare the results.
c
      call r8vec2_print ( n, x1, x2,
     &  '  Points from GW moment and orthogonal polynomial methods:' )

      call r8vec2_print ( n, w1, w2,
     &  '  Weights from GW moment and orthogonal polynomial methods:' )

      return
      end
      subroutine quadmom_prb05 ( )

c*********************************************************************72
c
cc QUADMOM_PRB05 tests the QUADMOM procedure for the truncated normal weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      integer m
      double precision moment(0:2*n)
      double precision mu
      integer order
      double precision sigma
      double precision w1(n)
      double precision x1(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB05:'
      write ( *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule for'
      write ( *, '(a)' ) '  a truncated normal weight,'
      write ( *, '(a)' ) 
     &  '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
      write ( *, '(a)' ) 
     &  '  over [a,b], using Golub and Welsch''s moment method.'
c
c  N is the order of the quadrature rule.
c
c  Compute the M = 2 * N + 1 moments.
c
      m = 2 * n + 1
      mu = 100.0D+00
      sigma = 25.0D+00
      a = 50.0D+00
      b = 150.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, '(a,g14.6)' ) '  B = ', b

      call moments_truncated_normal_ab ( m, mu, sigma, a, b, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Print the results.
c
      call r8vec_print ( n, x1, '  Points from GW moment method:' )

      call r8vec_print ( n, w1, '  Weights from GW moment method:' )

      return
      end
      subroutine quadmom_prb06 ( )

c*********************************************************************72
c
cc QUADMOM_PRB06 tests QUADMOM for the lower truncated normal weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 9 )

      double precision a
      integer m
      double precision moment(0:2*n)
      double precision mu
      integer order
      double precision sigma
      double precision w1(n)
      double precision x1(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB06:'
      write ( *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule for'
      write ( *, '(a)' ) '  a lower truncated normal weight,'
      write ( *, '(a)' ) 
     &  '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
      write ( *, '(a)' ) 
     &  '  over [a,+oo), using Golub and Welsch''s moment method.'
c
c  N is the order of the quadrature rule.
c
c  Compute the M = 2 * N + 1 moments.
c
      m = 2 * n + 1
      mu = 2.0D+00
      sigma = 0.5D+00
      a = 0.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,g14.6)' ) '  A = ', a

      call moments_truncated_normal_a ( m, mu, sigma, a, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Print the results.
c
      call r8vec_print ( n, x1, '  Points from GW moment method:' )

      call r8vec_print ( n, w1, '  Weights from GW moment method:' )

      return
      end
      subroutine quadmom_prb07 ( )

c*********************************************************************72
c
cc QUADMOM_PRB07 tests QUADMOM for the upper truncated normal weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n
      parameter ( n = 9 )

      double precision b
      integer m
      double precision moment(0:2*n)
      double precision mu
      integer order
      double precision sigma
      double precision w1(n)
      double precision x1(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB07:'
      write ( *, '(a)' ) 
     &  '  Compute the points and weights of a quadrature rule for'
      write ( *, '(a)' ) '  a truncated normal weight,'
      write ( *, '(a)' ) 
     &  '  rho(mu,s;x)=exp(-((x-mu)/sigma)^2/2)/sigma^2/sqrt(2pi),'
      write ( *, '(a)' ) 
     &  '  over (-oo,b], using Golub and Welsch''s moment method.'
c
c  N is the order of the quadrature rule.
c
c  Compute the M = 2 * N + 1 moments.
c
      m = 2 * n + 1
      mu = 2.0D+00
      sigma = 0.5D+00
      b = 3.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,g14.6)' ) '  B = ', b

      call moments_truncated_normal_b ( m, mu, sigma, b, moment )
c
c  Compute the points and weights by the method of moments.
c
      call moment_method ( n, moment, x1, w1 )
c
c  Print the results.
c
      call r8vec_print ( n, x1, '  Points from GW moment method:' )

      call r8vec_print ( n, w1, '  Weights from GW moment method:' )

      return
      end
      subroutine quadmom_prb08 ( )

c*********************************************************************72
c
cc QUADMOM_PRB08 integrates sin(x) against a lower truncated normal weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      double precision a
      integer i
      integer m
      double precision moment(0:2*n_max)
      double precision mu
      integer n
      integer order
      double precision q
      double precision sigma
      double precision w1(n_max)
      double precision x1(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QUADMOM_PRB08:'
      write ( *, '(a)' ) 
     &  '  Integrate sin(x) against a lower truncated normal weight.'

      mu = 0.0D+00
      sigma = 1.0D+00
      a = -3.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   N   Estimate'
      write ( *, '(a)' ) ''
c
c  N is the order of the quadrature rule.
c
      do n = 1, 9
c
c  Compute the M = 2 * N + 1 moments.
c
        m = 2 * n + 1

        call moments_truncated_normal_a ( m, mu, sigma, a, moment )
c
c  Compute the points and weights by the method of moments.
c
        call moment_method ( n, moment, x1, w1 )

        q = 0.0D+00
        do i = 1, n
          q = q + w1(i) * sin ( x1(i) )
        end do

        write ( *, '(2x,i2,2x,g14.6)' ) n, q

      end do

      return
      end
