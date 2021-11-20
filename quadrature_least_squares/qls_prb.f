      program main

c*********************************************************************72
c
cc MAIN is the main program for QLS_PRB.
c
c  Discussion:
c
c    QLS_PRB tests the QUADRATURE_LEAST_SQUARES library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QLS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the QUADRATURE_LEAST_SQUARES library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'QLS_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc QLS_TEST01 shows that we can compute the Newton-Cotes rules.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      double precision a
      double precision b
      integer d
      integer i
      integer n
      double precision w1(n_max)
      double precision w2(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  WEIGHTS_LS computes the weights for a'
      write ( *, '(a)' ) '  least squares quadrature rule.'
c
c  Demonstrate the 5 point Newton-Cotes closed rule.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  W1 = classical Newton Cotes weights, N = 5'
      write ( *, '(a)' ) '  W2 = least squares weights, D = 4, N = 5'

      n = 5
      call ncc_set ( n, x, w1 )
c
c  Using the same points, compute the least squares weights
c  for polynomial approximation up to degree 4.
c
      d = n - 1
      a = -1.0D+00
      b = +1.0D+00

      call weights_ls ( d, a, b, n, x, w2 )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   I        X(i)          W1(i)           W2(i)'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), w1(i), w2(i)
      end do
c
c  Look at a 9 point rule.
c  Note that Newton Cotes rules soon have many negative weights.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  W1 = classical Newton Cotes weights, N = 9'
      write ( *, '(a)' ) '  W2 = least squares weights, D = 4, N = 9'

      n = 9

      call ncc_set ( n, x, w1 )
c
c  Using the same points, compute the least squares weights
c  for polynomial approximation up to degree 4.
c
      d = 4
      a = -1.0D+00
      b = +1.0D+00

      call weights_ls ( d, a, b, n, x, w2 )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   I        X(i)          W1(i)           W2(i)'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), w1(i), w2(i)
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses random points as abscissas.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 50 )

      double precision a
      double precision b
      integer d
      double precision e
      double precision exact
      double precision f(n)
      integer j
      double precision q
      double precision r8vec_dot_product
      double precision r8vec_sum
      integer seed
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  WEIGHTS_LS computes the weights for a'
      write ( *, '(a)' ) '  least squares quadrature rule.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Pick 50 random values in [-1,+1].'
      write ( *, '(a)' ) 
     &  '  Compare Monte Carlo (equal weight) integral estimate'
      write ( *, '(a)' ) 
     &  '  to least squares estimates of degree D = 0, 1, 2, 3, 4.'
      write ( *, '(a)' ) 
     &  '  For low values of D, the least squares estimate improves.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  As D increases, the estimate can deteriorate.'
c
c  Define the integration interval.
c
      a = -5.0D+00
      b = +5.0D+00
c
c  Get random values.
c
      seed = 123456789
      call r8vec_uniform_ab ( n, a, b, seed, x )
c
c  Evaluate the function.
c
      do j = 1, n
        f(j) = 1.0D+00 / ( 1.0 + x(j) ** 2 )
      end do

      exact = atan ( b ) - atan ( a )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Rule         Estimate          Error'
c
c  Get the MC estimate.
c
      q = ( b - a ) * r8vec_sum ( n, f ) / dble ( n )
      e = abs ( q - exact )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,2x,g14.6)' ) '  MC     ', q, e
      write ( *, '(a)' ) ''
c
c  Using the same points, compute the least squares weights
c  for polynomial approximation of degree D.
c
      do d = 0, 15

        call weights_ls ( d, a, b, n, x, w )
        q = r8vec_dot_product ( n, w, f )
        e = abs ( q - exact )
        write ( *, '(a,i2,2x,g14.6,2x,g14.6)' ) '  LS', d, q, e

      end do

      q = exact
      e = abs ( q - exact )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,2x,g14.6)' ) '  EXACT ', q, e

      return
      end
