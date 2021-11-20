      program main

c*********************************************************************72
c
cc MAIN is the main program for QWGW_PRB.
c
c  Discussion:
c
c    QWGW_PRB tests the QWGW library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QWGW_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the QWGW library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QWGW_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests QWGW for the Chebyshev Type 1 weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision b
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  Set the quadrature interval and number of points.
c
      a = -1.0D+00
      b = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) 
     &  '  with the Chebyshev Type 1 weight w(x) = 1/sqrt(1-x^2).'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval = [', a, ',', b, ']'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = 0.0D+00
      end do

      bj(1) = 1.0D+00 / 2.0D+00
      do j = 2, n - 1
        bj(j) = 1.0D+00 / 4.0D+00
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = pi
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests QWGW for the Chebyshev Type 2 weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision b
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  Set the quadrature interval.
c
      a = -1.0D+00
      b = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) 
     &  '  with the Chebyshev Type 2 weight w(x) = sqrt(1-x^2).'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval = [', a, ',', b, ']'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = 0.0D+00
      end do

      do j = 1, n - 1
        bj(j) = 1.0D+00 / 4.0D+00
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = pi / 2.0D+00
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests QWGW for the Gegenbauer weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision alpha
      double precision b
      double precision bj(n)
      integer j
      double precision jr
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  Set the quadrature interval.
c
      a = -1.0D+00
      b = +1.0D+00
c
c  Set the weight function parameter.
c
      alpha = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) 
     &  '  with the Gegenbauer weight w(x) = (1-x^2)^alpha.'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4)' ) '  ALPHA = ', alpha
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval = [', a, ',', b, ']'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = 0.0D+00
      end do

      do j = 1, n - 1
        jr = dble ( j )
        bj(j) = ( jr * ( 2.0D+00 * alpha + jr ) )
     &             / ( 4.0D+00 * ( alpha + jr )**2 - 1.0D+00 )
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = gamma ( alpha + 1.0D+00 ) * gamma ( 0.5D+00 ) 
     &  / gamma ( alpha + 1.5D+00 )
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests QWGW for the generalized Hermite weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision aj(n)
      double precision alpha
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  The quadrature interval is (-oo,+oo).
c

c
c  Set the weight function parameter.
c
      alpha = 2.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature with'
      write ( *, '(a)' ) 
     &  '  the generalized Hermite weight w(x) = |x|^alpha * exp(-x^2).'
      write ( *, '(a,f10.4)' ) '  ALPHA = ', alpha
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) '  Interval = (-oo,+oo)'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = 0.0D+00
      end do

      do j = 1, n - 1
        if ( mod ( j, 2 ) .eq. 1 ) then
          bj(j) = ( dble ( j ) + alpha ) / 2.0D+00
        else
          bj(j) = dble ( j ) / 2.0D+00
        end if
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = gamma ( ( alpha + 1.0D+00 ) / 2.0D+00 )
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests QWGW for the generalized Laguerre weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision alpha
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  The quadrature interval is [0,+oo).
c
      a = 0.0D+00
c
c  Set the weight function parameter.
c
      alpha = 2.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature with'
      write ( *, '(a)' ) 
     &  '  the generalized Laguerre weight w(x) = x^alpha * exp(-x).'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4)' ) '  ALPHA = ', alpha
      write ( *, '(a,f10.4,a,f10.4,a)' ) '  Interval = [0,+oo)'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = alpha + dble ( 2 * j - 1 )
      end do

      do j = 1, n - 1
        bj(j) = dble ( j ) * ( alpha + dble ( j ) )
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = gamma ( alpha + 1.0D+00 )
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests QWGW for the Hermite weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision aj(n)
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  The quadrature interval is (-oo,+oo).
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) '  with the Hermite weight w(x) = exp(-x^2).'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) '  Interval = (-oo,+oo)'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = 0.0D+00
      end do

      do j = 1, n - 1
        bj(j) = dble ( j ) / 2.0D+00
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = sqrt ( pi )
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests QWGW for the Jacobi weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision alpha
      double precision b
      double precision beta
      double precision bj(n)
      integer j
      double precision jr
      double precision mu0
      double precision w(n)
      double precision x(n)
c
c  Set the quadrature interval.
c
      a = -1.0D+00
      b = +1.0D+00
c
c  Set the weight function parameters.
c
      alpha = 0.25D+00
      beta = 0.75D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) 
     &  '  with the Jacobi weight w(x) = (1-x^2)^alpha*(1+x)^beta'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4)' ) '  ALPHA = ', alpha
      write ( *, '(a,f10.4)' ) '  BETA =  ', beta
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval = [', a, ',', b, ']'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        jr = dble ( j )
        aj(j) = ( beta - alpha ) * ( beta + alpha )
     &        / ( alpha + beta + 2.0D+00 * jr - 2.0D+00 )
     &        / ( alpha + beta + 2.0D+00 * jr )
      end do

      do j = 1, n - 1
        jr = dble ( j )
        bj(j) = 4.0D+00 * jr * ( alpha + jr ) * ( beta + jr )
     &    * ( alpha + beta + jr )       
     &    / ( ( alpha + beta + 2.0D+00 * jr )**2 - 1.0D+00 )
     &    / ( alpha + beta + 2.0D+00 * jr )**2
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = 2.0D+00**( alpha + beta + 1.0 )
     &     * gamma ( alpha + 1.0D+00 ) * gamma ( beta + 1.0D+00 )
     &     / gamma ( alpha + beta + 2.0D+00 )
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests QWGW for the Laguerre weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  The quadrature interval is [a,+oo).
c
      a = 0.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) '  with the Laguerre weight w(x) = exp(-x).'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) '  Interval = [0,+oo)'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = dble ( 2 * j - 1 )
      end do

      do j = 1, n - 1
        bj(j) = dble ( j**2 )
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = 1.0D+00
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 tests QWGW for the Legendre weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 ) 

      double precision a
      double precision aj(n)
      double precision b
      double precision bj(n)
      integer j
      double precision mu0
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n)
      double precision x(n)
c
c  Set the quadrature interval.
c
      a = -1.0D+00
      b = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09:'
      write ( *, '(a)' ) 
     &  '  Compute points and weights for Gauss quadrature'
      write ( *, '(a)' ) '  with the Legendre weight w(x) = 1.'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval = [', a, ',', b, ']'
c
c  Set the recursion coefficients.
c
      do j = 1, n
        aj(j) = 0.0D+00
      end do

      do j = 1, n - 1
        bj(j) = dble ( j**2 ) / dble ( 4 * j**2 - 1 )
      end do
      bj(n) = 0.0D+00

      do j = 1, n
        bj(j) = sqrt ( bj(j) )
      end do

      mu0 = 2.0D+00
c
c  Compute the points and weights.
c
      call sgqf ( n, aj, bj, mu0, x, w )

      call r8vec_print ( n, x, '  Abscissas:' )
      call r8vec_print ( n, w, '  Weights:' )

      return
      end
