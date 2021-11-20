      program main

c*********************************************************************72
c
cc MAIN is the main program for QUADRULE_PRB.
c
c  Discussion:
c
c    QUADRULE_PRB tests the QUADRULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha
      integer n

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUADRULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the QUADRULE library.'

      call bashforth_set_test ( )
      call test04 ( )
      call chebyshev1_compute_test ( )
      n = 10
      call test065 ( n )
      call chebyshev3_compute_test ( )
      call clenshaw_curtis_compute_test ( )
      call clenshaw_curtis_set_test ( )
      call fejer1_compute_test ( )
      call fejer1_set_test ( )
      call fejer2_compute_test ( )
      call fejer2_set_test ( )
      n = 5
      alpha = 0.250D+00
      call gegenbauer_compute_test ( n, alpha )
      n = 5
      alpha = -0.5D+00
      call gegenbauer_compute_test ( n, alpha )
      n = 31
      call test087 ( n )
      n = 63
      call test087 ( n )
      call test089 ( )
      call test095 ( )
      call lobatto_compute_test ( )
      call lobatto_set_test ( )
      call moulton_set_test ( )
      call ncc_set_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUADRULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )
 
      stop
      end
      subroutine bashforth_set_test ( )

c*********************************************************************72
c
cc BASHFORTH_SET_TEST tests BASHFORTH_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BASHFORTH_SET_TEST'
      write ( *, '(a)' ) 
     &  '  BASHFORTH_SET sets up an Adams-Bashforth rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   Index       X                       W'
      write ( *, '(a)' ) ' '

      do n = 1, 10

        call bashforth_set ( n, x, w )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i2,2x,g25.16,2x,f24.16)' ) i, x(i), w(i)
        end do

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests CHEBYSHEV_SET and SUM_SUB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer function_num
      parameter ( function_num = 11 )
      integer n_max
      parameter ( n_max = 9 )

      double precision a
      double precision b
      character * ( 10 ) function_name
      double precision function_value
      external function_value
      integer i
      integer ihi
      integer ilo
      integer nsub
      integer n
      double precision result(function_num)
      double precision w(n_max)
      double precision x(n_max)
      double precision xhi
      double precision xlo

      a =  0.0D+00
      b =  1.0D+00

      nsub = 1

      xlo = -1.0D+00
      xhi =  1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  CHEBYSHEV_SET sets up a Chebyshev rule;'
      write ( *, '(a)' ) '  SUM_SUB carries it out.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  The integration interval is [', a, ',', b, ']'
      write ( *, '(a,i8)' ) '  Number of subintervals is ', nsub
      write ( *, '(a)' ) '  Quadrature order will vary.'
      write ( *, '(a)' ) '  Integrand will vary.'
      write ( *, '(a)' ) ' '

      do ilo = 1, function_num, 5

        ihi = min ( ilo + 4, function_num )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,a7, 5(a10,4x) )' ) 
     &    'Order  ', ( function_name ( i ), i = ilo, ihi )
        write ( *, '(a)' ) ' '

        do n = 1, n_max

          if ( n .ne. 8 ) then

            call chebyshev_set ( n, x, w )

            do i = ilo, ihi

              call function_set ( 'SET', i )

              call sum_sub ( function_value, a, b, nsub, n, xlo, xhi, 
     &          x, w, result(i) )
   
            end do

            write ( *, '(2x,i2,2x,5f14.8)' ) n, result(ilo:ihi)

          end if

       end do
 
      end do

      return
      end
      subroutine chebyshev1_compute_test ( )

c*********************************************************************72
c
cc CHEBYSHEV1_COMPUTE_TEST tests CHEBYSHEV1_COMPUTE
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBYSHEV1_COMPUTE_TEST'
      write ( *, '(a)' ) '  CHEBYSHEV1_COMPUTE computes'
      write ( *, '(a)' ) 
     &  '  a Chebyshev Type 1 quadrature rule over [-1,1]'
      write ( *, '(a)' ) '  of given order.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   Index       X                       W'
      write ( *, '(a)' ) ' '

      do n = 1, 10

        call chebyshev1_compute ( n, x, w )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i2,2x,g25.16,2x,f24.16)' ) i, x(i), w(i)
        end do

      end do

      return
      end
      subroutine test065 ( n )

c*********************************************************************72
c
cc TEST065 tests CHEBSHEV2_COMPUTE.
c
c  Discussion:
c
c    We estimate an integral over a semicircle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n

      double precision error
      double precision exact
      double precision f(n)
      integer i
      double precision q
      double precision r8vec_dot_product
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST065'
      write ( *, '(a)' ) 
     &  '  Approximate the integral of f(x,y) over the semicircle'
      write ( *, '(a)' ) '    -1 <= x <= 1, y = sqrt ( 1 - x^2 )'
      write ( *, '(a)' ) '  using N Chebyshev points.'
      write ( *, '(a)' ) 
     &  '  If p(x,y) involves any term of odd degree in y,'
      write ( *, '(a)' ) '  the estimate will only be approximate.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Polynomial    N    Integral        Estimate       Error'
      write ( *, '(a)' ) ' '

      call chebyshev2_compute ( n, x, w )
c
c  f(x,y) = 1
c
      exact = 1.5707963267948966192D+00
      do i = 1, n
        f(i) = 1.0D+00
      end do
      q = r8vec_dot_product ( n, w, f )
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '1      ', n, exact, q, error
c
c  f(x,y) = x
c
      exact = 0.0D+00
      do i = 1, n
        f(i) = x(i)
      end do
      q = r8vec_dot_product ( n, w, f )
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x      ', n, exact, q, error
c
c  f(x,y) = y = sqrt ( 1 - x^2 )
c
      exact = 0.66666666666666666667D+00
      do i = 1, n
        f(i) = sqrt ( 1.0D+00 - x(i)**2 )
      end do
      q = r8vec_dot_product ( n, w, f ) / 2.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '   y   ', n, exact, q, error
c
c  f(x,y) = x^2
c
      exact = 0.39269908169872415481D+00
      do i = 1, n
        f(i) = x(i)**2
      end do
      q = r8vec_dot_product ( n, w, f )
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^2    ', n, exact, q, error
c
c  f(x,y) = xy = x * sqrt ( 1 - x^2 )
c
      exact = 0.0D+00
      do i = 1, n
        f(i) = x(i) * sqrt ( 1.0D+00 - x(i)**2 )
      end do
      q = r8vec_dot_product ( n, w, f ) / 2.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x  y   ', n, exact, q, error
c
c  f(x,y) = y^2 -> ( 1 - x^2 )
c
      exact = 0.39269908169872415481D+00
      do i = 1, n
        f(i) = 1.0D+00 - x(i)**2
      end do
      q = r8vec_dot_product ( n, w, f ) / 3.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '   y^2 ', n, exact, q, error
c
c  f(x,y) = x^3
c
      exact = 0.0D+00
      do i = 1, n
        f(i) = x(i)**3
      end do
      q = r8vec_dot_product ( n, w, f )
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^3    ', n, exact, q, error
c
c  f(x,y) = x^2 y = x^2 sqrt ( 1 - x^2 )
c
      exact = 0.13333333333333333333D+00
      do i = 1, n
        f(i) = x(i)**2 * sqrt ( 1.0D+00 - x(i)**2 )
      end do
      q = dot_product ( w(1:n), f(1:n) ) / 2.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^2y   ', n, exact, q, error
c
c  f(x,y) = x y^2 = x * ( 1 - x^2 )
c
      exact = 0.0D+00
      do i = 1, n
        f(i) = x(i) * ( 1.0D+00 - x(i)**2 )
      end do
      q = dot_product ( w(1:n), f(1:n) ) / 3.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x  y^2 ', n, exact, q, error
c
c  f(x,y) = y^3
c
      exact = 0.26666666666666666667D+00
      do i = 1, n
        f(i) = ( 1.0D+00 - x(i)**2 )**(1.5D+00)
      end do
      q = r8vec_dot_product ( n, w, f ) / 4.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '   y^3 ', n, exact, q, error
c
c  f(x,y) = x^4
c
      exact = 0.19634954084936207740
      do i = 1, n
        f(i) = x(i)**4
      end do
      q = r8vec_dot_product ( n, w, f )
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^4    ', n, exact, q, error
c
c  f(x,y) = x^2y^2 -> x^2( 1 - x^2 )
c
      exact = 0.065449846949787359135D+00
      do i = 1, n
        f(i) = x(i)**2 * ( 1.0D+00 - x(i)**2 )
      end do
      q = r8vec_dot_product ( n, w, f ) / 3.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^2y^2 ', n, exact, q, error
c
c  f(x,y) = y^4 -> ( 1 - x^2 )^2
c
      exact = 0.19634954084936207740D+00
      do i = 1, n
        f(i) = ( 1.0D+00 - x(i)**2 )**2
      end do
      q = r8vec_dot_product ( n, w, f ) / 5.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '   y^4 ', n, exact, q, error
c
c  f(x,y) = x^4y = x^4 sqrt ( 1 - x^2 )
c
      exact = 0.057142857142857142857D+00
      do i = 1, n
        f(i) = x(i)**4 * sqrt ( 1.0D+00 - x(i)**2 )
      end do
      q = r8vec_dot_product ( n, w, f ) / 2.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^4y   ', n, exact, q, error
c
c  x^2y^3 = x^2 ( 1 - x^2 )**(3/2)
c
      exact = 0.038095238095238095238D+00
      do i = 1, n
        f(i) = x(i)**2 * ( 1.0D+00 - x(i)**2 )**(1.5D+00)
      end do
      q = r8vec_dot_product ( n, w, f ) / 4.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^2y^3 ', n, exact, q, error
c
c  f(x,y) = y^5
c
      exact = 0.15238095238095238095D+00
      do i = 1, n
        f(i) = ( 1.0D+00 - x(i)**2 )**(2.5D+00)
      end do
      q = r8vec_dot_product ( n, w, f ) / 6.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '   y^5 ', n, exact, q, error
c
c  f(x,y) = x^6
c
      exact = 0.12271846303085129838D+00
      do i = 1, n
        f(i) = x(i)**6
      end do
      q = r8vec_dot_product ( n, w, f )
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^6    ', n, exact, q, error
c
c  f(x,y) = x^4y^2 -> x^4( 1 - x^2 )
c
      exact = 0.024543692606170259675D+00
      do i = 1, n
        f(i) = x(i)**4 * ( 1.0D+00 - x(i)**2 )
      end do
      q = r8vec_dot_product ( n, w, f ) / 3.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^4y^2 ', n, exact, q, error
c
c  f(x,y) = x^2y^4 -> x^2( 1 - x^2 )**2
c
      exact = 0.024543692606170259675D+00
      do i = 1, n
        f(i) = x(i)**2 * ( 1.0D+00 - x(i)**2 )**2
      end do
      q = r8vec_dot_product ( n, w, f ) / 5.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  'x^2y^4 ', n, exact, q, error
c
c  f(x,y) = y^6 -> ( 1 - x^2 )^3
c
      exact = 0.12271846303085129838D+00
      do i = 1, n
        f(i) = ( 1.0D+00 - x(i)**2 )**3
      end do
      q = r8vec_dot_product ( n, w, f ) / 7.0D+00
      error = abs ( q - exact )
      write ( * , '(2x,a7,6x,i2,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  '   y^6 ', n, exact, q, error

      return
      end
      subroutine chebyshev3_compute_test ( )

c*********************************************************************72
c
cc CHEBYSHEV3_COMPUTE_TEST tests CHEBYSHEV3_COMPUTE
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBYSHEV3_COMPUTE_TEST'
      write ( *, '(a)' ) '  CHEBYSHEV3_COMPUTE computes'
      write ( *, '(a)' ) 
     &  '  a Chebyshev Type 3 quadrature rule over [-1,1]'
      write ( *, '(a)' ) '  of given order.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   Index       X                       W'
      write ( *, '(a)' ) ' '

      do n = 1, 10

        call chebyshev3_compute ( n, x, w )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i2,2x,g25.16,2x,f24.16)' ) i, x(i), w(i)
        end do

      end do

      return
      end
      subroutine clenshaw_curtis_compute_test ( )

c*********************************************************************72
c
cc CLENSHAW_CURTIS_COMPUTE_TEST tests CLENSHAW_CURTIS_COMPUTE
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 April 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CLENSHAW_CURTIS_COMPUTE_TEST'
      write ( *, '(a)' ) '  CLENSHAW_CURTIS_COMPUTE computes'
      write ( *, '(a)' ) 
     &  '  a Clenshaw-Curtis quadrature rule over [-1,1]'
      write ( *, '(a)' ) '  of given order.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   Index       X                       W'
      write ( *, '(a)' ) ' '

      do n = 1, 10

        call clenshaw_curtis_compute ( n, x, w )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i2,2x,g25.16,2x,f24.16)' ) i, x(i), w(i)
        end do

      end do

      return
      end
      subroutine clenshaw_curtis_set_test ( )

c*********************************************************************72
c
cc CLENSHAW_CURTIS_SET_TEST tests CLENSHAW_CURTIS_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision e
      double precision exact
      integer i
      integer n
      double precision q
      double precision v(n_max)
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CLENSHAW_CURTIS_SET_TEST'
      write ( *, '(a)' ) 
     &  '  CLENSHAW_CURTIS_SET sets up a Clenshaw-Curtis rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Estimate the integral of sqrt(abs(x)) over [-1,+1].'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   N           Estimate             Error'
      write ( *, '(a)' ) ' '

      exact = 4.0D+00 / 3.0D+00

      do n = 1, 10

        call clenshaw_curtis_set ( n, x, w )

        q = 0.0D+00
        do i = 1, n
          q = q + w(i) * sqrt ( abs ( x(i) ) )
        end do

        e = abs ( q - exact )

        write ( *, '(2x,i2,2x,g24.16,2x,e14.6)' ) n, q, e

      end do
 
      return
      end
      subroutine fejer1_compute_test ( )

c*********************************************************************72
c
cc FEJER1_COMPUTE_TEST tests FEJER1_COMPUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEJER1_COMPUTE_TEST'
      write ( *, '(a)' ) 
     &  '  FEJER1_COMPUTE computes a Fejer type 1 rule.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Order        W               X'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call fejer1_compute ( n, x, w )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,i8)' ) n

        do i = 1, n
          write ( *, '(10x,2x,g14.6,2x,g14.6)' ) w(i), x(i)
        end do

      end do

      return
      end
      subroutine fejer1_set_test ( )

c*********************************************************************72
c
cc FEJER1_SET_TEST tests FEJER1_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEJER1_SET_TEST'
      write ( *, '(a)' ) '  FEJER1_SET looks up a Fejer type 1 rule.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Order        W               X'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call fejer1_set ( n, x, w )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,i8)' ) n

        do i = 1, n
          write ( *, '(10x,2x,g14.6,2x,g14.6)' ) w(i), x(i)
        end do

      end do

      return
      end
      subroutine fejer2_compute_test ( )

c*********************************************************************72
c
cc FEJER2_COMPUTE_TEST tests FEJER2_COMPUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEJER2_COMPUTE_TEST'
      write ( *, '(a)' ) 
     &  '  FEJER2_COMPUTE computes a Fejer type 2 rule.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Order        W               X'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call fejer2_compute ( n, x, w )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,i8)' ) n

        do i = 1, n
          write ( *, '(10x,2x,g14.6,2x,g14.6)' ) w(i), x(i)
        end do

      end do

      return
      end
      subroutine fejer2_set_test ( )

c*********************************************************************72
c
cc FEJER2_SET_TEST tests FEJER2_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEJER2_SET_TEST'
      write ( *, '(a)' ) '  FEJER2_SET looks up a Fejer type 2 rule.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Order        W               X'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call fejer2_set ( n, x, w )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,i8)' ) n

        do i = 1, n
          write ( *, '(10x,2x,g14.6,2x,g14.6)' ) w(i), x(i)
        end do

      end do

      return
      end
      subroutine gegenbauer_compute_test ( n, alpha )

c*********************************************************************72
c
cc GEGENBAUER_COMPUTE_TEST tests GEGENBAUER_COMPUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Input, double precision ALPHA, the parameter.
c
      implicit none

      integer n

      double precision alpha
      integer i
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEGENBAUER_COMPUTE_TEST'
      write ( *, '(a)' ) 
     &  '  GEGENBAUER_COMPUTE computes a Gauss-Gegenbauer rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The printed output can be inserted'
      write ( *, '(a)' ) '  into a FORTRAN77 program.'

      call gegenbauer_compute ( n, alpha, x, w )
 
      write ( *, '(a)'       ) 'c'
      write ( *, '(a)'       ) 'c  Abscissas X and weights W for'
      write ( *, '(a,i8)'    ) 'c  Gegenbauer rule of order = ', n
      write ( *, '(a,g14.6)' ) 'c  with ALPHA = ', alpha
      write ( *, '(a)'       ) 'c'

      do i = 1, n
        write ( *, '(a,i3,a,d24.16)' ) '      x(', i, ') = ', x(i)
      end do
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(a,i3,a,d24.16)' ) '      w(', i, ') = ', w(i)
      end do

      return
      end
      subroutine test087 ( n )

c*********************************************************************72
c
cc TEST087 tests HERMITE_EK_COMPUTE.
c
c  Discussion:
c
c    I used this test to generate tabular values of weights and
c    abscissas for Gauss-Hermite quadrature.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
      implicit none

      integer n

      integer i
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST087'
      write ( *, '(a)' ) 
     &  '  HERMITE_EK_COMPUTE computes a Gauss-Hermite rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Compute the data for ORDER = ', n

      call hermite_ek_compute ( n, x, w )
 
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(a,i3,a,d40.32)' ) '    x(', i, ') = ', x(i)
      end do
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(a,i3,a,d40.32)' ) '    w(', i, ') = ', w(i)
      end do

      return
      end
      subroutine test089 ( )

c*********************************************************************72
c
cc TEST089 tests HERMITE_PROBABILIST_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer function_num
      parameter ( function_num = 11 )
      integer n_max 
      parameter ( n_max = 10 )

      character * ( 10 ) function_name
      double precision function_value
      external function_value
      integer i
      integer ihi
      integer ilo
      integer j
      integer n
      double precision result(function_num)
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST089'
      write ( *, '(a)' ) 
     &  '  HERMITE_PROBABILIST_SET sets a probabilist Hermite rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The integration interval is ( -oo, +oo ).'
      write ( *, '(a)' ) 
     &  '  Weight function is exp ( - x * x / 2 ) / sqrt ( 2 * pi ).'
      write ( *, '(a)' ) ' '

      do ilo = 1, function_num, 5

        ihi = min ( ilo + 4, function_num )

        write ( *, '(a)' ) ' '
        write ( *, '(2x,a7, 5(a10,4x) )' ) 
     &    'Order  ', ( function_name ( i ), i = ilo, ihi )
        write ( *, '(a)' ) ' '
     
        do n = 1, n_max
     
          call hermite_probabilist_set ( n, x, w )

          do i = ilo, ihi

            call function_set ( 'SET', i )
     
            result(i) = 0.0D+00
            do j = 1, n
              result(i) = result(i) + w(j) * function_value ( x(j) )
            end do
     
          end do

          write ( *, '(2x,i2,2x,5f14.8)' )  n, result(ilo:ihi)
     
        end do
     
      end do

      return
      end
      subroutine test095 ( )

c*********************************************************************72
c
cc TEST095 tests HERMITE_GK16_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 June 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l_max
      parameter ( l_max = 8 )
      integer n_max
      parameter ( n_max = 35 )
 
      double precision error
      double precision estimate
      double precision exact
      double precision f(n_max)
      integer i
      integer j
      integer l
      integer m
      integer n
      integer n_list(0:l_max)
      integer p
      integer p_list(0:l_max)
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      save n_list
      save p_list

      data n_list / 1, 3, 7, 9, 17, 19, 31, 33, 35 /
      data p_list / 1, 5, 7, 15, 17, 29, 31, 33, 51 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST095'
      write ( *, '(a)' ) 
     &  '  HERMITE_GK16_SET sets up a nested rule'
      write ( *, '(a)' ) '  for the Hermite integration problem.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The integration interval is ( -oo, +oo ).'
      write ( *, '(a)' ) '  The weight function is exp ( - x * x )'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  HERMITE_INTEGRAL determines the exact value'
      write ( *, '(a)' ) '  of the integal when f(x) = x^m.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         M         N       Estimate' //
     &  '       Exact            Error'

      do l = 0, l_max

        write ( *, '(a)' ) ' '
        n = n_list(l)
        p = p_list(l)

        call hermite_gk16_set ( n, x, w )

        do m = 0, min ( p + 2, 20 ), 2

          call hermite_integral ( m, exact )

          if ( m .eq. 0 ) then
            do i = 1, n
              f(i) = 1.0D+00
            end do
          else
            do i = 1, n
              f(i) = x(i) ** m
            end do
          end if

          estimate = r8vec_dot_product ( n, w, f )

          error = abs ( exact - estimate )
  
          write ( *, '(2x,i8,2x,i8,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      m, n, estimate, exact, error

        end do

      end do

      return
      end
      subroutine lobatto_compute_test ( )

c*********************************************************************72
c
cc LOBATTO_COMPUTE_TEST tests LOBATTO_COMPUTE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOBATTO_COMPUTE_TEST'
      write ( *, '(a)' ) '  LOBATTO_COMPUTE computes a Lobatto rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I      X             W'
 
      do n = 4, 12, 3

        call lobatto_compute ( n, x, w )

        write ( * , '(a)' ) ' '
        do i = 1, n
          write (  *, '(2x,i8,2x,f12.8,2x,f12.8)' ) i, x(i), w(i)
        end do

      end do
 
      return
      end
      subroutine lobatto_set_test ( )

c*********************************************************************72
c
cc LOBATTO_SET_TEST tests LOBATTO_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOBATTO_SET_TEST'
      write ( *, '(a)' ) '  LOBATTO_SET sets a Lobatto rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I      X             W'
 
      do n = 4, 12, 3

        call lobatto_set ( n, x, w )

        write ( * , '(a)' ) ' '
        do i = 1, n
          write (  *, '(2x,i8,2x,f12.8,2x,f12.8)' ) i, x(i), w(i)
        end do

      end do
 
      return
      end
      subroutine moulton_set_test ( )

c*********************************************************************72
c
cc MOULTON_SET_TEST tests MOULTON_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MOULTON_SET_TEST'
      write ( *, '(a)' ) 
     &  '  MOULTON_SET sets up an Adams-Moulton rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   Index       X                       W'
      write ( *, '(a)' ) ' '

      do n = 1, 10

        call moulton_set ( n, x, w )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i2,2x,g25.16,2x,f24.16)' ) i, x(i), w(i)
        end do

      end do

      return
      end
      subroutine ncc_set_test ( )

c*********************************************************************72
c
cc NCC_SET_TEST tests NCC_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer i
      integer n
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NCC_SET_TEST'
      write ( *, '(a)' ) 
     &  '  NCC_SET sets up a Newton-Cotes Closed rule;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   Index       X                       W'
      write ( *, '(a)' ) ' '

      do n = 1, 10

        call ncc_set ( n, x, w )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i2,2x,g25.16,2x,f24.16)' ) i, x(i), w(i)
        end do

      end do

      return
      end
      function function_name ( function_index )

c*********************************************************************72
c
cc FUNCTION_NAME returns the name of the function evaluated in FUNCTION_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FUNCTION_INDEX, the index of the function.
c
c    Output, character * ( 10 ) FUNCTION_NAME, the name of the function.
c
      implicit none

      integer function_index
      character * ( 10 ) function_name

      if ( function_index == 1 ) then
        function_name = '         1'
      else if ( function_index == 2 ) then
        function_name = '         X'
      else if ( function_index == 3 ) then
        function_name = '       X^2'
      else if ( function_index == 4 ) then
        function_name = '       X^3'
      else if ( function_index == 5 ) then
        function_name = '       X^4'
      else if ( function_index == 6 ) then
        function_name = '       X^5'
      else if ( function_index == 7 ) then
        function_name = '       X^6'
      else if ( function_index == 8 ) then
        function_name = '       X^7'
      else if ( function_index == 9 ) then
        function_name = '    SIN(X)'
      else if ( function_index == 10 ) then
        function_name = '    EXP(X)'
      else if ( function_index == 11 ) then
        function_name = ' SQRT(|X|)'
      else
        function_name = '??????????'
      end if

      return
      end
      subroutine function_set ( action, i )

c*********************************************************************72
c
cc FUNCTION_SET sets the function to be returned by FUNCTION_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( len = * ) ACTION, the action to be carried out.
c    'COUNT' means the call is made to count the number of functions available.
c    'GET' means the call is made to find out the current function index.
c    'SET' means the call is made to set the current function index.
c
c    Input/output, integer I.
c    For 'COUNT', I is output as the number of functions available;
c    For 'GET', I is output as the currently chosen function;
c    For 'SET', I is input as the user's new choice for the function.
c
      implicit none

      character * ( * ) action
      integer i
      integer function_index

      save function_index

      data function_index / -1 /

      if ( action .eq. 'COUNT' ) then
        i = 11
      else if ( action .eq. 'GET' ) then
        i = function_index
      else if ( action .eq. 'SET' ) then
        function_index = i
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FUNCTION_SET - Warningc'
        write ( *, '(a)' ) 
     &    '  Unrecognized action = "' // trim ( action ) // '".'
      end if

      return
      end
      function function_value ( x )

c*********************************************************************72
c
cc FUNCTION_VALUE evaluates a function of X, as chosen by the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision FUNCTION_VALUE, the value of the function.
c
      implicit none

      integer function_index
      double precision function_value
      double precision x

      call function_set ( 'GET', function_index )

      if ( function_index .eq. 1 ) then
        function_value = 1.0D+00
      else if ( function_index .eq. 2 ) then
        function_value = x
      else if ( function_index .eq. 3 ) then
        function_value = x**2
      else if ( function_index .eq. 4 ) then
        function_value = x**3
      else if ( function_index .eq. 5 ) then
        function_value = x**4
      else if ( function_index .eq. 6 ) then
        function_value = x**5
      else if ( function_index .eq. 7 ) then
        function_value = x**6
      else if ( function_index .eq. 8 ) then
        function_value = x**7
      else if ( function_index .eq. 9 ) then
        function_value = sin ( x )
      else if ( function_index .eq. 10 ) then
        function_value = exp ( x )
      else if ( function_index .eq. 11 ) then
        function_value = sqrt ( abs ( x ) )
      else
        function_value = 0.0D+00
      end if

      return
      end
