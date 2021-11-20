      program main

c*********************************************************************72
c
cc MAIN is the main program for CHEBYSHEV_POLYNOMIAL_PRB.
c
c  Discussion:
c
c    CHEBYSHEV_POLYNOMIAL_PRB tests the CHEBYSHEV_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBYSHEV_POLYNOMIAL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CHEBYSHEV_POLYNOMIAL library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )

      call test10 ( )
      call test11 ( )
      call test12 ( )
      call test13 ( )
      call test14 ( )
      call test15 ( )
      call test16 ( )
      call test17 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBYSHEV_POLYNOMIAL_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests T_PROJECT_COEFFICIENTS_DATA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 20 )
      integer n
      parameter ( n = 4 )

      double precision a
      double precision b
      double precision c(0:n)
      double precision d(m)
      double precision d2(m)
      integer i
      integer seed
      double precision x(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBYSHEV_POLYNOMIAL_TEST01:'
      write ( *, '(a)' ) 
     &  '  T_PROJECT_COEFFICIENTS_DATA estimates the Chebyshev'
      write ( *, '(a)' ) 
     &  '  polynomial coefficients for a function given as data (x,fx).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Here, we use fx = f(x) = x^2 for the data.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Since T(0,x) = 1 and T(2,x) = 2*x^2 - 1, the correct'
      write ( *, '(a)' ) 
     &  '  expansion is f(x) = 1/2 T(0,x) + 0 T(1,x) + 1/2 T(2,x)'
      write ( *, '(a)' ) '  if our polynomials are based in [-1,+1].'
c
c  Data in [0,1];
c
      a = 0.0D+00
      b = 1.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  Data interval [A,B] = [', a, ', ', b, ']'
c
c  Get sample values of X^2 in [A,B].
c
      seed = 123456789
      call r8vec_uniform_ab ( m, a, b, seed, x )
      do i = 1, m
        d(i) = x(i)**2
      end do

      call r8vec2_print ( m, x, d, '  Data ( X, D ):' )

      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      
      call r8vec_print ( n, c, 
     &  '  Coefficients of Chebyshev expansion of degree 4.' )
c
c  Compare Chebyshev expansion and original function.
c
      call t_project_value_ab ( m, n, x, c, a, b, d2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   I      X(I)     Data(I)      Chebyshev(X(I))'
      write ( *, '(a)' ) ' '
      do i = 1, m
        write ( *, '(2x,i2,2x,g12.4,2x,g12.4,2x,g12.4)' ) 
     &    i, x(i), d(i), d2(i)
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests T_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision c(0:n,0:n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  T_POLYNOMIAL_COEFFICIENTS determines coefficients for'
      write ( *, '(a)' ) '  Chebyshev polynomials T(n,x).'

      call t_polynomial_coefficients ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  T(', i, ',x) = '
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( c(i,j) .eq. 0.0D+00 ) then

          else if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x ^ ', j
          end if
        end do
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests T_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  T_POLYNOMIAL evaluates Chebyshev polynomials T(n,x).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                    Tabulated     Computed'
      write ( *, '(a)' ) '     N      X        T(n,x)        T(n,x)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call t_polynomial_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call t_polynomial ( 1, n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests T_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      double precision fx(n_max*(n_max+1))
      integer i
      integer n
      double precision z(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  T_POLYNOMIAL_ZEROS returns zeroes of T(n,x).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X        T(n,x)'

      do n = 1, n_max

        call t_polynomial_zeros ( n, z )

        call t_polynomial ( n, n, z, fx )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(i+n*n_max)
        end do

      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests T_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 7 )

      integer e
      double precision f(n)
      integer i
      double precision q
      double precision q_exact
      double precision r8vec_dot_product
      double precision t_integral
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05:'
      write ( *, '(a)' ) '  T_QUADRATURE_RULE computes the quadrature '
      write ( *, '(a)' ) '  rule associated with T(n,x);'

      call t_quadrature_rule ( n, x, w )

      call r8vec2_print ( n, x, w, '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Q = Integral ( -1 <= X <= +1 ) X^E / sqrt ( 1-x^2) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
      write ( *, '(a)' ) ' '

      do e = 0, 2 * n - 1
        if ( e .eq. 0 ) then
          do i = 1, n
            f(i) = 1.0D+00
          end do
        else
          do i = 1, n
            f(i) = x(i) ** e
          end do
        end if
        q = r8vec_dot_product ( n, w, f )
        q_exact = t_integral ( e )
        write ( *, '(2x,i2,g14.6,2x,g14.6)' ) e, q, q_exact
      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc CHEBYSHEV_POLYNOMIAL_TEST06 tests the projection of T(i,x) and T(j,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision c(0:n)
      integer i
      integer j
      integer k
      double precision phi(1:n+1,0:n)
      double precision phiw(0:n,1:n+1)
      character * ( 80 ) title
      double precision w(n+1)
      double precision x(n+1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06:'
      write ( *, '(a)' ) 
     &  '  As a sanity check, make sure that the projection of:'
      write ( *, '(a)' ) '  T(i,x) onto T(j,x) is:'
      write ( *, '(a)' ) '  0 if i is not equal to j;'
      write ( *, '(a)' ) '  pi if i = j = 0;'
      write ( *, '(a)' ) '  pi/2 if i = j =/= 0.'

      call t_quadrature_rule ( n + 1, x, w )

      call t_polynomial ( n + 1, n, x, phi )

      do k = 0, n

        do j = 1, n + 1
          do i = 0, n
            phiw(i,j) = phi(j,i) * w(j)
          end do
        end do

        call r8mat_mv ( n + 1, n + 1, phiw, phi(:,k), c )

        write ( title, '(a,i2,a)' ) 
     &    '  Chebyshev expansion coefficients for T(', k, ',x)'

        call r8vec_print ( n + 1, c, title )

      end do

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests T_PROJECT_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      double precision a
      double precision b
      double precision c(0:n_max)
      intrinsic dexp
      intrinsic dsin
      intrinsic dsqrt
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07:'
      write ( *, '(a)' ) 
     &  '  T_PROJECT_COEFFICIENTS computes the Chebyshev coefficients'
      write ( *, '(a)' ) '  of a function defined over [-1,+1].'
      write ( *, '(a)' ) '  T_PROJECT_COEFFICIENTS_AB works in [A,B].'

      n = 3
      call t_project_coefficients ( n, dexp, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for exp(x) in [-1,+1]' )

      n = 5
      call t_project_coefficients ( n, dexp, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for exp(x) in [-1,+1]' )

      n = 5
      call t_project_coefficients ( n, dsin, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sin(x) in [-1,+1]' )
c
c  Repeat calculation with T_PROJECT_COEFFICIENTS_AB.
c
      n = 5
      a = -1.0D+00
      b = +1.0D+00
      call t_project_coefficients_ab ( n, dsin, a, b, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sin(x) in [-1,+1]' )
c
c  Now try a different interval.
c
      n = 5
      a = 0.0D+00
      b = 1.0D+00
      call t_project_coefficients_ab ( n, dsqrt, a, b, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sqrt(x) in [0,+1]' )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests T_PROJECT_COEFFICIENTS_DATA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 10 )
      integer n_max
      parameter ( n_max = 5 )

      double precision a
      double precision b
      double precision c(0:n_max)
      double precision d(m)
      integer i
      integer n
      integer seed
      double precision x(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08:'
      write ( *, '(a)' ) 
     &  '  T_PROJECT_COEFFICIENTS_DATA computes the Chebyshev'
      write ( *, '(a)' ) '  coefficients of a function defined by data.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  We are looking for an approximation that is good in [-1,+1].'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Begin by using equally spaced points in [-1,+1].'

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )
      do i = 1, m
        d(i) = exp ( x(i) )
      end do
      n = 3
      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for exp(x) on [-1,+1]' )

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )
      do i = 1, m
        d(i) = exp ( x(i) )
      end do
      n = 5
      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for exp(x) on [-1,+1]' )

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )
      do i = 1, m
        d(i) = sin ( x(i) )
      end do
      n = 5
      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sin(x) on [-1,+1]' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now sample equally spaced points in [0,+1].'
      write ( *, '(a)' ) 
     &  '  The approximation still applies to the interval [-1,+1].'

      a = 0.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )
      do i = 1, m
        d(i) = sin ( x(i) )
      end do
      n = 5

      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sin(x) on [0,+1]' )

      a = 0.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )
      do i = 1, m
        d(i) = sqrt ( x(i) )
      end do
      n = 5
      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sqrt(x) on [0,+1]' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now random points in [-1,+1].'

      a = -1.0D+00
      b = +1.0D+00
      seed = 123456789
      call r8vec_uniform_01 ( m, seed, x )
      x = x * b + ( 1.0D+00 - x ) * a
      do i = 1, m
        d(i) = sin ( x(i) )
      end do
      n = 5
      call t_project_coefficients_data ( a, b, m, n, x, d, c )
      call r8vec_print ( n + 1, c, 
     &  '  Chebyshev coefficients for sin(x) on [-1,+1]' )

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 compares a function and projection over [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 101 )
      integer n_max
      parameter ( n_max = 10 )

      double precision a
      double precision b
      double precision c(0:n_max)
      intrinsic dexp
      integer i
      integer n
      double precision r(m)
      double precision t
      double precision v(m)
      double precision x(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09:'
      write ( *, '(a)' ) 
     &  '  T_PROJECT_COEFFICIENTS computes the Chebyshev interpolant '
      write ( *, '(a)' ) 
     &  '  C(F)(N,X) of a function F(X) defined over [-1,+1].'
      write ( *, '(a)' ) '  T_PROJECT_VALUE evaluates that projection.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Compute projections of order N to exp(x) over [-1,+1],'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N   Max||F(X)-C(F)(N,X)||'
      write ( *, '(a)' ) ' '

      a = -1.0D+00
      b = +1.0D+00

      do n = 0, n_max
        call t_project_coefficients ( n, dexp, c )
        call r8vec_linspace ( m, a, b, x )
        call t_project_value ( m, n, x, c, v )
        do i = 1, m
          r(i) = abs ( v(i) - exp ( x(i) ) )
        end do
        call r8vec_max ( m, r, t )
        write ( *, '(2x,i2,2x,g12.4)' ) n, t
      end do

      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 compares a function and projection over [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 101 )
      integer n_max
      parameter ( n_max = 10 )

      double precision a
      double precision b
      double precision c(0:n_max)
      intrinsic dexp
      integer i
      integer n
      double precision r(m)
      double precision t
      double precision v(m)
      double precision x(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10:'
      write ( *, '(a)' ) 
     &  '  T_PROJECT_COEFFICIENTS_AB computes the Chebyshev interpolant'
      write ( *, '(a)' ) 
     &  '  C(F)(N,X) of a function F(X) defined over [A,B].'
      write ( *, '(a)' ) 
     &  '  T_PROJECT_VALUE_AB evaluates that projection.'

      a = 0.0D+00
      b = 1.5D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,f4.1,a,f4.1,a)' ) 
     &  '  Compute projections of order N to exp(x) over [', 
     &  a, ',', b, ']'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N   Max||F(X)-C(F)(N,X)||'
      write ( *, '(a)' ) ' '

      do n = 0, n_max
        call t_project_coefficients_ab ( n, dexp, a, b, c )
        call r8vec_linspace ( m, a, b, x )
        call t_project_value_ab ( m, n, x, c, a, b, v )
        do i = 1, m
          r(i) = abs ( v(i) - exp ( x(i) ) )
        end do
        call r8vec_max ( m, r, t )
        write ( *, '(2x,i2,2x,g12.4)' ) n, t
      end do

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 tests U_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision c(0:n,0:n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) 
     &  '  U_POLYNOMIAL_COEFFICIENTS determines coefficients'
      write ( *, '(a)' ) '  for Chebyshev polynomials U(n,x).'

      call u_polynomial_coefficients ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  T(', i, ',x) = '
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( c(i,j) .eq. 0.0D+00 ) then

          else if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x ^ ', j
          end if
        end do
      end do

      return
      end
      subroutine test12 ( )

c*********************************************************************72
c
cc TEST12 tests U_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST12:'
      write ( *, '(a)' ) 
     &  '  U_POLYNOMIAL evaluates the Chebyshev polynomials U(n,x).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                    Tabulated     Computed'
      write ( *, '(a)' ) '     N      X        U(n,x)        U(n,x)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call u_polynomial_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call u_polynomial ( 1, n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine test13 ( )

c*********************************************************************72
c
cc TEST13 tests U_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      double precision fx(n_max*(n_max+1))
      integer i
      integer n
      double precision z(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST13:'
      write ( *, '(a)' ) 
     &  '  U_POLYNOMIAL_ZEROS returns zeroes of U(n,x).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X        U(n,x)'

      do n = 1, n_max

        call u_polynomial_zeros ( n, z )

        call u_polynomial ( n, n, z, fx )

        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(i+n*n_max)
        end do

      end do

      return
      end
      subroutine test14 ( )

c*********************************************************************72
c
cc TEST14 tests U_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 7 )

      integer e
      double precision f(n)
      integer i
      double precision q
      double precision q_exact
      double precision r8vec_dot_product
      double precision u_integral
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST14:'
      write ( *, '(a)' ) 
     &  '  U_QUADRATURE_RULE computes the quadrature rule'
      write ( *, '(a)' ) '  associated with U(n,x);'

      call u_quadrature_rule ( n, x, w )

      call r8vec2_print ( n, x, w, '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Q = Integral ( -1 <= X <= +1 ) X^E * sqrt ( 1-x^2) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
      write ( *, '(a)' ) ' '

      do e = 0, 2 * n - 1
        if ( e .eq. 0 ) then
          do i = 1, n
            f(i) = 1.0D+00
          end do
        else
          do i = 1, n
            f(i) = x(i) ** e
          end do
        end if
        q = r8vec_dot_product ( n, w, f )
        q_exact = u_integral ( e )
        write ( *, '(2x,i2,g14.6,2x,g14.6)' ) e, q, q_exact
      end do

      return
      end
      subroutine test15 ( )

c*********************************************************************72
c
cc TEST15 tests V_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST15:'
      write ( *, '(a)' ) 
     &  '  V_POLYNOMIAL evaluates Chebyshev polynomials V(n,x).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                    Tabulated     Computed'
      write ( *, '(a)' ) '     N      X        V(n,x)        V(n,x)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call v_polynomial_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call v_polynomial ( 1, n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine test16 ( )

c*********************************************************************72
c
cc TEST16 tests W_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST16:'
      write ( *, '(a)' ) 
     &  '  W_POLYNOMIAL evaluates Chebyshev polynomials W(n,x).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                    Tabulated     Computed'
      write ( *, '(a)' ) '     N      X        W(n,x)        W(n,x)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call w_polynomial_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call w_polynomial ( 1, n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine test17 ( )

c*********************************************************************72
c
cc TEST17 tests T_TRIPLE_PRODUCT_INTEGRAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 15 )

      double precision fx1
      double precision fx2
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer l
      integer seed
      double precision t_polynomial_value
      double precision t_triple_product_integral
      integer test
      integer test_num
      parameter ( test_num = 20 )
      double precision ti
      double precision tj
      double precision tk
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST17:'
      write ( *, '(a)' ) 
     &  '  T_TRIPLE_PRODUCT_INTEGRAL computes the triple integral'
      write ( *, '(a)' ) 
     &  '  Tijk = integral ( -1 <= x <= 1 ) T(i,x) T(j,x) T(k,x) '
     &  // '/ sqrt ( 1-x^2) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I   J   K     Tijk           Tijk'
      write ( *, '(a)' ) '                 computed       exact'
      write ( *, '(a)' ) ' '

      call t_quadrature_rule ( n, x, w )

      seed = 123456789

      do test = 1, test_num
        i = i4_uniform_ab ( 2, 6, seed )
        j = i4_uniform_ab ( 1, 3, seed )
        k = i4_uniform_ab ( 0, 4, seed )
        fx1 = t_triple_product_integral ( i, j, k )
        fx2 = 0.0D+00
        do l = 1, n
          ti = t_polynomial_value ( i, x(l) )
          tj = t_polynomial_value ( j, x(l) )
          tk = t_polynomial_value ( k, x(l) )
          fx2 = fx2 + w(l) * ti * tj * tk
        end do
        write ( *, '(2x,i2,2x,i2,2x,i2,2x,g14.6,2x,g14.6)' ) 
     &    i, j, k, fx1, fx2
      end do

      return
      end
