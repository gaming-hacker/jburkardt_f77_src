      program main

c*********************************************************************72
c
cc MAIN is the main program for LEGENDRE_POLYNOMIAL_PRB.
c
c  Discussion:
c
c    LEGENDRE_POLYNOMIAL_PRB tests the LEGENDRE_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      integer e
      integer p

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the LEGENDRE_POLYNOMIAL library.'

      call legendre_polynomial_test01 ( )
      call legendre_polynomial_test015 ( )
      call legendre_polynomial_test016 ( )

      call legendre_polynomial_test02 ( )
      call legendre_polynomial_test03 ( )
      call legendre_polynomial_test04 ( )

      p = 5
      b = 0.0D+00
      call legendre_polynomial_test05 ( p, b )

      p = 5
      b = 1.0D+00
      call legendre_polynomial_test05 ( p, b )

      p = 5
      e = 0
      call legendre_polynomial_test06 ( p, e )

      p = 5
      e = 1
      call legendre_polynomial_test06 ( p, e )

      call legendre_polynomial_test07 ( )
      call legendre_polynomial_test08 ( )
      call legendre_polynomial_test09 ( )
      call legendre_polynomial_test095 ( )

      p = 5
      call legendre_polynomial_test10 ( p )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine legendre_polynomial_test01 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST01 tests P_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 1 )
      integer n_max
      parameter ( n_max = 10 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      integer n
      double precision v(m,0:n_max)
      double precision x
      double precision x_vec(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST01:'
      write ( *, '(a)' ) '  P_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Legendre polynomial P(n,x).'
      write ( *, '(a)' ) '  P_POLYNOMIAL_VALUE evaluates.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '                        Tabulated                 Computed'
      write ( *, '(a)' ) 
     &  '     N        X           P(N,X)                    P(N,X)' 
     &  // '                     Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call p_polynomial_values ( n_data, n, x, fx1 )

        x_vec(1) = x

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call p_polynomial_value ( m, n, x_vec, v )
        fx2 = v(1,n)

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine legendre_polynomial_test015 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST015 tests P_POLYNOMIAL_PRIME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 11 )
      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      integer i
      integer j
      double precision vp(m,0:n)
      double precision x(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST015:'
      write ( *, '(a)' ) '  P_POLYNOMIAL_PRIME evaluates the derivative'
      write ( *, '(a)' ) '  of the Legendre polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                        Computed'
      write ( *, '(a)' ) '     N        X           P''(N,X)'
      write ( *, '(a)' ) ' '

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )

      call p_polynomial_prime ( m, n, x, vp )

      do i = 1, m
        write ( *, '(a)' ) ' '
        do j = 0, n
          write ( *, '(2x,i4,2x,f12.6,2x,g24.16)' ) j, x(i), vp(i,j)
        end do

      end do

      return
      end
      subroutine legendre_polynomial_test016 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST016 tests P_POLYNOMIAL_PRIME2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 11 )
      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      integer i
      integer j
      double precision vpp(m,0:n)
      double precision x(m)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST016:'
      write ( *, '(a)' ) '  P_POLYNOMIAL_PRIME2 evaluates the second'
      write ( *, '(a)' ) '  derivative of the Legendre polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                        Computed'
      write ( *, '(a)' ) '     N        X           P"(N,X)'
      write ( *, '(a)' ) ' '

      a = -1.0D+00
      b = +1.0D+00
      call r8vec_linspace ( m, a, b, x )

      call p_polynomial_prime2 ( m, n, x, vpp )

      do i = 1, m
        write ( *, '(a)' ) ' '
        do j = 0, n
          write ( *, '(2x,i4,2x,f12.6,2x,g24.16)' ) j, x(i), vpp(i,j)
        end do

      end do

      return
      end
      subroutine legendre_polynomial_test02 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST02 tests P_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision c(0:n,0:n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST02'
      write ( *, '(a)' ) 
     &  '  P_POLYNOMIAL_COEFFICIENTS determines polynomial '
     &  // 'coefficients of P(n,x).'

      call p_polynomial_coefficients ( n, c )
c
c  Because we allocated too much space, we'll need to index
c  the 2D array the hard way!
c
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  P(', i, ',x) = '
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( c(i,j) .eq. 0.0D+00 ) then

          else if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x^', j
          end if
        end do
      end do
     
      return
      end
      subroutine legendre_polynomial_test03 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST03 tests P_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max
      parameter ( degree_max = 5 )

      integer degree
      double precision lz(degree_max,0:degree_max)
      character * ( 80 ) title
      double precision z(degree_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST03:'
      write ( *, '(a)' ) 
     &  '  P_POLYNOMIAL_ZEROS computes the zeros of P(n,x)'
      write ( *, '(a)' ) '  Check by calling P_POLYNOMIAL_VALUE.'

      do degree = 1, degree_max

        call p_polynomial_zeros ( degree, z )
        write ( title, '(a,i1,a)' ) 
     &    '  Computed zeros for P(', degree, ',z):'
        call r8vec_print ( degree, z, title )

        call p_polynomial_value ( degree, degree, z, lz )
        write ( title, '(a,i1,a)' ) '  Evaluate P(', degree, ',z):'
        call r8vec_print ( degree, lz(1:degree,degree), title )

      end do

      return
      end
      subroutine legendre_polynomial_test04 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST04 tests P_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer e
      double precision f(n)
      integer j
      double precision q
      double precision q_exact
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST04:'
      write ( *, '(a)' ) '  P_QUADRATURE_RULE computes the quadrature'
      write ( *, '(a)' ) '  rule associated with P(n,x)'

      call p_quadrature_rule ( n, x, w )

      call r8vec2_print ( n, x, w,  '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Q = Integral ( -1 <= X < +1 ) X^E dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   E       Q_Estimate      Q_Exact'
      write ( *, '(a)' ) ' '

      do e = 0, 2 * n - 1
        if ( e .eq. 0 ) then
          do j = 1, n
            f(j) = 1.0D+00
          end do
        else
          do j = 1, n
            f(j) = x(j)**e
          end do
        end if
        q = 0.0D+00
        do j = 1, n
          q = q + w(j) * f(j)
        end do
        call p_integral ( e, q_exact )
        write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) e, q, q_exact
      end do

      return
      end
      subroutine legendre_polynomial_test05 ( p, b )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST05 tests P_EXPONENTIAL_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the maximum degree of the polynomial 
c    factors.
c
c    Input, double precision B, the coefficient of X in the exponential factor.
c
      implicit none

      integer p

      double precision b
      double precision table(0:p,0:p)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST05'
      write ( *, '(a)' ) 
     &  '  Compute an exponential product table for P(n,x):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Tij = integral ( -1 <= x <= +1 ) exp(b*x) P(i,x) P(j,x) dx'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) 
     &  '  Exponential argument coefficient B = ', b

      call p_exponential_product ( p, b, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Exponential product table:' )

      return
      end
      subroutine legendre_polynomial_test06 ( p, e )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST06 tests P_POWER_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the maximum degree of the polynomial 
c    factors.
c
c    Input, integer E, the exponent of X.
c
      implicit none

      integer p

      integer e
      double precision table(0:p,0:p)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST06'
      write ( *, '(a)' ) 
     &  '  Compute a power product table for P(n,x):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Tij = integral ( -1 <= x <= +1 ) x^e P(i,x) P(j,x) dx'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) '  Exponent of X, E = ', e

      call p_power_product ( p, e, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Power product table:' )

      return
      end
      subroutine legendre_polynomial_test07 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST07 tests PM_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mm
      parameter ( mm = 1 )
      integer n_max
      parameter ( n_max = 10 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      integer m
      integer n
      double precision v(mm,0:n_max)
      double precision x
      double precision x_vec(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST07:'
      write ( *, '(a)' ) '  PM_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Legendre polynomial Pm(n,m,x).'
      write ( *, '(a)' ) '  PM_POLYNOMIAL_VALUE evaluates.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                                '
     &  // 'Tabulated                 Computed'
      write ( *, '(a)' ) '     N     M        X           '
     &  // 'Pm(N,M,X)                 Pm(N,M,X)             Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call pm_polynomial_values ( n_data, n, m, x, fx1 )

        x_vec(1) = x

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call pm_polynomial_value ( mm, n, m, x_vec, v )
        fx2 = v(1,n)

        e = fx1 - fx2

        write ( *, 
     &    '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, m, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine legendre_polynomial_test08 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST08 tests PMN_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mm
      parameter ( mm = 1 )
      integer n_max
      parameter ( n_max = 10 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      integer m
      integer n
      double precision v(mm,0:n_max)
      double precision x
      double precision x_vec(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST08:'
      write ( *, '(a)' ) '  PMN_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Legendre polynomial Pmn(n,m,x).'
      write ( *, '(a)' ) '  PMN_POLYNOMIAL_VALUE evaluates.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                                '
     &  // 'Tabulated                 Computed'
      write ( *, '(a)' ) '     N     M        X           '
     &  // 'Pmn(N,M,X)                Pmn(N,M,X)             Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call pmn_polynomial_values ( n_data, n, m, x, fx1 )

        x_vec(1) = x

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call pmn_polynomial_value ( mm, n, m, x_vec, v )
        fx2 = v(1,n)

        e = fx1 - fx2

        write ( *, 
     &  '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, m, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine legendre_polynomial_test09 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST09 tests PMNS_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mm
      parameter ( mm = 1 )
      integer n_max
      parameter ( n_max = 10 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      integer m
      integer n
      double precision v(mm,0:n_max)
      double precision x
      double precision x_vec(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST09:'
      write ( *, '(a)' ) '  PMNS_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Legendre polynomial Pmns(n,m,x).'
      write ( *, '(a)' ) '  PMNS_POLYNOMIAL_VALUE evaluates.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                                '
     &  // 'Tabulated                 Computed'
      write ( *, '(a)' ) '     N     M        X           '
     &  // 'Pmns(N,M,X)                Pmns(N,M,X)             Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call pmns_polynomial_values ( n_data, n, m, x, fx1 )

        x_vec(1) = x

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call pmns_polynomial_value ( mm, n, m, x_vec, v )
        fx2 = v(1,n)

        e = fx1 - fx2

        write ( *, 
     &    '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, m, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine legendre_polynomial_test095 ( )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST095 tests PN_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision c(0:n,0:n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST095'
      write ( *, '(a)' ) 
     &  '  PN_POLYNOMIAL_COEFFICIENTS determines polynomial '
     &  // 'coefficients of Pn(n,x).'

      call pn_polynomial_coefficients ( n, c )

      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  P(', i, ',x) = '
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( c(i,j) .eq. 0.0D+00 ) then

          else if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x^', j
          end if
        end do
      end do
     
      return
      end
      subroutine legendre_polynomial_test10 ( p )

c*********************************************************************72
c
cc LEGENDRE_POLYNOMIAL_TEST10 tests PN_PAIR_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the maximum degree of the polynomial 
c    factors.
c
      implicit none

      integer p

      double precision table(0:p,0:p)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLYNOMIAL_TEST10'
      write ( *, '(a)' ) '  Compute a pair product table for Pn(n,x):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Tij = integral ( -1 <= x <= +1 ) Pn(i,x) Pn(j,x) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The Pn(n,x) polynomials are orthonormal,'
      write ( *, '(a)' ) '  so T should be the identity matrix.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p

      call pn_pair_product ( p, table )

      call r8mat_print ( p + 1, p + 1, table, '  Pair product table:' )

      return
      end
