      program main

c*********************************************************************72
c
cc MAIN is the main program for LAGUERRE_POLYNOMIAL_PRB.
c
c  Discussion:
c
c    LAGUERRE_POLYNOMIAL_PRB tests the LAGUERRE_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the LAGUERRE_POLYNOMIAL library.'

      call laguerre_polynomial_test01 ( )
      call laguerre_polynomial_test02 ( )
      call laguerre_polynomial_test03 ( )
      call laguerre_polynomial_test04 ( )
      call laguerre_polynomial_test05 ( )
      call laguerre_polynomial_test06 ( )

      p = 5
      b = 0.0D+00
      call laguerre_polynomial_test07 ( p, b )

      p = 5
      b = 1.0D+00
      call laguerre_polynomial_test07 ( p, b )

      p = 5
      e = 0
      call laguerre_polynomial_test08 ( p, e )

      p = 5
      e = 1
      call laguerre_polynomial_test08 ( p, e )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine laguerre_polynomial_test01 ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST01 tests L_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 1 )
      integer n_max
      parameter ( n_max = 20 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      integer n
      double precision v(m,n_max)
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST01:'
      write ( *, '(a)' ) '  L_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Laguerre polynomials.'
      write ( *, '(a)' ) '  L_POLYNOMIAL evaluates the polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                        '
     &  // 'Tabulated                 Computed'
      write ( *, '(a)' ) '     N        X           '
     &  // 'L(N,X)                    L(N,X)                     Error'

      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call l_polynomial_values ( n_data, n, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call l_polynomial ( m, n, x, v )
        fx2 = v(1,n+1)

        e = fx1 - fx2

        write ( *, 
     &    '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine laguerre_polynomial_test02 ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST02 tests L_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST02'
      write ( *, '(a)' ) 
     &  '  L_POLYNOMIAL_COEFFICIENTS determines polynomial '
     &  // 'coefficients of L(n,x).'

      call l_polynomial_coefficients ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  L(', i, ') = '
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
      subroutine laguerre_polynomial_test03 ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST03 tests L_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max
      parameter ( degree_max = 5 )

      integer degree
      double precision lz(degree_max*(degree_max+1))
      character * ( 80 ) title
      double precision z(degree_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST03:'
      write ( *, '(a)' ) 
     &  '  L_POLYNOMIAL_ZEROS computes the zeros of L(n,x)'
      write ( *, '(a)' ) '  Check by calling L_POLYNOMIAL there.'

      do degree = 1, degree_max

        call l_polynomial_zeros ( degree, z )
        write ( title, '(a,i1,a)' ) 
     &    '  Computed zeros for L(', degree, ',z):'
        call r8vec_print ( degree, z, title )

        call l_polynomial ( degree, degree, z, lz )
        write ( title, '(a,i1,a)' ) '  Evaluate L(', degree, ',z):'
        call r8vec_print ( degree, lz(degree*degree+1), title )

      end do

      return
      end
      subroutine laguerre_polynomial_test04 ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST04 tests L_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST04:'
      write ( *, '(a)' ) '  L_QUADRATURE_RULE computes the quadrature'
      write ( *, '(a)' ) '  rule associated with L(n,x)'

      call l_quadrature_rule ( n, x, w )

      call r8vec2_print ( n, x, w,  '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Q = Integral ( 0 <= X < +00 ) X^E exp(-X) dx'
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
            f(i) = x(i)**e
          end do
        end if
        q = r8vec_dot_product ( n, w, f )
        call l_integral ( e, q_exact )
        write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) e, q, q_exact
      end do

      return
      end
      subroutine laguerre_polynomial_test05 ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST05 tests LM_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mm
      parameter ( mm = 1 )
      integer n_max
      parameter ( n_max = 20 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      integer m
      integer n
      double precision v(mm,0:n_max)
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST05:'
      write ( *, '(a)' ) '  LM_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Laguerre polynomial Lm(n,m,x)'
      write ( *, '(a)' ) '  LM_POLYNOMIAL evaluates the polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                                 '
     &  // 'Tabulated                 Computed'
      write ( *, '(a)' ) '     N     M        X            '
     &  // 'Lm(N,M,X)                 Lm(N,M,X)               Error'

      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lm_polynomial_values ( n_data, n, m, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call lm_polynomial ( mm, n, m, x, v )
        fx2 = v(1,n)

        e = fx1 - fx2

        write ( *, 
     &    '(2x,i4,2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, m, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine laguerre_polynomial_test06 ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST06 tests LM_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
      integer m

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST06'
      write ( *, '(a)' ) 
     &  '  LM_POLYNOMIAL_COEFFICIENTS determines polynomial '
     &  // 'coefficients of Lm(n,m,x).'

      do m = 0, 4

        call lm_polynomial_coefficients ( n, m, c )
     
        do i = 0, n
          write ( *, '(a)' ) ' '
          write ( *, '(a,i2,a,i2,a)' ) '  Lm(', i, ',', m, ') = '
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

      end do
     
      return
      end
      subroutine laguerre_polynomial_test07 ( p, b )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST07 tests L_EXPONENTIAL_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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

      double precision b
      integer p
      double precision table(0:p,0:p)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST07'
      write ( *, '(a)' ) 
     &  '  Compute an exponential product table for L(n,x):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( 0 <= x < +oo ) '
     &  // 'exp(b*x) Ln(i,x) Ln(j,x) exp(-x) dx'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) 
     &  '  Exponential argument coefficient B = ', b

      call l_exponential_product ( p, b, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Exponential product table:' )

      return
      end
      subroutine laguerre_polynomial_test08 ( p, e )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_TEST08 tests L_POWER_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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

      integer e
      integer p
      double precision table(0:p,0:p)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_TEST08'
      write ( *, '(a)' ) '  Compute a power product table for L(n,x):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( 0 <= x < +oo ) x^e '
     &  // 'L(i,x) L(j,x) exp(-x) dx'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) '  Exponent of X, E = ', e

      call l_power_product ( p, e, table )

      call r8mat_print ( p + 1, p + 1, table, '  Power product table:' )

      return
      end
