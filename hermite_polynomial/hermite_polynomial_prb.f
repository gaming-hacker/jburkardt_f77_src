      program main

c*********************************************************************72
c
cc MAIN is the main program for HERMITE_POLYNOMIAL_PRB.
c
c  Discussion:
c
c    HERMITE_POLYNOMIAL_PRB tests the HERMITE_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the HERMITE_POLYNOMIAL library.'

      call hermite_polynomial_test01 ( )
      call hermite_polynomial_test02 ( )
      call hermite_polynomial_test03 ( )
      call hermite_polynomial_test04 ( )
      call hermite_polynomial_test05 ( )
      call hermite_polynomial_test06 ( )
      call hermite_polynomial_test07 ( )

      p = 5
      b = 0.0D+00
      call hermite_polynomial_test08 ( p, b )

      p = 5
      b = 1.0D+00
      call hermite_polynomial_test08 ( p, b )

      p = 5
      e = 0
      call hermite_polynomial_test09 ( p, e )

      p = 5
      e = 1
      call hermite_polynomial_test09 ( p, e )

      p = 5
      b = 0.0D+00
      call hermite_polynomial_test10 ( p, b )

      p = 5
      b = 1.0D+00
      call hermite_polynomial_test10 ( p, b )

      p = 5
      e = 0
      call hermite_polynomial_test11 ( p, e )

      p = 5
      e = 1
      call hermite_polynomial_test11 ( p, e )

      p = 5
      b = 0.0D+00
      call hermite_polynomial_test12 ( p, b )

      p = 5
      b = 1.0D+00
      call hermite_polynomial_test12 ( p, b )

      p = 5
      e = 0
      call hermite_polynomial_test13 ( p, e )

      p = 5
      e = 1
      call hermite_polynomial_test13 ( p, e )

      call hermite_polynomial_test14 ( )

      call hermite_polynomial_test15 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine hermite_polynomial_test01 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST01 tests H_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      double precision fx2_vec(n_max+1)
      integer n
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST01:'
      write ( *, '(a)' ) '  H_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the physicist''s Hermite polynomials.'
      write ( *, '(a)' ) 
     &  '  H_POLYNOMIAL_VALUE evaluates the polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '                        Tabulated                 Computed'
      write ( *, '(a)' ) 
     &  '     N        X           H(N,X)                    H(N,X)'
     &  // '                     Error'

      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call h_polynomial_values ( n_data, n, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call h_polynomial_value ( 1, n, x, fx2_vec )
        fx2 = fx2_vec(n+1)

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine hermite_polynomial_test02 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST02 tests HE_POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      double precision fx2_vec(n_max+1)
      integer n
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST02:'
      write ( *, '(a)' ) '  HE_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the probabilist''s Hermite polynomials.'
      write ( *, '(a)' ) 
     &  '  HE_POLYNOMIAL_VALUE evaluates the polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '                        Tabulated                 Computed'
      write ( *, '(a)' ) 
     &  '     N        X          He(N,X)' // 
     &  '                   He(N,X)                     Error'

      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call he_polynomial_values ( n_data, n, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call he_polynomial_value ( 1, n, x, fx2_vec )
        fx2 = fx2_vec(n+1)
        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine hermite_polynomial_test03 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST03 tests HF_FUNCTION_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      integer n_data
      double precision e
      double precision fx1
      double precision fx2
      double precision fx2_vec(n_max+1)
      integer n
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST03:'
      write ( *, '(a)' ) '  HF_FUNCTION_VALUES stores values of'
      write ( *, '(a)' ) '  the Hermite function Hf(n,x).'
      write ( *, '(a)' ) '  HF_FUNCTION_VALUE evaluates the function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '                        Tabulated                 Computed'
      write ( *, '(a)' ) 
     &  '     N        X          Hf(N,X)' // 
     &  '                   Hf(N,X)                     Error'

      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hf_function_values ( n_data, n, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call hf_function_value ( 1, n, x, fx2_vec )
        fx2 = fx2_vec(n+1)

        e = fx1 - fx2

        write ( *, '(2x,i4,2x,f12.6,2x,g24.16,2x,g24.16,2x,g8.2)' ) 
     &    n, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine hermite_polynomial_test04 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST04 tests H_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max
      parameter ( degree_max = 5 )

      integer degree
      double precision hz(degree_max,0:degree_max)
      character * ( 80 ) title
      double precision z(degree_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST04:'
      write ( *, '(a)' ) 
     &  '  H_POLYNOMIAL_ZEROS computes the zeros of H(n,x)'
      write ( *, '(a)' ) '  Check by calling H_POLYNOMIAL there.'

      do degree = 1, 5

        call h_polynomial_zeros ( degree, z )
        write ( title, '(a,i1,a)' ) '  Computed zeros for H(', 
     &    degree, ',z):'
        call r8vec_print ( degree, z, title )

        call h_polynomial_value ( degree, degree, z, hz )
        write ( title, '(a,i1,a)' ) '  Evaluate H(', degree, ',z):'
        call r8vec_print ( degree, hz(1:degree,degree), title )

      end do

      return
      end
      subroutine hermite_polynomial_test05 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST05 tests HE_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max
      parameter ( degree_max = 5 )

      integer degree
      double precision hz(degree_max,0:degree_max)
      character * ( 80 ) title
      double precision z(degree_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST05:'
      write ( *, '(a)' ) 
     &  '  HE_POLYNOMIAL_ZEROS computes the zeros of He(n,x)'
      write ( *, '(a)' ) '  Check by calling HE_POLYNOMIAL there.'

      do degree = 1, degree_max

        call he_polynomial_zeros ( degree, z )
        write ( title, '(a,i1,a)' ) '  Computed zeros for He(', 
     &    degree, ',z):'
        call r8vec_print ( degree, z, title )

        call he_polynomial_value ( degree, degree, z, hz )
        write ( title, '(a,i1,a)' ) '  Evaluate He(', degree, ',z):'
        call r8vec_print ( degree, hz(1:degree,degree), title )

      end do

      return
      end
      subroutine hermite_polynomial_test06 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST06 tests H_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST06:'
      write ( *, '(a)' ) '  H_QUADRATURE_RULE computes the quadrature'
      write ( *, '(a)' ) '  rule associated with H(n,x)'

      call h_quadrature_rule ( n, x, w )

      call r8vec2_print ( n, x, w, '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Q = Integral ( -oo < X < +00 ) X^E exp(-X^2) dx'
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
        call h_integral ( e, q_exact )
        write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) e, q, q_exact
      end do

      return
      end
      subroutine hermite_polynomial_test07 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST07 tests HE_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST07:'
      write ( *, '(a)' ) 
     &  '  HE_QUADRATURE_RULE computes the quadrature rule'
      write ( *, '(a)' ) '  associated with He(n,x)'

      call he_quadrature_rule ( n, x, w )

      call r8vec2_print ( n, x, w, '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Q = Integral ( -oo < X < +00 ) X^E exp(-0.5*X^2) dx'
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
        call he_integral ( e, q_exact )
        write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) e, q, q_exact
      end do

      return
      end
      subroutine hermite_polynomial_test08 ( p, b )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST08 tests HN_EXPONENTIAL_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 2012
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST08'
      write ( *, '(a)' ) 
     &  '  Compute a normalized physicist''s Hermite exponential '
     &  // 'product table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( -oo < X < +oo ) ' //
     &  'exp(B*X) Hn(I,X) Hn(J,X) exp(-X*X) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  where Hn(I,X) = normalized physicist''s '
     &  // 'Hermite polynomial of degree I.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) 
     &  '  Exponential argument coefficient B = ', b

      call hn_exponential_product ( p, b, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Exponential product table:' )

      return
      end
      subroutine hermite_polynomial_test09 ( p, e )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST09 tests HN_POWER_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST09'
      write ( *, '(a)' ) 
     &  '  Compute a normalized physicist''s Hermite power product '
     &  // 'table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( -oo < X < +oo ) '
     &  // 'X^E Hn(I,X) Hn(J,X) exp(-X*X) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  where Hn(I,X) = normalized physicist''s '
     &  // 'Hermite polynomial of degree I.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) '  Exponent of X, E = ', e

      call hn_power_product ( p, e, table )

      call r8mat_print ( p + 1, p + 1, table, '  Power product table:' )

      return
      end
      subroutine hermite_polynomial_test10 ( p, b )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST10 tests HEN_EXPONENTIAL_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST10'
      write ( *, '(a)' ) '  Compute a normalized probabilist''s '
     &  // 'Hermite exponential product table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( -oo < X < +oo ) '
     &  // 'exp(B*X) Hen(I,X) Hen(J,X) exp(-X*X) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  where Hen(I,X) = normalized '
     &  // 'probabilist''s Hermite polynomial of degree I.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) '  Exponential argument coefficient B = ', b

      call hen_exponential_product ( p, b, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Exponential product table:' )

      return
      end
      subroutine hermite_polynomial_test11 ( p, e )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST11 tests HEN_POWER_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST11'
      write ( *, '(a)' ) 
     &  '  Compute a normalized probabilist''s Hermite power '
     &  // 'product table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( -oo < X < +oo ) '
     &  // 'X^E Hen(I,X) Hen(J,X) exp(-X*X) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  where Hn(I,X) = normalized probabilist''s Hermite '
     &  // 'polynomial of degree I.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) '  Exponent of X, E = ', e

      call hen_power_product ( p, e, table )

      call r8mat_print ( p + 1, p + 1, table, '  Power product table:' )

      return
      end
      subroutine hermite_polynomial_test12 ( p, b )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST12 tests HF_EXPONENTIAL_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST12'
      write ( *, '(a)' ) 
     &  '  Compute a Hermite function exponential product table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Tij = integral ( -oo < X < +oo ) '
     &  // 'exp(B*X) Hf(I,X) Hf(J,X) exp(-X*X) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  where Hf(I,X) = Hermite function of "degree" I.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) 
     &  '  Exponential argument coefficient B = ', b

      call hf_exponential_product ( p, b, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Exponential product table:' )

      return
      end
      subroutine hermite_polynomial_test13 ( p, e )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST13 tests HF_POWER_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST13'
      write ( *, '(a)' ) 
     &  '  Compute a Hermite function power product table.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Tij = integral ( -oo < X < +oo ) '
     &  // 'X^E Hf(I,X) Hf(J,X) exp(-X*X) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  where Hf(I,X) = Hermite function of "degree" I.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Maximum degree P = ', p
      write ( *, '(a,g14.6)' ) '  Exponent of X, E = ', e

      call hf_power_product ( p, e, table )

      call r8mat_print ( p + 1, p + 1, table, 
     &  '  Power product table:' )

      return
      end
      subroutine hermite_polynomial_test14 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST14 tests H_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST14'
      write ( *, '(a)' ) 
     &  '  H_POLYNOMIAL_COEFFICIENTS determines the physicist''s '
     &  // 'Hermite polynomial coefficients.'

      call h_polynomial_coefficients ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  H(', i, ',x) ='
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
      subroutine hermite_polynomial_test15 ( )

c*********************************************************************72
c
cc HERMITE_POLYNOMIAL_TEST15 tests HE_POLYNOMIAL_COEFFICIENTS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 August 2013
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
      write ( *, '(a)' ) 'HERMITE_POLYNOMIAL_TEST15'
      write ( *, '(a)' ) 
     &  '  HE_POLYNOMIAL_COEFFICIENTS determines the probabilist''s ' 
     &  // 'Hermite polynomial coefficients.'

      call he_polynomial_coefficients ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  He(', i, ',x) ='
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
