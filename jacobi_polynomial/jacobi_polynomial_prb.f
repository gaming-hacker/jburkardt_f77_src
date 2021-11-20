      program main

c*********************************************************************72
c
cc MAIN is the main program for JACOBI_POLYNOMIAL_PRB.
c
c  Discussion:
c
c    JACOBI_POLYNOMIAL_PRB tests the JACOBI_POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_POLYNOMIAL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the JACOBI_POLYNOMIAL library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_POLYNOMIAL_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests J_POLYNOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision a
      double precision b
      double precision e
      double precision fx1
      double precision fx2
      double precision fx2_vec(1,0:n_max)
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  J_POLYNOMIAL_VALUES stores values of'
      write ( *, '(a)' ) '  the Jacobi polynomials.'
      write ( *, '(a)' ) '  J_POLYNOMIAL evaluates the polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '                                    '
     &  // 'Tabulated                 Computed'
      write ( *, '(a)' ) '     N     A     B        X           '
     &  // 'J(N,A,B,X)                    J(N,A,B,X)'
     &  // '                     Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call j_polynomial_values ( n_data, n, a, b, x, fx1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        m = 1
        call j_polynomial ( m, n, a, b, x, fx2_vec )
        fx2 = fx2_vec(1,n)
        e = fx1 - fx2

        write ( *, 
     &    '(2x,i4,2x,f6.2,2x,f6.2,2x,f6.2,2x,g24.16,2x,g24.16,2x,g8.2)'
     &     ) n, a, b, x, fx1, fx2, e

      go to 10

20    continue

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests J_POLYNOMIAL_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max
      parameter ( degree_max = 5 )
      integer test_num
      parameter ( test_num = 3 )

      double precision a
      double precision a_test(test_num)
      double precision b
      double precision b_test(test_num)
      integer degree
      double precision hz(degree_max*(degree_max+1))
      integer test
      character * ( 80 ) title
      double precision z(degree_max)

      save a_test
      save b_test

      data a_test / 0.5D+00, 1.0D+00, 2.0D+00 /
      data b_test / 0.5D+00, 1.5D+00, 0.5D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  J_POLYNOMIAL_ZEROS computes the zeros of J(n,a,b,x);'
      write ( *, '(a)' ) '  Check by calling J_POLYNOMIAL there.'

      do test = 1, 3

        a = a_test(test);
        b = b_test(test);

        do degree = 1, 5

          call j_polynomial_zeros ( degree, a, b, z )
          write ( title, '(a,i1,a,f3.1,a,f3.1,a)' ) 
     &      'Zeros for J(', degree, ',', a, ',', b, ')'
          call r8vec_print ( degree, z, title )

          call j_polynomial ( degree, degree, a, b, z, hz )
          write ( title, '(a,i1,a,f3.1,a,f3.1,a)' ) 
     &      'Evaluate J(', degree, ',', a, ',', b, ')'
          call r8vec_print ( degree, hz(degree*degree+1), 
     &      title )

        end do

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests J_QUADRATURE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 7 )

      double precision a
      double precision b
      integer i
      integer j
      double precision j_double_product_integral
      double precision ji(n,0:5)
      double precision jj(n,0:5)
      integer k
      double precision q
      double precision q_exact
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) '  J_QUADRATURE_RULE computes the quadrature'
      write ( *, '(a)' ) '  rule associated with J(n,a,b,x);'

      a = 1.0D+00
      b = 2.5D+00

      call j_quadrature_rule ( n, a, b, x, w )

      call r8vec2_print ( n, x, w, '      X            W' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use the quadrature rule to estimate:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Q = Integral (-1<x<+1) J(i,a,b,x) '
     &  // 'J(j,a,b,x) (1-x)^a (1+x)^b dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I   J      Q_Estimate         Q_Exact'
      write ( *, '(a)' ) ' '

      do i = 0, 5
        call j_polynomial ( n, i, a, b, x, ji )
        do j = i, 5
          call j_polynomial ( n, j, a, b, x, jj )
          q = 0.0D+00
          do k = 1, n
            q = q + w(k) * ji(k,i) * jj(k,j)
          end do
          q_exact = j_double_product_integral ( i, j, a, b )
          write ( *, '(2x,i2,2x,i2,2x,g14.6,2x,g14.6)' ) 
     &      i, j, q, q_exact
        end do
      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests J_DOUBLE_PRODUCT_INTEGRAL
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      integer i
      integer j
      double precision j_double_product_integral
      double precision q

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  J_DOUBLE_PRODUCT_INTEGRAL returns the value of'
      write ( *, '(a)' ) 
     &  '  the weighted integral of J(i,a,b,x) * J(j,a,b,x);'

      a = 1.0D+00
      b = 2.5D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Q = Integral (-1<x<+1) J(i,a,b,x) '
     &  // 'J(j,a,b,x) (1-x)^a (1+x)^b dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I   J      Q'
      write ( *, '(a)' ) ' '

      do i = 0, 5
        do j = i, 5
          q = j_double_product_integral ( i, j, a, b )
          write ( *, '(2x,i2,2x,i2,2x,g14.6)' ) i, j, q
        end do
      end do

      return
      end
