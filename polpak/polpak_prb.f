      program main

c*********************************************************************72
c
cc MAIN is the main program for POLPAK_PRB.
c
c  Discussion:
c
c    POLPAK_PRB tests the POLPAK library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLPAK_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the POLPAK library.'

      call agud_test ( )
      call align_enum_test ( )
      call bell_test ( )
      call benford_test ( )
      call bernoulli_number_test ( )
      call bernoulli_number2_test ( )
      call bernoulli_number3_test ( )
      call bernoulli_poly_test ( )
      call bernoulli_poly2_test ( )
      call bernstein_poly_test ( )
      call bpab_test ( )
      call cardan_poly_test ( )
      call cardan_poly_coef_test ( )
      call cardinal_cos_test ( )
      call cardinal_sin_test ( )
      call catalan_test ( )
      call catalan_row_next_test ( )
      call charlier_test ( )
      call cheby_t_poly_test ( )
      call cheby_t_poly_coef_test ( )
      call cheby_t_poly_zero_test ( )
      call cheby_u_poly_test ( )
      call cheby_u_poly_coef_test ( )
      call cheby_u_poly_zero_test ( )
      call chebyshev_discrete_test ( )
      call collatz_count_test ( )
      call collatz_count_max_test ( )
      call comb_row_next_test ( )
      call commul_test ( )
      call complete_symmetric_poly_test ( )
      call cos_power_int_test ( )
      call delannoy_test ( )
      call euler_number_test ( )
      call euler_number2_test ( )
      call euler_poly_test ( )
      call eulerian_test ( )
      call fibonacci_direct_test ( )
      call fibonacci_floor_test ( )
      call gegenbauer_poly_test ( )
      call gen_hermite_poly_test ( )
      call gen_laguerre_poly_test ( )
      call gud_test ( )
      call hermite_poly_phys_test ( )
      call hermite_poly_phys_coef_test ( )
      call i4_choose_test ( )
      call i4_factor_test ( )
      call i4_factorial_test ( )
      call i4_factorial2_test ( )
      call i4_is_triangular_test ()
      call i4_partition_distinct_count_test ( )
      call i4_to_triangle_lower_test ( )
      call jacobi_poly_test ( )
      call jacobi_symbol_test ( )
      call krawtchouk_test ( )
      call laguerre_associated_test ( )
      call laguerre_poly_test ( )
      call laguerre_poly_coef_test ( )
      call lambert_w_test ( )
      call lambert_w_crude_test ( )
      call legendre_associated_test ( )
      call legendre_associated_normalized_test ( )
      call legendre_function_q_test ( )
      call legendre_poly_test ( )
      call legendre_poly_coef_test ( )
      call legendre_symbol_test ( )
      call lerch_test ( )
      call lock_test ( )
      call meixner_test ( )
      call mertens_test ( )
      call moebius_test ( )
      call motzkin_test ( )
      call normal_01_cdf_inverse_test ( )
      call omega_test ( )
      call pentagon_num_test ( )
      call phi_test ( )
      call plane_partition_num_test ( )
      call poly_bernoulli_test ( )
      call poly_coef_count_test ( )
      call prime_test ( )
      call pyramid_num_test ( )
      call pyramid_square_num_test ( )
      call r8_agm_test ( )
      call r8_beta_test ( )
      call r8_choose_test ( )
      call r8_erf_test ( )
      call r8_erf_inverse_test ( )
      call r8_euler_constant_test ( )
      call r8_factorial_test ( )
      call r8_factorial_log_test ( )
      call r8_hyper_2f1_test ( )
      call r8_psi_test ( )
      call r8poly_degree_test ( )
      call r8poly_print_test ( )
      call r8poly_value_horner_test ( )
      call sigma_test ( )
      call simplex_num_test ( )
      call sin_power_int_test ( )
      call slice_test ( )
      call spherical_harmonic_test ( )
      call stirling1_test ( )
      call stirling2_test ( )
      call tau_test ( )
      call tetrahedron_num_test ( )
      call triangle_num_test ( )
      call triangle_lower_to_i4_test ( )
      call trinomial_test ( )
      call vibonacci_test ( )
      call zeckendorf_test ( )
      call zernike_poly_test ( )
      call zernike_poly_coef_test ( )
      call zeta_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLPAK_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine agud_test ( )

c*********************************************************************72
c
cc AGUD_TEST tests AGUD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    06 October 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision agud
      double precision g
      double precision gud
      integer i
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AGUD_TEST'
      write ( *, '(a)' ) '  AGUD computes the inverse Gudermannian;'
      write ( *, '(a)' ) '  GUD computes the Gudermannian.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            GUD(X)     AGUD(GUD(X))'
      write ( *, '(a)' ) ' '

      do i = 0, 10
        x = 1.0D+00 + dble ( i ) / 5.0D+00
        g = gud ( x )
        x2 = agud ( g )
        write ( *, '(2x,3g14.6)' ) x, g, x2
      end do

      return
      end
      subroutine align_enum_test ( )

c*********************************************************************72
c
cc ALIGN_ENUM_TEST tests ALIGN_ENUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m_max
      parameter ( m_max = 10 )
      integer n_max
      parameter ( n_max = 10 )

      integer align_enum
      integer i
      integer j
      integer table(0:m_max,0:n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ALIGN_ENUM_TEST'
      write ( *, '(a)' ) '  ALIGN_ENUM counts the number of possible'
      write ( *, '(a)' ) '  alignments of two biological sequences.'

      do i = 0, m_max
        do j = 0, n_max
          table(i,j) = align_enum ( i, j )
        end do
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Alignment enumeration table:'
      write ( *, '(a)' ) ' '
      write ( *, '(4x,5i5,6i8)' ) ( i, i = 0, n_max )
      write ( *, '(a)' ) ' '
      do i = 0, m_max
        write ( *, '(2x,i2,5i5,6i8)' ) i, table(i,0:n_max)
      end do

      return
      end
      subroutine bell_test ( )

c***********************************************************************72
c
cc BELL_TEST tests BELL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2(0:10)
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BELL_TEST'
      write ( *, '(a)' ) '  BELL computes Bell numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N  exact C(I)  computed C(I)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bell_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call bell ( n, c2 )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2(n)

      go to 10

20    continue
 
      return
      end
      subroutine benford_test ( )

c*********************************************************************72
c
cc BENFORD_TEST tests BENFORD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    04 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision benford
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BENFORD_TEST'
      write ( *, '(a)' ) '  BENFORD(I) is the Benford probability of'
      write ( *, '(a)' ) '  the initial digit sequence I.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I,  BENFORD(I)'
      write ( *, '(a)' ) ' '

      do i = 1, 9
        write ( *, '(2x,i2,2x,g14.6)' )  i, benford(i)
      end do

      return
      end
      subroutine bernoulli_number_test ( )

c*********************************************************************72
c
cc BERNOULLI_NUMBER_TEST tests BERNOULLI_NUMBER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c0
      double precision c1(0:30)
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_NUMBER_TEST'
      write ( *, '(a)' ) 
     &  '  BERNOULLI_NUMBER computes Bernoulli numbers;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I      Exact     Bernoulli'
      write ( *, '(a)' ) ' '
  
      n_data = 0

10    continue

        call bernoulli_number_values ( n_data, n, c0 )

        if ( n_data == 0 ) then
          go to 20
        end if

        call bernoulli_number ( n, c1 )

        write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) n, c0, c1(n)

      go to 10

20    continue
 
      return
      end
      subroutine bernoulli_number2_test ( )

c*********************************************************************72
c
cc BERNOULLI_NUMBER2_TEST tests BERNOULLI_NUMBER2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c0
      double precision c1(0:30)
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_NUMBER2_TEST'
      write ( *, '(a)' ) 
     &  '  BERNOULLI_NUMBER2 computes Bernoulli numbers;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I      Exact     Bernoulli2'
      write ( *, '(a)' ) ' '
  
      n_data = 0

10    continue

        call bernoulli_number_values ( n_data, n, c0 )

        if ( n_data == 0 ) then
          go to 20
        end if

        call bernoulli_number2 ( n, c1 )
 
        write ( *, '(2x,i4,2g14.6)' ) n, c0, c1(n)

      go to 10

20    continue
 
      return
      end
      subroutine bernoulli_number3_test ( )

c*********************************************************************72
c
cc BERNOULLI_NUMBER3_TEST tests BERNOULLI_NUMBER3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c0
      double precision c1
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_NUMBER3_TEST'
      write ( *, '(a)' ) 
     &  '  BERNOULLI_NUMBER3 computes Bernoulli numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   I      Exact     BERNOULLI3'
      write ( *, '(a)' ) ' '
  
      n_data = 0

10    continue

        call bernoulli_number_values ( n_data, n, c0 )

        if ( n_data == 0 ) then
          go to 20
        end if

        call bernoulli_number3 ( n, c1 )

        write ( *, '(2x,i4,2g14.6)' ) n, c0, c1

      go to 10

20    continue
 
      return
      end
      subroutine bernoulli_poly_test ( )

c*********************************************************************72
c
cc BERNOULLI_POLY_TEST tests BERNOULLI_POLY;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 February 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision bx
      integer i
      integer n
      parameter ( n = 15 )
      double precision x

      x = 0.2D+00
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_POLY_TEST'
      write ( *, '(a)' ) 
     &'  BERNOULLI_POLY evaluates Bernoulli polynomials;'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  X = ', x
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I          BX'
      write ( *, '(a)' ) ' '
 
      do i = 1, n
        call bernoulli_poly ( i, x, bx )
        write ( *, '(2x,i2,2x,g16.8)' ) i, bx
      end do
 
      return
      end
      subroutine bernoulli_poly2_test ( )

c*********************************************************************72
c
cc BERNOULLI_POLY2_TEST tests BERNOULLI_POLY2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 July 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision bx
      integer i
      integer n
      parameter ( n = 15 )
      double precision x

      x = 0.2D+00
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_POLY2_TEST'
      write ( *, '(a)' ) 
     &  '  BERNOULLI_POLY2 evaluates Bernoulli polynomials. '
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  X = ', x
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I          BX'
      write ( *, '(a)' ) ' '
 
      do i = 1, n
        call bernoulli_poly2 ( i, x, bx )
        write ( *, '(2x,i2,2x,2g16.8)' ) i, bx
      end do
 
      return
      end
      subroutine bernstein_poly_test ( )

c*********************************************************************72
c
cc BERNSTEIN_POLY_TEST tests BERNSTEIN_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision bvec(0:10)
      integer k
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNSTEIN_POLY_TEST:'
      write ( *, '(a)' ) 
     &  '  BERNSTEIN_POLY evaluates the Bernstein polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N   K   X   Exact   B(N,K)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bernstein_poly_values ( n_data, n, k, x, b )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call bernstein_poly ( n, x, bvec )

        write ( *, '(2x,i4,i4,f7.4,2g14.6)' ) n, k, x, b, bvec(k)

      go to 10

20    continue

      return
      end
      subroutine bpab_test ( )

c*********************************************************************72
c
cc BPAB_TEST tests BPAB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a
      double precision b
      double precision bern(0:n)
      integer i
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BPAB_TEST'
      write ( *, '(a)' ) '  BPAB evaluates Bernstein polynomials.'
      write ( *, '(a)' ) ' '

      x = 0.3D+00
      a = 0.0D+00
      b = 1.0D+00
      call bpab ( n, x, a, b, bern )
 
      write ( *, '(a,i4)' ) '  The Bernstein polynomials of degree ', n
      write ( *, '(a,g14.6)' ) '  based on the interval from ', a
      write ( *, '(a,g14.6)' ) '  to ', b
      write ( *, '(a,g14.6)' ) '  evaluated at X = ', x
      write ( *, '(a)' ) ' '
 
      do i = 0, n
        write ( *, '(2x,i4,2x,g14.6)' )  i, bern(i)
      end do
 
      return
      end
      subroutine cardan_poly_test ( )

c*********************************************************************72
c
cc CARDAN_POLY_TEST tests CARDAN_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision c(0:n_max)
      double precision cx1
      double precision cx2(0:n_max)
      integer n
      double precision r8poly_value_horner
      double precision s
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CARDAN_POLY_TEST'
      write ( *, '(a)' ) 
     &  '  CARDAN_POLY evaluates a Cardan polynomial directly.'

      n = n_max
      s = 0.5D+00
      x = 0.25D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Compare CARDAN_POLY_COEF + R8POLY_VALUE_HORNER'
      write ( *, '(a)' ) '  versus CARDAN_POLY alone.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Evaluate polynomials at X = ', x
      write ( *, '(a,g14.6)' ) '  We use the parameter S = ', s
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Order, Horner, Direct'
      write ( *, '(a)' ) ' '

      call cardan_poly ( n, x, s, cx2 )

      do n = 0, n_max

        call cardan_poly_coef ( n, s, c )
        cx1 = r8poly_value_horner ( n, c, x )

        write ( *, '(2x,i2,2g14.6)' ) n, cx1, cx2(n)

      end do

      return
      end
      subroutine cardan_poly_coef_test ( )

c*********************************************************************72
c
cc CARDAN_POLY_COEF_TEST tests CARDAN_POLY_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision c(0:n_max)
      double precision cx1
      double precision cx2(0:n_max)
      integer n
      double precision s
      double precision x

      s = 1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CARDAN_POLY_COEF_TEST'
      write ( *, '(a)' ) '  CARDAN_POLY_COEF returns the coefficients'
      write ( *, '(a)' ) '  of a Cardan polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  We use the parameter S = ', s
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Table of polynomial coefficients:'
      write ( *, '(a)' ) ' '

      do n = 0, n_max
        call cardan_poly_coef ( n, s, c )
        write ( *, '(2x,i2,11f7.0)' ) n, c(0:n)
      end do

      return
      end
      subroutine cardinal_cos_test ( )

c*********************************************************************72
c
cc CARDINAL_COS_TEST tests CARDINAL_COS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 11 )

      double precision c(0:m+1,0:m+1)
      integer i
      integer j
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision s(0:m+1,0:m+1)
      double precision t(0:m+1)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CARDINAL_COS_TEST'
      write ( *, '(a)' ) 
     &  '  CARDINAL_COS evaluates cardinal cosine functions.'
      write ( *, '(a)' ) 
     &  '  Ci(Tj) = Delta(i,j), where Tj = cos(pi*i/(n+1)).'
      write ( *, '(a)' ) 
     &  '  A simple check of all pairs should form the identity matrix.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  The CARDINAL_COS test matrix:'
      write ( *, '(a)' ) ''

      call r8vec_linspace ( m + 2, 0.0D+00, r8_pi, t )

      do j = 0, m + 1
        call cardinal_cos ( j, m, m + 2, t, c(0:m+1,j) )
      end do

      do i = 0, m + 1
        write ( *, '(13(2x,f4.1))' ) c(i,0:m+1)
      end do

      return
      end
      subroutine cardinal_sin_test ( )

c*********************************************************************72
c
cc CARDINAL_SIN_TEST tests CARDINAL_SIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 11 )

      integer i
      integer j
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision s(0:m+1,0:m+1)
      double precision t(0:m+1)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CARDINAL_SIN_TEST'
      write ( *, '(a)' ) 
     &  '  CARDINAL_SIN evaluates cardinal sine functions.'
      write ( *, '(a)' ) 
     &  '  Si(Tj) = Delta(i,j), where Tj = cos(pi*i/(n+1)).'
      write ( *, '(a)' ) 
     &  '  A simple check of all pairs should form the identity matrix.'

      call r8vec_linspace ( m + 2, 0.0D+00, r8_pi, t )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  The CARDINAL_SIN test matrix:'
      write ( *, '(a)' ) ''
      do j = 0, m + 1
        call cardinal_sin ( j, m, m + 2, t, s(0:m+1,j) )
      end do

      do i = 0, m + 1
        write ( *, '(13(2x,f4.1))' ) s(i,0:m+1)
      end do

      return
      end
      subroutine catalan_test ( )

c*********************************************************************72
c
cc CATALAN_TEST tests CATALAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2(0:10)
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CATALAN_TEST'
      write ( *, '(a)' ) '  CATALAN computes Catalan numbers.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '  N  exact C(I)  computed C(I)'
      write ( *, '(a)' ) ' '
 
      n_data = 0

10    continue

        call catalan_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call catalan ( n, c2 )
 
        write ( *, '(2x,i4,2i8)' ) n, c, c2(n)

      go to 10
 
20    continue

      return
      end
      subroutine catalan_row_next_test ( )

c*********************************************************************72
c
cc CATALAN_ROW_NEXT_TEST tests CATALAN_ROW_NEXT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer c(0:n)
      integer i
      integer ido

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CATALAN_ROW_NEXT_TEST'
      write ( *, '(a)' ) 
     &  '  CATALAN_ROW_NEXT computes a row of Catalan''s triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  First, compute row 7:'

      ido = 0
      i = 7
      call catalan_row_next ( ido, i, c )
      write ( *, '(2x,i2,2x,11i6)' ) i, c(0:i)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now compute rows one at a time:'
      write ( *, '(a)' ) ' '

      ido = 0
 
      do i = 0, n
        call catalan_row_next ( ido, i, c )
        ido = 1
        write ( *, '(2x,i2,2x,11i6)' ) i, c(0:i)
      end do
 
      return
      end
      subroutine charlier_test ( )

c*********************************************************************72
c
cc CHARLIER_TEST tests CHARLIER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer test_num
      parameter ( test_num = 5 )

      double precision a
      double precision a_test(test_num)
      integer i
      integer j
      integer test
      double precision x
      double precision value(0:n)

      save a_test

      data a_test / 0.25D+00, 0.5D+00, 1.0D+00, 2.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHARLIER_TEST:'
      write ( *, '(a)' ) '  CHARLIER evaluates Charlier polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      A         X        P(N,A,X)'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        a = a_test(test)

        write ( *, '(a)' ) ' '

        do j = 0, 5

          x = dble ( j ) / 2.0D+00

          call charlier ( n, a, x, value )

          write ( *, '(a)' ) ' '

          do i = 0, n

            write ( *, '(2x,i8,2x,f8.4,2x,f8.4,2x,g14.6)' ) 
     &        i, a, x, value(i)

          end do

        end do

      end do

      return
      end
      subroutine cheby_t_poly_test ( )

c*********************************************************************72
c
cc CHEBY_T_POLY_TEST tests CHEBY_T_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_T_POLY_TEST:'
      write ( *, '(a)' ) 
     &  '  CHEBY_T_POLY evaluates the Chebyshev T polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N      X        Exact F       T(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cheby_t_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call cheby_t_poly ( 1, n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine cheby_t_poly_coef_test ( )

c*********************************************************************72
c
cc CHEBY_T_POLY_COEF_TEST tests CHEBY_T_POLY_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
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
      integer  j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_T_POLY_COEF_TEST'
      write ( *, '(a)' ) '  CHEBY_T_POLY_COEF determines ' // 
     &  'the Chebyshev T polynomial coefficients.'

      call cheby_t_poly_coef ( n, c )
 
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  T(', i, ')'
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x**', j
          end if
        end do
      end do
 
      return
      end
      subroutine cheby_t_poly_zero_test ( )

c*********************************************************************72
c
cc CHEBY_T_POLY_ZERO_TEST tests CHEBY_T_POLY_ZERO.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 4 )

      double precision fx(0:n_max)
      integer i
      integer n
      double precision z(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_T_POLY_ZERO_TEST:'
      write ( *, '(a)' ) 
     &  '  CHEBY_T_POLY_ZERO returns zeroes of the T(N)(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X        T(N)(X)'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call cheby_t_poly_zero ( n, z )

        do i = 1, n

          call cheby_t_poly ( 1, n, z(i), fx )

          write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(n)

        end do

        write ( *, '(a)' ) ' '

      end do

      return
      end
      subroutine cheby_u_poly_test ( )

c*********************************************************************72
c
cc CHEBY_U_POLY_TEST tests CHEBY_U_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_U_POLY_TEST:'
      write ( *, '(a)' ) 
     &  '  CHEBY_U_POLY evaluates the Chebyshev U polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N      X        Exact F       U(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cheby_u_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call cheby_u_poly ( 1, n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine cheby_u_poly_coef_test ( )

c*********************************************************************72
c
cc CHEBY_U_POLY_COEF_TEST tests CHEBY_U_POLY_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
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
      integer  j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_U_POLY_COEF_TEST'
      write ( *, '(a)' ) '  CHEBY_U_POLY_COEF determines ' // 
     &  'the Chebyshev U polynomial coefficients.'

      call cheby_u_poly_coef ( n, c )
 
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  T(', i, ')'
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x**', j
          end if
        end do
      end do
 
      return
      end
      subroutine cheby_u_poly_zero_test ( )

c*********************************************************************72
c
cc CHEBY_U_POLY_ZERO_TEST tests CHEBY_U_POLY_ZERO.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 4 )

      double precision fx(0:n_max)
      integer i
      integer n
      double precision z(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_U_POLY_ZERO_TEST:'
      write ( *, '(a)' ) 
     &  '  CHEBY_U_POLY_ZERO returns zeroes of the U(N)(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X        U(N)(X)'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call cheby_u_poly_zero ( n, z )

        do i = 1, n

          call cheby_u_poly ( 1, n, z(i), fx )

          write ( *, '(2x,i8,2x,f8.4,2x,g14.6)' ) n, z(i), fx(n)

        end do

        write ( *, '(a)' ) ' '

      end do

      return
      end
      subroutine chebyshev_discrete_test ( )

c*********************************************************************72
c
cc CHEBYSHEV_DISCRETE_TEST tests CHEBYSHEV_DISCRETE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer i
      integer j
      integer m
      double precision x
      double precision value(0:n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBYSHEV_DISCRETE_TEST:'
      write ( *, '(a)' ) 
     &  '  CHEBYSHEV_DISCRETE evaluates discrete Chebyshev polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      M         X        T(N,M,X)'
      write ( *, '(a)' ) ' '

      m = 5

      do j = 0, 5

        x = dble ( j ) / 2.0D+00

        call chebyshev_discrete ( n, m, x, value )

        write ( *, '(a)' ) ' '

        do i = 0, n

          write ( *, '(2x,i8,2x,i8,2x,f8.4,2x,g14.6)' ) 
     &      i, m, x, value(i)

        end do

      end do

      return
      end
      subroutine collatz_count_test ( )

c*********************************************************************72
c
cc COLLATZ_COUNT_TEST tests COLLATZ_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer collatz_count
      integer count
      integer count2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COLLATZ_COUNT_TEST:'
      write ( *, '(a)' ) '  COLLATZ_COUNT(N) counts the length of the'
      write ( *, '(a)' ) '  Collatz sequence beginning with N.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       COUNT(N)     COUNT(N)'
      write ( *, '(a)' ) '              (computed)    (table)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call collatz_count_values ( n_data, n, count )

        if ( n_data .eq. 0 ) then
          go to 20
        end if
 
        count2 = collatz_count ( n )

        write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, count, count2

      go to 10

20    continue

      return
      end
      subroutine collatz_count_max_test ( )

c*********************************************************************72
c
cc COLLATZ_COUNT_MAX_TEST tests COLLATZ_COUNT_MAX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 April 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i_max
      integer j_max
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COLLATZ_COUNT_MAX_TEST:'
      write ( *, '(a)' ) '  COLLATZ_COUNT_MAX(N) returns the length of'
      write ( *, '(a)' ) '  the longest Collatz sequence from 1 to N.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N     I_MAX     J_MAX'
      write ( *, '(a)' ) ' '

      n = 10

10    continue

      if ( n <= 100000 ) then

        call collatz_count_max ( n, i_max, j_max )

        write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, i_max, j_max

        n = n * 10

        go to 10

      end if

      return
      end
      subroutine comb_row_next_test ( )

c*********************************************************************72
c
cc COMB_ROW_NEXT_TEST tests COMB_ROW_NEXT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      integer c(0:n_max)
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COMB_ROW_NEXT_TEST'
      write ( *, '(a)' ) 
     &  '  COMB_ROW_NEXT computes the next row of Pascal''s triangle.'
      write ( *, '(a)' ) ' '
  
      do n = 0, n_max
        call comb_row_next ( n, c )
        write ( *, '(2x,i2,2x,11i5)' ) n, c(0:n)
      end do
 
      return
      end
      subroutine commul_test ( )

c*********************************************************************72
c
cc COMMUL_TEST tests COMMUL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer factor(4)
      integer i
      integer ncomb
      integer nfactor

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COMMUL_TEST'
      write ( *, '(a)' ) '  COMMUL computes a multinomial coefficient.'
      write ( *, '(a)' ) ' '

      n = 8
      nfactor = 2
      factor(1) = 6
      factor(2) = 2

      call commul ( n, nfactor, factor, ncomb ) 

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  N = ', n
      write ( *, '(a,i8)' ) '  Number of factors = ', nfactor
      do i = 1, nfactor
        write ( *, '(2x,i2,2x,i8)' ) i, factor(i)
      end do
      write ( *, '(a,i12)' ) '  Value of coefficient = ', ncomb

      n = 8
      nfactor = 3
      factor(1) = 2
      factor(2) = 2
      factor(3) = 4
      call commul ( n, nfactor, factor, ncomb )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  N = ', n
      write ( *, '(a,i8)' ) '  Number of factors = ', nfactor
      do i = 1, nfactor
        write ( *, '(2x,i2,2x,i8)' ) i, factor(i)
      end do
      write ( *, '(a,i12)' ) '  Value of coefficient = ', ncomb

      n = 13
      nfactor = 4
      factor(1) = 5
      factor(2) = 3
      factor(3) = 3
      factor(4) = 2
      call commul ( n, nfactor, factor, ncomb )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  N = ', n
      write ( *, '(a,i8)' ) '  Number of factors = ', nfactor
      do i = 1, nfactor
        write ( *, '(2x,i2,2x,i8)' ) i, factor(i)
      end do
      write ( *, '(a,i12)' ) '  Value of coefficient = ', ncomb

      return
      end
      subroutine complete_symmetric_poly_test ( )

c*********************************************************************72
c
cc COMPLETE_SYMMETRIC_POLY_TEST tests COMPLETE_SYMMETRIC_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer nn
      integer r
      integer rr
      double precision tau(0:5)
      double precision value
      double precision x(n)

      save x

      data x / 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'COMPLETE_SYMMETRIC_POLY_TEST'
      write ( *, '(a)' ) 
     &  '  COMPLETE_SYMMETRIC_POLY evaluates a complete symmetric.'
      write ( *, '(a)' ) '  polynomial in a given set of variables X.'
 
      call r8vec_print ( n, x, '  Variable vector X:' );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '   N\R    0       1       2       3       4       5'
      write ( *, '(a)' ) ''

      do nn = 0, n
        do rr = 0, 5
          call complete_symmetric_poly ( nn, rr, x, value )
          tau(rr) = value
        end do
        write ( *, '(2x,i2,6(2x,f6.0))' ) n, tau(0:5)
      end do

      return
      end
      subroutine cos_power_int_test ( )

c*********************************************************************72
c
cc COS_POWER_INT_TEST tests COS_POWER_INT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    31 March 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision cos_power_int
      double precision fx
      double precision fx2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COS_POWER_INT_TEST:'
      write ( *, '(a)' ) '  COS_POWER_INT returns values of '
      write ( *, '(a)' ) '  the integral of COS(X)^N from A to B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      A         B          N      Exact           Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cos_power_int_values ( n_data, a, b, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = cos_power_int ( a, b, n )

        write ( *, '(2x,f8.4,2x,f8.4,2x,i8,2x,g14.6,2x,g14.6)' ) 
     &    a, b, n, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine delannoy_test ( )

c*********************************************************************72
c
cc DELANNOY_TEST tests DELANNOY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 8 )
      integer n
      parameter ( n = 8 )

      integer a(0:m,0:n)
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DELANNOY_TEST'
      write ( *, '(a)' ) 
     &  '  DELANNOY computes the Delannoy numbers A(0:M,0:N).'
      write ( *, '(a)' ) 
     &  '  A(M,N) counts the paths from (0,0) to (M,N).'
      write ( *, '(a)' ) ' '

      call delannoy ( m, n, a )

      do i = 0, m
        write ( *, '(2x,i4,2x,5i4,3i8,i10)' )  i, a(i,0:n)
      end do
  
      return
      end
      subroutine euler_number_test ( )

c*********************************************************************72
c
cc EULER_NUMBER_TEST tests EULER_NUMBER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c1
      integer c2(0:12)
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EULER_NUMBER_TEST'
      write ( *, '(a)' ) '  EULER_NUMBER computes Euler numbers.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '     N       exact   EULER_NUMBER'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call euler_number_values ( n_data, n, c1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call euler_number ( n, c2 )

        write ( *, '(2x,i4,2i12,g14.6)' ) n, c1, c2(n)

      go to 10

20    continue
 
      return
      end
      subroutine euler_number2_test ( )

c*********************************************************************72
c
cc EULER_NUMBER2_TEST tests EULER_NUMBER2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c1
      double precision c2
      double precision euler_number2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EULER_NUMBER2_TEST'
      write ( *, '(a)' ) 
     &  '  EULER_NUMBER2 computes Euler numbers.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '     N       exact   EULER_NUMBER2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call euler_number_values ( n_data, n, c1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        c2 = euler_number2 ( n )

        write ( *, '(2x,i4,i12,g14.6)' ) n, c1, c2

      go to 10

20    continue
 
      return
      end
      subroutine euler_poly_test ( )

c*********************************************************************72
c
cc EULER_POLY_TEST tests EULER_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision euler_poly
      double precision f
      integer i
      integer n
      parameter ( n = 15 )
      double precision x

      x = 0.5D+00
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EULER_POLY_TEST'
      write ( *, '(a)' ) '  EULER_POLY evaluates Euler polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N      X             F(X)'
      write ( *, '(a)' ) ' '
   
      do i = 0, n
        f = euler_poly ( i, x )
        write ( *, '(2x,i2,2x,2g14.6)' ) i, x, f
      end do
 
      return
      end
      subroutine eulerian_test ( )

c*********************************************************************72
c
cc EULERIAN_TEST tests EULERIAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 7 )

      integer e(n,n)
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EULERIAN_TEST'
      write ( *, '(a)' ) '  EULERIAN evaluates Eulerian numbers.'
      write ( *, '(a)' ) ' '
 
      call eulerian ( n, e )

      do i = 1, n
        write ( *, '(2x,10i6)' )  e(i,1:n)
      end do
 
      return
      end
      subroutine fibonacci_direct_test ( )

c*********************************************************************72
c
cc FIBONACCI_DIRECT_TEST tests FIBONACCI_DIRECT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f
      integer i
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FIBONACCI_DIRECT_TEST'
      write ( *, '(a)' ) 
     &  '  FIBONACCI_DIRECT evalutes a Fibonacci number directly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       I        F(I)'
      write ( *, '(a)' ) ' '

      n = 20
     
      do i = 1, n
        call fibonacci_direct ( i, f )
        write ( *, '(2x,i8,i10)' ) i, f
      end do
     
      return
      end
      subroutine fibonacci_floor_test ( )

c*********************************************************************72
c
cc FIBONACCI_FLOOR_TEST tests FIBONACCI_FLOOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f
      integer i
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FIBONACCI_FLOOR_TEST'
      write ( *, '(a)' ) 
     &  '  FIBONACCI_FLOOR computes the largest Fibonacci number'
      write ( *, '(a)' ) 
     &  '  less than or equal to a given positive integer.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N  Fibonacci  Index'
      write ( *, '(a)' ) ' ' 

      do n = 1, 20
        call fibonacci_floor ( n, f, i )
        write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, f, i
      end do
     
      return
      end
      subroutine gegenbauer_poly_test ( )

c*********************************************************************72
c
cc GEGENBAUER_POLY_TEST tests GEGENBAUER_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a
      double precision c(0:n_max)
      double precision fx
      double precision fx2
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEGENBAUER_POLY_TEST:'
      write ( *, '(a)' ) '  GEGENBAUER_POLY computes values of '
      write ( *, '(a)' ) '  the Gegenbauer polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N        A           X       GPV      GEGENBAUER'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gegenbauer_poly_values ( n_data, n, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call gegenbauer_poly ( n, a, x, c )
        fx2 = c(n)

        write ( *, '(2x,i8,2x,f10.4,2x,f10.4,2g14.6)' ) 
     &    n, a, x, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine gen_hermite_poly_test ( )

c*********************************************************************72
c
cc GEN_HERMITE_POLY_TEST tests GEN_HERMITE_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 6 )
      integer n
      parameter ( n = 10 )

      double precision c(0:n)
      integer j
      double precision mu
      double precision mu_test(test_num)
      integer test
      double precision x
      double precision x_test(test_num)

      save mu_test
      save x_test

      data mu_test /
     &  0.0D+00, 0.0D+00, 0.1D+00, 0.1D+00, 0.5D+00, 1.0D+00 /
      data x_test /
     &  0.0D+00, 1.0D+00, 0.0D+00, 0.5D+00, 0.5D+00, 0.5D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEN_HERMITE_POLY_TEST'
      write ( *, '(a)' ) '  GEN_HERMITE_POLY evaluates the generalized'
      write ( *, '(a)' ) '  Hermite functions.'

      do test = 1, test_num

        x = x_test(test)
        mu = mu_test(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Table of H(N,MU)(X) for'
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '    N(max) = ', n
        write ( *, '(a,g14.6)' ) '    MU =     ', mu
        write ( *, '(a,g14.6)' ) '    X =      ', x
        write ( *, '(a)' ) ' '
      
        call gen_hermite_poly ( n, x, mu, c )
     
        do j = 0, n
          write ( *, '(2x,i8,g14.6)' ) j, c(j)
        end do

      end do
     
      return
      end
      subroutine gen_laguerre_poly_test ( )

c*********************************************************************72
c
cc GEN_LAGUERRE_POLY_TEST tests GEN_LAGUERRE_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 6 )
      integer n
      parameter ( n = 10 )

      double precision alpha
      double precision alpha_test(test_num)
      double precision c(0:n)
      integer j
      integer test
      double precision x
      double precision x_test(test_num)

      save alpha_test
      save x_test

      data alpha_test /
     &  0.0D+00, 0.0D+00, 0.1D+00, 0.1D+00, 0.5D+00, 1.0D+00 /
      data x_test /
     &  0.0D+00, 1.0D+00, 0.0D+00, 0.5D+00, 0.5D+00, 0.5D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEN_LAGUERRE_POLY_TEST'
      write ( *, '(a)' ) '  GEN_LAGUERRE_POLY evaluates the generalized'
      write ( *, '(a)' ) '  Laguerre functions.'

      do test = 1, test_num

        x = x_test(test)
        alpha = alpha_test(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Table of L(N,ALPHA)(X) for'
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '    N(max) = ', n
        write ( *, '(a,g14.6)' ) '    ALPHA =  ', alpha
        write ( *, '(a,g14.6)' ) '    X =      ', x
        write ( *, '(a)' ) ' '
      
        call gen_laguerre_poly ( n, alpha, x, c )
     
        do j = 0, n
          write ( *, '(2x,i8,g14.6)' ) j, c(j)
        end do

      end do
     
      return
      end
      subroutine gud_test ( )

c*********************************************************************72
c
cc GUD_TEST tests GUD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision fx2
      double precision gud
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GUD_TEST'
      write ( *, '(a)' ) '  GUD evaluates the Gudermannian function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     X      Exact F       GUD(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gud_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = gud ( x )

        write ( *, '(2x,f8.4,2g14.6)' ) x, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine hermite_poly_phys_test ( )

c*********************************************************************72
c
cc HERMITE_POLY_PHYS_TEST tests HERMITE_POLY_PHYS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLY_PHYS_TEST:'
      write ( *, '(a)' ) 
     &  '  HERMITE_POLY_PHYS evaluates the ' //
     &  'physicist''s Hermite polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N    X      Exact F       H(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hermite_poly_phys_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call hermite_poly_phys ( n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine hermite_poly_phys_coef_test ( )

c*********************************************************************72
c
cc HERMITE_POLY_PHYS_COEF_TEST tests HERMITE_POLY_PHYS_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
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
      write ( *, '(a)' ) 'HERMITE_POLY_PHYS_COEF_TEST'
      write ( *, '(a)' ) 
     &  '  HERMITE_POLY_PHYS_COEF determines' // 
     &  ' the physicist''s Hermite polynomial coefficients.'

      call hermite_poly_phys_coef ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  H(', i, ')'
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x**', j
          end if
        end do
      end do
     
      return
      end
      subroutine i4_choose_test ( )

c*********************************************************************72
c
cc I4_CHOOSE_TEST tests I4_CHOOSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer cnk
      integer i4_choose
      integer k
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_CHOOSE_TEST'
      write ( *, '(a)' ) '  I4_CHOOSE evaluates C(N,K).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N     K    CNK'
      write ( *, '(a)' ) ' '
 
      do n = 0, 4
        do k = 0, n
          cnk = i4_choose ( n, k )
          write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, k, cnk
        end do
      end do
 
      return
      end
      subroutine i4_factor_test ( )

c*********************************************************************72
c
cc I4_FACTOR_TEST tests I4_FACTOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer maxfactor
      parameter ( maxfactor = 10 )

      integer i
      integer j
      integer n
      integer n_test(3)
      integer nfactor
      integer nleft
      integer factor(maxfactor)
      integer power(maxfactor)

      save n_test

      data n_test /
     &  60, 664048, 8466763 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTOR_TEST:'
      write ( *, '(a)' ) '  I4_FACTOR tries to factor an I4'

      do i = 1, 3
        n = n_test(i)
        call i4_factor ( n, maxfactor, nfactor, factor, power, nleft )
        write ( *, '(a)' ) ''
        write ( *, '(a,i9)' ) '  Factors of N = ', n
        do j = 1, nfactor
          write ( *, '(i9,a,i4)' ) factor(j), '^', power(j)
        end do
        if ( nleft .ne. 1 ) then
          write ( *, '(a,i4)' ) '  Unresolved factor NLEFT = ', nleft
        end if
      end do

      return
      end
      subroutine i4_factorial_test ( )

c*********************************************************************72
c
cc I4_FACTORIAL_TEST tests I4_FACTORIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer fn2
      integer i4_factorial
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTORIAL_TEST:'
      write ( *, '(a)' ) 
     &  '  I4_FACTORIAL evaluates the factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     X       Exact F       I4_FACTORIAL(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_factorial_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fn2 = i4_factorial ( n )

        write ( *, '(2x,i4,2i12)' ) n, fn, fn2

      go to 10

20    continue

      return
      end
      subroutine i4_factorial2_test ( )

c*********************************************************************72
c
cc I4_FACTORIAL2_TEST tests I4_FACTORIAL2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer fn2
      integer n
      integer n_data
      integer i4_factorial2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTORIAL2_TEST:'
      write ( *, '(a)' ) 
     &  '  I4_FACTORIAL2 evaluates the double factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N   Exact  I4_FACTORIAL2(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_factorial2_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fn2 = i4_factorial2 ( n )

        write ( *, '(2x,i4,2i8)' ) n, fn, fn2

      go to 10

20    continue

      return
      end
      subroutine i4_is_triangular_test ( )

c*********************************************************************72
c
cc I4_IS_TRIANGULAR_TEST tests I4_IS_TRIANGULAR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      logical i4_is_triangular
      logical l

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_IS_TRIANGULAR_TEST'
      write ( *, '(a)' ) '  I4_IS_TRIANGULAR returns T or F depending'
      write ( *, '(a)' ) '  on whether I is triangular.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I      T/F'
      write ( *, '(a)' ) ' '

      do i = 0, 20

        l = i4_is_triangular ( i )

        write ( *, '(2x,i4,4x,l1)' )  i, l

      end do
     
      return
      end
      subroutine i4_partition_distinct_count_test ( )

c*********************************************************************72
c
cc I4_PARTITION_DISTINCT_COUNT_TEST tests I4_PARTITION_DISTINCT_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data
      integer n_max
      parameter ( n_max = 20 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_PARTITION_DISTINCT_COUNT_TEST:'
      write ( *, '(a)' ) '  For the number of partitions of an integer'
      write ( *, '(a)' ) '  into distinct parts,'
      write ( *, '(a)' ) '  I4_PARTITION_DISTINCT_COUNT'
      write ( *, '(a)' ) '  computes any value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '           N       Exact F    Q(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call partition_distinct_count_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        if ( n_max .lt. n ) then
          go to 10
        end if

        call i4_partition_distinct_count ( n, c2 )

        write ( *, '(2x,3i10)' ) n, c, c2

      go to 10

20    continue

      return
      end
      subroutine i4_to_triangle_lower_test ( )

c*********************************************************************72
c
cc I4_TO_TRIANGLE_LOWER_TEST tests I4_TO_TRIANGLE_LOWER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_TO_TRIANGLE_LOWER_TEST'
      write ( *, '(a)' ) '  I4_TO_TRIANGLE_LOWER converts a linear'
      write ( *, '(a)' ) '  index to a lower triangular one.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     K  =>   I   J'
      write ( *, '(a)' ) ' '

      do k = 1, 20

        call i4_to_triangle_lower ( k, i, j )

        write ( *, '(2x,i4,4x,i4,i4)' ) k, i, j

      end do
     
      return
      end
      subroutine jacobi_poly_test ( )

c*********************************************************************72
c
cc JACOBI_POLY_TEST tests JACOBI_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 April 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c(0:6)
      double precision fx
      double precision fx2
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_POLY_TEST:'
      write ( *, '(a)' ) '  JACOBI_POLY computes values of '
      write ( *, '(a)' ) '  the Jacobi polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N       A       B      X       JPV      JACOBI'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jacobi_poly_values ( n_data, n, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call jacobi_poly ( n, a, b, x, c )
        fx2 = c(n)

        write ( *, '(2x,i8,2x,f8.4,2x,f8.4,f10.4,2g14.6)' ) 
     &  n, a, b, x, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine jacobi_symbol_test ( )

c*********************************************************************72
c
cc JACOBI_SYMBOL_TEST tests JACOBI_SYMBOL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 4 )

      integer l
      integer p
      integer p_test(test_num)
      integer q
      integer test

      save p_test

      data p_test / 3, 9, 10, 12 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_SYMBOL_TEST'
      write ( *, '(a)' ) '  JACOBI_SYMBOL computes the Jacobi symbol'
      write ( *, '(a)' ) '  (Q/P), which records if Q is a quadratic '
      write ( *, '(a)' ) '  residue modulo the number P.'

      do test = 1, test_num
        p = p_test(test)
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Jacobi Symbols for P = ', p
        write ( *, '(a)' ) ' '
        do q = 0, p
          call jacobi_symbol ( q, p, l )
          write ( *, '(2x,3i8)' ) p, q, l
        end do
      end do

      return
      end
      subroutine krawtchouk_test ( )

c*********************************************************************72
c
cc KRAWTCHOUK_TEST tests KRAWTCHOUK
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer test_num
      parameter ( test_num = 2 )

      integer i
      integer j
      integer m
      double precision p
      double precision p_test(test_num)
      integer test
      double precision x
      double precision value(0:n)

      save p_test

      data p_test / 0.25D+00, 0.50D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KRAWTCHOUK_TEST:'
      write ( *, '(a)' ) 
     &  '  KRAWTCHOUK evaluates Krawtchouk polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N      P         X            M    K(N,P,X,M)'
      write ( *, '(a)' ) ' '

      m = 5

      do test = 1, test_num

        p = p_test(test)

        do j = 0, 5

          x = dble ( j ) / 2.0D+00

          call krawtchouk ( n, p, x, m, value )

          write ( *, '(a)' ) ' '

          do i = 0, n

            write ( *, '(2x,i8,2x,f8.4,2x,f8.4,2x,i8,2x,g14.6)' ) 
     &        i, p, x, m, value(i)

          end do

        end do

      end do

      return
      end
      subroutine laguerre_associated_test ( )

c*********************************************************************72
c
cc LAGUERRE_ASSOCIATED_TEST tests LAGUERRE_ASSOCIATED.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 6 )
      integer n
      parameter ( n = 6 )

      double precision c(0:n)
      integer j
      integer m
      integer m_test(test_num)
      integer test
      double precision x
      double precision x_test(test_num)

      save m_test
      save x_test

      data m_test / 0, 0, 1, 2, 3, 1 /
      data x_test /
     &  0.0D+00, 1.0D+00, 0.0D+00, 0.5D+00, 0.5D+00, 0.5D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_ASSOCIATED_TEST'
      write ( *, '(a)' ) 
     &  '  LAGUERRE_ASSOCIATED evaluates the associated Laguerre'
      write ( *, '(a)' ) '  polynomials.'

      do test = 1, test_num

        m = m_test(test)
        x = x_test(test)

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Table of L(N,M)(X) for'
        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  N(max) = ', n
        write ( *, '(a,i4)' ) '  M      = ', m
        write ( *, '(a,g14.6)' ) '  X =      ', x
        write ( *, '(a)' ) ' '
     
        call laguerre_associated ( n, m, x, c )
     
        do j = 0, n
          write ( *, '(2x,i8,g14.6)' ) j, c(j)
        end do
     
      end do

      return
      end
      subroutine laguerre_poly_test ( )

c*********************************************************************72
c
cc LAGUERRE_POLY_TEST tests LAGUERRE_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLY_TEST:'
      write ( *, '(a)' ) 
     &  '  LAGUERRE_POLY evaluates the Laguerre polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N    X      Exact F       L(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

      do

        call laguerre_polynomial_values ( n_data, n, x, fx )

        if ( n_data == 0 ) then
          exit
        end if

        call laguerre_poly ( n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      end do

      return
      end
      subroutine laguerre_poly_coef_test ( )

c*********************************************************************72
c
cc LAGUERRE_POLY_COEF_TEST tests LAGUERRE_POLY_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision c(0:n,0:n)
      double precision fact
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLY_COEF_TEST'
      write ( *, '(a)' ) '  LAGUERRE_POLY_COEF determines the ' //
     &  'Laguerre polynomial coefficients.'

      call laguerre_poly_coef ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  L(', i, ')'
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x**', j
          end if
        end do
      end do
     
      fact = 1.0D+00

      do i = 0, n

        if ( 0 .lt. i ) then
          fact = fact * dble ( i )
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  Factorially scaled L(', i, ')'
        write ( *, '(a)' ) ' '

        do j = i, 0, -1
          if ( j == 0 ) then
            write ( *, '(2x,g14.6)' ) fact * c(i,j)
          else if ( j == 1 ) then
            write ( *, '(2x,g14.6,a)' ) fact * c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) fact * c(i,j), ' * x ^ ', j
          end if
        end do
        
      end do

      return
      end
      subroutine lambert_w_test ( )

c*********************************************************************72
c
cc LAMBERT_W_TEST tests LAMBERT_W.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision fx2
      double precision lambert_w
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAMBERT_W_TEST:'
      write ( *, '(a)' ) 
     &  '  LAMBERT_W estimates the Lambert W function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '           X           W(X)        W(X)'
      write ( *, '(a)' ) 
     &  '                   Tabulated     Estimate'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lambert_w_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = lambert_w ( x )

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    x, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine lambert_w_crude_test ( )

c*********************************************************************72
c
cc LAMBERT_W_CRUDE_TEST tests LAMBERT_W_CRUDE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision fx2
      double precision lambert_w_crude
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAMBERT_W_CRUDE_TEST:'
      write ( *, '(a)' ) 
     &  '  LAMBERT_W_CRUDE makes a crude estimate of the'
      write ( *, '(a)' ) '    Lambert W function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '           X           W(X)        W(X)'
      write ( *, '(a)' ) 
     &  '                   Tabulated       Crude'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lambert_w_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = lambert_w_crude ( x )

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    x, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine legendre_associated_test ( )

c*********************************************************************72
c
cc LEGENDRE_ASSOCIATED_TEST tests LEGENDRE_ASSOCIATED.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx2(0:n_max)
      double precision fx
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_ASSOCIATED_TEST:'
      write ( *, '(a)' ) 
     &  '  LEGENDRE_ASSOCIATED evaluates associated Legendre functions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N       M    X     Exact F       PNM(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_associated_values ( n_data, n, m, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call legendre_associated ( n, m, x, fx2 )

        write ( *, '(2x,i8,2x,i8,f8.4,2g14.6)' ) n, m, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine legendre_associated_normalized_test ( )

c*********************************************************************72
c
cc LEGENDRE_ASSOCIATED_NORMALIZED_TEST tests LEGENDRE_ASSOCIATED_NORMALIZED.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx2(0:n_max)
      double precision fx
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_ASSOCIATED_NORMALIZED_TEST:'
      write ( *, '(a)' ) 
     &  '  LEGENDRE_ASSOCIATED_NORMALIZED evaluates normalized ' //
     &  'associated Legendre functions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N       M    X     Exact F       PNM(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_associated_normalized_sphere_values ( 
     &    n_data, n, m, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call legendre_associated_normalized ( n, m, x, fx2 )

        write ( *, '(2x,i8,2x,i8,f8.4,2g14.6)' ) n, m, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine legendre_function_q_test ( )

c*********************************************************************72
c
cc LEGENDRE_FUNCTION_Q_TEST tests LEGENDRE_FUNCTION_Q.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_FUNCTION_Q_TEST:'
      write ( *, '(a)' ) 
     &  '  LEGENDRE_FUNCTION_Q evaluates the Legendre Q function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N    X      Exact F       Q(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_function_q_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call legendre_function_q ( n, x, fx2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine legendre_poly_test ( )

c*********************************************************************72
c
cc LEGENDRE_POLY_TEST tests LEGENDRE_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fp2(0:n_max)
      double precision fx2(0:n_max)
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLY_TEST:'
      write ( *, '(a)' ) 
     &  '  LEGENDRE_POLY evaluates the Legendre PN function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N    X      Exact F       P(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call legendre_poly ( n, x, fx2, fp2 )

        write ( *, '(2x,i8,f8.4,2g14.6)' ) n, x, fx, fx2(n)

      go to 10

20    continue

      return
      end
      subroutine legendre_poly_coef_test ( )

c*********************************************************************72
c
cc LEGENDRE_POLY_COEF_TEST tests LEGENDRE_POLY_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
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
      write ( *, '(a)' ) 'LEGENDRE_POLY_COEF_TEST'
      write ( *, '(a)' ) 
     &  '  LEGENDRE_POLY_COEF returns Legendre polynomial coefficients.'

      call legendre_poly_coef ( n, c )
     
      do i = 0, n
        write ( *, '(a)' ) ' '
        write ( *, '(a,i2,a)' ) '  P(', i, ')'
        write ( *, '(a)' ) ' '
        do j = i, 0, -1
          if ( j .eq. 0 ) then
            write ( *, '(2x,g14.6)' ) c(i,j)
          else if ( j .eq. 1 ) then
            write ( *, '(2x,g14.6,a)' ) c(i,j), ' * x'
          else
            write ( *, '(2x,g14.6,a,i2)' ) c(i,j), ' * x**', j
          end if
        end do
      end do

      return
      end
      subroutine legendre_symbol_test ( )

c*********************************************************************72
c
cc LEGENDRE_SYMBOL_TEST tests LEGENDRE_SYMBOL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 4 )

      integer l
      integer p
      integer p_test(test_num)
      integer q
      integer test

      save p_test

      data p_test / 7, 11, 13, 17 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_SYMBOL_TEST'
      write ( *, '(a)' ) '  LEGENDRE_SYMBOL computes the Legendre'
      write ( *, '(a)' ) '  symbol (Q/P) which records whether Q is '
      write ( *, '(a)' ) '  a quadratic residue modulo the prime P.'

      do test = 1, test_num
        p = p_test(test)
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Legendre Symbols for P = ', p
        write ( *, '(a)' ) ' '
        do q = 0, p
          call legendre_symbol ( q, p, l )
          write ( *, '(2x,3i8)' ) p, q, l
        end do
      end do

      return
      end
      subroutine lerch_test ( )

c*********************************************************************72
c
cc LERCH_TEST tests LERCH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      double precision fx2
      double precision lerch
      integer n_data
      integer s
      double precision z

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LERCH_TEST'
      write ( *, '(a)' ) '  LERCH computes the Lerch function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) 
     &  '       Z       S       A         Lerch           Lerch'
      write ( *, '(a)' ) 
     &  '                             Tabulated        Computed'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call lerch_values ( n_data, z, s, a, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = lerch ( z, s, a )

        write ( *, '(2x,f8.4,2x,i4,2x,f8.4,2x,g14.6,2x,g14.6)' ) 
     &    z, s, a, fx, fx2

      go to 10

20    continue
     
      return
      end
      subroutine lock_test ( )

c*********************************************************************72
c
cc LOCK_TEST tests LOCK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(0:n)
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOCK_TEST'
      write ( *, '(a)' ) 
     &  '  LOCK counts the combinations on a button lock.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I      LOCK(I)'
      write ( *, '(a)' ) ' '

      call lock ( n, a )

      do i = 0, n
        write ( *, '(2x,i8,2x,i10)' )  i, a(i)
      end do
     
      return
      end
      subroutine meixner_test ( )

c*********************************************************************72
c
cc MEIXNER_TEST tests MEIXNER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer test_num
      parameter ( test_num = 3 )

      double precision beta
      double precision beta_test(test_num)
      double precision c
      double precision c_test(test_num)
      integer i
      integer j
      integer test
      double precision v(0:n)
      double precision x

      save beta_test
      save c_test

      data beta_test / 0.5D+00, 1.0D+00, 2.0D+00 /
      data c_test / 0.125D+00, 0.25D+00, 0.5D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MEIXNER_TEST:'
      write ( *, '(a)' ) '  MEIXNER evaluates Meixner polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N      BETA         C         X        M(N,BETA,C,X)'

      do test = 1, test_num

        beta = beta_test(test)
        c = c_test(test)

        do j = 0, 5

          x = dble ( j ) / 2.0D+00

          call meixner ( n, beta, c, x, v )

          write ( *, '(a)' ) ' '

          do i = 0, n

            write ( *, '(2x,i8,2x,f8.4,2x,f8.4,2x,f8.4,2x,g14.6)' ) 
     &        i, beta, c, x, v(i)

          end do

        end do

      end do

      return
      end
      subroutine mertens_test ( )

c*********************************************************************72
c
cc MERTENS_TEST tests MERTENS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer mertens
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MERTENS_TEST'
      write ( *, '(a)' ) '  MERTENS computes the Mertens function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '         N     Exact   MERTENS(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call mertens_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        c2 = mertens ( n )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine moebius_test ( )

c*********************************************************************72
c
cc MOEBIUS_TEST tests MOEBIUS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MOEBIUS_TEST'
      write ( *, '(a)' ) '  MOEBIUS computes the Moebius function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '         N     Exact   MOEBIUS(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call moebius_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call moebius ( n, c2 )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine motzkin_test ( )

c*********************************************************************72
c
cc MOTZKIN_TEST tests MOTZKIN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer a(0:n)
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MOTZKIN_TEST'
      write ( *, '(a)' ) 
     &  '  MOTZKIN computes the Motzkin numbers A(0:N).'
      write ( *, '(a)' ) '  A(N) counts the paths from (0,0) to (N,0).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I         A(I)'
      write ( *, '(a)' ) ' '

      call motzkin ( n, a )

      do i = 0, n
        write ( *, '(2x,i8,2x,i10)' )  i, a(i)
      end do
     
      return
      end
      subroutine normal_01_cdf_inverse_test ( )

c*********************************************************************72
c
cc NORMAL_01_CDF_INVERSE_TEST tests NORMAL_01_CDF_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision normal_01_cdf_inverse
      double precision x1
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_CDF_INVERSE_TEST:'
      write ( *, '(a)' ) 
     &  '  NORMAL_01_CDF_INVERSE inverts the normal 01 CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    FX            X    NORMAL_01_CDF_INVERSE(FX)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call normal_01_cdf_values ( n_data, x1, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        x2 = normal_01_cdf_inverse ( fx )

        write ( *, '(2x,f8.4,2g14.6)' ) fx, x1, x2

      go to 10

20    continue

      return
      end
      subroutine omega_test ( )

c*********************************************************************72
c
cc OMEGA_TEST tests OMEGA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'OMEGA_TEST'
      write ( *, '(a)' ) 
     &  '  OMEGA counts the distinct prime divisors of an integer N.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '             N      Exact   OMEGA(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call omega_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call omega ( n, c2 )

        write ( *, '(2x,i12,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine pentagon_num_test ( )

c*********************************************************************72
c
cc PENTAGON_NUM_TEST tests PENTAGON_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer p

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PENTAGON_NUM_TEST'
      write ( *, '(a)' ) 
     &  '  PENTAGON_NUM computes the pentagonal numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I      Pent(I)'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        call pentagon_num ( n, p )
        write ( *, '(2x,i8,2x,i8)' ) n, p
      end do
     
      return
      end
      subroutine phi_test ( )

c*********************************************************************72
c
cc PHI_TEST tests PHI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PHI_TEST'
      write ( *, '(a)' ) '  PHI computes the PHI function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '     N     Exact     PHI(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call phi_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call phi ( n, c2 )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine plane_partition_num_test ( )

c*********************************************************************72
c
cc PLANE_PARTITION_NUM_TEST tests PLANE_PARTITION_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer p
      integer plane_partition_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PLANE_PARTITION_NUM_TEST'
      write ( *, '(a)' ) 
     &  '  PLANE_PARTITION_NUM counts the number of plane'
      write ( *, '(a)' ) '  partitions of an integer.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I      P(I)'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        p = plane_partition_num ( n )
        write ( *, '(2x,i8,2x,i8)' ) n, p
      end do
     
      return
      end
      subroutine poly_bernoulli_test ( )

c*********************************************************************72
c
cc POLY_BERNOULLI_TEST tests POLY_BERNOULLI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer b
      integer k
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLY_BERNOULLI_TEST'
      write ( *, '(a)' ) 
     &  '  POLY_BERNOULLI computes the poly-Bernoulli numbers'
      write ( *, '(a)' ) '  of negative index, B_n^(-k)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N     K    B_N^(-K)'
      write ( *, '(a)' ) ' '

      do k = 0, 6
        write ( *, '(a)' ) ' '
        do n = 0, 6

          call poly_bernoulli ( n, k, b )

          write ( *, '(2x,i4,2x,i4,2x,i12)' ) n, k, b

        end do
      end do

      return
      end
      subroutine poly_coef_count_test ( )

c*********************************************************************72
c
cc POLY_COEF_COUNT_TEST tests POLY_COEF_COUNT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree
      integer dim
      integer poly_coef_count

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLY_COEF_COUNT_TEST'
      write ( *, '(a)' ) 
     &  '  POLY_COEF_COUNT counts the number of coefficients'
      write ( *, '(a)' ) 
     &  '  in a polynomial of degree DEGREE and dimension DIM'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' Dimension    Degree     Count'

      do dim = 1, 10, 3
        write ( *, '(a)' ) ' '
        do degree = 0, 5
          write ( *, '(2x,i8,2x,i8,2x,i8)' ) 
     &     dim, degree, poly_coef_count ( dim, degree )
        end do
      end do
     
      return
      end
      subroutine prime_test ( )

c*********************************************************************72
c
cc PRIME_TEST tests PRIME.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer n
      integer prime
      integer prime_max

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PRIME_TEST'
      write ( *, '(a)' ) '  PRIME returns primes from a table.'

      n = -1
      prime_max = prime ( n )
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) 
     &  '  Number of primes stored is ', prime_max
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    Prime(I)'
      write ( *, '(a)' ) ''
      do i = 1, 10
        write ( *, '(4x,i4,2x,i6)' ) i, prime(i)
      end do
      write ( *, '(a)' ) ''
      do i = prime_max - 10, prime_max
        write ( *, '(4x,i4,2x,i6)' ) i, prime(i)
      end do
  
      return
      end
      subroutine pyramid_num_test ( )

c*********************************************************************72
c
cc PYRAMID_NUM_TEST tests PYRAMID_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer pyramid_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_NUM_TEST'
      write ( *, '(a)' ) '  PYRAMID_NUM computes the pyramidal numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I    PYR(I)'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        write ( *, '(2x,i8,2x,i8)' ) n, pyramid_num ( n )
      end do
     
      return
      end
      subroutine pyramid_square_num_test ( )

c*********************************************************************72
c
cc PYRAMID_SQUARE_NUM_TEST tests PYRAMID_SQUARE_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    04 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer pyramid_square_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_SQUARE_NUM_TEST'
      write ( *, '(a)' ) 
     &  '  PYRAMID_SQUARE_NUM computes the pyramidal square numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I    PYR(I)'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        write ( *, '(2x,i8,2x,i8)' ) n, pyramid_square_num ( n )
      end do
     
      return
      end
      subroutine r8_agm_test ( )

c*********************************************************************72
c
cc R8_AGM_TEST tests R8_AGM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 February 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      double precision fx2
      integer n_data
      double precision r8_agm

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_AGM_TEST'
      write ( *, '(a)' ) 
     &  '  R8_AGM computes the arithmetic geometric mean.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a,a)' ) '      A           B          ',
     &  '   AGM                       AGM                   Diff'
      write ( *, '(a,a)' ) '                             ',
     &  '  (Tabulated)              R8_AGM(A,B)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call agm_values ( n_data, a, b, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = r8_agm ( a, b )

        write ( *, '(2x,f10.6,2x,f10.6,2x,g24.16,2x,g24.16,2x,g10.4)' ) 
     &    a, b, fx, fx2, abs ( fx - fx2 )

      go to 10

20    continue
     
      return
      end
      subroutine r8_beta_test ( )

c*********************************************************************72
c
cc R8_BETA_TEST tests R8_BETA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fxy
      double precision fxy2
      integer n_data
      double precision r8_beta
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_BETA_TEST:'
      write ( *, '(a)' ) '  R8_BETA evaluates the Beta function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     X      Y        Exact F       R8_BETA(X,Y)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_values ( n_data, x, y, fxy )

        if ( n_data == 0 ) then
          go to 20
        end if

        fxy2 = r8_beta ( x, y )

        write ( *, '(2x,2f8.4,2g14.6)' ) x, y, fxy, fxy2

      go to 10

20    continue

      return
      end
      subroutine r8_choose_test ( )

c*********************************************************************72
c
cc R8_CHOOSE_TEST tests R8_CHOOSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cnk
      integer k
      integer n
      double precision r8_choose

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_CHOOSE_TEST'
      write ( *, '(a)' ) '  R8_CHOOSE evaluates C(N,K).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N       K      CNK'
      write ( *, '(a)' ) ' '
 
      do n = 0, 4
        do k = 0, n
          cnk = r8_choose ( n, k )
          write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) n, k, cnk
        end do
      end do
 
      return
      end
      subroutine r8_erf_test ( )

c*********************************************************************72
c
cc R8_ERF_TEST tests R8_ERF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision fx2
      integer n_data
      double precision r8_erf
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ERF_TEST:'
      write ( *, '(a)' ) '  R8_ERF evaluates the error function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     X      Exact F       R8_ERF(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call erf_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = r8_erf ( x )

        write ( *, '(2x,f8.4,2g14.6)' ) x, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine r8_erf_inverse_test ( )

c*********************************************************************72
c
cc R8_ERF_INVERSE_TEST tests R8_ERF_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 August 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision r8_erf_inverse
      double precision x1
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_ERF_INVERSE_TEST:'
      write ( *, '(a)' ) '  R8_ERF_INVERSE inverts the error function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    FX            X    R8_ERF_INVERSE(FX)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call erf_values ( n_data, x1, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        x2 = r8_erf_inverse ( fx )

        write ( *, '(2x,f8.4,2g14.6)' ) fx, x1, x2

      go to 10

20    continue

      return
      end
      subroutine r8_euler_constant_test ( )

c*********************************************************************72
c
cc R8_EULER_CONSTANT_TEST tests R8_EULER_CONSTANT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision g
      double precision g_approx
      integer i
      integer n
      double precision n_r8
      double precision r8_euler_constant
      integer test

      g = r8_euler_constant ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8_EULER_CONSTANT_TEST:'
      write ( *, '(a)' ) 
     &  '  R8_EULER_CONSTANT returns the Euler-Mascheroni constant'
      write ( *, '(a)' ) '  sometimes denoted by "gamma".'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  gamma = limit ( N -> oo ) ' //
     &  '( sum ( 1 <= I <= N ) 1 / I ) - log ( N )'
      write ( *, '(a)' ) ''
      write ( *, '(a,g24.16)' ) '  Numerically, g = ', g
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '         N      Partial Sum    |gamma - partial sum|'
      write ( *, '(a)' ) ''

      n = 1
      do test = 0, 20
        n_r8 = dble ( n )
        g_approx = - log ( n_r8 )    
        do i = 1, n
          g_approx = g_approx + 1.0D+00 / dble ( i )
        end do
        write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) 
     &    n, g_approx, abs ( g_approx - g )
        n = n * 2
      end do

      return
      end
      subroutine r8_factorial_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL_TEST tests R8_FACTORIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fn
      double precision fn2
      integer n_data
      integer n
      double precision r8_factorial

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL_TEST:'
      write ( *, '(a)' ) 
     &  '  R8_FACTORIAL evaluates the factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N       Exact F       R8_FACTORIAL(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fn2 = r8_factorial ( n )

        write ( *, '(2x,i4,2g14.6)' ) n, fn, fn2

      go to 10

20    continue

      return
      end
      subroutine r8_factorial_log_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL_LOG_TEST tests R8_FACTORIAL_LOG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fn
      double precision fn2
      double precision r8_factorial_log
      integer n_data
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL_LOG_TEST:'
      write ( *, '(a)' ) 
     &  '  R8_FACTORIAL_LOG evaluates the logarithm of the '
      write ( *, '(a)' ) '  factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N	   Exact F	 R8_FACTORIAL_LOG(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial_log_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fn2 = r8_factorial_log ( n )

        write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) n, fn, fn2

      go to 10

20    continue

      return
      end
      subroutine r8_hyper_2f1_test ( )

c*********************************************************************72
c
cc R8_HYPER_2F1_TEST tests R8_HYPER_2F1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision fx
      double precision fx2
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_HYPER_2F1_TEST:'
      write ( *, '(a)' ) 
     &  '  R8_HYPER_2F1 evaluates the hypergeometric 2F1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) '      A       B       C       X      ', 
     &  ' 2F1                       2F1                     DIFF'
      write ( *, '(a,a)' ) '                                     ', 
     &  '(tabulated)               (computed)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hyper_2f1_values ( n_data, a, b, c, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call r8_hyper_2f1 ( a, b, c, x, fx2 )

        write ( *, 
     &  '(2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,' //
     &  'g24.16,2x,g24.16,2x,g10.4)' ) 
     &  a, b, c, x, fx, fx2, abs ( fx - fx2 )

      go to 10

20    continue

      return
      end
      subroutine r8_psi_test ( )

c*********************************************************************72
c
cc R8_PSI_TEST tests R8_PSI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision fx2
      integer n_data
      double precision r8_psi
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_PSI_TEST:'
      write ( *, '(a)' ) '  R8_PSI evaluates the Psi function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         Psi(X)                     '
     &  // 'Psi(X)                 DIFF'
      write ( * , '(a)' ) 
     &  '               (Tabulated)                (R8_PSI)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call psi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = r8_psi ( x )

        write ( *, '(2x,f8.4,2x,g24.16,2x,g24.16,2x,g10.4)' ) 
     &    x, fx, fx2, abs ( fx - fx2 )

      go to 10

20    continue

      return
      end
      subroutine r8poly_degree_test ( )

c*********************************************************************72
c
cc R8POLY_DEGREE_TEST tests R8POLY_DEGREE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c1(0:3)
      double precision c2(0:3)
      double precision c3(0:3)
      double precision c4(0:3)
      double precision c5(0:3)
      integer d
      integer m
      integer r8poly_degree

      save c1
      save c2
      save c3
      save c4
      save c5

      data c1
     &  / 1.0, 2.0, 3.0, 4.0 /
      data c2
     &  / 1.0, 2.0, 3.0, 0.0 /
      data c3(0:3)
     &  / 1.0, 2.0, 0.0, 4.0 /
      data c4(0:3)
     &  / 1.0, 0.0, 0.0, 0.0 /
      data c5(0:3)
     &  / 0.0, 0.0, 0.0, 0.0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_DEGREE_TEST'
      write ( *, '(a)' ) 
     &  '  R8POLY_DEGREE determines the degree of an R8POLY.'

      m = 3

      call r8poly_print ( m, c1, '  The R8POLY:' )
      d = r8poly_degree ( m, c1 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c2, '  The R8POLY:' )
      d = r8poly_degree ( m, c2 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c3, '  The R8POLY:' )
      d = r8poly_degree ( m, c3 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c4, '  The R8POLY:' )
      d = r8poly_degree ( m, c4 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      call r8poly_print ( m, c5, '  The R8POLY:' )
      d = r8poly_degree ( m, c5 )
      write ( *, '(a,i2,a,i2)' ) 
     &  '  Dimensioned degree = ', m, '  Actual degree = ', d

      return
      end
      subroutine r8poly_print_test ( )

c*********************************************************************72
c
cc R8POLY_PRINT_TEST tests R8POLY_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )

      double precision c(0:m)

      save c

      data c /
     &  12.0D+00, -3.4D+00, 56.0D+00, 0.0D+00, 0.78D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_PRINT_TEST'
      write ( *, '(a)' ) '  R8POLY_PRINT prints an R8POLY.'

      call r8poly_print ( m, c, '  The R8POLY:' )

      return
      end
      subroutine r8poly_value_horner_test ( )

c*********************************************************************72
c
cc R8POLY_VALUE_HORNER_TEST tests R8POLY_VALUE_HORNER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 16 )

      double precision c(0:m)
      integer i
      double precision p
      double precision r8poly_value_horner
      double precision x(n)
      double precision x_hi
      double precision x_lo

      save c

      data c /
     &  24.0D+00, -50.0D+00, +35.0D+00, -10.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8POLY_VALUE_HORNER_TEST'
      write ( *, '(a)' ) '  R8POLY_VALUE_HORNER evaluates a polynomial'
      write ( *, '(a)' ) '  at one point, using Horner''s method.'

      call r8poly_print ( m, c, '  The polynomial coefficients:' )

      x_lo = 0.0D+00
      x_hi = 5.0D+00
      call r8vec_linspace ( n, x_lo, x_hi, x )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   I    X    P(X)'
      write ( *, '(a)' ) ''

      do i = 1, n
        p = r8poly_value_horner ( m, c, x(i) )
        write ( *, '(2x,i2,2x,f8.4,2x,g14.6)' ) i, x(i), p
      end do

      return
      end
      subroutine sigma_test ( )

c*********************************************************************72
c
cc SIGMA_TEST tests SIGMA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIGMA_TEST'
      write ( *, '(a)' ) '  SIGMA computes the SIGMA function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '     N     Exact   SIGMA(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call sigma_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call sigma ( n, c2 )

        write ( *, '(2x,i4,2i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine simplex_num_test ( )

c*********************************************************************72
c
cc SIMPLEX_NUM_TEST tests SIMPLEX_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer simplex_num
      integer value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIMPLEX_NUM_TEST'
      write ( *, '(a)' ) '  SIMPLEX_NUM computes the N-th simplex'
      write ( *, '(a)' ) '  number in M dimensions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      M: 0     1     2     3     4     5'
      write ( *, '(a)' ) '   N'
 
      do n = 0, 10
        write ( *, '(2x,i2)', advance = 'no' ) n
        do m = 0, 5
          value = simplex_num ( m, n )
          write ( *, '(2x,i4)', advance = 'no' ) value
        end do
        write ( *, '(a)' ) ''
      end do
 
      return
      end
      subroutine sin_power_int_test ( )

c*********************************************************************72
c
cc SIN_POWER_INT_TEST tests SIN_POWER_INT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      double precision fx2
      integer n
      integer n_data
      double precision sin_power_int

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIN_POWER_INT_TEST:'
      write ( *, '(a)' ) '  SIN_POWER_INT returns values of '
      write ( *, '(a)' ) '  the integral of SIN(X)^N from A to B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      A         B          N      Exact           Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sin_power_int_values ( n_data, a, b, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        fx2 = sin_power_int ( a, b, n )

        write ( *, '(2x,f8.4,2x,f8.4,2x,i8,2x,g14.6,2x,g14.6)' ) 
     &    a, b, n, fx, fx2

      go to 10

20    continue

      return
      end
      subroutine slice_test ( )

c*********************************************************************72
c
cc SLICE_TEST tests SLICE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 August 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_max
      parameter ( dim_max = 5 )
      integer slice_max
      parameter ( slice_max = 8 )

      integer dim_num
      integer p(dim_max,slice_max)
      integer piece_num
      integer slice_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SLICE_TEST:'
      write ( *, '(a)' ) 
     &  '  SLICE determines the maximum number of pieces created'
      write ( *, '(a)' ) '  by SLICE_NUM slices in a DIM_NUM space.'

      do dim_num = 1, dim_max
        do slice_num = 1, slice_max
          call slice ( dim_num, slice_num, piece_num )
          p(dim_num,slice_num) = piece_num
        end do
      end do

      call i4mat_print ( dim_max, slice_max, p, '  Slice Array:' )

      return
      end
      subroutine spherical_harmonic_test ( )

c*********************************************************************72
c
cc SPHERICAL_HARMONIC_TEST tests SPHERICAL_HARMONIC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision c(0:n_max)
      integer l
      integer m
      integer n_data
      double precision phi
      double precision s(0:n_max)
      double precision theta
      double precision yi
      double precision yi2
      double precision yr
      double precision yr2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERICAL_HARMONIC_TEST:'
      write ( *, '(a)' ) 
     &  '  SPHERICAL_HARMONIC evaluates spherical harmonic'
      write ( *, '(a)' ) '  functions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       L       M   THETA    PHI     C              S'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call spherical_harmonic_values ( n_data, l, m, theta, phi, 
     &    yr, yi )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call spherical_harmonic ( l, m, theta, phi, c, s )

        yr2 = c(l)
        yi2 = s(l)

        write ( *, '(2x,i8,2x,i6,2f8.4,2g14.6)' ) 
     &    l, m, theta, phi, yr,  yi
        write ( *, '(2x,8x,2x,6x,16x,  2g14.6)' )
     &                     yr2, yi2

      go to 10

20    continue

      return
      end
      subroutine stirling1_test ( )

c*********************************************************************72
c
cc STIRLING1_TEST tests STIRLING1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 8 )
      integer n
      parameter ( n = m )

      integer i
      integer s1(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STIRLING1_TEST'
      write ( *, '(a)' ) '  STIRLING1: Stirling numbers of first kind.'
      write ( *, '(a,i8)' ) '  Get rows 1 through ', m
      write ( *, '(a)' ) ' '
     
      call stirling1 ( m, n, s1 )
     
      do i = 1, m
        write ( *, '(2x,i8,8i8)' ) i, s1(i,1:n)
      end do
     
      return
      end
      subroutine stirling2_test ( )

c*********************************************************************72
c
cc STIRLING2_TEST tests STIRLING2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 8 )
      integer n
      parameter ( n = m )

      integer i
      integer s2(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STIRLING2_TEST'
      write ( *, '(a)' ) '  STIRLING2: Stirling numbers of second kind.'
      write ( *, '(a,i4)' ) '  Get rows 1 through ', m
      write ( *, '(a)' ) ' '
     
      call stirling2 ( m, n, s2 )
     
      do i = 1, m
        write ( *, '(2x,i8,8i8)' ) i, s2(i,1:n)
      end do
     
      return
      end
      subroutine tau_test ( )

c*********************************************************************72
c
cc TAU_TEST tests TAU.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TAU_TEST'
      write ( *, '(a)' ) '  TAU computes the Tau function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '         N  exact C(I)  computed C(I)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call tau_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call tau ( n, c2 )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine tetrahedron_num_test ( )

c*********************************************************************72
c
cc TETRAHEDRON_NUM_TEST tests TETRAHEDRON_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer tetrahedron_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_NUM_TEST'
      write ( *, '(a)' ) 
     &  '  TETRAHEDRON_NUM computes the tetrahedron numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I    TETR(I)'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        write ( *, '(2x,i8,2x,i8)' ) n, tetrahedron_num ( n )
      end do
     
      return
      end
      subroutine triangle_num_test ( )

c*********************************************************************72
c
cc TRIANGLE_NUM_TEST tests TRIANGLE_NUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer triangle_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_NUM_TEST'
      write ( *, '(a)' ) 
     &  '  TRIANGLE_NUM computes the triangular numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         I    TRI(I)'
      write ( *, '(a)' ) ' '
     
      do n = 1, 10
        write ( *, '(2x,i8,2x,i8)' ) n, triangle_num ( n )
      end do
     
      return
      end
      subroutine triangle_lower_to_i4_test ( )

c*********************************************************************72
c
cc TRIANGLE_TO_I4_TEST tests TRIANGLE_TO_I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer j
      integer k

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_LOWER_TO_I4_TEST'
      write ( *, '(a)' ) '  TRIANGLE_LOWER_TO_I4 converts a lower'
      write ( *, '(a)' ) '  triangular index to a linear one.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I, J  =>   K'
      write ( *, '(a)' ) ' '

      do i = 1, 4
        do j = 1, i
          call triangle_lower_to_i4 ( i, j, k )
          write ( *, '(2x,i4,i4,4x,i4)' ) i, j, k
        end do
      end do
     
      return
      end
      subroutine trinomial_test ( )

c*********************************************************************72
c
cc TRINOMIAL_TEST tests TRINOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      integer ( kind = 4 ) t
      integer ( kind = 4 ) trinomial

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRINOMIAL_TEST'
      write ( *, '(a)' ) 
     &  '  TRINOMIAL evaluates the trinomial coefficient:'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  T(I,J,K) = (I+J+K)! / I! / J! / K!'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I     J     K    T(I,J,K)'
      write ( *, '(a)' ) ' '
 
      do k = 0, 4
        do j = 0, 4
          do i = 0, 4
            t = trinomial ( i, j, k )
            write ( *, '(2x,i4,2x,i4,2x,i4,2x,i8)' ) i, j, k, t
          end do
        end do
      end do
 
      return
      end
      subroutine vibonacci_test ( )

c*********************************************************************72
c
cc VIBONACCI_TEST tests VIBONACCI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )
      integer n_time
      parameter ( n_time = 3 )

      integer i
      integer j
      integer seed
      integer v(n,n_time)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'VIBONACCI_TEST'
      write ( *, '(a)' ) '  VIBONACCI computes a Vibonacci sequence.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of times we compute the series: ', n_time
      write ( *, '(a)' ) ' '

      seed = 123456789

      do j = 1, n_time
        call vibonacci ( n, seed, v(1,j) ) 
      end do

      do i = 1, n
        write ( *, '(2x,i8,2x,3i8)' ) i, v(i,1:n_time)
      end do
     
      return
      end
      subroutine zeckendorf_test ( )

c*********************************************************************72
c
cc ZECKENDORF_TEST tests ZECKENDORF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m_max
      parameter ( m_max = 20 )

      integer i_list(m_max)
      integer f_list(m_max)
      integer m
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ZECKENDORF_TEST'
      write ( *, '(a)' ) 
     &  '  ZECKENDORF computes the Zeckendorf decomposition of'
      write ( *, '(a)' ) 
     &  '  an integer N into nonconsecutive Fibonacci numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N Sum M Parts'
      write ( *, '(a)' ) ' '

      do n = 1, 100

        call zeckendorf ( n, m_max, m, i_list, f_list )

        write ( *, '(2x,i8,2x,15i4)' ) n, f_list(1:m)

      end do

      return
      end
      subroutine zernike_poly_test ( )

c*********************************************************************72
c
cc ZERNIKE_POLY_TEST tests ZERNIKE_POLY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      double precision c(0:n_max)
      double precision cx1
      double precision cx2(0:n_max)
      integer m
      integer n
      double precision r8poly_value_horner
      double precision rho
      double precision z1
      double precision z2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ZERNIKE_POLY_TEST'
      write ( *, '(a)' ) 
     &  '  ZERNIKE_POLY evaluates a Zernike polynomial directly.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Table of polynomial coefficients:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N   M'
      write ( *, '(a)' ) ' '

      do n = 0, 5

        write ( *, '(a)' ) ' '

        do m = 0, n
          call zernike_poly_coef ( m, n, c )
          write ( *, '(2x,i2,2x,i2,2x,11f7.0)' ) n, m, c(0:n)
        end do

      end do

      rho = 0.987654321D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Z1: Compute polynomial coefficients,'
      write ( *, '(a)' ) '  then evaluate by Horner''s method;'
      write ( *, '(a)' ) '  Z2: Evaluate directly by recursion.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N   M       Z1              Z2'
      write ( *, '(a)' ) ' '

      do n = 0, 5

        write ( *, '(a)' ) ' '

        do m = 0, n

          call zernike_poly_coef ( m, n, c )
          z1 = r8poly_value_horner ( n, c, rho )

          call zernike_poly ( m, n, rho, z2 )

          write ( *, '(2x,i2,2x,i2,2x,g16.8,2x,g16.8)' ) n, m, z1, z2

        end do

      end do

      return
      end
      subroutine zernike_poly_coef_test ( )

c*********************************************************************72
c
cc ZERNIKE_POLY_COEF_TEST tests ZERNIKE_POLY_COEF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision c(0:n)
      integer m

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ZERNIKE_POLY_COEF_TEST:'
      write ( *, '(a)' ) '  ZERNIKE_POLY_COEF determines the Zernike'
      write ( *, '(a)' ) '  polynomial coefficients.'

      do m = 0, n

        call zernike_poly_coef ( m, n, c )
     
        call r8poly_print ( n, c, '  Zernike polynomial' )

      end do

      return
      end
      subroutine zeta_test ( )

c*********************************************************************72
c
cc ZETA_TEST tests ZETA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer n_data
      double precision n_real
      double precision z1
      double precision z2
      double precision zeta

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ZETA_TEST'
      write ( *, '(a)' ) '  ZETA computes the Zeta function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '       N    exact Zeta    computed Zeta'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call zeta_values ( n_data, n, z1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        n_real = dble ( n )

        z2 = zeta ( n_real )

        write ( *, '(2x,i8,2x,g20.12,2x,g20.12)' ) n, z1, z2

      go to 10

20    continue
     
      return
      end
