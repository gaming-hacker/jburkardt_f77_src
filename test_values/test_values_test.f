      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_VALUES_TEST.
c
c  Discussion:
c
c    TEST_VALUES_TEST tests the TEST_VALUE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 May 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_VALUES_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_VALUES library.'

      call abram0_values_test ( )
      call abram1_values_test ( )
      call abram2_values_test ( )
      call agm_values_test ( )
      call airy_ai_values_test ( )
      call airy_ai_int_values_test ( )
      call airy_ai_prime_values_test ( )
      call airy_bi_values_test ( )
      call airy_bi_int_values_test ( )
      call airy_bi_prime_values_test ( )
      call airy_gi_values_test ( )
      call airy_hi_values_test ( )
      call arccos_values_test ( )
      call arccosh_values_test ( )
      call arcsin_values_test ( )
      call arcsinh_values_test ( )
      call arctan_values_test ( )
      call arctan_int_values_test ( )
      call arctan2_values_test ( )
      call arctanh_values_test ( )

      call bei0_values_test ( )
      call bei1_values_test ( )
      call bell_values_test ( )
      call ber0_values_test ( )
      call ber1_values_test ( )
      call bernoulli_number_values_test ( )
      call bernoulli_poly_values_test ( )
      call bernstein_poly_01_values_test ( )
      call bessel_i0_values_test ( )
      call bessel_i0_int_values_test ( )
      call bessel_i0_spherical_values_test ( )
      call bessel_i1_values_test ( )
      call bessel_i1_spherical_values_test ( )
      call bessel_in_values_test ( )
      call bessel_ix_values_test ( )
      call bessel_j0_values_test ( )
      call bessel_j0_int_values_test ( )
      call bessel_j0_spherical_values_test ( )
      call bessel_j1_values_test ( )
      call bessel_j1_spherical_values_test ( )
      call bessel_jn_values_test ( )
      call bessel_jx_values_test ( )
      call bessel_k0_values_test ( )
      call bessel_k0_int_values_test ( )
      call bessel_k1_values_test ( )
      call bessel_kn_values_test ( )
      call bessel_kx_values_test ( )
      call bessel_y0_values_test ( )
      call bessel_y0_int_values_test ( )
      call bessel_y0_spherical_values_test ( )
      call bessel_y1_values_test ( )
      call bessel_y1_spherical_values_test ( )
      call bessel_yn_values_test ( )
      call bessel_yx_values_test ( )
      call beta_cdf_values_test ( )
      call beta_inc_values_test ( )
      call beta_log_values_test ( )
      call beta_noncentral_cdf_values_test ( )
      call beta_values_test ( )
      call binomial_values_test ( )
      call binomial_cdf_values_test ( )
      call bivariate_normal_cdf_values_test ( )

      call catalan_values_test ( )
      call cauchy_cdf_values_test ( )
      call cbrt_values_test ( )
      call cheby_t_poly_values_test ( )
      call cheby_u_poly_values_test ( )
      call cheby_v_poly_values_test ( )
      call cheby_w_poly_values_test ( )
      call chi_values_test ( )
      call chi_square_cdf_values_test ( )
      call chi_square_noncentral_cdf_values_test ( )
      call ci_values_test ( )
      call cin_values_test ( )
      call cinh_values_test ( )
      call clausen_values_test ( )
      call clebsch_gordan_values_test ( )
      call collatz_count_values_test ( )
      call cos_values_test ( )
      call cos_degree_values_test ( )
      call cos_power_int_values_test ( )
      call cosh_values_test ( )
      call cot_values_test ( )
      call cp_values_test ( )

      call dawson_values_test ( )
      call debye1_values_test ( )
      call debye2_values_test ( )
      call debye3_values_test ( )
      call debye4_values_test ( )
      call dielectric_values_test ( )
      call dilogarithm_values_test ( )

      call e1_values_test ( )
      call ei_values_test ( )
      call elliptic_ea_values_test ( )
      call elliptic_ek_values_test ( )
      call elliptic_em_values_test ( )
      call elliptic_fa_values_test ( )
      call elliptic_fk_values_test ( )
      call elliptic_fm_values_test ( )
      call elliptic_pia_values_test ( )
      call elliptic_pik_values_test ( )
      call elliptic_pim_values_test ( )
      call erf_values_test ( )
      call erfc_values_test ( )
      call euler_number_values_test ( )
      call euler_poly_values_test ( )
      call exp_values_test ( )
      call exp3_int_values_test ( )
      call exponential_cdf_values_test ( )
      call extreme_values_cdf_values_test ( )

      call f_cdf_values_test ( )
      call f_noncentral_cdf_values_test ( )
      call fresnel_cos_values_test ( )
      call fresnel_sin_values_test ( )
      call frobenius_number_data_values_test ( )
      call frobenius_number_order_values_test ( )
      call frobenius_number_order2_values_test ( )

      call gamma_values_test ( )
      call gamma_cdf_values_test ( )
      call gamma_inc_values_test ( )
      call gamma_inc_p_values_test ( )
      call gamma_inc_q_values_test ( )
      call gamma_inc_tricomi_values_test ( )
      call gamma_log_values_test ( )
      call gegenbauer_poly_values_test ( )
      call geometric_cdf_values_test ( )
      call goodwin_values_test ( )
      call gud_values_test ( )

      call hermite_function_values_test ( )
      call hermite_poly_phys_values_test ( )
      call hermite_poly_prob_values_test ( )
      call hyper_1f1_values_test ( )
      call hyper_2f1_values_test ( )
      call hypergeometric_cdf_values_test ( )
      call hypergeometric_pdf_values_test ( )
      call hypergeometric_u_values_test ( )

      call i0ml0_values_test ( )
      call i1ml1_values_test ( )
      call i4_factorial_values_test ( )
      call i4_factorial2_values_test ( )
      call i4_fall_values_test ( )
      call i4_rise_values_test ( )
      call int_values_test ( )

      call jacobi_cn_values_test ( )
      call jacobi_dn_values_test ( )
      call jacobi_poly_values_test ( )
      call jacobi_sn_values_test ( )
      call jed_ce_values_test ( )
      call jed_mjd_values_test ( )
      call jed_rd_values_test ( )
      call jed_weekday_values_test ( )

      call kei0_values_test ( )
      call kei1_values_test ( )
      call ker0_values_test ( )
      call ker1_values_test ( )

      call laguerre_associated_values_test ( )
      call laguerre_general_values_test ( )
      call laguerre_polynomial_values_test ( )
      call lambert_w_values_test ( )
      call laplace_cdf_values_test ( )
      call legendre_associated_values_test ( )
      call legendre_associated_normalized_values_test ( )
      call legendre_associated_normalized_sphere_values_test ( )
      call legendre_poly_values_test ( )
      call legendre_function_q_values_test ( )
      call lerch_values_test ( )
      call lobachevsky_values_test ( )
      call lobatto_polynomial_values_test ( )
      call lobatto_polynomial_derivatives_test ( )
      call log_values_test ( )
      call log_normal_cdf_values_test ( )
      call log_series_cdf_values_test ( )
      call log10_values_test ( )
      call logarithmic_integral_values_test ( )
      call logistic_cdf_values_test ( )

      call mathieu_even_values_test ( )
      call mathieu_odd_values_test ( )
      call moebius_values_test ( )

      call negative_binomial_cdf_values_test ( )
      call nine_j_values_test ( )
      call normal_cdf_values_test ( )
      call normal_01_cdf_values_test ( )

      call omega_values_test ( )
      call owen_values_test ( )

      call partition_count_values_test ( )
      call partition_distinct_count_values_test ( )
      call phi_values_test ( )
      call pi_values_test ( )
      call poisson_cdf_values_test ( )
      call polylogarithm_values_test ( )
      call prandtl_values_test ( )
      call prime_values_test ( )
      call psat_values_test ( )
      call psi_values_test ( )

      call r8_factorial_values_test ( )
      call r8_factorial_log_values_test ( )
      call r8_factorial2_values_test ( )
      call r8_fall_values_test ( )
      call r8_rise_values_test ( )
      call rayleigh_cdf_values_test ( )

      call secvir_values_test ( )
      call shi_values_test ( )
      call si_values_test ( )
      call sigma_values_test ( )
      call sin_values_test ( )
      call sin_degree_values_test ( )
      call sin_power_int_values_test ( )
      call six_j_values_test ( )
      call sound_values_test ( )
      call sphere_unit_area_values_test ( )
      call sphere_unit_volume_values_test ( )
      call spherical_harmonic_values_test ( )
      call sqrt_values_test ( )
      call stirling1_values_test ( )
      call stirling2_values_test ( )
      call stromgen_values_test ( )
      call struve_h0_values_test ( )
      call struve_h1_values_test ( )
      call struve_l0_values_test ( )
      call struve_l1_values_test ( )
      call student_cdf_values_test ( )
      call student_noncentral_cdf_values_test ( )
      call subfactorial_values_test ( )
      call surten_values_test ( )
      call synch1_values_test ( )
      call synch2_values_test ( )

      call tau_values_test ( )
      call thercon_values_test ( )
      call three_j_values_test ( )
      call tran02_values_test ( )
      call tran03_values_test ( )
      call tran04_values_test ( )
      call tran05_values_test ( )
      call tran06_values_test ( )
      call tran07_values_test ( )
      call tran08_values_test ( )
      call tran09_values_test ( )
      call trigamma_values_test ( )
      call truncated_normal_a_cdf_values_test ( )
      call truncated_normal_a_pdf_values_test ( )
      call truncated_normal_ab_cdf_values_test ( )
      call truncated_normal_ab_pdf_values_test ( )
      call truncated_normal_b_cdf_values_test ( )
      call truncated_normal_b_pdf_values_test ( )
      call tsat_values_test ( )

      call van_der_corput_values_test ( )
      call viscosity_values_test ( )
      call von_mises_cdf_values_test ( )

      call weekday_values_test ( )
      call weibull_cdf_values_test ( )

      call zeta_values_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_VALUES_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine abram0_values_test ( )

c*********************************************************************72
c
cc ABRAM0_VALUES_TEST tests ABRAM0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ABRAM0_VALUES_TEST:'
      write ( *, '(a)' ) '  ABRAM0_VALUES returns values of '
      write ( *, '(a)' ) '  the Abramowitz function of order 0'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Abram0'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call abram0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine abram1_values_test ( )

c*********************************************************************72
c
cc ABRAM1_VALUES_TEST tests ABRAM1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ABRAM1_VALUES_TEST:'
      write ( *, '(a)' ) '  ABRAM1_VALUES returns values of '
      write ( *, '(a)' ) '  the Abramowitz function of order 1'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Abram1'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call abram1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine abram2_values_test ( )

c*********************************************************************72
c
cc ABRAM2_VALUES_TEST tests ABRAM2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ABRAM2_VALUES_TEST:'
      write ( *, '(a)' ) '  ABRAM2_VALUES returns values of '
      write ( *, '(a)' ) '  the Abramowitz function of order 2'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Abram2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call abram2_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine agm_values_test ( )

c*********************************************************************72
c
cc AGM_VALUES_TEST tests AGM_VALUES.
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
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AGM_VALUES_TEST:'
      write ( *, '(a)' ) '  AGM_VALUES returns'
      write ( *, '(a)' ) '  values of the arithmetic geometric '
      write ( *, '(a)' ) '  mean function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               B            AGM(A,B)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call agm_values ( n_data, a, b, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) a, b, fx

      go to 10

20    continue

      return
      end
      subroutine airy_ai_values_test ( )

c*********************************************************************72
c
cc AIRY_AI_VALUES_TEST tests AIRY_AI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_AI_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_AI_VALUES returns values of '
      write ( *, '(a)' ) '  the Airy function Ai(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Ai'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_ai_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_ai_int_values_test ( )

c*********************************************************************72
c
cc AIRY_AI_INT_VALUES_TEST tests AIRY_AI_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_AI_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_AI_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the integral of the Airy function Ai(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Ai_Int'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_ai_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_ai_prime_values_test ( )

c*********************************************************************72
c
cc AIRY_AI_PRIME_VALUES_TEST tests AIRY_AI_PRIME_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_AI_PRIME_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_AI_PRIME_VALUES returns values of '
      write ( *, '(a)' ) '  the derivative of the Airy function Ai(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           AiP'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_ai_prime_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_bi_values_test ( )

c*********************************************************************72
c
cc AIRY_BI_VALUES_TEST tests AIRY_BI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_BI_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_BI_VALUES returns values of '
      write ( *, '(a)' ) '  the Airy function Bi(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Bi'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_bi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_bi_int_values_test ( )

c*********************************************************************72
c
cc AIRY_BI_INT_VALUES_TEST tests AIRY_BI_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_BI_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_BI_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the integral of the Airy function Bi(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Bi_Int'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_bi_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_bi_prime_values_test ( )

c*********************************************************************72
c
cc AIRY_BI_PRIME_VALUES_TEST tests AIRY_BI_PRIME_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_BI_PRIME_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_BI_PRIME_VALUES returns values of '
      write ( *, '(a)' ) '  the derivative of the Airy function Bi(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           BiP'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_bi_prime_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_gi_values_test ( )

c*********************************************************************72
c
cc AIRY_GI_VALUES_TEST tests AIRY_GI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_GI_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_GI_VALUES returns values of '
      write ( *, '(a)' ) '  the modified Airy function Gi(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Gi'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_gi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine airy_hi_values_test ( )

c*********************************************************************72
c
cc AIRY_HI_VALUES_TEST tests AIRY_HI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'AIRY_HI_VALUES_TEST:'
      write ( *, '(a)' ) '  AIRY_HI_VALUES returns values of '
      write ( *, '(a)' ) '  the modified Airy function Hi(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           Hi'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call airy_hi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arccos_values_test ( )

c*********************************************************************72
c
cc ARCCOS_VALUES_TEST tests ARCCOS_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCCOS_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCCOS_VALUES returns values of '
      write ( *, '(a)' ) '  the arccosine function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         ARCCOS(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arccos_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arccosh_values_test ( )

c*********************************************************************72
c
cc ARCCOSH_VALUES_TEST tests ARCCOSH_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCCOSH_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCCOSH_VALUES returns values of '
      write ( *, '(a)' ) '  the hyperbolic arccosine function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         ARCCOSH(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arccosh_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arcsin_values_test ( )

c*********************************************************************72
c
cc ARCSIN_VALUES_TEST tests ARCSIN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCSIN_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCSIN_VALUES returns values of '
      write ( *, '(a)' ) '  the arcsine function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         ARCSIN(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arcsin_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arcsinh_values_test ( )

c*********************************************************************72
c
cc ARCSINH_VALUES_TEST tests ARCSINH_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCSINH_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCSINH_VALUES returns values of '
      write ( *, '(a)' ) '  the hyperbolic arcsine function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         ARCSINH(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arcsinh_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arctan_values_test ( )

c*********************************************************************72
c
cc ARCTAN_VALUES_TEST tests ARCTAN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCTAN_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCTAN_VALUES returns values of '
      write ( *, '(a)' ) '  the arctangent function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         ARCTAN(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arctan_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arctan_int_values_test ( )

c*********************************************************************72
c
cc ARCTAN_INT_VALUES_TEST tests ARCTAN_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCTAN_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCTAN_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the arctangent integral.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arctan_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine arctan2_values_test ( )

c*********************************************************************72
c
cc ARCTAN2_VALUES_TEST tests ARCTAN2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f
      integer n_data
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCTAN2_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCTAN2_VALUES returns values of '
      write ( *, '(a)' ) '  the arctangent function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X               Y          ARCTAN2(X,Y)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arctan2_values ( n_data, x, y, f )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) x, y, f

      go to 10

20    continue

      return
      end
      subroutine arctanh_values_test ( )

c*********************************************************************72
c
cc ARCTANH_VALUES_TEST tests ARCTANH_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ARCTANH_VALUES_TEST:'
      write ( *, '(a)' ) '  ARCTANH_VALUES returns values of '
      write ( *, '(a)' ) '  the hyperbolic arctangent function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X         ARCTANH(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call arctanh_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bei0_values_test ( )

c*********************************************************************72
c
cc BEI0_VALUES_TEST tests BEI0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BEI0_VALUES_TEST:'
      write ( *, '(a)' ) '  BEI0_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin BEI function of order 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bei0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bei1_values_test ( )

c*********************************************************************72
c
cc BEI1_VALUES_TEST tests BEI1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BEI1_VALUES_TEST'
      write ( *, '(a)' ) '  BEI1 VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin BEI function of order 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bei1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bell_values_test ( )

c*********************************************************************72
c
cc BELL_VALUES_TEST tests BELL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BELL_VALUES_TEST:'
      write ( *, '(a)' ) '  BELL_VALUES returns values of '
      write ( *, '(a)' ) '  the Bell numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N        BELL(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bell_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, c

      go to 10

20    continue

      return
      end
      subroutine ber0_values_test ( )

c*********************************************************************72
c
cc BER0_VALUES_TEST tests BER0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BER0_VALUES_TEST:'
      write ( *, '(a)' ) '  BER0_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin BER function of order 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call ber0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine ber1_values_test ( )

c*********************************************************************72
c
cc BER1_VALUES_TEST tests BER1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BER1_VALUES_TEST:'
      write ( *, '(a)' ) '  BER1_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin BER function of order 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bei1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bernoulli_number_values_test ( )

c*********************************************************************72
c
cc BERNOULLI_NUMBER_VALUES_TEST tests BERNOULLI_NUMBER_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_NUMBER_VALUES_TEST:'
      write ( *, '(a)' ) '  BERNOULLI_NUMBER_VALUES returns values'
      write ( *, '(a)' ) '  of the Bernoulli numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N        BERNOULLI_NUMBER(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bernoulli_number_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,6x,g24.16)' ) n, c

      go to 10

20    continue

      return
      end
      subroutine bernoulli_poly_values_test ( )

c*********************************************************************72
c
cc BERNOULLI_POLY_VALUES_TEST tests BERNOULLI_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNOULLI_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  BERNOULLI_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Bernoulli polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N          X            BERNOULLI_POLY(N,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bernoulli_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bernstein_poly_01_values_test ( )

c*********************************************************************72
c
cc BERNSTEIN_POLY_01_VALUES_TEST tests BERNSTEIN_POLY_01_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      integer k
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BERNSTEIN_POLY_01_VALUES_TEST:'
      write ( *, '(a)' ) '  BERNSTEIN_POLY_01_VALUES returns values of '
      write ( *, '(a)' ) '  the Bernstein polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N         K          X           BERNSTEIN(N,K)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bernstein_poly_01_values ( n_data, n, k, x, b )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g24.16)' ) n, k, x, b

      go to 10

20    continue

      return
      end
      subroutine bessel_i0_values_test ( )

c*********************************************************************72
c
cc BESSEL_I0_VALUES_TEST tests BESSEL_I0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_I0_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_I0_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel I0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_i0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_i0_int_values_test ( )

c*********************************************************************72
c
cc BESSEL_I0_INT_VALUES_TEST tests BESSEL_I0_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_I0_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_I0_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the integral of the Bessel I0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_i0_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_i0_spherical_values_test ( )

c*********************************************************************72
c
cc BESSEL_I0_SPHERICAL_VALUES_TEST tests BESSEL_I0_SPHERICAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_I0_SPHERICAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_I0_SPHERICAL_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical Bessel i0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_i0_spherical_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_i1_values_test ( )

c*********************************************************************72
c
cc BESSEL_I1_VALUES_TEST tests BESSEL_I1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_I1_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_I1_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel I1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_i1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_i1_spherical_values_test ( )

c*********************************************************************72
c
cc BESSEL_I1_SPHERICAL_VALUES_TEST tests BESSEL_I1_SPHERICAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_I1_SPHERICAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_I1_SPHERICAL_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical Bessel i1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_i1_spherical_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_in_values_test ( )

c*********************************************************************72
c
cc BESSEL_IN_VALUES_TEST tests BESSEL_IN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_IN_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_IN_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel In function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_in_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_ix_values_test ( )

c*********************************************************************72
c
cc BESSEL_IX_VALUES_TEST tests BESSEL_IX_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_IX_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_IX_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel In function with NONINTEGER'
      write ( *, '(a)' ) '  order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          N             X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_ix_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_j0_values_test ( )

c*********************************************************************72
c
cc BESSEL_J0_VALUES_TEST tests BESSEL_J0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_J0_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_J0_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel J0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_j0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_j0_int_values_test ( )

c*********************************************************************72
c
cc BESSEL_J0_INT_VALUES_TEST tests BESSEL_J0_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_J0_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_J0_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the integral of the Bessel J0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_j0_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_j0_spherical_values_test ( )

c*********************************************************************72
c
cc BESSEL_J0_SPHERICAL_VALUES_TEST tests BESSEL_J0_SPHERICAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_J0_SPHERICAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_J0_SPHERICAL_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical Bessel j0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_j0_spherical_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_j1_values_test ( )

c*********************************************************************72
c
cc BESSEL_J1_VALUES_TEST tests BESSEL_J1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_J1_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_J1_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel J10 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_j1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_j1_spherical_values_test ( )

c*********************************************************************72
c
cc BESSEL_J1_SPHERICAL_VALUES_TEST tests BESSEL_J1_SPHERICAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_J1_SPHERICAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_J1_SPHERICAL_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical Bessel j1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_j1_spherical_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_jn_values_test ( )

c*********************************************************************72
c
cc BESSEL_JN_VALUES_TEST tests BESSEL_JN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_JN_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_JN_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Jn function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_jn_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_jx_values_test ( )

c*********************************************************************72
c
cc BESSEL_JX_VALUES_TEST tests BESSEL_JX_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_JX_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_JX_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Jn function with NONINTEGER'
      write ( *, '(a)' ) '  order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          N             X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_jx_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_k0_values_test ( )

c*********************************************************************72
c
cc BESSEL_K0_VALUES_TEST tests BESSEL_K0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_K0_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_K0_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel K0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_k0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_k0_int_values_test ( )

c*********************************************************************72
c
cc BESSEL_K0_INT_VALUES_TEST tests BESSEL_K0_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_K0_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_K0_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the integral of the Bessel K0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_k0_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_k1_values_test ( )

c*********************************************************************72
c
cc BESSEL_K1_VALUES_TEST tests BESSEL_K1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_K1_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_K1_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel K1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_k1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_kn_values_test ( )

c*********************************************************************72
c
cc BESSEL_KN_VALUES_TEST tests BESSEL_KN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_KN_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_KN_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Kn function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_kn_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_kx_values_test ( )

c*********************************************************************72
c
cc BESSEL_KX_VALUES_TEST tests BESSEL_KX_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_KX_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_KX_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Kn function with NONINTEGER'
      write ( *, '(a)' ) '  order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          N             X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_kx_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_y0_values_test ( )

c*********************************************************************72
c
cc BESSEL_Y0_VALUES_TEST tests BESSEL_Y0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_Y0_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_Y0_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Y0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_y0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_y0_int_values_test ( )

c*********************************************************************72
c
cc BESSEL_Y0_INT_VALUES_TEST tests BESSEL_Y0_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_Y0_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_Y0_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the integral of the Bessel Y0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_y0_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_y0_spherical_values_test ( )

c*********************************************************************72
c
cc BESSEL_Y0_SPHERICAL_VALUES_TEST tests BESSEL_Y0_SPHERICAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_Y0_SPHERICAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_Y0_SPHERICAL_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical Bessel y0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_y0_spherical_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_y1_values_test ( )

c*********************************************************************72
c
cc BESSEL_Y1_VALUES_TEST tests BESSEL_Y1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_Y1_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_Y1_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Y1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_y1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_y1_spherical_values_test ( )

c*********************************************************************72
c
cc BESSEL_Y1_SPHERICAL_VALUES_TEST tests BESSEL_Y1_SPHERICAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_Y1_SPHERICAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_Y1_SPHERICAL_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical Bessel y1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_y1_spherical_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_yn_values_test ( )

c*********************************************************************72
c
cc BESSEL_YN_VALUES_TEST tests BESSEL_YN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_YN_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_YN_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Yn function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N          X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_yn_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine bessel_yx_values_test ( )

c*********************************************************************72
c
cc BESSEL_YX_VALUES_TEST tests BESSEL_YX_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BESSEL_YX_VALUES_TEST:'
      write ( *, '(a)' ) '  BESSEL_YX_VALUES returns values of '
      write ( *, '(a)' ) '  the Bessel Yn function with NONINTEGER'
      write ( *, '(a)' ) '  order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          N             X                     FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_yx_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine beta_cdf_values_test ( )

c*********************************************************************72
c
cc BETA_CDF_VALUES_TEST tests BETA_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  BETA_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Beta CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '      A             B                 X',
     &  '                     CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_cdf_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine beta_inc_values_test ( )

c*********************************************************************72
c
cc BETA_INC_VALUES_TEST tests BETA_INC_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_INC_VALUES_TEST:'
      write ( *, '(a)' ) '  BETA_INC_VALUES returns values of '
      write ( *, '(a)' ) '  the incomplete Beta function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               B               X           CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_inc_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine beta_log_values_test ( )

c*********************************************************************72
c
cc BETA_LOG_VALUES_TEST tests BETA_LOG_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fxy
      integer n_data
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_LOG_VALUES_TEST:'
      write ( *, '(a)' ) '  BETA_LOG_VALUES returns values of '
      write ( *, '(a)' ) '  the logarithm of the Beta function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X               Y           Log(BETA(X,Y))'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_log_values ( n_data, x, y, fxy )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f12.8,2x,g24.16)' ) 
     &    x, y, fxy

      go to 10

20    continue

      return
      end
      subroutine beta_noncentral_cdf_values_test ( )

c*********************************************************************72
c
cc BETA_NONCENTRAL_CDF_VALUES_TEST tests BETA_NONCENTRAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      double precision lambda
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_NONCENTRAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  BETA_NONCENTRAL_CDF_VALUES returns values'
      write ( *, '(a)' ) '  of the noncentral Beta CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '      A             B           LAMBDA              ',
     &  'X                     CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_noncentral_cdf_values ( n_data, a, b, lambda, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f12.8,2x,f12.8,2x,f24.16,2x,g24.16)' )
     &    a, b, lambda, x, fx

      go to 10

20    continue

      return
      end
      subroutine beta_values_test ( )

c*********************************************************************72
c
cc BETA_VALUES_TEST tests BETA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fxy
      integer n_data
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_VALUES_TEST:'
      write ( *, '(a)' ) '  BETA_VALUES returns values of '
      write ( *, '(a)' ) '  the Beta function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X               Y            BETA(X,Y)'
      write ( *, '(a)' ) ' '

      n_data = 0
 
10    continue

        call beta_values ( n_data, x, y, fxy )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) x, y, fxy

      go to 10

20    continue

      return
      end
      subroutine binomial_values_test ( )

c*********************************************************************72
c
cc BINOMIAL_VALUES_TEST tests BINOMIAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer b
      integer c
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BINOMIAL_VALUES_TEST:'
      write ( *, '(a)' ) '  BINOMIAL_VALUES returns values of '
      write ( *, '(a)' ) '  the binomial numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         A         B        C(A,B)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call binomial_values ( n_data, a, b, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i12)' ) a, b, c

      go to 10

20    continue

      return
      end
      subroutine binomial_cdf_values_test ( )

c*********************************************************************72
c
cc BINOMIAL_CDF_VALUES_TEST tests BINOMIAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      double precision b
      double precision fx
      integer n_data
      integer x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BINOMIAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  BINOMIAL_CDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the Binomial Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         A        B            X       CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call binomial_cdf_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,i8,2x,g24.16)' ) a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine bivariate_normal_cdf_values_test ( )

c*********************************************************************72
c
cc BIVARIATE_NORMAL_CDF_VALUES_TEST tests BIVARIATE_NORMAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 May 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fxy
      integer n_data
      double precision r
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BIVARIATE_NORMAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  BIVARIATE_NORMAL_CDF_VALUES returns values'
      write ( *, '(a)' ) '  of the bivariate normal CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     & '          X               Y               R           F(R)(X,Y)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bivariate_normal_cdf_values ( n_data, x, y, r, fxy )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6,2x,g14.6)' )
     &    x, y, r, fxy

      go to 10

20    continue

      return
      end
      subroutine catalan_values_test ( )

c*********************************************************************72
c
cc CATALAN_VALUES_TEST tests CATALAN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CATALAN_VALUES_TEST:'
      write ( *, '(a)' ) '  CATALAN_VALUES returns values of '
      write ( *, '(a)' ) '  the Catalan numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N          C(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call catalan_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, c

      go to 10

20    continue

      return
      end
      subroutine cauchy_cdf_values_test ( )

c*********************************************************************72
c
cc CAUCHY_CDF_VALUES_TEST tests CAUCHY_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CAUCHY_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  CAUCHY_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Cauchy Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      MU     SIGMA    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cauchy_cdf_values ( n_data, mu, sigma, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, x, fx

      go to 10

20    continue

      return
      end
      subroutine cbrt_values_test ( )

c*********************************************************************72
c
cc CBRT_VALUES_TEST tests CBRT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CBRT_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  CBRT_VALUES returns values of the cube root.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X         CBRT(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cbrt_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cheby_t_poly_values_test ( )

c*********************************************************************72
c
cc CHEBY_T_POLY_VALUES_TEST tests CHEBY_T_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_T_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  CHEBY_T_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Chebyshev T polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       X      T(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cheby_t_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine cheby_u_poly_values_test ( )

c*********************************************************************72
c
cc CHEBY_U_POLY_VALUES_TEST tests CHEBY_U_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_U_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  CHEBY_U_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Chebyshev U polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       X      U(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cheby_u_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine cheby_v_poly_values_test ( )

c*********************************************************************72
c
cc CHEBY_V_POLY_VALUES_TEST tests CHEBY_V_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_V_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  CHEBY_V_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Chebyshev V polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       X      V(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cheby_v_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine cheby_w_poly_values_test ( )

c*********************************************************************72
c
cc CHEBY_W_POLY_VALUES_TEST tests CHEBY_W_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHEBY_W_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  CHEBY_W_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Chebyshev W polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       X      W(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cheby_w_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine chi_values_test ( )

c*********************************************************************72
c
cc CHI_VALUES_TEST tests CHI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHI_VALUES_TEST:'
      write ( *, '(a)' ) '  CHI_VALUES returns values of '
      write ( *, '(a)' ) '  the Hyperbolic Cosine Integral function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X      CHI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call chi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine chi_square_cdf_values_test ( )

c*********************************************************************72
c
cc CHI_SQUARE_CDF_VALUES_TEST tests CHI_SQUARE_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHI_SQUARE_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  CHI_SQUARE_CDF_VALUES returns values of the'
      write ( *, '(a)' ) '  Chi-Squared Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       X    CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call chi_square_cdf_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) a, x, fx

      go to 10

20    continue

      return
      end
      subroutine chi_square_noncentral_cdf_values_test ( )

c*********************************************************************72
c
cc CHI_SQUARE_NONCENTRAL_CDF_VALUES_TEST tests CHI_SQUARE_NONCENTRAL_CDF_VALUES.
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

      integer df
      double precision fx
      double precision lambda
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  CHI_SQUARE_NONCENTRAL_CDF_VALUES returns'
      write ( *, '(a)' ) '  values of the noncentral Chi-Squared '
      write ( *, '(a)' ) '  Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     DF      LAMBDA       X     CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call chi_square_noncentral_cdf_values ( n_data, df, lambda, 
     &    x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16,2x,g24.16)' ) 
     &    df, lambda, x, fx

      go to 10

20    continue

      return
      end
      subroutine ci_values_test ( )

c*********************************************************************72
c
cc CI_VALUES_TEST tests CI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CI_VALUES_TEST:'
      write ( *, '(a)' ) '  CI_VALUES returns values of '
      write ( *, '(a)' ) '  the Cosine Integral function CI(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            CI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call ci_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cin_values_test ( )

c*********************************************************************72
c
cc CIN_VALUES_TEST tests CIN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CIN_VALUES_TEST:'
      write ( *, '(a)' ) '  CIN_VALUES returns values of '
      write ( *, '(a)' ) '  the Cosine Integral function CIN(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            CIN(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cin_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cinh_values_test ( )

c*********************************************************************72
c
cc CINH_VALUES_TEST tests CINH_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CINH_VALUES_TEST:'
      write ( *, '(a)' ) '  CINH_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the Hyperbolic Cosine Integral function CINH(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            CINH(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cinh_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine clausen_values_test ( )

c*********************************************************************72
c
cc CLAUSEN_VALUES_TEST tests CLAUSEN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CLAUSEN_VALUES_TEST:'
      write ( *, '(a)' ) '  CLAUSEN_VALUES returns values of '
      write ( *, '(a)' ) '  Clausen''s integral.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call clausen_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine clebsch_gordan_values_test ( )

c*********************************************************************72
c
cc CLEBSCH_GORDAN_VALUES_TEST tests CLEBSCH_GORDAN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision j1
      double precision j2
      double precision j3
      double precision m1
      double precision m2
      double precision m3
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CLEBSCH_GORDAN_VALUES_TEST:'
      write ( *, '(a)' ) '  CLEBSCH_GORDAN_VALUES returns values of '
      write ( *, '(a)' ) '  the Clebsch-Gordan coefficient.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '      J1      J2      J3      ',
     &  'M1      M2      M3        CG'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call clebsch_gordan_values ( n_data, j1, j2, j3, m1, m2, 
     &  m3, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &  '(2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,g24.16)' ) 
     &    j1, j2, j3, m1, m2, m3, fx

      go to 10

20    continue

      return
      end
      subroutine collatz_count_values_test ( )

c*********************************************************************72
c
cc COLLATZ_COUNT_VALUES_TEST tests COLLATZ_COUNT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer count
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COLLATZ_COUNT_VALUES_TEST:'
      write ( *, '(a)' ) '  COLLATZ_COUNT_VALUES returns values of '
      write ( *, '(a)' ) '  the length of the Collatz sequence that'
      write ( *, '(a)' ) '  starts at N.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N      COLLATZ_COUNT(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call collatz_count_values ( n_data, n, count )
    
        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, count

      go to 10

20    continue

      return
      end
      subroutine cos_values_test ( )

c*********************************************************************72
c
cc COS_VALUES_TEST tests COS_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COS_VALUES_TEST:'
      write ( *, '(a)' ) '  COS_VALUES returns values of '
      write ( *, '(a)' ) '  the cosine function COS(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            COS(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cos_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cos_degree_values_test ( )

c*********************************************************************72
c
cc COS_DEGREE_VALUES_TEST tests COS_DEGREE_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COS_DEGREE_VALUES_TEST:'
      write ( *, '(a)' ) '  COS_DEGREE_)VALUES returns values of '
      write ( *, '(a)' ) '  the cosine function COS(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            COS(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cos_degree_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cos_power_int_values_test ( )

c*********************************************************************72
c
cc COS_POWER_INT_VALUES_TEST tests COS_POWER_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COS_POWER_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  COS_POWER_INT returns values of '
      write ( *, '(a)' ) '  the integral of COS(X)^N from A to B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '          A               B              N',
     &  '    COS_POWER_INT(A,B,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cos_power_int_values ( n_data, a, b, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,i8,2x,g14.6)' ) a, b, n, fx

      go to 10

20    continue

      return
      end
      subroutine cosh_values_test ( )

c*********************************************************************72
c
cc COSH_VALUES_TEST tests COSH_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COSH_VALUES_TEST:'
      write ( *, '(a)' ) '  COSH_VALUES returns values of '
      write ( *, '(a)' ) '  the hyperbolic cosine function COSH(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            COSH(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cosh_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cot_values_test ( )

c*********************************************************************72
c
cc COT_VALUES_TEST tests COT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COT_VALUES_TEST:'
      write ( *, '(a)' ) '  COT_VALUES returns values of '
      write ( *, '(a)' ) '  the cotangent function COT(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            COT(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cot_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine cp_values_test ( )

c*********************************************************************72
c
cc CP_VALUES_TEST tests CP_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cp
      integer n_data
      double precision p
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CP_VALUES_TEST:'
      write ( *, '(a)' ) '  CP_VALUES returns values of '
      write ( *, '(a)' ) '  the specific heat CP '
      write ( *, '(a)' ) '  as a function of temperature and pressure.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            P            CP(T,P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call cp_values ( n_data, tc, p, cp )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g14.6)' ) tc, p, cp

      go to 10

20    continue

      return
      end
      subroutine dawson_values_test ( )

c*********************************************************************72
c
cc DAWSON_VALUES_TEST tests DAWSON_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DAWSON_VALUES_TEST:'
      write ( *, '(a)' ) '  DAWSON_VALUES returns values of '
      write ( *, '(a)' ) '  Dawson''s Integral function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          DAWSON(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call dawson_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine debye1_values_test ( )

c*********************************************************************72
c
cc DEBYE1_VALUES_TEST tests DEBYE1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DEBYE1_VALUES_TEST:'
      write ( *, '(a)' ) '  DEBYE1_VALUES returns values of '
      write ( *, '(a)' ) '  Debye''s function of order 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          DEBYE1(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call debye1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine debye2_values_test ( )

c*********************************************************************72
c
cc DEBYE2_VALUES_TEST tests DEBYE2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DEBYE2_VALUES_TEST:'
      write ( *, '(a)' ) '  DEBYE2_VALUES returns values of '
      write ( *, '(a)' ) '  Debye''s function of order 2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          DEBYE2(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call debye2_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine debye3_values_test ( )

c*********************************************************************72
c
cc DEBYE3_VALUES_TEST tests DEBYE3_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DEBYE3_VALUES_TEST:'
      write ( *, '(a)' ) '  DEBYE3_VALUES returns values of '
      write ( *, '(a)' ) '  Debye''s function of order 3.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          DEBYE3(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call debye3_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine debye4_values_test ( )

c*********************************************************************72
c
cc DEBYE4_VALUES_TEST tests DEBYE4_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DEBYE4_VALUES_TEST:'
      write ( *, '(a)' ) '  DEBYE4_VALUES returns values of '
      write ( *, '(a)' ) '  Debye''s function of order 4.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          DEBYE4(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call debye4_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine dielectric_values_test ( )

c*********************************************************************72
c
cc DIELECTRIC_VALUES_TEST tests DIELECTRIC_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision eps
      integer n_data
      double precision p
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIELECTRIC_VALUES_TEST:'
      write ( *, '(a)' ) '  DIELECTRIC_VALUES returns values of '
      write ( *, '(a)' ) '  the dielectric function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T           P            EPS(T,P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call dielectric_values ( n_data, tc, p, eps )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g14.6)' ) tc, p, eps

      go to 10

20    continue

      return
      end
      subroutine dilogarithm_values_test ( )

c*********************************************************************72
c
cc DILOGARITHM_VALUES_TEST tests DILOGARITHM_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DILOGARITHM_VALUES_TEST:'
      write ( *, '(a)' ) '  DILOGARITHM_VALUES returns values of'
      write ( *, '(a)' ) '  the dilogarithm function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          DILOGARITHM(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call dilogarithm_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine e1_values_test ( )

c*********************************************************************72
c
cc E1_VALUES_TEST tests E1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'E1_VALUES_TEST:'
      write ( *, '(a)' ) '  E1_VALUES returns values of'
      write ( *, '(a)' ) '  the exponential integral function E1(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          E1(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call e1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine ei_values_test ( )

c*********************************************************************72
c
cc EI_VALUES_TEST tests EI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EI_VALUES_TEST:'
      write ( *, '(a)' ) '  EI_VALUES returns values of'
      write ( *, '(a)' ) '  the exponential integral function EI(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          EI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call ei_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_ea_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_EA_VALUES_TEST tests ELLIPTIC_EA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_EA_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_EA_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the complete elliptic integral of the second'
      write ( *, '(a)' ) 
     &  '  kind, with parameter angle ALPHA in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    ALPHA        E(ALPHA)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_ea_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_ek_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_EK_VALUES_TEST tests ELLIPTIC_EK_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 May 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_EK_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_EK_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the complete elliptic integral of the second'
      write ( *, '(a)' ) '  kind, with parameter modulus K.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      K            E(K)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_ek_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_em_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_EM_VALUES_TEST tests ELLIPTIC_EM_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_EM_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_EM_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the complete elliptic integral of the second'
      write ( *, '(a)' ) '  kind, with parameter modulus M.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      M            E(M)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_em_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_fa_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_FA_VALUES_TEST tests ELLIPTIC_FA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_FA_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_FA_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the complete elliptic integral of the first'
      write ( *, '(a)' ) 
     &  '  kind, with parameter angle ALPHA in degrees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    ALPHA        F(ALPHA)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_fa_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_fk_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_FK_VALUES_TEST tests ELLIPTIC_FK_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 May 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_FK_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_FK_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the complete elliptic integral of the first'
      write ( *, '(a)' ) 
     &  '  kind, with parameter K.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    K            F(K)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_fk_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_fm_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_FM_VALUES_TEST tests ELLIPTIC_FM_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_FM_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_FM_VALUES returns values of '
      write ( *, '(a)' ) '  the complete elliptic integral of the first'
      write ( *, '(a)' ) '  kind, with parameter modulus M.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      M            F(M)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_fm_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine elliptic_pia_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_PIA_VALUES_TEST tests ELLIPTIC_PIA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 May 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision n
      integer n_data
      double precision pia

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_PIA_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_PIA_VALUES returns values of '
      write ( *, '(a)' ) '  the complete elliptic integral of the'
      write ( *, '(a)' ) '  third kind, with parameter angle A.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      N           A            Pi(N,A)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_pia_values ( n_data, n, a, pia )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) n, a, pia

      go to 10

20    continue

      return
      end
      subroutine elliptic_pik_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_PIK_VALUES_TEST tests ELLIPTIC_PIK_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 May 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision k
      double precision n
      integer n_data
      double precision pik

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_PIK_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_PIK_VALUES returns values of '
      write ( *, '(a)' ) '  the complete elliptic integral of the'
      write ( *, '(a)' ) '  third kind, with parameter K.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      N           K            Pi(N,K)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_pik_values ( n_data, n, k, pik )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) n, k, pik

      go to 10

20    continue

      return
      end
      subroutine elliptic_pim_values_test ( )

c*********************************************************************72
c
cc ELLIPTIC_PIM_VALUES_TEST tests ELLIPTIC_PIM_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 May 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision m
      double precision n
      integer n_data
      double precision pim

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ELLIPTIC_PIM_VALUES_TEST:'
      write ( *, '(a)' ) '  ELLIPTIC_PIM_VALUES returns values of '
      write ( *, '(a)' ) '  the complete elliptic integral of the'
      write ( *, '(a)' ) '  third kind, with parameter modulus M.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      N           M            Pi(N,M)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call elliptic_pim_values ( n_data, n, m, pim )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) n, m, pim

      go to 10

20    continue

      return
      end
      subroutine erf_values_test ( )

c*********************************************************************72
c
cc ERF_VALUES_TEST tests ERF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ERF_VALUES_TEST:'
      write ( *, '(a)' ) '  ERF_VALUES returns values of '
      write ( *, '(a)' ) '  the error function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X               ERF(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call erf_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine erfc_values_test ( )

c*********************************************************************72
c
cc ERFC_VALUES_TEST tests ERFC_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 May 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ERFC_VALUES_TEST:'
      write ( *, '(a)' ) '  ERFC_VALUES returns values of '
      write ( *, '(a)' ) '  the complementary error function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X               ERFC(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call erfc_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine euler_number_values_test ( )

c*********************************************************************72
c
cc EULER_NUMBER_VALUES_TEST tests EULER_NUMBER_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EULER_NUMBER_VALUES_TEST:'
      write ( *, '(a)' ) '  EULER_NUMBER_VALUES returns values of '
      write ( *, '(a)' ) '  the Euler numbers.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N        EULER_NUMBER(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call euler_number_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, c

      go to 10

20    continue

      return
      end
      subroutine euler_poly_values_test ( )

c*********************************************************************72
c
cc EULER_POLY_VALUES_TEST tests EULER_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EULER_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  EULER_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Euler polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N     X             EULER_POLY(N,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call euler_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine exp_values_test ( )

c*********************************************************************72
c
cc EXP_VALUES_TEST tests EXP_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXP_VALUES_TEST:'
      write ( *, '(a)' ) '  EXP_VALUES returns values of '
      write ( *, '(a)' ) '  the exponential function'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           exp(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call exp_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine exp3_int_values_test ( )

c*********************************************************************72
c
cc EXP3_INT_VALUES_TEST tests EXP3_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXP3_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  EXP3_INT_VALUES returns values of '
      write ( *, '(a)' ) '  the EXP3 Integral function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          EXP3_INT(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call exp3_int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine exponential_cdf_values_test ( )

c*********************************************************************72
c
cc EXPONENTIAL_CDF_VALUES_TEST tests EXPONENTIAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision lambda
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXPONENTIAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  EXPONENTIAL_CDF_VALUES returns values of'
      write ( *, '(a)' ) '  the Exponential CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      LAMBDA       X          CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call exponential_cdf_values ( n_data, lambda, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) lambda, x, fx

      go to 10

20    continue

      return
      end
      subroutine extreme_values_cdf_values_test ( )

c*********************************************************************72
c
cc EXTREME_VALUES_CDF_VALUES_TEST tests EXTREME_VALUES_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha
      double precision beta
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EXTREME_VALUES_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  EXTREME_VALUES_CDF_VALUES returns values'
      write ( *, '(a)' ) 
     &  '  of the Extreme Values Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      ALPHA  BETA     X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call extreme_values_cdf_values ( n_data, alpha, beta, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    alpha, beta, x, fx

      go to 10

20    continue

      return
      end
      subroutine f_cdf_values_test ( )

c*********************************************************************72
c
cc F_CDF_VALUES_TEST tests F_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer b
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'F_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  F_CDF_VALUES returns values of'
      write ( *, '(a)' ) '  the F cumulative density function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       A       B      X            CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call f_cdf_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g24.16)' ) a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine f_noncentral_cdf_values_test ( )

c*********************************************************************72
c
cc F_NONCENTRAL_CDF_VALUES_TEST tests F_NONCENTRAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer a
      integer b
      double precision fx
      double precision lambda
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'F_NONCENTRAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  F_NONCENTRAL_CDF_VALUES returns values of'
      write ( *, '(a)' ) '  the F cumulative density function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       A       B      LAMBDA    X            CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call f_noncentral_cdf_values ( n_data, a, b, lambda, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f10.6,2x,g14.6,2x,g14.6)' ) 
     &    a, b, lambda, x, fx

      go to 10

20    continue

      return
      end
      subroutine fresnel_cos_values_test ( )

c*********************************************************************72
c
cc FRESNEL_COS_VALUES_TEST tests FRESNEL_COS_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FRESNEL_COS_VALUES_TEST:'
      write ( *, '(a)' ) '  FRESNEL_COS_VALUES returns values of'
      write ( *, '(a)' ) '  the Fresnel cosine integral C(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X           C(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call fresnel_cos_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine fresnel_sin_values_test ( )

c*********************************************************************72
c
cc FRESNEL_SIN_VALUES_TEST tests FRESNEL_SIN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FRESNEL_SIN_VALUES_TEST:'
      write ( *, '(a)' ) '  FRESNEL_SIN_VALUES returns values of'
      write ( *, '(a)' ) '  the Fresnel sine integral S(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           S(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call fresnel_sin_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine frobenius_number_data_values_test ( )

c***********************************************************************72
c
cc FROBENIUS_NUMBER_DATA_VALUES_TEST tests FROBENIUS_NUMBER_DATA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c(20)
      integer f
      integer n_data
      integer order

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FROBENIUS_NUMBER_DATA_VALUES_TEST:'
      write ( *, '(a)' ) '  FROBENIUS_NUMBER_DATA_VALUES returns'
      write ( *, '(a)' ) '  the data of a Frobenius problem'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call frobenius_number_order_values ( n_data, order )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call frobenius_number_data_values ( n_data, order, c, f )

        write ( *, '(a)'        ) ' '
        write ( *, '(a,i8)'    ) '  Order = ', order
        write ( *, '(10(2x,i6))' ) c(1:order)
        write ( *, '(a,i12)'   ) '  Frobenius number = ', f

      go to 10

20    continue

      return
      end
      subroutine frobenius_number_order_values_test ( )

c***********************************************************************72
c
cc FROBENIUS_NUMBER_ORDER_VALUES_TEST tests FROBENIUS_NUMBER_ORDER_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      integer order

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FROBENIUS_NUMBER_ORDER_VALUES_TEST:'
      write ( *, '(a)' ) '  FROBENIUS_NUMBER_ORDER_VALUES returns'
      write ( *, '(a)' ) '  the order of a Frobenius problem'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Problem   Order'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call frobenius_number_order_values ( n_data, order )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

       write ( *, '(2x,i4,2x,i8)' ) n_data, order

      go to 10

20    continue

      return
      end
      subroutine frobenius_number_order2_values_test ( )

c***********************************************************************72
c
cc FROBENIUS_NUMBER_ORDER2_VALUES_TEST tests FROBENIUS_NUMBER_ORDER2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c1
      integer c2
      integer f
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FROBENIUS_NUMBER_ORDER2_VALUES_TEST:'
      write ( *, '(a)' ) '  FROBENIUS_NUMBER_ORDER2_VALUES returns'
      write ( *, '(a)' ) '  values of the Frobenius number of order 2.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        C1        C2     F(C1,C2)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call frobenius_number_order2_values ( n_data, c1, c2, f )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

       write ( *, '(2x,i8,2x,i8,2x,i8)' ) c1, c2, f

      go to 10

20    continue

      return
      end
      subroutine gamma_values_test ( )

c*********************************************************************72
c
cc GAMMA_VALUES_TEST tests GAMMA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  GAMMA_VALUES returns values of the Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X            GAMMA(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine gamma_cdf_values_test ( )

c*********************************************************************72
c
cc GAMMA_CDF_VALUES_TEST tests GAMMA_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  GAMMA_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the GAMMA Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      MU     SIGMA    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_cdf_values ( n_data, mu, sigma, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, x, fx

      go to 10

20    continue

      return
      end
      subroutine gamma_inc_values_test ( )

c*********************************************************************72
c
cc GAMMA_INC_VALUES_TEST tests GAMMA_INC_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_INC_VALUES_TEST:'
      write ( *, '(a)' ) '  GAMMA_INC_VALUES returns values of '
      write ( *, '(a)' ) '  the incomplete Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               X           CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_inc_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    a, x, fx

      go to 10

20    continue

      return
      end
      subroutine gamma_inc_p_values_test ( )

c*********************************************************************72
c
cc GAMMA_INC_P_VALUES_TEST tests GAMMA_INC_P_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_INC_P_VALUES_TEST:'
      write ( *, '(a)' ) '  GAMMA_INC_P_VALUES returns values of '
      write ( *, '(a)' ) '  the incomplete P Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               X           CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_inc_p_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    a, x, fx

      go to 10

20    continue

      return
      end
      subroutine gamma_inc_q_values_test ( )

c*********************************************************************72
c
cc GAMMA_INC_Q_VALUES_TEST tests GAMMA_INC_Q_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_INC_Q_VALUES_TEST:'
      write ( *, '(a)' ) '  GAMMA_INC_Q_VALUES returns values of '
      write ( *, '(a)' ) '  the incomplete Q Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               X           CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_inc_q_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    a, x, fx

      go to 10

20    continue

      return
      end
      subroutine gamma_inc_tricomi_values_test ( )

c*********************************************************************72
c
cc GAMMA_INC_TRICOMI_VALUES_TEST tests GAMMA_INC_TRICOMI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_INC_TRICOMI_VALUES_TEST:'
      write ( *, '(a)' ) '  GAMMA_INC_TRICOMI_VALUES returns values of '
      write ( *, '(a)' ) '  the incomplete Tricomi Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               X           CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_inc_tricomi_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    a, x, fx

      go to 10

20    continue

      return
      end
      subroutine gamma_log_values_test ( )

c*********************************************************************72
c
cc GAMMA_LOG_VALUES_TEST tests GAMMA_LOG_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GAMMA_LOG_VALUES_TEST:'
      write ( *, '(a)' ) '  GAMMA_LOG_VALUES returns values of '
      write ( *, '(a)' ) '  the logarithm of the Gamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gamma_log_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine gegenbauer_poly_values_test ( )

c*********************************************************************72
c
cc GEGENBAUER_POLY_VALUES_TEST tests GEGENBAUER_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEGENBAUER_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  GEGENBAUER_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Gegenbauer polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N       A            X     G(N,A)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gegenbauer_poly_values ( n_data, n, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,f10.4,2x,g14.6)' ) n, a, x, fx

      go to 10

20    continue

      return
      end
      subroutine geometric_cdf_values_test ( )

c*********************************************************************72
c
cc GEOMETRIC_CDF_VALUES_TEST tests GEOMETRIC_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cdf
      integer n_data
      double precision p
      integer x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GEOMETRIC_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  GEOMETRIC_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Geometric Probability '
      write ( *, '(a)' ) '  Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X      P       CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call geometric_cdf_values ( n_data, x, p, cdf )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) x, p, cdf

      go to 10

20    continue

      return
      end
      subroutine goodwin_values_test ( )

c*********************************************************************72
c
cc GOODWIN_VALUES_TEST tests GOODWIN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GOODWIN_VALUES_TEST:'
      write ( *, '(a)' ) '  GOODWIN_VALUES returns values of'
      write ( *, '(a)' ) '  the Goodwin and Staton function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call goodwin_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine gud_values_test ( )

c*********************************************************************72
c
cc GUD_VALUES_TEST tests GUD_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GUD_VALUES_TEST:'
      write ( *, '(a)' ) '  GUD_VALUES returns values of '
      write ( *, '(a)' ) '  the Gudermannian function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            GUD(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call gud_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine hermite_function_values_test ( )

c*********************************************************************72
c
cc HERMITE_FUNCTION_VALUES_TEST tests HERMITE_FUNCTION_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_FUNCTION_VALUES_TEST:'
      write ( *, '(a)' ) '  HERMITE_FUNCTION_VALUES returns values of '
      write ( *, '(a)' ) '  the Hermite function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X            Hf(N,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hermite_function_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine hermite_poly_phys_values_test ( )

c*********************************************************************72
c
cc HERMITE_POLY_PHYS_VALUES_TEST tests HERMITE_POLY_PHYS_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLY_PHYS_VALUES_TEST:'
      write ( *, '(a)' ) '  HERMITE_POLY_PHYS_VALUES returns values of '
      write ( *, '(a)' ) '  the physicist''s Hermite polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X            H(N,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hermite_poly_phys_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine hermite_poly_prob_values_test ( )

c*********************************************************************72
c
cc HERMITE_POLY_PROB_VALUES_TEST tests HERMITE_POLY_PROB_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HERMITE_POLY_PROB_VALUES_TEST:'
      write ( *, '(a)' ) '  HERMITE_POLY_PROB_VALUES returns values of '
      write ( *, '(a)' ) '  the probabilist''s Hermite polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      X            He(N,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hermite_poly_prob_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine hyper_1f1_values_test ( )

c*********************************************************************72
c
cc HYPER_1F1_VALUES_TEST tests HYPER_1F1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPER_1F1_VALUES_TEST:'
      write ( *, '(a)' ) '  HYPER_1F1_VALUES returns values of '
      write ( *, '(a)' ) '  the hypergeometric 1F1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '         A           B            ',
     &  'X       Hyper_1F1(A,B,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hyper_1f1_values ( n_data, a, b, x, fx )

        if ( n_data == 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.6,2x,f10.6,2x,f10.6,2x,g24.16)' ) 
     &    a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine hyper_2f1_values_test ( )

c*********************************************************************72
c
cc HYPER_2F1_VALUES_TEST tests HYPER_2F1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 September 2007
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
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPER_2F1_VALUES_TEST:'
      write ( *, '(a)' ) '  HYPER_2F1_VALUES returns values of '
      write ( *, '(a)' ) '  the hypergeometric 2F1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '         A           B           C            ',
     &  'X       Hyper_2F1(A,B,C,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hyper_2f1_values ( n_data, a, b, c, x, fx )

        if ( n_data == 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.6,2x,f10.6,2x,f10.6,2x,f10.6,2x,g24.16)' ) 
     &    a, b, c, x, fx

      go to 10

20    continue

      return
      end
      subroutine hypergeometric_cdf_values_test ( )

c*********************************************************************72
c
cc HYPERGEOMETRIC_CDF_VALUES_TEST tests HYPERGEOMETRIC_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      integer pop
      integer sam
      integer suc
      integer x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERGEOMETRIC_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  HYPERGEOMETRIC_CDF_VALUES returns values'
      write ( *, '(a)' ) '  of the Hypergeometric '
      write ( *, '(a)' ) '  Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       SAM      SUC     POP       X   HyperCDF(S,S,P)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hypergeometric_cdf_values ( n_data, sam, suc, pop, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8,2x,g24.16)' ) 
     &  sam, suc, pop, x, fx

      go to 10

20    continue

      return
      end
      subroutine hypergeometric_pdf_values_test ( )

c*********************************************************************72
c
cc HYPERGEOMETRIC_PDF_VALUES_TEST tests HYPERGEOMETRIC_PDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 January 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      integer pop
      integer sam
      integer suc
      integer x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERGEOMETRIC_PDF_VALUES_TEST:'
      write ( *, '(a)' ) '  HYPERGEOMETRIC_PDF_VALUES returns values'
      write ( *, '(a)' ) '  of the Hypergeometric '
      write ( *, '(a)' ) '  Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       SAM      SUC     POP       X   HyperPDF(S,S,P)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hypergeometric_pdf_values ( n_data, sam, suc, pop, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8,2x,g24.16)' ) 
     &  sam, suc, pop, x, fx

      go to 10

20    continue

      return
      end
      subroutine hypergeometric_u_values_test ( )

c*********************************************************************72
c
cc HYPERGEOMETRIC_U_VALUES_TEST tests HYPERGEOMETRIC_U_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERGEOMETRIC_U_VALUES_TEST:'
      write ( *, '(a)' ) '  HYPERGEOMETRIC_VALUES returns values of '
      write ( *, '(a)' ) '  the hypergeometric U function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '         A           B            ',
     &  'X       HyperU(A,B,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call hypergeometric_u_values ( n_data, a, b, x, fx )

        if ( n_data == 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.6,2x,f10.6,2x,f10.6,2x,g24.16)' ) 
     &    a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine i0ml0_values_test ( )

c*********************************************************************72
c
cc I0ML0_VALUES_TEST tests I0ML0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I0ML0_VALUES_TEST:'
      write ( *, '(a)' ) '  I0ML0_VALUES returns values of'
      write ( *, '(a)' ) '  the I0ML0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i0ml0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine i1ml1_values_test ( )

c*********************************************************************72
c
cc I1ML1_VALUES_TEST tests I1ML1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I1ML1_VALUES_TEST:'
      write ( *, '(a)' ) '  I1ML1_VALUES returns values of'
      write ( *, '(a)' ) '  the I1ML1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i1ml1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine i4_factorial_values_test ( )

c*********************************************************************72
c
cc I4_FACTORIAL_VALUES_TEST tests I4_FACTORIAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTORIAL_VALUES_TEST:'
      write ( *, '(a)' ) '  I4_FACTORIAL_VALUES returns values of '
      write ( *, '(a)' ) '  the factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         Nc'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_factorial_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine i4_factorial2_values_test ( )

c*********************************************************************72
c
cc I4_FACTORIAL2_VALUES_TEST tests I4_FACTORIAL2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FACTORIAL2_VALUES_TEST:'
      write ( *, '(a)' ) '  I4_FACTORIAL2_VALUES returns values of '
      write ( *, '(a)' ) '  the double factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         N!!'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_factorial2_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine i4_fall_values_test ( )

c*********************************************************************72
c
cc I4_FALL_VALUES_TEST tests I4_FALL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fmn
      integer m
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_FALL_VALUES_TEST:'
      write ( *, '(a)' ) '  I4_FALL_VALUES returns some exact'
      write ( *, '(a)' ) '  values of the rising factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       M       N      I4_FALL(M,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_fall_values ( n_data, m, n, fmn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i12)' ) m, n, fmn

      go to 10

20    continue

      return
      end
      subroutine i4_rise_values_test ( )

c*********************************************************************72
c
cc I4_RISE_VALUES_TEST tests I4_RISE_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fmn
      integer m
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_RISE_VALUES_TEST:'
      write ( *, '(a)' ) '  I4_RISE_VALUES returns some exact'
      write ( *, '(a)' ) '  values of the rising factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       M       N      I4_RISE(M,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call i4_rise_values ( n_data, m, n, fmn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i12)' ) m, n, fmn

      go to 10

20    continue

      return
      end
      subroutine int_values_test ( )

c*********************************************************************72
c
cc INT_VALUES_TEST tests INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'INT_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  INT_VALUES returns the integer part of a real number.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X         I(NT(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call int_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine jacobi_cn_values_test ( )

c*********************************************************************72
c
cc JACOBI_CN_VALUES_TEST tests JACOBI_CN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_CN_VALUES_TEST:'
      write ( *, '(a)' ) '  JACOBI_CN_VALUES returns values of '
      write ( *, '(a)' ) '  the Jacobi elliptic CN function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A         X       CN(A,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jacobi_cn_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,f10.4,2x,g24.16)' ) a, x, fx

      go to 10

20    continue

      return
      end
      subroutine jacobi_dn_values_test ( )

c*********************************************************************72
c
cc JACOBI_DN_VALUES_TEST tests JACOBI_DN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_DN_VALUES_TEST:'
      write ( *, '(a)' ) '  JACOBI_DN_VALUES returns values of '
      write ( *, '(a)' ) '  the Jacobi elliptic DN function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A         X       DN(A,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jacobi_dn_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,f10.4,2x,g24.16)' ) a, x, fx

      go to 10

20    continue

      return
      end
      subroutine jacobi_poly_values_test ( )

c*********************************************************************72
c
cc JACOBI_POLY_VALUES_TEST tests JACOBI_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  JACOBI_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Jacobi polynomial.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N       A       B      X       J(N,A,B)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jacobi_poly_values ( n_data, n, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i6,2x,f8.4,2x,f8.4,2x,f24.16,2x,g24.16)' ) 
     &    n, a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine jacobi_sn_values_test ( )

c*********************************************************************72
c
cc JACOBI_SN_VALUES_TEST tests JACOBI_SN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JACOBI_SN_VALUES_TEST:'
      write ( *, '(a)' ) '  JACOBI_SN_VALUES returns values of '
      write ( *, '(a)' ) '  the Jacobi elliptic SN function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A         X       SN(A,X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jacobi_sn_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,f10.4,2x,g14.6)' ) a, x, fx

      go to 10

20    continue

      return
      end
      subroutine jed_ce_values_test ( )

c*********************************************************************72
c
cc JED_CE_VALUES_TEST tests JED_CE_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer d
      double precision f
      double precision jed
      integer n_data
      integer m
      integer y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JED_CE_VALUES_TEST:'
      write ( *, '(a)' ) '  JED_CE_VALUES returns:'
      write ( *, '(a)' ) '  JED, a Julian Ephemeris Date, and'
      write ( *, '(a)' ) 
     &  '  YMDF, the corresponding year, month, day, fraction.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        JED          Y   M   D    F'
      write ( *, '(a)' ) ' '

      n_data = 0
    
10    continue

        call jed_ce_values ( n_data, jed, y, m, d, f )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.2,2x,i6,2x,i2,2x,i2,2x,f6.4)' ) 
     &    jed, y, m, d, f

      go to 10

20    continue

      return
      end
      subroutine jed_mjd_values_test ( )

c*********************************************************************72
c
cc JED_MJD_VALUES_TEST tests JED_MJD_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision jed
      integer n_data
      double precision mjd

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JED_MJD_VALUES_TEST:'
      write ( *, '(a)' ) '  JED_MJD_VALUES returns:'
      write ( *, '(a)' ) '  JED, a Julian Ephemeris Date, and'
      write ( *, '(a)' ) 
     &  '  MJD, the corresponding Modified Julian Day count.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        JED           MJD'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jed_mjd_values ( n_data, jed, mjd )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.2,2x,f12.2)' ) jed, mjd

      go to 10

20    continue

      return
      end
      subroutine jed_rd_values_test ( )

c*********************************************************************72
c
cc JED_RD_VALUES_TEST tests JED_RD_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision jed
      integer n_data
      double precision rd

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JED_RD_VALUES_TEST:'
      write ( *, '(a)' ) '  JED_RD_VALUES returns:'
      write ( *, '(a)' ) '  JED, a Julian Ephemeris Date, and'
      write ( *, '(a)' ) '  RD, the corresponding '
      write ( *, '(a)' ) '  Reingold Dershowitz Day count.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        JED            RD'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jed_rd_values ( n_data, jed, rd )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.2,2x,f12.2)' ) jed, rd

      go to 10

20    continue

      return
      end
      subroutine jed_weekday_values_test ( )

c*********************************************************************72
c
cc JED_WEEKDAY_VALUES_TEST tests JED_WEEKDAY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision jed
      integer n_data
      integer weekday
      character*9 weekday_name(7)

      save weekday_name

      data weekday_name /
     &  'Sunday   ', 'Monday   ', 
     &  'Tuesday  ', 'Wednesday', 
     &  'Thursday ', 
     &  'Friday   ', 'Saturday ' /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JED_WEEKDAY_VALUES_TEST:'
      write ( *, '(a)' ) '  JED_WEEKDAY_VALUES returns '
      write ( *, '(a)' ) '  Julian Ephemeris Dates '
      write ( *, '(a)' ) '  (JED) and the corresponding weekday'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        JED     #  Weekday'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call jed_weekday_values ( n_data, jed, weekday )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.2,2x,i1,2x,a9)' ) 
     &    jed, weekday, weekday_name(weekday)

      go to 10

20    continue

      return
      end
      subroutine kei0_values_test ( )

c*********************************************************************72
c
cc KEI0_VALUES_TEST tests KEI0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KEI0_VALUES_TEST:'
      write ( *, '(a)' ) '  KEI0_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin KEI function of order 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call kei0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine kei1_values_test ( )

c*********************************************************************72
c
cc KEI1_VALUES_TEST tests KEI1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KEI1_VALUES_TEST:'
      write ( *, '(a)' ) '  KEI1_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin KEI function of order 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call kei1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine ker0_values_test ( )

c*********************************************************************72
c
cc KER0_VALUES_TEST tests KER0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KER0_VALUES_TEST:'
      write ( *, '(a)' ) '  KER0_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin KER function of order 0.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call ker0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine ker1_values_test ( )

c*********************************************************************72
c
cc KER1_VALUES_TEST tests KER1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KER1_VALUES_TEST:'
      write ( *, '(a)' ) '  KER1_VALUES returns values of '
      write ( *, '(a)' ) '  the Kelvin KER function of order 1.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          X           FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call ker1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f24.16,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine laguerre_associated_values_test ( )

c*********************************************************************72
c
cc LAGUERRE_ASSOCIATED_VALUES_TEST tests LAGUERRE_ASSOCIATED_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_ASSOCIATED_VALUES_TEST:'
      write ( *, '(a)' ) '  LAGUERRE_ASSOCIATED_VALUES returns values'
      write ( *, '(a)' ) '  of the associated Laguerre polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       M      X            L(N,M)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call laguerre_associated_values ( n_data, n, m, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g24.16)' ) n, m, x, fx

      go to 10

20    continue

      return
      end
      subroutine laguerre_general_values_test ( )

c*********************************************************************72
c
cc LAGUERRE_GENERAL_VALUES_TEST tests LAGUERRE_GENERAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_GENERAL_VALUES_TEST:'
      write ( *, '(a)' ) '  LAGUERRE_GENERAL_VALUES returns values'
      write ( *, '(a)' ) '  of the generalized Laguerre function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       A      X            L(N,A)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call laguerre_general_values ( n_data, n, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,f14.6,2x,g24.16)' ) n, a, x, fx

      go to 10

20    continue

      return
      end
      subroutine laguerre_polynomial_values_test ( )

c*********************************************************************72
c
cc LAGUERRE_POLYNOMIAL_VALUES_TEST tests LAGUERRE_POLYNOMIAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_POLYNOMIAL_VALUES_TEST:'
      write ( *, '(a)' ) '  LAGUERRE_POLYNOMIAL_VALUES returns values'
      write ( *, '(a)' ) '  of the Laguerre polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N     X            L(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call laguerre_polynomial_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine lambert_w_values_test ( )

c*********************************************************************72
c
cc LAMBERT_W_VALUES_TEST tests LAMBERT_W_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAMBERT_W_VALUES_TEST:'
      write ( *, '(a)' ) '  LAMBERT_W_VALUES returns values of '
      write ( *, '(a)' ) '  the Lambert W function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X               W(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lambert_w_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine laplace_cdf_values_test ( )

c*********************************************************************72
c
cc LAPLACE_CDF_VALUES_TEST tests LAPLACE_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision beta
      double precision fx
      double precision mu
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAPLACE_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  LAPLACE_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Laplace CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     MU     BETA      X            CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call laplace_cdf_values ( n_data, mu, beta, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6,2x,g24.16)' ) 
     &    mu, beta, x, fx

      go to 10

20    continue

      return
      end
      subroutine legendre_associated_values_test ( )

c*********************************************************************72
c
cc LEGENDRE_ASSOCIATED_VALUES_TEST tests LEGENDRE_ASSOCIATED_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_ASSOCIATED_VALUES_TEST:'
      write ( *, '(a)' ) '  LEGENDRE_ASSOCIATED_VALUES returns values'
      write ( *, '(a)' ) '  of the associated Legendre polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       M    X             P(N,M)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_associated_values ( n_data, n, m, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g24.16)' ) n, m, x, fx

      go to 10

20    continue

      return
      end
      subroutine legendre_associated_normalized_values_test ( )

c*********************************************************************72
c
cc LEGENDRE_ASSOCIATED_NORMALIED_VALUES_TEST tests LEGENDRE_ASSOCIATED_NORMALIZED_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_ASSOCIATED_NORMALIZED_VALUES_TEST:'
      write ( *, '(a)' ) '  LEGENDRE_ASSOCIATED_NORMALIZED_VALUES'
      write ( *, '(a)' ) '  returns values of the normalized associated'
      write ( *, '(a)' ) '  Legendre polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       M    X             P(N,M)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_associated_normalized_values ( n_data, n, m, x, 
     &    fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g24.16)' ) n, m, x, fx

      go to 10

20    continue

      return
      end
      subroutine legendre_associated_normalized_sphere_values_test ( )

c*********************************************************************72
c
cc LEGENDRE_ASSOCIATED_NORMALIZED_SPHERE_VALUES_TEST tests LEGENDRE_ASSOCIATED_NORMALIZED_SPHERE_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer m
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  'LEGENDRE_ASSOCIATED_NORMALIZED_SPHERE_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  LEGENDRE_ASSOCIATED_NORMALIZED_SPHERE_VALUES'
      write ( *, '(a)' ) '  returns values of the associated Legendre'
      write ( *, '(a)' ) '  polynomials, normalized for a sphere'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       M    X             P(N,M)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_associated_normalized_sphere_values ( n_data, n, 
     &    m, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,f14.6,2x,g24.16)' ) n, m, x, fx

      go to 10

20    continue

      return
      end
      subroutine legendre_poly_values_test ( )

c*********************************************************************72
c
cc LEGENDRE_POLY_VALUES_TEST tests LEGENDRE_POLY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_POLY_VALUES_TEST:'
      write ( *, '(a)' ) '  LEGENDRE_POLY_VALUES returns values of '
      write ( *, '(a)' ) '  the Legendre PN polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    X             P(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_poly_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine legendre_function_q_values_test ( )

c*********************************************************************72
c
cc LEGENDRE_FUNCTION_Q_VALUES_TEST tests LEGENDRE_FUNCTION_Q_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LEGENDRE_FUNCTION_Q_VALUES_TEST:'
      write ( *, '(a)' ) '  LEGENDRE_FUNCTION_Q_VALUES returns values'
      write ( *, '(a)' ) '  of the Legendre QN polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    X             Q(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call legendre_function_q_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g14.6)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine lerch_values_test ( )

c*********************************************************************72
c
cc LERCH_VALUES_TEST tests LERCH_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      integer s
      double precision z

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LERCH_VALUES_TEST:'
      write ( *, '(a)' ) '  LERCH_VALUES returns values of '
      write ( *, '(a)' ) '  the Lerch transcendent function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     Z        S        A       CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lerch_values ( n_data, z, s, a, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,i8,2x,f10.4,2x,g24.16)' ) 
     &    z, s, a, fx

      go to 10

20    continue

      return
      end
      subroutine lobachevsky_values_test ( )

c*********************************************************************72
c
cc LOBACHEVSKY_VALUES_TEST tests LOBACHEVSKY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOBACHEVSKY_VALUES_TEST:'
      write ( *, '(a)' ) '  LOBACHEVSKY_VALUES returns values of'
      write ( *, '(a)' ) '  the Lobachevsky function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lobachevsky_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine lobatto_polynomial_values_test ( )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_VALUES_TEST demonstrates LOBATTO_POLYNOMIAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_VALUES_TEST:'
      write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_VALUES returns values'
      write ( *, '(a)' ) '  of the completed Lobatto polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    X            Lo(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lobatto_polynomial_values ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine lobatto_polynomial_derivatives_test ( )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_DERIVATIVES_TEST tests LOBATTO_POLYNOMIAL_DERIVATIVES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOBATTO_POLYNOMIAL_DERIVATIVES_TEST:'
      write ( *, '(a)' ) '  LOBATTO_POLYNOMIAL_DERIVATIVES returns'
      write ( *, '(a)' ) 
     &  '  derivatives of completed Lobatto polynomials.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    X            Lo''(N)(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call lobatto_polynomial_derivatives ( n_data, n, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f14.6,2x,g24.16)' ) n, x, fx

      go to 10

20    continue

      return
      end
      subroutine log_values_test ( )

c*********************************************************************72
c
cc LOG_VALUES_TEST tests LOG_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 March 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOG_VALUES_TEST:'
      write ( *, '(a)' ) '  LOG_VALUES returns values of'
      write ( *, '(a)' ) '  the natural logarithm function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call log_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine log_normal_cdf_values_test ( )

c*********************************************************************72
c
cc LOG_NORMAL_CDF_VALUES_TEST tests LOG_NORMAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOG_NORMAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  LOG_NORMAL_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Log Normal Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      MU     SIGMA    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call log_normal_cdf_values ( n_data, mu, sigma, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, x, fx

      go to 10

20    continue

      return
      end
      subroutine log_series_cdf_values_test ( )

c*********************************************************************72
c
cc LOG_SERIES_CDF_VALUES_TEST tests LOG_SERIES_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOG_SERIES_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  LOG_SERIES_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Log Series Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     T          N   CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call log_series_cdf_values ( n_data, t, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,i8,2x,g24.16)' ) t, n, fx

      go to 10

20    continue

      return
      end
      subroutine log10_values_test ( )

c*********************************************************************72
c
cc LOG10_VALUES_TEST tests LOG10_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOG10_VALUES_TEST:'
      write ( *, '(a)' ) '  LOG10_VALUES returns values of'
      write ( *, '(a)' ) '  the base 10 logarithm function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call log10_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine logarithmic_integral_values_test ( )

c*********************************************************************72
c
cc LOGARITHMIC_INTEGRAL_VALUES_TEST tests LOGARITHMIC_INTEGRAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOGARITHMIC_INTEGRAL_VALUES_TEST:'
      write ( *, '(a)' ) '  LOGARITHMIC_INTEGAL_VALUES returns values'
      write ( *, '(a)' ) '  of the logarithmic integral function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            LI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call logarithmic_integral_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine logistic_cdf_values_test ( )

c*********************************************************************72
c
cc LOGISTIC_CDF_VALUES_TEST tests LOGISTIC_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision beta
      double precision fx
      double precision mu
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOGISTIC_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  LOGISTIC_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Logistic Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      MU     BETA     X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call logistic_cdf_values ( n_data, mu, beta, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    mu, beta, x, fx

      go to 10

20    continue

      return
      end
      subroutine mathieu_even_values_test ( )

c*********************************************************************72
c
cc MATHIEU_EVEN_VALUES_TEST tests MATHIEU_EVEN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      integer n_data
      integer q
      integer r

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATHIEU_EVEN_VALUES_TEST:'
      write ( *, '(a)' ) '  MATHIEU_EVEN_VALUES returns values of the'
      write ( *, '(a)' ) '  eigenvalues of Mathieu''s differential'
      write ( *, '(a)' ) '  equation associated with even periodic'
      write ( *, '(a)' ) '  solutions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       R       Q            A(R,Q)'
      write ( *, '(a)' ) ' '

      n_data = 0

 10   continue

      call mathieu_even_values ( n_data, r, q, a )

      if ( n_data .eq. 0 ) then
        go to 20
      end if

      write ( *, '(2x,i6,2x,i6,2x,g24.16)' ) r, q, a

      go to 10

20    continue

      return
      end
      subroutine mathieu_odd_values_test ( )

c*********************************************************************72
c
cc MATHIEU_ODD_VALUES_TEST tests MATHIEU_ODD_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      integer n_data
      integer q
      integer r

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATHIEU_ODD_VALUES_TEST:'
      write ( *, '(a)' ) '  MATHIEU_ODD_VALUES returns values of the'
      write ( *, '(a)' ) '  eigenvalues of Mathieu''s differential'
      write ( *, '(a)' ) '  equation associated with odd periodic'
      write ( *, '(a)' ) '  solutions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       R       Q            B(R,Q)'
      write ( *, '(a)' ) ' '

      n_data = 0

 10   continue

      call mathieu_odd_values ( n_data, r, q, b )

      if ( n_data .eq. 0 ) then
        go to 20
      end if

      write ( *, '(2x,i6,2x,i6,2x,g24.16)' ) r, q, b

      go to 10

20    continue

      return
      end
      subroutine moebius_values_test ( )

c*********************************************************************72
c
cc MOEBIUS_VALUES_TEST tests MOEBIUS_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MOEBIUS_VALUES_TEST:'
      write ( *, '(a)' ) '  MOEBIUS_VALUES returns values of '
      write ( *, '(a)' ) '  the Moebius function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '       N         MU(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call moebius_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine negative_binomial_cdf_values_test ( )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_CDF_VALUES_TEST tests NEGATIVE_BINOMIAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cdf
      integer f
      integer n_data
      double precision p
      integer s

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  NEGATIVE_BINOMIAL_CDF_VALUES returns'
      write ( *, '(a)' ) '  values of the Negative Binomial '
      write ( *, '(a)' ) '  Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       F       S         P         CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call negative_binomial_cdf_values ( n_data, f, s, p, cdf )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,g14.6,2x,g14.6)' ) f, s, p, cdf

      go to 10

20    continue

      return
      end
      subroutine nine_j_values_test ( )

c*********************************************************************72
c
cc NINE_J_VALUES_TEST tests NINE_J_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision j1
      double precision j2
      double precision j3
      double precision j4
      double precision j5
      double precision j6
      double precision j7
      double precision j8
      double precision j9
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NINE_J_VALUES_TEST:'
      write ( *, '(a)' ) '  NINE_J_VALUES returns values of '
      write ( *, '(a)' ) '  the Wigner 9J coefficient.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '      J1      J2      J3      J4      J5      J6',
     &  '      J7      J8      J9        NINE_J'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call nine_j_values ( n_data, j1, j2, j3, j4, j5, j6, 
     &    j7, j8, j9, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &  '(2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,
     &  f6.2,2x,f6.2,2x,f6.2,2x,g24.16)' ) 
     &  j1, j2, j3, j4, j5, j6, j7, j8, j9, fx

      go to 10

20    continue

      return
      end
      subroutine normal_cdf_values_test ( )

c*********************************************************************72
c
cc NORMAL_CDF_VALUES_TEST tests NORMAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  NORMAL_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Normal Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      MU     SIGMA    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call normal_cdf_values ( n_data, mu, sigma, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, x, fx

      go to 10

20    continue

      return
      end
      subroutine normal_01_cdf_values_test ( )

c*********************************************************************72
c
cc NORMAL_01_CDF_VALUES_TEST tests NORMAL_01_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  NORMAL_01_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Normal Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call normal_01_cdf_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine omega_values_test ( )

c*********************************************************************72
c
cc OMEGA_VALUES_TEST tests OMEGA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'OMEGA_VALUES_TEST:'
      write ( *, '(a)' ) '  OMEGA_VALUES returns values of '
      write ( *, '(a)' ) '  the Omega function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     N           OMEGA(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call omega_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i12,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine owen_values_test ( )

c*********************************************************************72
c
cc OWEN_VALUES_TEST tests OWEN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision h
      integer n_data
      double precision t

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'OWEN_VALUES_TEST:'
      write ( *, '(a)' ) '  OWEN_VALUES returns values of '
      write ( *, '(a)' ) '  the Owen T function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      H       A       T'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call owen_values ( n_data, h, a, t )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g24.16)' ) h, a, t

      go to 10

20    continue

      return
      end
      subroutine partition_count_values_test ( )

c*********************************************************************72
c
cc PARTITION_COUNT_VALUES_TEST tests PARTITION_COUNT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PARTITION_COUNT_VALUES_TEST:'
      write ( *, '(a)' ) '  PARTITION_COUNT_VALUES returns values of '
      write ( *, '(a)' ) '  the integer partition count function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         P(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call partition_count_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine partition_distinct_count_values_test ( )

c*********************************************************************72
c
cc PARTITION_DISTINCT_COUNT_VALUES_TEST tests PARTITION_DISTINCT_COUNT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PARTITION_DISTINCT_COUNT_VALUES_TEST:'
      write ( *, '(a)' ) '  PARTITION_DISTINCT_COUNT_VALUES returns '
      write ( *, '(a)' ) '  values of the integer distinct partition'
      write ( *, '(a)' ) '  count function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         Q(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call partition_distinct_count_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine phi_values_test ( )

c*********************************************************************72
c
cc PHI_VALUES_TEST tests PHI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PHI_VALUES_TEST:'
      write ( *, '(a)' ) '  PHI_VALUES returns values of '
      write ( *, '(a)' ) '  the PHI function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         PHI(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call phi_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine pi_values_test ( )

c*********************************************************************72
c
cc PI_VALUES_TEST tests PI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PI_VALUES_TEST:'
      write ( *, '(a)' ) '  PI_VALUES returns values of '
      write ( *, '(a)' ) '  the PI function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '             N         PI(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call pi_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i12,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine poisson_cdf_values_test ( )

c*********************************************************************72
c
cc POISSON_CDF_VALUES_TEST tests POISSON_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      integer n_data
      integer x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POISSON_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  POISSON_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Poisson Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      A          X    CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call poisson_cdf_values ( n_data, a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,i8,2x,g24.16)' ) a, x, fx

      go to 10

20    continue

      return
      end
      subroutine polylogarithm_values_test ( )

c*********************************************************************72
c
cc POLYLOGARITHM_VALUES_TEST tests POLYLOGARITHM_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n
      integer n_data
      double precision z

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYLOGARITHM_VALUES_TEST:'
      write ( *, '(a)' ) '  POLYLOGARITHM_VALUES returns values of '
      write ( *, '(a)' ) '  the polylogarithm.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N      Z        FX'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call polylogarithm_values ( n_data, n, z, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,f10.4,2x,g24.16)' ) n, z, fx

      go to 10

20    continue

      return
      end
      subroutine prandtl_values_test ( )

c*********************************************************************72
c
cc PRANDTL_VALUES_TEST tests PRANDTL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      double precision p
      double precision pr
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PRANDTL_VALUES_TEST:'
      write ( *, '(a)' ) '  PRANDTL_VALUES returns values of '
      write ( *, '(a)' ) '  the Prandtl number of water '
      write ( *, '(a)' ) '  as a function of temperature and pressure.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            P            Pr(T,P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call prandtl_values ( n_data, tc, p, pr )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g14.6)' ) tc, p, pr

      go to 10

20    continue

      return
      end
      subroutine prime_values_test ( )

c*********************************************************************72
c
cc PRIME_VALUES_TEST tests PRIME_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer n_data
      integer p

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PRIME_VALUES_TEST:'
      write ( *, '(a)' ) '  PRIME_VALUES returns values of '
      write ( *, '(a)' ) '  the prime function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '           N          P[N]'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call prime_values ( n_data, n, p )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i12,2x,i12)' ) n, p

      go to 10

20    continue

      return
      end
      subroutine psat_values_test ( )

c*********************************************************************72
c
cc PSAT_VALUES_TEST tests PSAT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      double precision psat
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PSAT_VALUES_TEST:'
      write ( *, '(a)' ) '  PSAT_VALUES returns values of '
      write ( *, '(a)' ) '  the saturation pressure of water '
      write ( *, '(a)' ) '  as a function of temperature.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            PSAT(T)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call psat_values ( n_data, tc, psat )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) tc, psat

      go to 10

20    continue

      return
      end
      subroutine psi_values_test ( )

c*********************************************************************72
c
cc PSI_VALUES_TEST tests PSI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PSI_VALUES_TEST:'
      write ( *, '(a)' ) '  PSI_VALUES returns values of '
      write ( *, '(a)' ) '  the Psi function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X               PSI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call psi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine r8_factorial_values_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL_VALUES_TEST tests R8_FACTORIAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL_VALUES_TEST:'
      write ( *, '(a)' ) '  R8_FACTORIAL_VALUES returns values of '
      write ( *, '(a)' ) '  the factorial function '
      write ( *, '(a)' ) '  (using real arithmetic).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        N       Factorial(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g24.16)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine r8_factorial_log_values_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL_LOG_VALUES_TEST tests R8_FACTORIAL_LOG_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL_LOG_VALUES_TEST:'
      write ( *, '(a)' ) '  R8_FACTORIAL_LOG_VALUES returns values of '
      write ( *, '(a)' ) '  the logarithm of the factorial function '
      write ( *, '(a)' ) '  (using real arithmetic).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '        N       Log(Factorial(N))'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial_log_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g24.16)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine r8_factorial2_values_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL2_VALUES_TEST tests R8_FACTORIAL2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL2_VALUES_TEST:'
      write ( *, '(a)' ) '  R8_FACTORIAL2_VALUES returns values of '
      write ( *, '(a)' ) '  the double factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         F'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial2_values ( n_data, n, f )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g14.6)' ) n, f

      go to 10

20    continue

      return
      end
      subroutine r8_fall_values_test ( )

c*********************************************************************72
c
cc R8_FALL_VALUES_TEST tests R8_FALL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FALL_VALUES_TEST:'
      write ( *, '(a)' ) '  R8_FALL_VALUES returns some exact'
      write ( *, '(a)' ) '  values of the rising factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X       N      R8_FALL(X,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_fall_values ( n_data, x, n, f )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f8.4,2x,i8,2x,f12.4)' ) x, n, f

      go to 10

20    continue

      return
      end
      subroutine r8_rise_values_test ( )

c*********************************************************************72
c
cc R8_RISE_VALUES_TEST tests R8_RISE_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f
      integer n
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_RISE_VALUES_TEST:'
      write ( *, '(a)' ) '  R8_RISE_VALUES returns some exact'
      write ( *, '(a)' ) '  values of the rising factorial function:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X       N      R8_RISE(X,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_rise_values ( n_data, x, n, f )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f8.4,2x,i8,2x,f12.4)' ) x, n, f

      go to 10

20    continue

      return
      end
      subroutine rayleigh_cdf_values_test ( )

c*********************************************************************72
c
cc RAYLEIGH_CDF_VALUES_TEST tests RAYLEIGH_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RAYLEIGH_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  RAYLEIGH_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Rayleigh CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '      SIGMA             X',
     &  '                     CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call rayleigh_cdf_values ( n_data, sigma, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f12.8,2x,f24.16,2x,g24.16)' ) 
     &    sigma, x, fx

      go to 10

20    continue

      return
      end
      subroutine secvir_values_test ( )

c*********************************************************************72
c
cc SECVIR_VALUES_TEST tests SECVIR_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      double precision tc
      double precision vir

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SECVIR_VALUES_TEST:'
      write ( *, '(a)' ) '  SECVIR_VALUES returns values of '
      write ( *, '(a)' ) '  the second virial coefficient of water '
      write ( *, '(a)' ) '  as a function of temperature.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            VIR(T)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call secvir_values ( n_data, tc, vir )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) tc, vir

      go to 10

20    continue

      return
      end
      subroutine shi_values_test ( )

c*********************************************************************72
c
cc SHI_VALUES_TEST tests SHI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SHI_VALUES_TEST:'
      write ( *, '(a)' ) '  SHI_VALUES returns values of '
      write ( *, '(a)' ) '  the hyperbolic sine integral function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            SHI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call shi_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine si_values_test ( )

c*********************************************************************72
c
cc SI_VALUES_TEST tests SI_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SI_VALUES_TEST:'
      write ( *, '(a)' ) '  SI_VALUES returns values of '
      write ( *, '(a)' ) '  the sine integral function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            SI(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call si_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine sigma_values_test ( )

c*********************************************************************72
c
cc SIGMA_VALUES_TEST tests SIGMA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIGMA_VALUES_TEST:'
      write ( *, '(a)' ) '  SIGMA_VALUES returns values of '
      write ( *, '(a)' ) '  the SIGMA function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         SIGMA(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sigma_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine sin_values_test ( )

c*********************************************************************72
c
cc SIN_VALUES_TEST tests SIN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIN_VALUES_TEST:'
      write ( *, '(a)' ) '  SIN_VALUES returns values of '
      write ( *, '(a)' ) '  the sine function SIN(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            SIN(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sin_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine sin_degree_values_test ( )

c*********************************************************************72
c
cc SIN_DEGREE_VALUES_TEST tests SIN_DEGREE_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIN_DEGREE_VALUES_TEST:'
      write ( *, '(a)' ) '  SIN_DEGREE_VALUES returns values of '
      write ( *, '(a)' ) '  the sine function SIN(X).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            SIN(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sin_degree_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine sin_power_int_values_test ( )

c*********************************************************************72
c
cc SIN_POWER_INT_VALUES_TEST tests SIN_POWER_INT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIN_POWER_INT_VALUES_TEST:'
      write ( *, '(a)' ) '  SIN_POWER_INT returns values of '
      write ( *, '(a)' ) '  the integral of SIN(X)^N from A to B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '          A               B              N',
     &  '    SIN_POWER_INT(A,B,N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sin_power_int_values ( n_data, a, b, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,i8,2x,g14.6)' ) a, b, n, fx

      go to 10

20    continue

      return
      end
      subroutine six_j_values_test ( )

c*********************************************************************72
c
cc SIX_J_VALUES_TEST tests SIX_J_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision j1
      double precision j2
      double precision j3
      double precision j4
      double precision j5
      double precision j6
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SIX_J_VALUES_TEST:'
      write ( *, '(a)' ) '  SIX_J_VALUES returns values of '
      write ( *, '(a)' ) '  the Wigner 6J coefficient.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      J1      J2      J3      J4      J5      J6        SIX_J'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call six_j_values ( n_data, j1, j2, j3, j4, j5, j6, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &  '(2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,g24.16)' ) 
     &  j1, j2, j3, j4, j5, j6, fx

      go to 10

20    continue

      return
      end
      subroutine sound_values_test ( )

c*********************************************************************72
c
cc SOUND_VALUES_TEST tests SOUND_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c
      integer n_data
      double precision p
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SOUND_VALUES_TEST:'
      write ( *, '(a)' ) '  SOUND_VALUES returns values of '
      write ( *, '(a)' ) '  the spead of sound in water '
      write ( *, '(a)' ) '  as a function of temperature and pressure.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            P            C(T,P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sound_values ( n_data, tc, p, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g14.6)' ) tc, p, c

      go to 10

20    continue

      return
      end
      subroutine sphere_unit_area_values_test ( )

c*********************************************************************72
c
cc SPHERE_UNIT_AREA_VALUES_TEST tests SPHERE_UNIT_AREA_VALUES.
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

      double precision fx
      integer n_data
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_UNIT_AREA_VALUES_TEST:'
      write ( *, '(a)' ) '  SPHERE_UNIT_AREA_VALUES returns values of '
      write ( *, '(a)' ) '  the area of the unit sphere'
      write ( *, '(a)' ) '  in various dimensions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      N            Area'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sphere_unit_area_values ( n_data, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g24.16)' ) n, fx

      go to 10

20    continue

      return
      end
      subroutine sphere_unit_volume_values_test ( )

c*********************************************************************72
c
cc SPHERE_UNIT_VOLUME_VALUES_TEST tests SPHERE_UNIT_VOLUME_VALUES.
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

      double precision fx
      integer n_data
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_UNIT_VOLUME_VALUES_TEST:'
      write ( *, '(a)' ) '  SPHERE_UNIT_VOLUME_VALUES returns values'
      write ( *, '(a)' ) '  of the volume of the unit sphere '
      write ( *, '(a)' ) '  in various dimensions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      N            Volume'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sphere_unit_volume_values ( n_data, n, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i4,2x,g24.16)' ) n, fx

      go to 10

20    continue

      return
      end
      subroutine spherical_harmonic_values_test ( )

c*********************************************************************72
c
cc SPHERICAL_HARMONIC_VALUES_TEST tests SPHERICAL_HARMONIC_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l
      integer m
      integer n_data
      double precision phi
      double precision theta
      double precision yi
      double precision yr

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERICAL_HARMONIC_VALUES_TEST:'
      write ( *, '(a)' ) '  SPHERICAL_HARMONIC_VALUES returns values'
      write ( *, '(a)' ) '  of the spherical harmonic functions.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '   L   M    THETA       PHI       Yr',
     &  '                           Yi'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call spherical_harmonic_values ( n_data, l, m, theta, 
     &    phi, yr, yi )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,i2,2x,i2,2x,f8.4,2x,f8.4,2x,g24.16,2x,g24.16)' ) 
     &    l, m, theta, phi, yr, yi

      go to 10

20    continue

      return
      end
      subroutine sqrt_values_test ( )

c*********************************************************************72
c
cc SQRT_VALUES_TEST tests SQRT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SQRT_VALUES_TEST:'
      write ( *, '(a)' ) '  SQRT_VALUES returns some exact values.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         X      SQRT(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call sqrt_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if


        write ( *, '(2x,f14.4,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine stirling1_values_test ( )

c*********************************************************************72
c
cc STIRLING1_VALUES_TEST tests STIRLING1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer n_data
      integer m
      integer s1

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STIRLING1_VALUES_TEST:'
      write ( *, '(a)' ) '  STIRLING1_VALUES returns values of '
      write ( *, '(a)' ) '  the Stirling numbers of the first kind.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N         M        S1(N,M)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call stirling1_values ( n_data, n, m, s1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i12)' ) n, m, s1

      go to 10

20    continue

      return
      end
      subroutine stirling2_values_test ( )

c*********************************************************************72
c
cc STIRLING2_VALUES_TEST tests STIRLING2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer n_data
      integer m
      integer s2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STIRLING2_VALUES_TEST:'
      write ( *, '(a)' ) '  STIRLING2_VALUES returns values of '
      write ( *, '(a)' ) '  the Stirling numbers of the first kind.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N       M        S2(N,M)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call stirling2_values ( n_data, n, m, s2 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,i12)' ) n, m, s2

      go to 10

20    continue

      return
      end
      subroutine stromgen_values_test ( )

c*********************************************************************72
c
cc STROMGEN_VALUES_TEST tests STROMGEN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STROMGEN_VALUES_TEST:'
      write ( *, '(a)' ) '  STROMGEN_VALUES returns values of'
      write ( *, '(a)' ) '  the Stromgen function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X          F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call stromgen_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine struve_h0_values_test ( )

c*********************************************************************72
c
cc STRUVE_H0_VALUES_TEST tests STRUVE_H0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STRUVE_H0_VALUES_TEST:'
      write ( *, '(a)' ) '  STRUVE_H0_VALUES returns values of '
      write ( *, '(a)' ) '  the Struve H0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            H0(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call struve_h0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine struve_h1_values_test ( )

c*********************************************************************72
c
cc STRUVE_H1_VALUES_TEST tests STRUVE_H1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STRUVE_H1_VALUES_TEST:'
      write ( *, '(a)' ) '  STRUVE_H1_VALUES returns values of '
      write ( *, '(a)' ) '  the Struve H1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            H1(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call struve_h1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine struve_l0_values_test ( )

c*********************************************************************72
c
cc STRUVE_L0_VALUES_TEST tests STRUVE_L0_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STRUVE_L0_VALUES_TEST:'
      write ( *, '(a)' ) '  STRUVE_L0_VALUES returns values of '
      write ( *, '(a)' ) '  the Struve L0 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            L0(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call struve_l0_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine struve_l1_values_test ( )

c*********************************************************************72
c
cc STRUVE_L1_VALUES_TEST tests STRUVE_L1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STRUVE_L1_VALUES_TEST:'
      write ( *, '(a)' ) '  STRUVE_L1_VALUES returns values of '
      write ( *, '(a)' ) '  the Struve L1 function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            L1(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call struve_l1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine student_cdf_values_test ( )

c*********************************************************************72
c
cc STUDENT_CDF_VALUES_TEST tests STUDENT_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2006
c
      implicit none

      double precision c
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STUDENT_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  STUDENT_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Student T Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      C     X       CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

 10   continue

      call student_cdf_values ( n_data, c, x, fx )

      if ( n_data .eq. 0 ) then
        go to 20
      end if

      write ( *, '(2x,f10.4,2x,f10.4,2x,g24.16)' ) c, x, fx

      go to 10

20    continue

      return
      end
      subroutine student_noncentral_cdf_values_test ( )

c*********************************************************************72
c
cc STUDENT_NONCENTRAL_CDF_VALUES_TEST tests STUDENT_NONCENTRAL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer df
      double precision fx
      double precision lambda
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STUDENT_NONCENTRAL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  STUDENT_NONCENTRAL_CDF_VALUES returns'
      write ( *, '(a)' ) '  values of the noncentral Student T '
      write ( *, '(a)' ) '  Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X      LAMBDA       DF     CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call student_noncentral_cdf_values ( n_data, df, lambda, 
     &    x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f10.4,2x,f10.4,2x,i8,2x,g14.6)' ) 
     &    x, lambda, df, fx

      go to 10

20    continue

      return
      end
      subroutine subfactorial_values_test ( )

c*********************************************************************72
c
cc SUBFACTORIAL_VALUES_TEST tests SUBFACTORIAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SUBFACTORIAL_VALUES_TEST:'
      write ( *, '(a)' ) '  SUBFACTORIAL_VALUES returns values of '
      write ( *, '(a)' ) '  the subfactorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N     Subfactorial[N]'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call subfactorial_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine surten_values_test ( )

c*********************************************************************72
c
cc SURTEN_VALUES_TEST tests SURTEN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      double precision sigma
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SURTEN_VALUES_TEST:'
      write ( *, '(a)' ) '  SURTEN_VALUES returns values of '
      write ( *, '(a)' ) '  the surface tension of water '
      write ( *, '(a)' ) '  as a function of temperature.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            SIGMA(T)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call surten_values ( n_data, tc, sigma )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) tc, sigma

      go to 10

20    continue

      return
      end
      subroutine synch1_values_test ( )

c*********************************************************************72
c
cc SYNCH1_VALUES_TEST tests SYNCH1_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SYNCH1_VALUES_TEST:'
      write ( *, '(a)' ) '  SYNCH1_VALUES returns values of '
      write ( *, '(a)' ) '  the synchrotron radiation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            S1(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call synch1_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine synch2_values_test ( )

c*********************************************************************72
c
cc SYNCH2_VALUES_TEST tests SYNCH2_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SYNCH2_VALUES_TEST:'
      write ( *, '(a)' ) '  SYNCH2_VALUES returns values of '
      write ( *, '(a)' ) '  the synchrotron radiation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            S2(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call synch2_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tau_values_test ( )

c*********************************************************************72
c
cc TAU_VALUES_TEST tests TAU_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer fn
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TAU_VALUES_TEST:'
      write ( *, '(a)' ) '  TAU_VALUES returns values of '
      write ( *, '(a)' ) '  the TAU function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N         TAU(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tau_values ( n_data, n, fn )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i12)' ) n, fn

      go to 10

20    continue

      return
      end
      subroutine thercon_values_test ( )

c*********************************************************************72
c
cc THERCON_VALUES_TEST tests THERCON_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision lambda
      integer n_data
      double precision p
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'THERCON_VALUES_TEST:'
      write ( *, '(a)' ) '  THERCON_VALUES returns values of '
      write ( *, '(a)' ) '  the thermal conductivity of water '
      write ( *, '(a)' ) '  as a function of temperature and pressure.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            P            LAMBDA(T,P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call thercon_values ( n_data, tc, p, lambda )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g14.6)' ) tc, p, lambda

      go to 10

20    continue

      return
      end
      subroutine three_j_values_test ( )

c*********************************************************************72
c
cc THREE_J_VALUES_TEST tests THREE_J_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      double precision j1
      double precision j2
      double precision j3
      double precision m1
      double precision m2
      double precision m3
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'THREE_J_VALUES_TEST:'
      write ( *, '(a)' ) '  THREE_J_VALUES returns values of '
      write ( *, '(a)' ) '  the Wigner 3J coefficient.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) 
     &  '      J1      J2      J3      ',
     &  'M1      M2      M3        THREE_J'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call three_j_values ( n_data, j1, j2, j3, m1, m2, m3, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &  '(2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,g24.16)' ) 
     &    j1, j2, j3, m1, m2, m3, fx

      go to 10

20    continue

      return
      end
      subroutine tran02_values_test ( )

c*********************************************************************72
c
cc TRAN02_VALUES_TEST tests TRAN02_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN02_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN02_VALUES returns values of '
      write ( *, '(a)' ) '  the order 2 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T2(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran02_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran03_values_test ( )

c*********************************************************************72
c
cc TRAN03_VALUES_TEST tests TRAN03_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN03_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN03_VALUES returns values of '
      write ( *, '(a)' ) '  the order 3 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T3(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran03_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran04_values_test ( )

c*********************************************************************72
c
cc TRAN04_VALUES_TEST tests TRAN04_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN04_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN04_VALUES returns values of '
      write ( *, '(a)' ) '  the order 4 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T4(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran04_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran05_values_test ( )

c*********************************************************************72
c
cc TRAN05_VALUES_TEST tests TRAN05_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN05_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN05_VALUES returns values of '
      write ( *, '(a)' ) '  the order 5 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T5(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran05_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran06_values_test ( )

c*********************************************************************72
c
cc TRAN06_VALUES_TEST tests TRAN06_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN06_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN06_VALUES returns values of '
      write ( *, '(a)' ) '  the order 6 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T6(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran06_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran07_values_test ( )

c*********************************************************************72
c
cc TRAN07_VALUES_TEST tests TRAN07_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN07_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN07_VALUES returns values of '
      write ( *, '(a)' ) '  the order 7 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T7(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran07_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran08_values_test ( )

c*********************************************************************72
c
cc TRAN08_VALUES_TEST tests TRAN08_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN08_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN08_VALUES returns values of '
      write ( *, '(a)' ) '  the order 8 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T8(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran08_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine tran09_values_test ( )

c*********************************************************************72
c
cc TRAN09_VALUES_TEST tests TRAN09_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRAN09_VALUES_TEST:'
      write ( *, '(a)' ) '  TRAN09_VALUES returns values of '
      write ( *, '(a)' ) '  the order 9 transportation function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            T9(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tran09_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine trigamma_values_test ( )

c*********************************************************************72
c
cc TRIGAMMA_VALUES_TEST tests TRIGAMMA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIGAMMA_VALUES_TEST:'
      write ( *, '(a)' ) '  TRIGAMMA_VALUES returns values of '
      write ( *, '(a)' ) '  the TriGamma function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      X            F(X)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call trigamma_values ( n_data, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g24.16)' ) x, fx

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_ab_cdf_values_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_CDF_VALUES_TEST tests TRUNCATED_NORMAL_AB_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_CDF_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_AB_CDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the CDF of the Truncated Normal AB Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU     SIGMA    A    B   X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_ab_cdf_values ( n_data, mu, sigma, 
     &    a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_ab_pdf_values_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_PDF_VALUES_TEST tests TRUNCATED_NORMAL_AB_PDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_PDF_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_AB_PDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the PDF of the Truncated Normal AB Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU     SIGMA    A    B   X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_ab_pdf_values ( n_data, mu, sigma, 
     &    a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_a_cdf_values_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_CDF_VALUES_TEST tests TRUNCATED_NORMAL_A_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_CDF_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_A_CDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the CDF of the lower Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU     SIGMA    A    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_a_cdf_values ( n_data, mu, sigma, 
     &    a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, a, x, fx

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_a_pdf_values_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_PDF_VALUES_TEST tests TRUNCATED_NORMAL_A_PDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_PDF_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_A_PDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the PDF of the lower Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU     SIGMA    A    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_a_pdf_values ( n_data, mu, sigma, 
     &    a, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, a, x, fx

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_b_cdf_values_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_CDF_VALUES_TEST tests TRUNCATED_NORMAL_B_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_CDF_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_B_CDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the CDF of the upper Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU     SIGMA    B   X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_b_cdf_values ( n_data, mu, sigma, 
     &    b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_b_pdf_values_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_PDF_VALUES_TEST tests TRUNCATED_NORMAL_B_PDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision fx
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_PDF_VALUES_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_B_PDF_VALUES returns values of '
      write ( *, '(a)' ) 
     &  '  the PDF of the upper Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU     SIGMA    B   X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_b_pdf_values ( n_data, mu, sigma, 
     &    b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,g14.6,2x,g24.16)' ) 
     &    mu, sigma, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine tsat_values_test ( )

c*********************************************************************72
c
cc TSAT_VALUES_TEST tests TSAT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_data
      double precision p
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TSAT_VALUES_TEST:'
      write ( *, '(a)' ) '  TSAT_VALUES returns values of '
      write ( *, '(a)' ) '  the saturation temperature '
      write ( *, '(a)' ) '  as a function of pressure.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      P           Tsat(P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call tsat_values ( n_data, p, tc )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,g14.6)' ) p, tc

      go to 10

20    continue

      return
      end
      subroutine van_der_corput_values_test ( )

c*********************************************************************72
c
cc VAN_DER_CORPUT_VALUES_TEST tests VAN_DER_CORPUT_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer base
      integer n_data
      integer seed
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'VAN_DER_CORPUT_VALUES_TEST:'
      write ( *, '(a)' ) '  VAN_DER_CORPUT_VALUES returns values of '
      write ( *, '(a)' ) '  the van der Corput sequence '
      write ( *, '(a)' ) '  in a given base.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      BASE      SEED    VDC(BASE,SEED)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call van_der_corput_values ( n_data, base, seed, value )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,i8,2x,g16.8)' ) base, seed, value

      go to 10

20    continue

      return
      end
      subroutine viscosity_values_test ( )

c*********************************************************************72
c
cc VISCOSITY_VALUES_TEST tests VISCOSITY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision eta
      integer n_data
      double precision p
      double precision tc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'VISCOSITY_VALUES_TEST:'
      write ( *, '(a)' ) '  VISCOSITY_VALUES returns values of '
      write ( *, '(a)' ) '  the viscosity of water '
      write ( *, '(a)' ) '  as a function of temperature and pressure.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      T            P            ETA(T,P)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call viscosity_values ( n_data, tc, p, eta )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,g14.6)' ) tc, p, eta

      go to 10

20    continue

      return
      end
      subroutine von_mises_cdf_values_test ( )

c*********************************************************************72
c
cc VON_MISES_CDF_VALUES_TEST tests VON_MISES_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'VON_MISES_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  VON_MISES_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the von Mises CDF.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      A            B            X            CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call von_mises_cdf_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6,2x,g14.6)' ) 
     &    a, b, x, fx

      go to 10

20    continue

      return
      end
      subroutine weekday_values_test ( )

c*********************************************************************72
c
cc WEEKDAY_VALUES_TEST tests WEEKDAY_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer d
      integer m
      integer n_data
      integer w
      integer y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEEKDAY_VALUES_TEST:'
      write ( *, '(a)' ) '  WEEKDAY_VALUES returns values of '
      write ( *, '(a)' ) '  the weekday for a given Y/M/D date.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     Y     M     D     W'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call weekday_values ( n_data, y, m, d, w )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i4,2x,i4,2x,i4,2x,i4)' ) y, m, d, w

      go to 10

20    continue

      return
      end
      subroutine weibull_cdf_values_test ( )

c*********************************************************************72
c
cc WEIBULL_CDF_VALUES_TEST tests WEIBULL_CDF_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha
      double precision beta
      double precision fx
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEIBULL_CDF_VALUES_TEST:'
      write ( *, '(a)' ) '  WEIBULL_CDF_VALUES returns values of '
      write ( *, '(a)' ) '  the Weibull Cumulative Density Function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      ALPHA   BETA    X                  CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call weibull_cdf_values ( n_data, alpha, beta, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g24.16)' ) 
     &    alpha, beta, x, fx

      go to 10

20    continue

      return
      end
      subroutine zeta_values_test ( )

c*********************************************************************72
c
cc ZETA_VALUES_TEST tests ZETA_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer n_data
      double precision zeta

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ZETA_VALUES_TEST:'
      write ( *, '(a)' ) '  ZETA_VALUES returns values of '
      write ( *, '(a)' ) '  the Riemann Zeta function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N        ZETA(N)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call zeta_values ( n_data, n, zeta )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        write ( *, '(2x,i8,2x,g24.16)' ) n, zeta

      go to 10

20    continue

      return
      end

