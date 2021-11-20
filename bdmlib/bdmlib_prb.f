      program main

c*****************************************************************************80
c
cc MAIN is the main program for BDMLIB_PRB.
c
c  Discussion:
c
c    BDMLIB_PRB tests the BDMLIB library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer acid_num
      parameter ( acid_num = 20 )
      integer comp_max
      parameter ( comp_max = 9 )
      integer iunit
      parameter ( iunit = 1 )

      character acid_sym(acid_num)
      double precision beta(acid_num,comp_max)
      double precision beta_sum(comp_max)
      integer comp_label(comp_max)
      integer comp_num
      double precision comp_weight(comp_max)
      integer ierror
      character * ( 30 ) mixture_file_name

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BDMLIB_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BDMLIB library.'
c
c  Read information about the mixture.
c
      mixture_file_name = 'mixture.txt'

      open ( unit = iunit, file = mixture_file_name, 
     &  form = 'formatted' )

      call mixture_read ( acid_num, acid_sym, beta, beta_sum, 
     &  comp_label, comp_max, comp_num, comp_weight, ierror, iunit )

      close ( unit = iunit )
c
c  Print the amino acid parameters.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Numeric key for amino acid abbreviations.'
      write ( *, '(a)' ) ' '

      call amino_print ( acid_num, acid_sym )
c
c  Print the component parameters.
c
      call comp_param_print ( acid_num, acid_sym, comp_max, comp_num, 
     &  beta, beta_sum, comp_weight )
c
c  Check the parameters.
c
      call dirichlet_mix_check ( comp_num, acid_num, acid_num, beta, 
     &  comp_weight )
c
c  Perform the simple test of generating 10 isoleucines in a row.
c
      call test01 ( acid_num, beta, comp_num, comp_weight )
c
c  Now test a random sampling.
c
      call test02 ( acid_num, beta, comp_num, comp_weight )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BDMLIB_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( acid_num, beta, comp_num, comp_weight )

c*****************************************************************************80
c
cc TEST01 generates 10 isoleucine events in a row.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer acid_num
      integer comp_num

      integer acid_i
      double precision alpha(comp_num)
      double precision alpha_0(comp_num)
      double precision beta(acid_num,comp_num)
      integer comp_i
      double precision comp_weight(comp_num)
      double precision comp_weight_est(comp_num)
      integer event_i
      integer event_num
      integer i
      double precision p(comp_num)
      double precision p_hat(comp_num)
      integer site_num
      integer x_sample(acid_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Generate a (nonrandom) sequence of'
      write ( *, '(a)' ) '  10 isoleucine results in a row.'
      write ( *, '(a)' ) ' '
c
c  Initialize information about the Bayesian process.
c
      do i = 1, comp_num
        alpha_0(i) = 1.0D+00
      end do

      do i = 1, comp_num
        alpha(i) = alpha_0(i)
      end do

      do i = 1, comp_num
        p(i) = 1.0D+00 / dble ( comp_num )
      end do

      do i = 1, comp_num
        p_hat(i) = 1.0D+00 / dble ( comp_num )
      end do

      event_num = 10

      call r8vec_print ( comp_num, comp_weight, 
     &  'Exact component weights:' )

      call r8vec_print ( comp_num, alpha, 'Initial ALPHA:' )
c
c  Based on the current ALPHA's, compute the mean/expected value/estimate 
c  for the weights.
c
      call dirichlet_mean ( comp_num, alpha, comp_weight_est )

      call r8vec_print ( comp_num, comp_weight_est, 
     &  'Initial estimated component weights:' )

      site_num = 1

      do event_i = 1, event_num
c
c  Observe a single isoleucine.
c
        do i = 1, acid_num
          x_sample(i) = 0
        end do
        x_sample(8) = site_num
c
c  Update ALPHA, the estimated weight parameters, based on X.
c
        call event_process ( acid_num, alpha, beta, comp_num, p, 
     &    p_hat, site_num, x_sample )

        call r8vec_print ( comp_num, alpha, 'Current ALPHA:' )
c
c  Based on the current ALPHA's, compute the mean/expected value/estimate 
c  for the weights.
c
        call dirichlet_mean ( comp_num, alpha, comp_weight_est )

        call r8vec_print ( comp_num, comp_weight_est, 
     &    'Estimated component weights:' )

      end do

      return
      end
      subroutine test02 ( acid_num, beta, comp_num, comp_weight )

c*****************************************************************************80
c
cc TEST02 generates random events.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer acid_num
      integer comp_num

      integer acid_i
      double precision alpha(comp_num)
      double precision alpha_0(comp_num)
      double precision beta(acid_num,comp_num)
      integer comp_i
      integer comp_sample
      double precision comp_weight(comp_num)
      double precision comp_weight_est(comp_num)
      integer event_i
      integer event_num
      integer i
      double precision p(comp_num)
      double precision p_hat(comp_num)
      double precision p_sample(acid_num)
      integer seed
      integer site_num
      integer x_sample(acid_num)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Generate many random events.'
      write ( *, '(a)' ) '  We should be able to approximate the'
      write ( *, '(a)' ) '  exact component weights.'
      write ( *, '(a)' ) ' '
c
c  Initialize information about the Bayesian process.
c
      do i = 1, comp_num
        alpha_0(i) = 1.0D+00
      end do

      do i = 1, comp_num
        alpha(i) = alpha_0(i)
      end do

      do i = 1, comp_num
        p(i) = 1.0D+00 / dble ( comp_num )
      end do

      do i = 1, comp_num
        p_hat(i) = 1.0D+00 / dble ( comp_num )
      end do

      event_num = 1000

      call r8vec_print ( comp_num, comp_weight, 
     &  'Exact component weights:' )

      call r8vec_print ( comp_num, alpha, 'Initial ALPHA:' )
c
c  Based on the current ALPHA's, compute the mean/expected value/estimate 
c  for the weights.
c
      call dirichlet_mean ( comp_num, alpha, comp_weight_est )

      call r8vec_print ( comp_num, comp_weight_est, 
     &  'Initial estimated component weights:' )

      site_num = 10

      do event_i = 1, event_num
c
c  Randomly choose COMP_SAMPLE, the component PDF to sample.
c
        call discrete_sample ( comp_num, comp_weight, seed, 
     &    comp_sample )
c
c  Now generate the probabilities P_SAMPLE for a multinomial PDF by sampling
c  the COMP_SAMPLE-th Dirichlet distribution.
c
        call dirichlet_sample ( acid_num, beta(1,comp_sample), seed, 
     &    p_sample )
c
c  Now generate the event X_SAMPLE by sampling the multinomial PDF with
c  the given P_SAMPLE parameters.
c
       call multinomial_sample ( site_num, acid_num, p_sample, seed, 
     &    x_sample )
c
c  Update ALPHA, the estimated weight parameters, based on X.
c
        call event_process ( acid_num, alpha, beta, comp_num, p, 
     &    p_hat, site_num, x_sample )
c
c  Based on the current ALPHA's, compute the mean/expected value/estimate 
c  for the weights.
c
        call dirichlet_mean ( comp_num, alpha, comp_weight_est )

        if ( event_i .le. 10 .or. 
     &    mod ( event_i, event_num / 10 ) .eq. 0 ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a,i6)' ) 'Event ', event_i

          call r8vec_print ( comp_num, alpha, 'Current ALPHA:' )

          call r8vec_print ( comp_num, comp_weight_est, 
     &      'Estimated component weights:' )

        end if

      end do

      return
      end
