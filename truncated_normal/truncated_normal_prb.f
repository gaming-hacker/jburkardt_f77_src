      program main

c*********************************************************************72
c
cc MAIN is the main program for TRUNCATED_NORMAL_PRB.
c
c  Discussion:
c
c    TRUNCATED_NORMAL_PRB tests the TRUNCATED_NORMAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Test the TRUNCATED_NORMAL library.'
c
c  Support and utility.
c
      call i4_uniform_ab_test ( )
      call r8_choose_test ( )
      call r8_factorial2_test ( )
      call r8_mop_test ( )
      call r8_uniform_01_test ( )
      call r8poly_print_test ( )
      call r8poly_value_horner_test ( )
      call r8vec_linspace_test ( )
      call r8vec_print_test ( )
c
c  Library functions.
c
      call normal_01_cdf_test ( )
      call normal_01_cdf_inv_test ( )
      call normal_01_mean_test ( )
      call normal_01_moment_test ( )
      call normal_01_pdf_test ( )
      call normal_01_sample_test ( )
      call normal_01_variance_test ( )

      call normal_ms_cdf_test ( )
      call normal_ms_cdf_inv_test ( )
      call normal_ms_mean_test ( )
      call normal_ms_moment_test ( )
      call normal_ms_moment_central_test ( )
      call normal_ms_moment_central_values_test ( )
      call normal_ms_pdf_test ( )
      call normal_ms_sample_test ( )
      call normal_ms_variance_test ( )

      call truncated_normal_a_cdf_test ( )
      call truncated_normal_a_cdf_inv_test ( )
      call truncated_normal_a_mean_test ( )
      call truncated_normal_a_moment_test ( )
      call truncated_normal_a_pdf_test ( )
      call truncated_normal_a_sample_test ( )
      call truncated_normal_a_variance_test ( )

      call truncated_normal_ab_cdf_test ( )
      call truncated_normal_ab_cdf_inv_test ( )
      call truncated_normal_ab_mean_test ( )
      call truncated_normal_ab_moment_test ( )
      call truncated_normal_ab_pdf_test ( )
      call truncated_normal_ab_sample_test ( )
      call truncated_normal_ab_variance_test ( )

      call truncated_normal_b_cdf_test ( )
      call truncated_normal_b_cdf_inv_test ( )
      call truncated_normal_b_mean_test ( )
      call truncated_normal_b_moment_test ( )
      call truncated_normal_b_pdf_test ( )
      call truncated_normal_b_sample_test ( )
      call truncated_normal_b_variance_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine i4_uniform_ab_test ( )

c*********************************************************************72
c
cc I4_UNIFORM_AB_TEST tests I4_UNIFORM_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4_hi
      parameter ( i4_hi = 200 )
      integer i4_lo
      parameter ( i4_lo = -100 )
      integer i
      integer i4
      integer i4_uniform_ab
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
      write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
      write ( *, '(a)' ) '  in an interval [A,B].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  The lower endpoint A = ', i4_lo
      write ( *, '(a,i12)' ) '  The upper endpoint B = ', i4_hi
      write ( *, '(a,i12)' ) '  The initial seed is ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 20

        i4 = i4_uniform_ab ( i4_lo, i4_hi, seed )

        write ( *, '(2x,i8,2x,i8)' ) i, i4

      end do

      return
      end
      subroutine normal_01_cdf_test ( )

c*********************************************************************72
c
cc NORMAL_01_CDF_TEST tests NORMAL_01_CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cdf1
      double precision cdf2
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_CDF_TEST'
      write ( *, '(a)' ) '  NORMAL_01_CDF evaluates the CDF;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       X              CDF                       CDF'
      write ( *, '(a)' ) 
     &  '                     (exact)                   (computed)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call normal_01_cdf_values ( n_data, x, cdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call normal_01_cdf ( x, cdf2 )

        write ( *, '(2x,g14.6,2x,g24.16,2x,g24.16)' ) x, cdf1, cdf2

      go to 10

20    continue

      return
      end
      subroutine normal_01_cdf_inv_test ( )

c*********************************************************************72
c
cc NORMAL_01_CDF_INV_TEST tests NORMAL_01_CDF_INV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cdf
      integer n_data
      double precision x1
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_CDF_INV_TEST'
      write ( *, '(a)' ) '  NORMAL_01_CDF_INV inverts the CDF;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      CDF             X                         X'
      write ( *, '(a)' ) 
     &  '                     (exact)                   (computed)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call normal_01_cdf_values ( n_data, x1, cdf )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call normal_01_cdf_inv ( cdf, x2 )

        write ( *, '(2x,g14.6,2x,g24.16,2x,g24.16)' ) cdf, x1, x2

      go to 10

20    continue

      return
      end
      subroutine normal_01_mean_test ( )

c*********************************************************************72
c
cc NORMAL_01_MEAN_TEST tests NORMAL_01_MEAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      integer i
      double precision mean
      integer seed
      double precision x(sample_num)
      double precision xmax
      double precision xmin

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_MEAN_TEST'
      write ( *, '(a)' ) '  NORMAL_01_MEAN computes the Normal 01 mean;'

      call normal_01_mean ( mean )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

      do i = 1, sample_num
        call normal_01_sample ( seed, x(i) )
      end do

      call r8vec_mean ( sample_num, x, mean )
      call r8vec_max ( sample_num, x, xmax )
      call r8vec_min ( sample_num, x, xmin )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
      write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
      write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

      return
      end
      subroutine normal_01_moment_test ( )

c*********************************************************************72
c
cc NORMAL_01_MOMENT_TEST tests NORMAL_01_MOMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer order
      double precision value
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_MOMENT_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_01_MOMENT evaluates Normal 01 moments;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Order    Moment'
      write ( *, '(a)' ) ' '

      do order = 0, 10

        call normal_01_moment ( order, value )

        write ( *, '(2x,i6,2x,g14.6)' ) order, value

      end do

      return
      end
      subroutine normal_01_pdf_test ( )

c*********************************************************************72
c
cc NORMAL_01_PDF_TEST tests NORMAL_01_PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision pdf
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_PDF_TEST'
      write ( *, '(a)' ) '  NORMAL_01_PDF evaluates the Normal 01 CDF;'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X              PDF'
      write ( *, '(a)' ) ' '

      do i = - 20, 20

        x = dble ( i ) / 10.0D+00

        call normal_01_pdf ( x, pdf )

        write ( *, '(2x,g14.6,2x,g14.6)' ) x, pdf

      end do

      return
      end
      subroutine normal_01_sample_test ( )

c*********************************************************************72
c
cc NORMAL_01_SAMPLE_TEST tests NORMAL_01_SAMPLE.
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

      integer i
      integer seed
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_SAMPLE_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_01_SAMPLE returns samples from the normal'
      write ( *, '(a)' ) 
     &  '  distribution with mean 0 and standard deviation 1.'
      write ( *, '(a)' ) ''

      seed = 123456789

      do i = 1, 10
        call normal_01_sample ( seed, x )
        write ( *, '(2x,i4,2x,g14.6)' ) i, x
      end do

      return
      end
      subroutine normal_01_variance_test ( )

c*********************************************************************72
c
cc NORMAL_01_VARIANCE_TEST tests NORMAL_01_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      integer i
      integer seed
      double precision variance
      double precision x(sample_num)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_01_VARIANCE_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_01_VARIANCE returns the Normal 01 variance.'

      call normal_01_variance ( variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

      do i = 1, sample_num
        call normal_01_sample ( seed, x(i) )
      end do

      call r8vec_variance ( sample_num, x, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

      return
      end
      subroutine normal_ms_cdf_test ( )

c*********************************************************************72
c
cc NORMAL_MS_CDF_TEST tests NORMAL_MS_CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cdf
      integer i
      double precision mu
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_CDF_TEST'
      write ( *, '(a)' ) '  NORMAL_MS_CDF evaluates the Normal MS CDF;'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            CDF'
      write ( *, '(a)' ) ' '

      do i = -20, 20

        x = mu + sigma * dble ( i ) / 10.0D+00
        call normal_ms_cdf ( x, mu, sigma, cdf )
        write ( *, '(2x,g14.6,2x,g14.6)' ) x, cdf

      end do

      return
      end
      subroutine normal_ms_cdf_inv_test ( )

c*********************************************************************72
c
cc NORMAL_MS_CDF_INV_TEST tests NORMAL_MS_CDF_INV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision cdf
      integer i
      double precision mu
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_CDF_INV_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_MS_CDF_INV inverts the Normal MS CDF;'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            CDF           CDF_INV'
      write ( *, '(a)' ) ' '

      do i = -20, 20

        x = mu + sigma * dble ( i ) / 10.0D+00
        call normal_ms_cdf ( x, mu, sigma, cdf )
        call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2

      end do

      return
      end
      subroutine normal_ms_mean_test ( )

c*********************************************************************72
c
cc NORMAL_MS_MEAN_TEST tests NORMAL_MS_MEAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      integer i
      double precision mean
      double precision mu
      integer seed
      double precision sigma
      double precision variance
      double precision x(sample_num)
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_MEAN_TEST'
      write ( *, '(a)' ) '  NORMAL_MS_MEAN computes the mean '
      write ( *, '(a)' ) '  for tne Normal MS PDF'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call normal_ms_mean ( mu, sigma, mean )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

      seed = 123456789
      do i = 1, sample_num
        call normal_ms_sample ( mu, sigma, seed, x(i) )
      end do

      call r8vec_mean ( sample_num, x, mean )
      call r8vec_max ( sample_num, x, xmax )
      call r8vec_min ( sample_num, x, xmin )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
      write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
      write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

      return
      end
      subroutine normal_ms_moment_test ( )

c*********************************************************************72
c
cc NORMAL_MS_MOMENT_TEST tests NORMAL_MS_MOMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none
    
      double precision mu
      integer order
      double precision sigma
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_MOMENT_TEST'
      write ( *, '(a)' ) '  NORMAL_MS_MOMENT returns the moments'
      write ( *, '(a)' ) '  for tne Normal MS PDF'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   Order      Moment'
      write ( *, '(a)' ) ''

      do order = 0, 10
        call normal_ms_moment ( order, mu, sigma, value )
        write ( *, '(2x,i6,2x,g14.6)' ) order, value
      end do

      return
      end
      subroutine normal_ms_moment_central_test ( )

c*********************************************************************72
c
cc NORMAL_MS_MOMENT_CENTRAL_TEST tests NORMAL_MS_MOMENT_CENTRAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision mu
      integer order
      double precision sigma
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_MOMENT_CENTRAL_TEST'
      write ( *, '(a)' ) '  NORMAL_MS_MOMENT_CENTRAL returns central'
      write ( *, '(a)' ) '  moments for tne Normal MS PDF'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   Order      Moment'
      write ( *, '(a)' ) ''

      do order = 0, 10
        call normal_ms_moment_central ( order, mu, sigma, value )
        write ( *, '(2x,i6,2x,g14.6)' ) order, value
      end do

      return
      end
      subroutine normal_ms_moment_central_values_test ( )

c*********************************************************************72
c
cc NORMAL_MS_MOMENT_CENTRAL_VALUES_TEST tests NORMAL_MS_MOMENT_CENTRAL_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision mu
      integer order
      double precision sigma
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_MOMENT_CENTRAL_VALUES_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_MS_MOMENT_CENTRAL_VALUES returns values '
      write ( *, '(a)' ) 
     &  '  of selected central moments for tne Normal MS PDF'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   Order      Moment'
      write ( *, '(a)' ) ''

      do order = 0, 10
        call normal_ms_moment_central_values ( order, mu, sigma, value )
        write ( *, '(2x,i6,2x,g14.6)' ) order, value
      end do

      return
      end
      subroutine normal_ms_pdf_test ( )

c*********************************************************************72
c
cc NORMAL_MS_PDF_TEST tests NORMAL_MS_PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision mu
      double precision pdf
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_PDF_TEST'
      write ( *, '(a)' ) '  NORMAL_MS_PDF evaluates the Normal MS PDF;'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            PDF'
      write ( *, '(a)' ) ' '

      do i = -20, 20

        x = mu + sigma * dble ( i ) / 10.0D+00
        call normal_ms_pdf ( x, mu, sigma, pdf )
        write ( *, '(2x,4g14.6)' ) x, pdf

      end do

      return
      end
      subroutine normal_ms_sample_test ( )

c*********************************************************************72
c
cc NORMAL_MS_SAMPLE_TEST tests NORMAL_MS_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NORMAL_MS_SAMPLE_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_MS_SAMPLE returns samples the Normal MS PDF.'
      write ( *, '(a)' ) ''

      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ''

      do i = 1, 10
        call normal_ms_sample ( mu, sigma, seed, x )
        write ( *, '(2x,i4,2x,g14.6)' ) i, x
      end do

      return
      end
      subroutine normal_ms_variance_test ( )

c*********************************************************************72
c
cc NORMAL_MS_VARIANCE_TEST tests NORMAL_MS_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision variance
      double precision x(sample_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_MS_VARIANCE_TEST'
      write ( *, '(a)' ) 
     &  '  NORMAL_MS_VARIANCE returns the Normal MS variance.'

      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call normal_ms_variance ( mu, sigma, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

      seed = 123456789
      do i = 1, sample_num
        call normal_ms_sample ( mu, sigma, seed, x(i) )
      end do

      call r8vec_variance ( sample_num, x, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

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
c    26 July 2014
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
      write ( *, '(a)' ) '         N         K       CNK'
 
      do n = 0, 5
        write ( *, '(a)' ) ' '
        do k = 0, n
          cnk = r8_choose ( n, k )
          write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) n, k, cnk
        end do
      end do
 
      return
      end
      subroutine r8_factorial2_test ( )

c*********************************************************************72
c
cc R8_FACTORIAL2_TEST tests R8_FACTORIAL2.
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

      double precision f1
      double precision f2
      integer n
      integer n_data
      double precision r8_factorial2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_FACTORIAL2_TEST'
      write ( *, '(a)' ) 
     &  '  R8_FACTORIAL2 computes the double factorial function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    N                Exact' //
     &  '                  Computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call r8_factorial2_values ( n_data, n, f1 )

        if ( n_data == 0 ) then
          go to 20
        end if

        f2 = r8_factorial2 ( n );

        write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) n, f1, f2

      go to 10

20    continue
     
      return
      end
      subroutine r8_mop_test ( )

c*********************************************************************72
c
cc R8_MOP_TEST tests R8_MOP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4
      integer i4_max
      integer i4_min
      integer i4_uniform_ab
      double precision r8
      double precision r8_mop
      integer seed
      integer test

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8_MOP_TEST'
      write ( *, '(a)' ) '  R8_MOP evaluates (-1.0)^I4 as an R8.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '    I4  R8_MOP(I4)'
      write ( *, '(a)' ) ''

      i4_min = -100
      i4_max = +100
      seed = 123456789

      do test = 1, 10
        i4 = i4_uniform_ab ( i4_min, i4_max, seed )
        r8 = r8_mop ( i4 )
        write ( *, '(2x,i4,2x,f4.1)' ) i4, r8
      end do

      return
      end
      subroutine r8_uniform_01_test ( )

c*********************************************************************72
c
cc R8_UNIFORM_01_TEST tests R8_UNIFORM_01
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 June 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r8_uniform_01
      integer seed
      integer seed_old
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
      write ( *, '(a)' ) 
     &  '  R8_UNIFORM_01 produces a sequence of random values.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  Using random seed ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SEED   R8_UNIFORM_01(SEED)'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        seed_old = seed
        x = r8_uniform_01 ( seed )
        write ( *, '(2x,i12,2x,g14.6)' ) seed, x
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Verify that the sequence can be restarted.'
      write ( *, '(a)' ) 
     &  '  Set the seed back to its original value, and see that'
      write ( *, '(a)' ) '  we generate the same sequence.'

      seed = 123456789
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  SEED   R8_UNIFORM_01(SEED)'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        seed_old = seed
        x = r8_uniform_01 ( seed )
        write ( *, '(2x,i12,2x,g14.6)' ) seed, x
      end do

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
      subroutine r8vec_linspace_test ( )

c*********************************************************************72
c
cc R8VEC_LINSPACE_TEST tests R8VEC_LINSPACE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_LINSPACE_TEST'
      write ( *, '(a)' ) '  For a R8VEC:'
      write ( *, '(a)' ) 
     &  '  R8VEC_LINSPACE: evenly spaced points between A and B;'

      a = 10.0D+00
      b = 20.0D+00

      call r8vec_linspace ( n, a, b, x )
      call r8vec_print ( n, x, '  r8vec_linspace ( 5, 10, 20 )' )

      return
      end
      subroutine r8vec_print_test ( )

c*********************************************************************72
c
cc R8VEC_PRINT_TEST tests R8VEC_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n)

      save a

      data a /
     &  123.456D+00,
     &  0.000005D+00,
     &  -1.0D+06,
     &  3.14159265D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
      write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

      call r8vec_print ( n, a, '  The vector:' )

      return
      end
      subroutine truncated_normal_a_cdf_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_CDF_TEST tests TRUNCATED_NORMAL_A_CDF.
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

      double precision cdf1
      double precision cdf2
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_CDF_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_A_CDF evaluates'
      write ( *, '(a)' ) 
     &  '  the CDF of the lower Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU         S         A        X' //
     &  '               CDF1                      CDF2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_a_cdf_values ( n_data, mu, sigma, 
     &    a, x, cdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call truncated_normal_a_cdf ( x, mu, sigma, a, cdf2 )

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,' // 
     &    'g24.16,2x,g24.16)' ) 
     &    mu, sigma, a, x, cdf1, cdf2

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_a_cdf_inv_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_CDF_INV_TEST tests TRUNCATED_NORMAL_A_CDF_INV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision cdf
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_CDF_INV_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_CDF_INV inverts '
      write ( *, '(a)' ) '  the lower Truncated Normal CDF;'

      a = 50.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            CDF           CDF_INV'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        call truncated_normal_a_sample ( mu, sigma, a, seed, x )
        call normal_ms_cdf ( x, mu, sigma, cdf )
        call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2
      end do

      return
      end
      subroutine truncated_normal_a_mean_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_MEAN_TEST tests TRUNCATED_NORMAL_A_MEAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      double precision a
      integer i
      double precision mean
      double precision mu
      integer seed
      double precision sigma
      double precision x(sample_num)
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_MEAN_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_MEAN computes the mean'
      write ( *, '(a)' ) '  for the lower Truncated Normal PDF'

      a = 50.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call truncated_normal_a_mean ( mu, sigma, a, mean )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

      seed = 123456789
      do i = 1, sample_num
        call truncated_normal_a_sample ( mu, sigma, a, seed, x(i) )
      end do

      call r8vec_mean ( sample_num, x, mean )
      call r8vec_max ( sample_num, x, xmax )
      call r8vec_min ( sample_num, x, xmin )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
      write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
      write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

      return
      end
      subroutine truncated_normal_a_moment_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_MOMENT_TEST tests TRUNCATED_NORMAL_A_MOMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision a_test(6)
      double precision moment
      double precision mu
      double precision mu_test(6)
      integer order
      double precision sigma
      double precision sigma_test(6)
      integer test
      integer test_num

      save a_test
      save mu_test
      save sigma_test

      data a_test /
     &  0.0D+00, -10.0D+00, 10.0D+00, -10.0D+00, 10.0D+00, -10.0D+00 /
      data mu_test /
     &  0.0D+00,  0.0D+00,  0.0D+00,  0.0D+00, 0.0D+00, -5.0D+00 /
      data sigma_test /
     &  1.0D+00,  1.0D+00,  1.0D+00, 2.0D+00, 2.0D+00,  1.0D+00 /

      test_num = 6
 
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_MOMENT_TEST'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_A_MOMENT evaluates the moments.'
      write ( *, '(a)' ) '  of the lower Truncated Normal PDF:'


      do test = 1, test_num
        mu = mu_test(test)
        sigma = sigma_test(test)
        a = a_test(test)
        write ( *, '(a)' ) ''
        write ( *, '(a,i2,a,g14.6,a,g14.6,a,g14.6)' ) 
     &    '  Test = ', test, ' Mu = ', mu, 
     &    ' Sigma = ', sigma, ' A = ', a
        write ( *, '(a)' ) ' Order  Moment'
        write ( *, '(a)' ) ''
        do order = 0, 8
          call truncated_normal_a_moment ( order, mu, sigma, a, moment )
          write ( *, '(2x,i2,2x,g14.6)' ) order, moment
        end do
      end do

      return
      end
      subroutine truncated_normal_a_pdf_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_PDF_TEST tests TRUNCATED_NORMAL_A_PDF.
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
      double precision mu
      integer n_data
      double precision pdf1
      double precision pdf2
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_PDF:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_A_PDF evaluates'
      write ( *, '(a)' ) 
     &  '  the PDF of the lower Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU         S         A         X' //
     &  '               PDF1                      PDF2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_a_pdf_values ( n_data, mu, sigma, 
     &    a, x, pdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call truncated_normal_a_pdf ( x, mu, sigma, a, pdf2 )

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,' // 
     &    'g24.16,2x,g24.16)' ) 
     &    mu, sigma, a, x, pdf1, pdf2

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_a_sample_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_SAMPLE_TEST tests TRUNCATED_NORMAL_A_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision cdf
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_SAMPLE_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_SAMPLE samples '
      write ( *, '(a)' ) '  the lower Truncated Normal CDF;'

      a = 50.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      write ( *, '(a)' ) ' '

      do i = 1, 10
        call truncated_normal_a_sample ( mu, sigma, a, seed, x )
        write ( *, '(2x,i2,2x4g14.6)' ) i, x
      end do

      return
      end
      subroutine truncated_normal_a_variance_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_VARIANCE_TEST tests TRUNCATED_NORMAL_A_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      double precision a
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision variance
      double precision x(sample_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_A_VARIANCE_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_A_VARIANCE returns the'
      write ( *, '(a)' ) 
     &  '  variance of the lower Truncated Normal distribution.'

      a = 50.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call truncated_normal_a_variance ( mu, sigma, a, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

      seed = 123456789
      do i = 1, sample_num
        call truncated_normal_a_sample ( mu, sigma, a, seed, x(i) )
      end do

      call r8vec_variance ( sample_num, x, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

      return
      end
      subroutine truncated_normal_ab_cdf_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_CDF_TEST tests TRUNCATED_NORMAL_AB_CDF.
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
      double precision cdf1
      double precision cdf2
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_CDF_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_AB_CDF evaluates'
      write ( *, '(a)' ) 
     &  '  the CDF of the Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU         S         A         B        X' //
     &  '               CDF1                      CDF2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_ab_cdf_values ( n_data, mu, sigma, 
     &    a, b, x, cdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call truncated_normal_ab_cdf ( x, mu, sigma, a, b, cdf2 )

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,' // 
     &    'g14.6,2x,g24.16,2x,g24.16)' ) 
     &    mu, sigma, a, b, x, cdf1, cdf2

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_ab_cdf_inv_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_CDF_INV_TEST tests TRUNCATED_NORMAL_AB_CDF_INV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_CDF_INV_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_CDF_INV inverts '
      write ( *, '(a)' ) '  the Truncated Normal CDF;'

      a = 50.0D+00
      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            CDF           CDF_INV'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x )
        call normal_ms_cdf ( x, mu, sigma, cdf )
        call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2
      end do

      return
      end
      subroutine truncated_normal_ab_mean_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_MEAN_TEST tests TRUNCATED_NORMAL_AB_MEAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      double precision a
      double precision b
      integer i
      double precision mean
      double precision mu
      integer seed
      double precision sigma
      double precision x(sample_num)
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MEAN_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_MEAN computes the mean'
      write ( *, '(a)' ) '  for the Truncated Normal PDF'

      a = 50.0D+00
      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call truncated_normal_ab_mean ( mu, sigma, a, b, mean )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

      seed = 123456789
      do i = 1, sample_num
        call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x(i) )
      end do

      call r8vec_mean ( sample_num, x, mean )
      call r8vec_max ( sample_num, x, xmax )
      call r8vec_min ( sample_num, x, xmin )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
      write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
      write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

      return
      end
      subroutine truncated_normal_ab_moment_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_MOMENT_TEST tests TRUNCATED_NORMAL_AB_MOMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision a_test(9)
      double precision b
      double precision b_test(9)
      double precision moment
      double precision mu
      double precision mu_test(9)
      integer order
      double precision sigma
      double precision sigma_test(9)
      integer test
      integer test_num

      save a_test
      save b_test
      save mu_test
      save sigma_test

      data a_test /
     &  -1.0D+00, 0.0D+00, -1.0D+00, -1.0D+00,  0.0D+00, 
     &  0.5D+00, -2.0D+00, -4.0D+00, 4.0D+00 /
      data b_test /
     &  1.0D+00, 1.0D+00,  0.0D+00,  1.0D+00,  2.0D+00, 
     &  2.0D+00,  2.0D+00,  4.0D+00, 7.0D+00  /
      data mu_test /
     &  0.0D+00, 0.0D+00,  0.0D+00,  0.0D+00,  1.0D+00, 
     &  0.0D+00, 0.0D+00,  0.0D+00, 5.0D+00 /
      data sigma_test /
     &  1.0D+00, 1.0D+00,  1.0D+00, 2.0D+00,  1.0D+00, 
     &  1.0D+00, 1.0D+00,  1.0D+00, 0.5D+00 /

      test_num = 9
 
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT_TEST'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_AB_MOMENT evaluates the moments'
      write ( *, '(a)' ) '  of the Truncated Normal PDF:'

      do test = 1, test_num
        mu = mu_test(test)
        sigma = sigma_test(test)
        a = a_test(test)
        b = b_test(test)
        write ( *, '(a)' ) ''
        write ( *, '(a,i2,a,g14.6,a,g14.6,a,g14.6,a,g14.6)' ) 
     &    '  Test = ', test, ' Mu = ', mu, 
     &    ' Sigma = ', sigma, ' A = ', a,
     &    '  B = ', b
        write ( *, '(a)' ) ' Order  Moment'
        write ( *, '(a)' ) ''
        do order = 0, 8
          call truncated_normal_ab_moment ( order, mu, sigma, a, b, 
     &      moment )
          write ( *, '(2x,i2,2x,g14.6)' ) order, moment
        end do
      end do

      return
      end
      subroutine truncated_normal_ab_pdf_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_CDF_TEST tests TRUNCATED_NORMAL_AB_PDF_VALUES.
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
      double precision mu
      integer n_data
      double precision pdf1
      double precision pdf2
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_PDF_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_AB_PDF evaluates '
      write ( *, '(a)' ) 
     &  '  the PDF of the Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU         S         A         B        X' //
     &  '               PDF1                      PDF2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_ab_pdf_values ( n_data, mu, sigma, 
     &    a, b, x, pdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call truncated_normal_ab_pdf ( x, mu, sigma, a, b, pdf2 )

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,' // 
     &    'g14.6,2x,g24.16,2x,g24.16)' ) 
     &    mu, sigma, a, b, x, pdf1, pdf2

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_ab_sample_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_SAMPLE_TEST tests TRUNCATED_NORMAL_AB_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_SAMPLE_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_SAMPLE samples '
      write ( *, '(a)' ) '  the Truncated Normal CDF;'

      a = 50.0D+00
      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      write ( *, '(a)' ) ' '

      do i = 1, 10
        call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x )
        write ( *, '(2x,i2,2x4g14.6)' ) i, x
      end do

      return
      end
      subroutine truncated_normal_ab_variance_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_VARIANCE_TEST tests TRUNCATED_NORMAL_AB_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      double precision a
      double precision b
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision variance
      double precision x(sample_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_VARIANCE_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_AB_VARIANCE returns the'
      write ( *, '(a)' ) 
     &  '  variance of the Truncated Normal distribution.'

      a = 50.0D+00
      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Lower limit A = ', a
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call truncated_normal_ab_variance ( mu, sigma, a, b, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

      seed = 123456789
      do i = 1, sample_num
        call truncated_normal_ab_sample ( mu, sigma, a, b, seed, x(i) )
      end do

      call r8vec_variance ( sample_num, x, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

      return
      end
      subroutine truncated_normal_b_cdf_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_CDF_TEST tests TRUNCATED_NORMAL_B_CDF.
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
      double precision cdf1
      double precision cdf2
      double precision mu
      integer n_data
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_CDF_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_B_CDF returns values of '
      write ( *, '(a)' ) 
     &  '  the CDF of the upper Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU         S         B        X' //
     &  '               CDF1                      CDF2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_b_cdf_values ( n_data, mu, sigma, 
     &    b, x, cdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call truncated_normal_b_cdf ( x, mu, sigma, b, cdf2 )

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,' // 
     &    'g24.16,2x,g24.16)' ) 
     &    mu, sigma, b, x, cdf1, cdf2

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_b_cdf_inv_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_CDF_INV_TEST tests TRUNCATED_NORMAL_B_CDF_INV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision cdf
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_CDF_INV_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_CDF_INV inverts '
      write ( *, '(a)' ) '  the upper Truncated Normal CDF;'

      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X            CDF           CDF_INV'
      write ( *, '(a)' ) ' '

      do i = 1, 10
        call truncated_normal_b_sample ( mu, sigma, b, seed, x )
        call normal_ms_cdf ( x, mu, sigma, cdf )
        call normal_ms_cdf_inv ( cdf, mu, sigma, x2 )
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) x, cdf, x2
      end do

      return
      end
      subroutine truncated_normal_b_mean_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_MEAN_TEST tests TRUNCATED_NORMAL_B_MEAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      double precision b
      integer i
      double precision mean
      double precision mu
      integer seed
      double precision sigma
      double precision x(sample_num)
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MEAN_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_MEAN computes the mean'
      write ( *, '(a)' ) '  for the upper Truncated Normal PDF'

      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call truncated_normal_b_mean ( mu, sigma, b, mean )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF mean =        ', mean

      seed = 123456789
      do i = 1, sample_num
        call truncated_normal_b_sample ( mu, sigma, b, seed, x(i) )
      end do

      call r8vec_mean ( sample_num, x, mean )
      call r8vec_max ( sample_num, x, xmax )
      call r8vec_min ( sample_num, x, xmin )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample mean =     ', mean
      write ( *, '(a,g14.6)' ) '  Sample maximum =  ', xmax
      write ( *, '(a,g14.6)' ) '  Sample minimum =  ', xmin

      return
      end
      subroutine truncated_normal_b_moment_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_MOMENT_TEST tests TRUNCATED_NORMAL_B_MOMENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision b_test(6)
      double precision moment
      double precision mu
      double precision mu_test(6)
      integer order
      double precision sigma
      double precision sigma_test(6)
      integer test
      integer test_num

      save b_test
      save mu_test
      save sigma_test

      data b_test /
     &  0.0D+00, 10.0D+00, -10.0D+00, 10.0D+00, -10.0D+00, 10.0D+00 /
      data mu_test /
     &  0.0D+00,  0.0D+00,  0.0D+00,  0.0D+00, 0.0D+00, 5.0D+00 /
      data sigma_test /
     &  1.0D+00,  1.0D+00,  1.0D+00, 2.0D+00, 2.0D+00,  1.0D+00 /

      test_num = 6
 
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT_TEST'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_B_MOMENT evaluates the moments'
      write ( *, '(a)' ) '  for the upper Truncated Normal PDF.'

      do test = 1, test_num
        mu = mu_test(test)
        sigma = sigma_test(test)
        b = b_test(test)
        write ( *, '(a)' ) ''
        write ( *, '(a,i2,a,g14.6,a,g14.6,a,g14.6)' ) 
     &    '  Test = ', test, ' Mu = ', mu, 
     &    ' Sigma = ', sigma, ' B = ', b
        write ( *, '(a)' ) ' Order  Moment'
        write ( *, '(a)' ) ''
        do order = 0, 8
          call truncated_normal_b_moment ( order, mu, sigma, b, moment )
          write ( *, '(2x,i2,2x,g14.6)' ) order, moment
        end do
      end do

      return
      end
      subroutine truncated_normal_b_pdf_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_PDF_TEST demonstrates the use of TRUNCATED_NORMAL_B_PDF.
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
      double precision mu
      integer n_data
      double precision pdf1
      double precision pdf2
      double precision sigma
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_PDF_TEST:'
      write ( *, '(a)' ) 
     &  '  TRUNCATED_NORMAL_B_PDF returns values of '
      write ( *, '(a)' ) 
     &  '  the PDF of the upper Truncated Normal Distribution.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      MU         S         B        X' //
     &  '               PDF1                      PDF2'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call truncated_normal_b_pdf_values ( n_data, mu, sigma, 
     &    b, x, pdf1 )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call truncated_normal_b_pdf ( x, mu, sigma, b, pdf2 )

        write ( *, 
     &    '(2x,f8.1,2x,f8.1,2x,f8.1,2x,f8.1,2x,' // 
     &    'g24.16,2x,g24.16)' ) 
     &    mu, sigma, b, x, pdf1, pdf2

      go to 10

20    continue

      return
      end
      subroutine truncated_normal_b_sample_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_SAMPLE_TEST tests TRUNCATED_NORMAL_B_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision b
      double precision cdf
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision x
      double precision x2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_SAMPLE_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_SAMPLE samples '
      write ( *, '(a)' ) '  the upper Truncated Normal CDF;'

      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU = ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      write ( *, '(a)' ) ' '

      do i = 1, 10
        call truncated_normal_b_sample ( mu, sigma, b, seed, x )
        write ( *, '(2x,i2,2x4g14.6)' ) i, x
      end do

      return
      end
      subroutine truncated_normal_b_variance_test ( )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_VARIANCE_TEST tests TRUNCATED_NORMAL_B_VARIANCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer sample_num
      parameter ( sample_num = 1000 )

      double precision b
      integer i
      double precision mu
      integer seed
      double precision sigma
      double precision variance
      double precision x(sample_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_VARIANCE_TEST'
      write ( *, '(a)' ) '  TRUNCATED_NORMAL_B_VARIANCE returns the'
      write ( *, '(a)' ) 
     &  '  variance of the upper Truncated Normal distribution.'

      b = 150.0D+00
      mu = 100.0D+00
      sigma = 15.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Upper limit B = ', b
      write ( *, '(a,g14.6)' ) '  PDF parameter MU =    ', mu
      write ( *, '(a,g14.6)' ) '  PDF parameter SIGMA = ', sigma

      call truncated_normal_b_variance ( mu, sigma, b, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF variance =    ', variance

      seed = 123456789
      do i = 1, sample_num
        call truncated_normal_b_sample ( mu, sigma, b, seed, x(i) )
      end do

      call r8vec_variance ( sample_num, x, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Sample size =     ', sample_num
      write ( *, '(a,g14.6)' ) '  Sample variance = ', variance

      return
      end