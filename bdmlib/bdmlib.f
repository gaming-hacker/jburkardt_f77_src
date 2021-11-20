      subroutine amino_print ( acid_num, acid_sym )

c*********************************************************************72
c
cc AMINO_PRINT prints the amino acid parameters.
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
c  Parameters:
c
c    Input, integer ACID_NUM, the number of amino acids.
c
c    Input, character ACID_SYM(ACID_NUM), the one letter amino acid codes.
c
      implicit none

      integer acid_num

      integer acid_i
      character * ( 27 ) acid_name
      character acid_sym(acid_num)
      character c

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  I  Amino Acid Symbol'
      write ( *, '(a)' ) ' '
      do acid_i = 1, acid_num
        c = acid_sym(acid_i)
        call ch_to_amino_name ( c, acid_name )
        write ( *, '(i3,2x,a,2x,a)' ) 
     &    acid_i, acid_sym(acid_i), acid_name
      end do

      return
      end
      subroutine binomial_sample ( a, b, seed, x )

c*********************************************************************72
c
cc BINOMIAL_SAMPLE samples the Binomial PDF.
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
c  Reference:
c
c    William Kennedy, James Gentle,
c    Algorithm BU,
c    Statistical Computing,
c    Dekker, 1980.
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c    1 <= A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0 <= B <= 1.0.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      integer a
      double precision b
      integer i
      double precision r8_uniform_01
      integer seed
      double precision u
      integer x

      x = 0

      do i = 1, a

        u = r8_uniform_01 ( seed )

        if ( u <= b ) then
          x = x + 1
        end if

      end do

      return
      end
      subroutine ch_cap ( ch )

c*********************************************************************72
c
cc CH_CAP capitalizes a single character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character CH, the character to capitalize.
c
      implicit none

      character ch
      integer itemp

      itemp = ichar ( ch )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        ch = char ( itemp - 32 )
      end if

      return
      end
      function ch_eqi ( c1, c2 )

c*********************************************************************72
c
cc CH_EQI is a case insensitive comparison of two characters for equality.
c
c  Example:
c
c    CH_EQI ( 'A', 'a' ) is TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C1, C2, the characters to compare.
c
c    Output, logical CH_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c1_cap
      character c2
      character c2_cap
      logical ch_eqi

      c1_cap = c1
      c2_cap = c2

      call ch_cap ( c1_cap )
      call ch_cap ( c2_cap )

      if ( c1_cap == c2_cap ) then
        ch_eqi = .true.
      else
        ch_eqi = .false.
      end if

      return
      end
      subroutine ch_next ( line, cval, done )

c*********************************************************************72
c
cc CH_NEXT "reads" space-separated characters from a string, one at a time.
c
c  Example:
c
c    Input:
c
c      LINE = ' A  B, C    DE  F'
c
c    Output:
c
c      'A', 'B', 'C', 'D', 'E', 'F', and then blanks.
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
c  Parameters:
c
c    Input, character * ( * ) LINE, a string, presumably containing
c    characters, possibly separated by spaces or commas.
c
c    Output, character CVAL.  If DONE is FALSE, then CVAL contains the
c    "next" character read from LINE.  If DONE is TRUE, then
c    CVAL is blank.
c
c    Input/output, logical DONE.
c    On input with a fresh value of LINE, the user should set
c    DONE to TRUE.
c    On output, the routine sets DONE to FALSE if another character
c    was read, or TRUE if no more characters could be read.
c
      implicit none

      character cval
      logical done
      integer i
      character * ( * ) line
      integer next

      save next

      data next / 1 /

      if ( done ) then
        next = 1
        done = .false.
      end if

      do i = next, len ( line )

        if ( line(i:i) .ne. ' ' .and. line(i:i) .ne. ',' ) then
          cval = line(i:i)
          next = i + 1
          return
        end if

      end do

      done = .true.
      next = 1
      cval = ' '

      return
      end
      subroutine ch_to_amino_name ( c, amino_name )

c*********************************************************************72
c
cc CH_TO_AMINO_NAME converts a character to an amino acid name.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Carl Branden, John Tooze,
c    Introduction to Protein Structure,
c    Garland Publishing, 1991.
c
c  Parameters:
c
c    Input, character C, the one letter code for an amino acid.
c    Lower and upper case letters are treated the same.
c
c    Output, character * ( * ) AMINO_NAME, the full name of the
c    corresponding amino acid.  The longest name is 27 characters.  
c    If the input code is not recognized, then AMINO_NAME will be 
c    set to '???'.
c
      implicit none

      integer n
      parameter ( n = 23 )

      character * ( * ) amino_name
      character * ( 27 ) amino_table(n)
      character c
      character c_table(n)
      logical ch_eqi
      integer i

      save amino_table
      save c_table

      data amino_table /
     &  'Alanine                    ', 
     &  'Aspartic acid or Asparagine', 
     &  'Cysteine                   ', 
     &  'Aspartic acid              ', 
     &  'Glutamic acid              ', 
     &  'Phenylalanine              ', 
     &  'Glycine                    ', 
     &  'Histidine                  ', 
     &  'Isoleucine                 ', 
     &  'Lysine                     ', 
     &  'Leucine                    ', 
     &  'Methionine                 ', 
     &  'Asparagine                 ', 
     &  'Proline                    ', 
     &  'Glutamine                  ', 
     &  'Arginine                   ', 
     &  'Serine                     ', 
     &  'Threonine                  ', 
     &  'Valine                     ', 
     &  'Tryptophan                 ', 
     &  'Undetermined amino acid    ', 
     &  'Tyrosine                   ', 
     &  'Glutamic acid or Glutamine ' /

      data c_table /
     &  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 
     &  'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 
     &  'X', 'Y', 'Z' /

      do i = 1, n
        if ( ch_eqi ( c, c_table(i) ) ) then
          amino_name = amino_table(i)
          return
        end if
      end do

      amino_name = '???'

      return
      end
      subroutine ch_to_digit ( c, digit )

c*********************************************************************72
c
cc CH_TO_DIGIT returns the integer value of a base 10 digit.
c
c  Example:
c
c     C   DIGIT
c    ---  -----
c    '0'    0
c    '1'    1
c    ...  ...
c    '9'    9
c    ' '    0
c    'X'   -1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C, the decimal digit, '0' through '9' or blank
c    are legal.
c
c    Output, integer DIGIT, the corresponding integer value.  If C was
c    'illegal', then DIGIT is -1.
c
      implicit none

      character c
      integer digit

      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

        digit = ichar ( c ) - 48

      else if ( c .eq. ' ' ) then

        digit = 0

      else

        digit = -1

      end if

      return
      end
      subroutine comp_param_print ( acid_num, acid_sym, comp_max, 
     &  comp_num, beta, beta_sum, comp_weight )

c*********************************************************************72
c
cc COMP_PARAM_PRINT prints the parameters for the mixture components.
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
c  Parameters:
c
c    Input, integer ACID_NUM, the number of amino acids.
c
c    Input, character ACID_SYM(ACID_NUM), the one letter amino acid codes.
c
c    Input, integer COMP_MAX, the maximum number of Dirichlet 
c    mixture components.
c
c    Input, integer COMP_NUM, the number of components in the 
c    Dirichlet mixture.
c
c    Input, double precision BETA(ACID_NUM,COMP_MAX); BETA(I,J) is the 
c    parameter for the J-th acid in the I-th Dirichlet mixture component.
c
c    Input, double precision BETA_SUM(COMP_MAX), the sum of the values of
c    BETA(ACID_I,COMP_I) for a given component COMP_I.
c
c    Input, double precision COMP_WEIGHT(COMP_NUM), the mixture weight of each
c    component.  These values should be nonnegative, and sum to 1.  They
c    represent the relative proportion of each component in the mixture.
c
      implicit none

      integer acid_num
      integer comp_max

      integer acid_i
      character acid_sym(acid_num)
      integer comp_i
      double precision beta(acid_num,comp_max)
      double precision beta_sum(comp_max)
      integer comp_num
      double precision comp_weight(comp_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Number of components = ', comp_num
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(''Compon:'',20i8)' ) ( comp_i, comp_i = 1, comp_num )
      write ( *, '(''Weight:'',20f8.4)' ) comp_weight(1:comp_num)
      write ( *, '(a)' ) ' '

      do acid_i = 1, acid_num
        write ( *, '(i2,2x,a1,2x,20f8.4)' ) acid_i, acid_sym(acid_i), 
     &    beta(acid_i,1:comp_num)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a3,4x,20f8.4)' ) 'Sum', beta_sum(1:comp_num)

      return
      end
      subroutine dirichlet_mean ( n, a, mean )

c*********************************************************************72
c
cc DIRICHLET_MEAN returns the means of the Dirichlet PDF.
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
c  Parameters:
c
c    Input, integer N, the number of components.
c
c    Input, double precision A(N), the probabilities for each component.
c    Each A(I) should be nonnegative, and at least one should be positive.
c
c    Output, double precision MEAN(N), the means of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_sum
      integer i
      double precision mean(n)
      double precision r8vec_sum

      do i = 1, n
        if ( a(i) .lt. 0.0D00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIRICHLET_MEAN - Fatal error!'
          write ( *, '(a)' ) '  At least one entry of A is negativec'
          stop
        end if
      end do

      a_sum = r8vec_sum ( n, a )

      if ( a_sum .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_MEAN - Fatal error!'
        write ( *, '(a)' ) '  All entries of A are zero.'
        stop
      end if

      do i = 1, n
        mean(i) = a(i) / a_sum
      end do

      return
      end
      subroutine dirichlet_mix_check ( comp_num, elem_max, elem_num, 
     &  a, comp_weight )

c*********************************************************************72
c
cc DIRICHLET_MIX_CHECK checks the parameters of a Dirichlet mixture PDF.
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
c  Parameters:
c
c    Input, integer COMP_NUM, the number of components in the 
c    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
c    that are mixed together.
c
c    Input, integer ELEM_MAX, the leading dimension of A, which
c    must be at least ELEM_NUM.
c
c    Input, integer ELEM_NUM, the number of elements of an 
c    observation.
c
c    Input, double precision A(ELEM_MAX,COMP_NUM), the probabilities for 
c    element ELEM_NUM in component COMP_NUM.
c    Each A(I,J) should be greater than or equal to 0.0.
c
c    Input, integer COMP_WEIGHT(COMP_NUM), the mixture weights of
c    the densities. These do not need to be normalized.  The weight of a given 
c    component is the relative probability that that component will be used 
c    to generate the sample.
c
      implicit none

      integer comp_num
      integer elem_max
      integer elem_num

      double precision a(elem_max,comp_num)
      integer comp_i
      double precision comp_weight(comp_num)
      integer elem_i
      logical positive

      do comp_i = 1, comp_num

        do elem_i = 1, elem_num
          if ( a(elem_i,comp_i) .lt. 0.0D+00 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Fatal error!'
            write ( *, '(a)' ) '  A(ELEM,COMP) < 0.'
            write ( *, '(a,i6)' ) '  COMP = ', comp_i
            write ( *, '(a,i6)' ) '  ELEM = ', elem_i
            write ( *, '(a,g14.6)' ) 
     &        '  A(COMP,ELEM) = ', a(elem_i,comp_i)
            stop
          end if
        end do

      end do

      positive = .false.

      do comp_i = 1, comp_num

        if ( comp_weight(comp_i) .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Fatal error!'
          write ( *, '(a)' ) '  COMP_WEIGHT(COMP) < 0.'
          write ( *, '(a,i6)' ) '  COMP = ', comp_i
          write ( *, '(a,g14.6)' ) 
     &      '  COMP_WEIGHT(COMP) = ', comp_weight(comp_i)
          stop
        else if ( 0.0D+00 .lt. comp_weight(comp_i)  ) then
          positive = .true.
        end if

      end do

      if ( .not. positive ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Fatal error!'
        write ( *, '(a)' ) '  All component weights are zero.'
        stop
      end if

      return
      end
      subroutine dirichlet_multinomial_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc DIRICHLET_MULTINOMIAL_PDF evaluates a Dirichlet Multinomial PDF.
c
c  Discussion:
c
c    PDF(X)(A,B,C) = Comb(A,B,X) * ( Gamma(C_Sum) / Gamma(C_Sum+A) )
c      Product ( 1 <= I <= B ) Gamma(C(I)+X(I)) / Gamma(C(I))
c
c    where:
c
c      Comb(A,B,X) is the multinomial coefficient C( A; X(1), X(2), ..., X(B) ),
c      C_Sum = Sum ( 1 <= I <= B ) C(I)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Kenneth Lange,
c    Mathematical and Statistical Methods for Genetic Analysis,
c    Springer, 1997, page 45.
c
c  Parameters:
c
c    Input, integer X(B); X(I) counts the number of occurrences of
c    outcome I, out of the total of A trials.
c
c    Input, integer A, the total number of trials.
c
c    Input, integer B, the number of different possible outcomes
c    on one trial.
c
c    Input, integer C(B); C(I) is the Dirichlet parameter 
c    associated with outcome I.
c
c    Output, double precision PDF, the value of the Dirichlet multinomial PDF.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      double precision c_sum
      integer i
      double precision pdf
      double precision pdf_log
      double precision r8_gamma_log
      double precision r8vec_sum
      integer x(b)

      c_sum = r8vec_sum ( b, c )

      pdf_log = - r8_gamma_log ( c_sum + dble ( a ) ) 
     &  + r8_gamma_log ( c_sum ) 
     &  + r8_gamma_log ( dble ( a + 1 ) )

      do i = 1, b

        pdf_log = pdf_log 
     &    + r8_gamma_log ( c(i) + dble ( x(i) ) ) 
     &    - r8_gamma_log ( c(i) ) 
     &    - r8_gamma_log ( dble ( x(i) + 1 ) )

      end do

      pdf = exp ( pdf_log )

      return
      end
      subroutine dirichlet_sample ( n, a, seed, x )

c*********************************************************************72
c
cc DIRICHLET_SAMPLE samples the Dirichlet PDF.
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
c  Reference:
c
c    Jerry Banks, editor,
c    Handbook of Simulation,
c    Engineering and Management Press Books, 1998, page 169.
c
c  Parameters:
c
c    Input, integer N, the number of components.
c
c    Input, double precision A(N), the probabilities for each component.
c    Each A(I) should be nonnegative, and at least one should be
c    positive.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X(N), a sample of the PDF.  The entries 
c    of X should sum to 1.
c
      implicit none

      integer n

      double precision a(n)
      double precision a2
      double precision b2
      double precision c2
      integer i
      double precision r8vec_sum
      integer seed
      double precision x(n)
      double precision x_sum

      a2 = 0.0D+00
      b2 = 1.0D+00

      do i = 1, n
        c2 = a(i)
        call gamma_sample ( a2, b2, c2, seed, x(i) )
      end do
c
c  Rescale the vector to have unit sum.
c
      x_sum = r8vec_sum ( n, x )

      do i = 1, n
        x(i) = x(i) / x_sum
      end do

      return
      end
      subroutine discrete_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc DISCRETE_CDF_INV inverts the Discrete CDF.
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
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0 <= CDF <= 1.0.
c
c    Input, integer A, the number of probabilities assigned.
c
c    Input, double precision B(A), the relative probabilities of outcomes 
c    1 through A.  Each entry must be nonnegative.
c
c    Output, integer X, the corresponding argument for which
c    CDF(X-1) < CDF <= CDF(X)
c
      implicit none

      integer a

      double precision b(a)
      double precision b_sum
      double precision cdf
      double precision cum
      integer j
      double precision r8vec_sum
      integer x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISCRETE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
        stop
      end if

      b_sum = r8vec_sum ( a, b )

      cum = 0.0D+00

      do j = 1, a

        cum = cum + b(j) / b_sum

        if ( cdf .le. cum ) then
          x = j
          return
        end if

      end do

      x = a

      return
      end
      subroutine discrete_sample ( a, b, seed, x )

c*********************************************************************72
c
cc DISCRETE_SAMPLE samples the Discrete PDF.
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
c  Parameters:
c
c    Input, integer A, the number of probabilities assigned.
c
c    Input, double precision B(A), the relative probabilities of 
c    outcomes 1 through A.  Each entry must be nonnegative.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c 
      implicit none

      integer a

      double precision b(a)
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call discrete_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine event_process ( acid_num, alpha, beta, comp_num, p, 
     &  p_hat, site_num, x_sample )

c*********************************************************************72
c
cc EVENT_PROCESS updates the mixture weight distribution parameters.
c
c  Discussion:
c
c    This routine updates the values of ALPHA.  It does this by
c    considering the results of the most recent event.  If we knew
c    which component PDF had generated the event, then we would 
c    simply add 1 to the ALPHA for that component.  Instead, we
c    use Bayesian analysis to estimate the proportion of the event
c    that is to be added to each ALPHA.
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
c  Reference:
c
c    BS Everitt, DJ Hand,
c    Finite Mixture Distributions,
c    Chapman and Hall, 1981.
c
c    AFM Smith, UE Makov,
c    A Quasi-Bayes Sequential Procedure for Mixtures,
c    Journal of the Royal Statistical Society,
c    Volume 40, Number 1, B, 1978, pages 106-112.
c
c  Parameters:
c
c    Input, integer ACID_NUM, the number of amino acids.
c
c    Input/output, double precision ALPHA(COMP_NUM), the Dirichlet parameters
c    for the weights.
c
c    Input, double precision BETA(ACID_NUM,COMP_MAX); BETA(I,J) is the 
c    multinomial Dirichlet parameter for the J-th acid in the I-th Dirichlet 
c    mixture component.
c
c    Input, integer COMP_NUM, the number of components in the 
c    Dirichlet mixture.
c
c    Input/output, double precision P(COMP_NUM); P(I) is the Bayesian 
c    posterior probability of component I, given the observation of the most 
c    recent event, which is proportional to the probability of the event under 
c    the component I PDF, times the prior probability of component I.
c
c    Input/output, double precision P_HAT(COMP_NUM), the prior probablities 
c    of the components.
c
c    Input, integer SITE_NUM, the number of sites observed for 
c    this event.  This value might change from call to call, although in the 
c    demonstration it is kept fixed.
c
c    Input, integer X_SAMPLE(ACID_NUM), the "current event", 
c    namely, the count vector for the number of occurrences of each acid out 
c    of the total of SITE_NUM sites analyzed.  This is the evidence used to 
c    update the "theory" for the value of ALPHA.
c
      implicit none

      integer acid_num
      integer comp_num

      double precision alpha(comp_num)
      double precision alpha_sum
      double precision beta(acid_num,comp_num)
      integer comp_i
      double precision comp_pdf
      integer i
      double precision p(comp_num)
      double precision p_hat(comp_num)
      double precision p_sum
      double precision r8vec_sum
      integer site_num
      integer x_sample(acid_num)
c
c  Sum the parameters.
c
      alpha_sum = r8vec_sum ( comp_num, alpha )
c
c  Update P_HAT.
c
      do comp_i = 1, comp_num
        p_hat(comp_i) = ( ( alpha_sum - 1.0D+00 ) 
     &    * p_hat(comp_i) + p(comp_i) ) / alpha_sum
      end do
c
c  Generate the new P's.
c  P(COMP_I) = the Bayesian posterior probability of component I,
c  given the observation of event EVENT_I, which is proportional
c  to the probability of event EVENT_I in the component I PDF,
c  times the prior probability of component I.
c
      do comp_i = 1, comp_num

        call dirichlet_multinomial_pdf ( x_sample, site_num, acid_num, 
     &    beta(1,comp_i), comp_pdf )

        p(comp_i) = comp_pdf * p_hat(comp_i)

      end do
c
c  Normalize the P's.
c
      p_sum = r8vec_sum ( comp_num, p )

      if ( p_sum .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EVENT_PROCESS - Fatal error!'
        write ( *, '(a)' ) '  The P''s sum to 0.'
        stop
      end if

      do i = 1, comp_num
        p(i) = p(i) / p_sum
      end do
c
c  Update the alpha's by adding adding appropriate portions of
c  the most recent event to each component's parameter.
c
      do i = 1, comp_num
        alpha(i) = alpha(i) + p(i)
      end do

      return
      end
      subroutine exponential_01_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc EXPONENTIAL_01_CDF_INV inverts the Exponential 01 CDF.
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
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0 <= CDF <= 1.0.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EXPONENTIAL_01_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
        stop
      end if

      x = - log ( 1.0D+00 - cdf )

      return
      end
      subroutine exponential_01_sample ( seed, x )

c*********************************************************************72
c
cc EXPONENTIAL_01_SAMPLE samples the Exponential PDF with parameter 1.
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
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      x = - log ( 1.0D+00 - cdf )

      return
      end
      subroutine exponential_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc EXPONENTIAL_CDF_INV inverts the Exponential CDF.
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
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0 <= CDF <= 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0 < B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EXPONENTIAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
        stop
      end if

      x = a - b * log ( 1.0D+00 - cdf )

      return
      end
      subroutine exponential_sample ( a, b, seed, x )

c*********************************************************************72
c
cc EXPONENTIAL_SAMPLE samples the Exponential PDF.
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
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 < B.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call exponential_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine gamma_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc GAMMA_SAMPLE samples the Gamma PDF.
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
c    Original FORTRAN77 version by Joachim Ahrens, Ulrich Dieter.
c    This FORTRAN77 version by John Burkardt
c
c  Reference:
c
c    Joachim Ahrens, Ulrich Dieter,
c    Generating Gamma Variates by a Modified Rejection Technique,
c    Communications of the ACM, 
c    Volume 25, Number 1, January 1982, pages 47 - 54.
c
c    Joachim Ahrens, Ulrich Dieter,
c    Computer Methods for Sampling from Gamma, Beta, Poisson and
c    Binomial Distributions.
c    Computing, 
c    Volume 12, 1974, pages 223 - 246.
c
c    Joachim Ahrens, KD Kohrt, Ulrich Dieter,
c    Algorithm 599,
c    ACM Transactions on Mathematical Software,
c    Volume 9, Number 2, June 1983, pages 255-257.
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 < B, 
c    0.0D+00 < C.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a1
      double precision a2
      double precision a3
      double precision a4
      double precision a5
      double precision a6
      double precision a7
      double precision b
      double precision bcoef
      double precision c
      double precision co
      double precision d
      double precision e
      double precision e1
      double precision e2
      double precision e3
      double precision e4
      double precision e5
      double precision euler
      double precision p
      double precision q
      double precision q0
      double precision q1
      double precision q2
      double precision q3
      double precision q4
      double precision q5
      double precision q6
      double precision q7
      double precision r
      double precision r8_uniform_01
      double precision s
      integer seed
      double precision si
      double precision s2
      double precision t
      double precision u
      double precision v
      double precision w
      double precision x

      save a1
      save a2
      save a3
      save a4
      save a5
      save a6
      save a7
      save e1
      save e2
      save e3
      save e4
      save e5
      save euler
      save q1
      save q2
      save q3
      save q4
      save q5
      save q6
      save q7

      data a1 /   0.3333333D+00 /
      data a2 / - 0.2500030D+00 /
      data a3 /   0.2000062D+00 /
      data a4 / - 0.1662921D+00 /
      data a5 /   0.1423657D+00 /
      data a6 / - 0.1367177D+00 /
      data a7 /   0.1233795D+00 /
      data e1 / 1.0D+00 /
      data e2 / 0.4999897D+00 /
      data e3 / 0.1668290D+00 /
      data e4 / 0.0407753D+00 /
      data e5 / 0.0102930D+00 /
      data euler / 2.71828182845904D+00 /
      data q1 /   0.04166669D+00 /
      data q2 /   0.02083148D+00 /
      data q3 /   0.00801191D+00 /
      data q4 /   0.00144121D+00 /
      data q5 / - 0.00007388D+00 /
      data q6 /   0.00024511D+00 /
      data q7 /   0.00024240D+00 /
c
c  Allow C = 0.
c
      if ( c .eq. 0.0D+00 ) then
        x = a
        return
      end if
c
c  C < 1.
c
      if ( c .lt. 1.0D+00 ) then

10      continue

          u = r8_uniform_01 ( seed )
          t = 1.0D+00 + c / euler
          p = u * t

          call exponential_01_sample ( seed, s )

          if ( p .lt. 1.0D+00 ) then
            x = exp ( log ( p ) / c )
            if ( x .le. s ) then
              go to 20
            end if
          else
            x = - log ( ( t - p ) / c )
            if ( ( 1.0D+00 - c ) * log ( x ) .le. s ) then
              go to 20
            end if
          end if

        go to 10

20      continue

        x = a + b * x
        return
c
c  1 <= C.
c
      else

        s2 = c - 0.5D+00
        s = sqrt ( c - 0.5D+00 )
        d = sqrt ( 32.0D+00 ) - 12.0D+00 * sqrt ( c - 0.5D+00 )

        call normal_01_sample ( seed, t )
        x = ( sqrt ( c - 0.5D+00 ) + 0.5D+00 * t )**2

        if ( 0.0D+00 .le. t ) then
          x = a + b * x
          return
        end if

        u = r8_uniform_01 ( seed )

        if ( d * u .le. t**3 ) then
          x = a + b * x
          return
        end if

        r = 1.0D+00 / c

        q0 = ( ( ( ( ( ( 
     &         q7   * r 
     &       + q6 ) * r 
     &       + q5 ) * r 
     &       + q4 ) * r 
     &       + q3 ) * r 
     &       + q2 ) * r 
     &       + q1 ) * r

        if ( c .le. 3.686D+00 ) then
          bcoef = 0.463D+00 + s - 0.178D+00 * s2
          si = 1.235D+00
          co = 0.195D+00 / s - 0.079D+00 + 0.016D+00 * s
        else if ( c .le. 13.022D+00 ) then
          bcoef = 1.654D+00 + 0.0076D+00 * s2
          si = 1.68D+00 / s + 0.275D+00
          co = 0.062D+00 / s + 0.024D+00
        else
          bcoef = 1.77D+00
          si = 0.75D+00
          co = 0.1515D+00 / s
        end if

        if ( 0.0D+00 .lt. sqrt ( c - 0.5D+00 ) + 0.5D+00 * t ) then

          v = 0.5D+00 * t / s

          if ( 0.25D+00 .lt. abs ( v ) ) then
            q = q0 - s * t + 0.25D+00 * t * t 
     &        + 2.0D+00 * s2 * log ( 1.0D+00 + v )
          else
            q = q0 + 0.5D+00 * t**2 * ( ( ( ( ( ( 
     &             a7   * v 
     &           + a6 ) * v
     &           + a5 ) * v 
     &           + a4 ) * v 
     &           + a3 ) * v 
     &           + a2 ) * v 
     &           + a1 ) * v
          end if

          if ( log ( 1.0D+00 - u ) .le. q ) then
            x = a + b * x
            return
          end if

        end if

30      continue

          call exponential_01_sample ( seed, e )

          u = r8_uniform_01 ( seed )

          u = 2.0D+00 * u - 1.0D+00
          t = bcoef + sign ( si * e, u )

          if ( -0.7187449D+00 .le. t ) then

            v = 0.5D+00 * t / s

            if ( 0.25D+00 .lt. abs ( v ) ) then
              q = q0 - s * t + 0.25D+00 * t**2 
     &          + 2.0D+00 * s2 * log ( 1.0D+00 + v )
            else
              q = q0 + 0.5D+00 * t**2 * ( ( ( ( ( ( 
     &             a7   * v 
     &           + a6 ) * v 
     &           + a5 ) * v 
     &           + a4 ) * v 
     &           + a3 ) * v 
     &           + a2 ) * v 
     &           + a1 ) * v
            end if

            if ( 0.0D+00 .lt. q ) then

              if ( 0.5D+00 .lt. q ) then
                w = exp ( q ) - 1.0D+00
              else
                w = ( ( ( ( 
     &                  e5   * q 
     &                + e4 ) * q 
     &                + e3 ) * q 
     &                + e2 ) * q 
     &                + e1 ) * q
              end if

              if ( co * abs ( u ) .le. 
     &          w * exp ( e - 0.5D+00 * t**2 ) ) then
                x = a + b * ( s + 0.5D+00 * t )**2
                return
              end if

            end if

          end if

        go to 30

      end if

      return
      end
      subroutine i4_next ( line, ival, done )

c*********************************************************************72
c
cc I4_NEXT "reads" integers from a string, one at a time.
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
c  Parameters:
c
c    Input, character * ( * ) LINE, a string, presumably containing
c    integers.  These may be separated by spaces or commas.
c
c    Output, integer IVAL.  If DONE is FALSE, then IVAL contains
c    the "next" integer read from LINE.  If DONE is TRUE, then
c    IVAL is zero.
c
c    Input/output, logical DONE.
c    On input with a fresh value of LINE, the user should set
c    DONE to TRUE.
c    On output, the routine sets DONE to FALSE if another integer
c    was read, or TRUE if no more integers could be read.
c
      implicit none

      logical done
      integer ierror
      integer ival
      integer lchar
      character * ( * ) line
      integer next

      save next

      data next / 1 /

      ival = 0

      if ( done ) then
        next = 1
        done = .false.
      end if

      if ( len ( line ) .lt. next ) then
        done = .true.
        return
      end if

      call s_to_i4 ( line(next:), ival, ierror, lchar )

      if ( ierror .ne. 0 .or. lchar .eq. 0 ) then
        done = .true.
        next = 1
      else
        done = .false.
        next = next + lchar
      end if

      return
      end
      subroutine mixture_read ( acid_num, acid_sym, beta, beta_sum, 
     &  comp_label, comp_max, comp_num, comp_weight, ierror, iunit )

c*********************************************************************72
c
cc MIXTURE_READ reads the Dirichlet mixture parameters from a file.
c
c  Discussion:
c
c    The data in the file is delimited by keywords.
c
c    The first lines (not necessarily in orderc) may include
c
c      ClassName = string
c      NumDistr = N           the number of components in the mixture.
c      Alphabet = string
c      Order = A C D E ...    the order of the amino acids.
c      AlphaChar = 20
c      NumDistr = 9           the number of distributions
c      EndClassName = string
c
c    For each component, there are four lines:
c
c      Number= N              the component number, starting with 0
c      Mixture= N             the mixture weight, out of a total of 1.0
c      Alpha=  |A| A1 A2 ...  the parameter sum, and individual parameters
c      Comment=               a comment, which describes the frequencies.
c
c    In the comment, the symbol "><" indicates the mean background frequency;
c    residues to the left of that symbol occur more frequently
c    than background, residues to the right less frequently.  Commas separate
c    residues differing in frequency by a factor of 2.
c
c    For example, the comment
c      S A T , C G P >< N V M , Q H R I K F L D W , E Y
c    indicates that for this component, the frequency of
c    proline is just above the mean, and serine, alanine and
c    threonine are twice as frequent in this component than they
c    are on average.  By contrast, tyrosine and glutamic acid are
c    between 4 and 8 times less likely in this component than on
c    average.
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
c  Parameters:
c
c    Input, integer ACID_NUM, the number of amino acids.
c
c    Output, character ACID_SYM(ACID_NUM), the one letter amino acid codes.
c
c    Output, double precision BETA(ACID_NUM,COMP_MAX); BETA(I,J) is the 
c    parameter for the J-th acid in the I-th Dirichlet mixture component.
c
c    Output, double precision BETA_SUM(COMP_MAX), the sum of the values of
c    BETA(ACID_I,COMP_I) for a given component COMP_I.
c
c    Output, integer COMP_LABEL(COMP_NUM), the label of each 
c    component.  Normally, component I has label I.
c
c    Input, integer COMP_MAX, the maximum number of Dirichlet 
c    mixture components.
c
c    Output, integer COMP_NUM, the number of components in the 
c    Dirichlet mixture.
c
c    Output, double precision COMP_WEIGHT(COMP_NUM), the mixture weight of 
c    each component.  These values should be nonnegative, and sum to 1.  
c    They represent the relative proportion of each component in the mixture.
c
c    Output, integer IERROR, error indicator.
c    0: no error occurred; nonzero: an error occurred.
c
c    Input, integer IUNIT, the FORTRAN unit from which the 
c    data is to be read.
c
      implicit none

      integer acid_num
      integer comp_max

      integer acid_i
      character acid_sym(acid_num)
      double precision beta(acid_num,comp_max)
      double precision beta_sum(comp_max)
      integer comp_i
      integer comp_label(comp_max)
      integer comp_num
      double precision comp_weight(comp_max)
      logical done
      integer iequal
      integer ierror
      integer iunit
      integer ngoofy
      integer nrec
      logical s_begin
      character * ( 500 ) string
      integer string_len

      ierror = 0
      comp_i = 0
      comp_num = 0
      nrec = 0
      ngoofy = 0

10    continue

      read ( iunit, '(a)', end = 20 ) string
      string_len = len_trim ( string )
      nrec = nrec + 1
c
c  Ignore blank lines.
c
      if ( string == ' ' ) then
c
c  Ignore the CLASSNAME field.
c
      else if ( s_begin ( string, 'CLASSNAME' ) ) then
c
c  Ignore the ENDCLASSNAME field.
c
      else if ( s_begin ( string, 'ENDCLASSNAME' ) ) then
c
c  Ignore the NAME field.
c
      else if ( s_begin ( string, 'NAME' ) ) then
c
c  Ignore the ALPHABET field.
c
      else if ( s_begin ( string, 'ALPHABET' ) ) then
c
c  Read the ORDER field, since it tells us how to interpret the ALPHA's.
c
      else if ( s_begin ( string, 'ORDER' ) ) then

        iequal = index ( string, '=' )
        done = .true.
        do acid_i = 1, acid_num
          call ch_next ( string(iequal+1:), acid_sym(acid_i), done )
        end do
c
c  Ignore the ALPHACHAR field.
c
      else if ( s_begin ( string, 'ALPHACHAR' ) ) then
c
c  Read the NUMDISTR field.
c
      else if ( s_begin ( string, 'NUMDISTR' ) ) then

        iequal = index ( string, '=' )
        done = .true.
        call i4_next ( string(iequal+1:), comp_num, done )

        if ( comp_num .lt. 1 ) then
          ierror = 1
          return
        else if ( comp_max .lt. comp_num ) then
          ierror = 2
          return
        end if
c
c  Read the NUMBER field.
c
      else if ( s_begin ( string, 'NUMBER' ) ) then

        comp_i = comp_i + 1

        if ( comp_num .lt. comp_i ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MIXTURE_READ - Fatal error!'
          write ( *, '(a,i6)' ) '  Number of components = ', comp_i
          write ( *, '(a,i6)' ) 
     &      '  exceeding reported value of ', comp_num
          stop
        end if

        iequal = index ( string, '=' )
        done = .true.
        call i4_next ( string(iequal+1:), comp_label(comp_i), done )
c
c  Read the MIXTURE field.
c
      else if ( s_begin ( string, 'MIXTURE' ) ) then

        iequal = index ( string, '=' )
 
        done = .true.
        call r8_next ( string(iequal+1:string_len), 
     &    comp_weight(comp_i), done )
c
c  Read the ALPHA field.
c
      else if ( s_begin ( string, 'ALPHA' ) ) then

        iequal = index ( string, '=' )
        done = .true.
        call r8_next ( string(iequal+1:), beta_sum(comp_i), done )
        do acid_i = 1, acid_num
          call r8_next ( string(iequal+1:), beta(acid_i,comp_i), done )
        end do
c
c  Ignore the COMMENT field.
c
      else if ( s_begin ( string, 'COMMENT' ) ) then
c
c  Unexpected field:
c
      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MIXTURE_READ - Warningc'
        write ( *, '(a)' ) '  Goofy record: '
        write ( *, '(a)' ) trim ( string )

        ngoofy = ngoofy + 1

      end if

      go to 10

20    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MIXTURE_READ - Note:'
      write ( *, '(a,i6)' ) '  Number of records read was ', nrec
      write ( *, '(a,i6)' ) '  Number of goofy records was ', ngoofy

      return
      end
      subroutine multinomial_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc MULTINOMIAL_SAMPLE samples the Multinomial PDF.
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
c  Reference:
c
c    Luc Devroye,
c    Non-Uniform Random Variate Generation,
c    Springer-Verlag, New York, 1986, page 559.
c
c  Parameters:
c
c    Input, integer A, the total number of trials.
c    0 <= A.
c
c    Input, integer B, the number of outcomes possible on
c    one trial.  1 <= B.
c
c    Input, double precision C(B).  C(I) is the probability of outcome I on
c    any trial.
c    0.0D+00 <= C(I) <= 1.0D+00,
c    sum ( 1 <= I <= B) C(I) = 1.0.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, integer X(B); X(I) is the number of
c    occurrences of event I during the N trials.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      integer ifactor
      integer ntot
      double precision prob
      integer seed
      double precision sum2
      integer x(b)

      ntot = a

      sum2 = 1.0D+00

      do ifactor = 1, b
        x(ifactor) = 0
      end do

      do ifactor = 1, b - 1

        prob = c(ifactor) / sum2
c
c  Generate a binomial random deviate for NTOT trials with 
c  single trial success probability PROB.
c
        call binomial_sample ( ntot, prob, seed, x(ifactor) )

        ntot = ntot - x(ifactor)
        if ( ntot .le. 0 ) then
          return
        end if

        sum2 = sum2 - c(ifactor)

      end do
c
c  The last factor gets what's left.
c
      x(b) = ntot

      return
      end
      subroutine normal_01_sample ( seed, x )

c*********************************************************************72
c
cc NORMAL_01_SAMPLE samples the standard normal probability distribution.
c
c  Discussion:
c
c    The standard normal probability distribution function (PDF) has 
c    mean 0 and standard deviation 1.
c
c    The Box-Muller method is used, which is efficient, but 
c    generates two values at a time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number 
c    generator.
c
c    Output, double precision X, a sample of the standard normal PDF.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r1
      double precision r2
      double precision r8_uniform_01
      integer seed
      integer used
      double precision x
      double precision y

      save used
      save y

      data used / -1 /
      data y / 0.0D+00 /

      if ( used .eq. -1 ) then
        used = 0
      end if
!
!  If we've used an even number of values so far, generate two more,
!  return one and save one.
!
      if ( mod ( used, 2 ) .eq. 0 ) then

10      continue

          r1 = r8_uniform_01 ( seed )

          if ( r1 .ne. 0.0D+00 ) then
            go to 20
          end if

        go to 10

20      continue

        r2 = r8_uniform_01 ( seed )

        x = sqrt ( -2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )
        y = sqrt ( -2.0D+00 * log ( r1 ) ) * sin ( 2.0D+00 * pi * r2 )
!
!  Otherwise, return the second, saved, value.
!
      else

        x = y

      end if

      used = used + 1

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      function r8_gamma_log ( x )

c*********************************************************************72
c
cc R8_GAMMA_LOG evaluates the logarithm of the gamma function.
c
c  Discussion:
c
c    This routine calculates the LOG(GAMMA) function for a positive real
c    argument X.  Computation is based on an algorithm outlined in
c    references 1 and 2.  The program uses rational functions that
c    theoretically approximate LOG(GAMMA) to at least 18 significant
c    decimal digits.  The approximation for X > 12 is from reference
c    3, while approximations for X < 12.0 are similar to those in
c    reference 1, but are unpublished.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody, Kenneth Hillstrom,
c    Chebyshev Approximations for the Natural Logarithm of the
c    Gamma Function,
c    Mathematics of Computation,
c    Volume 21, Number 98, April 1967, pages 198-203.
c
c    Kenneth Hillstrom,
c    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
c    May 1969.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
c    Charles Mesztenyi, John Rice, Henry Thatcher,
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision R8_GAMMA_LOG, the value of the function.
c
      implicit none

      double precision c(7)
      double precision corr
      double precision d1 
      parameter ( d1 = -5.772156649015328605195174D-01 )
      double precision d2
      parameter ( d2 = 4.227843350984671393993777D-01 )
      double precision d4
      parameter ( d4 = 1.791759469228055000094023D+00 )
      double precision frtbig
      parameter ( frtbig = 2.25D+76 )
      integer i
      double precision p1(8)
      double precision p2(8)
      double precision p4(8)
      double precision q1(8)
      double precision q2(8)
      double precision q4(8)
      double precision r8_epsilon
      double precision r8_gamma_log
      double precision res
      double precision sqrtpi 
      parameter ( sqrtpi = 0.9189385332046727417803297D+00 )
      double precision x
      double precision xbig
      parameter ( xbig = 2.55D+305 )
      double precision xden
      double precision xinf
      parameter ( xinf = 1.79D+308 )
      double precision xm1
      double precision xm2
      double precision xm4
      double precision xnum
      double precision y
      double precision ysq

      save c
      save p1
      save p2
      save p4
      save q1
      save q2
      save q4

      data c /
     &  -1.910444077728D-03, 
     &   8.4171387781295D-04, 
     &  -5.952379913043012D-04, 
     &   7.93650793500350248D-04, 
     &  -2.777777777777681622553D-03, 
     &   8.333333333333333331554247D-02, 
     &   5.7083835261D-03 /
      data p1 /
     &  4.945235359296727046734888D+00, 
     &  2.018112620856775083915565D+02, 
     &  2.290838373831346393026739D+03, 
     &  1.131967205903380828685045D+04, 
     &  2.855724635671635335736389D+04, 
     &  3.848496228443793359990269D+04, 
     &  2.637748787624195437963534D+04, 
     &  7.225813979700288197698961D+03 /
      data p2 /
     &  4.974607845568932035012064D+00, 
     &  5.424138599891070494101986D+02, 
     &  1.550693864978364947665077D+04, 
     &  1.847932904445632425417223D+05, 
     &  1.088204769468828767498470D+06, 
     &  3.338152967987029735917223D+06, 
     &  5.106661678927352456275255D+06, 
     &  3.074109054850539556250927D+06 /
      data p4 /
     &  1.474502166059939948905062D+04, 
     &  2.426813369486704502836312D+06, 
     &  1.214755574045093227939592D+08, 
     &  2.663432449630976949898078D+09, 
     &  2.940378956634553899906876D+10, 
     &  1.702665737765398868392998D+11, 
     &  4.926125793377430887588120D+11, 
     &  5.606251856223951465078242D+11 /
      data q1 /
     &  6.748212550303777196073036D+01, 
     &  1.113332393857199323513008D+03, 
     &  7.738757056935398733233834D+03, 
     &  2.763987074403340708898585D+04, 
     &  5.499310206226157329794414D+04, 
     &  6.161122180066002127833352D+04, 
     &  3.635127591501940507276287D+04, 
     &  8.785536302431013170870835D+03 /
      data q2 /
     &  1.830328399370592604055942D+02, 
     &  7.765049321445005871323047D+03, 
     &  1.331903827966074194402448D+05, 
     &  1.136705821321969608938755D+06, 
     &  5.267964117437946917577538D+06, 
     &  1.346701454311101692290052D+07, 
     &  1.782736530353274213975932D+07, 
     &  9.533095591844353613395747D+06 /
      data q4 /
     &  2.690530175870899333379843D+03, 
     &  6.393885654300092398984238D+05, 
     &  4.135599930241388052042842D+07, 
     &  1.120872109616147941376570D+09, 
     &  1.488613728678813811542398D+10, 
     &  1.016803586272438228077304D+11, 
     &  3.417476345507377132798597D+11, 
     &  4.463158187419713286462081D+11 /

      y = x

      if ( 0.0D+00 .lt. y .and. y .le. xbig ) then

        if ( y .le. r8_epsilon ( ) ) then

          res = - log ( y )
c
c  EPS < X <= 1.5.
c
        else if ( y .le. 1.5D+00 ) then

          if ( y .lt. 0.6796875D+00 ) then
            corr = -log ( y )
            xm1 = y
          else
            corr = 0.0D+00
            xm1 = ( y - 0.5D+00 ) - 0.5D+00
          end if

          if ( y .le. 0.5D+00 .or. 0.6796875D+00 .le. y ) then

            xden = 1.0D+00
            xnum = 0.0D+00
            do i = 1, 8
              xnum = xnum * xm1 + p1(i)
              xden = xden * xm1 + q1(i)
            end do

            res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

          else

            xm2 = ( y - 0.5D+00 ) - 0.5D+00
            xden = 1.0D+00
            xnum = 0.0D+00
            do i = 1, 8
              xnum = xnum * xm2 + p2(i)
              xden = xden * xm2 + q2(i)
            end do

            res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

          end if
c
c  1.5 < X <= 4.0.
c
        else if ( y .le. 4.0D+00 ) then

          xm2 = y - 2.0D+00
          xden = 1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm2 + p2(i)
            xden = xden * xm2 + q2(i)
          end do

          res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
c
c  4.0 < X <= 12.0.
c
        else if ( y .le. 12.0D+00 ) then

          xm4 = y - 4.0D+00
          xden = -1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm4 + p4(i)
            xden = xden * xm4 + q4(i)
          end do

          res = d4 + xm4 * ( xnum / xden )
c
c  Evaluate for 12 <= argument.
c
        else

          res = 0.0D+00

          if ( y .le. frtbig ) then

            res = c(7)
            ysq = y * y

            do i = 1, 6
              res = res / ysq + c(i)
            end do

          end if

          res = res / y
          corr = log ( y )
          res = res + sqrtpi - 0.5D+00 * corr
          res = res + y * ( corr - 1.0D+00 )

        end if
c
c  Return for bad arguments.
c
      else

        res = xinf

      end if
c
c  Final adjustments and return.
c
      r8_gamma_log = res

      return
      end
      subroutine r8_next ( line, rval, done )

c*********************************************************************72
c
cc R8_NEXT "reads" real numbers from a string, one at a time.
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
c  Parameters:
c
c    Input, character * ( * ) LINE, a string, presumably containing real
c    numbers.  These may be separated by spaces or commas.
c
c    Output, double precision RVAL.  If DONE is FALSE, then RVAL contains the
c    "next" real value read from LINE.  If DONE is TRUE, then
c    RVAL is zero.
c
c    Input/output, logical DONE.
c    On input with a fresh value of LINE, the user should set
c    DONE to TRUE.
c    On output, the routine sets DONE to FALSE if another real
c    value was read, or TRUE if no more reals could be read.
c
      implicit none

      logical done
      integer ierror
      integer lchar
      character * ( * ) line
      integer next
      double precision rval

      save next

      data next / 1 /

      rval = 0.0D+00

      if ( done ) then
        next = 1
        done = .false.
      end if

      if ( len ( line ) .lt. next ) then
        done = .true.
        return
      end if

      call s_to_r8_old ( line(next:), rval, ierror, lchar )

      if ( ierror .ne. 0 .or. lchar .eq. 0 ) then
        done = .true.
        next = 1
      else
        done = .false.
        next = next + lchar
      end if

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

      return
      end
      function s_begin ( s1, s2 )

c*********************************************************************72
c
cc S_BEGIN is TRUE if one string matches the beginning of the other.
c
c  Discussion:
c
c    The strings are compared, ignoring blanks, spaces and capitalization.
c
c  Example:
c
c     S1              S2      S_BEGIN
c
c    'Bob'          'BOB'     TRUE
c    '  B  o b '    ' bo b'   TRUE
c    'Bob'          'Bobby'   TRUE
c    'Bobo'         'Bobb'    FALSE
c    ' '            'Bob'     FALSE    (Do not allow a blank to match
c                                       anything but another blank string.)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 November 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( ) S1, S2, the strings to be compared.
c
c    Output, logical S_BEGIN, is TRUE if the strings match up to
c    the end of the shorter string, ignoring case.
c
      implicit none

      logical ch_eqi
      integer i1
      integer i2
      logical s_begin
      character * ( * )  s1
      integer s1_length
      character * ( * )  s2
      integer s2_length

      s1_length = len_trim ( s1 )
      s2_length = len_trim ( s2 )
c
c  If either string is blank, then both must be blank to match.
c  Otherwise, a blank string matches anything, which is not
c  what most people want.
c
      if ( s1_length .eq. 0 .or. s2_length .eq. 0 ) then

        if ( s1_length .eq. 0 .and. s2_length .eq. 0 ) then
          s_begin = .true.
        else
          s_begin = .false.
        end if

        return

      end if

      i1 = 0
      i2 = 0
c
c  Find the next nonblank in S1.
c
10    continue

20      continue

          i1 = i1 + 1

          if ( s1_length .lt. i1 ) then
            s_begin = .true.
            return
          end if

          if ( s1(i1:i1) .ne. ' ' ) then
            go to 30
          end if

        go to 20

30      continue
c
c  Find the next nonblank in S2.
c
40      continue

          i2 = i2 + 1

          if ( s2_length .lt. i2 ) then
            s_begin = .true.
            return
          end if

          if ( s2(i2:i2) .ne. ' ' ) then
            go to 50
          end if

        go to 40

50      continue
c
c  If the characters match, get the next pair.
c
        if ( .not. ch_eqi ( s1(i1:i1), s2(i2:i2) ) ) then
          go to 60
        end if

      go to 10

60    continue

      s_begin = .false.

      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

      return
      end
      subroutine s_to_i4 ( s, ival, ierror, length )

c*********************************************************************72
c
cc S_TO_I4 reads an I4 from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, a string to be examined.
c
c    Output, integer IVAL, the integer value read from the string.
c    If the string is blank, then IVAL will be returned 0.
c
c    Output, integer IERROR, an error flag.
c    0, no error.
c    1, an error occurred.
c
c    Output, integer LENGTH, the number of characters of S
c    used to make IVAL.
c
      implicit none

      character c
      integer i
      integer ierror
      integer isgn
      integer istate
      integer ival
      integer length
      character * ( * ) s
      integer s_len_trim

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, s_len_trim ( s )

        c = s(i:i)
c
c  Haven't read anything.
c
        if ( istate .eq. 0 ) then

          if ( c .eq. ' ' ) then

          else if ( c .eq. '-' ) then
            istate = 1
            isgn = -1
          else if ( c .eq. '+' ) then
            istate = 1
            isgn = + 1
          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read the sign, expecting digits.
c
        else if ( istate .eq. 1 ) then

          if ( c .eq. ' ' ) then

          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read at least one digit, expecting more.
c
        else if ( istate .eq. 2 ) then

          if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            ival = 10 * ival + ichar ( c ) - ichar ( '0' )
          else
            ival = isgn * ival
            length = i - 1
            return
          end if

        end if

      end do
c
c  If we read all the characters in the string, see if we're OK.
c
      if ( istate .eq. 2 ) then
        ival = isgn * ival
        length = s_len_trim ( s )
      else
        ierror = 1
        length = 0
      end if

      return
      end
      subroutine s_to_r8 ( s, r8 )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 value from a string.
c
c  Discussion:
c
c    An "R8" value is simply a real number to be stored as a
c    variable of type "double precision".
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 R8
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision R8, the value read from the string.
c
      implicit none

      character c
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer ndig
      double precision r8
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_length
      character TAB
      parameter ( TAB = char ( 9 ) )

      s_length = len_trim ( s )

      ierror = 0
      r8 = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( s_length .lt. length + 1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' .or. c .eq. TAB ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 < ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( c .eq. 'E' .or. c .eq. 'e' .or. 
     &            c .eq. 'D' .or. c .eq. 'd' ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if (  ihave .lt. 11 .and. lle ( '0', c ) 
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          ndig = ichar ( c ) - 48

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

      go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to S_LENGTH.
c
      if ( iterm .ne. 1 .and. length + 1 .eq. s_length ) then
        length = s_length
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7c
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or. ihave .eq. 6 .or. 
     &  ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious error!'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a)' ) '    ' // trim ( s )
        stop
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      r8 = dble ( isgn ) * rexp * rtop / rbot

      return
      end
      subroutine s_to_r8_old ( s, dval, ierror, length )

c*********************************************************************72
c
cc S_TO_R8_OLD reads an R8 from a string.
c
c  Discussion:
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 DVAL
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision DVAL, the value read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    1, 2, 6 or 7, the input number was garbled.  The
c    value of IERROR is the last type of input successfully
c    read.  For instance, 1 means initial blanks, 2 means
c    a plus or minus sign, and so on.
c
c    Output, integer LENGTH, the number of characters read
c    to form the number, including any terminating
c    characters such as a trailing comma or blanks.
c
      implicit none

      logical ch_eqi
      character c
      double precision dval
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer nchar
      integer ndig
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_len_trim

      nchar = s_len_trim ( s )

      ierror = 0
      dval = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( nchar .lt. length+1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 .lt. ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if ( ihave .lt. 11 .and. lle ( '0', c )
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          call ch_to_digit ( c, ndig )

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

        go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to NCHAR.
c
      if ( iterm .ne. 1 .and. length+1 .eq. nchar ) then
        length = nchar
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7.
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or.
     &     ihave .eq. 6 .or. ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8_OLD - Serious error!'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a,a)' ) '    ', s
        return
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      dval = dble ( isgn ) * rexp * rtop / rbot

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
