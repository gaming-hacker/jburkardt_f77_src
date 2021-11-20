      subroutine angle_cdf ( x, n, cdf )

c*********************************************************************72
c
cc ANGLE_CDF evaluates the Angle CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation and Sensitivity of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, integer N, the spatial dimension.
c    N must be at least 2.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      integer n
      double precision n_real
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_gamma
      double precision sin_power_int
      double precision x
      double precision zero
      parameter ( zero = 0.0D+00 )

      if ( n .lt. 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ANGLE_CDF - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 2.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop
      end if

      if ( x .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else if ( pi .le. x ) then
        cdf = 1.0D+00
      else if ( n .eq. 2 ) then
        cdf = x / pi
      else
        n_real = dble ( n )
        cdf = sin_power_int ( zero, x, n - 2 ) * r8_gamma ( n_real / 2.0
     &D+00 )       / ( sqrt ( pi ) * r8_gamma ( ( n_real - 1.0D+00 ) / 2
     &.0D+00 ) )
      end if

      return
      end
      subroutine angle_mean ( n, mean )

c*********************************************************************72
c
cc ANGLE_MEAN returns the mean of the Angle PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the spatial dimension.
c    N must be at least 2.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision mean
      integer n
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      mean = pi / 2.0D+00

      return
      end
      subroutine angle_pdf ( x, n, pdf )

c*********************************************************************72
c
cc ANGLE_PDF evaluates the Angle PDF.
c
c  Discussion:
c
c    X is an angle between 0 and PI, corresponding to the angle
c    made in an N-dimensional space, between a fixed line passing
c    through the origin, and an arbitrary line that also passes
c    through the origin, which is specified by a choosing any point
c    on the N-dimensional sphere with uniform probability.
c
c    The formula is
c
c      PDF(X) = ( sin ( X ) )^(N-2) * Gamma ( N / 2 )
c               / ( sqrt ( PI ) * Gamma ( ( N - 1 ) / 2 ) )
c
c      PDF(X) = 1 / PI if N = 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation and Sensitivity of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, integer N, the spatial dimension.
c    N must be at least 2.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer n
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_gamma
      double precision x

      if ( n .lt. 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ANGLE_PDF - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 2.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop
      end if

      if ( x .lt. 0.0D+00 .or. pi .lt. x ) then
        pdf = 0.0D+00
      else if ( n .eq. 2 ) then
        pdf = 1.0D+00 / pi
      else
        pdf = ( sin ( x ) )**( n - 2 ) 
     &    * r8_gamma ( dble ( n ) / 2.0D+00 ) / ( sqrt ( pi ) 
     &    * r8_gamma ( dble ( n - 1 ) / 2.0D+00 ) )
      end if

      return
      end
      subroutine anglit_cdf ( x, cdf )

c*********************************************************************72
c
cc ANGLIT_CDF evaluates the Anglit CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .lt.  - 0.25D+00 * pi ) then
        cdf = 0.0D+00
      else if ( x .lt. 0.25D+00 * pi ) then
        cdf = 0.5D+00 - 0.5D+00 * cos ( 2.0D+00 * x + pi / 2.0D+00 )
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine anglit_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc ANGLIT_CDF_INV inverts the Anglit CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ANGLIT_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = 0.5D+00 * ( acos ( 1.0D+00 - 2.0D+00 * cdf ) - pi / 2.0D+00 )

      return
      end
      subroutine anglit_mean ( mean )

c*********************************************************************72
c
cc ANGLIT_MEAN returns the mean of the Anglit PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision mean

      mean = 0.0D+00

      return
      end
      subroutine anglit_pdf ( x, pdf )

c*********************************************************************72
c
cc ANGLIT_PDF evaluates the Anglit PDF.
c
c  Discussion:
c
c    PDF(X) = sin ( 2 * X + PI / 2 ) for -PI/4 .le. X .le. PI/4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .le. - 0.25D+00 * pi .or. 0.25D+00 * pi .le. x ) then
        pdf = 0.0D+00
      else
        pdf = sin ( 2.0D+00 * x + 0.25D+00 * pi )
      end if

      return
      end
      subroutine anglit_sample ( seed, x )

c*********************************************************************72
c
cc ANGLIT_SAMPLE samples the Anglit PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 December 1999
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

      call anglit_cdf_inv ( cdf, x )

      return
      end
      subroutine anglit_variance ( variance )

c*********************************************************************72
c
cc ANGLIT_VARIANCE returns the variance of the Anglit PDF.
c
c  Discussion:
c
c    Variance =
c      Integral ( -PI/4 .le. X .le. PI/4 ) X^2 * sin ( 2 * X + PI / 2 )
c
c    Antiderivative =
c      0.5D+00 * X * sin ( 2 * X + PI / 2 )
c      + ( 0.25 - 0.5D+00 * X^2 ) * cos ( 2 * X + PI / 2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = 0.0625D+00 * pi * pi - 0.5D+00

      return
      end
      subroutine arcsin_cdf ( x, a, cdf )

c*********************************************************************72
c
cc ARCSIN_CDF evaluates the Arcsin CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, the parameter of the CDF.
c    A must be positive.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .le. -a ) then
        cdf = 0.0D+00
      else if ( x .lt. a ) then
        cdf = 0.5D+00 + asin ( x / a ) / pi
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine arcsin_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc ARCSIN_CDF_INV inverts the Arcsin CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, the parameter of the CDF.
c    A must be positive.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ARCSIN_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a * sin ( pi * ( cdf - 0.5D+00 ) )

      return
      end
      function arcsin_check ( a )

c*********************************************************************72
c
cc ARCSIN_CHECK checks the parameter of the Arcsin CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0 .lt. A.
c
c    Output, logical ARCSIN_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical arcsin_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ARCSIN_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        arcsin_check = .false.
        return
      end if

      arcsin_check = .true.

      return
      end
      subroutine arcsin_mean ( a, mean )

c*********************************************************************72
c
cc ARCSIN_MEAN returns the mean of the Arcsin PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the CDF.
c    A must be positive.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision mean

      mean = 0.0D+00

      return
      end
      subroutine arcsin_pdf ( x, a, pdf )

c*********************************************************************72
c
cc ARCSIN_PDF evaluates the Arcsin PDF.
c
c  Discussion:
c
c    The LOGISTIC EQUATION has the form:
c
c      X(N+1) = 4 * LAMBDA * ( 1 - X(N) ).
c
c    where 0 .lt. LAMBDA .le. 1.  This nonlinear difference equation maps
c    the unit interval into itself, and is a simple example of a system
c    exhibiting chaotic behavior.  Ulam and von Neumann studied the
c    logistic equation with LAMBDA = 1, and showed that iterates of the
c    function generated a sequence of pseudorandom numbers with
c    the Arcsin probability density function.
c
c    The derived sequence
c
c      Y(N) = ( 2 / PI ) * Arcsin ( SQRT ( X(N) ) )
c
c    is a pseudorandom sequence with the uniform probability density
c    function on [0,1].  For certain starting values, such as X(0) = 0, 0.75,
c    or 1.0D+00, the sequence degenerates into a constant sequence, and for
c    values very near these, the sequence takes a while before becoming
c    chaotic.
c
c    The formula is:
c
c      PDF(X) = 1 / ( pi * sqrt ( A^2 - X^2 ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Daniel Zwillinger, Stephen Kokoska,
c    CRC Standard Probability and Statistics Tables and Formulae,
c    Chapman and Hall/CRC, 2000, pages 114-115.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    -A .lt. X .lt. A.
c
c    Input, double precision A, the parameter of the CDF.
c    A must be positive.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ARCSIN_PDF - Fatal error!'
        write ( *, '(a)' ) '  Parameter A must be positive.'
        stop
      end if

      if ( x .le. -a .or. a .le. x ) then
        pdf = 0.0D+00
      else
        pdf = 1.0D+00 / ( pi * sqrt ( a * a - x * x ) )
      end if

      return
      end
      subroutine arcsin_sample ( a, seed, x )

c*********************************************************************72
c
cc ARCSIN_SAMPLE samples the Arcsin PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the CDF.
c    A must be positive.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call arcsin_cdf_inv ( cdf, a, x )

      return
      end
      subroutine arcsin_variance ( a, variance )

c*********************************************************************72
c
cc ARCSIN_VARIANCE returns the variance of the Arcsin PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the CDF.
c    A must be positive.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision variance

      variance = a * a / 2.0D+00

      return
      end
      subroutine benford_pdf ( x, pdf )

c*********************************************************************72
c
cc BENFORD_PDF returns the Benford PDF.
c
c  Discussion:
c
c    Benford's law is an empirical formula explaining the observed
c    distribution of initial digits in lists culled from newspapers,
c    tax forms, stock market prices, and so on.  It predicts the observed
c    high frequency of the initial digit 1, for instance.
c
c    Note that the probabilities of digits 1 through 9 are guaranteed
c    to add up to 1, since
c      LOG10 ( 2/1 ) + LOG10 ( 3/2) + LOG10 ( 4/3 ) + ... + LOG10 ( 10/9 )
c      = LOG10 ( 2/1 * 3/2 * 4/3 * ... * 10/9 ) = LOG10 ( 10 ) = 1.
c
c    The formula is:
c
c      PDF(X) = LOG10 ( ( X + 1 ) / X ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 1998
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Frank Benford,
c    The Law of Anomalous Numbers,
c    Proceedings of the American Philosophical Society,
c    Volume 78, pages 551-572, 1938.
c
c    Ted Hill,
c    The First Digit Phenomenon,
c    American Scientist,
c    Volume 86, July/August 1998, pages 358 - 363.
c
c    Ralph Raimi,
c    The Peculiar Distribution of First Digits,
c    Scientific American,
c    December 1969, pages 109-119.
c
c  Parameters:
c
c    Input, integer X, the string of significant digits to be
c    checked.  If X is 1, then we are asking for the Benford probability that
c    a value will have first digit 1.  If X is 123, we are asking for
c    the probability that the first three digits will be 123, and so on.
c
c    Output, double precision PDF, the Benford probability that an item taken
c    from a real world distribution will have the initial digits X.
c
      implicit none

      double precision pdf
      integer x

      if ( x .le. 0 ) then
        pdf = 0.0D+00
      else
        pdf = log10 ( dble ( x + 1 ) / dble ( x ) )
      end if

      return
      end
      subroutine bessel_ix_values ( n_data, nu, x, fx )

c*********************************************************************72
c
cc BESSEL_IX_VALUES returns some values of the Ix Bessel function.
c
c  Discussion:
c
c    This set of data considers the less common case in which the
c    index of the Bessel function In is actually not an integer.
c    We may suggest this case by occasionally replacing the symbol
c    "In" by "Ix".
c
c    The modified Bessel functions In(Z) and Kn(Z) are solutions of
c    the differential equation
c
c      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
c
c    In Mathematica, the function can be evaluated by:
c
c      BesselI[n,x]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision NU, the order of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 28 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision nu
      double precision nu_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save nu_vec
      save x_vec

      data fx_vec /
     &  0.3592084175833614D+00,
     &  0.9376748882454876D+00,
     &  2.046236863089055D+00,
     &  3.053093538196718D+00,
     &  4.614822903407601D+00,
     &  26.47754749755907D+00,
     &  2778.784603874571D+00,
     &  4.327974627242893D+07,
     &  0.2935253263474798D+00,
     &  1.099473188633110D+00,
     &  21.18444226479414D+00,
     &  2500.906154942118D+00,
     &  2.866653715931464D+20,
     &  0.05709890920304825D+00,
     &  0.3970270801393905D+00,
     &  13.76688213868258D+00,
     &  2028.512757391936D+00,
     &  2.753157630035402D+20,
     &  0.4139416015642352D+00,
     &  1.340196758982897D+00,
     &  22.85715510364670D+00,
     &  2593.006763432002D+00,
     &  2.886630075077766D+20,
     &  0.03590910483251082D+00,
     &  0.2931108636266483D+00,
     &  11.99397010023068D+00,
     &  1894.575731562383D+00,
     &  2.716911375760483D+20  /
      data nu_vec /
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  1.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  2.50D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  1.25D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00,
     &  2.75D+00 /
      data x_vec /
     &   0.2D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.5D+00,
     &   3.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  50.0D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        nu = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        nu = nu_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine birthday_cdf ( n, cdf )

c*********************************************************************72
c
cc BIRTHDAY_CDF returns the Birthday Concurrence CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of people whose birthdays have
c    been disclosed.
c
c    Output, double precision CDF, the probability that at least
c    two of the N people have matching birthays..
c
      implicit none

      double precision cdf
      integer i
      integer n

      if ( n .lt. 1 ) then
        cdf = 0.0D+00
        return
      else if ( 365 .lt. n ) then
        cdf = 1.0D+00
        return
      end if
c
c  Compute the probability that N people have distinct birthdays.
c
      cdf = 1.0D+00
      do i = 1, n
        cdf = cdf * dble ( 365 + 1 - i ) / 365.0D+00
      end do
c
c  Compute the probability that it is NOT the case that N people
c  have distinct birthdays.  This is the cumulative probability
c  that person 2 matches person 1, or person 3 matches 1 or 2,
c  etc.
c
      cdf = 1.0D+00 - cdf

      return
      end
      subroutine birthday_cdf_inv ( cdf, n )

c*********************************************************************72
c
cc BIRTHDAY_CDF_INV inverts the Birthday Concurrence CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the probability that at least
c    two of the N people have matching birthays.
c
c    Output, integer N, the corresponding number of people whose
c    birthdays need to be disclosed.
c
      implicit none

      double precision cdf
      double precision cdf_not
      integer i
      integer n

      if ( cdf .le. 0.0D+00 ) then
        n = 0
        return
      else if ( 1.0D+00 .le. cdf ) then
        n = 365
        return
      end if
c
c  Compute the probability that N people have distinct birthdays.
c
      cdf_not = 1.0D+00

      do i = 1, 365
        cdf_not = cdf_not * dble ( 365 + 1 - i ) / 365.0D+00
        if ( cdf .le. 1.0D+00 - cdf_not ) then
          n = i
          return
        end if
      end do

      n = 365

      return
      end
      subroutine birthday_pdf ( n, pdf )

c*********************************************************************72
c
cc BIRTHDAY_PDF returns the Birthday Concurrence PDF.
c
c  Discussion:
c
c    The probability is the probability that the N-th person is the
c    first one to match a birthday with someone earlier.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of people whose birthdays have
c    been disclosed.
c
c    Output, double precision PDF, the probability that the N-th person
c    is the first to match a birthday with someone earlier.
c
      implicit none

      integer i
      integer n
      double precision pdf

      if ( n .lt. 1 .or. 365 .lt. n ) then
        pdf = 0.0D+00
        return
      end if

      pdf = 1.0D+00
c
c  Compute the probability that N-1 people have distinct birthdays.
c
      do i = 1, n-1
        pdf = pdf * dble ( 365 + 1 - i ) / 365.0D+00
      end do
c
c  Compute the probability that person N has one of those N-1 birthdays.
c
      pdf = pdf * dble ( n - 1 ) / 365.0D+00

      return
      end
      subroutine bernoulli_cdf ( x, a, cdf )

c*********************************************************************72
c
cc BERNOULLI_CDF evaluates the Bernoulli CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the number of successes on a single trial.
c    X = 0 or 1.
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      integer x

      if ( x .lt. 0 ) then
        cdf = 0.0D+00
      else if ( x .eq. 0 ) then
        cdf = 1.0D+00 - a
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine bernoulli_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc BERNOULLI_CDF_INV inverts the Bernoulli CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0 .le. CDF .le. 1.0.
c
c    Input, double precision A, the parameter of the PDF.
c    0.0 .le. A .le. 1.0.
c
c    Output, integer X, the corresponding argument.
c
      implicit none

      double precision a
      double precision cdf
      integer x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BERNOULLI_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .le. 1.0D+00 - a ) then
        x = 0
      else
        x = 1
      end if

      return
      end
      function bernoulli_check ( a )

c*********************************************************************72
c
cc BERNOULLI_CHECK checks the parameter of the Bernoulli CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0 .le. A .le. 1.0.
c
c    Output, logical BERNOULLI_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical bernoulli_check

      if ( a .lt. 0.0D+00 .or. 1.0D+00 .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BERNOULLI_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 0 or 1 .lt. A.'
        bernoulli_check = .false.
        return
      end if

      bernoulli_check = .true.

      return
      end
      subroutine bernoulli_mean ( a, mean )

c*********************************************************************72
c
cc BERNOULLI_MEAN returns the mean of the Bernoulli PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success.
c    0.0 .le. A .le. 1.0.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision mean

      mean = a

      return
      end
      subroutine bernoulli_pdf ( x, a, pdf )

c*********************************************************************72
c
cc BERNOULLI_PDF evaluates the Bernoulli PDF.
c
c  Discussion:
c
c    PDF(A;X) = A^X * ( 1 - A )^( X - 1 )
c
c    X = 0 or 1.
c
c    The Bernoulli PDF describes the simple case in which a single trial
c    is carried out, with two possible outcomes, called "success" and
c    "failure"; the probability of success is A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the number of successes on a single trial.
c    X = 0 or 1.
c
c    Input, double precision A, the probability of success on one trial.
c    0.0 .le. A .le. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision pdf
      integer x

      if ( x .lt. 0 ) then
        pdf = 0.0D+00
      else if ( x .eq. 0 ) then
        pdf = 1.0D+00 - a
      else if ( x .eq. 1 ) then
        pdf = a
      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine bernoulli_sample ( a, seed, x )

c*********************************************************************72
c
cc BERNOULLI_SAMPLE samples the Bernoulli PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call bernoulli_cdf_inv ( cdf, a, x )

      return
      end
      subroutine bernoulli_variance ( a, variance )

c*********************************************************************72
c
cc BERNOULLI_VARIANCE returns the variance of the Bernoulli PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success on one trial.
c    0.0 .le. A .le. 1.0.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision variance

      variance = a * ( 1.0D+00 - a )

      return
      end
      function bessel_i0 ( arg )

c*********************************************************************72
c
cc BESSEL_I0 evaluates the modified Bessel function I0(X).
c
c  Discussion:
c
c    The main computation evaluates slightly modified forms of
c    minimax approximations generated by Blair and Edwards, Chalk
c    River (Atomic Energy of Canada Limited) Report AECL-4928,
c    October, 1974.  This transportable program is patterned after
c    the machine dependent FUNPACK packet NATSI0, but cannot match
c    that version for efficiency or accuracy.  This version uses
c    rational functions that theoretically approximate I-SUB-0(X)
c    to at least 18 significant decimal digits.
c
c  Machine dependent constants:
c
c    beta   = Radix for the floating-point system
c    maxexp = Smallest power of beta that overflows
c    XMAX =   Largest argument acceptable to BESI0;  Solution to
c             equation:
c               W(X) * (1+1/(8*X)+9/(128*X^2) = beta^maxexp
c             where  W(X) = EXP(X)/sqrt(2*PI*X)
c
c    Approximate values for some important machines are:
c
c                             beta       maxexp       XMAX
c
c    CRAY-1        (S.P.)       2         8191       5682.810
c    Cyber 180/855
c      under NOS   (S.P.)       2         1070        745.893
c    IEEE (IBM/XT,
c      SUN, etc.)  (S.P.)       2          128         91.900
c    IEEE (IBM/XT,
c      SUN, etc.)  (D.P.)       2         1024        713.986
c    IBM 3033      (D.P.)      16           63        178.182
c    VAX           (S.P.)       2          127         91.203
c    VAX D-Format  (D.P.)       2          127         91.203
c    VAX G-Format  (D.P.)       2         1023        713.293
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2008
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    FORTRAN90 version by John Burkardt.
c
c  Parameters:
c
c    Input, double precision ARG, the argument.
c
c    Output, double precision BESSEL_I0, the value of the modified
c    Bessel function of the first kind.
c
      implicit none

      double precision a
      double precision arg
      double precision b
      double precision bessel_i0
      double precision exp40
      parameter ( exp40 = 2.353852668370199854D+17 )
      integer i
      double precision, parameter, dimension ( 15 ) :: p = (/     -5.24
     &87866627945699800D-18,     -1.5982226675653184646D-14,     -2.6843
     &448573468483278D-11,     -3.0517226450451067446D-08,     -2.517264
     &4670688975051D-05,     -1.5453977791786851041D-02,     -7.09353474
     &49210549190D+00,     -2.4125195876041896775D+03,     -5.9545626019
     &847898221D+05,     -1.0313066708737980747D+08,     -1.191274610498
     &5237192D+10,     -8.4925101247114157499D+11,     -3.29400876274077
     &49166D+13,     -5.5050369673018427753D+14,     -2.2335582639474375
     &249D+15 /)
      double precision, parameter, dimension ( 8 ) :: pp = (/     -3.98
     &43750000000000000D-01,      2.9205384596336793945D+00,     -2.4708
     &469169133954315D+00,      4.7914889422856814203D-01,     -3.738499
     &1926068969150D-03,     -2.6801520353328635310D-03,      9.91687776
     &70983678974D-05,     -2.1877128189032726730D-06 /)
      double precision, parameter, dimension ( 5 ) :: q = (/     -3.727
     &7560179962773046D+03,      6.5158506418655165707D+06,     -6.56265
     &60740833869295D+09,      3.7604188704092954661D+12,     -9.7087946
     &179594019126D+14 /)
      double precision qq(7)
      double precision r8_epsilon
      double precision r8_huge
      double precision rec15
      parameter ( rec15 = 6.6666666666666666666D-02 )
      double precision sump
      double precision sumq
      double precision value
      double precision x
      double precision xmax
      parameter ( xmax = 91.9D+00 )
      double precision xx

      save qq

      data qq /
     &  -3.1446690275135491500D+01,      8.5539563258012929600D+01,
     &  -6.0228002066743340583D+01,      1.3982595353892851542D+01,
     &  -1.1151759188741312645D+00,      3.2547697594819615062D-02,
     &  -5.5194330231005480228D-04 /

      x = abs ( arg )

      if ( x .lt. r8_epsilon ( ) ) then
        value = 1.0D+00
      else if ( x .lt. 15.0D+00 ) then
c
c  EPSILON ( ARG ) .le. ABS(ARG) .lt. 15.0D+00
c
        xx = x * x
        sump = p(1)
        do i = 2, 15
          sump = sump * xx + p(i)
        end do

        xx = xx - 225.0D+00
        sumq = ((((
     &     xx + q(1) )
     &   * xx + q(2) )       
     &   * xx + q(3) )
     &   * xx + q(4) )
     &   * xx + q(5)

        value = sump / sumq

      else if ( 15.0D+00 .le. x ) then

        if ( xmax .lt. x ) then
          value = r8_huge ( )
        else
c
c  15.0D+00 .le. ABS(ARG)
c
          xx = 1.0D+00 / x - rec15

          sump = ((((((
     &          pp(1)
     &      * xx + pp(2) )
     &      * xx + pp(3) )
     &      * xx + pp(4) )            
     &      * xx + pp(5) )
     &      * xx + pp(6) )
     &      * xx + pp(7) )
     &      * xx + pp(8)

          sumq = ((((((
     &        xx + qq(1) )
     &      * xx + qq(2) )
     &      * xx + qq(3) )
     &      * xx + qq(4) )           
     &      * xx + qq(5) )
     &      * xx + qq(6) )
     &      * xx + qq(7)

          value = sump / sumq
c
c  Calculation reformulated to avoid premature overflow.
c
          if ( x .le. xmax - 15.0D+00 ) then
            a = exp ( x )
            b = 1.0D+00
          else
            a = exp ( x - 40.0D+00 )
            b = exp40
          end if

          value = ( ( value * a - pp(1) * a ) / sqrt ( x ) ) * b

        end if

      end if

      bessel_i0 = value

      return
      end
      subroutine bessel_i0_values ( n_data, x, fx )

c*********************************************************************72
c
cc BESSEL_I0_VALUES returns some values of the I0 Bessel function.
c
c  Discussion:
c
c    The modified Bessel functions In(Z) and Kn(Z) are solutions of
c    the differential equation
c
c      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
c
c    The modified Bessel function I0(Z) corresponds to N = 0.
c
c    In Mathematica, the function can be evaluated by:
c
c      BesselI[0,x]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save x_vec

      data fx_vec /
     &  0.1000000000000000D+01,
     &  0.1010025027795146D+01,
     &  0.1040401782229341D+01,
     &  0.1092045364317340D+01,
     &  0.1166514922869803D+01,
     &  0.1266065877752008D+01,
     &  0.1393725584134064D+01,
     &  0.1553395099731217D+01,
     &  0.1749980639738909D+01,
     &  0.1989559356618051D+01,
     &  0.2279585302336067D+01,
     &  0.3289839144050123D+01,
     &  0.4880792585865024D+01,
     &  0.7378203432225480D+01,
     &  0.1130192195213633D+02,
     &  0.1748117185560928D+02,
     &  0.2723987182360445D+02,
     &  0.6723440697647798D+02,
     &  0.4275641157218048D+03,
     &  0.2815716628466254D+04 /
      data x_vec /
     &  0.00D+00,
     &  0.20D+00,
     &  0.40D+00,
     &  0.60D+00,
     &  0.80D+00,
     &  0.10D+01,
     &  0.12D+01,
     &  0.14D+01,
     &  0.16D+01,
     &  0.18D+01,
     &  0.20D+01,
     &  0.25D+01,
     &  0.30D+01,
     &  0.35D+01,
     &  0.40D+01,
     &  0.45D+01,
     &  0.50D+01,
     &  0.60D+01,
     &  0.80D+01,
     &   0.10D+02 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function bessel_i1 ( arg )

c*********************************************************************72
c
cc BESSEL_I1 evaluates the Bessel I function of order I.
c
c  Discussion:
c
c    The main computation evaluates slightly modified forms of
c    minimax approximations generated by Blair and Edwards.
c    This transportable program is patterned after the machine-dependent
c    FUNPACK packet NATSI1, but cannot match that version for efficiency
c    or accuracy.  This version uses rational functions that theoretically
c    approximate I-SUB-1(X) to at least 18 significant decimal digits.
c    The accuracy achieved depends on the arithmetic system, the compiler,
c    the intrinsic functions, and proper selection of the machine-dependent
c    constants.
c
c  Machine-dependent constants:
c
c    beta   = Radix for the floating-point system.
c    maxexp = Smallest power of beta that overflows.
c    XMAX =   Largest argument acceptable to BESI1;  Solution to
c             equation:
c               EXP(X) * (1-3/(8*X)) / SQRT(2*PI*X) = beta**maxexp
c
c
c    Approximate values for some important machines are:
c
c                            beta       maxexp    XMAX
c
c    CRAY-1        (S.P.)       2         8191    5682.810
c    Cyber 180/855
c      under NOS   (S.P.)       2         1070     745.894
c    IEEE (IBM/XT,
c      SUN, etc.)  (S.P.)       2          128      91.906
c    IEEE (IBM/XT,
c      SUN, etc.)  (D.P.)       2         1024     713.987
c    IBM 3033      (D.P.)      16           63     178.185
c    VAX           (S.P.)       2          127      91.209
c    VAX D-Format  (D.P.)       2          127      91.209
c    VAX G-Format  (D.P.)       2         1023     713.293
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2004
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Blair, Edwards,
c    Chalk River Report AECL-4928,
c    Atomic Energy of Canada, Limited,
c    October, 1974.
c
c  Parameters:
c
c    Input, real ARG, the argument.
c
c    Output, double precision BESSEL_I1, the value of the Bessel
c    I1 function.
c
      implicit none

      double precision a
      double precision arg
      double precision b
      double precision bessel_i1
      double precision exp40
      parameter ( exp40 = 2.353852668370199854D+17 )
      double precision forty
      parameter ( forty = 40.0D+00 )
      double precision half
      parameter ( half = 0.5D+00 )
      integer j
      double precision, parameter :: one = 1.0D+00
      double precision, parameter :: one5 = 15.0D+00
      double precision, dimension(15) :: p = (/     -1.9705291802535139
     &930D-19,     -6.5245515583151902910D-16,     -1.192878890360323875
     &4D-12,     -1.4831904935994647675D-09,     -1.3466829827635152875D
     &-06,     -9.1746443287817501309D-04,     -4.7207090827310162436D-0
     &1,     -1.8225946631657315931D+02,     -5.1894091982308017540D+04,
     &     -1.0588550724769347106D+07,     -1.4828267606612366099D+09,  
     &   -1.3357437682275493024D+11,     -6.9876779648010090070D+12,    
     & -1.7732037840791591320D+14,     -1.4577180278143463643D+15 /)
      double precision :: pbar = 3.98437500D-01
      double precision, dimension(8) :: pp = (/     -6.0437159056137600
     &000D-02,      4.5748122901933459000D-01,     -4.284376690330480640
     &3D-01,      9.7356000150886612134D-02,     -3.2457723974465568321D
     &-03,     -3.6395264712121795296D-04,      1.6258661867440836395D-0
     &5,     -3.6347578404608223492D-07 /)
      double precision, dimension(5) :: q = (/     -4.00768646799041899
     &21D+03,      7.4810580356655069138D+06,     -8.0059518998619764991
     &D+09,      4.8544714258273622913D+12,     -1.3218168307321442305D+
     &15 /)
      double precision, dimension(6) :: qq = (/     -3.8806586721556593
     &450D+00,      3.2593714889036996297D+00,     -8.501747646321792440
     &8D-01,      7.4212010813186530069D-02,     -2.2835624489492512649D
     &-03,      3.7510433111922824643D-05 /)
      double precision r8_epsilon
      double precision r8_huge
      double precision, parameter :: rec15 = 6.6666666666666666666D-02
      double precision sump
      double precision sumq
      double precision, parameter :: two25 = 225.0D+00
      double precision value
      double precision x
      double precision, parameter :: xmax = 713.987D+00
      double precision xx
      double precision zero
      parameter ( zero = 0.0D+00 )

      x = abs ( arg )
c
c  ABS(ARG) .lt. EPSILON ( ARG )
c
      if ( x .lt. r8_epsilon ( ) ) then

        value = half * x
c
c  EPSILON ( ARG ) .le. ABS(ARG) .lt. 15.0
c
      else if ( x .lt. one5 ) then

        xx = x * x
        sump = p(1)
        do j = 2, 15
          sump = sump * xx + p(j)
        end do

        xx = xx - two25

        sumq = ((((           xx + q(1)       ) * xx + q(2)       ) * xx
     & + q(3)       ) * xx + q(4)       ) * xx + q(5)

        value = ( sump / sumq ) * x

      else if ( xmax .lt. x ) then

        value = r8_huge ( )
c
c  15.0 .le. ABS(ARG)
c
      else

        xx = one / x - rec15

        sump = ((((((                   pp(1)         * xx + pp(2)      
     & ) * xx + pp(3)       ) * xx + pp(4)       ) * xx + pp(5)       ) 
     &* xx + pp(6)       ) * xx + pp(7)       ) * xx + pp(8)

        sumq = (((((               xx + qq(1)       ) * xx + qq(2)      
     & ) * xx + qq(3)       ) * xx + qq(4)       ) * xx + qq(5)       ) 
     &* xx + qq(6)

        value = sump / sumq

        if ( xmax - one5 .lt. x ) then
          a = exp ( x - forty )
          b = exp40
        else
          a = exp ( x )
          b = one
        end if

        value = ( ( value * a + pbar * a ) / sqrt ( x ) ) * b

      end if

      if ( arg .lt. zero ) then
        value = -value
      end if

      bessel_i1 = value

      return
      end
      subroutine bessel_i1_values ( n_data, x, fx )

c*********************************************************************72
c
cc BESSEL_I1_VALUES returns some values of the I1 Bessel function.
c
c  Discussion:
c
c    The modified Bessel functions In(Z) and Kn(Z) are solutions of
c    the differential equation
c
c      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
c
c    In Mathematica, the function can be evaluated by:
c
c      BesselI[1,x]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save x_vec

      data fx_vec /
     &  0.0000000000000000D+00,
     &  0.1005008340281251D+00,
     &  0.2040267557335706D+00,
     &  0.3137040256049221D+00,
     &  0.4328648026206398D+00,
     &  0.5651591039924850D+00,
     &  0.7146779415526431D+00,
     &  0.8860919814143274D+00,
     &  0.1084810635129880D+01,
     &  0.1317167230391899D+01,
     &  0.1590636854637329D+01,
     &  0.2516716245288698D+01,
     &  0.3953370217402609D+01,
     &  0.6205834922258365D+01,
     &  0.9759465153704450D+01,
     &  0.1538922275373592D+02,
     &  0.2433564214245053D+02,
     &  0.6134193677764024D+02,
     &  0.3998731367825601D+03,
     &  0.2670988303701255D+04 /
      data x_vec /
     &  0.00D+00,
     &  0.20D+00,
     &  0.40D+00,
     &  0.60D+00,
     &  0.80D+00,
     &  0.10D+01,
     &  0.12D+01,
     &  0.14D+01,
     &  0.16D+01,
     &  0.18D+01,
     &  0.20D+01,
     &  0.25D+01,
     &  0.30D+01,
     &  0.35D+01,
     &  0.40D+01,
     &  0.45D+01,
     &  0.50D+01,
     &  0.60D+01,
     &  0.80D+01,
     &  0.10D+02 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function beta ( a, b )

c*********************************************************************72
c
cc BETA returns the value of the Beta function.
c
c  Discussion:
c
c    The Beta function is defined as
c
c      BETA(A,B) = ( GAMMA ( A ) * GAMMA ( B ) ) / GAMMA ( A + B )
c                = Integral ( 0 .le. T .le. 1 ) T^(A-1) (1-T)^(B-1) dT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the function.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision BETA, the value of the function.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      double precision gamma_log

      if ( a .le. 0.0D+00 .or. b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA - Fatal error!'
        write ( *, '(a)' ) '  Both A and B must be greater than 0.'
        stop
      end if

      beta = exp ( gamma_log ( a ) + gamma_log ( b ) - gamma_log ( a + b
     & ) )

      return
      end
      subroutine beta_binomial_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc BETA_BINOMIAL_CDF evaluates the Beta Binomial CDF.
c
c  Discussion:
c
c    A simple summing approach is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the CDF.
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      integer c
      double precision cdf
      double precision pdf
      integer x
      integer y

      if ( x .lt. 0 ) then

        cdf = 0.0D+00

      else if ( x .lt. c ) then

        cdf = 0.0D+00
        do y = 0, x
          pdf = beta ( a + dble ( y ), b + dble ( c - y ) ) 
     &      / ( dble ( c + 1 ) 
     &      * beta ( dble ( y + 1 ), dble ( c - y + 1  ) ) 
     &      * beta ( a, b ) )
          cdf = cdf + pdf
        end do

      else if ( c .le. x ) then

        cdf = 1.0D+00

      end if

      return
      end
      subroutine beta_binomial_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc BETA_BINOMIAL_CDF_INV inverts the Beta Binomial CDF.
c
c  Discussion:
c
c    A simple discrete approach is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. C.
c
c    Output, integer X, the smallest X whose cumulative density
c    function is greater than or equal to CDF.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      integer c
      double precision cdf
      double precision cum
      double precision pdf
      integer x
      integer y

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_BINOMIAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cum = 0.0D+00

      do y = 0, c

        pdf = beta ( a + dble ( y ), b + dble ( c - y ) ) 
     &    / ( dble ( c + 1 ) 
     &    * beta ( dble ( y + 1 ), dble ( c - y + 1 ) ) 
     &    * beta ( a, b ) )

        cum = cum + pdf

        if ( cdf .le. cum ) then
          x = y
          return
        end if

      end do

      x = c

      return
      end
      function beta_binomial_check ( a, b, c )

c*********************************************************************72
c
cc BETA_BINOMIAL_CHECK checks the parameters of the Beta Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. C.
c
c    Output, logical BETA_BINOMIAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical beta_binomial_check
      integer c

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        beta_binomial_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        beta_binomial_check = .false.
        return
      end if

      if ( c .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .lt. 0.'
        beta_binomial_check = .false.
        return
      end if

      beta_binomial_check = .true.

      return
      end
      subroutine beta_binomial_mean ( a, b, c, mean )

c*********************************************************************72
c
cc BETA_BINOMIAL_MEAN returns the mean of the Beta Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. N.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision mean

      mean = dble ( c ) * a / ( a + b )

      return
      end
      subroutine beta_binomial_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc BETA_BINOMIAL_PDF evaluates the Beta Binomial PDF.
c
c  Discussion:
c
c    The PDF is defined as:
c
c      PDF(A,B,C;X) = Beta(A+X,B+C-X)
c        / ( (C+1) * Beta(X+1,C-X+1) * Beta(A,B) )  for 0 .le. X .le. C.
c
c    This PDF can be reformulated as:
c
c      The beta binomial probability density function for X successes
c      out of N trials is
c
c      PDF2(X)( N, MU, THETA ) =
c        C(N,X) * Product ( 0 .le. R .le. X - 1 ) ( MU + R * THETA )
c               * Product ( 0 .le. R .le. N - X - 1 ) ( 1 - MU + R * THETA )
c               / Product ( 0 .le. R .le. N - 1 )  ( 1 + R * THETA )
c
c      where
c
c        C(N,X) is the combinatorial coefficient;
c        MU is the expectation of the underlying Beta distribution;
c        THETA is a shape parameter.
c
c      A THETA value of 0 ( or A+B --> +oo ) results in the binomial
c      distribution:
c
c        PDF2(X) ( N, MU, 0 ) = C(N,X) * MU^X * ( 1 - MU )^(N-X)
c
c    Given A, B, C for PDF, then the equivalent PDF2 has:
c
c      N     = C
c      MU    = A / ( A + B )
c      THETA = 1 / ( A + B )
c
c    Given N, MU, THETA for PDF2, the equivalent PDF has:
c
c      A = MU / THETA
c      B = ( 1 - MU ) / THETA
c      C = N
c
c    BETA_BINOMIAL_PDF(1,1,C;X) = UNIFORM_DISCRETE_PDF(0,C-1;X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      integer c
      double precision pdf
      integer x

      if ( x .lt. 0 ) then

        pdf = 0.0D+00

      else if ( x .le. c ) then

        pdf = beta ( a + dble ( x ), b + dble ( c - x ) )
     &    / ( dble ( c + 1 ) 
     &    * beta ( dble ( x + 1 ), dble ( c - x + 1 ) ) 
     &    * beta ( a, b ) )

      else if ( c .lt. x ) then

        pdf = 0.0D+00

      end if

      return
      end
      subroutine beta_binomial_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc BETA_BINOMIAL_SAMPLE samples the Beta Binomial CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call beta_binomial_cdf_inv ( cdf, a, b, c, x )

      return
      end
      subroutine beta_binomial_variance ( a, b, c, variance )

c*********************************************************************72
c
cc BETA_BINOMIAL_VARIANCE returns the variance of the Beta Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, integer C, a parameter of the PDF.
c    0 .le. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision variance

      variance = ( dble ( c ) * a * b ) 
     &  * ( a + b + dble ( c ) ) 
     &  / ( ( a + b )**2 * ( a + b + 1.0D+00 ) )

      return
      end
      subroutine beta_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc BETA_CDF evaluates the Beta CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision beta_inc
      double precision cdf
      double precision x

      if ( x .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else if ( x .le. 1.0D+00 ) then
        cdf = beta_inc ( a, b, x )
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine beta_cdf_inv ( cdf, p, q, x )

c*********************************************************************72
c
cc BETA_CDF_INV computes the inverse of the incomplete Beta function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by GW Cran, KJ Martin, GE Thomas.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    GW Cran, KJ Martin, GE Thomas,
c    Remark AS R19 and Algorithm AS 109:
c    A Remark on Algorithms AS 63: The Incomplete Beta Integral
c    and AS 64: Inverse of the Incomplete Beta Integeral,
c    Applied Statistics,
c    Volume 26, Number 1, 1977, pages 111-114.
c
c  Parameters:
c
c    Input, double precision CDF, the value of the Beta CDF.
c    0 .le. CDF .le. 1.
c
c    Input, double precision P, Q, the parameters of the incomplete
c    Beta function.
c
c    Output, double precision X, the argument of the incomplete
c    Beta function which produces the value CDF.
c
c  Local Parameters:
c
c    Local, double precision SAE, the most negative decimal exponent
c    which does not cause an underflow.
c
      implicit none

      double precision a
      double precision acu
      double precision adj
      double precision beta_inc
      double precision beta_log
      double precision cdf
      double precision fpu
      double precision g
      double precision gamma_log
      double precision h
      integer iex
      logical indx
      double precision p
      double precision pp
      double precision prev
      double precision q
      double precision qq
      double precision r
      double precision s
      double precision sae
      parameter ( sae = -37.0D+00 )
      double precision sq
      double precision t
      double precision tx
      double precision w
      double precision x
      double precision xin
      double precision y
      double precision yprev

      fpu = 10.0D+00 ** sae
      beta_log = gamma_log ( p ) + gamma_log ( q ) - gamma_log ( p + q )
c
c  Test for admissibility of parameters.
c
      if ( p .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  P .le. 0.0'
        return
      end if

      if ( q .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  Q .le. 0.0'
        return
      end if

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0.0 or 1.0 .lt. CDF.'
        return
      end if
c
c  Return immediately if the answer is easy to determine.
c
      if ( cdf .eq. 0.0D+00  ) then
        x = 0.0D+00
        return
      else if ( cdf .eq. 1.0D+00 ) then
        x = 1.0D+00
        return
      end if
c
c  Change tail if necessary.
c
      if ( 0.5D+00 .lt. cdf ) then
        a = 1.0D+00 - cdf
        pp = q
        qq = p
        indx = .true.
      else
        a = cdf
        pp = p
        qq = q
        indx = .false.
      end if
c
c  Calculate the initial approximation.
c
      r = sqrt ( - log ( a * a ) )

      y = r - ( 2.30753D+00 + 0.27061D+00 * r )     / ( 1.0D+00 + ( 0.99
     &229D+00 + 0.04481D+00 * r ) * r )

      if ( 1.0D+00 .lt. pp .and. 1.0D+00 .lt. qq ) then

        r = ( y * y - 3.0D+00 ) / 6.0D+00
        s = 1.0D+00 / ( pp + pp - 1.0D+00 )
        t = 1.0D+00 / ( qq + qq - 1.0D+00 )
        h = 2.0D+00 / ( s + t )
        w = y * sqrt ( h + r ) / h - ( t - s )     * ( r + 5.0D+00 / 6.0
     &D+00 - 2.0D+00 / ( 3.0D+00 * h ) )
        x = pp / ( pp + qq * exp ( w + w ) )

      else

        r = qq + qq
        t = 1.0D+00 / ( 9.0D+00 * qq )
        t = r * ( 1.0D+00 - t + y * sqrt ( t ) )**3

        if ( t .le. 0.0D+00 ) then
          x = 1.0D+00 - exp ( ( log ( ( 1.0D+00 - a ) * qq )         + b
     &eta_log ) / qq )
        else

          t = ( 4.0D+00 * pp + r - 2.0D+00 ) / t

          if ( t .le. 1.0D+00 ) then
            x = exp ( ( log ( a * pp ) + beta_log ) / pp )
          else
            x = 1.0D+00 - 2.0D+00 / ( t + 1.0D+00 )
          end if

        end if

      end if
c
c  Solve for X by a modified Newton-Raphson method.
c
      r = 1.0D+00 - pp
      t = 1.0D+00 - qq
      yprev = 0.0D+00
      sq = 1.0D+00
      prev = 1.0D+00

      if ( x .lt. 0.0001D+00 ) then
        x = 0.0001D+00
      end if

      if ( 0.9999D+00 .lt. x ) then
        x = 0.9999D+00
      end if

      iex = max ( - 5.0D+00 / pp**2 - 1.0D+00 / a**0.2D+00 - 13.0D+00, s
     &ae )

      acu = 10.0D+00 ** iex

10    continue

        y = beta_inc ( pp, qq, x )

        xin = x
        y = ( y - a ) * exp ( beta_log + r * log ( xin )       + t * log
     & ( 1.0D+00 - xin ) )

        if ( y * yprev .le. 0.0D+00 ) then
          prev = max ( sq, fpu )
        end if

        g = 1.0D+00

20      continue

30        continue

            adj = g * y
            sq = adj * adj

            if ( sq .lt. prev ) then

              tx = x - adj

              if ( 0.0D+00 .le. tx .and. tx .le. 1.0D+00 ) then
                go to 40
              end if

            end if

            g = g / 3.0D+00

          go to 30

40        continue

          if ( prev .le. acu ) then
            if ( indx ) then
              x = 1.0D+00 - x
            end if
            return
          end if

          if ( y * y .le. acu ) then
            if ( indx ) then
              x = 1.0D+00 - x
            end if
            return
          end if

          if ( tx .ne. 0.0D+00 .and. tx .ne. 1.0D+00 ) then
            go to 50
          end if

          g = g / 3.0D+00

        go to 20

50      continue

        if ( tx .eq. x ) then
          go to 60
        end if

        x = tx
        yprev = y

      go to 10

60    continue

      if ( indx ) then
        x = 1.0D+00 - x
      end if

      return
      end
      subroutine beta_cdf_inv_old ( cdf, a, b, x )

c*********************************************************************72
c
cc BETA_CDF_INV_OLD inverts the Beta CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2001
c
c  Author:
c
c    Original FORTRAN77 version by Roger Abernathy, Robert Smith.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    Roger Abernathy, Robert Smith,
c    Algorithm 724,
c    Program to Calculate F Percentiles,
c    ACM Transactions on Mathematical Software,
c    Volume 19, Number 4, December 1993, pages 481-483.
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the argument of the CDF.
c
      implicit none

      integer maxk
      parameter ( maxk = 20 )

      double precision a
      double precision b
      double precision bcoeff
      double precision cdf
      double precision cdf_x
      double precision d(2:maxk,0:maxk-2)
      double precision error
      parameter ( error = 0.0001D+00 )
      double precision errapp
      parameter ( errapp = 0.01D+00 )
      integer i
      integer j
      integer k
      integer loopct
      double precision pdf_x
      double precision q
      double precision s1
      double precision s2
      double precision sum2
      double precision t
      double precision tail
      double precision x
      double precision xold

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if
c
c  Estimate the solution.
c
      x = a / ( a + b )

      xold = 0.0D+00
      loopct = 2

10    continue

      if ( errapp .le. abs ( ( x - xold ) / x ) .and. 
     &  loopct .ne. 0 ) then

        xold = x
        loopct = loopct - 1
c
c  CDF_X = PROB { BETA(A,B) .le. X }.
c  Q = ( CDF - CDF_X ) / PDF_X.
c
        call beta_cdf ( x, a, b, cdf_x )

        call beta_pdf ( x, a, b, pdf_x )

        q = ( cdf - cdf_x ) / pdf_x
c
c  D(N,K) = C(N,K) * Q**(N+K-1) / (N-1)!
c
        t = 1.0D+00 - x
        s1 = q * ( b - 1.0D+00 ) / t
        s2 = q * ( 1.0D+00 - a ) / x
        d(2,0) = s1 + s2
        tail = d(2,0) * q / 2.0D+00
        x = x + q + tail

        k = 3

20      continue

        if ( error .lt. abs ( tail / x ) .and. k .le. maxk ) then
c
c  Find D(2,K-2).
c
          s1 = q * ( dble ( k ) - 2.0D+00 ) * s1 / t
          s2 = q * ( 2.0D+00 - dble ( k ) ) * s2 / x
          d(2,k-2) = s1 + s2
c
c  Find D(3,K-3), D(4,K-4), D(5,K-5), ... , D(K-1,1).
c
          do i = 3, k-1
            sum2 = d(2,0) * d(i-1,k-i)
            bcoeff = 1.0D+00
            do j = 1, k-i
              bcoeff = ( bcoeff * dble ( k - i - j + 1 ) )    
     &         / dble ( j )
              sum2 = sum2 + bcoeff * d(2,j) * d(i-1,k-i-j)
            end do
            d(i,k-i) = sum2 + d(i-1,k-i+1) / dble ( i - 1 )
          end do
c
c  Compute D(K,0) and use it to expand the series.
c
          d(k,0) = d(2,0) * d(k-1,0) + d(k-1,1) / dble ( k - 1 )
          tail = d(k,0) * q / dble ( k )
          x = x + tail
c
c  Check for divergence.
c
          if ( x .le. 0.0D+00 .or. 1.0D+00 .le. x )  then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
            write ( *, '(a)' ) '  The series has diverged.'
            write ( *, '(a,g14.6)' ) '  X = ', x
            x = - 1.0D+00
            return
          end if

          k = k + 1

          go to 20

        end if

        go to 10

      end if

      return
      end
      subroutine beta_cdf_values ( n_data, a, b, x, fx )

c*********************************************************************72
c
cc BETA_CDF_VALUES returns some values of the Beta CDF.
c
c  Discussion:
c
c    The incomplete Beta function may be written
c
c      BETA_INC(A,B,X) = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
c                      / Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
c
c    Thus,
c
c      BETA_INC(A,B,0.0) = 0.0
c      BETA_INC(A,B,1.0) = 1.0
c
c    The incomplete Beta function is also sometimes called the
c    "modified" Beta function, or the "normalized" Beta function
c    or the Beta CDF (cumulative density function.
c
c    In Mathematica, the function can be evaluated by:
c
c      BETA[X,A,B] / BETA[A,B]
c
c    The function can also be evaluated by using the Statistics package:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = BetaDistribution [ a, b ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Karl Pearson,
c    Tables of the Incomplete Beta Function,
c    Cambridge University Press, 1968.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision A, B, the parameters of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 45 )

      double precision a
      double precision a_vec(n_max)
      double precision b
      double precision b_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save b_vec
      save fx_vec
      save x_vec

      data a_vec /
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   5.5D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  30.0D+00,
     &  30.0D+00,
     &  40.0D+00,
     &  0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.2D+01,
     &   0.3D+01,
     &   0.4D+01,
     &   0.5D+01,
     &   1.30625D+00, 
     &   1.30625D+00, 
     &   1.30625D+00 /
      data b_vec /
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &   0.5D+00,
     &   5.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.2D+01,
     &   0.3D+01,
     &   0.4D+01,
     &   0.5D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &  11.7562D+00, 
     &  11.7562D+00, 
     &  11.7562D+00 /
      data fx_vec /
     &  0.6376856085851985D-01,
     &  0.2048327646991335D+00,
     &  0.1000000000000000D+01,
     &  0.0000000000000000D+00,
     &  0.5012562893380045D-02,
     &  0.5131670194948620D-01,
     &  0.2928932188134525D+00,
     &  0.5000000000000000D+00,
     &  0.2800000000000000D-01,
     &  0.1040000000000000D+00,
     &  0.2160000000000000D+00,
     &  0.3520000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.6480000000000000D+00,
     &  0.7840000000000000D+00,
     &  0.8960000000000000D+00,
     &  0.9720000000000000D+00,
     &  0.4361908850559777D+00,
     &  0.1516409096347099D+00,
     &  0.8978271484375000D-01,
     &  0.1000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.4598773297575791D+00,
     &  0.2146816102371739D+00,
     &  0.9507364826957875D+00,
     &  0.5000000000000000D+00,
     &  0.8979413687105918D+00,
     &  0.2241297491808366D+00,
     &  0.7586405487192086D+00,
     &  0.7001783247477069D+00,
     &  0.5131670194948620D-01,
     &  0.1055728090000841D+00,
     &  0.1633399734659245D+00,
     &  0.2254033307585166D+00,
     &  0.3600000000000000D+00,
     &  0.4880000000000000D+00,
     &  0.5904000000000000D+00,
     &  0.6723200000000000D+00,
     &  0.2160000000000000D+00,
     &  0.8370000000000000D-01,
     &  0.3078000000000000D-01,
     &  0.1093500000000000D-01,
     &  0.918884684620518D+00,
     &  0.21052977489419D+00,
     &  0.1824130512500673D+00 /
      data x_vec /
     &  0.01D+00,
     &  0.10D+00,
     &  1.00D+00,
     &  0.00D+00,
     &  0.01D+00,
     &  0.10D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.10D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.50D+00,
     &  0.60D+00,
     &  0.70D+00,
     &  0.80D+00,
     &  0.90D+00,
     &  0.50D+00,
     &  0.90D+00,
     &  0.50D+00,
     &  1.00D+00,
     &  0.50D+00,
     &  0.80D+00,
     &  0.60D+00,
     &  0.80D+00,
     &  0.50D+00,
     &  0.60D+00,
     &  0.70D+00,
     &  0.80D+00,
     &  0.70D+00,
     &  0.10D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.225609D+00, 
     &  0.0335568D+00, 
     &  0.0295222D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0.0D+00
        b = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        b = b_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function beta_check ( a, b )

c*********************************************************************72
c
cc BETA_CHECK checks the parameters of the Beta PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, logical BETA_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical beta_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        beta_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        beta_check = .false.
        return
      end if

      beta_check = .true.

      return
      end
      function beta_inc ( a, b, x )

c*********************************************************************72
c
cc BETA_INC returns the value of the incomplete Beta function.
c
c  Discussion:
c
c    This calculation requires an iteration.  In some cases, the iteration
c    may not converge rapidly, or may become inaccurate.
c
c    The formula is:
c
c      BETA_INC(A,B,X)
c
c        =   Integral ( 0 .le. T .le. X ) T^(A-1) (1-T)^(B-1) dT
c          / Integral ( 0 .le. T .le. 1 ) T^(A-1) (1-T)^(B-1) dT
c
c        =   Integral ( 0 .le. T .le. X ) T^(A-1) (1-T)^(B-1) dT
c          / BETA(A,B)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 2004
c
c  Author:
c
c    Original FORTRAN77 version by KL Majumder, GP Bhattacharjee.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    KL Majumder, GP Bhattacharjee,
c    Algorithm AS63,
c    Applied Statistics,
c    1973, volume 22, number 3.
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the function.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input, double precision X, the argument of the function.
c    Normally, 0.0D+00 .le. X .le. 1.0.
c
c    Output, double precision BETA_INC, the value of the function.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      double precision beta_inc
      double precision cx
      integer i
      integer it
      integer it_max
      parameter ( it_max = 1000 )
      logical indx
      integer ns
      double precision pp
      double precision psq
      double precision qq
      double precision rx
      double precision temp
      double precision term
      double precision tol
      parameter ( tol = 1.0D-07 )
      double precision x
      double precision xx

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_INC - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        stop
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BETA_INC - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        stop
      end if

      if ( x .le. 0.0D+00 ) then
        beta_inc = 0.0D+00
        return
      else if ( 1.0D+00 .le. x ) then
        beta_inc = 1.0D+00
        return
      end if
c
c  Change tail if necessary and determine S.
c
      psq = a + b

      if ( a .lt. ( a + b ) * x ) then
        xx = 1.0D+00 - x
        cx = x
        pp = b
        qq = a
        indx = .true.
      else
        xx = x
        cx = 1.0D+00 - x
        pp = a
        qq = b
        indx = .false.
      end if

      term = 1.0D+00
      i = 1
      beta_inc = 1.0D+00

      ns = int ( qq + cx * ( a + b ) )
c
c  Use Soper's reduction formulas.
c
      rx = xx / cx

      temp = qq - dble ( i )
      if ( ns .eq. 0 ) then
        rx = xx
      end if

      it = 0

10    continue

        it = it + 1

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'BETA_INC - Fatal error!'
          write ( *, '(a)' ) '  Maximum number of iterations exceeded!'
          write ( *, '(a,i8)' ) '  IT_MAX = ', it_max
          stop
        end if

        term = term * temp * rx / ( pp + dble ( i ) )
        beta_inc = beta_inc + term
        temp = abs ( term )

        if ( temp .le. tol .and. temp .le. tol * beta_inc ) then
          go to 20
        end if

        i = i + 1
        ns = ns - 1

        if ( 0 .le. ns ) then
          temp = qq - dble ( i )
          if ( ns .eq. 0 ) then
            rx = xx
          end if
        else
          temp = psq
          psq = psq + 1.0D+00
        end if

      go to 10

20    continue
c
c  Finish calculation.
c
      beta_inc = beta_inc * exp ( pp * log ( xx ) + ( qq - 1.0D+00 )
     & * log ( cx ) ) / ( beta ( a, b ) * pp )

      if ( indx ) then
        beta_inc = 1.0D+00 - beta_inc
      end if

      return
      end
      subroutine beta_inc_values ( n_data, a, b, x, fx )

c*********************************************************************72
c
cc BETA_INC_VALUES returns some values of the incomplete Beta function.
c
c  Discussion:
c
c    The incomplete Beta function may be written
c
c      BETA_INC(A,B,X) = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
c                      / Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
c
c    Thus,
c
c      BETA_INC(A,B,0.0) = 0.0
c      BETA_INC(A,B,1.0) = 1.0
c
c    The incomplete Beta function is also sometimes called the
c    "modified" Beta function, or the "normalized" Beta function
c    or the Beta CDF (cumulative density function.
c
c    In Mathematica, the function can be evaluated by:
c
c      BETA[X,A,B] / BETA[A,B]
c
c    The function can also be evaluated by using the Statistics package:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = BetaDistribution [ a, b ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Karl Pearson,
c    Tables of the Incomplete Beta Function,
c    Cambridge University Press, 1968.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision A, B, the parameters of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 45 )

      double precision a
      double precision a_vec(n_max)
      double precision b
      double precision b_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save b_vec
      save fx_vec
      save x_vec

      data a_vec /
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   5.5D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  30.0D+00,
     &  30.0D+00,
     &  40.0D+00,
     &  0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.2D+01,
     &   0.3D+01,
     &   0.4D+01,
     &   0.5D+01,
     &   1.30625D+00, 
     &   1.30625D+00, 
     &   1.30625D+00 /
      data b_vec /
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   1.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   2.0D+00,
     &   5.0D+00,
     &   0.5D+00,
     &   5.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &  20.0D+00,
     &  10.0D+00,
     &  10.0D+00,
     &  20.0D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.2D+01,
     &   0.3D+01,
     &   0.4D+01,
     &   0.5D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &  11.7562D+00, 
     &  11.7562D+00, 
     &  11.7562D+00 /
      data fx_vec /
     &  0.6376856085851985D-01,
     &  0.2048327646991335D+00,
     &  0.1000000000000000D+01,
     &  0.0000000000000000D+00,
     &  0.5012562893380045D-02,
     &  0.5131670194948620D-01,
     &  0.2928932188134525D+00,
     &  0.5000000000000000D+00,
     &  0.2800000000000000D-01,
     &  0.1040000000000000D+00,
     &  0.2160000000000000D+00,
     &  0.3520000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.6480000000000000D+00,
     &  0.7840000000000000D+00,
     &  0.8960000000000000D+00,
     &  0.9720000000000000D+00,
     &  0.4361908850559777D+00,
     &  0.1516409096347099D+00,
     &  0.8978271484375000D-01,
     &  0.1000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.4598773297575791D+00,
     &  0.2146816102371739D+00,
     &  0.9507364826957875D+00,
     &  0.5000000000000000D+00,
     &  0.8979413687105918D+00,
     &  0.2241297491808366D+00,
     &  0.7586405487192086D+00,
     &  0.7001783247477069D+00,
     &  0.5131670194948620D-01,
     &  0.1055728090000841D+00,
     &  0.1633399734659245D+00,
     &  0.2254033307585166D+00,
     &  0.3600000000000000D+00,
     &  0.4880000000000000D+00,
     &  0.5904000000000000D+00,
     &  0.6723200000000000D+00,
     &  0.2160000000000000D+00,
     &  0.8370000000000000D-01,
     &  0.3078000000000000D-01,
     &  0.1093500000000000D-01,
     &  0.918884684620518D+00,
     &  0.21052977489419D+00,
     &  0.1824130512500673D+00 /
      data x_vec /
     &  0.01D+00,
     &  0.10D+00,
     &  1.00D+00,
     &  0.00D+00,
     &  0.01D+00,
     &  0.10D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.10D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.50D+00,
     &  0.60D+00,
     &  0.70D+00,
     &  0.80D+00,
     &  0.90D+00,
     &  0.50D+00,
     &  0.90D+00,
     &  0.50D+00,
     &  1.00D+00,
     &  0.50D+00,
     &  0.80D+00,
     &  0.60D+00,
     &  0.80D+00,
     &  0.50D+00,
     &  0.60D+00,
     &  0.70D+00,
     &  0.80D+00,
     &  0.70D+00,
     &  0.10D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.225609D+00, 
     &  0.0335568D+00, 
     &  0.0295222D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0.0D+00
        b = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        b = b_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine beta_mean ( a, b, mean )

c*********************************************************************72
c
cc BETA_MEAN returns the mean of the Beta PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a / ( a + b )

      return
      end
      subroutine beta_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc BETA_PDF evaluates the Beta PDF.
c
c  Discussion:
c
c    The formula for the PDF is:
c
c      PDF(A,B;X) = X^(A-1) * (1-X)^(B-1) / BETA(A,B).
c
c    A = B = 1 yields the Uniform distribution on [0,1].
c    A = B = 1/2 yields the Arcsin distribution.
c        B = 1 yields the power function distribution.
c    A = B -> Infinity tends to the Normal distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      double precision pdf
      double precision x

      if ( x .lt. 0.0D+00 .or. 1.0D+00 .lt. x ) then
        pdf = 0.0D+00
      else
        pdf = x**( a - 1.0D+00 ) * ( 1.0D+00 - x )**( b - 1.0D+00 ) / be
     &ta ( a, b )
      end if

      return
      end
      subroutine beta_sample ( a, b, seed, x )

c*********************************************************************72
c
cc BETA_SAMPLE samples the Beta PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    William Kennedy, James Gentle,
c    Algorithm BN,
c    Statistical Computing,
c    Dekker, 1980.
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision r8_uniform_01
      double precision mu
      integer seed
      double precision stdev
      double precision test
      double precision u
      double precision x
      double precision y

      mu = ( a - 1.0D+00 ) / ( a + b - 2.0D+00 )
      stdev = 0.5D+00 / sqrt ( a + b - 2.0D+00 )

10    continue

        call normal_01_sample ( seed, y )

        x = mu + stdev * y

        if ( x .lt. 0.0D+00 .or. 1.0D+00 .lt. x ) then
          go to 10
        end if

        u = r8_uniform_01 ( seed )

        test = 
     &      ( a - 1.0D+00 ) * log ( x   / ( a - 1.0D+00 ) )
     &    + ( b - 1.0D+00 ) * log ( ( 1.0D+00 - x ) / ( b - 1.0D+00 ) )
     &    + ( a + b - 2.0D+00 ) * log ( a + b - 2.0D+00 ) 
     &    + 0.5D+00 * y * y

        if ( log ( u ) .le. test ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine beta_variance ( a, b, variance )

c*********************************************************************72
c
cc BETA_VARIANCE returns the variance of the Beta PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = ( a * b ) / ( ( a + b )**2 * ( 1.0D+00 + a + b ) )

      return
      end
      subroutine binomial_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc BINOMIAL_CDF evaluates the Binomial CDF.
c
c  Discussion:
c
c    CDF(X)(A,B) is the probability of at most X successes in A trials,
c    given that the probability of success on a single trial is B.
c
c    A sequence of trials with fixed probability of success on
c    any trial is known as a sequence of Bernoulli trials.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the desired number of successes.
c    0 .le. X .le. A.
c
c    Input, integer A, the number of trials.
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer a
      double precision b
      integer cnk
      double precision cdf
      integer j
      double precision pr
      integer x

      if ( x .lt. 0 ) then

        cdf = 0.0D+00

      else if ( a .le. x ) then

        cdf = 1.0D+00

      else if ( b .eq. 0.0D+00 ) then

        cdf = 1.0D+00

      else if ( b .eq. 1.0D+00 ) then

        cdf = 0.0D+00

      else

        cdf = 0.0D+00

        do j = 0, x

          call binomial_coef ( a, j, cnk )

          pr = dble ( cnk ) * b**j * ( 1.0D+00 - b )**( a - j )

          cdf = cdf + pr

        end do

      end if

      return
      end
      subroutine binomial_cdf_values ( n_data, a, b, x, fx )

c*********************************************************************72
c
cc BINOMIAL_CDF_VALUES returns some values of the binomial CDF.
c
c  Discussion:
c
c    CDF(X)(A,B) is the probability of at most X successes in A trials,
c    given that the probability of success on a single trial is B.
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`DiscreteDistributions`]
c      dist = BinomialDistribution [ n, p ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996,
c    ISBN: 0-8493-2479-3,
c    LC: QA47.M315.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer A, a parameter of the function.
c
c    Output, double precision B, a parameter of the function.
c
c    Output, integer X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 17 )

      integer a
      integer a_vec(n_max)
      double precision b
      double precision b_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      integer x
      integer x_vec(n_max)

      save a_vec
      save b_vec
      save fx_vec
      save x_vec

      data a_vec /
     &   2,  2,  2,  2,
     &   2,  4,  4,  4,
     &   4, 10, 10, 10,
     &  10, 10, 10, 10,
     &  10 /
      data b_vec /
     &  0.05D+00,
     &  0.05D+00,
     &  0.05D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.05D+00,
     &  0.10D+00,
     &  0.15D+00,
     &  0.20D+00,
     &  0.25D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.50D+00 /
      data fx_vec /
     &  0.9025000000000000D+00,
     &  0.9975000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2500000000000000D+00,
     &  0.7500000000000000D+00,
     &  0.3164062500000000D+00,
     &  0.7382812500000000D+00,
     &  0.9492187500000000D+00,
     &  0.9960937500000000D+00,
     &  0.9999363101685547D+00,
     &  0.9983650626000000D+00,
     &  0.9901259090013672D+00,
     &  0.9672065024000000D+00,
     &  0.9218730926513672D+00,
     &  0.8497316674000000D+00,
     &  0.6331032576000000D+00,
     &  0.3769531250000000D+00 /
      data x_vec /
     &   0, 1, 2, 0,
     &   1, 0, 1, 2,
     &   3, 4, 4, 4,
     &   4, 4, 4, 4,
     &   4 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0
        b = 0.0D+00
        x = 0
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        b = b_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine binomial_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc BINOMIAL_CDF_INV inverts the Binomial CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, integer A, the number of trials.
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Output, integer X, the corresponding argument.
c
      implicit none

      integer a
      double precision b
      double precision cdf
      double precision cdf2
      double precision pdf
      integer x
      integer x2

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BINOMIAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cdf2 = 0.0D+00

      do x2 = 0, a

        call binomial_pdf ( x2, a, b, pdf )

        cdf2 = cdf2 + pdf

        if ( cdf .le. cdf2 ) then
          x = x2
          return
        end if

      end do

      return
      end
      function binomial_check ( a, b )

c*********************************************************************72
c
cc BINOMIAL_CHECK checks the parameter of the Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Output, logical BINOMIAL_CHECK, is true if the parameters are legal.
c
      implicit none

      integer a
      double precision b
      logical binomial_check

      if ( a .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 1.'
        binomial_check = .false.
        return
      end if

      if ( b .lt. 0.0D+00 .or. 1.0D+00 .lt. b ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. 0 or 1 .lt. B.'
        binomial_check = .false.
        return
      end if

      binomial_check = .true.

      return
      end
      subroutine binomial_coef ( n, k, cnk )

c*********************************************************************72
c
cc BINOMIAL_COEF computes the Binomial coefficient C(N,K).
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in integer arithmetic.
c
c    The formula is:
c
c      CNK = C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Combinatorial of M Things Taken N at a Time,
c    ACM Algorithm 160,
c    Communications of the ACM,
c    April, 1963.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, integer CNK, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer cnk
      integer i
      integer k
      integer mn
      integer mx
      integer n

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        cnk = 0

      else if ( mn .eq. 0 ) then

        cnk = 1

      else

        mx = max ( k, n - k )
        cnk = mx + 1

        do i = 2, mn
          cnk = ( cnk * ( mx + i ) ) / i
        end do

      end if

      return
      end
      subroutine binomial_coef_log ( n, k, cnk_log )

c*********************************************************************72
c
cc BINOMIAL_COEF_LOG computes the logarithm of the Binomial coefficient.
c
c  Discussion:
c
c    The formula is:
c
c      CNK_LOG = LOG ( C(N,K) ) = LOG ( N! / ( K! * (N-K)! ) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, double precision CNK_LOG, the logarithm of C(N,K).
c
      implicit none

      double precision cnk_log
      double precision factorial_log
      integer k
      integer n

      cnk_log = factorial_log ( n ) - factorial_log ( k ) - factorial_lo
     &g ( n - k )

      return
      end
      subroutine binomial_mean ( a, b, mean )

c*********************************************************************72
c
cc BINOMIAL_MEAN returns the mean of the Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Output, double precision MEAN, the expected value of the number of
c    successes in A trials.
c
      implicit none

      integer a
      double precision b
      double precision mean

      mean = dble ( a ) * b

      return
      end
      subroutine binomial_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc BINOMIAL_PDF evaluates the Binomial PDF.
c
c  Discussion:
c
c    PDF(A,B;X) is the probability of exactly X successes in A trials,
c    given that the probability of success on a single trial is B.
c
c    The formula is:
c
c      PDF(A,B;X) = C(N,X) * B^X * ( 1.0D+00 - B )^( A - X )
c
c    Binomial_PDF(1,B;X) = Bernoulli_PDF(B;X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the desired number of successes.
c    0 .le. X .le. A.
c
c    Input, integer A, the number of trials.
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      double precision b
      integer cnk
      double precision pdf
      integer x

      if ( a .lt. 1 ) then

        pdf = 0.0D+00

      else if ( x .lt. 0 .or. a .lt. x ) then

        pdf = 0.0D+00

      else if ( b .eq. 0.0D+00 ) then

        if ( x .eq. 0 ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if

      else if ( b .eq. 1.0D+00 ) then

        if ( x .eq. a ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if

      else

        call binomial_coef ( a, x, cnk )

        pdf = dble ( cnk ) * b**x * ( 1.0D+00 - b )**( a - x )

      end if

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
c    02 February 1999
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
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision r8_uniform_01
      integer i
      integer seed
      double precision u
      integer x

      x = 0

      do i = 1, a

        u = r8_uniform_01 ( seed )

        if ( u .le. b ) then
          x = x + 1
        end if

      end do

      return
      end
      subroutine binomial_variance ( a, b, variance )

c*********************************************************************72
c
cc BINOMIAL_VARIANCE returns the variance of the Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c    1 .le. A.
c
c    Input, double precision B, the probability of success on one trial.
c    0.0D+00 .le. B .le. 1.0.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision variance

      variance = dble ( a ) * b * ( 1.0D+00 - b )

      return
      end
      subroutine bradford_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc BRADFORD_CDF evaluates the Bradford CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x

      if ( x .le. a ) then
        cdf = 0.0D+00
      else if ( x .le. b ) then
        cdf = log ( 1.0D+00 + c * ( x - a ) / ( b - a ) ) / log ( c + 1.
     &0D+00 )
      else if ( b .lt. x ) then
        cdf = 1.0D+00
      end if

      return
      end
      subroutine bradford_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc BRADFORD_CDF_INV inverts the Bradford CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BRADFORD_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .le. 0.0D+00 ) then
        x = a
      else if ( cdf .lt. 1.0D+00 ) then
        x = a + ( b - a ) * ( ( c + 1.0D+00 )**cdf - 1.0D+00 ) / c
      else if ( 1.0D+00 .le. cdf ) then
        x = b
      end if

      return
      end
      function bradford_check ( a, b, c )

c*********************************************************************72
c
cc BRADFORD_CHECK checks the parameters of the Bradford PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, logical BRADFORD_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical bradford_check
      double precision c

      if ( b .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BRADFORD_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. A.'
        bradford_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BRADFORD_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        bradford_check = .false.
        return
      end if

      bradford_check = .true.

      return
      end
      subroutine bradford_mean ( a, b, c, mean )

c*********************************************************************72
c
cc BRADFORD_MEAN returns the mean of the Bradford PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean

      mean = ( c * ( b - a ) + log ( c + 1.0D+00 ) * ( a * ( c + 1.0D+00
     & ) - b ) )     / ( c * log ( c + 1.0D+00 ) )

      return
      end
      subroutine bradford_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc BRADFORD_PDF evaluates the Bradford PDF.
c
c  Discussion:
c
c    The formula is:
c
c      PDF(A,B,C;X) =
c        C / ( ( C * ( X - A ) + B - A ) * log ( C + 1 ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision x

      if ( x .le. a ) then
        pdf = 0.0D+00
      else if ( x .le. b ) then
        pdf = c / ( ( c * ( x - a ) + b - a ) * log ( c + 1.0D+00 ) )
      else if ( b .lt. x ) then
        pdf = 0.0D+00
      end if

      return
      end
      subroutine bradford_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc BRADFORD_SAMPLE samples the Bradford PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      x = a + ( b - a ) * ( ( c + 1.0D+00 )**cdf - 1.0D+00 ) / c

      return
      end
      subroutine bradford_variance ( a, b, c, variance )

c*********************************************************************72
c
cc BRADFORD_VARIANCE returns the variance of the Bradford PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision variance

      variance = ( b - a )**2 *     ( c * ( log ( c + 1.0D+00 ) - 2.0D+0
     &0 ) + 2.0D+00 * log ( c + 1.0D+00 ) )     / ( 2.0D+00 * c * ( log 
     &( c + 1.0D+00 ) )**2 )

      return
      end
      subroutine buffon_laplace_pdf ( a, b, l, pdf )

c*********************************************************************72
c
cc BUFFON_LAPLACE_PDF evaluates the Buffon-Laplace PDF.
c
c  Discussion:
c
c    In the Buffon-Laplace needle experiment, we suppose that the plane has been
c    tiled into a grid of rectangles of width A and height B, and that a
c    needle of length L is dropped "at random" onto this grid.
c
c    We may assume that one end, the "eye" of the needle falls at the point
c    (X1,Y1), taken uniformly at random in the cell [0,A]x[0,B].
c
c    ANGLE, the angle that the needle makes is taken to be uniformly random.
c    The point of the needle, (X2,Y2), therefore lies at
c
c      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
c
c    The needle will have crossed at least one grid line if any of the
c    following are true:
c
c      X2 .le. 0, A .le. X2, Y2 .le. 0, B .le. Y2.
c
c    If L is larger than sqrt ( A*A + B*B ), then the needle will
c    cross every time, and the computation is uninteresting.  However, if
c    L is smaller than this limit, then the probability of a crossing on
c    a single trial is
c
c      P(L,A,B) = ( 2 * L * ( A + B ) - L * L ) / ( PI * A * B )
c
c    and therefore, a record of the number of hits for a given number of
c    trials can be used as a very roundabout way of estimating PI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Sudarshan Raghunathan,
c    Making a Supercomputer Do What You Want: High Level Tools for
c    Parallel Programming,
c    Computing in Science and Engineering,
c    Volume 8, Number 5, September/October 2006, pages 70-80.
c
c  Parameters:
c
c    Input, double precision A, B, the horizontal and vertical dimensions
c    of each cell of the grid.  0 .le. A, 0 .le. B.
c
c    Input, double precision L, the length of the needle.
c    0 .le. L .le. min ( A, B ).
c
c    Output, double precision PDF, the Buffon-Laplace PDF.
c
      implicit none

      double precision a
      double precision b
      double precision l
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      if ( a .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BUFFON_LAPLACE_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input A .lt. 0.'
        stop
      else if ( a .eq. 0.0D+00 ) then
        pdf = 1.0D+00
        return
      end if

      if ( b .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BUFFON_LAPLACE_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input B .lt. 0.'
        stop
      else if ( b .eq. 0.0D+00 ) then
        pdf = 1.0D+00
        return
      end if

      if ( l .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BUFFON_LAPLACE_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input L .lt. 0.'
        stop
      else if ( l .eq. 0.0D+00 ) then
        pdf = 0.0D+00
        return
      else if ( min ( a, b ) .lt. l ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BUFFON_LAPLACE_PDF - Fatal error!'
        write ( *, '(a)' ) '  min ( A, B ) .lt. L.'
        stop
      end if

      pdf = l * ( 2.0D+00 * ( a + b ) - l ) / ( pi * a * b )

      return
      end
      function buffon_laplace_simulate ( a, b, l, trial_num, seed )

c*********************************************************************72
c
cc BUFFON_LAPLACE_SIMULATE simulates a Buffon-Laplace needle experiment.
c
c  Discussion:
c
c    In the Buffon-Laplace needle experiment, we suppose that the plane has
c    been tiled into a grid of rectangles of width A and height B, and that a
c    needle of length L is dropped "at random" onto this grid.
c
c    We may assume that one end, the "eye" of the needle falls at the point
c    (X1,Y1), taken uniformly at random in the cell [0,A]x[0,B].
c
c    ANGLE, the angle that the needle makes is taken to be uniformly random.
c    The point of the needle, (X2,Y2), therefore lies at
c
c      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
c
c    The needle will have crossed at least one grid line if any of the
c    following are true:
c
c      X2 .le. 0, A .le. X2, Y2 .le. 0, B .le. Y2.
c
c    This routine simulates the tossing of the needle, and returns the number
c    of times that the needle crossed at least one grid line.
c
c    If L is larger than sqrt ( A*A + B*B ), then the needle will
c    cross every time, and the computation is uninteresting.  However, if
c    L is smaller than this limit, then the probability of a crossing on
c    a single trial is
c
c      P(L,A,B) = ( 2 * L * ( A + B ) - L * L ) / ( PI * A * B )
c
c    and therefore, a record of the number of hits for a given number of
c    trials can be used as a very roundabout way of estimating PI.
c    (Particularly roundabout, since we actually will use a good value of
c    PI in order to pick the random angles!)
c
c    Note that this routine will try to generate 5 * TRIAL_NUM random
c    double precision values at one time, using automatic arrays.
c    When I tried this with TRIAL_NUM = 1,000,000, the program failed,
c    because of internal system limits on such arrays.
c
c    Such a problem could be avoided by using a DO loop running through
c    each trial individually, but this tend to run much more slowly than
c    necessary.
c
c    Since this routine invokes the FORTRAN90 random number generator,
c    the user should initialize the random number generator, particularly
c    if it is desired to control whether the sequence is to be varied
c    or repeated.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Sudarshan Raghunathan,
c    Making a Supercomputer Do What You Want: High Level Tools for
c    Parallel Programming,
c    Computing in Science and Engineering,
c    Volume 8, Number 5, September/October 2006, pages 70-80.
c
c  Parameters:
c
c    Input, double precision A, B, the horizontal and vertical dimensions
c    of each cell of the grid.  0 .le. A, 0 .le. B.
c
c    Input, double precision L, the length of the needle.
c    0 .le. L .le. min ( A, B ).
c
c    Input, integer TRIAL_NUM, the number of times the needle is
c    to be dropped onto the grid.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, integer BUFFON_LAPLACE_SIMULATE, the number of times
c    the needle crossed at least one line of the grid of cells.
c
c  Local Parameters:
c
c    Local, integer BATCH_SIZE, specifies the number of trials to be done
c    in a single batch.  Setting BATCH_SIZE to 1 will be very slow.
c    Replacing it by TRIAL_NUM would be fine except that your system
c    may have a limit on the size of automatic arrays.  We have set a default
c    value of 10,000 here which should be large enough to be efficient
c    but small enough not to annoy the system.
c
      implicit none

      integer batch_size
      parameter ( batch_size = 10000 )
      integer trial_num

      double precision a
      double precision angle(batch_size)
      double precision b
      integer batch
      integer buffon_laplace_simulate
      integer hits
      integer i
      double precision l
      integer n
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision x1(batch_size)
      double precision x2(batch_size)
      double precision y1(batch_size)
      double precision y2(batch_size)

      hits = 0

      do batch = 1, trial_num, batch_size

        n = min ( batch_size, trial_num + 1 - batch )
c
c  Randomly choose the location of the eye of the needle in [0,0]x[A,B],
c  and the angle the needle makes.
c
        call r8vec_uniform_01 ( n, seed, x1 )
        call r8vec_uniform_01 ( n, seed, y1 )
        call r8vec_uniform_01 ( n, seed, angle )

        do i = 1, n
          x1(i) = a * x1(i)
          y1(i) = b * y1(i)
          angle(i) = 2.0D+00 * pi * angle(i)
c
c  Compute the location of the point of the needle.
c
          x2(i) = x1(i) + l * cos ( angle(i) )
          y2(i) = y1(i) + l * sin ( angle(i) )
c
c  Count the end locations that lie outside the cell.
c
          if ( x2(i) .le. 0.0D+00 .or. a .le. x2(i) .or.
     &         y2(i) .le. 0.0D+00 .or. b .le. y2(i) ) then
            hits = hits + 1
          end if

        end do

      end do

      buffon_laplace_simulate = hits

      return
      end
      subroutine buffon_pdf ( a, l, pdf )

c*********************************************************************72
c
cc BUFFON_PDF evaluates the Buffon PDF.
c
c  Discussion:
c
c    In the Buffon needle experiment, we suppose that the plane has been
c    ruled by vertical lines with a spacing of A units, and that a
c    needle of length L is dropped "at random" onto this grid.
c
c    Because of the various symmetries, we may assume that this eye of
c    this needle lands in the first infinite strip, and we may further
c    assume that its Y coordinate is 0.  Thus, we have
c    the eye as (X1,Y1) with 0 .le. X1 .le. A and Y1 = 0.
c
c    ANGLE, the angle that the needle makes is taken to be uniformly random.
c    The point of the needle, (X2,Y2), therefore lies at
c
c      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
c
c    The needle will have crossed at least one grid line if any of the
c    following are true:
c
c      X2 .le. 0, A .le. X2.
c
c    The probability of a crossing on a single trial is
c
c      P(A,L) = ( 2 * L ) / ( PI * A )
c
c    and therefore, a record of the number of hits for a given number of
c    trials can be used as a very roundabout way of estimating PI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the horizontal spacing between the
c    vertical grid lines.  0 .le. A.
c
c    Input, double precision L, the length of the needle.
c
c    Output, double precision PDF, the Buffon PDF.
c
      implicit none

      double precision a
      double precision l
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      if ( a .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BUFFON_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input A .lt. 0.'
        stop
      else if ( a .eq. 0.0D+00 ) then
        pdf = 1.0D+00
        return
      end if

      if ( l .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BUFFON_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input L .lt. 0.'
        stop
      else if ( l .eq. 0.0D+00 ) then
        pdf = 0.0D+00
        return
      end if

      pdf = ( 2.0D+00 * l ) / ( pi * a )

      return
      end
      function buffon_simulate ( a, l, trial_num )

c*********************************************************************72
c
cc BUFFON_SIMULATE simulates a Buffon needle experiment.
c
c  Discussion:
c
c    In the Buffon needle experiment, we suppose that the plane has been
c    ruled by vertical lines with a spacing of A units, and that a
c    needle of length L is dropped "at random" onto this grid.
c
c    Because of the various symmetries, we may assume that this eye of
c    this needle lands in the first infinite strip, and we may further
c    assume that its Y coordinate is 0.  Thus, we have
c    the eye as (X1,Y1) with 0 .le. X1 .le. A and Y1 = 0.
c
c    ANGLE, the angle that the needle makes is taken to be uniformly random.
c    The point of the needle, (X2,Y2), therefore lies at
c
c      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
c
c    The needle will have crossed at least one grid line if any of the
c    following are true:
c
c      X2 .le. 0, A .le. X2.
c
c    The probability of a crossing on a single trial is
c
c      P(A,L) = ( 2 * L ) / ( PI * A )
c
c    and therefore, a record of the number of hits for a given number of
c    trials can be used as a very roundabout way of estimating PI.
c
c    Note that this routine will try to generate 4 * TRIAL_NUM random
c    double precision values at one time, using automatic arrays.
c    When I tried this with TRIAL_NUM = 1,000,000, the program failed,
c    because of internal system limits on such arrays.
c
c    Such a problem could be avoided by using a DO loop running through
c    each trial individually, but this tend to run much more slowly than
c    necessary.
c
c    Since this routine invokes the FORTRAN90 random number generator,
c    the user should initialize the random number generator, particularly
c    if it is desired to control whether the sequence is to be varied
c    or repeated.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the horizontal spacing between the
c    vertical grid lines.  0 .le. A.
c
c    Input, double precision L, the length of the needle.
c
c    Input, integer TRIAL_NUM, the number of times the needle is
c    to be dropped onto the grid.
c
c    Output, integer BUFFON_SIMULATE, the number of times the
c    needle crossed at least one line of the grid of cells.
c
c  Local Parameters:
c
c    Local, integer BATCH_SIZE, specifies the number of trials to be done
c    in a single batch.  Setting BATCH_SIZE to 1 will be very slow.
c    Replacing it by TRIAL_NUM would be fine except that your system
c    may have a limit on the size of automatic arrays.  We have set a default
c    value of 10,000 here which should be large enough to be efficient
c    but small enough not to annoy the system.
c
      implicit none

      integer batch_size
      parameter ( batch_size = 10000 )
      integer trial_num

      double precision a
      double precision angle(batch_size)
      integer batch
      integer buffon_simulate
      integer hits
      double precision l
      integer n
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x1(batch_size)
      double precision x2(batch_size)

      hits = 0

      do batch = 1, trial_num, batch_size

        n = min ( batch_size, trial_num + 1 - batch )
c
c  Randomly choose the location (X1,Y1) of the eye of the needle
c  in [0,0]x[A,0], and the angle the needle makes.
c
        call random_number ( harvest = x1(1:n) )
        call random_number ( harvest = angle(1:n) )

        x1(1:n) = a * x1(1:n)
        angle(1:n) = 2.0D+00 * pi * angle(1:n)
c
c  Compute the location of the point of the needle.
c  We only need to know the value of X2, not Y2!
c
        x2(1:n) = x1(1:n) + l * cos ( angle(1:n) )
c
c  Count the end locations that lie outside the cell.
c
        hits = hits + count (      x2(1:n) .le. 0.0 .or.                  
     &         a .le. x2(1:n) )

      end do

      buffon_simulate = hits

      return
      end
      subroutine burr_cdf ( x, a, b, c, d, cdf )

c*********************************************************************72
c
cc BURR_CDF evaluates the Burr CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision d
      double precision x

      if ( x .le. a ) then

        cdf = 0.0D+00

      else

        cdf = 1.0D+00 / ( 1.0D+00 + ( b / ( x - a ) )**c  )**d

      end if

      return
      end
      subroutine burr_cdf_inv ( cdf, a, b, c, d, x )

c*********************************************************************72
c
cc BURR_CDF_INV inverts the Burr CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision d
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BURR_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a + b / ( ( 1.0D+00 / cdf )**(1.0D+00 / d ) - 1.0D+00 )**( 1.0
     &D+00 / c )

      return
      end
      function burr_check ( a, b, c, d )

c*********************************************************************72
c
cc BURR_CHECK checks the parameters of the Burr CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, logical BURR_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical burr_check
      double precision c
      double precision d

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BURR_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        burr_check = .false.
        return
      end if

      if ( c .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BURR_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        burr_check = .false.
        return
      end if

      burr_check = .true.

      return
      end
      subroutine burr_mean ( a, b, c, d, mean )

c*********************************************************************72
c
cc BURR_MEAN returns the mean of the Burr PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision d
      double precision mean
      double precision r8_gamma

      mean = a + b * r8_gamma ( 1.0D+00 - 1.0D+00 / c )     * r8_gamma (
     & d + 1.0D+00 / c ) / r8_gamma ( d )

      return
      end
      subroutine burr_pdf ( x, a, b, c, d, pdf )

c*********************************************************************72
c
cc BURR_PDF evaluates the Burr PDF.
c
c  Discussion:
c
c    The formula is:
c
c      PDF(A,B,C,D;X) = ( C * D / B ) * ( ( X - A ) / B )^( - C - 1 )
c        * ( 1 + ( ( X - A ) / B )^( - C ) )^( - D - 1 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Johnson,
c    Multivariate Statistical Simulation,
c    Wiley, 1987.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision d
      double precision pdf
      double precision x
      double precision y

      if ( x .le. a ) then
        pdf = 0.0D+00
      else

        y = ( x - a ) / b

        pdf = ( c * d / b ) * y**( - c - 1.0D+00 )       * ( 1.0D+00 + y
     &**( - c ) )**( - d - 1.0D+00 )

      end if

      return
      end
      subroutine burr_sample ( a, b, c, d, seed, x )

c*********************************************************************72
c
cc BURR_SAMPLE samples the Burr PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision d
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call burr_cdf_inv ( cdf, a, b, c, d, x )

      return
      end
      subroutine burr_variance ( a, b, c, d, variance )

c*********************************************************************72
c
cc BURR_VARIANCE returns the variance of the Burr PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, D, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision d
      double precision k
      double precision r8_gamma
      double precision r8_huge
      double precision variance

      if ( c .le. 2.0D+00 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BURR_VARIANCE - Warning!'
        write ( *, '(a)' ) '  Variance undefined for C .le. 2.'
        variance = r8_huge ( )

      else

        k = r8_gamma ( d ) * r8_gamma ( 1.0D+00 - 2.0D+00 / c )       * 
     &r8_gamma ( d + 2.0D+00 / c )       - ( r8_gamma ( 1.0D+00 - 1.0D+0
     &0 / c ) * r8_gamma ( d + 1.0D+00 / c ) )**2

        variance = k * b * b / ( r8_gamma ( d ) )**2

      end if

      return
      end
      subroutine c4_normal_01_sample ( seed, x )

c*********************************************************************72
c
cc C4_NORMAL_01_SAMPLE samples the complex Normal 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2005
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
c    Output, complex X, a sample of the PDF.
c
      implicit none

      real r4_uniform_01
      real pi
      parameter ( pi = 3.141592653589793E+00 )
      integer seed
      real v1
      real v2
      complex x
      real x_c
      real x_r

      v1 = r4_uniform_01 ( seed )
      v2 = r4_uniform_01 ( seed )

      x_r = sqrt ( - 2.0E+00 * log ( v1 ) ) * cos ( 2.0E+00 * pi * v2 )
      x_c = sqrt ( - 2.0E+00 * log ( v1 ) ) * sin ( 2.0E+00 * pi * v2 )

      x = cmplx ( x_r, x_c )

      return
      end
      subroutine cardioid_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc CARDIOID_CDF evaluates the Cardioid CDF.
c
c  Discussion:
c
c    The angle X is assumed to lie between A - PI and A + PI.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    A - PI .le. X .le. A + PI.
c
c    Input, double precision A, B, the parameters.
c    -0.5 .le. B .le. 0.5.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .le. a - pi ) then
        cdf = 0.0D+00
      else if ( x .lt. a + pi ) then
        cdf = ( pi + x - a + 2.0D+00 * b * sin ( x - a ) ) / ( 2.0D+00 *
     & pi )
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine cardioid_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc CARDIOID_CDF_INV inverts the Cardioid CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0 .le. CDF .le. 1.
c
c    Input, double precision A, B, the parameters.
c    -0.5 .le. B .le. 0.5.
c
c    Output, double precision X, the argument with the given CDF.
c    A - PI .le. X .le. A + PI.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision fp
      double precision fx
      integer it
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision tol
      parameter ( tol = 0.000001D+00 )
      double precision x

      if ( cdf .le. 0.0D+00 ) then

        x = a - pi

      else if ( cdf .lt. 1.0D+00 ) then

        x = a

        it = 0

10      continue

          fx = cdf - ( pi + x - a + 2.0D+00 * b * sin ( x - a ) ) 
     &      / ( 2.0D+00 * pi )

          if ( abs ( fx ) .lt. tol ) then
            go to 20
          end if

          if ( 10 .lt. it ) then
            stop
          end if

          fp = - ( 1.0D+00 + 2.0D+00 * b * cos ( x - a ) ) 
     &      / ( 2.0D+00 * pi )

          x = x - fx / fp
          x = max ( x, a - pi )
          x = min ( x, a + pi )

          it = it + 1

        go to 10

20      continue

      else

        x = a + pi

      end if

      return
      end
      function cardioid_check ( a, b )

c*********************************************************************72
c
cc CARDIOID_CHECK checks the parameters of the Cardioid CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    -0.5 .le. B .le. 0.5.
c
c    Output, logical CARDIOID_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical cardioid_check

      if ( b .lt. -0.5D+00 .or. 0.5D+00 .lt. b ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CARDIOID_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. -0.5 or 0.5 .lt. B.'
        cardioid_check = .false.
        return
      end if

      cardioid_check = .true.

      return
      end
      subroutine cardioid_mean ( a, b, mean )

c*********************************************************************72
c
cc CARDIOID_MEAN returns the mean of the Cardioid PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    -0.5 .le. B .le. 0.5.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine cardioid_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc CARDIOID_PDF evaluates the Cardioid PDF.
c
c  Discussion:
c
c    The cardioid PDF can be thought of as being applied to points on
c    a circle.  Compare this distribution with the "Cosine PDF".
c
c    PDF(A,B;X) = ( 1 / ( 2 * PI ) ) * ( 1 + 2 * B * COS ( X - A ) )
c    for  A - PI .le. X .le. A + PI, -1/2 .le. B .le. 1/2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Fisher,
c    Statistical Analysis of Circular Data,
c    Cambridge, 1993.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A - PI .le. X .le. A + PI.
c
c    Input, double precision A, B, the parameters of the PDF.
c    -0.5 .le. B .le. 0.5.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      pdf = ( 1.0D+00 + 2.0D+00 * b * cos ( x - a ) ) / ( 2.0D+00 * pi )

      return
      end
      subroutine cardioid_sample ( a, b, seed, x )

c*********************************************************************72
c
cc CARDIOID_SAMPLE samples the Cardioid PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    -0.5 .le. B .le. 0.5.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c    A - PI .le. X .le. A + PI.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call cardioid_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine cardioid_variance ( a, b, variance )

c*********************************************************************72
c
cc CARDIOID_VARIANCE returns the variance of the Cardioid PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    -0.5 .le. B .le. 0.5.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = 0.0D+00

      return
      end
      subroutine cauchy_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc CAUCHY_CDF evaluates the Cauchy CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      cdf = 0.5D+00 + atan2 ( x - a, b ) / PI

      return
      end
      subroutine cauchy_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc CAUCHY_CDF_INV inverts the Cauchy CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CAUCHY_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a + b * tan ( pi * ( cdf - 0.5D+00 ) )

      return
      end
      subroutine cauchy_cdf_values ( n_data, mu, sigma, x, fx )

c*********************************************************************72
c
cc CAUCHY_CDF_VALUES returns some values of the Cauchy CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = CauchyDistribution [ mu, sigma ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision MU, the mean of the distribution.
c
c    Output, double precision SIGMA, the variance of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx_vec(n_max)
      double precision mu
      double precision mu_vec(n_max)
      integer n_data
      double precision sigma
      double precision sigma_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save mu_vec
      save sigma_vec
      save x_vec

      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.8524163823495667D+00,
     &  0.9220208696226307D+00,
     &  0.9474315432887466D+00,
     &  0.6475836176504333D+00,
     &  0.6024163823495667D+00,
     &  0.5779791303773693D+00,
     &  0.5628329581890012D+00,
     &  0.6475836176504333D+00,
     &  0.5000000000000000D+00,
     &  0.3524163823495667D+00,
     &  0.2500000000000000D+00 /
      data mu_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data sigma_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        mu = 0.0D+00
        sigma = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        mu = mu_vec(n_data)
        sigma = sigma_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function cauchy_check ( a, b )

c*********************************************************************72
c
cc CAUCHY_CHECK checks the parameters of the Cauchy CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical CAUCHY_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical cauchy_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CAUCHY_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        cauchy_check = .false.
        return
      end if

      cauchy_check = .true.

      return
      end
      subroutine cauchy_mean ( a, b, mean )

c*********************************************************************72
c
cc CAUCHY_MEAN returns the mean of the Cauchy PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine cauchy_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc CAUCHY_PDF evaluates the Cauchy PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = 1 / ( PI * B * ( 1 + ( ( X - A ) / B )^2 ) )
c
c    The Cauchy PDF is also known as the Breit-Wigner PDF.  It
c    has some unusual properties.  In particular, the integrals for the
c    expected value and higher order moments are "singular", in the
c    sense that the limiting values do not exist.  A result can be
c    obtained if the upper and lower limits of integration are set
c    equal to +T and -T, and the limit as T => +oo is taken, but
c    this is a very weak and unreliable sort of limit.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      y = ( x - a ) / b

      pdf = 1.0D+00 / ( pi * b * ( 1.0D+00 + y * y ) )

      return
      end
      subroutine cauchy_sample ( a, b, seed, x )

c*********************************************************************72
c
cc CAUCHY_SAMPLE samples the Cauchy PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call cauchy_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine cauchy_variance ( a, b, variance )

c*********************************************************************72
c
cc CAUCHY_VARIANCE returns the variance of the Cauchy PDF.
c
c  Discussion:
c
c    The variance of the Cauchy PDF is not well defined.  This routine
c    is made available for completeness only, and simply returns
c    a "very large" number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision r8_huge
      double precision variance

      variance = r8_huge ( )

      return
      end
      subroutine chi_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc CHI_CDF evaluates the Chi CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision gamma_inc
      double precision p2
      double precision x
      double precision x2
      double precision y

      if ( x .le. a ) then

        cdf = 0.0D+00

      else

        y = ( x - a ) / b
        x2 = 0.5D+00 * y * y
        p2 = 0.5D+00 * c

        cdf = gamma_inc ( p2, x2 )

      end if

      return
      end
      subroutine chi_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc CHI_CDF_INV inverts the Chi CDF.
c
c  Discussion:
c
c    A simple bisection method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision r8_huge
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = a
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
        return
      end if

      x1 = a
      cdf1 = 0.0D+00

      x2 = a + 1.0D+00

10    continue

        call chi_cdf ( x2, a, b, c, cdf2 )

        if ( cdf .lt. cdf2 ) then
          go to 20
        end if

        x2 = a + 2.0D+00 * ( x2 - a )

      go to 10

20    continue
c
c  Now use bisection.
c
      it = 0

30    continue

        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call chi_cdf ( x3, a, b, c, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          return
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'CHI_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          return
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 30

      return
      end
      function chi_check ( a, b, c )

c*********************************************************************72
c
cc CHI_CHECK checks the parameters of the Chi CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, logical CHI_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical chi_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0.'
        chi_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.0.'
        chi_check = .false.
        return
      end if

      chi_check = .true.

      return
      end
      subroutine chi_mean ( a, b, c, mean )

c*********************************************************************72
c
cc CHI_MEAN returns the mean of the Chi PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision MEAN, the mean value.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean
      double precision r8_gamma

      mean = a + sqrt ( 2.0D+00 ) * b * r8_gamma ( 0.5D+00 * ( c + 1.0D+
     &00 ) )     / r8_gamma ( 0.5D+00 * c )

      return
      end
      subroutine chi_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc CHI_PDF evaluates the Chi PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = EXP ( - 0.5D+00 * ( ( X - A ) / B )^2 )
c      * ( ( X - A ) / B )^( C - 1 ) /
c      ( 2^( 0.5D+00 * C - 1 ) * B * GAMMA ( 0.5D+00 * C ) )
c
c    CHI(A,B,1) is the Half Normal PDF;
c    CHI(0,B,2) is the Rayleigh PDF;
c    CHI(0,B,3) is the Maxwell PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision r8_gamma
      double precision x
      double precision y

      if ( x .le. a ) then

        pdf = 0.0D+00

      else

        y = ( x - a ) / b

        pdf = exp ( - 0.5D+00 * y * y ) * y**( c - 1.0D+00 ) /       ( 2
     &.0D+00**( 0.5D+00 * c - 1.0D+00 ) * b * r8_gamma ( 0.5D+00 * c ) )

      end if

      return
      end
      subroutine chi_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc CHI_SAMPLE samples the Chi PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      integer seed
      double precision x

      call chi_square_sample ( c, seed, x )

      x = a + b * sqrt ( x )

      return
      end
      subroutine chi_variance ( a, b, c, variance )

c*********************************************************************72
c
cc CHI_VARIANCE returns the variance of the Chi PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0 .lt. B,
c    0 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision r8_gamma
      double precision variance

      variance = b * b * ( c - 2.0D+00 *     ( r8_gamma ( 0.5D+00 * ( c 
     &+ 1.0D+00 ) ) / r8_gamma ( 0.5D+00 * c ) )**2 )

      return
      end
      subroutine chi_square_cdf ( x, a, cdf )

c*********************************************************************72
c
cc CHI_SQUARE_CDF evaluates the Chi squared CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the value of the random deviate.
c
c    Input, double precision A, the parameter of the distribution, usually
c    the number of degrees of freedom.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b2
      double precision c2
      double precision cdf
      double precision x
      double precision x2

      x2 = 0.5D+00 * x

      a2 = 0.0D+00
      b2 = 1.0D+00
      c2 = 0.5D+00 * a

      call gamma_cdf ( x2, a2, b2, c2, cdf )

      return
      end
      subroutine chi_square_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc CHI_SQUARE_CDF_INV inverts the Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 October 2004
c
c  Author:
c
c    Original FORTAN77 version by Donald Best, Roberts.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    Donald Best, Roberts,
c    The Percentage Points of the Chi-Squared Distribution,
c    Algorithm AS 91,
c    Applied Statistics,
c    Volume 24, Number ?, pages 385-390, 1975.
c
c  Parameters:
c
c    Input, double precision CDF, a value of the chi-squared cumulative
c    probability density function.
c    0.000002 .le. CDF .le. 0.999998.
c
c    Input, double precision A, the parameter of the chi-squared
c    probability density function.  0 .lt. A.
c
c    Output, double precision X, the value of the chi-squared random deviate
c    with the property that the probability that a chi-squared random
c    deviate with parameter A is less than or equal to X is CDF.
c
      implicit none

      double precision a
      double precision a2
      double precision aa
      parameter ( aa = 0.6931471806D+00 )
      double precision b
      double precision c
      double precision c1
      parameter ( c1 = 0.01D+00 )
      double precision c2
      parameter ( c2 = 0.222222D+00 )
      double precision c3
      parameter ( c3 = 0.32D+00 )
      double precision c4
      parameter ( c4 = 0.4D+00 )
      double precision c5
      parameter ( c5 = 1.24D+00 )
      double precision c6
      parameter ( c6 = 2.2D+00 )
      double precision c7
      parameter ( c7 = 4.67D+00 )
      double precision c8
      parameter ( c8 = 6.66D+00 )
      double precision c9
      parameter ( c9 = 6.73D+00 )
      double precision c10
      parameter ( c10 = 13.32D+00 )
      double precision c11
      parameter ( c11 = 60.0D+00 )
      double precision c12
      parameter ( c12 = 70.0D+00 )
      double precision c13
      parameter ( c13 = 84.0D+00 )
      double precision c14
      parameter ( c14 = 105.0D+00 )
      double precision c15
      parameter ( c15 = 120.0D+00 )
      double precision c16
      parameter ( c16 = 127.0D+00 )
      double precision c17
      parameter ( c17 = 140.0D+00 )
      double precision c18
      parameter ( c18 = 175.0D+00 )
      double precision c19
      parameter ( c19 = 210.0D+00 )
      double precision c20
      parameter ( c20 = 252.0D+00 )
      double precision c21
      parameter ( c21 = 264.0D+00 )
      double precision c22
      parameter ( c22 = 294.0D+00 )
      double precision c23
      parameter ( c23 = 346.0D+00 )
      double precision c24
      parameter ( c24 = 420.0D+00 )
      double precision c25
      parameter ( c25 = 462.0D+00 )
      double precision c26
      parameter ( c26 = 606.0D+00 )
      double precision c27
      parameter ( c27 = 672.0D+00 )
      double precision c28
      parameter ( c28 = 707.0D+00 )
      double precision c29
      parameter ( c29 = 735.0D+00 )
      double precision c30
      parameter ( c30 = 889.0D+00 )
      double precision c31
      parameter ( c31 = 932.0D+00 )
      double precision c32
      parameter ( c32 = 966.0D+00 )
      double precision c33
      parameter ( c33 = 1141.0D+00 )
      double precision c34
      parameter ( c34 = 1182.0D+00 )
      double precision c35
      parameter ( c35 = 1278.0D+00 )
      double precision c36
      parameter ( c36 = 1740.0D+00 )
      double precision c37
      parameter ( c37 = 2520.0D+00 )
      double precision c38
      parameter ( c38 = 5040.0D+00 )
      double precision cdf
      double precision cdf_max
      parameter ( cdf_max = 0.999998D+00 )
      double precision cdf_min
      parameter ( cdf_min = 0.000002D+00 )
      double precision ch
      double precision e
      parameter ( e = 0.0000005D+00 )
      double precision g
      double precision gamma_inc
      double precision gamma_log
      integer i
      integer it_max
      parameter ( it_max = 20 )
      double precision p1
      double precision p2
      double precision q
      double precision s1
      double precision s2
      double precision s3
      double precision s4
      double precision s5
      double precision s6
      double precision t
      double precision x
      double precision x2
      double precision xx

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        write ( *, '(a,g14.6)' ) '  CDF = ', cdf
        stop
      end if

      if ( cdf .lt. cdf_min ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Warning!'
        write ( *, '(a)' ) '  CDF .lt. CDF_MIN.'
        write ( *, '(a,g14.6)' ) '  CDF = ', cdf
        write ( *, '(a,g14.6)' ) '  CDF_MIN = ', cdf_min
      end if

      if ( cdf_max .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Warning!'
        write ( *, '(a)' ) '  CDF_MAX .lt. CDF.'
        write ( *, '(a,g14.6)' ) '  CDF = ', cdf
        write ( *, '(a,g14.6)' ) '  CDF_MAX = ', cdf_max
      end if

      xx = 0.5D+00 * a
      c = xx - 1.0D+00
c
c  Compute Log ( Gamma ( A/2 ) ).
c
      g = gamma_log ( a / 2.0D+00 )
c
c  Starting approximation for small chi-squared.
c
      if ( a .lt. - c5 * log ( cdf ) ) then

        ch = ( cdf * xx * exp ( g + xx * aa ) ) ** ( 1.0D+00 / xx )

        if ( ch .lt. e ) then
          x = ch
          return
        end if
c
c  Starting approximation for A less than or equal to 0.32.
c
      else if ( a .le. c3 ) then

        ch = c4
        a2 = log ( 1.0D+00 - cdf )

10      continue

          q = ch
          p1 = 1.0D+00 + ch * ( c7 + ch )
          p2 = ch * ( c9 + ch * ( c8 + ch ) )

          t = - 0.5D+00 + ( c7 + 2.0D+00 * ch ) / p1
     &      - ( c9 + ch * ( c10 + 3.0D+00 * ch ) ) / p2

          ch = ch - ( 1.0D+00 - exp ( a2 + g + 0.5D+00 * ch + c * aa )  
     &       * p2 / p1 ) / t

          if ( abs ( q / ch - 1.0D+00 ) .le. c1 ) then
            go to 20
          end if

        go to 10

20      continue
c
c  Call to algorithm AS 111.
c  Note that P has been tested above.
c  AS 241 could be used as an alternative.
c
      else

        call normal_01_cdf_inv ( cdf, x2 )
c
c  Starting approximation using Wilson and Hilferty estimate.
c
        p1 = c2 / a
        ch = a * ( x2 * sqrt ( p1 ) + 1.0D+00 - p1 ) ** 3
c
c  Starting approximation for P tending to 1.
c
        if ( c6 * a + 6.0D+00 .lt. ch ) then
          ch = - 2.0D+00 * ( log ( 1.0D+00 - cdf ) - c * log ( 0.5D+00 *
     & ch ) + g )
        end if

      end if
c
c  Call to algorithm AS 239 and calculation of seven term Taylor series.
c
      do i = 1, it_max

        q = ch
        p1 = 0.5D+00 * ch
        p2 = cdf - gamma_inc ( xx, p1 )
        t = p2 * exp ( xx * aa + g + p1 - c * log ( ch ) )
        b = t / ch
        a2 = 0.5D+00 * t - b * c

        s1 = ( c19 + a2        * ( c17 + a2        * ( c14 + a2        *
     & ( c13 + a2        * ( c12 + a2        *   c11 ) ) ) ) ) / c24

        s2 = ( c24 + a2        * ( c29 + a2        * ( c32 + a2        *
     & ( c33 + a2        *   c35 ) ) ) ) / c37

        s3 = ( c19 + a2        * ( c25 + a2        * ( c28 + a2        *
     &   c31 ) ) ) / c37

        s4 = ( c20 + a2        * ( c27 + a2        *   c34 ) + c        
     &* ( c22 + a2        * ( c30 + a2        *   c36 ) ) ) / c38

        s5 = ( c13 + c21 * a2 + c * ( c18 + c26 * a2 ) ) / c37

        s6 = ( c15 + c * ( c23 + c16 * c ) ) / c38

        ch = ch + t * ( 1.0D+00 + 0.5D+00 * t * s1 - b * c       * ( s1 
     &- b       * ( s2 - b       * ( s3 - b       * ( s4 - b       * ( s
     &5 - b       *   s6 ) ) ) ) ) )

        if ( e .lt. abs ( q / ch - 1.0D+00 ) ) then
          x = ch
          return
        end if

      end do

      x = ch
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Warning!'
      write ( *, '(a)' ) '  Convergence not reached.'

      return
      end
      subroutine chi_square_cdf_values ( n_data, a, x, fx )

c*********************************************************************72
c
cc CHI_SQUARE_CDF_VALUES returns some values of the Chi-Square CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = ChiSquareDistribution [ df ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer A, the parameter of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 21 )

      integer a
      integer a_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save fx_vec
      save x_vec

      data a_vec /
     &   1,  2,  1,  2,
     &   1,  2,  3,  4,
     &   1,  2,  3,  4,
     &   5,  3,  3,  3,
     &   3,  3, 10, 10,
     &  10 /
      data fx_vec /
     &  0.7965567455405796D-01,
     &  0.4987520807317687D-02,
     &  0.1124629160182849D+00,
     &  0.9950166250831946D-02,
     &  0.4729107431344619D+00,
     &  0.1812692469220181D+00,
     &  0.5975750516063926D-01,
     &  0.1752309630642177D-01,
     &  0.6826894921370859D+00,
     &  0.3934693402873666D+00,
     &  0.1987480430987992D+00,
     &  0.9020401043104986D-01,
     &  0.3743422675270363D-01,
     &  0.4275932955291202D+00,
     &  0.6083748237289110D+00,
     &  0.7385358700508894D+00,
     &  0.8282028557032669D+00,
     &  0.8883897749052874D+00,
     &  0.1721156299558408D-03,
     &  0.3659846827343712D-02,
     &  0.1857593622214067D-01 /
      data x_vec /
     &  0.01D+00,
     &  0.01D+00,
     &  0.02D+00,
     &  0.02D+00,
     &  0.40D+00,
     &  0.40D+00,
     &  0.40D+00,
     &  0.40D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  3.00D+00,
     &  4.00D+00,
     &  5.00D+00,
     &  6.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  3.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function chi_square_check ( a )

c*********************************************************************72
c
cc CHI_SQUARE_CHECK checks the parameter of the central Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the distribution.
c    1 .le. A.
c
c    Output, logical CHI_SQUARE_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical chi_square_check

      if ( a .lt. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_SQUARE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 1.0.'
        chi_square_check = .false.
        return
      end if

      chi_square_check = .true.

      return
      end
      subroutine chi_square_mean ( a, mean )

c*********************************************************************72
c
cc CHI_SQUARE_MEAN returns the mean of the central Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the distribution.
c    1 .le. A.
c
c    Output, double precision MEAN, the mean value.
c
      implicit none

      double precision a
      double precision mean

      mean = a

      return
      end
      subroutine chi_square_pdf ( x, a, pdf )

c*********************************************************************72
c
cc CHI_SQUARE_PDF evaluates the central Chi squared PDF.
c
c  Discussion:
c
c    PDF(A;X) =
c      EXP ( - X / 2 ) * X**((A-2)/2) / ( 2**(A/2) * GAMMA ( A/2 ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X
c
c    Input, double precision A, the parameter of the PDF.
c    1 .le. A.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision r8_gamma
      double precision x

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        b = a / 2.0D+00
        pdf = exp ( - 0.5D+00 * x ) * x**( b - 1.0D+00 )       / ( 2.0D+
     &00**b * r8_gamma ( b ) )
      end if

      return
      end
      subroutine chi_square_sample ( a, seed, x )

c*********************************************************************72
c
cc CHI_SQUARE_SAMPLE samples the central Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    1 .le. A.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b2
      double precision c2
      integer i
      integer it_max
      parameter ( it_max = 100 )
      integer n
      integer seed
      double precision x
      double precision x2

      n = int ( a )

      if ( dble ( n ) .eq. a .and. n .le. it_max ) then

        x = 0.0D+00
        do i = 1, n
          call normal_01_sample ( seed, x2 )
          x = x + x2 * x2
        end do

      else

        a2 = 0.0D+00
        b2 = 1.0D+00
        c2 = a / 2.0D+00

        call gamma_sample ( a2, b2, c2, seed, x )

        x = 2.0D+00 * x

      end if

      return
      end
      subroutine chi_square_variance ( a, variance )

c*********************************************************************72
c
cc CHI_SQUARE_VARIANCE returns the variance of the central Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the distribution.
c    1 .le. A.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision variance

      variance = 2.0D+00 * a

      return
      end
      function chi_square_noncentral_check ( a, b )

c*********************************************************************72
c
cc CHI_SQUARE_NONCENTRAL_CHECK check parameters of noncentral Chi Squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the parameter of the PDF.
c    1.0D+00 .le. A.
c
c    Input, double precision B, the noncentrality parameter of the PDF.
c    0.0D+00 .le. B.
c
c    Output, logical CHI_SQUARE_NONCENTRAL_CHECK, is true if the parameters
c    are legal.
c
      implicit none

      double precision a
      double precision b
      logical chi_square_noncentral_check

      if ( a .lt. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 1.'
        chi_square_noncentral_check = .false.
        return
      end if

      if ( b .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CHI_SQUARE_NONCENTRAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. 0.'
        chi_square_noncentral_check = .false.
        return
      end if

      chi_square_noncentral_check = .true.

      return
      end
      subroutine chi_square_noncentral_cdf_values ( n_data, df, lambda,
     &  x, cdf )

c*********************************************************************72
c
cc CHI_SQUARE_NONCENTRAL_CDF_VALUES returns values of the noncentral chi CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = NoncentralChiSquareDistribution [ df, lambda ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer DF, the number of degrees of freedom.
c
c    Output, double precision LAMBDA, the noncentrality parameter.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision CDF, the noncentral chi CDF.
c
      implicit none

      integer n_max
      parameter ( n_max = 28 )

      double precision cdf
      double precision cdf_vec(n_max)
      integer df
      integer df_vec(n_max)
      double precision lambda
      double precision lambda_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save cdf_vec
      save df_vec
      save lambda_vec
      save x_vec

      data cdf_vec /
     &  0.8399444269398261D+00,
     &  0.6959060300435139D+00,
     &  0.5350879697078847D+00,
     &  0.7647841496310313D+00,
     &  0.6206436532195436D+00,
     &  0.4691667375373180D+00,
     &  0.3070884345937569D+00,
     &  0.2203818092990903D+00,
     &  0.1500251895581519D+00,
     &  0.3071163194335791D-02,
     &  0.1763982670131894D-02,
     &  0.9816792594625022D-03,
     &  0.1651753140866208D-01,
     &  0.2023419573950451D-03,
     &  0.4984476352854074D-06,
     &  0.1513252400654827D-01,
     &  0.2090414910614367D-02,
     &  0.2465021206048452D-03,
     &  0.2636835050342939D-01,
     &  0.1857983220079215D-01,
     &  0.1305736595486640D-01,
     &  0.5838039534819351D-01,
     &  0.4249784402463712D-01,
     &  0.3082137716021596D-01,
     &  0.1057878223400849D+00,
     &  0.7940842984598509D-01,
     &  0.5932010895599639D-01,
     &  0.2110395656918684D+00 /
      data df_vec /
     &    1,   2,   3,
     &    1,   2,   3,
     &    1,   2,   3,
     &    1,   2,   3,
     &   60,  80, 100,
     &    1,   2,   3,
     &   10,  10,  10,
     &   10,  10,  10,
     &   10,  10,  10,
     &    8 /
      data lambda_vec /
     &    0.5D+00,
     &    0.5D+00,
     &    0.5D+00,
     &    1.0D+00,
     &    1.0D+00,
     &    1.0D+00,
     &    5.0D+00,
     &    5.0D+00,
     &    5.0D+00,
     &   20.0D+00,
     &   20.0D+00,
     &   20.0D+00,
     &   30.0D+00,
     &   30.0D+00,
     &   30.0D+00,
     &    5.0D+00,
     &    5.0D+00,
     &    5.0D+00,
     &    2.0D+00,
     &    3.0D+00,
     &    4.0D+00,
     &    2.0D+00,
     &    3.0D+00,
     &    4.0D+00,
     &    2.0D+00,
     &    3.0D+00,
     &    4.0D+00,
     &    0.5D+00 /
      data x_vec /
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &   3.000D+00,
     &  60.000D+00,
     &  60.000D+00,
     &  60.000D+00,
     &   0.050D+00,
     &   0.050D+00,
     &   0.050D+00,
     &   4.000D+00,
     &   4.000D+00,
     &   4.000D+00,
     &   5.000D+00,
     &   5.000D+00,
     &   5.000D+00,
     &   6.000D+00,
     &   6.000D+00,
     &   6.000D+00,
     &   5.000D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0.0D+00
        lambda = 0.0D+00
        df = 0
        cdf = 0.0D+00
      else
        x = x_vec(n_data)
        lambda = lambda_vec(n_data)
        df = df_vec(n_data)
        cdf = cdf_vec(n_data)
      end if

      return
      end
      subroutine chi_square_noncentral_mean ( a, b, mean )

c*********************************************************************72
c
cc CHI_SQUARE_NONCENTRAL_MEAN: mean of the noncentral Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the parameter of the PDF.
c    1.0D+00 .le. A.
c
c    Input, double precision B, the noncentrality parameter of the PDF.
c    0.0D+00 .le. B.
c
c    Output, double precision MEAN, the mean value.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a + b

      return
      end
      subroutine chi_square_noncentral_sample ( a, b, seed, x )

c*********************************************************************72
c
cc CHI_SQUARE_NONCENTRAL_SAMPLE samples the noncentral Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the parameter of the PDF.
c    1.0D+00 .le. A.
c
c    Input, double precision B, the noncentrality parameter of the PDF.
c    0.0D+00 .le. B.
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
      double precision b
      double precision b2
      integer seed
      double precision x
      double precision x1
      double precision x2

      a1 = a - 1.0D+00

      call chi_square_sample ( a1, seed, x1 )

      a2 = sqrt ( b )
      b2 = 1.0D+00
      call normal_sample ( a2, b2, seed, x2 )

      x = x1 + x2 * x2

      return
      end
      subroutine chi_square_noncentral_variance ( a, b, variance )

c*********************************************************************72
c
cc CHI_SQUARE_NONCENTRAL_VARIANCE: variance of the noncentral Chi squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    1 .le. A.
c
c    Input, double precision B, the noncentrality parameter of the PDF.
c    0.0D+00 .le. B.
c
c    Output, double precision VARIANCE, the variance value.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = 2.0D+00 * ( a + 2.0D+00 * b )

      return
      end
      subroutine circle_sample ( a, b, c, seed, x1, x2 )

c*********************************************************************72
c
cc CIRCLE_SAMPLE samples points from a circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the circle.
c    The circle is centered at (A,B) and has radius C.
c    0.0D+00 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X1, X2, a sampled point of the circle.
c
      implicit none

      double precision a
      double precision angle
      double precision b
      double precision c
      double precision r8_uniform_01
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision radius_frac
      integer seed
      double precision x1
      double precision x2

      radius_frac = r8_uniform_01 ( seed )
      radius_frac = sqrt ( radius_frac )

      angle = 2.0D+00 * pi * r8_uniform_01 ( seed )

      x1 = a + c * radius_frac * cos ( angle )
      x2 = b + c * radius_frac * sin ( angle )

      return
      end
      subroutine circular_normal_01_mean ( mean )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_01_MEAN returns the mean of the Circular Normal 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN(2), the mean of the PDF.
c
      implicit none

      double precision mean(2)

      mean(1) = 0.0D+00
      mean(2) = 0.0D+00

      return
      end
      subroutine circular_normal_01_pdf ( x, pdf )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_01_PDF evaluates the Circular Normal 01 PDF.
c
c  Discussion:
c
c    PDF(X) = EXP ( - 0.5D+00 * ( X(1)^2 + X(2)^2 ) ) / ( 2 * PI )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X(2), the argument of the PDF.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(2)

      pdf = exp ( - 0.5D+00 * ( x(1)**2 + x(2)**2 ) ) / ( 2.0D+00 * pi )

      return
      end
      subroutine circular_normal_01_sample ( seed, x )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_01_SAMPLE samples the Circular Normal 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
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
c    Output, double precision X(2), a sample of the PDF.
c
      implicit none

      double precision r8_uniform_01
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision v1
      double precision v2
      double precision x(2)

      v1 = r8_uniform_01 ( seed )
      v2 = r8_uniform_01 ( seed )

      x(1) = sqrt ( - 2.0D+00 * log ( v1 ) ) * cos ( 2.0D+00 * pi * v2 )
      x(2) = sqrt ( - 2.0D+00 * log ( v1 ) ) * sin ( 2.0D+00 * pi * v2 )

      return
      end
      subroutine circular_normal_01_variance ( variance )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_01_VARIANCE: variance of the Circular Normal 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE(2), the variance of the PDF.
c
      implicit none

      double precision variance(2)

      variance(1) = 1.0D+00
      variance(2) = 1.0D+00

      return
      end
      subroutine circular_normal_mean ( a, b, mean )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_MEAN returns the mean of the Circular Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(2), a parameter of the PDF, the mean value.
c
c    Input, double precision B, a parameter of the PDF, the standard deviation.
c
c    Output, double precision MEAN(2), the mean of the PDF.
c
      implicit none

      double precision a(2)
      double precision b
      double precision mean(2)

      mean(1) = a(1)
      mean(2) = a(2)

      return
      end
      subroutine circular_normal_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_PDF evaluates the Circular Normal PDF.
c
c  Discussion:
c
c    PDF(X) = EXP ( - 0.5D+00 * ( ( (X(1)-A(1))^2 + (X(2)-A(2))^2 ) / B^2 )
c      / ( 2 * PI * B^2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X(2), the argument of the PDF.
c
c    Input, double precision A(2), a parameter of the PDF, the mean value.
c
c    Input, double precision B, a parameter of the PDF, the standard deviation.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a(2)
      double precision b
      double precision d
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(2)

      d = ( ( x(1) - a(1) )**2 + ( x(2) - a(2) )**2 ) / b**2

      pdf = exp ( - 0.5D+00 * d ) / ( 2.0D+00 * b**2 * pi )

      return
      end
      subroutine circular_normal_sample ( a, b, seed, x )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_SAMPLE samples the Circular Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(2), a parameter of the PDF, the mean value.
c
c    Input, double precision B, a parameter of the PDF, the standard deviation.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(2), a sample of the PDF.
c
      implicit none

      double precision a(2)
      double precision b
      double precision r
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision v1
      double precision v2
      double precision x(2)

      v1 = r8_uniform_01 ( seed )
      v2 = r8_uniform_01 ( seed )

      r = sqrt ( - 2.0D+00 * log ( v1 ) )

      x(1) = a(1) + b * r * cos ( 2.0D+00 * pi * v2 )
      x(2) = a(2) + b * r * sin ( 2.0D+00 * pi * v2 )

      return
      end
      subroutine circular_normal_variance ( a, b, variance )

c*********************************************************************72
c
cc CIRCULAR_NORMAL_VARIANCE returns the variance of the Circular Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(2), a parameter of the PDF, the mean value.
c
c    Input, double precision B, a parameter of the PDF, the standard deviation.
c
c    Output, double precision VARIANCE(2), the variance of the PDF.
c
      implicit none

      double precision a(2)
      double precision b
      double precision variance(2)

      variance(1) = b**2
      variance(2) = b**2

      return
      end
      function combinatorial ( n, k )

c*********************************************************************72
c
cc COMBINATORIAL computes the binomial coefficient C(N,K).
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in integer arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    April, 1963.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, integer COMBINATORIAL, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer combinatorial
      integer i
      integer k
      integer mn
      integer mx
      integer n
      integer value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0

      else if ( mn .eq. 0 ) then

        value = 1

      else

        mx = max ( k, n - k )
        value = mx + 1

        do i = 2, mn
          value = ( value * ( mx + i ) ) / i
        end do

      end if

      combinatorial = value

      return
      end
      subroutine cosine_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc COSINE_CDF evaluates the Cosine CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      if ( x .le. a - pi * b ) then

        cdf = 0.0D+00

      else if ( x .le. a + pi * b ) then

        y = ( x - a ) / b

        cdf = 0.5D+00 + ( y + sin ( y ) ) / ( 2.0D+00 * pi )

      else if ( a + pi * b .lt. x ) then

        cdf = 1.0D+00

      end if

      return
      end
      subroutine cosine_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc COSINE_CDF_INV inverts the Cosine CDF.
c
c  Discussion:
c
c    A simple bisection method is used on the interval
c    [ A - PI * B, A + PI * B ].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COSINE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = a - pi * b
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = a + pi * b
        return
      end if

      x1 = a - pi * b
      cdf1 = 0.0D+00

      x2 = a + pi * b
      cdf2 = 1.0D+00
c
c  Now use bisection.
c
      it = 0

      do it = 1, it_max

        x3 = 0.5D+00 * ( x1 + x2 )
        call cosine_cdf ( x3, a, b, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          return
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COSINE_CDF_INV - Fatal error!'
      write ( *, '(a)' ) '  Iteration limit exceeded.'

      stop
      end
      function cosine_check ( a, b )

c*********************************************************************72
c
cc COSINE_CHECK checks the parameters of the Cosine CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical COSINE_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical cosine_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COSINE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0'
        cosine_check = .false.
        return
      end if

      cosine_check = .true.

      return
      end
      subroutine cosine_mean ( a, b, mean )

c*********************************************************************72
c
cc COSINE_MEAN returns the mean of the Cosine PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine cosine_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc COSINE_PDF evaluates the Cosine PDF.
c
c  Discussion:
c
c    The cosine PDF can be thought of as being applied to points on
c    a circle.
c
c    PDF(A,B;X) = ( 1 / ( 2 * PI * B ) ) * COS ( ( X - A ) / B )
c    for A - PI * B .le. X .le. A + PI * B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      if ( x .lt. a - pi * b ) then
        pdf = 0.0D+00

      else if ( x .le. a + pi * b ) then

        y = ( x - a ) / b

        pdf = 1.0D+00 / ( 2.0D+00 * pi * b ) * cos ( y )

      else if ( a + pi * b .lt. x ) then

        pdf = 0.0D+00

      end if

      return
      end
      subroutine cosine_sample ( a, b, seed, x )

c*********************************************************************72
c
cc COSINE_SAMPLE samples the Cosine PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call cosine_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine cosine_variance ( a, b, variance )

c*********************************************************************72
c
cc COSINE_VARIANCE returns the variance of the Cosine PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = ( pi * pi / 3.0D+00 - 2.0D+00 ) * b * b

      return
      end
      subroutine coupon_complete_pdf ( type_num, box_num, pdf )

c*********************************************************************72
c
cc COUPON_COMPLETE_PDF evaluates the Complete Coupon Collection PDF.
c
c  Discussion:
c
c    PDF(TYPE_NUM;BOX_NUM) is the probability that, given an inexhaustible
c    supply of boxes, inside each of which there is one of TYPE_NUM distinct
c    coupons, which are uniformly distributed among the boxes, that it will
c    require opening exactly BOX_NUM boxes to achieve at least one of each
c    kind of coupon.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Herbert Wilf,
c    Some New Aspects of the Coupon Collector's Problem,
c    SIAM Review,
c    Volume 48, Number 3, September 2006, pages 549-565.
c
c  Parameters:
c
c    Input, integer BOX_NUM, the number of boxes that had to be
c    opened in order to just get at least one of each coupon.
c    0 .le. BOX_NUM.  If BOX_NUM .lt. TYPE_NUM, then PDF is surely 0.
c
c    Input, integer TYPE_NUM, the number of distinct coupons.
c    1 .le. TYPE_NUM.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer box_num
      double precision factor
      integer i
      double precision pdf
      integer stirling2_value
      integer type_num
c
c  Nonsense cases.
c
      if ( box_num .lt. 0 ) then

        pdf = 0.0D+00

      else if ( type_num .lt. 1 ) then

        pdf = 0.0D+00
c
c  Degenerate but meaningful case.
c
      else if ( type_num .eq. 1 ) then

        if ( box_num .eq. 1 ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if
c
c  Easy cases.
c
      else if ( box_num .lt. type_num ) then

        pdf = 0.0D+00
c
c  General case.
c
      else

        factor = 1.0D+00
        do i = 1, type_num
          factor = factor * dble ( i ) / dble ( type_num )
        end do
        do i = type_num+1, box_num
          factor = factor / dble ( type_num )
        end do

        pdf = factor * 
     &    dble ( stirling2_value ( box_num - 1, type_num - 1 ) )

      end if

      return
      end
      subroutine coupon_mean ( j, type_num, mean )

c*********************************************************************72
c
cc COUPON_MEAN returns the mean of the Coupon PDF.
c
c  Discussion:
c
c    In this version of the coupon collector's problem, we assume
c    that each box contains 1 coupon, that there are TYPE_NUM distinct types
c    of coupon, uniformly distributed among an inexhaustible supply
c    of boxes, and that the collector's goal is to get J distinct
c    types of coupons by opening one box after another.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer J, the number of distinct coupons to be
c    collected.  J must be between 1 and TYPE_NUM.
c
c    Input, integer TYPE_NUM, the number of distinct coupons.
c
c    Output, double precision MEAN, the mean number of boxes that
c    must be opened in order to just get J distinct kinds.
c
      implicit none

      integer i
      integer j
      integer n
      double precision mean
      integer type_num

      if ( type_num .lt. j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COUPON_MEAN - Fatal error!'
        write ( *, '(a)' ) '  Number of distinct coupons desired must be
     & no more'
        write ( *, '(a)' ) '  than the total number of distinct coupons.
     &'
        stop
      end if

      mean = 0.0D+00
      do i = 1, j
        mean = mean + 1.0D+00 / dble ( type_num - i + 1 )
      end do
      mean = mean * dble ( type_num )

      return
      end
      subroutine coupon_simulate ( type_num, seed, coupon, box_num )

c*********************************************************************72
c
cc COUPON_SIMULATE simulates the coupon collector's problem.
c
c  Discussion:
c
c    The coupon collector needs to collect one of each of TYPE_NUM
c    coupons.  The collector may draw one coupon (or, in some settings,
c    open one box) on each trial, and takes as many trials as necessary
c    to complete the task.  On each trial, the probability of picking
c    any particular type of coupon is always 1 / TYPE_NUM.
c
c    Interesting questions include;
c
c    * what is the expected number of drawings necessary to complete
c      the collection?
c
c    * How does the expected number of drawings necessary to complete
c      the collection vary as TYPE_NUM increases?
c
c    * What is the distribution of the numbers of each type of coupon
c      in a typical collection when it is just completed?
c
c    As TYPE_NUM increases, the number of coupons necessary to be
c    collected in order to get a complete set in any simulation
c    strongly tends to the value TYPE_NUM * LOG ( TYPE_NUM ).
c
c    If TYPE_NUM is 1, the simulation ends with a single drawing.
c
c    If TYPE_NUM is 2, then we may call the coupon taken on the first drawing
c    a "Head", say, and the process then is similar to the question of the
c    length, plus one, of a run of Heads or Tails in coin flipping.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 May 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TYPE_NUM, the number of types of coupons.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, integer COUPON(TYPE_NUM), the number of coupons
c    of each type that were collected during the simulation.
c
c    Output, integer BOX_NUM, the total number of boxes opened.
c
      implicit none

      integer type_num

      integer box_max
      parameter ( box_max = 2000 )
      integer box_num
      integer coupon(type_num)
      integer i
      integer i4_uniform_ab
      integer seed
      integer straight

      do i = 1, type_num
        coupon(i) = 0
      end do

      straight = 0
      box_num = 0
c
c  Draw another coupon.
c
10    continue

      if ( box_num .lt. box_max ) then

        i = i4_uniform_ab ( 1, type_num, seed )
c
c  Increment the number of I coupons.
c
        coupon(i) = coupon(i) + 1
        box_num = box_num + 1
c
c  If I is the next one we needed, increase STRAIGHT by 1.
c
        if ( i .eq. straight + 1 ) then

20        continue

            straight = straight + 1
c
c  If STRAIGHT = TYPE_NUM, we have all of them.
c
            if ( type_num .le. straight ) then
              return
            end if
c
c  If the next coupon has not been collected, our straight is over.
c
            if ( coupon(straight+1) .le. 0 ) then
              go to 30
            end if

          go to 20

30        continue

        end if

        go to 10

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COUPON_SIMULATE - Fatal error!'
      write ( *, '(a)' ) 
     &  '  Maximum number of coupons drawn without success.'

      stop
      end
      subroutine coupon_variance ( j, type_num, variance )

c*********************************************************************72
c
cc COUPON_VARIANCE returns the variance of the Coupon PDF.
c
c  Discussion:
c
c    In this version of the coupon collector's problem, we assume
c    that each box contains 1 coupon, that there are TYPE_NUM distinct types
c    of coupon, uniformly distributed among an inexhaustible supply
c    of boxes, and that the collector's goal is to get J distinct
c    types of coupons by opening one box after another.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer J, the number of distinct coupons to be
c    collected.  1 .le. J .le. TYPE_NUM.
c
c    Input, integer TYPE_NUM, the number of types of coupons.
c
c    Output, double precision VARIANCE, the variance of the number of
c    boxes that must be opened in order to just get J distinct kinds
c    of coupons.
c
      implicit none

      integer i
      integer j
      integer type_num
      double precision variance

      if ( type_num .lt. j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COUPON_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  Number of distinct coupons desired must be
     & no more'
        write ( *, '(a)' ) '  than the total number of distinct coupons.
     &'
        stop
      end if

      variance = 0.0D+00
      do i = 1, j
        variance = variance + dble ( i - 1 )
     &    / dble ( type_num - i + 1 )**2
      end do
      variance = variance * dble ( type_num )

      return
      end
      subroutine deranged_cdf ( x, a, cdf )

c*********************************************************************72
c
cc DERANGED_CDF evaluates the Deranged CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the maximum number of items in
c    their correct places.
c    0 .le. X .le. A.
c
c    Input, integer A, the number of items.
c    1 .le. A.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer a
      double precision cdf
      integer cnk
      integer deranged_enum
      integer dnmk
      double precision i4_factorial
      integer sum2
      integer x
      integer x2

      if ( x .lt. 0 .or. a .lt. x ) then
        cdf = 0.0D+00
      else
        sum2 = 0
        do x2 = 0, x
          call binomial_coef ( a, x2, cnk )
          dnmk = deranged_enum ( a - x2 )
          sum2 = sum2 + cnk * dnmk
        end do
        cdf = dble ( sum2 ) / i4_factorial ( a )
      end if

      return
      end
      subroutine deranged_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc DERANGED_CDF_INV inverts the Deranged CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, integer A, the number of items.
c    1 .le. A.
c
c    Output, integer X, the corresponding argument.
c
      implicit none

      integer a
      double precision cdf
      double precision cdf2
      double precision pdf
      integer x
      integer x2

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DERANGED_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cdf2 = 0.0D+00

      do x2 = 0, a

        call deranged_pdf ( x2, a, pdf )

        cdf2 = cdf2 + pdf

        if ( cdf .le. cdf2 ) then
          x = x2
          return
        end if

      end do

      x = a

      return
      end
      function deranged_check ( a )

c*********************************************************************72
c
cc DERANGED_CHECK checks the parameter of the Deranged PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the total number of items.
c    1 .le. A.
c
c    Output, logical DERANGED_CHECK, is true if the parameters are legal.
c
      implicit none

      integer a
      logical deranged_check

      if ( a .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DERANGED_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 1.'
        deranged_check = .false.
        return
      end if

      deranged_check = .true.

      return
      end
      function deranged_enum ( n )

c*********************************************************************72
c
cc DERANGED_ENUM returns the number of derangements of N objects.
c
c  Discussion:
c
c    A derangement of N objects is a permutation with no fixed
c    points.  If we symbolize the permutation operation by "P",
c    then for a derangment, P(I) is never equal to I.
c
c      D(0) = 1
c      D(1) = 0
c      D(2) = 1
c      D(N) = (N-1) * ( D(N-1) + D(N-2) )
c
c    or
c
c      D(0) = 1
c      D(1) = 0
c      D(N) = N * D(N-1) + (-1)^N
c
c    D(N) = N! * ( 1 - 1/1! + 1/2! - 1/3! ... 1/N! )
c
c    Based on the inclusion/exclusion law.
c
c    D(N) is the number of ways of placing N non-attacking rooks on
c    an N by N chessboard with one diagonal deleted.
c
c    Limit ( N -> Infinity ) D(N)/N! = 1 / e.
c
c    The number of permutations with exactly K items in the right
c    place is COMB(N,K) * D(N-K).
c
c  First values:
c
c     N         D(N)
c     0           1
c     1           0
c     2           1
c     3           2
c     4           9
c     5          44
c     6         265
c     7        1854
c     8       14833
c     9      133496
c    10     1334961
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of objects to be permuted.
c
c    Output, integer DERANGED_ENUM, the number of derangements
c    of N objects.
c
      implicit none

      integer deranged_enum
      integer dn
      integer dnm1
      integer dnm2
      integer i
      integer n

      if ( n .lt. 0 ) then

        dn = 0

      else if ( n .eq. 0 ) then

        dn = 1

      else if ( n .eq. 1 ) then

        dn = 0

      else if ( n .eq. 2 ) then

        dn = 1

      else

        dnm1 = 0
        dn = 1

        do i = 3, n
          dnm2 = dnm1
          dnm1 = dn
          dn = ( i - 1 ) * ( dnm1 + dnm2 )
        end do

      end if

      deranged_enum = dn

      return
      end
      subroutine deranged_mean ( a, mean )

c*********************************************************************72
c
cc DERANGED_MEAN returns the mean of the Deranged CDF.
c
c  Discussion:
c
c    The mean is computed by straightforward summation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of items.
c    1 .le. A.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer a
      double precision mean
      double precision pdf
      integer x

      mean = 0.0D+00
      do x = 1, a
        call deranged_pdf ( x, a, pdf )
        mean = mean + pdf * x
      end do

      return
      end
      subroutine deranged_pdf ( x, a, pdf )

c*********************************************************************72
c
cc DERANGED_PDF evaluates the Deranged PDF.
c
c  Discussion:
c
c    PDF(A;X) is the probability that exactly X items will occur in
c    their proper place after a random permutation of A items.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the number of items in their
c    correct places.  0 .le. X .le. A.
c
c    Input, integer A, the total number of items.
c    1 .le. A.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      integer cnk
      integer deranged_enum
      integer dnmk
      double precision i4_factorial
      double precision pdf
      integer x

      if ( x .lt. 0 .or. a .lt. x ) then
        pdf = 0.0D+00
      else
        call binomial_coef ( a, x, cnk )
        dnmk = deranged_enum ( a-x )
        pdf = dble ( cnk * dnmk ) / i4_factorial ( a )
      end if

      return
      end
      subroutine deranged_sample ( a, seed, x )

c*********************************************************************72
c
cc DERANGED_SAMPLE samples the Deranged PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of items.
c    1 .le. A.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      integer a
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call deranged_cdf_inv ( cdf, a, x )

      return
      end
      subroutine deranged_variance ( a, variance )

c*********************************************************************72
c
cc DERANGED_VARIANCE returns the variance of the Deranged CDF.
c
c  Discussion:
c
c    The variance is computed by straightforward summation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of items.
c    1 .le. A.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer a
      double precision mean
      double precision pdf
      integer x
      double precision variance

      call deranged_mean ( a, mean )

      variance = 0.0D+00
      do x = 1, a
        call deranged_pdf ( x, a, pdf )
        variance = variance + pdf * ( x - mean )**2
      end do

      return
      end
      function digamma ( x )

c*********************************************************************72
c
cc DIGAMMA calculates the digamma or Psi function.
c
c  Discussion:
c
c    DiGamma ( X ) = d ( log ( Gamma ( X ) ) ) / dX
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    Original FORTRAN77 version by Jose Bernardo.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Jose Bernardo,
c    Algorithm AS 103:
c    Psi ( Digamma ) Function,
c    Applied Statistics,
c    Volume 25, Number 3, pages 315-317, 1976.
c
c  Parameters:
c
c    Input, double precision X, the argument of the digamma function.
c    0 .lt. X.
c
c    Output, double precision DIGAMMA, the value of the digamma function at X.
c
      implicit none

      double precision c
      parameter ( c = 8.5D+00 )
      double precision d1
      parameter ( d1 = -0.5772156649D+00 )
      double precision digamma
      double precision r
      double precision, parameter :: s = 0.00001D+00
      double precision, parameter :: s3 = 0.08333333333D+00
      double precision, parameter :: s4 = 0.0083333333333D+00
      double precision, parameter :: s5 = 0.003968253968D+00
      double precision x
      double precision y
c
c  The argument must be positive.
c
      if ( x .le. 0.0D+00 ) then

        digamma = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIGAMMA - Fatal error!'
        write ( *, '(a)' ) '  X .le. 0.'
        stop
c
c  Use approximation if argument .le. S.
c
      else if ( x .le. s ) then

        digamma = d1 - 1.0D+00 / x
c
c  Reduce the argument to DIGAMMA(X + N) where C .le. (X + N).
c
      else

        digamma = 0.0D+00
        y = x

10      continue

        if ( y .lt. c ) then
          digamma = digamma - 1.0D+00 / y
          y = y + 1.0D+00
          go to 10
        end if
c
c  Use Stirling's (actually de Moivre's) expansion if C .lt. argument.
c
        r = 1.0D+00 / ( y * y )
        digamma = digamma + log ( y ) - 0.5D+00 / y - r * ( s3 - r
     &    * ( s4 - r * s5 ) )

      end if

      return
      end
      subroutine dipole_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc DIPOLE_CDF evaluates the Dipole CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A is arbitrary, but represents an angle, so only 0 .le. A .le. 2 * PI
c    is interesting, and -1.0D+00 .le. B .le. 1.0.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      cdf = 0.5D+00 + ( 1.0D+00 / pi ) * atan ( x )     + b * b * ( x * 
     &cos ( 2.0D+00 * a )     - sin ( 2.0D+00 * a ) ) / ( pi * ( 1.0D+00
     & + x * x ) )

      return
      end
      subroutine dipole_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc DIPOLE_CDF_INV inverts the Dipole CDF.
c
c  Discussion:
c
c    A simple bisection method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    -1.0D+00 .le. B .le. 1.0.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision r8_huge
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIPOLE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = - r8_huge ( )
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
        return
      end if
c
c  Seek X1 .lt. X .lt. X2.
c
      x1 = - 1.0D+00

10    continue

        call dipole_cdf ( x1, a, b, cdf1 )

        if ( cdf1 .le. cdf ) then
          go to 20
        end if

        x1 = 2.0D+00 * x1

      go to 10

20    continue

      x2 = 1.0D+00

30    continue

        call dipole_cdf ( x2, a, b, cdf2 )

        if ( cdf .le. cdf2 ) then
          go to 40
        end if

        x2 = 2.0D+00 * x2

      go to 30

40    continue
c
c  Now use bisection.
c
      it = 0

50    continue

        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call dipole_cdf ( x3, a, b, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          go to 60
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIPOLE_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          stop
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 50

60    continue

      return
      end
      function dipole_check ( a, b )

c*********************************************************************72
c
cc DIPOLE_CHECK checks the parameters of the Dipole CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A is arbitrary, but represents an angle, so only 0 .le. A .le. 2 * PI
c    is interesting, and -1.0D+00 .le. B .le. 1.0.
c
c    Output, logical DIPOLE_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical dipole_check

      if ( b .lt. -1.0D+00 .or. 1.0D+00 .lt. b ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIPOLE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  -1.0D+00 .le. B .le. 1.0D+00 is required.'
        write ( *, '(a,g14.6)' ) '  The input B = ', b
        dipole_check = .false.
        return
      end if

      dipole_check = .true.

      return
      end
      subroutine dipole_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc DIPOLE_PDF evaluates the Dipole PDF.
c
c  Discussion:
c
c    PDF(A,B;X) =
c        1 / ( PI * ( 1 + X^2 ) )
c      + B^2 * ( ( 1 - X^2 ) * cos ( 2 * A ) + 2 * X * sin ( 2 * A ) )
c      / ( PI * ( 1 + X^2 )^2 )
c
c    Densities of this kind commonly occur in the analysis of resonant
c    scattering of elementary particles.
c
c    DIPOLE_PDF(A,0;X) = CAUCHY_PDF(A;X)
c
c    A = 0, B = 1 yields the single channel dipole distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Knop,
c    Algorithm 441,
c    Random Deviates from the Dipole Distribution,
c    ACM Transactions on Mathematical Software,
c    Volume 16, Number 1, 1973, page 51.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A is arbitrary, but represents an angle, so only 0 .le. A .le. 2 * PI
c      is interesting,
c    and -1.0D+00 .le. B .le. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      pdf = 1.0D+00 / ( pi * ( 1.0D+00 + x * x ) )     + b * b * ( ( 1.0
     &D+00 - x * x ) * cos ( 2.0D+00 * a )     + 2.0D+00 * x * sin ( 2.0
     &D+00 * x ) ) / ( pi * ( 1.0D+00 + x * x )**2 )

      return
      end
      subroutine dipole_sample ( a, b, seed, x )

c*********************************************************************72
c
cc DIPOLE_SAMPLE samples the Dipole PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Knop,
c    Algorithm 441,
c    Random Deviates from the Dipole Distribution,
c    ACM Transactions on Mathematical Software,
c    Volume 16, Number 1, 1973, page 51.
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A is arbitrary, but represents an angle, so only 0 .le. A .le. 2 * PI
c      is interesting,
c    and -1.0D+00 .le. B .le. 1.0D+00.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision b2
      double precision c2
      integer seed
      double precision x
      double precision x1
      double precision x2
c
c  Find (X1,X2) at random in a circle.
c
      a2 = b * sin ( a )
      b2 = b * cos ( a )
      c2 = 1.0D+00

      call circle_sample ( a2, b2, c2, seed, x1, x2 )
c
c  The dipole variate is the ratio X1 / X2.
c
      x = x1 / x2

      return
      end
      function dirichlet_check ( n, a )

c*********************************************************************72
c
cc DIRICHLET_CHECK checks the parameters of the Dirichlet PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
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
c    Each A(I) should be positive.
c
c    Output, logical DIRICHLET_CHECK, is true if the parameters are legal.
c
      implicit none

      integer n

      double precision a(n)
      logical dirichlet_check
      integer i
      logical positive

      positive = .false.

      do i = 1, n

        if ( a(i) .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIRICHLET_CHECK - Fatal error!'
          write ( *, '(a)' ) '  A(I) .le. 0.'
          write ( *, '(a,i8)' ) '  For I = ', i
          write ( *, '(a,g14.6)' ) '  A(I) = ', a(i)
          dirichlet_check = .false.
          return
        else if ( 0.0D+00 .lt. a(i) ) then
          positive = .true.
        end if

      end do

      if ( .not. positive ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_CHECK - Fatal error!'
        write ( *, '(a)' ) '  All parameters are zero!'
        dirichlet_check = .false.
        return
      end if

      dirichlet_check = .true.

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
c    23 November 1999
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
c    Each A(I) should be positive.
c
c    Output, double precision MEAN(N), the means of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision mean(n)

      do i = 1, n
        mean(i) = a(i)
      end do

      call r8vec_unit_sum ( n, mean )

      return
      end
      function dirichlet_mix_check ( comp_num, elem_num, a, comp_weight 
     &)

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
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer COMP_NUM, the number of components in the
c    Dirichlet
c    mixture density, that is, the number of distinct Dirichlet PDF's
c    that are mixed together.
c
c    Input, integer ELEM_NUM, the number of elements of an
c    observation.
c
c    Input, double precision A(ELEM_NUM,COMP_NUM), the probabilities
c    for element ELEM_NUM in component COMP_NUM.
c    Each A(I,J) should be positive.
c
c    Input, double precision COMP_WEIGHT(COMP_NUM), the mixture weights of
c    the densities.  These do not need to be normalized.  The weight of a
c    given component is the relative probability that that component will
c    be used to generate the sample.
c
c    Output, logical DIRICHLET_MIX_CHECK, is true if the parameters are legal.
c
      implicit none

      integer comp_num
      integer elem_num

      double precision a(elem_num,comp_num)
      integer comp_i
      double precision comp_weight(comp_num)
      logical dirichlet_mix_check
      integer elem_i
      logical positive

      do comp_i = 1, comp_num

        do elem_i = 1, elem_num
          if ( a(elem_i,comp_i) .le. 0.0D+00 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Fatal error!'
            write ( *, '(a)' ) '  A(ELEM,COMP) .le. 0.'
            write ( *, '(a,i8)' ) '  COMP = ', comp_i
            write ( *, '(a,i8)' ) '  ELEM = ', elem_i
            write ( *, '(a,g14.6)' ) '  A(COMP,ELEM) = ', a(elem_i,comp_
     &i)
            dirichlet_mix_check = .false.
            return
          end if
        end do

      end do

      positive = .false.

      do comp_i = 1, comp_num

        if ( comp_weight(comp_i) .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Fatal error!'
          write ( *, '(a)' ) '  COMP_WEIGHT(COMP) .lt. 0.'
          write ( *, '(a,i8)' ) '  COMP = ', comp_i
          write ( *, '(a,g14.6)' ) '  COMP_WEIGHT(COMP) = ', comp_weight
     &(comp_i)
          dirichlet_mix_check = .false.
          return
        else if ( 0.0D+00 .lt. comp_weight(comp_i) ) then
          positive = .true.
        end if

      end do

      if ( .not. positive ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Fatal error!'
        write ( *, '(a)' ) '  All component weights are zero.'
        dirichlet_mix_check = .false.
        return
      end if

      dirichlet_mix_check = .true.

      return
      end
      subroutine dirichlet_mix_mean ( comp_num, elem_num, a, comp_weight
     &,   mean )

c*********************************************************************72
c
cc DIRICHLET_MIX_MEAN returns the means of a Dirichlet mixture PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2004
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
c    Input, integer ELEM_NUM, the number of elements of an
c    observation.
c
c    Input, double precision A(ELEM_NUM,COMP_NUM), the probabilities for
c    element ELEM_NUM in component COMP_NUM.
c    Each A(I,J) should be positive.
c
c    Input, double precision COMP_WEIGHT(COMP_NUM), the mixture weights of
c    the densities.  These do not need to be normalized.  The weight of a
c    given component is the relative probability that that component will
c    be used to generate the sample.
c
c    Output, double precision MEAN(ELEM_NUM), the means for each element.
c
      implicit none

      integer comp_num
      integer elem_num

      double precision a(elem_num,comp_num)
      integer comp_i
      double precision comp_mean(elem_num)
      double precision comp_weight(comp_num)
      double precision comp_weight_sum
      integer i
      double precision mean(elem_num)
      double precision r8vec_sum

      do i = 1, elem_num
        mean(i) = 0.0D+00
      end do

      do comp_i = 1, comp_num
        call dirichlet_mean ( elem_num, a(1,comp_i), comp_mean )
        mean(1:elem_num) = mean(1:elem_num)       + comp_weight(comp_i) 
     &* comp_mean(1:elem_num)
      end do

      do i = 1, elem_num
        mean(i) = mean(i) / comp_weight_sum
      end do

      return
      end
      subroutine dirichlet_mix_pdf ( x, comp_num, elem_num, a,
     &  comp_weight, pdf )

c*********************************************************************72
c
cc DIRICHLET_MIX_PDF evaluates a Dirichlet mixture PDF.
c
c  Discussion:
c
c    The PDF is a weighted sum of Dirichlet PDF's.  Each PDF is a
c    "component", with an associated weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X(ELEM_NUM), the argument of the PDF.
c
c    Input, integer COMP_NUM, the number of components in the
c    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
c    that are mixed together.
c
c    Input, integer ELEM_NUM, the number of elements of an
c    observation.
c
c    Input, double precision A(ELEM_NUM,COMP_NUM), the probabilities for
c    element ELEM_NUM in component COMP_NUM.
c    Each A(I,J) should be positive.
c
c    Input, double precision COMP_WEIGHT(COMP_NUM), the mixture weights of
c    the densities.  These do not need to be normalized.  The weight of a
c    given component is the relative probability that that component will
c    be used to generate the sample.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer comp_num
      integer elem_num

      double precision a(elem_num,comp_num)
      integer comp_i
      double precision comp_pdf
      double precision comp_weight(comp_num)
      double precision comp_weight_sum
      double precision pdf
      double precision r8vec_sum
      double precision x(elem_num)

      comp_weight_sum = r8vec_sum ( comp_num, comp_weight )

      pdf = 0.0D+00
      do comp_i = 1, comp_num

        call dirichlet_pdf ( x, elem_num, a(1,comp_i), comp_pdf )

        pdf = pdf + comp_weight(comp_i) * comp_pdf / comp_weight_sum

      end do

      return
      end
      subroutine dirichlet_mix_sample ( comp_num, elem_num, a,   comp_we
     &ight, seed, comp, x )

c*********************************************************************72
c
cc DIRICHLET_MIX_SAMPLE samples a Dirichlet mixture PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2004
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
c    Input, integer ELEM_NUM, the number of elements of
c    an observation.
c
c    Input, double precision A(ELEM_NUM,COMP_NUM), the probabilities for
c    element ELEM_NUM in component COMP_NUM.
c    Each A(I,J) should be positive.
c
c    Input, double precision COMP_WEIGHT(COMP_NUM), the mixture weights of
c    the densities.  These do not need to be normalized.  The weight of a
c    given component is the relative probability that that component will
c    be used to generate the sample.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer COMP, the index of the component of the
c    Dirichlet mixture that was chosen to generate the sample.
c
c    Output, double precision X(ELEM_NUM), a sample of the PDF.
c
      implicit none

      integer comp_num
      integer elem_num

      double precision a(elem_num,comp_num)
      integer comp
      double precision comp_weight(comp_num)
      integer seed
      double precision x(elem_num)
c
c  Choose a particular density component COMP.
c
      call discrete_sample ( comp_num, comp_weight, seed, comp )
c
c  Sample the density number COMP.
c
      call dirichlet_sample ( elem_num, a(1,comp), seed, x )

      return
      end
      subroutine dirichlet_moment2 ( n, a, m2 )

c*********************************************************************72
c
cc DIRICHLET_MOMENT2 returns the second moments of the Dirichlet PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 November 1999
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
c    Each A(I) should be positive.
c
c    Output, double precision M2(N,N), the second moments of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_sum
      double precision m2(n,n)
      integer i
      integer j
      double precision r8vec_sum

      a_sum = r8vec_sum ( n, a )

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            m2(i,j) = a(i) * ( a(i) + 1.0D+00 ) / ( a_sum * ( a_sum + 1.
     &0D+00 ) )
          else
            m2(i,j) = a(i) * a(j) / ( a_sum * ( a_sum + 1.0D+00 ) )
          end if
        end do
      end do

      return
      end
      subroutine dirichlet_multinomial_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc DIRICHLET_MULTINOMIAL_PDF evaluates a Dirichlet Multinomial PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = Comb(A,B,X) * ( Gamma(C_Sum) / Gamma(C_Sum+A) )
c      Product ( 1 .le. I .le. B ) Gamma(C(I)+X(I)) / Gamma(C(I))
c
c    where:
c
c      Comb(A,B,X) is the multinomial coefficient C( A; X(1), X(2), ..., X(B) ),
c      C_Sum = Sum ( 1 .le. I .le. B ) C(I)
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
c    Input, integer B, the number of different possible outcomes on
c    one trial.
c
c    Input, double precision C(B); C(I) is the Dirichlet parameter associated
c    with outcome I.
c
c    Output, double precision PDF, the value of the Dirichlet multinomial PDF.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      double precision c_sum
      double precision gamma_log
      integer i
      double precision pdf
      double precision pdf_log
      double precision r8vec_sum
      integer x(b)

      c_sum = r8vec_sum ( b, c )

      pdf_log = - gamma_log ( c_sum + dble ( a ) ) 
     &  + gamma_log ( c_sum )
     &  + gamma_log ( dble ( a + 1 ) )

      do i = 1, b
        pdf_log = pdf_log + gamma_log ( c(i) + dble ( x(i) ) )
     &    - gamma_log ( c(i) ) - gamma_log ( dble ( x(i) + 1 ) )
      end do

      pdf = exp ( pdf_log )

      return
      end
      subroutine dirichlet_pdf ( x, n, a, pdf )

c*********************************************************************72
c
cc DIRICHLET_PDF evaluates the Dirichlet PDF.
c
c  Discussion:
c
c    PDF(N,A;X) = Product ( 1 .le. I .le. N ) X(I)^( A(I) - 1 )
c      * Gamma ( A_SUM ) / A_PROD
c
c    where
c
c      0 .le. A(I) for all I;
c      0 .le. X(I) for all I;
c      Sum ( 1 .le. I .le. N ) X(I) = 1;
c      A_SUM = Sum ( 1 .le. I .le. N ) A(I).
c      A_PROD = Product ( 1 .le. I .le. N ) Gamma ( A(I) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X(N), the argument of the PDF.  Each X(I) should
c    be greater than 0.0D+00, and the X(I)'s must add up to 1.0.
c
c    Input, integer N, the number of components.
c
c    Input, double precision A(N), the probabilities for each component.
c    Each A(I) should be nonnegative, and at least one should be
c    positive.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_prod
      double precision a_sum
      integer i
      double precision pdf
      double precision r8_gamma
      double precision r8vec_sum
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x(n)
      double precision x_sum

      do i = 1, n
        if ( x(i) .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DIRICHLET_PDF - Fatal error!'
          write ( *, '(a)' ) '  X(I) .le. 0.'
        end if
      end do

      x_sum = r8vec_sum ( n, x )

      if ( tol .lt. abs ( x_sum - 1.0D+00 ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_PDF - Fatal error!'
        write ( *, '(a)' ) '  SUM X(I) =/= 1.'
      end if

      a_sum = r8vec_sum ( n, a )

      a_prod = 1.0D+00
      do i = 1, n
        a_prod = a_prod * r8_gamma ( a(i) )
      end do

      pdf = r8_gamma ( a_sum ) / a_prod
      do i = 1, n
        pdf = pdf * x(i)**( a(i) - 1.0D+00 )
      end do

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
c    23 November 1999
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
      integer seed
      double precision x(n)

      a2 = 0.0D+00
      b2 = 1.0D+00

      do i = 1, n
        c2 = a(i)
        call gamma_sample ( a2, b2, c2, seed, x(i) )
      end do
c
c  Rescale the vector to have unit sum.
c
      call r8vec_unit_sum ( n, x )

      return
      end
      subroutine dirichlet_variance ( n, a, variance )

c*********************************************************************72
c
cc DIRICHLET_VARIANCE returns the variances of the Dirichlet PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 1999
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
c    Output, double precision VARIANCE(N), the variances of the PDF.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_sum
      integer i
      double precision r8vec_sum
      double precision variance(n)

      a_sum = r8vec_sum ( n, a )

      do i = 1, n
        variance(i) = a(i) * ( a_sum - a(i) ) 
     &    / ( a_sum**2 * ( a_sum + 1.0D+00 ) )
      end do

      return
      end
      subroutine discrete_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc DISCRETE_CDF evaluates the Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the item whose probability is desired.
c
c    Input, integer A, the number of probabilities assigned.
c
c    Input, double precision B(A), the relative probabilities of outcomes
c    1 through A.  Each entry must be nonnegative.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision cdf
      double precision r8vec_sum
      integer x

      if ( x .lt. 1 ) then
        cdf = 0.0D+00
      else if ( x .lt. a ) then
        cdf = r8vec_sum ( x, b ) / r8vec_sum ( a, b )
      else if ( a .le. x ) then
        cdf = 1.0D+00
      end if

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
c    05 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, integer A, the number of probabilities assigned.
c
c    Input, double precision B(A), the relative probabilities of outcomes
c    1 through A.  Each entry must be nonnegative.
c
c    Output, integer X, the corresponding argument for which
c    CDF(X-1) .lt. CDF .le. CDF(X)
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
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
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
      function discrete_check ( a, b )

c*********************************************************************72
c
cc DISCRETE_CHECK checks the parameters of the Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
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
c    Output, logical DISCRETE_CHECK, is true if the parameters are legal.
c
      implicit none

      integer a

      double precision b(a)
      double precision b_sum
      logical discrete_check
      integer j
      double precision r8vec_sum

      do j = 1, a
        if ( b(j) .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DISCRETE_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Negative probabilities not allowed.'
          discrete_check = .false.
          return
        end if
      end do

      b_sum = r8vec_sum ( a, b )

      if ( b_sum .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DISCRETE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  Total probablity is zero.'
        discrete_check = .false.
        return
      end if

      discrete_check = .true.

      return
      end
      subroutine discrete_mean ( a, b, mean )

c*********************************************************************72
c
cc DISCRETE_MEAN evaluates the mean of the Discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
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
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision b_sum
      integer j
      double precision mean
      double precision r8vec_sum

      b_sum = r8vec_sum ( a, b )

      mean = 0.0D+00
      do j = 1, a
        mean = mean + dble ( j ) * b(j)
      end do

      mean = mean / b_sum

      return
      end
      subroutine discrete_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc DISCRETE_PDF evaluates the Discrete PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = B(X) if 1 .le. X .le. A
c                = 0    otherwise
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the item whose probability is desired.
c
c    Input, integer A, the number of probabilities assigned.
c
c    Input, double precision B(A), the relative probabilities of
c    outcomes 1 through A.  Each entry must be nonnegative.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision b_sum
      double precision pdf
      double precision r8vec_sum
      integer x

      b_sum = r8vec_sum ( a, b )

      if ( 1 .le. x .and. x .le. a ) then
        pdf = b(x) / b_sum
      else
        pdf = 0.0D+00
      end if

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
c    05 December 1999
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
      double precision b_sum
      double precision cdf
      double precision r8_uniform_01
      double precision r8vec_sum
      integer seed
      integer x

      b_sum = r8vec_sum ( a, b )

      cdf = r8_uniform_01 ( seed )

      call discrete_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine discrete_variance ( a, b, variance )

c*********************************************************************72
c
cc DISCRETE_VARIANCE evaluates the variance of the Discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
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
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision b_sum
      integer j
      double precision mean
      double precision r8vec_sum
      double precision variance

      b_sum = r8vec_sum ( a, b )

      mean = 0.0D+00
      do j = 1, a
        mean = mean + dble ( j ) * b(j)
      end do

      mean = mean / b_sum

      variance = 0.0D+00
      do j = 1, a
        variance = variance + b(j) * ( j - mean )**2
      end do

      variance = variance / b_sum

      return
      end
      function e_constant ( )

c*********************************************************************72
c
cc E_CONSTANT returns the value of E.
c
c  Discussion:
c
c   "E" was named in honor of Euler.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision E_CONSTANT, the base of the natural
c    logarithm system.
c
      implicit none

      double precision e_constant

      e_constant = 2.71828182845904523536028747135266249775724709369995D
     &+00

      return
      end
      subroutine empirical_discrete_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_CDF evaluates the Empirical Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision bsum
      double precision c(a)
      double precision cdf
      integer i
      double precision r8vec_sum
      double precision x

      cdf = 0.0D+00

      bsum = r8vec_sum ( a, b )

      do i = 1, a

        if ( x .lt. c(i) ) then
          return
        end if

        cdf = cdf + b(i) / bsum

      end do

      return
      end
      subroutine empirical_discrete_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_CDF_INV inverts the Empirical Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Output, double precision X, the smallest argument whose CDF is greater
c    than or equal to CDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision bsum
      double precision c(a)
      double precision cdf
      double precision cdf2
      integer i
      double precision r8vec_sum
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      bsum = r8vec_sum ( a, b )

      x = c(1)
      cdf2 = b(1) / bsum

      do i = 2, a

        if ( cdf .le. cdf2 ) then
          return
        end if

        x = c(i)
        cdf2 = cdf2 + b(i) / bsum

      end do

      return
      end
      function empirical_discrete_check ( a, b, c )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_CHECK checks the parameters of the Empirical Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Output, logical EMPIRICAL_DISCRETE_CHECK, is true if the parameters
c    are legal.
c
      implicit none

      integer a

      double precision b(a)
      double precision c(a)
      logical empirical_discrete_check
      integer i
      integer j

      if ( a .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A must be positive.'
        write ( *, '(a,i12)' ) '  Input A = ', a
        write ( *, '(a)' ) '  A is the number of weights.'
        empirical_discrete_check = .false.
        return
      end if

      do i = 1, a
        if ( b(i) .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Some B(*) .lt. 0.'
          write ( *, '(a)' ) '  But all B values must be nonnegative.'
          empirical_discrete_check = .false.
          return
        end if
      end do

      if ( all ( b(1:a) .eq. 0.0D+00 ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  All B(*) = 0.'
        write ( *, '(a)' ) 
     &    '  But at least one B values must be nonzero.'
        empirical_discrete_check = .false.
        return
      end if

      do i = 1, a
        do j = i+1, a
          if ( c(i) .eq. c(j) ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Fatal error!'
            write ( *, '(a)' ) '  All values C must be unique.'
            write ( *, '(a)' ) '  But at least two values are identical.
     &'
            empirical_discrete_check = .false.
            return
          end if
        end do
      end do

      do i = 1, a-1
        if ( c(i+1) .lt. c(i) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Fatal error!'
          write ( *, '(a)' ) '  The values in C must be in ascending ord
     &er.'
          empirical_discrete_check = .false.
          return
        end if
      end do

      empirical_discrete_check = .true.

      return
      end
      subroutine empirical_discrete_mean ( a, b, c, mean )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_MEAN returns the mean of the Empirical Discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision c(a)
      double precision mean
      double precision r8vec_sum
      double precision r8vec_dot_product

      mean = r8vec_dot_product ( a, b, c ) / r8vec_sum ( a, b )

      return
      end
      subroutine empirical_discrete_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_PDF evaluates the Empirical Discrete PDF.
c
c  Discussion:
c
c    A set of A values C(*) are assigned nonnegative weights B(*),
c    with at least one B nonzero.  The probability of C(I) is the
c    value of B(I) divided by the sum of the weights.
c
c    The C's must be distinct, and given in ascending order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision c(a)
      integer i
      double precision pdf
      double precision r8vec_sum
      double precision x

      do i = 1, a
        if ( x .eq. c(i) ) then
          pdf = b(i) / r8vec_sum ( a, b )
          return
        end if
      end do

      pdf = 0.0D+00

      return
      end
      subroutine empirical_discrete_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_SAMPLE samples the Empirical Discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision c(a)
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call empirical_discrete_cdf_inv ( cdf, a, b, c, x )

      return
      end
      subroutine empirical_discrete_variance ( a, b, c, variance )

c*********************************************************************72
c
cc EMPIRICAL_DISCRETE_VARIANCE: variance of the Empirical Discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of values.
c    0 .lt. A.
c
c    Input, double precision B(A), the weights of each value.
c    0 .le. B(*) and at least one value is nonzero.
c
c    Input, double precision C(A), the values.
c    The values must be distinct and in ascending order.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer a

      double precision b(a)
      double precision bsum
      double precision c(a)
      integer i
      double precision mean
      double precision r8vec_sum
      double precision variance

      bsum = r8vec_sum ( a, b )

      call empirical_discrete_mean ( a, b, c, mean )

      variance = 0.0D+00

      do i = 1, a
        variance = variance + ( b(i) / bsum ) * ( c(i) - mean )**2
      end do

      return
      end
      subroutine english_sentence_length_cdf ( x, cdf )

c*********************************************************************72
c
cc ENGLISH_SENTENCE_LENGTH_CDF evaluates the English Sentence Length CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input, integer X, the sentence length whose CDF is desired.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer sentence_length_max
      parameter ( sentence_length_max = 79 )

      double precision cdf
      double precision pdf_vec(sentence_length_max)
      double precision pdf_sum
      double precision r8vec_sum
      integer x

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /

      data pdf_vec / 
     &  0.00806D+00,     0.01370D+00,     0.01862D+00,     0.02547D+00,
     &  0.03043D+00,     0.03189D+00,     0.03516D+00,     0.03545D+00,
     &  0.03286D+00,     0.03533D+00,     0.03562D+00,     0.03788D+00,
     &  0.03669D+00,     0.03751D+00,     0.03518D+00,     0.03541D+00,
     &  0.03434D+00,     0.03305D+00,     0.03329D+00,     0.03103D+00,
     &  0.02867D+00,     0.02724D+00,     0.02647D+00,     0.02526D+00,
     &  0.02086D+00,     0.02178D+00,     0.02128D+00,     0.01801D+00,
     &  0.01690D+00,     0.01556D+00,     0.01512D+00,     0.01326D+00,
     &  0.01277D+00,     0.01062D+00,     0.01051D+00,     0.00901D+00,
     &  0.00838D+00,     0.00764D+00,     0.00683D+00,     0.00589D+00,
     &  0.00624D+00,     0.00488D+00,     0.00477D+00,     0.00406D+00,
     &  0.00390D+00,     0.00350D+00,     0.00318D+00,     0.00241D+00,
     &  0.00224D+00,     0.00220D+00,     0.00262D+00,     0.00207D+00,
     &  0.00174D+00,     0.00174D+00,     0.00128D+00,     0.00121D+00,
     &  0.00103D+00,     0.00117D+00,     0.00124D+00,     0.00082D+00, 
     &  0.00088D+00,     0.00061D+00,     0.00061D+00,     0.00075D+00,
     &  0.00063D+00,     0.00056D+00,     0.00052D+00,     0.00057D+00,
     &  0.00031D+00,     0.00029D+00,     0.00021D+00,     0.00017D+00,
     &  0.00021D+00,     0.00034D+00,     0.00031D+00,     0.00011D+00,
     &  0.00011D+00,     0.00008D+00,     0.00006D+00 /

      if ( x .lt. 1 ) then
        cdf = 0.0D+00
      else if ( x .lt. sentence_length_max ) then
        cdf = r8vec_sum ( x, pdf_vec ) / pdf_sum
      else if ( sentence_length_max .le. x ) then
        cdf = 1.0D+00
      end if

      return
      end
      subroutine english_sentence_length_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc ENGLISH_SENTENCE_LENGTH_CDF_INV inverts the English Sentence Length CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0 .le. CDF .le. 1.0.
c
c    Output, integer X, the corresponding sentence length for which
c    CDF(X-1) .lt. CDF .le. CDF(X)
c
      implicit none

      integer sentence_length_max
      parameter ( sentence_length_max = 79 )

      double precision cdf
      double precision cum
      integer j
      double precision pdf_vec(sentence_length_max)
      double precision pdf_sum
      integer x

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec / 
     &  0.00806D+00,     0.01370D+00,     0.01862D+00,     0.02547D+00,
     &  0.03043D+00,     0.03189D+00,     0.03516D+00,     0.03545D+00,
     &  0.03286D+00,     0.03533D+00,     0.03562D+00,     0.03788D+00,
     &  0.03669D+00,     0.03751D+00,     0.03518D+00,     0.03541D+00,
     &  0.03434D+00,     0.03305D+00,     0.03329D+00,     0.03103D+00,
     &  0.02867D+00,     0.02724D+00,     0.02647D+00,     0.02526D+00,
     &  0.02086D+00,     0.02178D+00,     0.02128D+00,     0.01801D+00,
     &  0.01690D+00,     0.01556D+00,     0.01512D+00,     0.01326D+00,
     &  0.01277D+00,     0.01062D+00,     0.01051D+00,     0.00901D+00,
     &  0.00838D+00,     0.00764D+00,     0.00683D+00,     0.00589D+00,
     &  0.00624D+00,     0.00488D+00,     0.00477D+00,     0.00406D+00,
     &  0.00390D+00,     0.00350D+00,     0.00318D+00,     0.00241D+00,
     &  0.00224D+00,     0.00220D+00,     0.00262D+00,     0.00207D+00,
     &  0.00174D+00,     0.00174D+00,     0.00128D+00,     0.00121D+00,
     &  0.00103D+00,     0.00117D+00,     0.00124D+00,     0.00082D+00, 
     &  0.00088D+00,     0.00061D+00,     0.00061D+00,     0.00075D+00,
     &  0.00063D+00,     0.00056D+00,     0.00052D+00,     0.00057D+00,
     &  0.00031D+00,     0.00029D+00,     0.00021D+00,     0.00017D+00,
     &  0.00021D+00,     0.00034D+00,     0.00031D+00,     0.00011D+00,
     &  0.00011D+00,     0.00008D+00,     0.00006D+00 /

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    'ENGLISH_SENTENCE_LENGTH_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cum = 0.0D+00

      do j = 1, sentence_length_max

        cum = cum + pdf_vec(j)

        if ( cdf .le. cum / pdf_sum ) then
          x = j
          return
        end if

      end do

      x = sentence_length_max

      return
      end
      subroutine english_sentence_length_mean ( mean )

c*********************************************************************72
c
cc ENGLISH_SENTENCE_LENGTH_MEAN: mean of the English Sentence Length PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer sentence_length_max
      parameter ( sentence_length_max = 79 )

      integer j
      double precision mean
      double precision pdf_vec(sentence_length_max)
      double precision pdf_sum

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec / 
     &  0.00806D+00,     0.01370D+00,     0.01862D+00,     0.02547D+00,
     &  0.03043D+00,     0.03189D+00,     0.03516D+00,     0.03545D+00,
     &  0.03286D+00,     0.03533D+00,     0.03562D+00,     0.03788D+00,
     &  0.03669D+00,     0.03751D+00,     0.03518D+00,     0.03541D+00,
     &  0.03434D+00,     0.03305D+00,     0.03329D+00,     0.03103D+00,
     &  0.02867D+00,     0.02724D+00,     0.02647D+00,     0.02526D+00,
     &  0.02086D+00,     0.02178D+00,     0.02128D+00,     0.01801D+00,
     &  0.01690D+00,     0.01556D+00,     0.01512D+00,     0.01326D+00,
     &  0.01277D+00,     0.01062D+00,     0.01051D+00,     0.00901D+00,
     &  0.00838D+00,     0.00764D+00,     0.00683D+00,     0.00589D+00,
     &  0.00624D+00,     0.00488D+00,     0.00477D+00,     0.00406D+00,
     &  0.00390D+00,     0.00350D+00,     0.00318D+00,     0.00241D+00,
     &  0.00224D+00,     0.00220D+00,     0.00262D+00,     0.00207D+00,
     &  0.00174D+00,     0.00174D+00,     0.00128D+00,     0.00121D+00,
     &  0.00103D+00,     0.00117D+00,     0.00124D+00,     0.00082D+00, 
     &  0.00088D+00,     0.00061D+00,     0.00061D+00,     0.00075D+00,
     &  0.00063D+00,     0.00056D+00,     0.00052D+00,     0.00057D+00,
     &  0.00031D+00,     0.00029D+00,     0.00021D+00,     0.00017D+00,
     &  0.00021D+00,     0.00034D+00,     0.00031D+00,     0.00011D+00,
     &  0.00011D+00,     0.00008D+00,     0.00006D+00 /

      mean = 0.0D+00
      do j = 1, sentence_length_max
        mean = mean + dble ( j ) * pdf_vec(j)
      end do

      mean = mean / pdf_sum

      return
      end
      subroutine english_sentence_length_pdf ( x, pdf )

c*********************************************************************72
c
cc ENGLISH_SENTENCE_LENGTH_PDF evaluates the English Sentence Length PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = B(X) if 1 .le. X .le. A
c                = 0    otherwise
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input, integer X, the sentence length whose probability
c    is desired.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer sentence_length_max
      parameter ( sentence_length_max = 79 )

      double precision pdf
      double precision pdf_vec(sentence_length_max)
      double precision pdf_sum
      integer x

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec / 
     &  0.00806D+00,     0.01370D+00,     0.01862D+00,     0.02547D+00,
     &  0.03043D+00,     0.03189D+00,     0.03516D+00,     0.03545D+00,
     &  0.03286D+00,     0.03533D+00,     0.03562D+00,     0.03788D+00,
     &  0.03669D+00,     0.03751D+00,     0.03518D+00,     0.03541D+00,
     &  0.03434D+00,     0.03305D+00,     0.03329D+00,     0.03103D+00,
     &  0.02867D+00,     0.02724D+00,     0.02647D+00,     0.02526D+00,
     &  0.02086D+00,     0.02178D+00,     0.02128D+00,     0.01801D+00,
     &  0.01690D+00,     0.01556D+00,     0.01512D+00,     0.01326D+00,
     &  0.01277D+00,     0.01062D+00,     0.01051D+00,     0.00901D+00,
     &  0.00838D+00,     0.00764D+00,     0.00683D+00,     0.00589D+00,
     &  0.00624D+00,     0.00488D+00,     0.00477D+00,     0.00406D+00,
     &  0.00390D+00,     0.00350D+00,     0.00318D+00,     0.00241D+00,
     &  0.00224D+00,     0.00220D+00,     0.00262D+00,     0.00207D+00,
     &  0.00174D+00,     0.00174D+00,     0.00128D+00,     0.00121D+00,
     &  0.00103D+00,     0.00117D+00,     0.00124D+00,     0.00082D+00, 
     &  0.00088D+00,     0.00061D+00,     0.00061D+00,     0.00075D+00,
     &  0.00063D+00,     0.00056D+00,     0.00052D+00,     0.00057D+00,
     &  0.00031D+00,     0.00029D+00,     0.00021D+00,     0.00017D+00,
     &  0.00021D+00,     0.00034D+00,     0.00031D+00,     0.00011D+00,
     &  0.00011D+00,     0.00008D+00,     0.00006D+00 /

      if ( 1 .le. x .and. x .le. sentence_length_max ) then
        pdf = pdf_vec(x) / pdf_sum
      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine english_sentence_length_sample ( seed, x )

c*********************************************************************72
c
cc ENGLISH_SENTENCE_LENGTH_SAMPLE samples the English Sentence Length PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call english_sentence_length_cdf_inv ( cdf, x )

      return
      end
      subroutine english_sentence_length_variance ( variance )

c*********************************************************************72
c
cc ENGLISH_SENTENCE_LENGTH_VARIANCE: variance of English Sentence Length PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer sentence_length_max
      parameter ( sentence_length_max = 79 )

      integer j
      double precision mean
      double precision pdf_vec(sentence_length_max)
      double precision pdf_sum
      double precision variance

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec / 
     &  0.00806D+00,     0.01370D+00,     0.01862D+00,     0.02547D+00,
     &  0.03043D+00,     0.03189D+00,     0.03516D+00,     0.03545D+00,
     &  0.03286D+00,     0.03533D+00,     0.03562D+00,     0.03788D+00,
     &  0.03669D+00,     0.03751D+00,     0.03518D+00,     0.03541D+00,
     &  0.03434D+00,     0.03305D+00,     0.03329D+00,     0.03103D+00,
     &  0.02867D+00,     0.02724D+00,     0.02647D+00,     0.02526D+00,
     &  0.02086D+00,     0.02178D+00,     0.02128D+00,     0.01801D+00,
     &  0.01690D+00,     0.01556D+00,     0.01512D+00,     0.01326D+00,
     &  0.01277D+00,     0.01062D+00,     0.01051D+00,     0.00901D+00,
     &  0.00838D+00,     0.00764D+00,     0.00683D+00,     0.00589D+00,
     &  0.00624D+00,     0.00488D+00,     0.00477D+00,     0.00406D+00,
     &  0.00390D+00,     0.00350D+00,     0.00318D+00,     0.00241D+00,
     &  0.00224D+00,     0.00220D+00,     0.00262D+00,     0.00207D+00,
     &  0.00174D+00,     0.00174D+00,     0.00128D+00,     0.00121D+00,
     &  0.00103D+00,     0.00117D+00,     0.00124D+00,     0.00082D+00, 
     &  0.00088D+00,     0.00061D+00,     0.00061D+00,     0.00075D+00,
     &  0.00063D+00,     0.00056D+00,     0.00052D+00,     0.00057D+00,
     &  0.00031D+00,     0.00029D+00,     0.00021D+00,     0.00017D+00,
     &  0.00021D+00,     0.00034D+00,     0.00031D+00,     0.00011D+00,
     &  0.00011D+00,     0.00008D+00,     0.00006D+00 /

      mean = 0.0D+00
      do j = 1, sentence_length_max
        mean = mean + dble ( j ) * pdf_vec(j)
      end do

      mean = mean / pdf_sum

      variance = 0.0D+00
      do j = 1, sentence_length_max
        variance = variance + pdf_vec(j) * ( j - mean )**2
      end do

      variance = variance / pdf_sum

      return
      end
      subroutine english_word_length_cdf ( x, cdf )

c*********************************************************************72
c
cc ENGLISH_WORD_LENGTH_CDF evaluates the English Word Length CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input, integer X, the word length whose CDF is desired.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer word_length_max
      parameter ( word_length_max = 27 )

      double precision cdf
      double precision pdf_vec(word_length_max)
      double precision pdf_sum
      double precision r8vec_sum
      integer x

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec /
     &  0.03160D+00,     0.16975D+00,     0.21192D+00,     0.15678D+00,
     &  0.10852D+00,     0.08524D+00,     0.07724D+00,     0.05623D+00,
     &  0.04032D+00,     0.02766D+00,     0.01582D+00,     0.00917D+00,
     &  0.00483D+00,     0.00262D+00,     0.00099D+00,     0.00050D+00,
     &  0.00027D+00,     0.00022D+00,     0.00011D+00,     0.00006D+00,
     &  0.00005D+00,     0.00002D+00,     0.00001D+00,     0.00001D+00,
     &  0.00001D+00,     0.00001D+00,     0.00001D+00 /

      if ( x .lt. 1 ) then
        cdf = 0.0D+00
      else if ( x .lt. word_length_max ) then
        cdf = r8vec_sum ( x, pdf_vec ) / pdf_sum
      else if ( word_length_max .le. x ) then
        cdf = 1.0D+00
      end if

      return
      end
      subroutine english_word_length_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc ENGLISH_WORD_LENGTH_CDF_INV inverts the English Word Length CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0 .le. CDF .le. 1.0.
c
c    Output, integer X, the corresponding word length for which
c    CDF(X-1) .lt. CDF .le. CDF(X)
c
      implicit none

      integer word_length_max
      parameter ( word_length_max = 27 )

      double precision cdf
      double precision cum
      integer j
      double precision pdf_vec(word_length_max)
      double precision pdf_sum
      integer x

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec /
     &  0.03160D+00,     0.16975D+00,     0.21192D+00,     0.15678D+00,
     &  0.10852D+00,     0.08524D+00,     0.07724D+00,     0.05623D+00,
     &  0.04032D+00,     0.02766D+00,     0.01582D+00,     0.00917D+00,
     &  0.00483D+00,     0.00262D+00,     0.00099D+00,     0.00050D+00,
     &  0.00027D+00,     0.00022D+00,     0.00011D+00,     0.00006D+00,
     &  0.00005D+00,     0.00002D+00,     0.00001D+00,     0.00001D+00,
     &  0.00001D+00,     0.00001D+00,     0.00001D+00 /

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ENGLISH_WORD_LENGTH_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cum = 0.0D+00

      do j = 1, word_length_max

        cum = cum + pdf_vec(j)

        if ( cdf .le. cum / pdf_sum ) then
          x = j
          return
        end if

      end do

      x = word_length_max

      return
      end
      subroutine english_word_length_mean ( mean )

c*********************************************************************72
c
cc ENGLISH_WORD_LENGTH_MEAN evaluates the mean of the English Word Length PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer word_length_max
      parameter ( word_length_max = 27 )

      integer j
      double precision mean
      double precision pdf_vec(word_length_max)
      double precision pdf_sum

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec /
     &  0.03160D+00,     0.16975D+00,     0.21192D+00,     0.15678D+00,
     &  0.10852D+00,     0.08524D+00,     0.07724D+00,     0.05623D+00,
     &  0.04032D+00,     0.02766D+00,     0.01582D+00,     0.00917D+00,
     &  0.00483D+00,     0.00262D+00,     0.00099D+00,     0.00050D+00,
     &  0.00027D+00,     0.00022D+00,     0.00011D+00,     0.00006D+00,
     &  0.00005D+00,     0.00002D+00,     0.00001D+00,     0.00001D+00,
     &  0.00001D+00,     0.00001D+00,     0.00001D+00 /

      mean = 0.0D+00
      do j = 1, word_length_max
        mean = mean + dble ( j ) * pdf_vec(j)
      end do

      mean = mean / pdf_sum

      return
      end
      subroutine english_word_length_pdf ( x, pdf )

c*********************************************************************72
c
cc ENGLISH_WORD_LENGTH_PDF evaluates the English Word Length PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = B(X) if 1 .le. X .le. A
c                = 0    otherwise
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input, integer X, the word length whose probability
c    is desired.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer word_length_max
      parameter ( word_length_max = 27 )

      double precision pdf
      double precision pdf_vec(word_length_max)
      double precision pdf_sum
      integer x

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec /
     &  0.03160D+00,     0.16975D+00,     0.21192D+00,     0.15678D+00,
     &  0.10852D+00,     0.08524D+00,     0.07724D+00,     0.05623D+00,
     &  0.04032D+00,     0.02766D+00,     0.01582D+00,     0.00917D+00,
     &  0.00483D+00,     0.00262D+00,     0.00099D+00,     0.00050D+00,
     &  0.00027D+00,     0.00022D+00,     0.00011D+00,     0.00006D+00,
     &  0.00005D+00,     0.00002D+00,     0.00001D+00,     0.00001D+00,
     &  0.00001D+00,     0.00001D+00,     0.00001D+00 /

      if ( 1 .le. x .and. x .le. word_length_max ) then
        pdf = pdf_vec(x) / pdf_sum
      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine english_word_length_sample ( seed, x )

c*********************************************************************72
c
cc ENGLISH_WORD_LENGTH_SAMPLE samples the English Word Length PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call english_word_length_cdf_inv ( cdf, x )

      return
      end
      subroutine english_word_length_variance ( variance )

c*********************************************************************72
c
cc ENGLISH_WORD_LENGTH_VARIANCE: variance of the English Word Length PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Henry Kucera, Winthrop Francis,
c    Computational Analysis of Present-Day American English,
c    Brown University Press, 1967.
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer word_length_max
      parameter ( word_length_max = 27 )

      integer j
      double precision mean
      double precision pdf_vec(word_length_max)
      double precision pdf_sum
      double precision variance

      save pdf_sum
      save pdf_vec

      data pdf_sum / 0.99768D+00 /
      data pdf_vec /
     &  0.03160D+00,     0.16975D+00,     0.21192D+00,     0.15678D+00,
     &  0.10852D+00,     0.08524D+00,     0.07724D+00,     0.05623D+00,
     &  0.04032D+00,     0.02766D+00,     0.01582D+00,     0.00917D+00,
     &  0.00483D+00,     0.00262D+00,     0.00099D+00,     0.00050D+00,
     &  0.00027D+00,     0.00022D+00,     0.00011D+00,     0.00006D+00,
     &  0.00005D+00,     0.00002D+00,     0.00001D+00,     0.00001D+00,
     &  0.00001D+00,     0.00001D+00,     0.00001D+00 /

      mean = 0.0D+00
      do j = 1, word_length_max
        mean = mean + dble ( j ) * pdf_vec(j)
      end do

      mean = mean / pdf_sum

      variance = 0.0D+00
      do j = 1, word_length_max
        variance = variance + pdf_vec(j) * ( j - mean )**2
      end do

      variance = variance / pdf_sum

      return
      end
      function error_f ( x )

c*********************************************************************72
c
cc ERROR_F evaluates the error function ERF.
c
c  Discussion:
c
c    Since some compilers already supply a routine named ERF which evaluates
c    the error function, this routine has been given a distinct, if
c    somewhat unnatural, name.
c
c    The function is defined by:
c
c      ERF(X) = ( 2 / sqrt ( PI ) ) * Integral ( 0 .le. T .le. X ) EXP ( - T^2 ) dT.
c
c    Properties of the function include:
c
c      Limit ( X -> -Infinity ) ERF(X) =          -1.0;
c                               ERF(0) =           0.0;
c                               ERF(0.476936...) = 0.5;
c      Limit ( X -> +Infinity ) ERF(X) =          +1.0.
c
c      0.5D+00 * ( ERF(X/sqrt(2)) + 1 ) = Normal_01_CDF(X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 November 2006
c
c  Author:
c
c    Original FORTRAN77 version by William Cody.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    William Cody,
c    Rational Chebyshev Approximations for the Error Function,
c    Mathematics of Computation,
c    1969, pages 631-638.
c
c  Parameters:
c
c    Input, double precision X, the argument of the error function.
c
c    Output, double precision ERROR_F, the value of the error function.
c
      implicit none

      double precision, parameter, dimension ( 5 ) :: a = (/     3.1611
     &2374387056560D+00,     1.13864154151050156D+02,     3.774852376853
     &02021D+02,     3.20937758913846947D+03,     1.85777706184603153D-0
     &1 /)
      double precision, parameter, dimension ( 4 ) :: b = (/     2.3601
     &2909523441209D+01,     2.44024637934444173D+02,     1.282616526077
     &37228D+03,     2.84423683343917062D+03 /)
      double precision, parameter, dimension ( 9 ) :: c = (/     5.6418
     &8496988670089D-01,     8.88314979438837594D+00,     6.611919063714
     &16295D+01,     2.98635138197400131D+02,     8.81952221241769090D+0
     &2,     1.71204761263407058D+03,     2.05107837782607147D+03,     1
     &.23033935479799725D+03,     2.15311535474403846D-08 /)
      double precision, parameter, dimension ( 8 ) :: d = (/     1.5744
     &9261107098347D+01,     1.17693950891312499D+02,     5.371811018620
     &09858D+02,     1.62138957456669019D+03,     3.29079923573345963D+0
     &3,     4.36261909014324716D+03,     3.43936767414372164D+03,     1
     &.23033935480374942D+03 /)
      double precision del
      double precision error_f
      integer i
      double precision, parameter, dimension ( 6 ) :: p = (/     3.0532
     &6634961232344D-01,     3.60344899949804439D-01,     1.257817261112
     &29246D-01,     1.60837851487422766D-02,     6.58749161529837803D-0
     &4,     1.63153871373020978D-02 /)
      double precision, parameter, dimension ( 5 ) :: q = (/     2.5685
     &2019228982242D+00,     1.87295284992346047D+00,     5.279051029514
     &28412D-01,     6.05183413124413191D-02,     2.33520497626869185D-0
     &3 /)
      double precision r8_epsilon
      double precision, parameter :: sqrpi = 0.56418958354775628695D+00
      double precision thresh
      parameter ( thresh = 0.46875D+00 )
      double precision x
      double precision xabs
      double precision xbig
      parameter ( xbig = 26.543D+00 )
      double precision xden
      double precision xnum
      double precision xsq

      xabs = abs ( ( x ) )
c
c  Evaluate ERF(X) for |X| .le. 0.46875.
c
      if ( xabs .le. thresh ) then

        if ( r8_epsilon ( ) .lt. xabs ) then
          xsq = xabs * xabs
        else
          xsq = 0.0D+00
        end if

        xnum = a(5) * xsq
        xden = xsq
        do i = 1, 3
          xnum = ( xnum + a(i) ) * xsq
          xden = ( xden + b(i) ) * xsq
        end do

        error_f = x * ( xnum + a(4) ) / ( xden + b(4) )
c
c  Evaluate ERFC(X) for 0.46875 .le. |X| .le. 4.0.
c
      else if ( xabs .le. 4.0D+00 ) then

        xnum = c(9) * xabs
        xden = xabs
        do i = 1, 7
          xnum = ( xnum + c(i) ) * xabs
          xden = ( xden + d(i) ) * xabs
        end do

        error_f = ( xnum + c(8) ) / ( xden + d(8) )
        xsq = dble ( int ( xabs * 16.0D+00 ) ) / 16.0D+00
        del = ( xabs - xsq ) * ( xabs + xsq )
        error_f = exp ( - xsq * xsq ) * exp ( - del ) * error_f

        error_f = ( 0.5D+00 - error_f ) + 0.5D+00

        if ( x .lt. 0.0D+00 ) then
          error_f = - error_f
        end if
c
c  Evaluate ERFC(X) for 4.0D+00 .lt. |X|.
c
      else

        if ( xbig .le. xabs ) then

          if ( 0.0D+00 .lt. x ) then
            error_f = 1.0D+00
          else
            error_f = - 1.0D+00
          end if

        else

          xsq = 1.0D+00 / ( xabs * xabs )

          xnum = p(6) * xsq
          xden = xsq
          do i = 1, 4
            xnum = ( xnum + p(i) ) * xsq
            xden = ( xden + q(i) ) * xsq
          end do

          error_f = xsq * ( xnum + p(5) ) / ( xden + q(5) )
          error_f = ( sqrpi - error_f ) / xabs
          xsq = dble ( int ( xabs * 16.0D+00 ) ) / 16.0D+00
          del = ( xabs - xsq ) * ( xabs + xsq )
          error_f = exp ( - xsq * xsq ) * exp ( - del ) * error_f

          error_f = ( 0.5D+00 - error_f ) + 0.5D+00

          if ( x .lt. 0.0D+00 ) then
            error_f = - error_f
          end if

        end if

      end if

      return
      end
      function error_f_inverse ( y )

c*********************************************************************72
c
cc ERROR_F_INVERSE inverts the error function ERF.
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
c  Parameters:
c
c    Input, double precision Y, the value of the error function.
c
c    Output, double precision ERROR_F_INVERSE, the value X such that
c    ERROR_F(X) = Y.
c
      implicit none

      double precision error_f_inverse
      double precision x
      double precision y
      double precision z

      z = ( y + 1.0D+00 ) / 2.0D+00

      call normal_01_cdf_inv ( z, x )

      error_f_inverse = x / sqrt ( 2.0D+00 )

      return
      end
      subroutine erlang_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc ERLANG_CDF evaluates the Erlang CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision cdf
      double precision gamma_inc
      double precision p2
      double precision x
      double precision x2

      if ( x .lt. a ) then

        cdf = 0.0D+00

      else

        x2 = ( x - a ) / b
        p2 = dble ( c )

        cdf = gamma_inc ( p2, x2 )

      end if

      return
      end
      subroutine erlang_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc ERLANG_CDF_INV inverts the Erlang CDF.
c
c  Discussion:
c
c    A simple bisection method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision r8_huge
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ERLANG_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = a
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
        return
      end if

      x1 = a
      cdf1 = 0.0D+00

      x2 = a + 1.0D+00

10    continue

        call erlang_cdf ( x2, a, b, c, cdf2 )

        if ( cdf .lt. cdf2 ) then
          go to 20
        end if

        x2 = a + 2.0D+00 * ( x2 - a )

      go to 10

20    continue
c
c  Now use bisection.
c
      it = 0

30    continue

        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call erlang_cdf ( x3, a, b, c, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          go to 40
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'ERLANG_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          return
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 30

40    continue

      return
      end
      function erlang_check ( a, b, c )

c*********************************************************************72
c
cc ERLANG_CHECK checks the parameters of the Erlang PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Output, logical ERLANG_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      integer c
      logical erlang_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ERLANG_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0'
        erlang_check = .false.
        return
      end if

      if ( c .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ERLANG_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        erlang_check = .false.
        return
      end if

      erlang_check = .true.

      return
      end
      subroutine erlang_mean ( a, b, c, mean )

c*********************************************************************72
c
cc ERLANG_MEAN returns the mean of the Erlang PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision mean

      mean =  a + b * dble ( c )

      return
      end
      subroutine erlang_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc ERLANG_PDF evaluates the Erlang PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = ( ( X - A ) / B )^( C - 1 )
c      / ( B * Gamma ( C ) * EXP ( ( X - A ) / B ) )
c
c    for 0 .lt. B, 0 .lt. C integer, A .le. X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision i4_factorial
      double precision pdf
      double precision x
      double precision y

      if ( x .le. a ) then

        pdf = 0.0D+00

      else

        y = ( x - a ) / b

        pdf = y**( c - 1 ) / ( b * i4_factorial ( c - 1 ) * exp ( y ) )

      end if

      return
      end
      subroutine erlang_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc ERLANG_SAMPLE samples the Erlang PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision b2
      integer c
      integer i
      integer seed
      double precision x
      double precision x2

      a2 = 0.0D+00
      b2 = b
      x = a
      do i = 1, c
        call exponential_sample ( a2, b2, seed, x2 )
        x = x + x2
      end do

      return
      end
      subroutine erlang_variance ( a, b, c, variance )

c*********************************************************************72
c
cc ERLANG_VARIANCE returns the variance of the Erlang PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, integer C, the parameters of the PDF.
c    0.0D+00 .lt. B.
c    0 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer c
      double precision variance

      variance =  b * b * real ( c )

      return
      end
      function euler_constant ( )

c*********************************************************************72
c
cc EULER_CONSTANT returns the value of the Euler-Mascheroni constant.
c
c  Discussion:
c
c    The Euler-Mascheroni constant is often denoted by a lower-case
c    Gamma.  Gamma is defined as
c
c      Gamma = limit ( M -> Infinity )
c        ( Sum ( 1 .le. N .le. M ) 1 / N ) - Log ( M )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision EULER_CONSTANT, the value of the
c    Euler-Mascheroni constant.
c
      implicit none

      double precision euler_constant

      euler_constant = 0.577215664901532860606512090082402431042D+00

      return
      end
      subroutine exponential_01_cdf ( x, cdf )

c*********************************************************************72
c
cc EXPONENTIAL_01_CDF evaluates the Exponential 01 CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      double precision x

      if ( x .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - exp ( - x )
      end if

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
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EXPONENTIAL_01_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = - log ( 1.0D+00 - cdf )

      return
      end
      subroutine exponential_01_mean ( mean )

c*********************************************************************72
c
cc EXPONENTIAL_01_MEAN returns the mean of the Exponential 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision mean

      mean = 1.0D+00

      return
      end
      subroutine exponential_01_pdf ( x, pdf )

c*********************************************************************72
c
cc EXPONENTIAL_01_PDF evaluates the Exponential 01 PDF.
c
c  Discussion:
c
c    PDF(X) = EXP ( - X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision x

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = exp ( - x )
      end if

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
c    20 March 2003
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
      subroutine exponential_01_variance ( variance )

c*********************************************************************72
c
cc EXPONENTIAL_01_VARIANCE returns the variance of the Exponential 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision variance

      variance = 1.0D+00

      return
      end
      subroutine exponential_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc EXPONENTIAL_CDF evaluates the Exponential CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .le. a ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - exp ( ( a - x ) / b )
      end if

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
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a - b * log ( 1.0D+00 - cdf )

      return
      end
      subroutine exponential_cdf_values ( n_data, lambda, x, fx )

c*********************************************************************72
c
cc EXPONENTIAL_CDF_VALUES returns some values of the Exponential CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = ExponentialDistribution [ lambda ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision LAMBDA, the parameter of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      double precision fx
      double precision fx_vec(n_max)
      double precision lambda
      double precision lambda_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save lambda_vec
      save x_vec

      data fx_vec /
     &  0.3934693402873666D+00,
     &  0.6321205588285577D+00,
     &  0.7768698398515702D+00,
     &  0.8646647167633873D+00,
     &  0.8646647167633873D+00,
     &  0.9816843611112658D+00,
     &  0.9975212478233336D+00,
     &  0.9996645373720975D+00,
     &  0.9999546000702375D+00 /
      data lambda_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        lambda = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        lambda = lambda_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function exponential_check ( a, b )

c*********************************************************************72
c
cc EXPONENTIAL_CHECK checks the parameters of the Exponential CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical EXPONENTIAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical exponential_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EXPONENTIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0'
        exponential_check = .false.
        return
      end if

      exponential_check = .true.

      return
      end
      subroutine exponential_mean ( a, b, mean )

c*********************************************************************72
c
cc EXPONENTIAL_MEAN returns the mean of the Exponential PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a + b

      return
      end
      subroutine exponential_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc EXPONENTIAL_PDF evaluates the Exponential PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = ( 1 / B ) * EXP ( ( A - X ) / B )
c
c    The time interval between two Poisson events is a random
c    variable with the Exponential PDF.  The parameter B is the
c    average interval between events.
c
c    In another context, the Exponential PDF is related to
c    the Boltzmann distribution, which describes the relative
c    probability of finding a system, which is in thermal equilibrium
c    at absolute temperature T, in a given state having energy E.
c    The relative probability is
c
c      Boltzmann_Relative_Probability(E,T) = exp ( - E / ( k * T ) ),
c
c    where k is the Boltzmann constant,
c
c      k = 1.38 * 10**(-23) joules / degree Kelvin
c
c    and normalization requires a determination of the possible
c    energy states of the system.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .lt. a ) then
        pdf = 0.0D+00
      else
        pdf = ( 1.0D+00 / b ) * exp ( ( a - x ) / b )
      end if

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
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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
      subroutine exponential_variance ( a, b, variance )

c*********************************************************************72
c
cc EXPONENTIAL_VARIANCE returns the variance of the Exponential PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = b * b

      return
      end
      subroutine extreme_values_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc EXTREME_VALUES_CDF evaluates the Extreme Values CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x
      double precision y

      y = ( x - a ) / b

      cdf = exp ( - exp ( - y ) )

      return
      end
      subroutine extreme_values_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc EXTREME_VALUES_CDF_INV inverts the Extreme Values CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EXTREME_VALUES_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a - b * log ( - log ( cdf ) )

      return
      end
      subroutine extreme_values_cdf_values ( n_data, alpha, beta, x,
     &  fx )

c*********************************************************************72
c
cc EXTREME_VALUES_CDF_VALUES returns some values of the Extreme Values CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = ExtremeValuesDistribution [ alpha, beta ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision ALPHA, the first parameter of the distribution.
c
c    Output, double precision BETA, the second parameter of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision alpha
      double precision alpha_vec(n_max)
      double precision beta
      double precision beta_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save alpha_vec
      save beta_vec
      save fx_vec
      save x_vec

      data alpha_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data beta_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data fx_vec /
     &  0.3678794411714423D+00,
     &  0.8734230184931166D+00,
     &  0.9818510730616665D+00,
     &  0.9975243173927525D+00,
     &  0.5452392118926051D+00,
     &  0.4884435800065159D+00,
     &  0.4589560693076638D+00,
     &  0.4409910259429826D+00,
     &  0.5452392118926051D+00,
     &  0.3678794411714423D+00,
     &  0.1922956455479649D+00,
     &  0.6598803584531254D-01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        alpha = 0.0D+00
        beta = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        alpha = alpha_vec(n_data)
        beta = beta_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function extreme_values_check ( a, b )

c*********************************************************************72
c
cc EXTREME_VALUES_CHECK checks the parameters of the Extreme Values CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical EXTREME_VALUES_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical extreme_values_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EXTREME_VALUES_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        extreme_values_check = .false.
        return
      end if

      extreme_values_check = .true.

      return
      end
      subroutine extreme_values_mean ( a, b, mean )

c*********************************************************************72
c
cc EXTREME_VALUES_MEAN returns the mean of the Extreme Values PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision euler_constant
      double precision mean

      mean = a + b * euler_constant ( )

      return
      end
      subroutine extreme_values_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc EXTREME_VALUES_PDF evaluates the Extreme Values PDF.
c
c  Discussion:
c
c    PDF(A,B;X) =
c      ( 1 / B ) * exp ( ( A - X ) / B ) * exp ( - exp ( ( A - X ) / B  ) ).
c
c    The Extreme Values PDF is also known as the Fisher-Tippet PDF
c    and the Log-Weibull PDF.
c
c    The special case A = 0 and B = 1 is the Gumbel PDF.
c
c    The Extreme Values PDF is the limiting distribution for the
c    smallest or largest value in a large sample drawn from
c    any of a great variety of distributions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Eric Weisstein, editor,
c    CRC Concise Encylopedia of Mathematics,
c    CRC Press, 1998.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      pdf = ( 1.0D+00 / b ) * exp ( ( a - x ) / b - exp ( ( a - x ) / b 
     &) )

      return
      end
      subroutine extreme_values_sample ( a, b, seed, x )

c*********************************************************************72
c
cc EXTREME_VALUES_SAMPLE samples the Extreme Values PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call extreme_values_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine extreme_values_variance ( a, b, variance )

c*********************************************************************72
c
cc EXTREME_VALUES_VARIANCE returns the variance of the Extreme Values PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = pi * pi * b * b / 6.0D+00

      return
      end
      subroutine f_cdf ( x, m, n, cdf )

c*********************************************************************72
c
cc F_CDF evaluates the F central CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    LC: QA47.A34,
c    ISBN: 0-486-61272-4.
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision arg1
      double precision arg2
      double precision arg3
      double precision beta_inc
      double precision cdf
      integer m
      integer n
      double precision x

      if ( x .le. 0.0D+00 ) then

        cdf = 0.0D+00

      else

        arg1 = 0.5D+00 * dble ( n )
        arg2 = 0.5D+00 * dble ( m )
        arg3 = dble ( n ) / ( dble ( n ) + dble ( m ) * x )

        cdf = 1.0D+00 - beta_inc ( arg1, arg2, arg3 )

      end if

      return
      end
      subroutine f_cdf_values ( n_data, a, b, x, fx )

c*********************************************************************72
c
cc F_CDF_VALUES returns some values of the F CDF test function.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = FRatioDistribution [ dfn, dfd ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer A, integer B, the parameters of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      integer a
      integer a_vec(n_max)
      integer b
      integer b_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save b_vec
      save fx_vec
      save x_vec

      data a_vec /
     &  1,
     &  1,
     &  5,
     &  1,
     &  2,
     &  4,
     &  1,
     &  6,
     &  8,
     &  1,
     &  3,
     &  6,
     &  1,
     &  1,
     &  1,
     &  1,
     &  2,
     &  3,
     &  4,
     &  5 /
      data b_vec /
     &   1,
     &   5,
     &   1,
     &   5,
     &  10,
     &  20,
     &   5,
     &   6,
     &  16,
     &   5,
     &  10,
     &  12,
     &   5,
     &   5,
     &   5,
     &   5,
     &   5,
     &   5,
     &   5,
     &   5 /
      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.4999714850534485D+00,
     &  0.4996034370170990D+00,
     &  0.7496993658293228D+00,
     &  0.7504656462757382D+00,
     &  0.7514156325324275D+00,
     &  0.8999867031372156D+00,
     &  0.8997127554259699D+00,
     &  0.9002845660853669D+00,
     &  0.9500248817817622D+00,
     &  0.9500574946122442D+00,
     &  0.9501926400000000D+00,
     &  0.9750133887312993D+00,
     &  0.9900022327445249D+00,
     &  0.9949977837872073D+00,
     &  0.9989999621122122D+00,
     &  0.5687988496283079D+00,
     &  0.5351452100063650D+00,
     &  0.5143428032407864D+00,
     &  0.5000000000000000D+00 /
      data x_vec /
     &   1.00D+00,
     &   0.528D+00,
     &   1.89D+00,
     &   1.69D+00,
     &   1.60D+00,
     &   1.47D+00,
     &   4.06D+00,
     &   3.05D+00,
     &   2.09D+00,
     &   6.61D+00,
     &   3.71D+00,
     &   3.00D+00,
     &  10.01D+00,
     &  16.26D+00,
     &  22.78D+00,
     &  47.18D+00,
     &   1.00D+00,
     &   1.00D+00,
     &   1.00D+00,
     &   1.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0
        b = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        b = b_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function f_check ( m, n )

c*********************************************************************72
c
cc F_CHECK checks the parameters of the F PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c
c    Output, logical F_CHECK, is TRUE if the parameters are legal.
c
      implicit none

      logical f_check
      integer m
      integer n

      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_CHECK - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1.'
        f_check = .false.
        return
      end if

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_CHECK - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        f_check = .false.
        return
      end if

      f_check = .true.

      return
      end
      subroutine f_mean ( m, n, mean )

c*********************************************************************72
c
cc F_MEAN returns the mean of the F central PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c    Note, however, that the mean is not defined unless 3 .le. N.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer m
      double precision mean
      integer n

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_MEAN - Fatal error!'
        write ( *, '(a)' ) '  The mean is not defined for N .lt. 3.'
        stop
      end if

      mean = dble ( n ) / dble ( n - 2 )

      return
      end
      subroutine f_pdf ( x, m, n, pdf )

c*********************************************************************72
c
cc F_PDF evaluates the F central PDF.
c
c  Discussion:
c
c    PDF(M,N;X) = M^(M/2) * X^((M-2)/2)
c      / ( Beta(M/2,N/2) * N^(M/2) * ( 1 + (M/N) * X )^((M+N)/2)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision beta
      double precision bot1
      double precision bot2
      integer m
      integer n
      double precision pdf
      double precision top
      double precision x

      if ( x .lt. 0.0D+00 ) then

        pdf = 0.0D+00

      else

        a = dble ( m )
        b = dble ( n )

        top = sqrt ( a**m * b**n * x**( m - 2 ) )
        bot1 = beta ( a / 2.0D+00, b / 2.0D+00 )
        bot2 =  sqrt ( ( b + a * x ) ** ( m + n ) )

        pdf = top / ( bot1 * bot2 )

      end if

      return
      end
      subroutine f_sample ( m, n, seed, x )

c*********************************************************************72
c
cc F_SAMPLE samples the F central PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      integer m
      integer n
      integer seed
      double precision x
      double precision xm
      double precision xn

      a = dble ( m )
      call chi_square_sample ( a, seed, xm )

      a = dble ( n )
      call chi_square_sample ( a, seed, xn )

      x = dble ( n ) * xm / ( dble ( m  ) * xn )

      return
      end
      subroutine f_variance ( m, n, variance )

c*********************************************************************72
c
cc F_VARIANCE returns the variance of the F central PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c    Note, however, that the variance is not defined unless 5 .le. N.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer m
      integer n
      double precision variance

      if ( n .lt. 5 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  The variance is not defined for N .lt. 5.'
        stop
      end if

      variance = dble ( 2 * n * n * ( m + n - 2 ) ) 
     &  / dble ( m * ( n - 2 )**2 * ( n - 4 ) )

      return
      end
      subroutine f_noncentral_cdf_values ( n_data, n1, n2, lambda,
     &  x, fx )

c*********************************************************************72
c
cc F_NONCENTRAL_CDF_VALUES returns some values of the F CDF test function.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = NoncentralFRatioDistribution [ n1, n2, lambda ]
c      CDF [ dist, x ]
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N1, integer N2, the numerator and denominator
c    degrees of freedom.
c
c    Output, double precision LAMBDA, the noncentrality parameter.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 22 )

      double precision fx
      double precision fx_vec(n_max)
      double precision lambda
      double precision lambda_vec(n_max)
      integer n_data
      integer n1
      integer n1_vec(n_max)
      integer n2
      integer n2_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save lambda_vec
      save n1_vec
      save n2_vec
      save x_vec

      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.6367825323508774D+00,
     &  0.5840916116305482D+00,
     &  0.3234431872392788D+00,
     &  0.4501187879813550D+00,
     &  0.6078881441188312D+00,
     &  0.7059275551414605D+00,
     &  0.7721782003263727D+00,
     &  0.8191049017635072D+00,
     &  0.3170348430749965D+00,
     &  0.4327218008454471D+00,
     &  0.4502696915707327D+00,
     &  0.4261881186594096D+00,
     &  0.6753687206341544D+00,
     &  0.4229108778879005D+00,
     &  0.6927667261228938D+00,
     &  0.3632174676491226D+00,
     &  0.4210054012695865D+00,
     &  0.4266672258818927D+00,
     &  0.4464016600524644D+00,
     &  0.8445888579504827D+00,
     &  0.4339300273343604D+00 /
      data lambda_vec /
     &  0.00D+00,
     &  0.00D+00,
     &  0.25D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  0.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00 /
      data n1_vec /
     &   1,  1,  1,  1,
     &   1,  1,  1,  1,
     &   1,  1,  2,  2,
     &   3,  3,  4,  4,
     &   5,  5,  6,  6,
     &   8, 16 /
      data n2_vec /
     &   1,  5,  5,  5,
     &   5,  5,  5,  5,
     &   5,  5,  5, 10,
     &   5,  5,  5,  5,
     &   1,  5,  6, 12,
     &  16,  8 /
      data x_vec /
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  0.50D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  3.00D+00,
     &  4.00D+00,
     &  5.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  2.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n1 = 0
        n2 = 0
        lambda = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        n1 = n1_vec(n_data)
        n2 = n2_vec(n_data)
        lambda = lambda_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function f_noncentral_check ( a, m, n )

c*********************************************************************72
c
cc F_NONCENTRAL_CHECK checks the parameters of the F noncentral PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c
c    Input, integer M, N, the parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c
c    Output, logical F_NONCENTRAL_CHECK, is TRUE if the parameters are legal.
c
      implicit none

      double precision a
      logical f_noncentral_check
      integer m
      integer n

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_NONCENTRAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        f_noncentral_check = .false.
        return
      end if

      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_NONCENTRAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1.'
        f_noncentral_check = .false.
        return
      end if

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_NONCENTRAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 1.'
        f_noncentral_check = .false.
        return
      end if

      f_noncentral_check = .true.

      return
      end
      subroutine f_noncentral_mean ( a, m, n, mean )

c*********************************************************************72
c
cc F_NONCENTRAL_MEAN returns the mean of the F noncentral PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c
c    Input, integer M, N, parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c    Note, however, that the mean is not defined unless 3 .le. N.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      integer m
      double precision mean
      integer n

      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_NONCENTRAL_MEAN - Fatal error!'
        write ( *, '(a)' ) '  The mean is not defined for N .lt. 3.'
        stop
      end if

      mean = ( dble ( m ) + a ) * dble ( n )
     &  / ( dble ( m ) * dble ( n - 2 ) )

      return
      end
      subroutine f_noncentral_variance ( a, m, n, variance )

c*********************************************************************72
c
cc F_NONCENTRAL_VARIANCE returns the variance of the F noncentral PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c
c    Input, integer M, N, parameters of the PDF.
c    1 .le. M,
c    1 .le. N.
c    Note, however, that the variance is not defined unless 5 .le. N.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      integer m
      double precision mr
      integer n
      double precision nr
      double precision variance

      if ( n .lt. 5 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'F_NONCENTRAL_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  The variance is not defined for N .lt. 5.'
        stop
      end if

      mr = dble ( m )
      nr = dble ( n )

      variance = ( ( mr + a )**2 + 2.0D+00 * ( mr + a ) * nr**2 )
     &  / ( ( nr - 2.0D+00 ) * ( nr - 4.0D+00 ) * mr**2 ) 
     &  - ( mr + a )**2 * nr**2 / ( ( nr - 2.0D+00 )**2 * mr**2 )

      return
      end
      function factorial_log ( n )

c*********************************************************************72
c
cc FACTORIAL_LOG returns the logarithm of N factorial.
c
c  Discussion:
c
c    N! = Product ( 1 .le. I .le. N ) I
c
c    N! = Gamma(N+1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the function.
c    0 .le. N.
c
c    Output, double precision FACTORIAL_LOG, the logarithm of N!.
c
      implicit none

      double precision factorial_log
      integer i
      integer n

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FACTORIAL_LOG - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop
      end if

      factorial_log = 0.0D+00

      do i = 2, n
        factorial_log = factorial_log + log ( dble ( i ) )
      end do

      return
      end
      function factorial_stirling ( n )

c*********************************************************************72
c
cc FACTORIAL_STIRLING computes Stirling's approximation to N!.
c
c  Discussion:
c
c    This routine returns the raw approximation for all nonnegative
c    values of N.  If N is less than 0, the value is returned as 0,
c    and if N is 0, the value of 1 is returned.  In all other cases,
c    Stirling's formula is used.
c
c    The factorial function N! is defined by
c
c      N! = Product ( 1 .le. I .le. N ) I
c
c    Stirling's approximation to N! is
c
c      Stirling ( N ) = sqrt ( 2 * PI * N ) * ( N / E )^N * E^(1/(12*N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the function.
c
c    Output, double precision FACTORIAL_STIRLING, an approximation to N!.
c
      implicit none

      double precision e_natural
      parameter ( e_natural = 2.718281828459045D+00 )
      double precision factorial_stirling
      integer n
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision value

      if ( n .lt. 0 ) then

        value = 0.0D+00

      else if ( n .eq. 0 ) then

        value = 1.0D+00

      else

        value = sqrt ( 2.0D+00 * pi * dble ( n ) )
     &    * ( dble ( n ) / e_natural )**n 
     &    * exp ( 1.0D+00 / dble ( 12 * n ) )

      end if

      factorial_stirling = value

      return
      end
      subroutine fermi_dirac_sample ( u, v, seed, z )

c*********************************************************************72
c
cc FERMI_DIRAC_SAMPLE samples a (continuous) Fermi-Dirac distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2008
c
c  Author:
c
c    Original BASIC version by Frederick Ruckdeschel.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    Frederick Ruckdeschel,
c    BASIC Scientific       subroutines,
c    Volume I,
c    McGraw Hill, 1980,
c    ISBN: 0-07-054201-5,
c    LC: QA76.95.R82.
c
c  Parameters:
c
c    Input, double precision U, V, the parameters of the distribution.
c    The value of U represents the halfway point for the distribution.
c    Half the probability is to the left, and half to the right, of
c    the value U.  The value of V controls the shape of the distribution.
c    The ratio U/V determines the relative shape of the distribution.
c    Values of U/V in excess of 100 will risk overflow.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision Z, a sample from the Fermi-Dirac distribution.
c    Output values will be nonnegative, and roughly half of them should
c    be less than or equal to U.
c
      implicit none

      double precision a
      double precision b
      integer it_max
      parameter ( it_max = 1000 )
      integer iter_num
      double precision r8_uniform_01
      integer seed
      double precision u
      double precision v
      double precision x
      double precision y
      double precision y1
      double precision z

      x = r8_uniform_01 ( seed )
      y = 1.0D+00
      a = exp ( 4.0D+00 * u / v )
      b = ( x - 1.0D+00 ) * log ( 1.0D+00 + a )

      iter_num = 0

10    continue

        y1 = b + log ( a + exp ( y ) )

        if ( abs ( y - y1 ) .lt. 0.001D+00 ) then
          go to 20
        end if

        y = y1

        iter_num = iter_num + 1

        if ( it_max .lt. iter_num ) then
          go to 20
        end if

      go to 10

20    continue

      z = v * y1 / 4.0D+00

      return
      end
      subroutine fisher_pdf ( x, kappa, mu, pdf )

c*********************************************************************72
c
cc FISHER_PDF evaluates the Fisher PDF.
c
c  Discussion:
c
c    The formula for the PDF is:
c
c      PDF(KAPPA,MU;X) = C(KAPPA) * exp ( KAPPA * MU' * X )
c
c    where:
c
c      0 .le. KAPPA is the concentration parameter,
c      MU is a point on the unit sphere, the mean direction,
c      X is any point on the unit sphere,
c      and C(KAPPA) is a normalization factor:
c
c      C(KAPPA) = sqrt ( KAPPA ) / ( ( 2 * pi )^(3/2) * I(0.5,KAPPA) )
c
c    where
c
c      I(nu,X) is the Bessel function of order NU and argument X.
c
c    For a fixed value of MU, the value of KAPPA determines the
c    tendency of sample points to tend to be near MU.  In particular,
c    KAPPA = 0 corresponds to a uniform distribution of points on the
c    sphere, but as KAPPA increases, the sample points will tend to
c    cluster more closely to MU.
c
c    The Fisher distribution for points on the unit sphere is
c    analogous to the normal distribution of points on a line,
c    and, more precisely, to the von Mises distribution on a circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Kanti Mardia, Peter Jupp,
c    Directional Statistics,
c    Wiley, 2000,
c    LC: QA276.M335
c
c  Parameters:
c
c    Input, double precision X(3), the argument of the PDF.
c    X should have unit Euclidean norm, but this routine will
c    automatically work with a normalized version of X.
c
c    Input, double precision KAPPA, the concentration parameter.
c    0 .le. KAPPA is required.
c
c    Input, double precision MU(3), the mean direction.
c    MU should have unit Euclidean norm, but this routine will
c    automatically work with a normalized version of MU.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer nb
      parameter ( nb = 1 )

      double precision alpha
      double precision arg
      double precision b(nb)
      double precision cf
      integer ize
      double precision kappa
      double precision mu(3)
      double precision mu_norm
      integer ncalc
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8vec_dot_product
      double precision x(3)
      double precision x_norm

      if ( kappa .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISHER_PDF - Fatal error!'
        write ( *, '(a)' ) '  KAPPA must be nonnegative.'
        write ( *, '(a,g14.6)' ) '  Input KAPPA = ', kappa
        stop
      end if

      if ( kappa .eq. 0.0D+00 ) then
        pdf = 1.0D+00 / ( 4.0D+00 * pi )
        return
      end if
c
c  Compute the normalization factor CF.
c
      alpha = 0.5D+00
      ize = 1

      call ribesl ( kappa, alpha, nb, ize, b, ncalc )

      cf = sqrt ( kappa ) / ( sqrt ( ( 2.0D+00 * pi )**3 ) * b(1) )
c
c  Normalize MU.
c
      mu_norm = sqrt ( mu(1)**2 + mu(2)**2 + mu(3)**2 )

      if ( mu_norm .eq. 0.0D+00 ) then
        pdf = cf
        return
      end if
c
c  Normalize X.
c
      x_norm = sqrt ( x(1)**2 + x(2)**2 + x(3)**2 )

      if ( x_norm .eq. 0.0D+00 ) then
        pdf = cf
        return
      end if
c
c  Evaluate the PDF.
c
      arg = kappa * r8vec_dot_product ( 3, x, mu ) /
     &  ( x_norm * mu_norm )

      pdf = cf * exp ( arg )

      return
      end
      subroutine fisher_sample ( kappa, mu, n, seed, xyz )

c*********************************************************************72
c
cc FISHER_SAMPLE samples the Fisher distribution.
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
c  Reference:
c
c    Nicholas Fisher, Toby Lewis, Brian Embleton,
c    Statistical Analysis of Spherical Data,
c    Cambridge, 2003,
c    ISBN13: 978-0521456999,
c    LC: QA276.F489.
c
c  Parameters:
c
c    Input, double precision KAPPA, the concentration parameter.
c
c    Input, double precision MU(3), the mean direction.
c    MU should have unit Euclidean norm, but this routine will
c    automatically work with a normalized version of MU.
c
c    Input, integer N, the number of samples to choose.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision XYZ(3,N), a sample of the Fisher distribution.
c
c  Local Parameters:
c
c    Local, double precision ALPHA, BETA, the colatitude (theta) and
c    longitude (phi) of the mean direction.
c
      implicit none

      integer n

      double precision a(3,3)
      double precision alpha
      double precision beta
      integer i
      double precision kappa
      double precision lambda
      double precision mu(3)
      double precision mu_norm
      double precision phi(n)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision theta(n)
      double precision xyz(3,n)

      mu_norm = sqrt ( mu(1)**2 + mu(2)**2 + mu(3)**2 )

      if ( mu_norm .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISHER_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  MU = 0.'
        stop
      end if

      alpha = - acos ( mu(3) / mu_norm )
      beta = atan2 ( mu(2), mu(1) )

      lambda = exp ( - 2.0D+00 * kappa )

      call r8vec_uniform_01 ( n, seed, theta )

      if ( kappa .eq. 0.0D+00 ) then

        do i = 1, n
          theta(i) = 2.0D+00 * asin ( sqrt ( 1.0D+00 - theta(i) ) )
        end do

      else

        do i = 1, n
          theta(i) = 2.0D+00 * asin ( sqrt ( - log ( theta(i) * 
     &      ( 1.0D+00 - lambda ) + lambda ) / ( 2.0D+00 * kappa ) ) )
        end do

      end if

      call r8vec_uniform_01 ( n, seed, phi )

      do i = 1, n
        phi(i) = 2.0D+00 * pi * phi(i)
      end do
c
c  Compute the unrotated points.
c
      xyz(1,1:n) = sin ( theta(1:n) ) * cos ( phi(1:n) )
      xyz(2,1:n) = sin ( theta(1:n) ) * sin ( phi(1:n) )
      xyz(3,1:n) = cos ( theta(1:n) )
c
c  Compute the rotation matrix.
c
      a(1,1) =   cos ( alpha ) * cos ( beta )
      a(2,1) =                 - sin ( beta )
      a(3,1) =   sin ( alpha ) * cos ( beta )

      a(1,2) =   cos ( alpha ) * sin ( beta )
      a(2,2) =                 + cos ( beta )
      a(3,2) =   sin ( alpha ) * sin ( beta )

      a(1,3) = - sin ( alpha )
      a(2,3) =   0.0D+00
      a(3,3) =   cos ( alpha )
c
c  Rotate the points.
c
      xyz(1:3,1:n) = matmul ( a(1:3,1:3), xyz(1:3,1:n) )

      return
      end
      subroutine fisk_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc FISK_CDF evaluates the Fisk CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x

      if ( x .le. a ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 / ( 1.0D+00 + ( b / ( x - a ) )**c )
      end if

      return
      end
      subroutine fisk_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc FISK_CDF_INV inverts the Fisk CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_huge
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISK_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .le. 0.0D+00 ) then
        x = a
      else if ( cdf .lt. 1.0D+00 ) then
        x = a + b * ( cdf / ( 1.0D+00 - cdf ) )**( 1.0D+00 / c )
      else if ( 1.0D+00 .le. cdf ) then
        x = r8_huge ( )
      end if

      return
      end
      function fisk_check ( a, b, c )

c*********************************************************************72
c
cc FISK_CHECK checks the parameters of the Fisk PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, logical FISK_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical fisk_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISK_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        fisk_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISK_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        fisk_check = .false.
        return
      end if

      fisk_check = .true.

      return
      end
      subroutine fisk_mean ( a, b, c, mean )

c*********************************************************************72
c
cc FISK_MEAN returns the mean of the Fisk PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_csc

      if ( c .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISK_MEAN - Fatal error!'
        write ( *, '(a)' ) '  No mean defined for C .le. 1.0'
        stop
      end if

      mean = a + pi * ( b / c ) * r8_csc ( pi / c )

      return
      end
      subroutine fisk_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc FISK_PDF evaluates the Fisk PDF.
c
c  Discussion:
c
c    The Fisk PDF is also known as the Log Logistic PDF.
c
c    The formula for the PDF is:
c
c    PDF(A,B,C;X) =
c      ( C / B ) * ( ( X - A ) / B )^( C - 1 ) /
c      ( 1 + ( ( X - A ) / B )^C )^2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision x
      double precision y

      if ( x .le. a ) then

        pdf = 0.0D+00

      else

        y = ( x - a ) / b

        pdf = ( c / b ) * y**( c - 1.0D+00 ) / ( 1.0D+00 + y**c )**2

      end if

      return
      end
      subroutine fisk_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc FISK_SAMPLE samples the Fisk PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call fisk_cdf_inv ( cdf, a, b, c, x )

      return
      end
      subroutine fisk_variance ( a, b, c, variance )

c*********************************************************************72
c
cc FISK_VARIANCE returns the variance of the Fisk PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision g
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_csc
      double precision variance

      if ( c .le. 2.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FISK_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  No variance defined for C .le. 2.0'
        stop
      end if

      g = pi / c

      variance = b * b     * ( 2.0D+00 * g * r8_csc ( 2.0D+00 * g ) - ( 
     &g * r8_csc ( g ) )**2 )

      return
      end
      subroutine folded_normal_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc FOLDED_NORMAL_CDF evaluates the Folded Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    0.0D+00 .le. X.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision x
      double precision x1
      double precision x2

      if ( x .lt. 0.0D+00 ) then
        cdf = 0.0D+00
      else
        x1 = ( x - a ) / b
        call normal_01_cdf ( x1, cdf1 )
        x2 = ( - x - a ) / b
        call normal_01_cdf ( x2, cdf2 )
        cdf = cdf1 - cdf2
      end if

      return
      end
      subroutine folded_normal_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc FOLDED_NORMAL_CDF_INV inverts the Folded Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the argument of the CDF.
c    0.0D+00 .le. X.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision r8_huge
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3
      double precision xa
      double precision xb

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FOLDED_NORMAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = 0.0D+00
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
        return
      end if
c
c  Find X1, for which the value of CDF will be too small.
c
      if ( 0.0D+00 .le. a ) then
        call normal_cdf_inv ( cdf, a, b, x1 )
      else
        call normal_cdf_inv ( cdf, -a, b, x1 )
      end if
      x1 = max ( x1, 0.0D+00 )
      call folded_normal_cdf ( x1, a, b, cdf1 )
c
c  Find X2, for which the value of CDF will be too big.
c
      cdf2 = ( 1.0D+00 - cdf ) / 2.0D+00

      call normal_cdf_inv ( cdf2, a, b, xa )
      call normal_cdf_inv ( cdf2, -a, b, xb )
      x2 = max ( abs ( xa ), abs ( xb ) )
      call folded_normal_cdf ( x2, a, b, cdf2 )
c
c  Now use bisection.
c
      it = 0

10    continue

        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call folded_normal_cdf ( x3, a, b, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          go to 20
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'FOLDED_NORMAL_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          stop
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 10

20    continue

      return
      end
      function folded_normal_check ( a, b )

c*********************************************************************72
c
cc FOLDED_NORMAL_CHECK checks the parameters of the Folded Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
c
c    Output, logical FOLDED_NORMAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical folded_normal_check

      if ( a .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FOLDED_NORMAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 0.'
        folded_normal_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FOLDED_NORMAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        folded_normal_check = .false.
        return
      end if

      folded_normal_check = .true.

      return
      end
      subroutine folded_normal_mean ( a, b, mean )

c*********************************************************************72
c
cc FOLDED_NORMAL_MEAN returns the mean of the Folded Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision cdf
      double precision mean
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      a2 = a / b

      call normal_01_cdf ( a2, cdf )

      mean = b * sqrt ( 2.0D+00 / PI ) * exp ( - 0.5D+00 * a2 * a2 )    
     & - a * ( 1.0D+00 - 2.0D+00 * cdf )

      return
      end
      subroutine folded_normal_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc FOLDED_NORMAL_PDF evaluates the Folded Normal PDF.
c
c  Discussion:
c
c    The formula for the PDF is:
c
c    PDF(A;X) = sqrt ( 2 / PI ) * ( 1 / B ) * cosh ( A * X / B^2 )
c      * exp ( - 0.5D+00 * ( X^2 + A^2 ) / B^2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = sqrt ( 2.0D+00 / PI ) * ( 1.0D+00 / b ) * cosh ( a * x / b
     &**2 )       * exp ( - 0.5D+00 * ( x * x + a * a ) / b**2 )
      end if

      return
      end
      subroutine folded_normal_sample ( a, b, seed, x )

c*********************************************************************72
c
cc FOLDED_NORMAL_SAMPLE samples the Folded Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
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

      call folded_normal_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine folded_normal_variance ( a, b, variance )

c*********************************************************************72
c
cc FOLDED_NORMAL_VARIANCE returns the variance of the Folded Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean
      double precision variance

      call folded_normal_mean ( a, b, mean )

      variance = a * a + b * b - mean * mean

      return
      end
      subroutine frechet_cdf ( x, alpha, cdf )

c*********************************************************************72
c
cc FRECHET_CDF evaluates the Frechet CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ALPHA, the parameter.
c    It is required that 0.0 .lt. ALPHA.
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision alpha
      double precision cdf
      double precision x

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_CDF - Fatal error!'
        write ( *, '(a)' ) '  ALPHA .le. 0.0.'
        stop
      end if

      if ( x .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else
        cdf = exp ( - 1.0D+00 / x**alpha )
      end if

      return
      end
      subroutine frechet_cdf_inv ( cdf, alpha, x )

c*********************************************************************72
c
cc FRECHET_CDF_INV inverts the Frechet CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision ALPHA, the parameter.
c    It is required that 0.0 .lt. ALPHA.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision alpha
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  ALPHA .le. 0.0.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = 0.0D+00
      else
        x =  ( - 1.0D+00 / log ( cdf ) ) ** ( 1.0D+00 / alpha )
      end if

      return
      end
      subroutine frechet_mean ( alpha, mean )

c*********************************************************************72
c
cc FRECHET_MEAN returns the mean of the Frechet PDF.
c
c  Discussion:
c
c    The distribution does not have a mean value unless 1 .lt. ALPHA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ALPHA, the parameter.
c    It is required that 1.0 .lt. ALPHA.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision alpha
      double precision mean
      double precision r8_gamma

      if ( alpha .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_MEAN - Fatal error!'
        write ( *, '(a)' ) '  Mean does not exist if ALPHA .le. 1.'
        stop
      end if

      mean = r8_gamma ( ( alpha - 1.0D+00 ) / alpha )

      return
      end
      subroutine frechet_pdf ( x, alpha, pdf )

c*********************************************************************72
c
cc FRECHET_PDF evaluates the Frechet PDF.
c
c  Discussion:
c
c    PDF(X) = ALPHA * exp ( -1 / X^ALPHA ) / X^(ALPHA+1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision ALPHA, the parameter.
c    It is required that 0.0 .lt. ALPHA.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision alpha
      double precision pdf
      double precision x

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_PDF - Fatal error!'
        write ( *, '(a)' ) '  ALPHA .le. 0.0.'
        stop
      end if

      pdf = alpha * exp ( - 1.0D+00 / x**alpha ) / x**( alpha + 1.0D+00 
     &)

      return
      end
      subroutine frechet_sample ( alpha, seed, x )

c*********************************************************************72
c
cc FRECHET_SAMPLE samples the Frechet PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ALPHA, the parameter.
c    It is required that 0.0 .lt. ALPHA.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision alpha
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      if ( alpha .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  ALPHA .le. 0.0.'
        stop
      end if

      cdf = r8_uniform_01 ( seed )

      call frechet_cdf_inv ( cdf, alpha, x )

      return
      end
      subroutine frechet_variance ( alpha, variance )

c*********************************************************************72
c
cc FRECHET_VARIANCE returns the variance of the Frechet PDF.
c
c  Discussion:
c
c    The PDF does not have a variance unless 2 .lt. ALPHA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ALPHA, the parameter.
c    It is required that 2.0 .lt. ALPHA.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision alpha
      double precision mean
      double precision r8_gamma
      double precision variance

      if ( alpha .le. 2.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FRECHET_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  Variance does not exist if ALPHA .le. 2.'
        stop
      end if

      mean = r8_gamma ( ( alpha - 1.0D+00 ) / alpha )

      variance = r8_gamma ( ( alpha - 2.0D+00 ) / alpha ) - mean * mean

      return
      end
      subroutine gamma_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc GAMMA_CDF evaluates the Gamma CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision gamma_inc
      double precision p2
      double precision x
      double precision x2

      x2 = ( x - a ) / b
      p2 = c

      cdf = gamma_inc ( p2, x2 )

      return
      end
      subroutine gamma_cdf_values ( n_data, mu, sigma, x, fx )

c*********************************************************************72
c
cc GAMMA_CDF_VALUES returns some values of the Gamma CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = GammaDistribution [ mu, sigma ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision MU, the mean of the distribution.
c
c    Output, double precision SIGMA, the variance of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx_vec(n_max)
      double precision mu
      double precision mu_vec(n_max)
      integer n_data
      double precision sigma
      double precision sigma_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save mu_vec
      save sigma_vec
      save x_vec

      data fx_vec /
     &  0.8646647167633873D+00,
     &  0.9816843611112658D+00,
     &  0.9975212478233336D+00,
     &  0.9996645373720975D+00,
     &  0.6321205588285577D+00,
     &  0.4865828809674080D+00,
     &  0.3934693402873666D+00,
     &  0.3296799539643607D+00,
     &  0.4421745996289254D+00,
     &  0.1911531694619419D+00,
     &  0.6564245437845009D-01,
     &  0.1857593622214067D-01 /
      data mu_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data sigma_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        mu = 0.0D+00
        sigma = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        mu = mu_vec(n_data)
        sigma = sigma_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function gamma_check ( a, b, c )

c*********************************************************************72
c
cc GAMMA_CHECK checks the parameters of the Gamma PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, logical GAMMA_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical gamma_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GAMMA_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        write ( *, '(a,g14.6)' ) '  B = ', b
        gamma_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GAMMA_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        write ( *, '(a,g14.6)' ) '  C = ', c
        gamma_check = .false.
        return
      end if

      gamma_check = .true.

      return
      end
      function gamma_inc ( p, x )

c*********************************************************************72
c
cc GAMMA_INC computes the incomplete Gamma function.
c
c  Discussion:
c
c    GAMMA_INC(P,       0) = 0,
c    GAMMA_INC(P,Infinity) = 1.
c
c    GAMMA_INC(P,X) = Integral ( 0 .le. T .le. X ) T**(P-1) EXP(-T) DT / GAMMA(P).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 2001
c
c  Author:
c
c    Original FORTRAN77 version by B L Shea.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    BL Shea,
c    Chi-squared and Incomplete Gamma Integral,
c    Algorithm AS239,
c    Applied Statistics,
c    Volume 37, Number 3, 1988, pages 466-473.
c
c  Parameters:
c
c    Input, double precision P, the exponent parameter.
c    0.0D+00 .lt. P.
c
c    Input, double precision X, the integral limit parameter.
c    If X is less than or equal to 0, GAMMA_INC is returned as 0.
c
c    Output, double precision GAMMA_INC, the value of the function.
c
      implicit none

      double precision a
      double precision arg
      double precision b
      double precision c
      double precision cdf
      double precision exp_arg_min
      parameter ( exp_arg_min = -88.0D+00 )
      double precision gamma_inc
      double precision gamma_log
      double precision overflow
      parameter ( overflow = 1.0D+37 )
      double precision p
      double precision, parameter :: plimit = 1000.0D+00
      double precision pn1
      double precision pn2
      double precision pn3
      double precision pn4
      double precision pn5
      double precision pn6
      double precision rn
      double precision, parameter :: tol = 1.0D-07
      double precision x
      double precision, parameter :: xbig = 1.0D+08

      gamma_inc = 0.0D+00

      if ( p .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GAMMA_INC - Fatal error!'
        write ( *, '(a)' ) '  Parameter P .le. 0.'
        stop
      end if

      if ( x .le. 0.0D+00 ) then
        gamma_inc = 0.0D+00
        return
      end if
c
c  Use a normal approximation if PLIMIT .lt. P.
c
      if ( plimit .lt. p ) then
        pn1 = 3.0D+00 * sqrt ( p ) * ( ( x / p ) ** ( 1.0D+00 / 3.0D+00 
     &)       + 1.0D+00 / ( 9.0D+00 * p ) - 1.0D+00 )
        call normal_01_cdf ( pn1, cdf )
        gamma_inc = cdf
        return
      end if
c
c  Is X extremely large compared to P?
c
      if ( xbig .lt. x ) then
        gamma_inc = 1.0D+00
        return
      end if
c
c  Use Pearson's series expansion.
c  (P is not large enough to force overflow in the log of Gamma.
c
      if ( x .le. 1.0D+00 .or. x .lt. p ) then

        arg = p * log ( x ) - x - gamma_log ( p + 1.0D+00 )
        c = 1.0D+00
        gamma_inc = 1.0D+00
        a = p

10      continue

          a = a + 1.0D+00
          c = c * x / a
          gamma_inc = gamma_inc + c

          if ( c .le. tol ) then
            go to 20
          end if

        go to 10

20      continue

        arg = arg + log ( gamma_inc )

        if ( exp_arg_min .le. arg ) then
          gamma_inc = exp ( arg )
        else
          gamma_inc = 0.0D+00
        end if

      else
c
c  Use a continued fraction expansion.
c
        arg = p * log ( x ) - x - gamma_log ( p )
        a = 1.0D+00 - p
        b = a + x + 1.0D+00
        c = 0.0D+00
        pn1 = 1.0D+00
        pn2 = x
        pn3 = x + 1.0D+00
        pn4 = x * b
        gamma_inc = pn3 / pn4

30      continue

          a = a + 1.0D+00
          b = b + 2.0D+00
          c = c + 1.0D+00
          pn5 = b * pn3 - a * c * pn1
          pn6 = b * pn4 - a * c * pn2

          if ( 0.0D+00 .lt. abs ( pn6 ) ) then

            rn = pn5 / pn6

            if ( abs ( gamma_inc - rn ) .le. 
     &        min ( tol, tol * rn ) ) then

              arg = arg + log ( gamma_inc )

              if ( exp_arg_min .le. arg ) then
                gamma_inc = 1.0D+00 - exp ( arg )
              else
                gamma_inc = 1.0D+00
              end if

              return

            end if

            gamma_inc = rn

          end if

          pn1 = pn3
          pn2 = pn4
          pn3 = pn5
          pn4 = pn6
c
c  Rescale terms in continued fraction if terms are large.
c
          if ( overflow .le. abs ( pn5 ) ) then
            pn1 = pn1 / overflow
            pn2 = pn2 / overflow
            pn3 = pn3 / overflow
            pn4 = pn4 / overflow
          end if

        go to 30

      end if

      return
      end
      subroutine gamma_inc_values ( n_data, a, x, fx )

c*********************************************************************72
c
cc GAMMA_INC_VALUES: values of the incomplete Gamma function.
c
c  Discussion:
c
c    The incomplete Gamma function is defined as:
c
c      Integral ( X <= T < oo ) T^(A-1) * exp(-T) dT.
c
c    In Mathematica, the function can be evaluated by:
c
c      Gamma[A,X]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision A, the parameter of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision a
      double precision a_vec ( n_max )
      double precision fx
      double precision fx_vec ( n_max )
      integer n_data
      double precision x
      double precision x_vec ( n_max )

      data a_vec /
     &   0.10D+00,
     &   0.10D+00,
     &   0.10D+00,
     &   0.50D+00,
     &   0.50D+00,
     &   0.50D+00,
     &   0.10D+01,
     &   0.10D+01,
     &   0.10D+01,
     &   0.11D+01,
     &   0.11D+01,
     &   0.11D+01,
     &   0.20D+01,
     &   0.20D+01,
     &   0.20D+01,
     &   0.60D+01,
     &   0.60D+01,
     &   0.11D+02,
     &   0.26D+02,
     &   0.41D+02 /
      data fx_vec /
     &   2.490302836300570D+00,
     &   0.8718369702247978D+00,
     &   0.1079213896175866D+00,
     &   1.238121685818417D+00,
     &   0.3911298052193973D+00,
     &   0.01444722098952533D+00,
     &   0.9048374180359596D+00,
     &   0.3678794411714423D+00,
     &   0.006737946999085467D+00,
     &   0.8827966752611692D+00,
     &   0.3908330082003269D+00,
     &   0.008051456628620993D+00,
     &   0.9898141728888165D+00,
     &   0.5578254003710746D+00,
     &   0.007295055724436130D+00,
     &   114.9574754165633D+00,
     &   2.440923530031405D+00,
     &   280854.6620274718D+00,
     &   8.576480283455533D+24,
     &   2.085031346403364D+47 /
      data x_vec /
     &   0.30D-01,
     &   0.30D+00,
     &   0.15D+01,
     &   0.75D-01,
     &   0.75D+00,
     &   0.35D+01,
     &   0.10D+00,
     &   0.10D+01,
     &   0.50D+01,
     &   0.10D+00,
     &   0.10D+01,
     &   0.50D+01,
     &   0.15D+00,
     &   0.15D+01,
     &   0.70D+01,
     &   0.25D+01,
     &   0.12D+02,
     &   0.16D+02,
     &   0.25D+02,
     &   0.45D+02 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function gamma_log ( x )

c*********************************************************************72
c
cc GAMMA_LOG calculates the natural logarithm of GAMMA ( X ).
c
c  Discussion:
c
c    Computation is based on an algorithm outlined in references 1 and 2.
c    The program uses rational functions that theoretically approximate
c    LOG(GAMMA(X)) to at least 18 significant decimal digits.  The
c    approximation for 12 .lt. X is from Hart et al, while approximations
c    for X .lt. 12.0D+00 are similar to those in Cody and Hillstrom,
c    but are unpublished.
c
c    The accuracy achieved depends on the arithmetic system, the compiler,
c    intrinsic functions, and proper selection of the machine dependent
c    constants.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 June 1999
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    William Cody, Kenneth Hillstrom,
c    Chebyshev Approximations for the Natural Logarithm of the Gamma Function,
c    Mathematics of Computation,
c    Volume 21, 1967, pages 198-203.
c
c    Kenneth Hillstrom,
c    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
c    May 1969.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
c    Charles Mesztenyi, John Rice, Henry Thacher, Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968.
c
c  Parameters:
c
c    Input, double precision X, the argument of the Gamma function.
c    X must be positive.
c
c    Output, double precision GAMMA_LOG, the logarithm of the Gamma
c    function of X.
c
c  Local Parameters:
c
c    Local, double precision BETA, the radix for the floating-point
c    representation.
c
c    Local, integer MAXEXP, the smallest positive power of BETA that overflows.
c
c    Local, double precision XBIG, the largest argument for which
c    LN(GAMMA(X)) is representable in the machine, the solution to the equation
c      LN(GAMMA(XBIG)) = BETA**MAXEXP.
c
c    Local, double precision FRTBIG, a rough estimate of the fourth root
c    of XBIG.
c
c  Approximate values for some important machines are:
c
c                            BETA      MAXEXP         XBIG     FRTBIG
c
c  CRAY-1        (S.P.)        2        8191       9.62D+2461  3.13D+615
c  Cyber 180/855 (S.P.)        2        1070       1.72D+319   6.44D+79
c  IEEE (IBM/XT) (S.P.)        2         128       4.08D+36    1.42D+9
c  IEEE (IBM/XT) (D.P.)        2        1024       2.55D+305   2.25D+76
c  IBM 3033      (D.P.)       16          63       4.29D+73    2.56D+18
c  VAX D-Format  (D.P.)        2         127       2.05D+36    1.20D+9
c  VAX G-Format  (D.P.)        2        1023       1.28D+305   1.89D+76
c
      implicit none

      double precision, parameter, dimension ( 7 ) :: c = (/     -1.910
     &444077728D-03,      8.4171387781295D-04,     -5.952379913043012D-0
     &4,      7.93650793500350248D-04,     -2.777777777777681622553D-03,
     &      8.333333333333333331554247D-02,      5.7083835261D-03 /)
      double precision corr
      double precision d1
      parameter ( d1 = -5.772156649015328605195174D-01 )
      double precision d2
      parameter ( d2 =  4.227843350984671393993777D-01 )
      double precision, parameter :: d4 =  1.791759469228055000094023D+
     &00
      integer i
      double precision, parameter :: frtbig = 1.42D+09
      double precision gamma_log
      double precision, parameter, dimension ( 8 ) :: p1 = (/     4.945
     &235359296727046734888D+00,     2.018112620856775083915565D+02,    
     & 2.290838373831346393026739D+03,     1.131967205903380828685045D+0
     &4,     2.855724635671635335736389D+04,     3.848496228443793359990
     &269D+04,     2.637748787624195437963534D+04,     7.225813979700288
     &197698961D+03 /)
      double precision, parameter, dimension ( 8 ) :: p2 = (/     4.974
     &607845568932035012064D+00,     5.424138599891070494101986D+02,    
     & 1.550693864978364947665077D+04,     1.847932904445632425417223D+0
     &5,     1.088204769468828767498470D+06,     3.338152967987029735917
     &223D+06,     5.106661678927352456275255D+06,     3.074109054850539
     &556250927D+06 /)
      double precision, parameter, dimension ( 8 ) :: p4 = (/     1.474
     &502166059939948905062D+04,     2.426813369486704502836312D+06,    
     & 1.214755574045093227939592D+08,     2.663432449630976949898078D+0
     &9,     2.940378956634553899906876D+10,     1.702665737765398868392
     &998D+11,     4.926125793377430887588120D+11,     5.606251856223951
     &465078242D+11 /)
      double precision, parameter :: pnt68 = 0.6796875D+00
      double precision, parameter, dimension ( 8 ) :: q1 = (/     6.748
     &212550303777196073036D+01,     1.113332393857199323513008D+03,    
     & 7.738757056935398733233834D+03,     2.763987074403340708898585D+0
     &4,     5.499310206226157329794414D+04,     6.161122180066002127833
     &352D+04,     3.635127591501940507276287D+04,     8.785536302431013
     &170870835D+03 /)
      double precision, parameter, dimension ( 8 ) :: q2 = (/     1.830
     &328399370592604055942D+02,     7.765049321445005871323047D+03,    
     & 1.331903827966074194402448D+05,     1.136705821321969608938755D+0
     &6,     5.267964117437946917577538D+06,     1.346701454311101692290
     &052D+07,     1.782736530353274213975932D+07,     9.533095591844353
     &613395747D+06 /)
      double precision, parameter, dimension ( 8 ) :: q4 = (/     2.690
     &530175870899333379843D+03,     6.393885654300092398984238D+05,    
     & 4.135599930241388052042842D+07,     1.120872109616147941376570D+0
     &9,     1.488613728678813811542398D+10,     1.016803586272438228077
     &304D+11,     3.417476345507377132798597D+11,     4.463158187419713
     &286462081D+11 /)
      double precision r8_epsilon
      double precision r8_huge
      double precision res
      double precision sqrtpi
      parameter ( sqrtpi = 0.9189385332046727417803297D+00 )
      double precision x
      double precision xbig
      parameter ( xbig = 4.08D+36 )
      double precision xden
      double precision xm1
      double precision xm2
      double precision xm4
      double precision xnum
      double precision xsq
c
c  Return immediately if the argument is out of range.
c
      if ( x .le. 0.0D+00 .or. xbig .lt. x ) then
        gamma_log = r8_huge ( )
        return
      end if

      if ( x .le. r8_epsilon ( ) ) then

        res = -log ( x )

      else if ( x .le. 1.5D+00 ) then

        if ( x .lt. pnt68 ) then
          corr = - log ( x )
          xm1 = x
        else
          corr = 0.0D+00
          xm1 = ( x - 0.5D+00 ) - 0.5D+00
        end if

        if ( x .le. 0.5D+00 .or. pnt68 .le. x ) then

          xden = 1.0D+00
          xnum = 0.0D+00

          do i = 1, 8
            xnum = xnum * xm1 + p1(i)
            xden = xden * xm1 + q1(i)
          end do

          res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

        else

          xm2 = ( x - 0.5D+00 ) - 0.5D+00
          xden = 1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm2 + p2(i)
            xden = xden * xm2 + q2(i)
          end do

          res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

        end if

      else if ( x .le. 4.0D+00 ) then

        xm2 = x - 2.0D+00
        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm2 + p2(i)
          xden = xden * xm2 + q2(i)
        end do

        res = xm2 * ( d2 + xm2 * ( xnum / xden ) )

      else if ( x .le. 12.0D+00 ) then

        xm4 = x - 4.0D+00
        xden = - 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm4 + p4(i)
          xden = xden * xm4 + q4(i)
        end do

        res = d4 + xm4 * ( xnum / xden )

      else

        res = 0.0D+00

        if ( x .le. frtbig ) then

          res = c(7)
          xsq = x * x

          do i = 1, 6
            res = res / xsq + c(i)
          end do

        end if

        res = res / x
        corr = log ( x )
        res = res + sqrtpi - 0.5D+00 * corr
        res = res + x * ( corr - 1.0D+00 )

      end if

      gamma_log = res

      return
      end
      function gamma_log_int ( n )

c*********************************************************************72
c
cc GAMMA_LOG_INT computes the logarithm of Gamma of an integer N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the logarithm of the
c    Gamma function.  0 .lt. N.
c
c    Output, double precision GAMMA_LOG_INT, the logarithm of
c    the Gamma function of N.
c
      implicit none

      double precision gamma_log
      double precision gamma_log_int
      integer n

      if ( n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GAMMA_LOG_INT - Fatal error!'
        write ( *, '(a,i12)' ) '  Illegal input value of N = ', n
        write ( *, '(a)' ) '  But N must be strictly positive.'
        stop
      end if

      gamma_log_int = gamma_log ( dble ( n ) )

      return
      end
      subroutine gamma_mean ( a, b, c, mean )

c*********************************************************************72
c
cc GAMMA_MEAN returns the mean of the Gamma PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 September 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean

      mean = a + b * c

      return
      end
      subroutine gamma_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc GAMMA_PDF evaluates the Gamma PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = exp ( - ( X - A ) / B ) * ( ( X - A ) / B )^(C-1)
c      / ( B * GAMMA ( C ) )
c
c    GAMMA_PDF(A,B,C;X), where C is an integer, is the Erlang PDF.
c    GAMMA_PDF(A,B,1;X) is the Exponential PDF.
c    GAMMA_PDF(0,2,C/2;X) is the Chi Squared PDF with C degrees of freedom.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A controls the location of the peak;  A is often chosen to be 0.0.
c    B is the "scale" parameter; 0.0 .lt. B, and is often 1.0.
c    C is the "shape" parameter; 0.0 .lt. C, and is often 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision r8_gamma
      double precision x
      double precision y

      if ( x .le. a ) then

        pdf = 0.0D+00

      else

        y = ( x - a ) / b

        pdf = y**( c - 1.0D+00 ) / ( b * r8_gamma ( c ) * exp ( y ) )

      end if

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
c    15 September 2000
c
c  Author:
c
c    Original FORTRAN77 version by Joachim Ahrens, Ulrich Dieter.
c    FORTRAN90 version by John Burkardt
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
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a1
      parameter ( a1 =   0.3333333D+00 )
      double precision a2
      parameter ( a2 = - 0.2500030D+00 )
      double precision, parameter :: a3 =   0.2000062D+00
      double precision, parameter :: a4 = - 0.1662921D+00
      double precision, parameter :: a5 =   0.1423657D+00
      double precision, parameter :: a6 = - 0.1367177D+00
      double precision, parameter :: a7 =   0.1233795D+00
      double precision b
      double precision bcoef
      double precision c
      double precision co
      double precision d
      double precision r8_uniform_01
      double precision e
      double precision, parameter :: e1 = 1.0D+00
      double precision, parameter :: e2 = 0.4999897D+00
      double precision, parameter :: e3 = 0.1668290D+00
      double precision, parameter :: e4 = 0.0407753D+00
      double precision, parameter :: e5 = 0.0102930D+00
      double precision, parameter :: euler = 2.71828182845904D+00
      double precision p
      double precision q
      double precision q0
      double precision, parameter :: q1 =   0.04166669D+00
      double precision, parameter :: q2 =   0.02083148D+00
      double precision, parameter :: q3 =   0.00801191D+00
      double precision, parameter :: q4 =   0.00144121D+00
      double precision, parameter :: q5 = - 0.00007388D+00
      double precision, parameter :: q6 =   0.00024511D+00
      double precision q7
      parameter ( q7 =   0.00024240D+00 )
      double precision r
      double precision s
      integer seed
      double precision si
      double precision s2
      double precision t
      double precision u
      double precision v
      double precision w
      double precision x
c
c  Allow C = 0.
c
      if ( c .eq. 0.0D+00 ) then
        x = a
        return
      end if
c
c  C .lt. 1.
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
c  1 .le. C.
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
     &      q7   * r
     &    + q6 ) * r        
     &    + q5 ) * r
     &    + q4 ) * r
     &    + q3 ) * r
     &    + q2 ) * r
     &    + q1 ) * r

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
            q = q0 - s * t + 0.25D+00 * t * t + 2.0D+00 * s2 * log ( 1.0
     &D+00 + v )
          else
            q = q0 + 0.5D+00 * t**2 * ( ( ( ( ( (                a7   * 
     &v              + a6 ) * v              + a5 ) * v              + a
     &4 ) * v              + a3 ) * v              + a2 ) * v           
     &   + a1 ) * v
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
     &            a7   * v
     &          + a6 ) * v
     &          + a5 ) * v
     &          + a4 ) * v
     &          + a3 ) * v
     &          + a2 ) * v         
     &          + a1 ) * v
            end if

            if ( 0.0D+00 .lt. q ) then

              if ( 0.5D+00 .lt. q ) then
                w = exp ( q ) - 1.0D+00
              else
                w = ( ( ( (
     &            e5   * q                
     &          + e4 ) * q
     &          + e3 ) * q
     &          + e2 ) * q
     &          + e1 ) * q
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
      subroutine gamma_variance ( a, b, c, variance )

c*********************************************************************72
c
cc GAMMA_VARIANCE returns the variance of the Gamma PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision variance

      variance = b * b * c

      return
      end
      subroutine genlogistic_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc GENLOGISTIC_CDF evaluates the Generalized Logistic CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x
      double precision y

      y = ( x - a ) / b

      cdf = 1.0D+00 / ( 1.0D+00 + exp ( - y ) )**c

      return
      end
      subroutine genlogistic_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc GENLOGISTIC_CDF_INV inverts the Generalized Logistic CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_huge
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENLOGISTIC_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = - r8_huge ( )
      else if ( cdf .lt. 1.0D+00 ) then
        x = a - b * log ( cdf**( - 1.0D+00 / c ) - 1.0D+00 )
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
      end if

      return
      end
      function genlogistic_check ( a, b, c )

c*********************************************************************72
c
cc GENLOGISTIC_CHECK checks the parameters of the Generalized Logistic CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, logical GENLOGISTIC_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical genlogistic_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENLOGISTIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        genlogistic_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GENLOGISTIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        genlogistic_check = .false.
        return
      end if

      genlogistic_check = .true.

      return
      end
      subroutine genlogistic_mean ( a, b, c, mean )

c*********************************************************************72
c
cc GENLOGISTIC_MEAN returns the mean of the Generalized Logistic PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision digamma
      double precision euler_constant
      double precision mean

      mean = a + b * ( euler_constant ( ) + digamma ( c ) )

      return
      end
      subroutine genlogistic_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc GENLOGISTIC_PDF evaluates the Generalized Logistic PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = ( C / B ) * exp ( ( A - X ) / B ) /
c      ( ( 1 + exp ( ( A - X ) / B ) )^(C+1) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision x
      double precision y

      y = ( x - a ) / b

      pdf = ( c / b ) * exp ( - y ) / ( 1.0D+00 + exp ( - y ) )**( c + 1
     &.0D+00 )

      return
      end
      subroutine genlogistic_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc GENLOGISTIC_SAMPLE samples the Generalized Logistic PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call genlogistic_cdf_inv ( cdf, a, b, c, x )

      return
      end
      subroutine genlogistic_variance ( a, b, c, variance )

c*********************************************************************72
c
cc GENLOGISTIC_VARIANCE returns the variance of the Generalized Logistic PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision trigamma
      double precision variance

      variance = b * b * ( pi * pi / 6.0D+00 + trigamma ( c ) )

      return
      end
      subroutine geometric_cdf ( x, a, cdf )

c*********************************************************************72
c
cc GEOMETRIC_CDF evaluates the Geometric CDF.
c
c  Discussion:
c
c    CDF(X,P) is the probability that there will be at least one
c    successful trial in the first X Bernoulli trials, given that
c    the probability of success in a single trial is P.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the maximum number of trials.
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      integer x

      if ( x .le. 0 ) then
        cdf = 0.0D+00
      else if ( a .eq. 0.0D+00 ) then
        cdf = 0.0D+00
      else if ( a .eq. 1.0D+00 ) then
        cdf = 1.0D+00
      else
        cdf = 1.0D+00 - ( 1.0D+00 - a )**x
      end if

      return
      end
      subroutine geometric_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc GEOMETRIC_CDF_INV inverts the Geometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0D+00
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, integer X, the corresponding value of X.
c
      implicit none

      double precision a
      double precision cdf
      double precision i4_huge
      integer x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GEOMETRIC_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( a .eq. 1.0D+00 ) then
        x = 1
      else if ( a .eq. 0.0D+00 ) then
        x = i4_huge ( )
      else
        x = 1 + int ( log ( 1.0D+00 - cdf ) / log ( 1.0D+00 - a ) )
      end if

      return
      end
      subroutine geometric_cdf_values ( n_data, x, p, cdf )

c*********************************************************************72
c
cc GEOMETRIC_CDF_VALUES returns values of the geometric CDF.
c
c  Discussion:
c
c    The geometric or Pascal probability density function gives the
c    probability that the first success will happen on the X-th Bernoulli
c    trial, given that the probability of a success on a single trial is P.
c
c    The value of CDF ( X, P ) is the probability that the first success
c    will happen on or before the X-th trial.
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`DiscreteDistributions`]
c      dist = GeometricDistribution [ p ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, Steven Kokoska,
c    Standard Probability and Statistical Tables,
c    CRC Press, 2000,
c    ISBN: 1-58488-059-7,
c    LC: QA273.3.Z95.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer X, the number of trials.
c
c    Output, double precision P, the probability of success
c    on one trial.
c
c    Output, double precision CDF, the cumulative density function.
c
      implicit none

      integer n_max
      parameter ( n_max = 14 )

      double precision cdf
      double precision cdf_vec(n_max)
      integer n_data
      double precision p
      double precision p_vec(n_max)
      integer x
      integer x_vec(n_max)

      save cdf_vec
      save p_vec
      save x_vec

      data cdf_vec /
     &  0.1900000000000000D+00,
     &  0.2710000000000000D+00,
     &  0.3439000000000000D+00,
     &  0.6861894039100000D+00,
     &  0.3600000000000000D+00,
     &  0.4880000000000000D+00,
     &  0.5904000000000000D+00,
     &  0.9141006540800000D+00,
     &  0.7599000000000000D+00,
     &  0.8704000000000000D+00,
     &  0.9375000000000000D+00,
     &  0.9843750000000000D+00,
     &  0.9995117187500000D+00,
     &  0.9999000000000000D+00 /
      data p_vec /
     &  0.1D+00,
     &  0.1D+00,
     &  0.1D+00,
     &  0.1D+00,
     &  0.2D+00,
     &  0.2D+00,
     &  0.2D+00,
     &  0.2D+00,
     &  0.3D+00,
     &  0.4D+00,
     &  0.5D+00,
     &  0.5D+00,
     &  0.5D+00,
     &  0.9D+00 /
      data x_vec /
     &  1,  2,  3, 10, 1,
     &  2,  3, 10,  3, 3,
     &  3,  5, 10,  3 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0
        p = 0.0D+00
        cdf = 0.0D+00
      else
        x = x_vec(n_data)
        p = p_vec(n_data)
        cdf = cdf_vec(n_data)
      end if

      return
      end
      function geometric_check ( a )

c*********************************************************************72
c
cc GEOMETRIC_CHECK checks the parameter of the Geometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, logical GEOMETRIC_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical geometric_check

      if ( a .lt. 0.0D+00 .or. 1.0D+00 .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GEOMETRIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 0 or 1 .lt. A.'
        geometric_check = .false.
        return
      end if

      geometric_check = .true.

      return
      end
      subroutine geometric_mean ( a, mean )

c*********************************************************************72
c
cc GEOMETRIC_MEAN returns the mean of the Geometric PDF.
c
c  Discussion:
c
c    MEAN is the expected value of the number of trials required
c    to obtain a single success.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision mean

      mean = 1.0D+00 / a

      return
      end
      subroutine geometric_pdf ( x, a, pdf )

c*********************************************************************72
c
cc GEOMETRIC_PDF evaluates the Geometric PDF.
c
c  Discussion:
c
c    PDF(A;X) = A * ( 1 - A )^(X-1)
c
c    PDF(A;X) is the probability that exactly X Bernoulli trials, each
c    with probability of success A, will be required to achieve
c    a single success.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the number of trials.
c    0 .lt. X
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision pdf
      integer x
c
c  Special cases.
c
      if ( x .lt. 1 ) then

        pdf = 0.0D+00

      else if ( a .eq. 0.0D+00 ) then

        pdf = 0.0D+00

      else if ( a .eq. 1.0D+00 ) then

        if ( x .eq. 1 ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if

      else

        pdf = a * ( 1.0D+00 - a )**( x - 1 )

      end if

      return
      end
      subroutine geometric_sample ( a, seed, x )

c*********************************************************************72
c
cc GEOMETRIC_SAMPLE samples the Geometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call geometric_cdf_inv ( cdf, a, x )

      return
      end
      subroutine geometric_variance ( a, variance )

c*********************************************************************72
c
cc GEOMETRIC_VARIANCE returns the variance of the Geometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of success on one trial.
c    0.0D+00 .le. A .le. 1.0.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision variance

      variance = ( 1.0D+00 - a ) / ( a * a )

      return
      end
      subroutine gompertz_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc GOMPERTZ_CDF evaluates the Gompertz CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Norman Johnson, Samuel Kotz, Balakrishnan,
c    Continuous Univariate Distributions, second edition,
c    Wiley, 1994,
c    QA273.6.J6
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    1 .lt. A, 0 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - exp ( - b * ( a**x - 1.0D+00 ) / log ( a ) )
      end if

      return
      end
      subroutine gompertz_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc GOMPERTZ_CDF_INV inverts the Gompertz CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Norman Johnson, Samuel Kotz, Balakrishnan,
c    Continuous Univariate Distributions, second edition,
c    Wiley, 1994,
c    QA273.6.J6
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    1 .lt. A, 0 .lt. B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision r8_huge
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GOMPERTZ_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .lt. 1.0D+00 ) then
        x = log ( 1.0D+00 - log ( 1.0D+00 - cdf ) * log ( a ) / b  ) / l
     &og ( a )
      else
        x = r8_huge ( )
      end if

      return
      end
      function gompertz_check ( a, b )

c*********************************************************************72
c
cc GOMPERTZ_CHECK checks the parameters of the Gompertz PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Norman Johnson, Samuel Kotz, Balakrishnan,
c    Continuous Univariate Distributions, second edition,
c    Wiley, 1994,
c    QA273.6.J6
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    1 .lt. A, 0 .lt. B.
c
c    Output, logical GOMPERTZ_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical gompertz_check

      if ( a .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GOMPERTZ_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 1.0!'
        gompertz_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GOMPERTZ_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0!'
        gompertz_check = .false.
        return
      end if

      gompertz_check = .true.

      return
      end
      subroutine gompertz_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc GOMPERTZ_PDF evaluates the Gompertz PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = B * A^X / exp ( B * ( A^X - 1 ) / log ( A ) )
c
c    for
c
c      0.0 .le. X
c      1.0 .lt.  A
c      0.0 .lt.  B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Norman Johnson, Samuel Kotz, Balakrishnan,
c    Continuous Univariate Distributions, second edition,
c    Wiley, 1994,
c    QA273.6.J6
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    1 .lt. A, 0 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .lt. 0.0D+00 ) then

        pdf = 0.0D+00

      else if ( 1.0D+00 .lt. a ) then

        pdf = exp ( log ( b ) + x * log ( a )       - ( b / log ( a ) ) 
     &* ( a**x - 1.0D+00 ) )

      end if

      return
      end
      subroutine gompertz_sample ( a, b, seed, x )

c*********************************************************************72
c
cc GOMPERTZ_SAMPLE samples the Gompertz PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    1 .lt. A, 0 .lt. B.
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

      call gompertz_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine gumbel_cdf ( x, cdf )

c*********************************************************************72
c
cc GUMBEL_CDF evaluates the Gumbel CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      double precision x

      cdf = exp ( - exp ( - x ) )

      return
      end
      subroutine gumbel_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc GUMBEL_CDF_INV inverts the Gumbel CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GUMBEL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x =  - log ( - log ( cdf ) )

      return
      end
      subroutine gumbel_mean ( mean )

c*********************************************************************72
c
cc GUMBEL_MEAN returns the mean of the Gumbel PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision euler_constant
      double precision mean

      mean = euler_constant ( )

      return
      end
      subroutine gumbel_pdf ( x, pdf )

c*********************************************************************72
c
cc GUMBEL_PDF evaluates the Gumbel PDF.
c
c  Discussion:
c
c    PDF(X) = exp ( -X ) * exp ( - exp ( -X  ) ).
c
c    GUMBEL_PDF(X) = EXTREME_PDF(0,1;X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Eric Weisstein, editor,
c    CRC Concise Encylopedia of Mathematics,
c    CRC Press, 1998.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision x

      pdf = exp ( - x - exp ( - x ) )

      return
      end
      subroutine gumbel_sample ( seed, x )

c*********************************************************************72
c
cc GUMBEL_SAMPLE samples the Gumbel PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
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

      call gumbel_cdf_inv ( cdf, x )

      return
      end
      subroutine gumbel_variance ( variance )

c*********************************************************************72
c
cc GUMBEL_VARIANCE returns the variance of the Gumbel PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = pi * pi / 6.0D+00

      return
      end
      subroutine half_normal_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc HALF_NORMAL_CDF evaluates the Half Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf2
      double precision x

      if ( x .le. a ) then
        cdf = 0.0D+00
      else
        call normal_cdf ( x, a, b, cdf2 )
        cdf = 2.0D+00 * cdf2 - 1.0D+00
      end if

      return
      end
      subroutine half_normal_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc HALF_NORMAL_CDF_INV inverts the Half Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf2
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HALF_NORMAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cdf2 = 0.5D+00 * ( cdf + 1.0D+00 )

      call normal_cdf_inv ( cdf2, a, b, x )

      return
      end
      function half_normal_check ( a, b )

c*********************************************************************72
c
cc HALF_NORMAL_CHECK checks the parameters of the Half Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical HALF_NORMAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical half_normal_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HALF_NORMAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        half_normal_check = .false.
        return
      end if

      half_normal_check = .true.

      return
      end
      subroutine half_normal_mean ( a, b, mean )

c*********************************************************************72
c
cc HALF_NORMAL_MEAN returns the mean of the Half Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      mean = a + b * sqrt ( 2.0D+00 / pi )

      return
      end
      subroutine half_normal_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc HALF_NORMAL_PDF evaluates the Half Normal PDF.
c
c  Discussion:
c
c    PDF(A,B;X) =
c      sqrt ( 2 / PI ) * ( 1 / B ) * exp ( - 0.5D+00 * ( ( X - A ) / B )^2 )
c
c    for A .le. X
c
c    The Half Normal PDF is a special case of both the Chi PDF and the
c    Folded Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      if ( x .le. a ) then

        pdf = 0.0D+00

      else

        y = ( x - a ) / b

        pdf = sqrt ( 2.0D+00 / pi ) * ( 1.0D+00 / b ) * exp ( - 0.5D+00 
     &* y * y )

      end if

      return
      end
      subroutine half_normal_sample ( a, b, seed, x )

c*********************************************************************72
c
cc HALF_NORMAL_SAMPLE samples the Half Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call half_normal_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine half_normal_variance ( a, b, variance )

c*********************************************************************72
c
cc HALF_NORMAL_VARIANCE returns the variance of the Half Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = b * b * ( 1.0D+00 - 2.0D+00 / pi )

      return
      end
      subroutine hypergeometric_cdf ( x, n, m, l, cdf )

c*********************************************************************72
c
cc HYPERGEOMETRIC_CDF evaluates the Hypergeometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the CDF.
c
c    Input, integer N, the number of balls selected.
c    0 .le. N .le. L.
c
c    Input, integer M, the number of white balls.
c    0 .le. M .le. L.
c
c    Input, integer L, the number of balls to select from.
c    0 .le. L.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      double precision c1_log
      double precision c2_log
      integer l
      integer m
      integer n
      double precision pdf
      integer x
      integer x2

      call binomial_coef_log ( l - m, n, c1_log )
      call binomial_coef_log ( l, n, c2_log )

      pdf = exp ( c1_log - c2_log )
      cdf = pdf

      do x2 = 0, x - 1

        pdf = pdf * dble ( ( m - x2 ) * ( n - x2 ) )
     &    / dble ( ( x2 + 1 ) * ( l - m - n + x2 + 1 ) )

        cdf = cdf + pdf

      end do

      return
      end
      subroutine hypergeometric_cdf_values ( n_data, sam, suc, pop,
     &  n, fx )

c*********************************************************************72
c
cc HYPERGEOMETRIC_CDF_VALUES returns some values of the hypergeometric CDF.
c
c  Discussion:
c
c    CDF(X)(A,B) is the probability of at most X successes in A trials,
c    given that the probability of success on a single trial is B.
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`DiscreteDistributions`]
c      dist = HypergeometricDistribution [ sam, suc, pop ]
c      CDF [ dist, n ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996,
c    ISBN: 0-8493-2479-3,
c    LC: QA47.M315.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer SAM, integer SUC, integer POP, the sample size,
c    success size, and population parameters of the function.
c
c    Output, integer N, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      integer pop
      integer pop_vec(n_max)
      integer sam
      integer sam_vec(n_max)
      integer suc
      integer suc_vec(n_max)

      save fx_vec
      save n_vec
      save pop_vec
      save sam_vec
      save suc_vec

      data fx_vec /
     &  0.6001858177500578D-01,
     &  0.2615284665839845D+00,
     &  0.6695237889132748D+00,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.5332595856827856D+00,
     &  0.1819495964117640D+00,
     &  0.4448047017527730D-01,
     &  0.9999991751316731D+00,
     &  0.9926860896560750D+00,
     &  0.8410799901444538D+00,
     &  0.3459800113391901D+00,
     &  0.0000000000000000D+00,
     &  0.2088888139634505D-02,
     &  0.3876752992448843D+00,
     &  0.9135215248834896D+00 /
      data n_vec /
     &   7,  8,  9, 10,
     &   6,  6,  6,  6,
     &   6,  6,  6,  6,
     &   0,  0,  0,  0 /
      data pop_vec /
     &  100, 100, 100, 100,
     &  100, 100, 100, 100,
     &  100, 100, 100, 100,
     &  90,  200, 1000, 10000 /
      data sam_vec /
     &  10, 10, 10, 10,
     &   6,  7,  8,  9,
     &  10, 10, 10, 10,
     &  10, 10, 10, 10 /
      data suc_vec /
     &  90, 90, 90, 90,
     &  90, 90, 90, 90,
     &  10, 30, 50, 70,
     &  90, 90, 90, 90 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        sam = 0
        suc = 0
        pop = 0
        n = 0
        fx = 0.0D+00
      else
        sam = sam_vec(n_data)
        suc = suc_vec(n_data)
        pop = pop_vec(n_data)
        n = n_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function hypergeometric_check ( n, m, l )

c*********************************************************************72
c
cc HYPERGEOMETRIC_CHECK checks the parameters of the Hypergeometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of balls selected.
c    0 .le. N .le. L.
c
c    Input, integer M, the number of white balls in the population.
c    0 .le. M .le. L.
c
c    Input, integer L, the number of balls to select from.
c    0 .le. L.
c
c    Output, logical HYPERGEOMETRIC_CHECK, is true if the parameters are legal.
c
      implicit none

      logical hypergeometric_check
      integer l
      integer m
      integer n

      if ( n .lt. 0 .or. l .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERGEOMETRIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  Input N is out of range.'
        hypergeometric_check = .false.
        return
      end if

      if ( m .lt. 0 .or. l .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERGEOMETRIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  Input M is out of range.'
        hypergeometric_check = .false.
        return
      end if

      if ( l .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERGEOMETRIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  Input L is out of range.'
        hypergeometric_check = .false.
        return
      end if

      hypergeometric_check = .true.

      return
      end
      subroutine hypergeometric_mean ( n, m, l, mean )

c*********************************************************************72
c
cc HYPERGEOMETRIC_MEAN returns the mean of the Hypergeometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of balls selected.
c    0 .le. N .le. L.
c
c    Input, integer M, the number of white balls in the population.
c    0 .le. M .le. L.
c
c    Input, integer L, the number of balls to select from.
c    0 .le. L.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer l
      integer m
      double precision mean
      integer n

      mean = dble ( n * m ) / dble ( l )

      return
      end
      subroutine hypergeometric_pdf ( x, n, m, l, pdf )

c*********************************************************************72
c
cc HYPERGEOMETRIC_PDF evaluates the Hypergeometric PDF.
c
c  Discussion:
c
c    PDF(N,M,L;X) = C(M,X) * C(L-M,N-X) / C(L,N).
c
c    PDF(N,M,L;X) is the probability of drawing X white balls in a
c    single random sample of size N from a population containing
c    M white balls and a total of L balls.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the desired number of white balls.
c    0 .le. X .le. N, usually, although any value of X can be given.
c
c    Input, integer N, the number of balls selected.
c    0 .le. N .le. L.
c
c    Input, integer M, the number of white balls.
c    0 .le. M .le. L.
c
c    Input, integer L, the number of balls to select from.
c    0 .le. L.
c
c    Output, double precision PDF, the probability of exactly K white balls.
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      integer l
      integer m
      integer n
      double precision pdf
      double precision pdf_log
      integer x
c
c  Special cases.
c
      if ( x .lt. 0 ) then

        pdf = 1.0D+00

      else if ( n .lt. x ) then

        pdf = 0.0D+00

      else if ( m .lt. x ) then

        pdf = 0.0D+00

      else if ( l .lt. x ) then

        pdf = 0.0D+00

      else if ( n .eq. 0 ) then

        if ( x .eq. 0 ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if

      else

        call binomial_coef_log ( m, x, c1 )
        call binomial_coef_log ( l-m, n-x, c2 )
        call binomial_coef_log ( l, n, c3 )

        pdf_log = c1 + c2 - c3

        pdf = exp ( pdf_log )

      end if

      return
      end
      subroutine hypergeometric_sample ( n, m, l, seed, x )

c*********************************************************************72
c
cc HYPERGEOMETRIC_SAMPLE samples the Hypergeometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jerry Banks, editor,
c    Handbook of Simulation,
c    Engineering and Management Press Books, 1998, page 165.
c
c  Parameters:
c
c    Input, integer N, the number of balls selected.
c    0 .le. N .le. L.
c
c    Input, integer M, the number of white balls.
c    0 .le. M .le. L.
c
c    Input, integer L, the number of balls to select from.
c    0 .le. L.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c1_log
      double precision c2_log
      double precision r8_uniform_01
      integer l
      integer m
      integer n
      integer seed
      double precision u
      integer x

      call binomial_coef_log ( l - m, n, c1_log )
      call binomial_coef_log ( l, n, c2_log )

      a = exp ( c1_log - c2_log )
      b = a

      u = r8_uniform_01 ( seed )

      x = 0

10    continue

      if ( a .lt. u ) then

        b = b * dble ( ( m - x ) * ( n - x ) )
     &    / dble ( ( x + 1 ) * ( l - m - n + x + 1 ) )

        a = a + b

        x = x + 1

        go to 10

      end if

      return
      end
      subroutine hypergeometric_variance ( n, m, l, variance )

c*********************************************************************72
c
cc HYPERGEOMETRIC_VARIANCE returns the variance of the Hypergeometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of balls selected.
c    0 .le. N .le. L.
c
c    Input, integer M, the number of white balls.
c    0 .le. M .le. L.
c
c    Input, integer L, the number of balls to select from.
c    0 .le. L.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer l
      integer m
      integer n
      double precision variance

      variance = dble ( n * m * ( l - m ) * ( l - n ) )
     &  / dble ( l * l * ( l - 1 ) )

      return
      end
      function i4_factorial ( n )

c*********************************************************************72
c
cc I4_FACTORIAL returns N!.
c
c  Discussion:
c
c    N! = Product ( 1 .le. I .le. N ) I
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the function.
c    0 .le. N.
c
c    Output, double precision I4_FACTORIAL, the factorial of N.
c
      implicit none

      double precision i4_factorial
      integer i
      integer n

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_FACTORIAL - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop
      end if

      i4_factorial = 1.0D+00

      do i = 2, n
        i4_factorial = i4_factorial * dble ( i )
      end do

      return
      end
      function i4_huge ( )

c*********************************************************************72
c
cc I4_HUGE returns a "huge" I4.
c
c  Discussion:
c
c    On an IEEE 32 bit machine, I4_HUGE should be 2^31 - 1, and its
c    bit pattern should be
c
c     01111111111111111111111111111111
c
c    In this case, its numerical value is 2147483647.
c
c    Using the Dec/Compaq/HP Alpha FORTRAN compiler FORT, I could
c    use I4_HUGE() and HUGE interchangeably.
c
c    However, when using the G95, the values returned by HUGE were
c    not equal to 2147483647, apparently, and were causing severe
c    and obscure errors in my random number generator, which needs to
c    add I4_HUGE to the seed whenever the seed is negative.  So I
c    am backing away from invoking HUGE, whereas I4_HUGE is under
c    my control.
c
c    Explanation: because under G95 the default integer type is 64 bits!
c    So HUGE ( 1 ) = a very very huge integer indeed, whereas
c    I4_HUGE ( ) = the same old 32 bit big value.
c
c    An I4 is an integer value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer I4_HUGE, a "huge" I4.
c
      implicit none

      integer i4
      integer i4_huge

      i4_huge = 2147483647

      return
      end
      function i4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM_AB returns a scaled pseudorandom I4.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number will be scaled to be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2006
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
c    Peter Lewis, Allen Goodman, James Miller
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM_AB, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      integer i4_uniform_ab
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge ( )
      end if

      r = real ( seed ) * 4.656612875E-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0E+00 - r ) * ( real ( min ( a, b ) ) - 0.5E+00 )     
     &  +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform_ab = value

      return
      end
      subroutine i4row_max ( m, n, a, amax )

c*********************************************************************72
c
cc I4ROW_MAX returns the maximums of the rows of an I4ROW.
c
c  Discussion:
c
c    An I4ROW is an M by N array of I4 values, regarded
c    as an array of M rows of length N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), the array to be examined.
c
c    Output, integer AMAX(M), the maximums of the rows
c    of the array.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer amax(m)
      integer i
      integer j

      do i = 1, m

        amax(i) = a(i,1)
        do j = 2, n
          if ( amax(i) .lt. a(i,j) ) then
            amax(i) = a(i,j)
          end if
        end do

      end do

      return
      end
      subroutine i4row_mean ( m, n, a, mean )

c*********************************************************************72
c
cc I4ROW_MEAN returns the means of the rows of an I4ROW.
c
c  Discussion:
c
c    An I4ROW is an M by N array of I4 values, regarded
c    as an array of M rows of length N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an array of data.
c
c    Output, double precision MEAN(M), the mean of each row.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer j
      double precision mean(m)

      do i = 1, m
        mean(i) = 0.0D+00
        do j = 1, n
          mean(i) = mean(i) + dble ( a(i,j) )
        end do
        mean(i) = mean(i) / dble ( n )
      end do

      return
      end
      subroutine i4row_min ( m, n, a, amin )

c*********************************************************************72
c
cc I4ROW_MIN returns the minimums of the rows of an I4ROW.
c
c  Discussion:
c
c    An I4ROW is an M by N array of I4 values, regarded
c    as an array of M rows of length N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), the array to be examined.
c
c    Output, integer AMIN(M), the minimums of the rows.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer amin(m)
      integer i
      integer j

      do i = 1, m

        amin(i) = a(i,1)
        do j = 2, n
          if ( a(i,j) .lt. amin(i) ) then
            amin(i) = a(i,j)
          end if
        end do

      end do

      return
      end
      subroutine i4row_variance ( m, n, a, variance )

c*********************************************************************72
c
cc I4ROW_VARIANCE returns the variances of the rows of an I4ROW.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of data.
c
c    Input, integer A(M,N), the array.
c
c    Output, double precision VARIANCE(M), the variance of each row.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer j
      double precision mean
      integer t
      double precision variance(m)

      do i = 1, m

        t = 0
        do j = 1, n
          t = t + a(i,j)
        end do

        mean = dble ( t ) / dble ( n )

        variance(i) = 0.0D+00
        do j = 1, n
          variance(i) = variance(i) + ( dble ( a(i,j) ) - mean )**2
        end do

        if ( 1 .lt. n ) then
          variance(i) = variance(i) / dble ( n - 1 )
        else
          variance(i) = 0.0D+00
        end if

      end do

      return
      end
      subroutine i4vec_max ( n, a, amax )

c*********************************************************************72
c
cc I4VEC_MAX computes the maximum element of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer AMAX, the value of the largest entry.
c
      implicit none

      integer n

      integer a(n)
      integer amax
      integer i

      amax = a(1)

      do i = 2, n
        amax = max ( amax, a(i) )
      end do

      return
      end
      subroutine i4vec_mean ( n, x, mean )

c*********************************************************************72
c
cc I4VEC_MEAN returns the mean of an I4VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, integer X(N), the vector whose mean is desired.
c
c    Output, double precision MEAN, the mean, or average, of
c    the vector entries.
c
      implicit none

      integer n

      integer i4vec_sum
      double precision mean
      integer x(n)

      mean = dble ( i4vec_sum ( n, x ) ) / dble ( n )

      return
      end
      subroutine i4vec_min ( n, a, amin )

c*********************************************************************72
c
cc I4VEC_MIN computes the minimum element of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer AMIN, the value of the smallest entry.
c
      implicit none

      integer n

      integer a(n)
      integer amin
      integer i

      amin = a(1)

      do i = 2, n
        amin = min ( amin, a(i) )
      end do

      return
      end
      subroutine i4vec_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_PRINT prints an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i12)' ) i, ':', a(i)
      end do

      return
      end
      subroutine i4vec_run_count ( n, a, run_count )

c*********************************************************************72
c
cc I4VEC_RUN_COUNT counts runs of equal values in an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c    A run is a sequence of equal values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, integer A(N), the vector to be examined.
c
c    Output, integer RUN_COUNT, the number of runs.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer run_count
      integer test

      run_count = 0

      if ( n .lt. 1 ) then
        return
      end if

      test = 0

      do i = 1, n

        if ( i .eq. 1 .or. a(i) .ne. test ) then
          run_count = run_count + 1
          test = a(i)
        end if

      end do

      return
      end
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
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
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

      return
      end
      subroutine i4vec_variance ( n, x, variance )

c*********************************************************************72
c
cc I4VEC_VARIANCE returns the variance of an I4VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 May 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, integer X(N), the vector whose variance is desired.
c
c    Output, double precision VARIANCE, the variance of the vector entries.
c
      implicit none

      integer n

      integer i
      double precision mean
      double precision variance
      integer x(n)

      call i4vec_mean ( n, x, mean )

      variance = 0.0D+00
      do i = 1, n
        variance = variance + ( dble ( x(i) ) - mean ) ** 2
      end do

      if ( 1 .lt. n ) then
        variance = variance / dble ( n - 1 )
      else
        variance = 0.0D+00
      end if

      return
      end
      subroutine inverse_gaussian_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc INVERSE_GAUSSIAN_CDF evaluates the Inverse Gaussian CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    0.0D+00 .lt. X.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision x
      double precision x1
      double precision x2

      if ( x .le. 0.0D+00 ) then

        cdf = 0.0D+00

      else

        x1 = sqrt ( b / x ) * ( x - a ) / a
        call normal_01_cdf ( x1, cdf1 )

        x2 = - sqrt ( b / x ) * ( x + a ) / a
        call normal_01_cdf ( x2, cdf2 )

        cdf = cdf1 + exp ( 2.0D+00 * b / a ) * cdf2

      end if

      return
      end
      function inverse_gaussian_check ( a, b )

c*********************************************************************72
c
cc INVERSE_GAUSSIAN_CHECK checks the parameters of the Inverse Gaussian CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, logical INVERSE_GAUSSIAN_CHECK, is true if the parameters
c    are legal.
c
      implicit none

      double precision a
      double precision b
      logical inverse_gaussian_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'INVERSE_GAUSSIAN_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        inverse_gaussian_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'INVERSE_GAUSSIAN_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        inverse_gaussian_check = .false.
        return
      end if

      inverse_gaussian_check = .true.

      return
      end
      subroutine inverse_gaussian_mean ( a, b, mean )

c*********************************************************************72
c
cc INVERSE_GAUSSIAN_MEAN returns the mean of the Inverse Gaussian PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine inverse_gaussian_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc INVERSE_GAUSSIAN_PDF evaluates the Inverse Gaussian PDF.
c
c  Discussion:
c
c    The Inverse Gaussian PDF is also known as the Wald PDF
c    and the Inverse Normal PDF.
c
c    PDF(A,B;X)
c      = sqrt ( B / ( 2 * PI * X^3 ) )
c        * exp ( - B * ( X - A )^2 / ( 2.0D+00 * A^2 * X ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .lt. X
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .le. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = sqrt ( b / ( 2.0D+00 * pi * x**3 ) ) *       exp ( - b * (
     & x - a )**2 / ( 2.0D+00 * a * a * x ) )
      end if

      return
      end
      subroutine inverse_gaussian_sample ( a, b, seed, x )

c*********************************************************************72
c
cc INVERSE_GAUSSIAN_SAMPLE samples the Inverse Gaussian PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision r8_uniform_01
      double precision phi
      integer seed
      double precision t
      double precision u
      double precision x
      double precision y
      double precision z

      phi = b / a
      call normal_01_sample ( seed, z )
      y = z * z

      t = 1.0D+00 + 0.5D+00 * ( y - sqrt ( 4.0D+00 * phi * y + y * y ) )
     & / phi
      u = r8_uniform_01 ( seed )

      if ( u * ( 1.0D+00 + t ) .le. 1.0D+00 ) then
        x = a * t
      else
        x = a / t
      end if

      return
      end
      subroutine inverse_gaussian_variance ( a, b, variance )

c*********************************************************************72
c
cc INVERSE_GAUSSIAN_VARIANCE returns the variance of the Inverse Gaussian PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = a**3 / b

      return
      end
      subroutine laplace_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc LAPLACE_CDF evaluates the Laplace CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x
      double precision y

      y = ( x - a ) / b

      if ( x .le. a ) then
        cdf = 0.5D+00 * exp ( y )
      else
        cdf = 1.0D+00 - 0.5D+00 * exp ( - y )
      end if

      return
      end
      subroutine laplace_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc LAPLACE_CDF_INV inverts the Laplace CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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
        write ( *, '(a)' ) 'LAPLACE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .le. 0.5D+00 ) then
        x = a + b * log ( 2.0D+00 * cdf )
      else
        x = a - b * log ( 2.0D+00 * ( 1.0D+00 - cdf ) )
      end if

      return
      end
      subroutine laplace_cdf_values ( n_data, mu, beta, x, fx )

c*********************************************************************72
c
cc LAPLACE_CDF_VALUES returns some values of the Laplace CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = LaplaceDistribution [ mu, beta ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision MU, the mean of the distribution.
c
c    Output, double precision BETA, the shape parameter.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision beta
      double precision beta_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      double precision mu
      double precision mu_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save beta_vec
      save fx_vec
      save mu_vec
      save x_vec

      data beta_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.8160602794142788D+00,
     &  0.9323323583816937D+00,
     &  0.9751064658160680D+00,
     &  0.6967346701436833D+00,
     &  0.6417343447131054D+00,
     &  0.6105996084642976D+00,
     &  0.5906346234610091D+00,
     &  0.5000000000000000D+00,
     &  0.3032653298563167D+00,
     &  0.1839397205857212D+00,
     &  0.1115650800742149D+00 /
      data mu_vec /
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.0000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01 /
      data x_vec /
     &  0.0000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        mu = 0.0D+00
        beta = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        mu = mu_vec(n_data)
        beta = beta_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function laplace_check ( a, b )

c*********************************************************************72
c
cc LAPLACE_CHECK checks the parameters of the Laplace PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical LAPLACE_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical laplace_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LAPLACE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        laplace_check = .false.
        return
      end if

      laplace_check = .true.

      return
      end
      subroutine laplace_mean ( a, b, mean )

c*********************************************************************72
c
cc LAPLACE_MEAN returns the mean of the Laplace PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine laplace_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc LAPLACE_PDF evaluates the Laplace PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = exp ( - abs ( X - A ) / B ) / ( 2 * B )
c
c    The Laplace PDF is also known as the Double Exponential PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      pdf = exp ( - abs ( x - a ) / b ) / ( 2.0D+00 * b )

      return
      end
      subroutine laplace_sample ( a, b, seed, x )

c*********************************************************************72
c
cc LAPLACE_SAMPLE samples the Laplace PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call laplace_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine laplace_variance ( a, b, variance )

c*********************************************************************72
c
cc LAPLACE_VARIANCE returns the variance of the Laplace PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = 2.0D+00 * b * b

      return
      end
      function lerch ( a, b, c )

c*********************************************************************72
c
cc LERCH estimates the Lerch transcendent function.
c
c  Discussion:
c
c    The Lerch transcendent function is defined as:
c
c      LERCH ( A, B, C ) = Sum ( 0 .le. K .lt. Infinity ) A^K / ( C + K )^B
c
c    excluding any term with ( C + K ) = 0.
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
c    Eric Weisstein, editor,
c    CRC Concise Encylopedia of Mathematics,
c    CRC Press, 1998.
c
c  Thanks:
c
c    Oscar van Vlijmen
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the function.
c
c    Output, double precision LERCH, an approximation to the Lerch
c    transcendent function.
c
      implicit none

      double precision a
      double precision a_k
      double precision b
      double precision c
      integer k
      double precision lerch
      double precision sum2
      double precision sum2_old

      sum2 = 0.0D+00
      k = 0
      a_k = 1.0D+00

10    continue

        sum2_old = sum2

        if ( c + dble ( k ) .eq. 0.0D+00 ) then
          k = k + 1
          a_k = a_k * a
          go to 10
        end if

        sum2 = sum2 + a_k / ( c + dble ( k ) )**b

        if ( sum2 .le. sum2_old ) then
          go to 20
        end if

        k = k + 1
        a_k = a_k * a

      go to 10

20    continue

      lerch = sum2

      return
      end
      subroutine levy_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc LEVY_CDF evaluates the Levy CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 April 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    Normally, A .le. X.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0 .lt. B.
c
c    Output, double precision CDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision error_f
      double precision x

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEVY_CDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter B .le. 0.0'
        stop
      end if

      if ( x .le. a ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - error_f ( sqrt ( b / ( 2.0D+00 * ( x - a ) ) ) )
      end if

      return
      end
      subroutine levy_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc LEVY_CDF_INV inverts the Levy CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0 .lt. B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision x
      double precision x1

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEVY_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEVY_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  Input parameter B .le. 0.0'
        stop
      end if

      cdf1 = 1.0D+00 - 0.5D+00 * cdf
      call normal_01_cdf_inv ( cdf1, x1 )
      x = a + b / ( x1 * x1 )

      return
      end
      subroutine levy_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc LEVY_PDF evaluates the Levy PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = sqrt ( B / ( 2 * PI ) )
c               * exp ( - B / ( 2 * ( X - A ) )
c               / ( X - A )^(3/2)
c
c    for A .le. X.
c
c    Note that the Levy PDF does not have a finite mean or variance.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    Normally, A .le. X.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEVY_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter B .le. 0.0'
        stop
      end if

      if ( x .le. a ) then
        pdf = 0.0D+00
      else
        pdf = sqrt ( b / ( 2.0D+00 * pi ) )         * exp ( - b / ( 2.0D
     &+00 * ( x - a ) ) )         / sqrt ( ( x - a )**3 )
      end if

      return
      end
      subroutine levy_sample ( a, b, seed, x )

c*********************************************************************72
c
cc LEVY_SAMPLE samples the Levy PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call levy_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine logistic_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc LOGISTIC_CDF evaluates the Logistic CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      cdf = 1.0D+00 / ( 1.0D+00 + exp ( ( a - x ) / b ) )

      return
      end
      subroutine logistic_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc LOGISTIC_CDF_INV inverts the Logistic CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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
        write ( *, '(a)' ) 'LOGISTIC_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a - b * log ( ( 1.0D+00 - cdf ) / cdf )

      return
      end
      subroutine logistic_cdf_values ( n_data, mu, beta, x, fx )

c*********************************************************************72
c
cc LOGISTIC_CDF_VALUES returns some values of the Logistic CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = LogisticDistribution [ mu, beta ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision MU, the mean of the distribution.
c
c    Output, double precision BETA, the shape parameter of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision beta
      double precision beta_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      double precision mu
      double precision mu_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save beta_vec
      save fx_vec
      save mu_vec
      save x_vec

      data beta_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.8807970779778824D+00,
     &  0.9820137900379084D+00,
     &  0.9975273768433652D+00,
     &  0.6224593312018546D+00,
     &  0.5825702064623147D+00,
     &  0.5621765008857981D+00,
     &  0.5498339973124779D+00,
     &  0.6224593312018546D+00,
     &  0.5000000000000000D+00,
     &  0.3775406687981454D+00,
     &  0.2689414213699951D+00 /
      data mu_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        mu = 0.0D+00
        beta = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        mu = mu_vec(n_data)
        beta = beta_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function logistic_check ( a, b )

c*********************************************************************72
c
cc LOGISTIC_CHECK checks the parameters of the Logistic CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical LOGISTIC_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical logistic_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOGISTIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        logistic_check = .false.
        return
      end if

      logistic_check = .true.

      return
      end
      subroutine logistic_mean ( a, b, mean )

c*********************************************************************72
c
cc LOGISTIC_MEAN returns the mean of the Logistic PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine logistic_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc LOGISTIC_PDF evaluates the Logistic PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = exp ( ( A - X ) / B ) /
c      ( B * ( 1 + exp ( ( A - X ) / B ) )^2 )
c
c    The Logistic PDF is also known as the Sech-Squared PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision temp
      double precision x

      temp = exp ( ( a - x ) / b )

      pdf = temp / ( b * ( 1.0D+00 + temp )**2 )

      return
      end
      subroutine logistic_sample ( a, b, seed, x )

c*********************************************************************72
c
cc LOGISTIC_SAMPLE samples the Logistic PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call logistic_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine logistic_variance ( a, b, variance )

c*********************************************************************72
c
cc LOGISTIC_VARIANCE returns the variance of the Logistic PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = pi * pi * b * b / 3.0D+00

      return
      end
      subroutine log_normal_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc LOG_NORMAL_CDF evaluates the Lognormal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .lt. X.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision logx
      double precision x

      if ( x .le. 0.0D+00 ) then

        cdf = 0.0D+00

      else

        logx = log ( x )

        call normal_cdf ( logx, a, b, cdf )

      end if

      return
      end
      subroutine log_normal_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc LOG_NORMAL_CDF_INV inverts the Lognormal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Input, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision logx
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOG_NORMAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      call normal_cdf_inv ( cdf, a, b, logx )

      x = exp ( logx )

      return
      end
      subroutine log_normal_cdf_values ( n_data, mu, sigma, x, fx )

c*********************************************************************72
c
cc LOG_NORMAL_CDF_VALUES returns some values of the Log Normal CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = LogNormalDistribution [ mu, sigma ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision MU, the mean of the distribution.
c
c    Output, double precision SIGMA, the shape parameter of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx_vec(n_max)
      double precision mu
      double precision mu_vec(n_max)
      integer n_data
      double precision sigma
      double precision sigma_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save mu_vec
      save sigma_vec
      save x_vec

      data fx_vec /
     &  0.2275013194817921D-01,
     &  0.2697049307349095D+00,
     &  0.5781741008028732D+00,
     &  0.7801170895122241D+00,
     &  0.4390310097476894D+00,
     &  0.4592655190218048D+00,
     &  0.4694258497695908D+00,
     &  0.4755320473858733D+00,
     &  0.3261051056816658D+00,
     &  0.1708799040927608D+00,
     &  0.7343256357952060D-01,
     &  0.2554673736161761D-01 /
      data mu_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data sigma_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        mu = 0.0D+00
        sigma = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        mu = mu_vec(n_data)
        sigma = sigma_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function log_normal_check ( a, b )

c*********************************************************************72
c
cc LOG_NORMAL_CHECK checks the parameters of the Lognormal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical LOG_NORMAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical log_normal_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOG_NORMAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        log_normal_check = .false.
        return
      end if

      log_normal_check = .true.

      return
      end
      subroutine log_normal_mean ( a, b, mean )

c*********************************************************************72
c
cc LOG_NORMAL_MEAN returns the mean of the Lognormal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = exp ( a + 0.5D+00 * b * b )

      return
      end
      subroutine log_normal_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc LOG_NORMAL_PDF evaluates the Lognormal PDF.
c
c  Discussion:
c
c    PDF(A,B;X)
c      = exp ( - 0.5 * ( ( log ( X ) - A ) / B )^2 )
c        / ( B * X * sqrt ( 2 * PI ) )
c
c    The Lognormal PDF is also known as the Cobb-Douglas PDF,
c    and as the Antilog_normal PDF.
c
c    The Lognormal PDF describes a variable X whose logarithm
c    is normally distributed.
c
c    The special case A = 0, B = 1 is known as Gilbrat's PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .lt. X
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .le. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = exp ( - 0.5D+00 * ( ( log ( x ) - a ) / b )**2 )       / (
     & b * x * sqrt ( 2.0D+00 * pi ) )
      end if

      return
      end
      subroutine log_normal_sample ( a, b, seed, x )

c*********************************************************************72
c
cc LOG_NORMAL_SAMPLE samples the Lognormal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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

      call log_normal_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine log_normal_variance ( a, b, variance )

c*********************************************************************72
c
cc LOG_NORMAL_VARIANCE returns the variance of the Lognormal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = exp ( 2.0D+00 * a + b * b ) * ( exp ( b * b ) - 1.0D+00
     & )

      return
      end
      subroutine log_series_cdf ( x, a, cdf )

c*********************************************************************72
c
cc LOG_SERIES_CDF evaluates the Logarithmic Series CDF.
c
c  Discussion:
c
c    Simple summation is used, with a recursion to generate successive
c    values of the PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Thanks:
c
c    Oscar van Vlijmen
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c    0 .lt. X
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision pdf
      integer x
      integer x2

      cdf = 0.0D+00

      do x2 = 1, x

        if ( x2 .eq. 1 ) then
          pdf = - a / log ( 1.0D+00 - a )
        else
          pdf = dble ( x2 - 1 ) * a * pdf / dble ( x2 )
        end if

        cdf = cdf + pdf

      end do

      return
      end
      subroutine log_series_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc LOG_SERIES_CDF_INV inverts the Logarithmic Series CDF.
c
c  Discussion:
c
c    Simple summation is used.  The only protection against an
c    infinite loop caused by roundoff is that X cannot be larger
c    than 1000.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Output, double precision X, the argument of the CDF for which
c    CDF(X-1) .le. CDF .le. CDF(X).
c
      implicit none

      double precision a
      double precision cdf
      double precision cdf2
      double precision pdf
      integer x
      integer xmax
      parameter ( xmax = 1000 )

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOG_SERIES_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      cdf2 = 0.0D+00
      x = 1

10    continue

      if ( cdf2 .lt. cdf .and. x .lt. xmax ) then

        if ( x .eq. 1 ) then
          pdf = - a / log ( 1.0D+00 - a )
        else
          pdf = dble ( x - 1 ) * a * pdf / dble ( x )
        end if

        cdf2 = cdf2 + pdf

        x = x + 1

        go to 10

      end if

      return
      end
      subroutine log_series_cdf_values ( n_data, t, n, fx )

c*********************************************************************72
c
cc LOG_SERIES_CDF_VALUES returns some values of the log series CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`DiscreteDistributions`]
c      dist = LogSeriesDistribution [ t ]
c      CDF [ dist, n ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision T, the parameter of the function.
c
c    Output, integer N, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 29 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision t
      double precision t_vec(n_max)

      save fx_vec
      save n_vec
      save t_vec

      data fx_vec /
     &  0.9491221581029903D+00,
     &  0.9433541128559735D+00,
     &  0.9361094611773272D+00,
     &  0.9267370278044118D+00,
     &  0.9141358246245129D+00,
     &  0.8962840235449100D+00,
     &  0.8690148741955517D+00,
     &  0.8221011541254772D+00,
     &  0.7213475204444817D+00,
     &  0.6068261510845583D+00,
     &  0.5410106403333613D+00,
     &  0.4970679476476894D+00,
     &  0.4650921887927060D+00,
     &  0.4404842934597863D+00,
     &  0.4207860535926143D+00,
     &  0.4045507673897055D+00,
     &  0.3908650337129266D+00,
     &  0.2149757685421097D+00,
     &  0.0000000000000000D+00,
     &  0.2149757685421097D+00,
     &  0.3213887739704539D+00,
     &  0.3916213575531612D+00,
     &  0.4437690508633213D+00,
     &  0.4850700239649681D+00,
     &  0.5191433267738267D+00,
     &  0.5480569580144867D+00,
     &  0.5731033910767085D+00,
     &  0.5951442521714636D+00,
     &  0.6147826594068904D+00 /
      data n_vec /
     &   1, 1, 1, 1, 1,
     &   1, 1, 1, 1, 1,
     &   1, 1, 1, 1, 1,
     &   1, 1, 1, 0, 1,
     &   2, 3, 4, 5, 6,
     &   7, 8, 9, 10 /
      data t_vec /
     &  0.1000000000000000D+00,
     &  0.1111111111111111D+00,
     &  0.1250000000000000D+00,
     &  0.1428571428571429D+00,
     &  0.1666666666666667D+00,
     &  0.2000000000000000D+00,
     &  0.2500000000000000D+00,
     &  0.3333333333333333D+00,
     &  0.5000000000000000D+00,
     &  0.6666666666666667D+00,
     &  0.7500000000000000D+00,
     &  0.8000000000000000D+00,
     &  0.8333333333333333D+00,
     &  0.8571485714857149D+00,
     &  0.8750000000000000D+00,
     &  0.8888888888888889D+00,
     &  0.9000000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00,
     &  0.9900000000000000D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        t = 0.0D+00
        n = 0
        fx = 0.0D+00
      else
        t = t_vec(n_data)
        n = n_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function log_series_check ( a )

c*********************************************************************72
c
cc LOG_SERIES_CHECK checks the parameter of the Logarithmic Series PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Output, logical LOG_SERIES_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical log_series_check

      if ( a .le. 0.0D+00 .or. 1.0D+00 .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOG_SERIES_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.0D+00 or 1.0D+00 .le. A'
        log_series_check = .false.
        return
      end if

      log_series_check = .true.

      return
      end
      subroutine log_series_mean ( a, mean )

c*********************************************************************72
c
cc LOG_SERIES_MEAN returns the mean of the Logarithmic Series PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision mean

      mean = - a / ( ( 1.0D+00 - a ) * log ( 1.0D+00 - a ) )

      return
      end
      subroutine log_series_pdf ( x, a, pdf )

c*********************************************************************72
c
cc LOG_SERIES_PDF evaluates the Logarithmic Series PDF.
c
c  Discussion:
c
c    PDF(A;X) = - A**X / ( X * log ( 1 - A ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c    0 .lt. X
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision pdf
      integer x

      if ( x .le. 0 ) then
        pdf = 0.0D+00
      else
        pdf = - a**x / ( dble ( x ) * log ( 1.0D+00 - a ) )
      end if

      return
      end
      subroutine log_series_sample ( a, seed, x )

c*********************************************************************72
c
cc LOG_SERIES_SAMPLE samples the Logarithmic Series PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Luc Devroye,
c    Non-Uniform Random Variate Generation,
c    Springer-Verlag, 1986, page 547.
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision r8_uniform_01
      integer seed
      double precision u
      double precision v
      integer x

      u = r8_uniform_01 ( seed )
      v = r8_uniform_01 ( seed )

      x = int ( 1.0D+00 + log ( v ) / ( log ( 1.0D+00 - ( 1.0D+00 - a )*
     &*u ) ) )

      return
      end
      subroutine log_series_variance ( a, variance )

c*********************************************************************72
c
cc LOG_SERIES_VARIANCE returns the variance of the Logarithmic Series PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A .lt. 1.0.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision variance

      alpha = - 1.0D+00 / log ( 1.0D+00 - a )

      variance = a * alpha * ( 1.0D+00 - alpha * a ) / ( 1.0D+00 - a )**
     &2

      return
      end
      subroutine log_uniform_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc LOG_UNIFORM_CDF evaluates the Log Uniform CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .le. a ) then
        cdf = 0.0D+00
      else if ( x .lt. b ) then
        cdf = ( log ( x ) - log ( a ) ) / ( log ( b ) - log ( a ) )
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine log_uniform_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc LOG_UNIFORM_CDF_INV inverts the Log Uniform CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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
        write ( *, '(a)' ) 'LOG_UNIFORM_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a * exp ( ( log ( b ) - log ( a ) ) * cdf )

      return
      end
      function log_uniform_check ( a, b )

c*********************************************************************72
c
cc LOG_UNIFORM_CHECK checks the parameters of the Log Uniform CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    1.0 .lt. A .lt. B.
c
c    Output, logical LOG_UNIFORM_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical log_uniform_check

      if ( a .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOG_UNIFORM_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 1.'
        log_uniform_check = .false.
        return
      end if

      if ( b .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LOG_UNIFORM_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. A.'
        log_uniform_check = .false.
        return
      end if

      log_uniform_check = .true.

      return
      end
      subroutine log_uniform_mean ( a, b, mean )

c*********************************************************************72
c
cc LOG_UNIFORM_MEAN returns the mean of the Log Uniform PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    1.0 .lt. A .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = ( b - a ) / ( log ( b ) - log ( a ) )

      return
      end
      subroutine log_uniform_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc LOG_UNIFORM_PDF evaluates the Log Uniform PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = 1 / ( X * ( log ( B ) - log ( A ) ) ) for A .le. X .le. B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    1.0 .lt. A .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .lt. a ) then
        pdf = 0.0D+00
      else if ( x .le. b ) then
        pdf = 1.0D+00 / ( x * ( log ( b ) - log ( a ) ) )
      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine log_uniform_sample ( a, b, seed, x )

c*********************************************************************72
c
cc LOG_UNIFORM_SAMPLE samples the Log Uniform PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    1.0 .lt. A .lt. B.
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

      call log_uniform_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine lorentz_cdf ( x, cdf )

c*********************************************************************72
c
cc LORENTZ_CDF evaluates the Lorentz CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      cdf = 0.5D+00 + atan ( x ) / pi

      return
      end
      subroutine lorentz_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc LORENTZ_CDF_INV inverts the Lorentz CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LORENTZ_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = tan ( pi * ( cdf - 0.5D+00 ) )

      return
      end
      subroutine lorentz_mean ( mean )

c*********************************************************************72
c
cc LORENTZ_MEAN returns the mean of the Lorentz PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision mean

      mean = 0.0D+00

      return
      end
      subroutine lorentz_pdf ( x, pdf )

c*********************************************************************72
c
cc LORENTZ_PDF evaluates the Lorentz PDF.
c
c  Discussion:
c
c    PDF(X) = 1 / ( PI * ( 1 + X^2 ) )
c
c    The chief interest of the Lorentz PDF is that it is easily
c    inverted, and can be used to dominate other PDF's in an
c    acceptance/rejection method.
c
c    LORENTZ_PDF(X) = CAUCHY_PDF(0,1;X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      pdf = 1.0D+00 / ( pi * ( 1.0D+00 + x * x ) )

      return
      end
      subroutine lorentz_sample ( seed, x )

c*********************************************************************72
c
cc LORENTZ_SAMPLE samples the Lorentz PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
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

      call lorentz_cdf_inv ( cdf, x )

      return
      end
      subroutine lorentz_variance ( variance )

c*********************************************************************72
c
cc LORENTZ_VARIANCE returns the variance of the Lorentz PDF.
c
c  Discussion:
c
c    The variance of the Lorentz PDF is not well defined.  This routine
c    is made available for completeness only, and simply returns
c    a "very large" number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE, the mean of the PDF.
c
      implicit none

      double precision r8_huge
      double precision variance

      variance = r8_huge ( )

      return
      end
      subroutine maxwell_cdf ( x, a, cdf )

c*********************************************************************72
c
cc MAXWELL_CDF evaluates the Maxwell CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision gamma_inc
      double precision p2
      double precision x
      double precision x2

      if ( x .le. 0.0D+00 ) then

        cdf = 0.0D+00

      else

        x2 = x / a
        p2 = 1.5D+00

        cdf = gamma_inc ( p2, x2 )

      end if

      return
      end
      subroutine maxwell_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc MAXWELL_CDF_INV inverts the Maxwell CDF.
c
c  Discussion:
c
c    A simple bisection method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision r8_huge
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MAXWELL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = 0.0D+00
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
        return
      end if

      x1 = 0.0D+00
      cdf1 = 0.0D+00

      x2 = 1.0D+00

10    continue

        call maxwell_cdf ( x2, a, cdf2 )

        if ( cdf .lt. cdf2 ) then
          go to 20
        end if

        x2 = 2.0D+00 * x2

        if ( 1000000.0D+00 .lt. x2 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MAXWELL_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Initial bracketing effort fails.'
          stop
        end if

      go to 10

20    continue
c
c  Now use bisection.
c
      it = 0

30    continue

        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call maxwell_cdf ( x3, a, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          go to 40
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MAXWELL_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          stop
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 30

40    continue

      return
      end
      function maxwell_check ( a )

c*********************************************************************72
c
cc MAXWELL_CHECK checks the parameters of the Maxwell CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, logical MAXWELL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical maxwell_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MAXWELL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.0.'
        maxwell_check = .false.
        return
      end if

      maxwell_check = .true.

      return
      end
      subroutine maxwell_mean ( a, mean )

c*********************************************************************72
c
cc MAXWELL_MEAN returns the mean of the Maxwell PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, double precision MEAN, the mean value.
c
      implicit none

      double precision a
      double precision mean
      double precision r8_gamma

      mean = sqrt ( 2.0D+00 ) * a * r8_gamma ( 2.0D+00 ) / r8_gamma ( 1.
     &5D+00 )

      return
      end
      subroutine maxwell_pdf ( x, a, pdf )

c*********************************************************************72
c
cc MAXWELL_PDF evaluates the Maxwell PDF.
c
c  Discussion:
c
c    PDF(A;X) = exp ( - 0.5D+00 * ( X / A )^2 ) * ( X / A )^2 /
c      ( sqrt ( 2 ) * A * GAMMA ( 1.5D+00 ) )
c
c    MAXWELL_PDF(A;X) = CHI_PDF(0,A,3;X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0 .lt. X
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision pdf
      double precision r8_gamma
      double precision x
      double precision y

      if ( x .le. 0.0D+00 ) then

        pdf = 0.0D+00

      else

        y = x / a

        pdf = exp ( - 0.5D+00 * y * y ) * y * y       / ( sqrt ( 2.0D+00
     & ) * a * r8_gamma ( 1.5D+00 ) )

      end if

      return
      end
      subroutine maxwell_sample ( a, seed, x )

c*********************************************************************72
c
cc MAXWELL_SAMPLE samples the Maxwell PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      integer seed
      double precision x

      a2 = 3.0D+00
      call chi_square_sample ( a2, seed, x )

      x = a * sqrt ( x )

      return
      end
      subroutine maxwell_variance ( a, variance )

c*********************************************************************72
c
cc MAXWELL_VARIANCE returns the variance of the Maxwell PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision r8_gamma
      double precision variance

      variance = a * a * ( 3.0D+00 - 2.0D+00     * ( r8_gamma ( 2.0D+00 
     &) / r8_gamma ( 1.5D+00 ) )**2 )

      return
      end
      function multicoef_check ( nfactor, factor )

c*********************************************************************72
c
cc MULTICOEF_CHECK checks the parameters of the multinomial coefficient.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NFACTOR, the number of factors.
c    1 .le. NFACTOR.
c
c    Input, integer FACTOR(NFACTOR), contains the factors.
c    0.0D+00 .le. FACTOR(I).
c
c    Output, logical MULTICOEF_CHECK, is true if the parameters are legal.
c
      implicit none

      integer nfactor

      integer factor(nfactor)
      integer i
      logical multicoef_check

      if ( nfactor .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTICOEF_CHECK - Fatal error!'
        write ( *, '(a)' ) '  NFACTOR .lt. 1.'
        multicoef_check = .false.
        return
      end if

      do i = 1, nfactor

        if ( factor(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MULTICOEF_CHECK - Fatal error'
          write ( *, '(a,i8)' ) '  Factor ', I
          write ( *, '(a,i8)' ) '  = ', factor(i)
          write ( *, '(a)' ) '  But this value must be nonnegative.'
          multicoef_check = .false.
          return
        end if

      end do

      multicoef_check = .true.

      return
      end
      subroutine multinomial_coef1 ( nfactor, factor, ncomb )

c*********************************************************************72
c
cc MULTINOMIAL_COEF1 computes a Multinomial coefficient.
c
c  Discussion:
c
c    The multinomial coefficient is a generalization of the binomial
c    coefficient.  It may be interpreted as the number of combinations of
c    N objects, where FACTOR(1) objects are indistinguishable of type 1,
c    ... and FACTOR(NFACTOR) are indistinguishable of type NFACTOR,
c    and N is the sum of FACTOR(1) through FACTOR(NFACTOR).
c
c    NCOMB = N! / ( FACTOR(1)! FACTOR(2)! ... FACTOR(NFACTOR)! )
c
c    The log of the gamma function is used, to avoid overflow.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NFACTOR, the number of factors.
c    1 .le. NFACTOR.
c
c    Input, integer FACTOR(NFACTOR), contains the factors.
c    0.0D+00 .le. FACTOR(I).
c
c    Output, integer NCOMB, the value of the multinomial
c    coefficient.
c
      implicit none

      integer nfactor

      logical check
      double precision facn
      integer factor(nfactor)
      double precision factorial_log
      integer i
      integer i4_huge
      integer i4vec_sum
      logical multicoef_check
      integer n
      integer ncomb

      check = multicoef_check ( nfactor, factor )

      if ( .not. check ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTINOMIAL_COEF1 - Fatal error!'
        write ( *, '(a)' ) '  MULTICOEF_CHECK failed.'
        ncomb = - i4_huge ( )
        return
      end if
c
c  The factors sum to N.
c
      n = i4vec_sum ( nfactor, factor )

      facn = factorial_log ( n )

      do i = 1, nfactor

        facn = facn - factorial_log ( factor(i) )

      end do

      ncomb = nint ( exp ( facn ) )

      return
      end
      subroutine multinomial_coef2 ( nfactor, factor, ncomb )

c*********************************************************************72
c
cc MULTINOMIAL_COEF2 computes a Multinomial coefficient.
c
c  Discussion:
c
c    The multinomial coefficient is a generalization of the binomial
c    coefficient.  It may be interpreted as the number of combinations of
c    N objects, where FACTOR(1) objects are indistinguishable of type 1,
c    ... and FACTOR(NFACTOR) are indistinguishable of type NFACTOR,
c    and N is the sum of FACTOR(1) through FACTOR(NFACTOR).
c
c    NCOMB = N! / ( FACTOR(1)! FACTOR(2)! ... FACTOR(NFACTOR)! )
c
c    A direct method is used, which should be exact.  However, there
c    is a possibility of intermediate overflow of the result.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NFACTOR, the number of factors.
c    1 .le. NFACTOR.
c
c    Input, integer FACTOR(NFACTOR), contains the factors.
c    0.0D+00 .le. FACTOR(I).
c
c    Output, integer NCOMB, the value of the multinomial
c    coefficient.
c
      implicit none

      integer nfactor

      logical check
      integer factor(nfactor)
      integer i
      integer i4_huge
      integer j
      integer k
      logical multicoef_check
      integer ncomb

      check = multicoef_check ( nfactor, factor )

      if ( .not. check ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTINOMIAL_COEF2 - Fatal error!'
        write ( *, '(a)' ) '  MULTICOEF_CHECK failed.'
        ncomb = - i4_huge ( )
        return
      end if

      ncomb = 1
      k = 0

      do i = 1, nfactor

        do j = 1, factor(i)
          k = k + 1
          ncomb = ( ncomb * k ) / j
        end do

      end do

      return
      end
      function multinomial_check ( a, b, c )

c*********************************************************************72
c
cc MULTINOMIAL_CHECK checks the parameters of the Multinomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c
c    Input, integer B, the number of outcomes possible on one
c    trial.  1 .le. B.
c
c    Input, double precision C(B).  C(I) is the probability of outcome I on
c    any trial.
c    0.0D+00 .le. C(I) .le. 1.0D+00,
c    Sum ( 1 .le. I .le. B ) C(I) = 1.0.
c
c    Output, logical MULTINOMIAL_CHECK, is true if the parameters are legal.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      double precision c_sum
      integer i
      logical multinomial_check
      double precision r8vec_sum

      if ( b .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. 1.'
        multinomial_check = .false.
        return
      end if

      do i = 1, b

        if ( c(i) .lt. 0.0D+00 .or. 1.0D+00 .lt. c(i) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MULTINOMIAL_CHECK - Fatal error!'
          write ( *, '(a)' ) '  Input C(I) is out of range.'
          multinomial_check = .false.
          return
        end if

      end do

      c_sum = r8vec_sum ( b, c )

      if ( 0.0001D+00 .lt. abs ( 1.0D+00 - c_sum ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  The probabilities do not sum to 1.'
        multinomial_check = .false.
        return
      end if

      multinomial_check = .true.

      return
      end
      subroutine multinomial_covariance ( a, b, c, covariance )

c*********************************************************************72
c
cc MULTINOMIAL_COVARIANCE returns the covariances of the Multinomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c
c    Input, integer B, the number of outcomes possible on one
c    trial.  1 .le. B.
c
c    Input, double precision C(B).  C(I) is the probability of outcome I on
c    any trial.
c    0.0D+00 .le. C(I) .le. 1.0D+00,
c    SUM ( 1 .le. I .le. B) C(I) = 1.0.
c
c    Output, double precision COVARIANCE(B,B), the covariance matrix.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      double precision covariance(b,b)
      integer i
      integer j

      do i = 1, b
        do j = 1, b

          if ( i .eq. j ) then
            covariance(i,j) = dble ( a ) * c(i) * ( 1.0D+00 - c(i) )
          else
            covariance(i,j) = - dble ( a ) * c(i) * c(j)
          end if

        end do
      end do

      return
      end
      subroutine multinomial_mean ( a, b, c, mean )

c*********************************************************************72
c
cc MULTINOMIAL_MEAN returns the means of the Multinomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c
c    Input, integer B, the number of outcomes possible on one
c    trial.  1 .le. B.
c
c    Input, double precision C(B).  C(I) is the probability of outcome I on
c    any trial.
c    0.0D+00 .le. C(I) .le. 1.0D+00,
c    SUM ( 1 .le. I .le. B) C(I) = 1.0.
c
c    Output, double precision MEAN(B), MEAN(I) is the expected value of the
c    number of outcome I in N trials.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      integer i
      double precision mean(b)

      do i = 1, b
        mean(i) = dble ( a ) * c(i)
      end do

      return
      end
      subroutine multinomial_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc MULTINOMIAL_PDF computes a Multinomial PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = Comb(A,B,X) * Product ( 1 .le. I .le. B ) C(I)^X(I)
c
c    where Comb(A,B,X) is the multinomial coefficient
c      C( A; X(1), X(2), ..., X(B) )
c
c    PDF(A,B,C;X) is the probability that in A trials there
c    will be exactly X(I) occurrences of event I, whose probability
c    on one trial is C(I), for I from 1 to B.
c
c    As soon as A or B gets large, the number of possible X's explodes,
c    and the probability of any particular X can become extremely small.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X(B); X(I) counts the number of occurrences of
c    outcome I, out of the total of A trials.
c
c    Input, integer A, the total number of trials.
c
c    Input, integer B, the number of different possible outcomes on
c    one trial.
c
c    Input, double precision C(B); C(I) is the probability of outcome I on
c    any one trial.
c
c    Output, double precision PDF, the value of the multinomial PDF.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      double precision gamma_log
      integer i
      double precision pdf
      double precision pdf_log
      integer x(b)
c
c  To try to avoid overflow, do the calculation in terms of logarithms.
c  Note that Gamma(A+1) = A factorial.
c
      pdf_log = gamma_log ( dble ( a + 1 ) )

      do i = 1, b
        pdf_log = pdf_log + x(i) * log ( c(i) )      
     &    - gamma_log ( dble ( x(i) + 1 ) )
      end do

      pdf = exp ( pdf_log )

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
c    14 February 1999
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
c    0 .le. A.
c
c    Input, integer B, the number of outcomes possible on
c    one trial.  1 .le. B.
c
c    Input, double precision C(B).  C(I) is the probability of outcome I on
c    any trial.
c    0.0D+00 .le. C(I) .le. 1.0D+00,
c    sum ( 1 .le. I .le. B) C(I) = 1.0.
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
      integer i
      integer ifactor
      integer ntot
      double precision prob
      integer seed
      double precision sum2
      integer x(b)

      ntot = a

      sum2 = 1.0D+00

      do i = 1, b
        x(i) = 0
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
      subroutine multinomial_variance ( a, b, c, variance )

c*********************************************************************72
c
cc MULTINOMIAL_VARIANCE returns the variances of the Multinomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, the number of trials.
c
c    Input, integer B, the number of outcomes possible on one
c    trial.  1 .le. B.
c
c    Input, double precision C(B).  C(I) is the probability of outcome I on
c    any trial.
c    0.0D+00 .le. C(I) .le. 1.0D+00,
c    sum ( 1 .le. I .le. B ) C(I) = 1.0.
c
c    Output, double precision VARIANCE(B), VARIANCE(I) is the variance of the
c    total number of events of type I.
c
      implicit none

      integer b

      integer a
      double precision c(b)
      integer i
      double precision variance(b)

      do i = 1, b
        variance(i) = dble ( a ) * c(i) * ( 1.0D+00 - c(i) )
      end do

      return
      end
      subroutine multivariate_normal_sample ( n, mean, covar_factor, see
     &d, x )

c*********************************************************************72
c
cc MULTIVARIATE_NORMAL_SAMPLE samples the Multivariate Normal PDF.
c
c  Discussion:
c
c    PDF ( Mean(1:N), S(1:N,1:N); X(1:N) ) =
c      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / det ( S )
c      * exp ( - ( X - Mean )' * inverse ( S ) * ( X - Mean ) / 2 )
c
c    Here,
c
c      X is the argument vector of length N,
c      Mean is the mean vector of length N,
c      S is an N by N positive definite symmetric covariance matrix.
c
c    The properties of S guarantee that it has a lower triangular
c    matrix L, the Cholesky factor, such that S = L * L'.  It is the
c    matrix L, rather than S, that is required by this routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jerry Banks, editor,
c    Handbook of Simulation,
c    Engineering and Management Press Books, 1998, page 167.
c
c  Parameters:
c
c    Input, integer N, the spatial dimension.
c
c    Input, double precision MEAN(N), the mean vector.
c
c    Input, double precision COVAR_FACTOR(N,N), the lower triangular Cholesky
c    factor L of the covariance matrix S.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(N), a sample point of the distribution.
c
      implicit none

      integer n

      double precision covar_factor(n,n)
      integer i
      integer j
      double precision mean(n)
      integer seed
      double precision x(n)
      double precision z

      do i = 1, n

        call normal_01_sample ( seed, z )

        x(i) = mean(i)

        do j = 1, i
          x(i) = x(i) + covar_factor(i,j) * z
        end do

      end do

      return
      end
      subroutine nakagami_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc NAKAGAMI_CDF evaluates the Nakagami CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B
c    0.0D+00 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision gamma_inc
      double precision p2
      double precision x
      double precision x2
      double precision y

      if ( x .le. 0.0D+00 ) then

        cdf = 0.0D+00

      else if ( 0.0D+00 .lt. x ) then

        y = ( x - a ) / b
        x2 = c * y * y
        p2 = c

        cdf = gamma_inc ( p2, x2 )

      end if

      return
      end
      function nakagami_check ( a, b, c )

c*********************************************************************72
c
cc NAKAGAMI_CHECK checks the parameters of the Nakagami PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, logical NAKAGAMI_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical nakagami_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NAKAGAMI_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        nakagami_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NAKAGAMI_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        nakagami_check = .false.
        return
      end if

      nakagami_check = .true.

      return
      end
      subroutine nakagami_mean ( a, b, c, mean )

c*********************************************************************72
c
cc NAKAGAMI_MEAN returns the mean of the Nakagami PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B
c    0.0D+00 .lt. C
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean
      double precision r8_gamma

      mean = a + b * r8_gamma ( c + 0.5D+00 ) / ( sqrt ( c ) * r8_gamma 
     &( c ) )

      return
      end
      subroutine nakagami_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc NAKAGAMI_PDF evaluates the Nakagami PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B
c    0.0D+00 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision r8_gamma
      double precision x
      double precision y

      if ( x .le. 0.0D+00 ) then

        pdf = 0.0D+00

      else if ( 0.0D+00 .lt. x ) then

        y = ( x - a ) / b

        pdf = 2.0D+00 * c**c / ( b * r8_gamma ( c ) )       * y**( 2.0D+
     &00 * c - 1.0D+00 )       * exp ( - c * y * y )

      end if

      return
      end
      subroutine nakagami_variance ( a, b, c, variance )

c*********************************************************************72
c
cc NAKAGAMI_VARIANCE returns the variance of the Nakagami PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B
c    0.0D+00 .lt. C
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision r8_gamma
      double precision t1
      double precision t2
      double precision variance

      t1 = r8_gamma ( c + 0.5D+00 )
      t2 = r8_gamma ( c )

      variance = b * b * ( 1.0D+00 - t1 * t1 / ( c * t2 * t2 ) )

      return
      end
      subroutine negative_binomial_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_CDF evaluates the Negative Binomial CDF.
c
c  Discussion:
c
c    A simple summing approach is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the CDF.
c
c    Input, integer A, a parameter of the PDF.
c    0 .le. A.
c
c    Input, double precision B, a parameter of the PDF.
c    0 .lt. B .le. 1.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer a
      double precision b
      double precision cdf
      integer cnk
      double precision pdf
      integer x
      integer y

      cdf = 0.0D+00

      do y = a, x

        call binomial_coef ( y - 1, a - 1, cnk )

        pdf = dble ( cnk ) * b**a * ( 1.0D+00 - b )**( y - a )

        cdf = cdf + pdf

      end do

      return
      end
      subroutine negative_binomial_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_CDF_INV inverts the Negative Binomial CDF.
c
c  Discussion:
c
c    A simple discrete approach is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, integer A, a parameter of the PDF.
c    0 .le. A.
c
c    Input, double precision B, a parameter of the PDF.
c    0 .lt. B .le. 1.
c
c    Output, integer X, the smallest X whose cumulative density
c    function is greater than or equal to CDF.
c
      implicit none

      integer a
      double precision b
      double precision cdf
      double precision cum
      double precision pdf
      integer x
      integer x_max
      parameter ( x_max = 1000 )

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if


      cum = 0.0D+00

      x = a

10    continue

        call negative_binomial_pdf ( x, a, b, pdf )

        cum = cum + pdf

        if ( cdf .le. cum .or. x_max .le. x ) then
          go to 20
        end if

        x = x + 1

      go to 10

20    continue

      return
      end
      subroutine negative_binomial_cdf_values ( n_data, f, s, p, cdf )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_CDF_VALUES returns values of the negative binomial CDF.
c
c  Discussion:
c
c    Assume that a coin has a probability P of coming up heads on
c    any one trial.  Suppose that we plan to flip the coin until we
c    achieve a total of S heads.  If we let F represent the number of
c    tails that occur in this process, then the value of F satisfies
c    a negative binomial PDF:
c
c      PDF(F,S,P) = Choose ( F from F+S-1 ) * P**S * (1-P)**F
c
c    The negative binomial CDF is the probability that there are F or
c    fewer failures upon the attainment of the S-th success.  Thus,
c
c      CDF(F,S,P) = sum ( 0 <= G <= F ) PDF(G,S,P)
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`DiscreteDistributions`]
c      dist = NegativeBinomialDistribution [ s, p ]
c      CDF [ dist, f ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Frank Powell,
c    Statistical Tables for Sociology, Biology and Physical Sciences,
c    Cambridge University Press, 1982.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer F, the maximum number of failures.
c
c    Output, integer S, the number of successes.
c
c    Output, double precision P, the probability of a success on one trial.
c
c    Output, double precision CDF, the probability of at most F failures
c    before the S-th success.
c
      implicit none

      integer n_max
      parameter ( n_max = 27 )

      double precision cdf
      double precision cdf_vec(n_max)
      integer f
      integer f_vec(n_max)
      integer n_data
      double precision p
      double precision p_vec(n_max)
      integer s
      integer s_vec(n_max)

      save cdf_vec
      save f_vec
      save p_vec
      save s_vec

      data cdf_vec /
     &  0.6367187500000000D+00,
     &  0.3632812500000000D+00,
     &  0.1445312500000000D+00,
     &  0.5000000000000000D+00,
     &  0.2265625000000000D+00,
     &  0.6250000000000000D-01,
     &  0.3437500000000000D+00,
     &  0.1093750000000000D+00,
     &  0.1562500000000000D-01,
     &  0.1792000000000000D+00,
     &  0.4096000000000000D-01,
     &  0.4096000000000000D-02,
     &  0.7047000000000000D-01,
     &  0.1093500000000000D-01,
     &  0.7290000000000000D-03,
     &  0.9861587127990000D+00,
     &  0.9149749500510000D+00,
     &  0.7471846521450000D+00,
     &  0.8499053647030009D+00,
     &  0.5497160941090026D+00,
     &  0.2662040052146710D+00,
     &  0.6513215599000000D+00,
     &  0.2639010709000000D+00,
     &  0.7019082640000000D-01,
     &  0.1000000000000000D+01,
     &  0.1990000000000000D-01,
     &  0.1000000000000000D-03 /
      data f_vec /
     &   4,  3,  2,
     &   3,  2,  1,
     &   2,  1,  0,
     &   2,  1,  0,
     &   2,  1,  0,
     &  11, 10,  9,
     &  17, 16, 15,
     &   9,  8,  7,
     &   2,  1,  0 /
      data p_vec /
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.40D+00,
     &  0.40D+00,
     &  0.40D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.30D+00,
     &  0.10D+00,
     &  0.10D+00,
     &  0.10D+00,
     &  0.10D+00,
     &  0.10D+00,
     &  0.10D+00,
     &  0.10D-01,
     &  0.10D-01,
     &  0.10D-01 /
      data s_vec /
     &  4, 5, 6,
     &  4, 5, 6,
     &  4, 5, 6,
     &  4, 5, 6,
     &  4, 5, 6,
     &  1, 2, 3,
     &  1, 2, 3,
     &  1, 2, 3,
     &  0, 1, 2 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        f = 0
        s = 0
        p = 0.0D+00
        cdf = 0.0D+00
      else
        f = f_vec(n_data)
        s = s_vec(n_data)
        p = p_vec(n_data)
        cdf = cdf_vec(n_data)
      end if

      return
      end
      function negative_binomial_check ( a, b )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_CHECK checks the parameters of the Negative Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, a parameter of the PDF.
c    0 .le. A.
c
c    Input, double precision B, a parameter of the PDF.
c    0 .lt. B .le. 1.
c
c    Output, logical NEGATIVE_BINOMIAL_CHECK, is true if the
c    parameters are legal.
c
      implicit none

      integer a
      double precision b
      logical negative_binomial_check

      if ( a .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 0.'
        negative_binomial_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 .or. 1.0D+00 .lt. b ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0 or 1 .lt. B.'
        negative_binomial_check = .false.
        return
      end if

      negative_binomial_check = .true.

      return
      end
      subroutine negative_binomial_mean ( a, b, mean )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_MEAN returns the mean of the Negative Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, a parameter of the PDF.
c    0 .le. A.
c
c    Input, double precision B, a parameter of the PDF.
c    0 .lt. B .le. 1.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision mean

      mean = dble ( a ) / b

      return
      end
      subroutine negative_binomial_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_PDF evaluates the Negative Binomial PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = C(X-1,A-1) * B^A * ( 1 - B )^(X-A)
c
c    PDF(A,B;X) is the probability that the A-th success will
c    occur on the X-th trial, given that the probability
c    of a success on a single trial is B.
c
c    The Negative Binomial PDF is also known as the Pascal PDF or
c    the "Polya" PDF.
c
c    NEGATIVE_BINOMIAL_PDF(1,B;X) = GEOMETRIC_PDF(B;X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the number of trials.
c    A .le. X.
c
c    Input, integer A, the number of successes required.
c    0 .le. A .le. X, normally.
c
c    Input, double precision B, the probability of a success on a single trial.
c    0.0 .lt. B .le. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      double precision b
      integer cnk
      double precision pdf
      integer x

      if ( x .lt. a ) then

        pdf = 0.0D+00

      else

        call binomial_coef ( x - 1, a - 1, cnk )

        pdf = dble ( cnk ) * b**a * ( 1.0D+00 - b )**( x - a )

      end if

      return
      end
      subroutine negative_binomial_sample ( a, b, seed, x )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_SAMPLE samples the Negative Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, a parameter of the PDF.
c    0 .le. A.
c
c    Input, double precision B, a parameter of the PDF.
c    0 .lt. B .le. 1.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      integer a
      double precision b
      integer i4_huge
      integer num_success
      double precision r
      double precision r8_uniform_01
      integer seed
      integer x

      if ( b .eq. 1.0D+00 ) then
        x = a
        return
      else if ( b .eq. 0.0D+00 ) then
        x = i4_huge ( )
        return
      end if

      x = 0
      num_success = 0

10    continue

      if ( num_success .lt. a ) then

        x = x + 1
        r = r8_uniform_01 ( seed )

        if ( r .le. b ) then
          num_success = num_success + 1
        end if

        go to 10

      end if

      return
      end
      subroutine negative_binomial_variance ( a, b, variance )

c*********************************************************************72
c
cc NEGATIVE_BINOMIAL_VARIANCE returns the variance of the Negative Binomial PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, a parameter of the PDF.
c    0 .le. A.
c
c    Input, double precision B, a parameter of the PDF.
c    0 .lt. B .le. 1.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision variance

      variance = dble ( a ) * ( 1.0D+00 - b ) / ( b * b )

      return
      end
      subroutine normal_01_cdf ( x, cdf )

c*********************************************************************72
c
cc NORMAL_01_CDF evaluates the Normal 01 CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    AG Adams,
c    Algorithm 39,
c    Areas Under the Normal Curve,
c    Computer Journal,
c    Volume 12, pages 197-198, 1969.
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a1
      parameter ( a1 = 0.398942280444D+00 )
      double precision a2
      parameter ( a2 = 0.399903438504D+00 )
      double precision, parameter :: a3 = 5.75885480458D+00
      double precision, parameter :: a4 = 29.8213557808D+00
      double precision, parameter :: a5 = 2.62433121679D+00
      double precision, parameter :: a6 = 48.6959930692D+00
      double precision, parameter :: a7 = 5.92885724438D+00
      double precision, parameter :: b0 = 0.398942280385D+00
      double precision, parameter :: b1 = 3.8052D-08
      double precision, parameter :: b2 = 1.00000615302D+00
      double precision, parameter :: b3 = 3.98064794D-04
      double precision, parameter :: b4 = 1.98615381364D+00
      double precision, parameter :: b5 = 0.151679116635D+00
      double precision, parameter :: b6 = 5.29330324926D+00
      double precision, parameter :: b7 = 4.8385912808D+00
      double precision, parameter :: b8 = 15.1508972451D+00
      double precision, parameter :: b9 = 0.742380924027D+00
      double precision, parameter :: b10 = 30.789933034D+00
      double precision b11
      parameter ( b11 = 3.99019417011D+00 )
      double precision cdf
      double precision q
      double precision x
      double precision y
c
c  |X| .le. 1.28.
c
      if ( abs ( x ) .le. 1.28D+00 ) then

        y = 0.5D+00 * x * x

        q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 / ( y + 
     &a5       + a6 / ( y + a7 ) ) ) )
c
c  1.28 .lt. |X| .le. 12.7
c
      else if ( abs ( x ) .le. 12.7D+00 ) then

        y = 0.5D+00 * x * x

        q = exp ( - y ) * b0 / ( abs ( x ) - b1       + b2 / ( abs ( x )
     & + b3       + b4 / ( abs ( x ) - b5       + b6 / ( abs ( x ) + b7 
     &      - b8 / ( abs ( x ) + b9       + b10 / ( abs ( x ) + b11 ) ) 
     &) ) ) )
c
c  12.7 .lt. |X|
c
      else

        q = 0.0D+00

      end if
c
c  Take account of negative X.
c
      if ( x .lt. 0.0D+00 ) then
        cdf = q
      else
        cdf = 1.0D+00 - q
      end if

      return
      end
      subroutine normal_01_cdf_inv ( p, x )

c*********************************************************************72
c
cc NORMAL_01_CDF_INV inverts the standard normal CDF.
c
c  Discussion:
c
c    The result is accurate to about 1 part in 10**16.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2013
c
c  Author:
c
c    Original FORTRAN77 version by Michael Wichura.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Michael Wichura,
c    Algorithm AS241:
c    The Percentage Points of the Normal Distribution,
c    Applied Statistics,
c    Volume 37, Number 3, pages 477-484, 1988.
c
c  Parameters:
c
c    Input, double precision P, the value of the cumulative probability
c    densitity function.  0 .lt. P .lt. 1.  If P is outside this range, an
c    "infinite" value will be returned.
c
c    Output, double precision X, the normal deviate value
c    with the property that the probability of a standard normal deviate being
c    less than or equal to the value is P.
c
      implicit none

      double precision a(8)
      double precision b(8)
      double precision c(8)
      double precision const1
      parameter ( const1 = 0.180625D+00 )
      double precision const2
      parameter ( const2 = 1.6D+00 )
      double precision d(8)
      double precision e(8)
      double precision f(8)
      double precision p
      double precision q
      double precision r
      double precision r8_huge
      double precision r8poly_value
      double precision split1
      parameter ( split1 = 0.425D+00 )
      double precision split2
      parameter ( split2 = 5.0D+00 )
      double precision x

      save a
      save b
      save c
      save d
      save e
      save f

      data a /
     &  3.3871328727963666080D+00,
     &  1.3314166789178437745D+02,
     &  1.9715909503065514427D+03,
     &  1.3731693765509461125D+04,
     &  4.5921953931549871457D+04,     
     &  6.7265770927008700853D+04,     
     &  3.3430575583588128105D+04,     
     &  2.5090809287301226727D+03 /
      data b /
     &  1.0D+00,
     &  4.2313330701600911252D+01,
     &  6.8718700749205790830D+02,  
     &  5.3941960214247511077D+03,
     &  2.1213794301586595867D+04,
     &  3.9307895800092710610D+04,
     &  2.8729085735721942674D+04,
     &  5.2264952788528545610D+03 /
      data c /
     &  1.42343711074968357734D+00,
     &  4.63033784615654529590D+00,
     &  5.76949722146069140550D+00,
     &  3.64784832476320460504D+00,
     &  1.27045825245236838258D+00,
     &  2.41780725177450611770D-01,
     &  2.27238449892691845833D-02,
     &  7.74545014278341407640D-04 /
      data d /
     &  1.0D+00,
     &  2.05319162663775882187D+00,
     &  1.67638483018380384940D+00,
     &  6.89767334985100004550D-01,
     &  1.48103976427480074590D-01,  
     &  1.51986665636164571966D-02,
     &  5.47593808499534494600D-04,    
     &  1.05075007164441684324D-09 /
      data e /
     &  6.65790464350110377720D+00,
     &  5.46378491116411436990D+00,
     &  1.78482653991729133580D+00,
     &  2.96560571828504891230D-01,
     &  2.65321895265761230930D-02,
     &  1.24266094738807843860D-03,
     &  2.71155556874348757815D-05,
     &  2.01033439929228813265D-07 /
      data f /
     &  1.0D+00,
     &  5.99832206555887937690D-01,
     &  1.36929880922735805310D-01,
     &  1.48753612908506148525D-02,
     &  7.86869131145613259100D-04,  
     &  1.84631831751005468180D-05,
     &  1.42151175831644588870D-07,    
     &  2.04426310338993978564D-15 /

      if ( p .le. 0.0D+00 ) then
        x = - r8_huge ( )
        return
      end if

      if ( 1.0D+00 .le. p ) then
        x = r8_huge ( )
        return
      end if

      q = p - 0.5D+00

      if ( abs ( q ) .le. split1 ) then

        r = const1 - q * q
        x = q * r8poly_value ( 8, a, r ) / r8poly_value ( 8, b, r )

      else

        if ( q .lt. 0.0D+00 ) then
          r = p
        else
          r = 1.0D+00 - p
        end if

        if ( r .le. 0.0D+00 ) then

          x = r8_huge ( )

        else

          r = sqrt ( - log ( r ) )

          if ( r .le. split2 ) then

            r = r - const2
            x = r8poly_value ( 8, c, r ) / r8poly_value ( 8, d, r )

          else

            r = r - split2
            x = r8poly_value ( 8, e, r ) / r8poly_value ( 8, f, r )

          end if

        end if

        if ( q .lt. 0.0D+00 ) then
          x = -x
        end if

      end if

      return
      end
      subroutine normal_01_cdf_values ( n_data, x, fx )

c*********************************************************************72
c
cc NORMAL_01_CDF_VALUES returns some values of the Normal 01 CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = NormalDistribution [ 0, 1 ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 17 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save x_vec

      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.5398278372770290D+00,
     &  0.5792597094391030D+00,
     &  0.6179114221889526D+00,
     &  0.6554217416103242D+00,
     &  0.6914624612740131D+00,
     &  0.7257468822499270D+00,
     &  0.7580363477769270D+00,
     &  0.7881446014166033D+00,
     &  0.8159398746532405D+00,
     &  0.8413447460685429D+00,
     &  0.9331927987311419D+00,
     &  0.9772498680518208D+00,
     &  0.9937903346742239D+00,
     &  0.9986501019683699D+00,
     &  0.9997673709209645D+00,
     &  0.9999683287581669D+00 /
      data x_vec /
     &  0.0000000000000000D+00,
     &  0.1000000000000000D+00,
     &  0.2000000000000000D+00,
     &  0.3000000000000000D+00,
     &  0.4000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.6000000000000000D+00,
     &  0.7000000000000000D+00,
     &  0.8000000000000000D+00,
     &  0.9000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.1500000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2500000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3500000000000000D+01,
     &  0.4000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine normal_01_mean ( mean )

c*********************************************************************72
c
cc NORMAL_01_MEAN returns the mean of the Normal 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision mean

      mean = 0.0D+00

      return
      end
      subroutine normal_01_pdf ( x, pdf )

c*********************************************************************72
c
cc NORMAL_01_PDF evaluates the Normal 01 PDF.
c
c  Discussion:
c
c    The Normal 01 PDF is also called the "Standard Normal" PDF, or
c    the Normal PDF with 0 mean and variance 1.
c
c    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      pdf = exp ( -0.5D+00 * x * x ) / sqrt ( 2.0D+00 * pi )

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
c
c  If we've used an even number of values so far, generate two more,
c  return one and save one.
c
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
c
c  Otherwise, return the second, saved, value.
c
      else

        x = y

      end if

      used = used + 1

      return
      end
      subroutine normal_01_variance ( variance )

c*********************************************************************72
c
cc NORMAL_01_VARIANCE returns the variance of the Normal 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision variance

      variance = 1.0D+00

      return
      end
      subroutine normal_01_vector ( n, seed, x )

c*********************************************************************72
c
cc NORMAL_01_VECTOR samples the standard normal probability distribution.
c
c  Discussion:
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c    Before calling this routine, the user may call RANDOM_SEED
c    in order to set the seed of the random number generator.
c
c    The Box-Muller method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, real R(N+1), is used to store some uniform random values.
c    Its dimension is N+1, but really it is only needed to be the
c    smallest even number greater than or equal to N.
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.
c
      implicit none

      integer n

      integer m
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(n+1)
      double precision r8_uniform_01
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n

      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )
        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =       
     &    sqrt ( -2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        m = ( x_hi_index - x_lo_index + 1 ) / 2

        call r8vec_uniform_01 ( 2 * m, seed, r )

        x(x_lo_index:x_hi_index-1:2) =       
     &    sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) 
     &    * cos ( 2.0D+00 * pi * r(2:2*m:2) )

        x(x_lo_index+1:x_hi_index:2) =
     &    sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) )
     &    * sin ( 2.0D+00 * pi * r(2:2*m:2) )
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        x_hi_index = x_hi_index - 1

        m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

        call r8vec_uniform_01 ( 2 * m, seed, r )

        x(x_lo_index:x_hi_index-1:2) =
     &    sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) )
     &    * cos ( 2.0D+00 * pi * r(2:2*m-2:2) )

        x(x_lo_index+1:x_hi_index:2) =
     &    sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) )
     &    * sin ( 2.0D+00 * pi * r(2:2*m-2:2) )

        x(n) = sqrt ( -2.0D+00 * log ( r(2*m-1) ) )
     &    * cos ( 2.0D+00 * pi * r(2*m) )

      end if

      return
      end
      subroutine normal_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc NORMAL_CDF evaluates the Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x
      double precision y

      y = ( x - a ) / b

      call normal_01_cdf ( y, cdf )

      return
      end
      subroutine normal_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc NORMAL_CDF_INV inverts the Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x
      double precision x2

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NORMAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      call normal_01_cdf_inv ( cdf, x2 )

      x = a + b * x2

      return
      end
      subroutine normal_cdf_values ( n_data, mu, sigma, x, fx )

c*********************************************************************72
c
cc NORMAL_CDF_VALUES returns some values of the Normal CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = NormalDistribution [ mu, sigma ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision MU, the mean of the distribution.
c
c    Output, double precision SIGMA, the variance of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision fx
      double precision fx_vec(n_max)
      double precision mu
      double precision mu_vec(n_max)
      integer n_data
      double precision sigma
      double precision sigma_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save mu_vec
      save sigma_vec
      save x_vec

      data fx_vec /
     &  0.5000000000000000D+00,
     &  0.9772498680518208D+00,
     &  0.9999683287581669D+00,
     &  0.9999999990134124D+00,
     &  0.6914624612740131D+00,
     &  0.6305586598182364D+00,
     &  0.5987063256829237D+00,
     &  0.5792597094391030D+00,
     &  0.6914624612740131D+00,
     &  0.5000000000000000D+00,
     &  0.3085375387259869D+00,
     &  0.1586552539314571D+00 /
      data mu_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data sigma_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        mu = 0.0D+00
        sigma = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        mu = mu_vec(n_data)
        sigma = sigma_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function normal_check ( a, b )

c*********************************************************************72
c
cc NORMAL_CHECK checks the parameters of the Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical NORMAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical normal_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NORMAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        normal_check = .false.
        return
      end if

      normal_check = .true.

      return
      end
      subroutine normal_mean ( a, b, mean )

c*********************************************************************72
c
cc NORMAL_MEAN returns the mean of the Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine normal_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc NORMAL_PDF evaluates the Normal PDF.
c
c  Discussion:
c
c    PDF(A,B;X)
c      = exp ( - 0.5D+00 * ( ( X - A ) / B )^2 ) / ( B * sqrt ( 2 * PI ) )
c
c    The normal PDF is also known as the Gaussian PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      y = ( x - a ) / b

      pdf = exp ( - 0.5D+00 * y * y )  / ( b * sqrt ( 2.0D+00 * pi ) )

      return
      end
      subroutine normal_sample ( a, b, seed, x )

c*********************************************************************72
c
cc NORMAL_SAMPLE samples the Normal PDF.
c
c  Discussion:
c
c    The Box-Muller method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer seed
      double precision x

      call normal_01_sample ( seed, x )

      x = a + b * x

      return
      end
      subroutine normal_variance ( a, b, variance )

c*********************************************************************72
c
cc NORMAL_VARIANCE returns the variance of the Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = b * b

      return
      end
      subroutine normal_vector ( n, mean, dev, seed, x )

c*********************************************************************72
c
cc NORMAL_VECTOR samples the normal probability distribution.
c
c  Discussion:
c
c    The normal probability distribution function (PDF) has
c    a user-specified mean and standard deviation.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c    Before calling this routine, the user may call RANDOM_SEED
c    in order to set the seed of the random number generator.
c
c    The Box-Muller method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.
c
c    Input, double precision MEAN, the desired mean value.
c
c    Input, double precision DEV, the desired standard deviation.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
      implicit none

      integer n

      integer i
      integer seed
      double precision x(n)
      double precision dev
      double precision mean

      call normal_01_vector ( n, seed, x )

      do i = 1, n
        x(i) = mean + dev * x(i)
      end do

      return
      end
      subroutine normal_truncated_ab_cdf ( x, mu, s, a, b, cdf )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_AB_CDF evaluates the truncated Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision b
      double precision beta
      double precision beta_cdf
      double precision cdf
      double precision mu
      double precision s
      double precision x
      double precision xi
      double precision xi_cdf

      alpha = ( a - mu ) / s
      beta = ( b - mu ) / s
      xi = ( x - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( beta, beta_cdf )
      call normal_01_cdf ( xi, xi_cdf )

      cdf = ( xi_cdf - alpha_cdf ) / ( beta_cdf - alpha_cdf )

      return
      end
      subroutine normal_truncated_ab_cdf_inv ( cdf, mu, s, a, b, x )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_AB_CDF_INV inverts the truncated Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 <= CDF <= 1.0.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision b
      double precision beta
      double precision beta_cdf
      double precision cdf
      double precision mu
      double precision s
      double precision x
      double precision xi
      double precision xi_cdf

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NORMAL_TRUNCATED_AB_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
        stop
      end if

      alpha = ( a - mu ) / s
      beta = ( b - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( beta, beta_cdf )

      xi_cdf = ( beta_cdf - alpha_cdf ) * cdf + alpha_cdf
      call normal_01_cdf_inv ( xi_cdf, xi )

      x = mu + s * xi

      return
      end
      subroutine normal_truncated_ab_mean ( mu, s, a, b, mean )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_AB_MEAN returns the mean of the truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviatione of the
c    parent Normal distribution.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision alpha_pdf
      double precision b
      double precision beta
      double precision beta_cdf
      double precision beta_pdf
      double precision mean
      double precision mu
      double precision s

      alpha = ( a - mu ) / s
      beta = ( b - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( beta, beta_cdf )

      call normal_01_pdf ( alpha, alpha_pdf )
      call normal_01_pdf ( beta, beta_pdf )

      mean = mu + s * ( alpha_pdf - beta_pdf ) 
     &  / ( beta_cdf - alpha_cdf )

      return
      end
      subroutine normal_truncated_ab_pdf ( x, mu, s, a, b, pdf )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_AB_PDF evaluates the truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision b
      double precision beta
      double precision beta_cdf
      double precision mu
      double precision pdf
      double precision s
      double precision x
      double precision xi
      double precision xi_pdf

      alpha = ( a - mu ) / s
      beta = ( b - mu ) / s
      xi = ( x - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( beta, beta_cdf )
      call normal_01_pdf ( xi, xi_pdf )

      pdf = xi_pdf / ( beta_cdf - alpha_cdf ) / s

      return
      end
      subroutine normal_truncated_ab_sample ( mu, s, a, b, seed, x )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_AB_SAMPLE samples the truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision b
      double precision beta
      double precision beta_cdf
      double precision mu
      double precision r8_uniform_01
      double precision s
      integer seed
      double precision u
      double precision x
      double precision xi
      double precision xi_cdf

      alpha = ( a - mu ) / s
      beta = ( b - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( beta, beta_cdf )

      u = r8_uniform_01 ( seed )
      xi_cdf = alpha_cdf + u * ( beta_cdf - alpha_cdf )
      call normal_01_cdf_inv ( xi_cdf, xi )

      x = mu + s * xi

      return
      end
      subroutine normal_truncated_ab_variance ( mu, s, a, b, variance )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_AB_VARIANCE returns the variance of the truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision alpha_pdf
      double precision b
      double precision beta
      double precision beta_cdf
      double precision beta_pdf
      double precision mu
      double precision s
      double precision variance

      alpha = ( a - mu ) / s
      beta = ( b - mu ) / s

      call normal_01_pdf ( alpha, alpha_pdf )
      call normal_01_pdf ( beta, beta_pdf )

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( beta, beta_cdf )

      variance = s * s * ( 1.0D+00 
     &  + ( alpha * alpha_pdf - beta * beta_pdf ) 
     &  / ( beta_cdf - alpha_cdf ) 
     &  - ( ( alpha_pdf - beta_pdf ) / ( beta_cdf - alpha_cdf ) ) ** 2 )

      return
      end
      subroutine normal_truncated_a_cdf ( x, mu, s, a, cdf )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_A_CDF evaluates the lower truncated Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision cdf
      double precision mu
      double precision s
      double precision x
      double precision xi
      double precision xi_cdf

      alpha = ( a - mu ) / s
      xi = ( x - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_cdf ( xi, xi_cdf )

      cdf = ( xi_cdf - alpha_cdf ) / ( 1.0D+00 - alpha_cdf )

      return
      end
      subroutine normal_truncated_a_cdf_inv ( cdf, mu, s, a, x )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_A_CDF_INV inverts the lower truncated Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 <= CDF <= 1.0.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision cdf
      double precision mu
      double precision s
      double precision x
      double precision xi
      double precision xi_cdf

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NORMAL_TRUNCATED_A_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
        stop
      end if

      alpha = ( a - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )

      xi_cdf = ( 1.0D+00 - alpha_cdf ) * cdf + alpha_cdf
      call normal_01_cdf_inv ( xi_cdf, xi )

      x = mu + s * xi

      return
      end
      subroutine normal_truncated_a_mean ( mu, s, a, mean )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_A_MEAN returns the mean of the lower truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviatione of the
c    parent Normal distribution.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision alpha_pdf
      double precision mean
      double precision mu
      double precision s

      alpha = ( a - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )

      call normal_01_pdf ( alpha, alpha_pdf )

      mean = mu + s * alpha_pdf
     &  / ( 1.0D+00 - alpha_cdf )

      return
      end
      subroutine normal_truncated_a_pdf ( x, mu, s, a, pdf )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_A_PDF evaluates the lower truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision mu
      double precision pdf
      double precision s
      double precision x
      double precision xi
      double precision xi_pdf

      alpha = ( a - mu ) / s
      xi = ( x - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )
      call normal_01_pdf ( xi, xi_pdf )

      pdf = xi_pdf / ( 1.0D+00 - alpha_cdf ) / s

      return
      end
      subroutine normal_truncated_a_sample ( mu, s, a, seed, x )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_A_SAMPLE samples the lower truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, the lower truncation limit.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision mu
      double precision r8_uniform_01
      double precision s
      integer seed
      double precision u
      double precision x
      double precision xi
      double precision xi_cdf

      alpha = ( a - mu ) / s

      call normal_01_cdf ( alpha, alpha_cdf )

      u = r8_uniform_01 ( seed )
      xi_cdf = alpha_cdf + u * ( 1.0D+00 - alpha_cdf )
      call normal_01_cdf_inv ( xi_cdf, xi )

      x = mu + s * xi

      return
      end
      subroutine normal_truncated_a_variance ( mu, s, a, variance )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_A_VARIANCE: variance of the lower truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision alpha
      double precision alpha_cdf
      double precision alpha_pdf
      double precision mu
      double precision s
      double precision variance

      alpha = ( a - mu ) / s

      call normal_01_pdf ( alpha, alpha_pdf )

      call normal_01_cdf ( alpha, alpha_cdf )

      variance = s * s * ( 1.0D+00 
     &  + ( alpha * alpha_pdf ) 
     &  / ( 1.0D+00 - alpha_cdf ) 
     &  - ( alpha_pdf / ( 1.0D+00 - alpha_cdf ) ) ** 2 )

      return
      end
      subroutine normal_truncated_b_cdf ( x, mu, s, b, cdf )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_B_CDF evaluates the upper truncated Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision b
      double precision beta
      double precision beta_cdf
      double precision cdf
      double precision mu
      double precision s
      double precision x
      double precision xi
      double precision xi_cdf

      beta = ( b - mu ) / s
      xi = ( x - mu ) / s

      call normal_01_cdf ( beta, beta_cdf )
      call normal_01_cdf ( xi, xi_cdf )

      cdf = xi_cdf / beta_cdf

      return
      end
      subroutine normal_truncated_b_cdf_inv ( cdf, mu, s, b, x )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_B_CDF_INV inverts the upper truncated Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 <= CDF <= 1.0.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision b
      double precision beta
      double precision beta_cdf
      double precision cdf
      double precision mu
      double precision s
      double precision x
      double precision xi
      double precision xi_cdf

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NORMAL_TRUNCATED_B_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
        stop
      end if

      beta = ( b - mu ) / s

      call normal_01_cdf ( beta, beta_cdf )

      xi_cdf = beta_cdf * cdf
      call normal_01_cdf_inv ( xi_cdf, xi )

      x = mu + s * xi

      return
      end
      subroutine normal_truncated_b_mean ( mu, s, b, mean )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_B_MEAN returns the mean of the upper truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviatione of the
c    parent Normal distribution.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision b
      double precision beta
      double precision beta_cdf
      double precision beta_pdf
      double precision mean
      double precision mu
      double precision s

      beta = ( b - mu ) / s

      call normal_01_cdf ( beta, beta_cdf )

      call normal_01_pdf ( beta, beta_pdf )

      mean = mu - s * beta_pdf / beta_cdf

      return
      end
      subroutine normal_truncated_b_pdf ( x, mu, s, b, pdf )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_B_PDF evaluates the upper truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision b
      double precision beta
      double precision beta_cdf
      double precision mu
      double precision pdf
      double precision s
      double precision x
      double precision xi
      double precision xi_pdf

      beta = ( b - mu ) / s
      xi = ( x - mu ) / s

      call normal_01_cdf ( beta, beta_cdf )
      call normal_01_pdf ( xi, xi_pdf )

      pdf = xi_pdf / beta_cdf / s

      return
      end
      subroutine normal_truncated_b_sample ( mu, s, b, seed, x )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_B_SAMPLE samples the upper truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision B, the upper truncation limit.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision b
      double precision beta
      double precision beta_cdf
      double precision mu
      double precision r8_uniform_01
      double precision s
      integer seed
      double precision u
      double precision x
      double precision xi
      double precision xi_cdf

      beta = ( b - mu ) / s

      call normal_01_cdf ( beta, beta_cdf )

      u = r8_uniform_01 ( seed )
      xi_cdf = u * beta_cdf
      call normal_01_cdf_inv ( xi_cdf, xi )

      x = mu + s * xi

      return
      end
      subroutine normal_truncated_b_variance ( mu, s, b, variance )

c*********************************************************************72
c
cc NORMAL_TRUNCATED_B_VARIANCE: variance of the upper truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision b
      double precision beta
      double precision beta_cdf
      double precision beta_pdf
      double precision mu
      double precision s
      double precision variance

      beta = ( b - mu ) / s

      call normal_01_pdf ( beta, beta_pdf )

      call normal_01_cdf ( beta, beta_cdf )

      variance = s * s * ( 1.0D+00 
     &  - beta * beta_pdf / beta_cdf 
     &  - ( beta_pdf / beta_cdf ) ** 2 )

      return
      end
      subroutine owen_values ( n_data, h, a, t )

c*********************************************************************72
c
cc OWEN_VALUES returns some values of Owen's T function.
c
c  Discussion:
c
c    Owen's T function is useful for computation of the bivariate normal
c    distribution and the distribution of a skewed normal distribution.
c
c    Although it was originally formulated in terms of the bivariate
c    normal function, the function can be defined more directly as
c
c      T(H,A) = 1 / ( 2 * pi ) *
c        Integral ( 0 <= X <= A ) e^(-H^2*(1+X^2)/2) / (1+X^2) dX
c
c    In Mathematica, the function can be evaluated by:
c
c      fx = 1/(2*Pi) * Integrate [ E^(-h^2*(1+x^2)/2)/(1+x^2), {x,0,a} ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mike Patefield, David Tandy,
c    Fast and Accurate Calculation of Owen's T Function,
c    Journal of Statistical Software,
c    Volume 5, Number 5, 2000, pages 1-25.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision H, a parameter.
c
c    Output, double precision A, the upper limit of the integral.
c
c    Output, double precision T, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 28 )

      double precision a
      double precision a_vec(n_max)
      double precision h
      double precision h_vec(n_max)
      integer n_data
      double precision t
      double precision t_vec(n_max)

      save a_vec
      save h_vec
      save t_vec

      data a_vec /
     &  0.2500000000000000D+00,
     &  0.4375000000000000D+00,
     &  0.9687500000000000D+00,
     &  0.0625000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.9999975000000000D+00,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.1000000000000000D+02,
     &  0.1000000000000000D+03 /
      data h_vec /
     &  0.0625000000000000D+00,
     &  6.5000000000000000D+00,
     &  7.0000000000000000D+00,
     &  4.7812500000000000D+00,
     &  2.0000000000000000D+00,
     &  1.0000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2500000000000000D+00,
     &  0.2500000000000000D+00,
     &  0.2500000000000000D+00,
     &  0.2500000000000000D+00,
     &  0.1250000000000000D+00,
     &  0.1250000000000000D+00,
     &  0.1250000000000000D+00,
     &  0.1250000000000000D+00,
     &  0.7812500000000000D-02,
     &  0.7812500000000000D-02,
     &  0.7812500000000000D-02,
     &  0.7812500000000000D-02,
     &  0.7812500000000000D-02,
     &  0.7812500000000000D-02 /
      data t_vec /
     &  3.8911930234701366D-02,
     &  2.0005773048508315D-11,
     &  6.3990627193898685D-13,
     &  1.0632974804687463D-07,
     &  8.6250779855215071D-03,
     &  6.6741808978228592D-02,
     &  0.4306469112078537D-01,
     &  0.6674188216570097D-01,
     &  0.7846818699308410D-01,
     &  0.7929950474887259D-01,
     &  0.6448860284750376D-01,
     &  0.1066710629614485D+00,
     &  0.1415806036539784D+00,
     &  0.1510840430760184D+00,
     &  0.7134663382271778D-01,
     &  0.1201285306350883D+00,
     &  0.1666128410939293D+00,
     &  0.1847501847929859D+00,
     &  0.7317273327500385D-01,
     &  0.1237630544953746D+00,
     &  0.1737438887583106D+00,
     &  0.1951190307092811D+00,
     &  0.7378938035365546D-01,
     &  0.1249951430754052D+00,
     &  0.1761984774738108D+00,
     &  0.1987772386442824D+00,
     &  0.2340886964802671D+00,
     &  0.2479460829231492D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        h = 0.0D+00
        a = 0.0D+00
        t = 0.0D+00
      else
        h = h_vec(n_data)
        a = a_vec(n_data)
        t = t_vec(n_data)
      end if

      return
      end
      subroutine pareto_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc PARETO_CDF evaluates the Pareto CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .lt. a ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - ( a / x )**b
      end if

      return
      end
      subroutine pareto_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc PARETO_CDF_INV inverts the Pareto CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
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
        write ( *, '(a)' ) 'PARETO_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a / ( 1.0D+00 - cdf )**( 1.0D+00 / b )

      return
      end
      function pareto_check ( a, b )

c*********************************************************************72
c
cc PARETO_CHECK checks the parameters of the Pareto CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, logical PARETO_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical pareto_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARETO_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        pareto_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARETO_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        pareto_check = .false.
        return
      end if

      pareto_check = .true.

      return
      end
      subroutine pareto_mean ( a, b, mean )

c*********************************************************************72
c
cc PARETO_MEAN returns the mean of the Pareto PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      if ( b .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARETO_MEAN - Fatal error!'
        write ( *, '(a)' ) '  For B .le. 1, the mean does not exist.'
        mean = 0.0D+00
        return
      end if

      mean = b * a / ( b - 1.0D+00 )

      return
      end
      subroutine pareto_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc PARETO_PDF evaluates the Pareto PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = B * A**B / X**(B+1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .lt. a ) then
        pdf = 0.0D+00
      else
        pdf = b * a**b / x**( b + 1.0D+00 )
      end if

      return
      end
      subroutine pareto_sample ( a, b, seed, x )

c*********************************************************************72
c
cc PARETO_SAMPLE samples the Pareto PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A.
c    0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
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

      call pareto_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine pareto_variance ( a, b, variance )

c*********************************************************************72
c
cc PARETO_VARIANCE returns the variance of the Pareto PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      if ( b .le. 2.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARETO_VARIANCE - Warning!'
        write ( *, '(a)' ) '  For B <= 2, the variance does not exist.'
        variance = 0.0D+00
        return
      end if

      variance = a * a * b / ( ( b - 1.0D+00 )**2 * ( b - 2.0D+00 ) )

      return
      end
      function pearson_05_check ( a, b, c )

c*********************************************************************72
c
cc PEARSON_05_CHECK checks the parameters of the Pearson 5 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, logical PEARSON_05_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical pearson_05_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PEARSON_05_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        pearson_05_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PEARSON_05_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        pearson_05_check = .false.
        return
      end if

      pearson_05_check = .true.

      return
      end
      subroutine pearson_05_mean ( a, b, c, mean )

c*********************************************************************72
c
cc PEARSON_05_MEAN evaluates the mean of the Pearson 5 PDF.
c
c  Discussion:
c
c    The mean is undefined for B .le. 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean

      if ( b .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PEARSON_05_MEAN - Warning!'
        write ( *, '(a)' ) '  MEAN undefined for B .le. 1.'
        mean = c
        return
      end if

      mean = c + a / ( b - 1.0D+00 )

      return
      end
      subroutine pearson_05_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc PEARSON_05_PDF evaluates the Pearson 5 PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = A^B * ( X - C )^(-B-1)
c      * exp ( - A / ( X - C ) ) / Gamma ( B )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    C .lt. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision r8_gamma
      double precision x

      if ( x .le. c ) then
        pdf = 0.0D+00
      else
        pdf = a**b * ( x - c )**( - b - 1.0D+00 )       * exp ( - a / ( 
     &x - c ) ) / r8_gamma ( b )
      end if

      return
      end
      subroutine pearson_05_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc PEARSON_05_SAMPLE samples the Pearson 5 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision b2
      double precision c
      double precision c2
      integer seed
      double precision x
      double precision x2

      a2 = 0.0D+00
      b2 = b
      c2 = 1.0D+00 / a

      call gamma_sample ( a2, b2, c2, seed, x2 )

      x = c + 1.0D+00 / x2

      return
      end
      function planck_check ( a, b )

c*********************************************************************72
c
cc PLANCK_CHECK checks the parameters of the Planck PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 October 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A,
c    0.0D+00 .lt. B.
c
c    Output, logical PLANCK_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical planck_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PLANCK_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        planck_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PLANCK_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        planck_check = .false.
        return
      end if

      planck_check = .true.

      return
      end
      subroutine planck_mean ( a, b, mean )

c*********************************************************************72
c
cc PLANCK_MEAN returns the mean of the Planck PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0 .lt. A, 0.0 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean
      double precision zeta

      mean = ( b + 1.0D+00 ) * zeta ( b + 2.0D+00 ) / zeta ( b + 1.0D+00
     & )

      return
      end
      subroutine planck_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc PLANCK_PDF evaluates the Planck PDF.
c
c  Discussion:
c
c    The Planck PDF has the form
c
c      PDF(A,B;X) = A**(B+1) * X**B / ( exp ( A * X ) - 1 ) / K
c
c    where K is the normalization constant, and has the value
c
c      K = Gamma ( B + 1 ) * Zeta ( B + 1 ).
c
c    The original Planck distribution governed the frequencies in
c    blackbody radiation at a given temperature T, and has the form
c
c      PDF(A;X) = K * X**3 / ( exp ( A * X ) - 1 )
c
c    with
c
c      K = 15 / PI**4.
c
c    Thus, in terms of the Planck PDF, the original Planck distribution
c    has A = 1, B = 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Norman Johnson, Samuel Kotz, Balakrishnan,
c    Continuous Univariate Distributions, second edition,
c    Wiley, 1994,
c    QA273.6.J6
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0 .lt. A, 0.0 .lt. B.
c
c    Input, double precision X, the argument of the PDF.
c    0.0 .le. X
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision k
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_gamma
      double precision x
      double precision zeta

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        k = r8_gamma ( b + 1.0D+00 ) * zeta ( b + 1.0D+00 )
        pdf = a**( b + 1.0D+00 ) * x**b / ( exp ( a * x ) - 1.0D+00 ) / 
     &k
      end if

      return
      end
      subroutine planck_sample ( a, b, seed, x )

c*********************************************************************72
c
cc PLANCK_SAMPLE samples the Planck PDF.
c
c  Discussion:
c
c    The Planck sampling seems to be giving incorrect results.
c    I suspect this has to do with a possible problem in the
c    ZIPF_SAMPLE routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Luc Devroye,
c    Non-Uniform Random Variate Generation,
c    Springer Verlag, 1986, pages 552.
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0 .lt. A, 0.0 .lt. B.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision b2
      double precision c2
      double precision g
      integer seed
      double precision x
      integer z

      a2 = 0.0D+00
      b2 = 1.0D+00
      c2 = b + 1.0D+00

      call gamma_sample ( a2, b2, c2, seed, g )

      call zipf_sample ( c2, seed, z )

      x = g / ( a * dble ( z ) )

      return
      end
      subroutine planck_variance ( a, b, variance )

c*********************************************************************72
c
cc PLANCK_VARIANCE returns the variance of the Planck PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0 .lt. A, 0.0 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean
      double precision variance
      double precision zeta

      call planck_mean ( a, b, mean )

      variance = ( b + 1.0D+00 ) * ( b + 2.0D+00 )     * zeta ( b + 3.0D
     &+00 ) / zeta ( b + 1.0D+00 ) - mean * mean

      return
      end
      subroutine point_distance_1d_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc POINT_DISTANCE_1D_PDF evaluates the point distance PDF in 1D.
c
c  Discussion:
c
c    It is assumed that a set of points has been generated in 1D
c    according to a Poisson process.  The number of points in a region
c    of size LENGTH is a Poisson variate with mean value B * LENGTH.
c
c    For a point chosen at random, we may now find the nearest
c    Poisson point, the second nearest and so on.  We are interested
c    in the PDF that governs the expected behavior of the distances
c    of rank A = 1, 2, 3, ... with Poisson density B.
c
c    Note that this PDF is a form of the Gamma PDF.???
c
c    PDF(A,B;X) = B**A * X**( A - 1 ) * exp ( - B * X ) / ( A - 1 )!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X.
c
c    Input, integer A, indicates the degree of nearness of the
c    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
c    0 .lt. A.
c
c    Input, double precision B, the point density.  0.0 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision i4_factorial
      double precision pdf
      double precision x

      if ( a .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINT_DISTANCE_1D_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter A .lt. 1.'
        stop
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINT_DISTANCE_1D_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter B .le. 0.0.'
        stop
      end if

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = b**a * x**( a - 1 ) * exp ( - b * x ) / i4_factorial ( a -
     & 1 )
      end if

      return
      end
      subroutine point_distance_2d_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc POINT_DISTANCE_2D_PDF evaluates the point distance PDF in 2D.
c
c  Discussion:
c
c    It is assumed that a set of points has been generated in 2D
c    according to a Poisson process.  The number of points in a region
c    of size AREA is a Poisson variate with mean value B * AREA.
c
c    For a point chosen at random, we may now find the nearest
c    Poisson point, the second nearest and so on.  We are interested
c    in the PDF that governs the expected behavior of the distances
c    of rank A = 1, 2, 3, ... with Poisson density B.
c
c    PDF(A,B;X) = 2 * ( B * PI )^A * X^( 2 * A - 1 )
c      * EXP ( - B * PI * X * X ) / ( A - 1 )!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2002
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996, pages 579.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X.
c
c    Input, integer A, indicates the degree of nearness of the
c    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
c    0 .lt. A.
c
c    Input, double precision B, the point density.  0.0 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision i4_factorial
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( a .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINT_DISTANCE_2D_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter A .lt. 1.'
        stop
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINT_DISTANCE_2D_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter B .le. 0.0.'
        stop
      end if

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = 2.0D+00 * ( b * pi )**a * x**( 2 * a - 1 )       * exp ( -
     & b * pi * x * x ) / i4_factorial ( a - 1 )
      end if

      return
      end
      subroutine point_distance_3d_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc POINT_DISTANCE_3D_PDF evaluates the point distance PDF in the 3D.
c
c  Discussion:
c
c    It is assumed that a set of points has been generated in 3D
c    according to a Poisson process.  The number of points in a region
c    of size VOLUME is a Poisson variate with mean value B * VOLUME.
c
c    For a point chosen at random, we may now find the nearest
c    Poisson point, the second nearest and so on.  We are interested
c    in the PDF that governs the expected behavior of the distances
c    of rank A = 1, 2, 3, ... with Poisson density B.
c
c    PDF(A,B;X) = 3 * ( (4/3) * B * PI )^A * X^( 3 * A - 1 )
c      * EXP ( - (4/3) * B * PI * X * X * X ) / ( A - 1 )!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2002
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996, pages 580.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X.
c
c    Input, integer A, indicates the degree of nearness of the
c    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
c    0 .lt. A.
c
c    Input, double precision B, the Poisson point density.  0.0 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      double precision b
      double precision i4_factorial
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( a .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINT_DISTANCE_3D_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter A .lt. 1.'
        stop
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POINT_DISTANCE_3D_PDF - Fatal error!'
        write ( *, '(a)' ) '  Input parameter B .le. 0.0.'
        stop
      end if

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = 3.0D+00 * ( ( 4.0D+00 / 3.0D+00 ) * b * pi )**a       * x*
     &*( 3 * a - 1 ) * exp ( - ( 4.0D+00 / 3.0D+00 ) * b * pi * x**3 )  
     &     / i4_factorial ( a - 1 )
      end if

      return
      end
      subroutine poisson_cdf ( x, a, cdf )

c*********************************************************************72
c
cc POISSON_CDF evaluates the Poisson CDF.
c
c  Discussion:
c
c    CDF(X,A) is the probability that the number of events observed
c    in a unit time period will be no greater than X, given that the
c    expected number of events in a unit time period is A.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the CDF.
c    0 .le. X.
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      integer i
      double precision last
      double precision new
      double precision sum2
      integer x

      if ( x .lt. 0 ) then

        cdf = 0.0D+00

      else

        new = exp ( - a )
        sum2 = new

        do i = 1, x
          last = new
          new = last * a / dble ( i )
          sum2 = sum2 + new
        end do

        cdf = sum2

      end if

      return
      end
      subroutine poisson_cdf_values ( n_data, a, x, fx )

c*********************************************************************72
c
cc POISSON_CDF_VALUES returns some values of the Poisson CDF.
c
c  Discussion:
c
c    CDF(X)(A) is the probability of at most X successes in unit time,
c    given that the expected mean number of successes is A.
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`DiscreteDistributions`]
c      dist = PoissonDistribution [ a ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996,
c    ISBN: 0-8493-2479-3,
c    LC: QA47.M315.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision A, the parameter of the function.
c
c    Output, integer X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 21 )

      double precision a
      double precision a_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      integer x
      integer x_vec(n_max)

      save a_vec
      save fx_vec
      save x_vec

      data a_vec /
     &  0.02D+00,
     &  0.10D+00,
     &  0.10D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  1.00D+00,
     &  2.00D+00,
     &  2.00D+00,
     &  2.00D+00,
     &  2.00D+00,
     &  5.00D+00,
     &  5.00D+00,
     &  5.00D+00,
     &  5.00D+00,
     &  5.00D+00,
     &  5.00D+00,
     &  5.00D+00 /
      data fx_vec /
     &  0.9801986733067553D+00,
     &  0.9048374180359596D+00,
     &  0.9953211598395555D+00,
     &  0.6065306597126334D+00,
     &  0.9097959895689501D+00,
     &  0.9856123220330293D+00,
     &  0.3678794411714423D+00,
     &  0.7357588823428846D+00,
     &  0.9196986029286058D+00,
     &  0.9810118431238462D+00,
     &  0.1353352832366127D+00,
     &  0.4060058497098381D+00,
     &  0.6766764161830635D+00,
     &  0.8571234604985470D+00,
     &  0.6737946999085467D-02,
     &  0.4042768199451280D-01,
     &  0.1246520194830811D+00,
     &  0.2650259152973617D+00,
     &  0.4404932850652124D+00,
     &  0.6159606548330631D+00,
     &  0.7621834629729387D+00 /
      data x_vec /
     &   0, 0, 1, 0,
     &   1, 2, 0, 1,
     &   2, 3, 0, 1,
     &   2, 3, 0, 1,
     &   2, 3, 4, 5,
     &   6 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0.0D+00
        x = 0
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine poisson_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc POISSON_CDF_INV inverts the Poisson CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2002
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, a value of the CDF.
c    0 .le. CDF .lt. 1.
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, integer X, the corresponding argument.
c
      implicit none

      double precision a
      double precision cdf
      integer i
      double precision last
      double precision new
      double precision sum2
      double precision sumold
      integer x
      integer xmax
      parameter ( xmax = 100 )

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POISSON_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if
c
c  Now simply start at X = 0, and find the first value for which
c  CDF(X-1) .le. CDF .le. CDF(X).
c
      sum2 = 0.0D+00

      do i = 0, xmax

        sumold = sum2

        if ( i .eq. 0 ) then
          new = exp ( - a )
          sum2 = new
        else
          last = new
          new = last * a / dble ( i )
          sum2 = sum2 + new
        end if

        if ( sumold .le. cdf .and. cdf .le. sum2 ) then
          x = i
          return
        end if

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POISSON_CDF_INV - Warning!'
      write ( *, '(a,i8)' ) '  Exceeded XMAX = ', xmax

      x = xmax

      return
      end
      function poisson_check ( a )

c*********************************************************************72
c
cc POISSON_CHECK checks the parameter of the Poisson PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, logical POISSON_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical poisson_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POISSON_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        poisson_check = .false.
        return
      end if

      poisson_check = .true.

      return
      end
      subroutine poisson_mean ( a, mean )

c*********************************************************************72
c
cc POISSON_MEAN returns the mean of the Poisson PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision mean

      mean = a

      return
      end
      subroutine poisson_kernel ( r, n, c, x, y, p )

c*********************************************************************72
c
cc POISSON_KERNEL evaluates the Poisson kernel.
c
c  Discussion:
c
c    P(X,Y) = ( R^2 - |X-C|^2 ) / ( R * A * |X-Y|^N )
c
c    where the N-dimensional ball has radius R and center C,
c    and A is the area of the unit sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the radius of the ball.
c
c    Input, integer N, the spatial dimension.
c
c    Input, double precision C(N), the center of the ball.
c
c    Input, double precision X(N), a point inside the ball.
c
c    Input, double precision Y(N), a point on the surface of the ball.
c
c    Output, double precision P, the Poisson kernel function P(X,Y).
c
      implicit none

      integer n

      double precision area
      double precision b
      double precision c(n)
      double precision p
      double precision r
      double precision r8vec_diff_norm
      double precision sphere_unit_area_nd
      double precision t
      double precision x(n)
      double precision xc_diff_norm
      double precision xy_diff_norm
      double precision y(n)

      xc_diff_norm = r8vec_diff_norm ( n, x, c )
      xy_diff_norm = r8vec_diff_norm ( n, x, y )
      area = sphere_unit_area_nd ( n )

      t = ( r + xc_diff_norm ) * ( r - xc_diff_norm )
      b = r * area * ( xy_diff_norm ) ** n
      p = t / b

      return
      end
      subroutine poisson_pdf ( x, a, pdf )

c*********************************************************************72
c
cc POISSON_PDF evaluates the Poisson PDF.
c
c  Discussion:
c
c    PDF(A;X) = EXP ( - A ) * A**X / X!
c
c    PDF(A;X) is the probability that the number of events observed
c    in a unit time period will be X, given the expected number
c    of events in a unit time.
c
c    The parameter A is the expected number of events per unit time.
c
c    The Poisson PDF is a discrete version of the Exponential PDF.
c
c    The time interval between two Poisson events is a random
c    variable with the Exponential PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c    0 .le. X
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision i4_factorial
      double precision pdf
      integer x

      if ( x .lt. 0 ) then
        pdf = 0.0D+00
      else
        pdf = exp ( - a ) * a**x / i4_factorial ( x )
      end if

      return
      end
      subroutine poisson_sample ( a, seed, x )

c*********************************************************************72
c
cc POISSON_SAMPLE samples the Poisson PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call poisson_cdf_inv ( cdf, a, x )

      return
      end
      subroutine poisson_variance ( a, variance )

c*********************************************************************72
c
cc POISSON_VARIANCE returns the variance of the Poisson PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision variance

      variance = a

      return
      end
      subroutine power_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc POWER_CDF evaluates the Power CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B,
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .le. 0.0D+00 ) then
        cdf = 0.0D+00
      else if ( x .le. b ) then
        cdf = ( x / b )**a
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine power_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc POWER_CDF_INV inverts the Power CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, double precision X, the argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = 0.0D+00
      else if ( cdf .lt. 1.0D+00 ) then
        x = b * exp ( log ( cdf ) / a )
      else
        x = b
      end if

      return
      end
      function power_check ( a, b )

c*********************************************************************72
c
cc POWER_CHECK checks the parameter of the Power PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, logical POWER_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical power_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        power_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        power_check = .false.
        return
      end if

      power_check = .true.

      return
      end
      subroutine power_mean ( a, b, mean )

c*********************************************************************72
c
cc POWER_MEAN returns the mean of the Power PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a * b / ( a + 1.0D+00 )

      return
      end
      subroutine power_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc POWER_PDF evaluates the Power PDF.
c
c  Discussion:
c
c    PDF(A;X) = (A/B) * (X/B)^(A-1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Daniel Zwillinger, Stephen Kokoska,
c    CRC Standard Probability and Statistics Tables and Formulae,
c    Chapman and Hall/CRC, 2000, pages 152-153.
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X .le. B.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .lt. 0.0D+00 .or. b .lt. x ) then
        pdf = 0.0D+00
      else
        pdf = ( a / b ) * ( x / b )**( a - 1.0D+00 )
      end if

      return
      end
      subroutine power_sample ( a, b, seed, x )

c*********************************************************************72
c
cc POWER_SAMPLE samples the Power PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
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

      call power_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine power_variance ( a, b, variance )

c*********************************************************************72
c
cc POWER_VARIANCE returns the variance of the Power PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A, 0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = b * b * a / ( ( a + 1.0D+00 )**2 * ( a + 2.0D+00 ) )

      return
      end
      subroutine psi_values ( n_data, x, fx )

c*********************************************************************72
c
cc PSI_VALUES returns some values of the Psi or Digamma function for testing.
c
c  Discussion:
c
c    PSI(X) = d LN ( GAMMA ( X ) ) / d X = GAMMA'(X) / GAMMA(X)
c
c    PSI(1) = - Euler's constant.
c
c    PSI(X+1) = PSI(X) + 1 / X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fxvec ( n_max )
      integer n_data
      double precision x
      double precision xvec ( n_max )

      data fxvec /
     & -10.42375494041108D+00, 
     &  -5.289039896592188D+00, 
     &  -3.502524222200133D+00, 
     &  -2.561384544585116D+00, 
     &  -1.963510026021423D+00, 
     &  -1.540619213893190D+00, 
     &  -1.220023553697935D+00, 
     &  -0.9650085667061385D+00, 
     &  -0.7549269499470514D+00, 
     &  -0.5772156649015329D+00,
     &  -0.4237549404110768D+00,
     &  -0.2890398965921883D+00,
     &  -0.1691908888667997D+00,
     &  -0.6138454458511615D-01,
     &   0.3648997397857652D-01,
     &   0.1260474527734763D+00,
     &   0.2085478748734940D+00,
     &   0.2849914332938615D+00,
     &   0.3561841611640597D+00,
     &   0.4227843350984671D+00 /

      data xvec /
     &  0.1D+00,
     &  0.2D+00,
     &  0.3D+00,
     &  0.4D+00,
     &  0.5D+00,
     &  0.6D+00,
     &  0.7D+00,
     &  0.8D+00,
     &  0.9D+00,
     &  1.0D+00,
     &  1.1D+00,
     &  1.2D+00,
     &  1.3D+00,
     &  1.4D+00,
     &  1.5D+00,
     &  1.6D+00,
     &  1.7D+00,
     &  1.8D+00,
     &  1.9D+00,
     &  2.0D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        x = xvec(n_data)
        fx = fxvec(n_data)
      end if

      return
      end
      subroutine quasigeometric_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc QUASIGEOMETRIC_CDF evaluates the Quasigeometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the maximum number of trials.
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      integer x

      if ( x .lt. 0 ) then
        cdf = 0.0D+00
      else if ( x .eq. 0 ) then
        cdf = a
      else if ( b .eq. 0.0D+00 ) then
        cdf = 1.0D+00
      else
        cdf = a + ( 1.0D+00 - a ) * ( 1.0D+00 - b**x )
      end if

      return
      end
      subroutine quasigeometric_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc QUASIGEOMETRIC_CDF_INV inverts the Quasigeometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0D+00
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Output, integer X, the corresponding value of X.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      integer x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUASIGEOMETRIC_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .lt. a ) then
        x = 0
      else if ( b .eq. 0.0D+00 ) then
        x = 1
      else
        x = 1 + int ( ( log ( 1.0D+00 - cdf ) - log ( 1.0D+00 - a ) ) / 
     &log ( b ) )
      end if

      return
      end
      function quasigeometric_check ( a, b )

c*********************************************************************72
c
cc QUASIGEOMETRIC_CHECK checks the parameters of the Quasigeometric CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Output, logical QUASIGEOMETRIC_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical              quasigeometric_check

      if ( a .lt. 0.0D+00 .or. 1.0D+00 .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUASIGEOMETRIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 0 or 1 .lt. A.'
        quasigeometric_check = .false.
        return
      end if

      if ( b .lt. 0.0D+00 .or. 1.0D+00 .le. b ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUASIGEOMETRIC_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. 0 or 1 .le. B.'
        quasigeometric_check = .false.
        return
      end if

      quasigeometric_check = .true.

      return
      end
      subroutine quasigeometric_mean ( a, b, mean )

c*********************************************************************72
c
cc QUASIGEOMETRIC_MEAN returns the mean of the Quasigeometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = ( 1.0D+00 - a  ) / ( 1.0D+00 - b )

      return
      end
      subroutine quasigeometric_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc QUASIGEOMETRIC_PDF evaluates the Quasigeometric PDF.
c
c  Discussion:
c
c    PDF(A,B;X) =    A                     if 0  = X;
c               = (1-A) * (1-B) * B^(X-1)  if 1 .le. X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Darren Glass, Philip Lowry,
c    Quasiquasigeometric Distributions and Extra Inning Baseball Games,
c    Mathematics Magazine,
c    Volume 81, Number 2, April 2008, pages 127-137.
c
c    Paul Nahin,
c    Digital Dice: Computational Solutions to Practical Probability Problems,
c    Princeton University Press, 2008,
c    ISBN13: 978-0-691-12698-2,
c    LC: QA273.25.N34.
c
c  Parameters:
c
c    Input, integer X, the independent variable.
c    0 .le. X
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      integer x

      if ( x .lt. 0 ) then

        pdf = 0.0D+00

      else if ( x .eq. 0 ) then

        pdf = a

      else if ( b .eq. 0.0D+00 ) then

        if ( x .eq. 1 ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if

      else

        pdf = ( 1.0D+00 - a ) * ( 1.0D+00 - b ) * b**( x - 1 )

      end if

      return
      end
      subroutine quasigeometric_sample ( a, b, seed, x )

c*********************************************************************72
c
cc QUASIGEOMETRIC_SAMPLE samples the Quasigeometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call quasigeometric_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine quasigeometric_variance ( a, b, variance )

c*********************************************************************72
c
cc QUASIGEOMETRIC_VARIANCE returns the variance of the Quasigeometric PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the probability of 0 successes.
c    0.0 .le. A .le. 1.0.
c
c    Input, double precision B, the depreciation constant.
c    0.0 .le. B .lt. 1.0.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = ( 1.0D+00 - a ) * ( a + b ) / ( 1.0D+00 - b ) / ( 1.0D+
     &00 - b )

      return
      end
      function r4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc R4_UNIFORM_AB returns a scaled real pseudorandom number.
c
c  Discussion:
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, real R4_UNIFORM_AB, a number strictly between A and B.
c
      implicit none

      real a
      real  b
      integer i4_huge
      integer k
      real r4_uniform_ab
      integer seed

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge ( )
      end if

      r4_uniform_ab = a + ( b - a ) * real ( seed ) * 4.656612875E-10

      return
      end
      function r4_uniform_01 ( seed )

c*********************************************************************72
c
cc R4_UNIFORM_01 returns a unit real pseudorandom number.
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r4_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R4_UNIFORM_01
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
c    Peter Lewis, Allen Goodman, James Miller
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, real R4_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      integer i4_huge
      integer k
      integer seed
      real r4_uniform_01

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge ( )
      end if

      r4_uniform_01 = real ( seed ) * 4.656612875E-10

      return
      end
      function r8_ceiling ( r )

c*********************************************************************72
c
cc R8_CEILING rounds an R8 "up" to the nearest integral R8.
c
c  Example:
c
c     R     Value
c
c    -1.1  -1.0
c    -1.0  -1.0
c    -0.9   0.0
c     0.0   0.0
c     5.0   5.0
c     5.1   6.0
c     5.9   6.0
c     6.0   6.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the value to be rounded up.
c
c    Output, double precision R8_CEILING, the rounded value.
c
      implicit none

      double precision r
      double precision r8_ceiling
      double precision value

      value = dble ( int ( r ) )
      if ( value .lt. r ) then
        value = value + 1.0D+00
      end if

      r8_ceiling = value

      return
      end
      function r8_csc ( theta )

c*********************************************************************72
c
cc R8_CSC returns the cosecant of X.
c
c  Discussion:
c
c    R8_CSC ( THETA ) = 1.0 / SIN ( THETA )
c
c    The cosecant is not a built-in function in FORTRAN, and occasionally it
c    is handier, or more concise, to be able to refer to it directly
c    rather than through its definition in terms of the sine function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision THETA, the angle, in radians, whose
c    cosecant is desired.  It must be the case that SIN ( THETA ) is not zero.
c
c    Output, double precision R8_CSC, the cosecant of THETA.
c
      implicit none

      double precision r8_csc
      double precision theta
      double precision value

      value = sin ( theta )

      if ( value .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_CSC - Fatal error!'
        write ( *, '(a,g14.6)' ) 
     &    '  Cosecant undefined for THETA = ', theta
        stop
      end if

      r8_csc = 1.0D+00 / value

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
      function r8_gamma ( x )

c*********************************************************************72
c
cc R8_GAMMA evaluates Gamma(X) for a real argument.
c
c  Discussion:
c
c    This routine calculates the gamma function for a real argument X.
c    Computation is based on an algorithm outlined in reference 1.
c    The program uses rational functions that approximate the gamma
c    function to at least 20 significant decimal digits.  Coefficients
c    for the approximation over the interval (1,2) are unpublished.
c    Those for the approximation for 12 <= X are from reference 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 January 2008
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody,
c    An Overview of Software Development for Special Functions,
c    in Numerical Analysis Dundee, 1975,
c    edited by GA Watson,
c    Lecture Notes in Mathematics 506,
c    Springer, 1976.
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
c    Output, double precision R8_GAMMA, the value of the function.
c
      implicit none

      double precision c(7)
      double precision eps
      double precision fact
      integer i
      integer n
      double precision p(8)
      logical parity
      double precision pi
      double precision q(8)
      double precision r8_gamma
      double precision res
      double precision sqrtpi
      double precision sum
      double precision x
      double precision xbig
      double precision xden
      double precision xinf
      double precision xminin
      double precision xnum
      double precision y
      double precision y1
      double precision ysq
      double precision z
c
c  Mathematical constants
c
      data sqrtpi / 0.9189385332046727417803297D+00 /
      data pi / 3.1415926535897932384626434D+00 /
c
c  Machine dependent parameters
c
      data xbig / 171.624D+00 /
      data xminin / 2.23D-308 /
      data eps / 2.22D-16 /
      data xinf /1.79D+308 /
c
c  Numerator and denominator coefficients for rational minimax
c  approximation over (1,2).
c
      data p /
     & -1.71618513886549492533811d+00,
     &  2.47656508055759199108314d+01,
     & -3.79804256470945635097577d+02,
     &  6.29331155312818442661052d+02,
     &  8.66966202790413211295064d+02,
     & -3.14512729688483675254357d+04,
     & -3.61444134186911729807069d+04,
     &  6.64561438202405440627855d+04 /

      data q /
     & -3.08402300119738975254353d+01,
     &  3.15350626979604161529144d+02,
     & -1.01515636749021914166146d+03,
     & -3.10777167157231109440444d+03,
     &  2.25381184209801510330112d+04,
     &  4.75584627752788110767815d+03,
     & -1.34659959864969306392456d+05,
     & -1.15132259675553483497211d+05 /
c
c  Coefficients for minimax approximation over (12, INF).
c
      data c /
     & -1.910444077728D-03,
     &  8.4171387781295D-04,
     & -5.952379913043012D-04,
     &  7.93650793500350248D-04,
     & -2.777777777777681622553D-03,
     &  8.333333333333333331554247D-02,
     &  5.7083835261D-03 /

      parity = .false.
      fact = 1.0D+00
      n = 0
      y = x
c
c  Argument is negative.
c
      if ( y .le. 0.0D+00 ) then

        y = - x
        y1 = aint ( y )
        res = y - y1

        if ( res .ne. 0.0D+00 ) then

          if ( y1 .ne. aint ( y1 * 0.5D+00 ) * 2.0D+00 ) then
            parity = .true.
          end if

          fact = - pi / sin ( pi * res )
          y = y + 1.0D+00

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Argument is positive.
c
      if ( y .lt. eps ) then
c
c  Argument < EPS.
c
        if ( xminin .le. y ) then
          res = 1.0D+00 / y
        else
          res = xinf
          r8_gamma = res
          return
        end if

      else if ( y .lt. 12.0D+00 ) then

        y1 = y
c
c  0.0 < argument < 1.0.
c
        if ( y .lt. 1.0D+00 ) then

          z = y
          y = y + 1.0D+00
c
c  1.0 < argument < 12.0.
c  Reduce argument if necessary.
c
        else

          n = int ( y ) - 1
          y = y - dble ( n )
          z = y - 1.0D+00

        end if
c
c  Evaluate approximation for 1.0 < argument < 2.0.
c
        xnum = 0.0D+00
        xden = 1.0D+00
        do i = 1, 8
          xnum = ( xnum + p(i) ) * z
          xden = xden * z + q(i)
        end do

        res = xnum / xden + 1.0D+00
c
c  Adjust result for case  0.0 < argument < 1.0.
c
        if ( y1 .lt. y ) then

          res = res / y1
c
c  Adjust result for case 2.0 < argument < 12.0.
c
        else if ( y .lt. y1 ) then

          do i = 1, n
            res = res * y
            y = y + 1.0D+00
          end do

        end if

      else
c
c  Evaluate for 12.0 <= argument.
c
        if ( y .le. xbig ) then

          ysq = y * y
          sum = c(7)
          do i = 1, 6
            sum = sum / ysq + c(i)
          end do
          sum = sum / y - y + sqrtpi
          sum = sum + ( y - 0.5D+00 ) * log ( y )
          res = exp ( sum )

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Final adjustments and return.
c
      if ( parity ) then
        res = - res
      end if

      if ( fact .ne. 1.0D+00 ) then
        res = fact / res
      end if

      r8_gamma = res

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
      function r8_is_int ( r )

c*********************************************************************72
c
cc R8_IS_INT determines if an R8 represents an integer value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision R, the number to be checked.
c
c    Output, logical R8_IS_INT, is TRUE if R is an integer value.
c
      implicit none

      integer i
      integer i4_huge
      double precision r
      logical r8_is_int

      if ( dble ( i4_huge ( ) ) .lt. r ) then
        r8_is_int = .false.
      else if ( r .lt. - dble ( i4_huge ( ) ) ) then
        r8_is_int = .false.
      else if ( r .eq. dble ( int ( r ) ) ) then
        r8_is_int = .true.
      else
        r8_is_int = .false.
      end if

      return
      end
      function r8_pi ( )

c*********************************************************************72
c
cc R8_PI returns the value of pi to 16 decimal places.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_PI, the value of pi.
c
      implicit none

      double precision r8_pi

      r8_pi = 3.141592653589793D+00

      return
      end
      function r8_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc R8_UNIFORM_AB returns a scaled pseudorandom R8.
c
c  Discussion:
c
c    An R8 is a double precision value.
c
c    For now, the input quantity SEED is an integer variable.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should
c    NOT be 0.  On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_AB, a number strictly between A and B.
c
      implicit none

      double precision a
      double precision b
      integer i4_huge
      integer k
      double precision r8_uniform_ab
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge ( )
      end if

      r8_uniform_ab = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a unit pseudorandom R8.
c
c  Discussion:
c
c    An R8 is a double precision value.
c
c    For now, the input quantity SEED is an integer variable.
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
c    05 July 2006
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
c    Peter Lewis, Allen Goodman, James Miller
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which should
c    NOT be 0. On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      integer i4_huge
      integer k
      double precision r8_uniform_01
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
        seed = seed + i4_huge ( )
      end if
c
c  Although SEED can be represented exactly as a 32 bit integer,
c  it generally cannot be represented exactly as a 32 bit real number!
c
      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      function r8poly_value ( n, a, x )

c*********************************************************************72
c
cc R8POLY_VALUE evaluates an R8POLY
c
c  Discussion:
c
c    For sanity's sake, the value of N indicates the NUMBER of
c    coefficients, or more precisely, the ORDER of the polynomial,
c    rather than the DEGREE of the polynomial.  The two quantities
c    differ by 1, but cause a great deal of confusion.
c
c    Given N and A, the form of the polynomial is:
c
c      p(x) = a(1) + a(2) * x + ... + a(n-1) * x^(n-2) + a(n) * x^(n-1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Input, double precision A(N), the coefficients of the polynomial.
c    A(1) is the constant term.
c
c    Input, double precision X, the point at which the polynomial is
c    to be evaluated.
c
c    Output, double precision R8POLY_VALUE, the value of the polynomial at X.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8poly_value
      double precision x

      r8poly_value = a(n)
      do i = n - 1, 1, -1
        r8poly_value = r8poly_value * x + a(i)
      end do

      return
      end
      subroutine r8row_max ( m, n, a, amax )

c*********************************************************************72
c
cc R8ROW_MAX returns the maximums of an R8ROW.
c
c  Discussion:
c
c    An R8ROW is an M by N array of R8 values, regarded
c    as an array of M rows of length N.
c
c  Example:
c
c    A =
c      1  2  3
c      2  6  7
c
c    MAX =
c      3
c      7
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the array.
c
c    Input, double precision A(M,N), the array to be examined.
c
c    Output, double precision AMAX(M), the maximums of the rows.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision amax(m)
      integer i
      integer j

      do i = 1, m

        amax(i) = a(i,1)
        do j = 2, n
          if ( amax(i) .lt. a(i,j) ) then
            amax(i) = a(i,j)
          end if
        end do

      end do

      return
      end
      subroutine r8row_mean ( m, n, a, mean )

c*********************************************************************72
c
cc R8ROW_MEAN returns the means of an R8ROW.
c
c  Discussion:
c
c    An R8ROW is an M by N array of R8 values, regarded
c    as an array of M rows of length N.
c
c  Example:
c
c    A =
c      1  2  3
c      2  6  7
c
c    MEAN =
c      2
c      5
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), the array to be examined.
c
c    Output, double precision MEAN(M), the means, or averages, of the rows.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision mean(m)

      do i = 1, m
        mean(i) = 0.0D+00
        do j = 1, n
          mean(i) = mean(i) + a(i,j)
        end do
        mean(i) = mean(i) / dble ( n )
      end do

      return
      end
      subroutine r8row_min ( m, n, a, amin )

c*********************************************************************72
c
cc R8ROW_MIN returns the minimums of an R8ROW.
c
c  Discussion:
c
c    An R8ROW is an M by N array of R8 values, regarded
c    as an array of M rows of length N.
c
c  Example:
c
c    A =
c      1  2  3
c      2  6  7
c
c    MIN =
c      1
c      2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the array.
c
c    Input, double precision A(M,N), the array to be examined.
c
c    Output, double precision AMIN(M), the minimums of the rows.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision amin(m)
      integer i
      integer j

      do i = 1, m

        amin(i) = a(i,1)
        do j = 2, n
          if ( a(i,j) .lt. amin(i) ) then
            amin(i) = a(i,j)
          end if
        end do

      end do

      return
      end
      subroutine r8row_variance ( m, n, a, variance )

c*********************************************************************72
c
cc R8ROW_VARIANCE returns the variances of an R8ROW.
c
c  Discussion:
c
c    An R8ROW is an M by N array of R8 values, regarded
c    as an array of M rows of length N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the array.
c
c    Input, double precision A(M,N), the array whose variances are desired.
c
c    Output, double precision VARIANCE(M), the variances of the rows.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision mean
      double precision variance(m)

      do i = 1, m

        mean = 0.0D+00
        do j = 1, n
          mean = mean + a(i,j)
        end do
        mean = mean / dble ( n )

        variance(i) = 0.0D+00
        do j = 1, n
          variance(i) = variance(i) + ( a(i,j) - mean )**2
        end do

        if ( 1 .lt. n ) then
          variance(i) = variance(i) / dble ( n - 1 )
        else
          variance(i) = 0.0D+00
        end if

      end do

      return
      end
      subroutine r8vec_circular_variance ( n, x, circular_variance )

c*********************************************************************72
c
cc R8VEC_CIRCULAR_VARIANCE returns the circular variance of an R8VEC.
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
c    29 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision X(N), the vector whose variance is desired.
c
c    Output, double precision CIRCULAR VARIANCE, the circular variance
c    of the vector entries.
c
      implicit none

      integer n

      double precision circular_variance
      double precision csum
      integer i
      double precision mean
      double precision ssum
      double precision x(n)

      call r8vec_mean ( n, x, mean )

      csum = 0.0D+00
      do i = 1, n
        csum = csum + cos ( x(i) - mean )
      end do

      ssum = 0.0D+00
      do i = 1, n
        ssum = ssum + sin ( x(i) - mean )
      end do

      circular_variance = csum * csum + ssum * ssum

      circular_variance = sqrt ( circular_variance ) / dble ( n )

      circular_variance = 1.0D+00 - circular_variance

      return
      end
      function r8vec_diff_norm ( n, a, b )

c*********************************************************************72
c
cc R8VEC_DIFF_NORM returns the L2 norm of the difference of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), B(N), the vectors.
c
c    Output, double precision R8VEC_DIFF_NORM, the L2 norm of A - B.
c
      implicit none

      integer n

      double precision a(n)
      double precision b(n)
      integer i
      double precision r8vec_diff_norm
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + ( a(i) - b(i) )**2
      end do
      value = sqrt ( value )

      r8vec_diff_norm = value

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      subroutine r8vec_max ( n, a, amax )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision AMAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      double precision amax
      integer i

      amax = a(1)
      do i = 2, n
        amax = max ( amax, a(i) )
      end do

      return
      end
      subroutine r8vec_mean ( n, x, mean )

c*********************************************************************72
c
cc R8VEC_MEAN returns the mean of an R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision X(N), the vector whose mean is desired.
c
c    Output, double precision MEAN, the mean, or average,
c    of the vector entries.
c
      implicit none

      integer n

      integer i
      double precision mean
      double precision x(n)

      mean = 0.0D+00
      do i = 1, n
        mean = mean + x(i)
      end do
      mean = mean / dble ( n )

      return
      end
      subroutine r8vec_min ( n, a, amin )

c*********************************************************************72
c
cc R8VEC_MIN returns the minimum value in an R8VEC.
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
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision AMIN, the value of the smallest entry.
c
      implicit none

      integer n

      double precision a(n)
      double precision amin
      integer i

      amin = a(1)
      do i = 2, n
        amin = min ( amin, a(i) )
      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 December 1999
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
c    Input, character ( len = * ) TITLE, a title to be printed first.
c    TITLE may be blank.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      if ( title .ne. ' ' ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) trim ( title )
      end if

      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(i8,g14.6)' ) i, a(i)
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
      subroutine r8vec_uniform_ab ( n, a, b, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of double precision values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 July 2006
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
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer M, the number of entries in the vector.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge ( )
        end if

        r(i) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of double precision values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 July 2006
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
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer i4_huge
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge ( )
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine r8vec_unit_sum ( n, a )

c*********************************************************************72
c
cc R8VEC_UNIT_SUM normalizes an R8VEC to have unit sum.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, real A(N), the vector to be normalized.  On output,
c    the entries of A should have unit sum.  However, if the input vector
c    has zero sum, the routine halts.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_sum
      integer i

      a_sum = 0.0D+00
      do i = 1, n
        a_sum = a_sum + a(i)
      end do

      if ( a_sum .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIT_SUM - Fatal error!'
        write ( *, '(a)' ) '  The vector entries sum to 0.'
        stop
      end if

      do i = 1, n
        a(i) = a(i) / a_sum
      end do

      return
      end
      subroutine r8vec_variance ( n, x, variance )

c*********************************************************************72
c
cc R8VEC_VARIANCE returns the variance of an R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision X(N), the vector whose variance is desired.
c
c    Output, double precision VARIANCE, the variance of the vector entries.
c
      implicit none

      integer n

      integer i
      double precision mean
      double precision variance
      double precision x(n)

      call r8vec_mean ( n, x, mean )

      variance = 0.0D+00
      do i = 1, n
        variance = variance + ( x(i) - mean )**2
      end do

      if ( 1 .lt. n ) then
        variance = variance / dble ( n - 1 )
      else
        variance = 0.0D+00
      end if

      return
      end
      subroutine rayleigh_cdf ( x, a, cdf )

c*********************************************************************72
c
cc RAYLEIGH_CDF evaluates the Rayleigh CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    0.0D+00 .le. X.
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision x

      if ( x .lt. 0.0D+00 ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - exp ( - x**2 / ( 2.0D+00 * a**2 ) )
      end if

      return
      end
      subroutine rayleigh_cdf_inv ( cdf, a, x )

c*********************************************************************72
c
cc RAYLEIGH_CDF_INV inverts the Rayleigh CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RAYLEIGH_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = sqrt ( - 2.0D+00 * a * a * log ( 1.0D+00 - cdf ) )

      return
      end
      subroutine rayleigh_cdf_values ( n_data, sigma, x, fx )

c*********************************************************************72
c
cc RAYLEIGH_CDF_VALUES returns some values of the Rayleigh CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = RayleighDistribution [ sigma ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision SIGMA, the shape parameter of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision sigma
      double precision sigma_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save sigma_vec
      save x_vec

      data fx_vec /
     &  0.8646647167633873D+00,
     &  0.9996645373720975D+00,
     &  0.9999999847700203D+00,
     &  0.999999999999987D+00,
     &  0.8646647167633873D+00,
     &  0.3934693402873666D+00,
     &  0.1992625970831920D+00,
     &  0.1175030974154046D+00,
     &  0.7688365361336422D-01 /
      data sigma_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        sigma = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        sigma = sigma_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function rayleigh_check ( a )

c*********************************************************************72
c
cc RAYLEIGH_CHECK checks the parameter of the Rayleigh PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, logical RAYLEIGH_CHECK, is true if the parameter is legal.
c
      implicit none

      double precision a
      logical rayleigh_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RAYLEIGH_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.'
        rayleigh_check = .false.
        return
      end if

      rayleigh_check = .true.

      return
      end
      subroutine rayleigh_mean ( a, mean )

c*********************************************************************72
c
cc RAYLEIGH_MEAN returns the mean of the Rayleigh PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision mean
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      mean = a * sqrt ( 0.5D+00 * pi )

      return
      end
      subroutine rayleigh_pdf ( x, a, pdf )

c*********************************************************************72
c
cc RAYLEIGH_PDF evaluates the Rayleigh PDF.
c
c  Discussion:
c
c    PDF(A;X) = ( X / A^2 ) * EXP ( - X^2 / ( 2 * A^2 ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X
c
c    Input, double precision A, the parameter of the PDF.
c    0 .lt. A.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision pdf
      double precision x

      if ( x .lt. 0.0D+00 ) then
        pdf = 0.0D+00
      else
        pdf = ( x / a**2 ) * exp ( - x**2 / ( 2.0D+00 * a**2 ) )
      end if

      return
      end
      subroutine rayleigh_sample ( a, seed, x )

c*********************************************************************72
c
cc RAYLEIGH_SAMPLE samples the Rayleigh PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    0.0D+00 .lt. A.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call rayleigh_cdf_inv ( cdf, a, x )

      return
      end
      subroutine rayleigh_variance ( a, variance )

c*********************************************************************72
c
cc RAYLEIGH_VARIANCE returns the variance of the Rayleigh PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameters of the PDF.
c    0.0D+00 .lt. A.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = 2.0D+00 * a**2 * ( 1.0D+00 - 0.25D+00 * pi )

      return
      end
      subroutine reciprocal_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc RECIPROCAL_CDF evaluates the Reciprocal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .le. 0.0D+00 ) then

        cdf = 0.0D+00

      else if ( 0.0D+00 .lt. x ) then

        cdf = log ( a / x ) / log ( a / b )

      end if

      return
      end
      subroutine reciprocal_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc RECIPROCAL_CDF_INV inverts the Reciprocal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RECIPROCAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = 0.0D+00
      else if ( 0.0D+00 .lt. cdf ) then
        x = b**cdf / a**( cdf - 1.0D+00 )
      end if

      return
      end
      function reciprocal_check ( a, b )

c*********************************************************************72
c
cc RECIPROCAL_CHECK checks the parameters of the Reciprocal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
c
c    Output, logical RECIPROCAL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical reciprocal_check

      if ( a .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RECIPROCAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 0.0'
        reciprocal_check = .false.
        return
      end if

      if ( b .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RECIPROCAL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. A'
        reciprocal_check = .false.
        return
      end if

      reciprocal_check = .true.

      return
      end
      subroutine reciprocal_mean ( a, b, mean )

c*********************************************************************72
c
cc RECIPROCAL_MEAN returns the mean of the Reciprocal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = ( a - b ) / log ( a / b )

      return
      end
      subroutine reciprocal_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc RECIPROCAL_PDF evaluates the Reciprocal PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = 1.0D+00 / ( X * LOG ( B / A ) )
c    for 0.0D+00 .le. X
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .le. 0.0D+00 ) then
        pdf = 0.0D+00
      else if ( 0.0D+00 .lt. x ) then
        pdf = 1.0D+00 / ( x * log ( b / a ) )
      end if

      return
      end
      subroutine reciprocal_sample ( a, b, seed, x )

c*********************************************************************72
c
cc RECIPROCAL_SAMPLE samples the Reciprocal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
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

      x = b**cdf / a**( cdf - 1.0D+00 )

      return
      end
      subroutine reciprocal_variance ( a, b, variance )

c*********************************************************************72
c
cc RECIPROCAL_VARIANCE returns the variance of the Reciprocal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. A .le. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision d
      double precision variance

      d = log ( a / b )

      variance = ( a - b )* ( a * ( d - 2.0D+00 )     + b * ( d + 2.0D+0
     &0 ) ) / ( 2.0D+00 * d**2 )

      return
      end
      subroutine ribesl ( x, alpha, nb, ize, b, ncalc )

c*********************************************************************72
c
cc RIBESL calculates I Bessel function with non-integer orders.
c
c  Discussion:
c
c    This routine calculates Bessel functions I SUB(N+ALPHA) (X)
c    for non-negative argument X, and non-negative order N+ALPHA,
c    with or without exponential scaling.
c
c    This program is based on a program written by David
c    Sookne that computes values of the Bessel functions J or
c    I of real argument and integer order.  Modifications include
c    the restriction of the computation to the I Bessel function
c    of non-negative real argument, the extension of the computation
c    to arbitrary positive order, the inclusion of optional
c    exponential scaling, and the elimination of most underflow.
c
c    In case of an error, NCALC will not equal NB, and not all I's are
c    calculated to the desired accuracy.
c
c    If NCALC .lt. 0:  An argument is out of range. For example,
c    NB .le. 0, IZE is not 1 or 2, or IZE = 1 and EXPARG .le. ABS(X)
c    In this case, the B-vector is not calculated, and NCALC is
c    set to MIN(NB,0)-1 so that NCALC /= NB.
c
c    If 0 .lt. NCALC .lt. NB, then not all requested function values could
c    be calculated accurately.  This usually occurs because NB is
c    much larger than ABS(X).  In this case, B(N) is calculated
c    to the desired accuracy for N .le. NCALC, but precision
c    is lost for NCALC .lt. N .le. NB.  If B(N) does not vanish
c    for NCALC .lt. N (because it is too small to be represented),
c    and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
c    significant figures of B(N) can be trusted.
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
c    Original FORTRAN77 version by William Cody.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Frank Olver, David Sookne,
c    A Note on Backward Recurrence Algorithms,
c    Mathematics of Computation,
c    Volume 26, 1972, pages 941-947.
c
c    David Sookne,
c    Bessel Functions of Real Argument and Integer Order,
c    NBS Journal of Research B,
c    Volume 77B, 1973, pages 125-132.
c
c    William Cody,
c    Algorithm 597:
c    Sequence of Modified Bessel Functions of the First Kind,
c    ACM Transactions of Mathematical Software,
c    Volume 9, Number 2, June 1983, pages 242-245.
c
c  Parameters:
c
c    Input, double precision X, the argument for which the functions
c    are to be calculated.
c
c    Input, double precision ALPHA,the fractional part of the order
c    for which the functions are to be calculated.
c    0 .le. ALPHA .lt. 1.0.
c
c    Input, integer NB, the number of functions to be calculated.
c    The first function calculated is of order ALPHA, and the
c    last is of order (NB - 1 + ALPHA).  1 .le. NB.
c
c    Input, integer IZE, scaling option.
c    1, unscaled I's are to calculated,
c    2, exponentially scaled I's are to be calculated.
c
c    Output, double precision B(NB), the values of the functions
c    I(ALPHA,X) through I(NB-1+ALPHA,X), with scaling if requested.
c
c    Output, integer NCALC, error indicator.
c    If NCALC = NB, then all the requested values were calculated
c    to the desired accuracy.
c
c  Local Parameeters:
c
c    BETA, the radix for the floating-point system.
c
c    MINEXP, smallest representable power of BETA.
c
c    MAXEXP, smallest power of BETA that overflows
c
c    IT, number of bits in the mantissa of a working precision variable.
c
c    NSIG, decimal significance desired.  Should be set to
c    INT(LOG10(2)*IT+1).  Setting NSIG lower will result
c    in decreased accuracy while setting NSIG higher will
c    increase CPU time without increasing accuracy.  The
c    truncation error is limited to a relative error of
c    T=.5*10^(-NSIG).
c
c    ENTEN, 10.0^K, where K is the largest integer such that
c    ENTEN is machine-representable in working precision
c
c    ENSIG, 10.0^NSIG
c
c    RTNSIG, 10.0^(-K) for the smallest integer K such that
c    NSIG/4 .le. K.
c
c    ENMTEN, smallest ABS(X) such that X/4 does not underflow
c
c    XLARGE, upper limit on the magnitude of X when IZE=2.  Bear
c    in mind that if ABS(X)=N, then at least N iterations
c    of the backward recursion will be executed.  The value
c    of 10.0^4 is used on every machine.
c
c    EXPARG, largest working precision argument that the library
c    EXP routine can handle and upper limit on the
c    magnitude of X when IZE=1; approximately log(BETA^MAXEXP).
c
c    Approximate values for some important machines are:
c
c                        beta       minexp      maxexp       it
c
c  CRAY-1        (S.P.)    2        -8193        8191        48
c  Cyber 180/855
c    under NOS   (S.P.)    2         -975        1070        48
c  IEEE (IBM/XT,
c    SUN, etc.)  (S.P.)    2         -126         128        24
c  IEEE (IBM/XT,
c    SUN, etc.)  (D.P.)    2        -1022        1024        53
c  IBM 3033      (D.P.)   16          -65          63        14
c  VAX           (S.P.)    2         -128         127        24
c  VAX D-Format  (D.P.)    2         -128         127        56
c  VAX G-Format  (D.P.)    2        -1024        1023        53
c
c
c                        NSIG       ENTEN       ENSIG      RTNSIG
c
c CRAY-1        (S.P.)    15       1.0E+2465   1.0E+15     1.0E-4
c Cyber 180/855
c   under NOS   (S.P.)    15       1.0E+322    1.0E+15     1.0E-4
c IEEE (IBM/XT,
c   SUN, etc.)  (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
c IEEE (IBM/XT,
c   SUN, etc.)  (D.P.)    16       1.0D+308    1.0D+16     1.0D-4
c IBM 3033      (D.P.)     5       1.0D+75     1.0D+5      1.0D-2
c VAX           (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
c VAX D-Format  (D.P.)    17       1.0D+38     1.0D+17     1.0D-5
c VAX G-Format  (D.P.)    16       1.0D+307    1.0D+16     1.0D-4
c
c
c                         ENMTEN      XLARGE   EXPARG
c
c CRAY-1        (S.P.)   1.84E-2466   1.0E+4    5677
c Cyber 180/855
c   under NOS   (S.P.)   1.25E-293    1.0E+4     741
c IEEE (IBM/XT,
c   SUN, etc.)  (S.P.)   4.70E-38     1.0E+4      88
c IEEE (IBM/XT,
c   SUN, etc.)  (D.P.)   8.90D-308    1.0D+4     709
c IBM 3033      (D.P.)   2.16D-78     1.0D+4     174
c VAX           (S.P.)   1.17E-38     1.0E+4      88
c VAX D-Format  (D.P.)   1.17D-38     1.0D+4      88
c VAX G-Format  (D.P.)   2.22D-308    1.0D+4     709
c
      implicit none

      integer nb

      double precision alpha
      double precision b(nb)
      double precision const
      parameter ( const = 1.585D+00 )
      double precision em
      double precision empal
      double precision emp2al
      double precision en
      double precision enmten
      parameter ( enmten = 8.9D-308 )
      double precision ensig
      parameter ( ensig = 1.0D+16 )
      double precision, parameter :: enten = 1.0D+308
      double precision, parameter :: exparg = 709.0D+00
      logical flag
      double precision, parameter :: half = 0.5D+00
      double precision halfx
      integer ize
      integer k
      integer l
      integer magx
      integer n
      integer nbmx
      integer ncalc
      integer nend
      integer, parameter :: nsig = 16
      integer nstart
      double precision, parameter :: one = 1.0D+00
      double precision p
      double precision plast
      double precision pold
      double precision psave
      double precision psavel
      double precision r8_gamma
      double precision, parameter :: rtnsig = 1.0D-04
      double precision tempa
      double precision tempb
      double precision tempc
      double precision test
      double precision total
      double precision tover
      double precision, parameter :: two = 2.0D+00
      double precision x
      double precision, parameter :: xlarge = 1.0D+04
      double precision zero
      parameter ( zero = 0.0D+00 )
c
c  Check for X, NB, OR IZE out of range.
c
      if ( nb .le. 0 ) then
        ncalc = min ( nb, 0 ) - 1
        return
      end if

      if ( x .lt. 0.0D+00 ) then
        ncalc = min ( nb, 0 ) - 1
        return
      end if

      if ( alpha .lt. 0.0D+00 ) then
        ncalc = min ( nb, 0 ) - 1
        return
      end if

      if ( 1.0D+00 .le. alpha ) then
        ncalc = min ( nb, 0 ) - 1
        return
      end if

      if ( ize .eq. 1 .and. exparg .lt. x ) then
        ncalc = min ( nb, 0 ) - 1
        return
      end if

      if ( ize .eq. 2 .and. xlarge .lt. x ) then
        ncalc = min ( nb, 0 ) - 1
        return
      end if
c
c  Use 2-term ascending series for small X.
c
      ncalc = nb
      magx = int ( x )
c
c  Initialize the forward sweep, the P-sequence of Olver.
c
      if ( rtnsig .le. x ) then

        nbmx = nb - magx
        n = magx + 1
        en = dble ( n + n ) + ( alpha + alpha )
        plast = one
        p = en / x
c
c  Calculate general significance test.
c
        test = ensig + ensig

        if ( 5 * nsig .lt. 2 * magx ) then
          test = sqrt ( test * p )
        else
          test = test / const**magx
        end if
c
c  Calculate P-sequence until N = NB-1.  Check for possible overflow.
c
        flag = .false.

        if ( 3 .le. nbmx ) then

          tover = enten / ensig
          nstart = magx + 2
          nend = nb - 1

          do k = nstart, nend

            n = k
            en = en + two
            pold = plast
            plast = p
            p = en * plast / x + pold
c
c  To avoid overflow, divide P-sequence by TOVER.  Calculate
c  P-sequence until 1 .lt. ABS(P).
c
            if ( tover .lt. p ) then

              tover = enten
              p = p / tover
              plast = plast / tover
              psave = p
              psavel = plast
              nstart = n + 1

10            continue

                n = n + 1
                en = en + two
                pold = plast
                plast = p
                p = en * plast / x + pold

                if ( 1.0D+00 .lt. p ) then
                  go to 20
                end if

              go to 10

20            continue

              tempb = en / x
c
c  Calculate backward test, and find NCALC, the highest N
c  such that the test is passed.
c
              test = pold * plast / ensig
              test = test * ( half - half / ( tempb * tempb ) )
              p = plast * tover
              n = n - 1
              en = en - two
              nend = min ( nb, n )

              ncalc = nend + 1

              do l = nstart, nend

                pold = psavel
                psavel = psave
                psave = en * psavel / x + pold

                if ( test .lt. psave * psavel ) then
                  ncalc = l
                  go to 30
                end if

              end do

30            continue

              ncalc = ncalc - 1
              flag = .true.
              go to 40

            end if

          end do

40        continue

          if ( .not. flag ) then

            n = nend
            en = dble ( n + n ) + ( alpha + alpha )
c
c  Calculate special significance test for 2 .lt. NBMX.
c
            test = max ( test, sqrt ( plast * ensig ) * sqrt ( p + p ) )

          end if

        end if
c
c  Calculate P-sequence until significance test passed.
c
        if ( .not. flag ) then

50        continue

            n = n + 1
            en = en + two
            pold = plast
            plast = p
            p = en * plast / x + pold

            if ( test .le. p ) then
              go to 60
            end if

          go to 50

60        continue

        end if
c
c  Initialize the backward recursion and the normalization sum.
c
        n = n + 1
        en = en + two
        tempb = zero
        tempa = one / p
        em = dble ( n ) - one
        empal = em + alpha
        emp2al = ( em - one ) + ( alpha + alpha )
        total = tempa * empal * emp2al / em
        nend = n - nb
c
c  N .lt. NB, so store B(N) and set higher orders to zero.
c
        if ( nend .lt. 0 ) then

          b(n) = tempa
          nend = -nend

          do l = 1, nend
            b(n+l) = zero
          end do

          nend = n - 2
c
c  Calculate via difference equation and store B(N), until N = 2.
c
          if ( 0 .lt. nend ) then

            do l = 1, nend
              n = n - 1
              en = en - two
              b(n) = ( en * b(n+1) ) / x + b(n+2)
              em = em - one
              emp2al = emp2al - one
              if ( n .eq. 2 ) then
                emp2al = one
              end if
              empal = empal - one
              total = ( total + b(n) * empal ) * emp2al / em
            end do

          end if
c
c  Calculate B(1).
c
          b(1) = two * empal * b(2) / x + b(3)

          total = ( total + total ) + b(1)
c
c  Recur backward via difference equation, calculating (but
c  not storing) B(N), until N = NB.
c
        else

          if ( 0 .lt. nend ) then

            do l = 1, nend

              n = n - 1
              en = en - two
              tempc = tempb
              tempb = tempa
              tempa = ( en * tempb ) / x + tempc
              em = em - one
              emp2al = emp2al - one

              if ( n .eq. 1 ) then
                go to 70
              end if

              if ( n .eq. 2 ) then
                emp2al = one
              end if

              empal = empal - one
              total = ( total + tempa * empal ) * emp2al / em

            end do

70          continue

          end if
c
c  Store B(NB).
c
          b(n) = tempa

          if ( nb .le. 1 ) then

            total = ( total + total ) + tempa
c
c  Calculate and Store B(NB-1).
c
          else

            n = n - 1
            en = en - two
            b(n) = ( en * tempa ) / x + tempb

            if ( 1 .lt. n  ) then

              em = em - one
              emp2al = emp2al - one

              if ( n .eq. 2 ) then
                emp2al = one
              end if

              empal = empal - one
              total = ( total + b(n) * empal ) * emp2al / em

              nend = n - 2
c
c  Calculate via difference equation and store B(N), until N = 2.
c
              if ( 0 .lt. nend ) then

                do l = 1, nend
                  n = n - 1
                  en = en - two
                  b(n) = ( en * b(n+1) ) / x + b(n+2)
                  em = em - one
                  emp2al = emp2al - one
                  if ( n .eq. 2 ) then
                    emp2al = one
                  end if
                  empal = empal - one
                  total = ( total + b(n) * empal ) * emp2al / em
                end do

              end if
c
c  Calculate B(1).
c
              b(1) = two * empal * b(2) / x + b(3)

            end if

            total = ( total + total ) + b(1)

          end if

        end if
c
c  Normalize.  Divide all B(N) by TOTAL.
c
        if ( alpha .ne. zero ) then
          total = total * r8_gamma ( one + alpha ) 
     &      * ( x * half )**( -alpha )
        end if

        if ( ize .eq. 1 ) then
          total = total * exp ( -x )
        end if

        tempa = enmten

        if ( 1.0D+00 .lt. total ) then
          tempa = tempa * total
        end if

        do n = 1, nb
          if ( b(n) .lt. tempa ) then
            b(n) = zero
          end if
          b(n) = b(n) / total
        end do

        return
c
c  Two-term ascending series for small X.
c
      else

        tempa = one
        empal = one + alpha
        halfx = zero

        if ( enmten .lt. x ) then
          halfx = half * x
        end if

        if ( alpha .ne. zero ) then
          tempa = halfx**alpha / r8_gamma ( empal )
        end if

        if ( ize .eq. 2 ) then
          tempa = tempa * exp ( - x )
        end if

        tempb = zero

        if ( one .lt. x + one ) then
          tempb = halfx * halfx
        end if

        b(1) = tempa + tempa * tempb / empal

        if ( x .ne. zero .and. b(1) .eq. zero ) then
          ncalc = 0
        end if

        if ( 1 .lt. nb ) then

          if ( x .eq. zero ) then

            b(2:nb) = zero
c
c  Calculate higher-order functions.
c
          else

            tempc = halfx
            tover = ( enmten + enmten ) / x

            if ( tempb .ne. zero ) then
              tover = enmten / tempb
            end if

            do n = 2, nb

              tempa = tempa / empal
              empal = empal + one
              tempa = tempa * tempc

              if ( tempa .le. tover * empal ) then
                tempa = zero
              end if

              b(n) = tempa + tempa * tempb / empal

              if ( b(n) .eq. zero .and. n .lt. ncalc ) then
                ncalc = n - 1
              end if

            end do

          end if

        end if

      end if

      return
      end
      subroutine runs_mean ( m, n, mean )

c*********************************************************************72
c
cc RUNS_MEAN returns the mean of the Runs PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer m
      double precision mean
      integer n

      mean = dble ( m + 2 * m * n + n ) / dble ( m + n )

      return
      end
      subroutine runs_pdf ( m, n, r, pdf )

c*********************************************************************72
c
cc RUNS_PDF evaluates the Runs PDF.
c
c  Discussion:
c
c    Suppose we have M symbols of one type and N of another, and we consider
c    the various possible permutations of these symbols.
c
c    Let "R" be the number of runs in a given permutation.  By a "run", we
c    mean a maximal sequence of identical symbols.  Thus, for instance,
c    the permutation
c
c      ABBBAAAAAAAA
c
c    has three runs.
c
c    The probability that a permutation of M+N symbols, with M of one kind
c    and N of another, will have exactly R runs is:
c
c      PDF(M,N)(R) = 2 * C(M-1,R/2-1) * C(N-1,R/2-1)
c                    / C(M+N,N) for R even;
c
c                  = ( C(M-1,(R-1)/2) * C(N-1,(R-3)/2 )
c                    + C(M-1,(R-3)/2) * C(N-1,(R-1)/2 )
c                    ) / C(M+N,N) for R odd.
c
c    The minimum number of runs is:
c
c      1 if M or N is 0,
c      2 otherwise.
c
c    The maximum number of runs is:
c
c      M + N,                if M = N
c      2 * min ( M, N ) + 1  otherwise
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Kalimutha Krishnamoorthy,
c    Handbook of Statistical Distributions with Applications,
c    Chapman and Hall, 2006,
c    ISBN: 1-58488-635-8,
c    LC: QA273.6.K75.
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c
c    Input, integer R, the number of runs.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer combinatorial
      integer m
      integer n
      double precision pdf
      integer r

      if ( m .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RUN_PDF - Fatal error!'
        write ( *, '(a)' ) '  M must be at least 0.'
        write ( *, '(a,i8)' ) '  The input value of M = ', m
        stop
      end if

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RUN_PDF - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 0.'
        write ( *, '(a,i8)' ) '  The input value of N = ', n
        stop
      end if

      if ( n + m .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'RUN_PDF - Fatal error!'
        write ( *, '(a)' ) '  M+N must be at least 1.'
        write ( *, '(a,i8)' ) '  The input value of M+N = ', m + n
        stop
      end if
c
c  If all the symbols are of one type, there is always 1 run.
c
      if ( m .eq. 0 .or. n .eq. 0 ) then
        if ( r .eq. 1 ) then
          pdf = 1.0D+00
        else
          pdf = 0.0D+00
        end if
        return
      end if
c
c  Take care of extreme values of R.
c
      if ( r .lt. 2 .or. m + n .lt. r ) then
        pdf = 0.0D+00
        return
      end if
c
c  The normal cases.
c
      if ( mod ( r, 2 ) .eq. 0 ) then

        pdf = dble ( 2 * combinatorial ( m - 1, ( r / 2 ) - 1 )         
     &           * combinatorial ( n - 1, ( r / 2 ) - 1 ) )   
     &      / dble (     combinatorial ( m + n, n ) )

      else

        pdf = dble (   
     &      combinatorial ( m - 1, ( r - 1 ) / 2 )           
     &    * combinatorial ( n - 1, ( r - 3 ) / 2 )
     &    + combinatorial ( m - 1, ( r - 3 ) / 2 )
     &    * combinatorial ( n - 1, ( r - 1 ) / 2 ) )
     &    / dble ( combinatorial ( m + n, n )  )

      end if

      return
      end
      subroutine runs_sample ( m, n, seed, r )

c*********************************************************************72
c
cc RUNS_SAMPLE samples the Runs PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer R, the number of runs.
c
      implicit none

      integer m
      integer n

      integer a(m+n)
      integer r
      integer seed

      call runs_simulate ( m, n, seed, a )

      call i4vec_run_count ( m+n, a, r )

      return
      end
      subroutine runs_simulate ( m, n, seed, a )

c*********************************************************************72
c
cc RUNS_SIMULATE simulates a case governed by the Runs PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer A(M+N), a sequence of M 0's and N 1's chosen
c    uniformly at random.
c
      implicit none

      integer m
      integer n

      integer a(m+n)
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer seed

      do i = 1, m
        a(i) = 0
      end do

      do i = m + 1, m + n
        a(i) = 1
      end do

      do i = 1, m + n - 1

        j = i4_uniform_ab ( i, m + n, seed )

        k    = a(i)
        a(i) = a(j)
        a(j) = k

      end do

      return
      end
      subroutine runs_variance ( m, n, variance )

c*********************************************************************72
c
cc RUNS_VARIANCE returns the variance of the Runs PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the parameters of the PDF.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer m
      integer n
      double precision variance

      variance = dble ( 2 * m * n * ( 2 * m * n - m - n ) )   
     &         / dble ( ( m + n ) * ( m + n ) * ( m + n - 1 ) )

      return
      end
      function sech ( x )

c*********************************************************************72
c
cc SECH returns the hyperbolic secant.
c
c  Discussion:
c
c    SECH ( X ) = 1.0D+00 / COSH ( X ) = 2.0D+00 / ( EXP ( X ) + EXP ( - X ) )
c
c    SECH is not a built-in function in FORTRAN, and occasionally it
c    is handier, or more concise, to be able to refer to it directly
c    rather than through its definition in terms of the sine function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument.
c
c    Output, double precision SECH, the hyperbolic secant of X.
c
      implicit none

      double precision sech
      double precision x

      sech = 1.0D+00 / cosh ( x )

      return
      end
      subroutine sech_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc SECH_CDF evaluates the Hyperbolic Secant CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      y = ( x - a ) / b

      cdf = 2.0D+00 * atan ( exp ( y ) ) / pi

      return
      end
      subroutine sech_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc SECH_CDF_INV inverts the Hyperbolic Secant CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_huge
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SECH_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = - r8_huge ( )
      else if ( cdf .lt. 1.0D+00 ) then
        x = a + b * log ( tan ( 0.5D+00 * pi * cdf ) )
      else if ( 1.0D+00 .eq. cdf ) then
        x = r8_huge ( )
      end if

      return
      end
      function sech_check ( a, b )

c*********************************************************************72
c
cc SECH_CHECK checks the parameters of the Hyperbolic Secant CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical SECH_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical sech_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SECH_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0'
        sech_check = .false.
        return
      end if

      sech_check = .true.

      return
      end
      subroutine sech_mean ( a, b, mean )

c*********************************************************************72
c
cc SECH_MEAN returns the mean of the Hyperbolic Secant PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine sech_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc SECH_PDF evaluates the Hypebolic Secant PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = sech ( ( X - A ) / B ) / ( PI * B )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision sech
      double precision x
      double precision y

      y = ( x - a ) / b

      pdf = sech ( y ) / ( pi * b )

      return
      end
      subroutine sech_sample ( a, b, seed, x )

c*********************************************************************72
c
cc SECH_SAMPLE samples the Hyperbolic Secant PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
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
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      x = a + b * log ( tan ( 0.5D+00 * pi * cdf ) )

      return
      end
      subroutine sech_variance ( a, b, variance )

c*********************************************************************72
c
cc SECH_VARIANCE returns the variance of the Hyperbolic Secant PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision variance

      variance = 0.25D+00 * ( pi * b )**2

      return
      end
      subroutine semicircular_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc SEMICIRCULAR_CDF evaluates the Semicircular CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      if ( x .le. a - b ) then

        cdf = 0.0D+00

      else if ( x .le. a + b ) then

        y = ( x - a ) / b

        cdf = 0.5D+00 + ( y * sqrt ( 1.0D+00 - y**2 ) + asin ( y ) ) / p
     &i

      else if ( a + b .lt. x ) then

        cdf = 1.0D+00

      end if

      return
      end
      subroutine semicircular_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc SEMICIRCULAR_CDF_INV inverts the Semicircular CDF.
c
c  Discussion:
c
c    A simple bisection method is used on the interval [ A - B, A + B ].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision tol
      parameter ( tol = 0.0001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SEMICIRCULAR_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = a - b
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = a + b
        return
      end if

      x1 = a - b
      cdf1 = 0.0D+00

      x2 = a + b
      cdf2 = 1.0D+00
c
c  Now use bisection.
c
      it = 0

10    continue


        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call semicircular_cdf ( x3, a, b, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          go to 20
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SEMICIRCULAR_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          stop
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 10

20    continue

      return
      end
      function semicircular_check ( a, b )

c*********************************************************************72
c
cc SEMICIRCULAR_CHECK checks the parameters of the Semicircular CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameter of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, logical SEMICIRCULAR_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical semicircular_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SEMICIRCULAR_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.0'
        semicircular_check = .false.
        return
      end if

      semicircular_check = .true.

      return
      end
      subroutine semicircular_mean ( a, b, mean )

c*********************************************************************72
c
cc SEMICIRCULAR_MEAN returns the mean of the Semicircular PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine semicircular_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc SEMICIRCULAR_PDF evaluates the Semicircular PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = ( 2 / ( B * PI ) ) * SQRT ( 1 - ( ( X - A ) / B )^2 )
c    for A - B .le. X .le. A + B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y

      if ( x .lt. a - b ) then

        pdf = 0.0D+00

      else if ( x .le. a + b ) then

        y = ( x - a ) / b

        pdf = 2.0D+00 / ( b * pi ) * sqrt ( 1.0D+00 - y**2 )

      else if ( a + b .lt. x ) then

        pdf = 0.0D+00

      end if

      return
      end
      subroutine semicircular_sample ( a, b, seed, x )

c*********************************************************************72
c
cc SEMICIRCULAR_SAMPLE samples the Semicircular PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision angle
      double precision b
      double precision r8_uniform_01
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision radius
      integer seed
      double precision x

      radius = r8_uniform_01 ( seed )
      radius = b * sqrt ( radius )
      angle = pi * r8_uniform_01 ( seed )
      x = a + radius * cos ( angle )

      return
      end
      subroutine semicircular_variance ( a, b, variance )

c*********************************************************************72
c
cc SEMICIRCULAR_VARIANCE returns the variance of the Semicircular PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = b * b / 4.0D+00

      return
      end
      function sin_power_int ( a, b, n )

c*********************************************************************72
c
cc SIN_POWER_INT evaluates the sine power integral.
c
c  Discussion:
c
c    The function is defined by
c
c      SIN_POWER_INT(A,B,N) = Integral ( A .le. T .le. B ) ( sin ( t ))^n dt
c
c    The algorithm uses the following fact:
c
c      Integral sin^n ( t ) = (1/n) * (
c        sin^(n-1)(t) * cos(t) + ( n-1 ) * Integral sin^(n-2) ( t ) dt )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, integer N, the power of the sine function.
c
c    Output, double precision SIN_POWER_INT, the value of the integral.
c
      implicit none

      double precision a
      double precision b
      double precision ca
      double precision cb
      integer m
      integer mlo
      integer n
      double precision sa
      double precision sb
      double precision sin_power_int
      double precision value

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SIN_POWER_INT - Fatal error!'
        write ( *, '(a)' ) '  Power N .lt. 0.'
        value = 0.0D+00
        stop
      end if

      sa = sin ( a )
      sb = sin ( b )
      ca = cos ( a )
      cb = cos ( b )

      if ( mod ( n, 2 ) .eq. 0 ) then

        value = b - a
        mlo = 2
      else
        value = ca - cb
        mlo = 3
      end if

      do m = mlo, n, 2
        value = ( dble ( m - 1 ) * value 
     &    + sa**(m-1) * ca - sb**(m-1) * cb ) / dble ( m )
      end do

      sin_power_int = value

      return
      end
      function sphere_unit_area_nd ( dim_num )

c*********************************************************************72
c
cc SPHERE_UNIT_AREA_ND computes the surface area of a unit sphere in ND.
c
c  Discussion:
c
c    The unit sphere in ND satisfies:
c
c      sum ( 1 .le. I .le. DIM_NUM ) X(I) * X(I) = 1
c
c    Results for the first few values of N are:
c
c    DIM_NUM   Area
c
c     2    2        * PI
c     3    4        * PI
c     4  ( 2 /   1) * PI^2
c     5  ( 8 /   3) * PI^2
c     6  ( 1 /   1) * PI^3
c     7  (16 /  15) * PI^3
c     8  ( 1 /   3) * PI^4
c     9  (32 / 105) * PI^4
c    10  ( 1 /  12) * PI^5
c
c    For the unit sphere, Area(DIM_NUM) = DIM_NUM * Volume(DIM_NUM)
c
c    Sphere_Unit_Area ( DIM_NUM ) = 2 * PI**(DIM_NUM/2) / Gamma ( DIM_NUM / 2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Output, double precision SPHERE_UNIT_AREA_ND, the area of the sphere.
c
      implicit none

      double precision area
      integer dim_num
      integer i
      integer m
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision sphere_unit_area_nd

      if ( mod ( dim_num, 2 ) .eq. 0 ) then
        m = dim_num / 2
        area = 2.0D+00 * ( pi )**m
        do i = 1, m - 1
          area = area / dble ( i )
        end do
      else
        m = ( dim_num - 1 ) / 2
        area = ( pi )**m * 2.0D+00**dim_num
        do i = m + 1, 2 * m
          area = area / dble ( i )
        end do
      end if

      sphere_unit_area_nd = area

      return
      end
      function stirling2_value ( n, m )

c*********************************************************************72
c
cc STIRLING2_VALUE computes a Stirling number of the second kind.
c
c  Discussion:
c
c    S2(N,M) represents the number of distinct partitions of N elements
c    into M nonempty sets.  For a fixed N, the sum of the Stirling
c    numbers S2(N,M) is represented by B(N), called "Bell's number",
c    and represents the number of distinct partitions of N elements.
c
c    For example, with 4 objects, there are:
c
c    1 partition into 1 set:
c
c      (A,B,C,D)
c
c    7 partitions into 2 sets:
c
c      (A,B,C) (D)
c      (A,B,D) (C)
c      (A,C,D) (B)
c      (A) (B,C,D)
c      (A,B) (C,D)
c      (A,C) (B,D)
c      (A,D) (B,C)
c
c    6 partitions into 3 sets:
c
c      (A,B) (C) (D)
c      (A) (B,C) (D)
c      (A) (B) (C,D)
c      (A,C) (B) (D)
c      (A,D) (B) (C)
c      (A) (B,D) (C)
c
c    1 partition into 4 sets:
c
c      (A) (B) (C) (D)
c
c    So S2(4,1) = 1, S2(4,2) = 7, S2(4,3) = 6, S2(4,4) = 1, and B(4) = 15.
c
c
c  First terms:
c
c    N/M: 1    2    3    4    5    6    7    8
c
c    1    1    0    0    0    0    0    0    0
c    2    1    1    0    0    0    0    0    0
c    3    1    3    1    0    0    0    0    0
c    4    1    7    6    1    0    0    0    0
c    5    1   15   25   10    1    0    0    0
c    6    1   31   90   65   15    1    0    0
c    7    1   63  301  350  140   21    1    0
c    8    1  127  966 1701 1050  266   28    1
c
c  Recursion:
c
c    S2(N,1) = 1 for all N.
c    S2(I,I) = 1 for all I.
c    S2(I,J) = 0 if I .lt. J.
c
c    S2(N,M) = M * S2(N-1,M) + S2(N-1,M-1)
c
c  Properties:
c
c    sum ( 1 .le. K .le. M ) S2(I,K) * S1(K,J) = Delta(I,J)
c
c    X^N = sum ( 0 .le. K .le. N ) S2(N,K) X_K
c    where X_K is the falling factorial function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of rows of the table.
c
c    Input, integer M, the number of columns of the table.
c
c    Output, integer STIRLING2_VALUE, the value of S2(N,M).
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer s2(n,m)
      integer stirling2_value

      if ( n .le. 0 ) then
        stirling2_value = 0
        return
      end if

      if ( m .le. 0 ) then
        stirling2_value = 0
        return
      end if

      s2(1,1) = 1
      s2(1,2:m) = 0

      do i = 2, n

        s2(i,1) = 1

        do j = 2, m
          s2(i,j) = j * s2(i-1,j) + s2(i-1,j-1)
        end do

      end do

      stirling2_value = s2(n,m)

      return
      end
      subroutine student_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc STUDENT_CDF evaluates the central Student T CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, shape parameters of the PDF,
c    used to transform the argument X to a shifted and scaled
c    value Y = ( X - A ) / B.  It is required that B be nonzero.
c    For the standard distribution, A = 0 and B = 1.
c
c    Input, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision b2
      double precision beta_inc
      double precision c
      double precision c2
      double precision cdf
      double precision x
      double precision y

      y = ( x - a ) / b

      a2 = 0.5D+00 * c
      b2 = 0.5D+00
      c2 = c / ( c + y * y )

      if ( y .le. 0.0D+00 ) then
        cdf = 0.5D+00 * beta_inc ( a2, b2, c2 )
      else
        cdf = 1.0D+00 - 0.5D+00 * beta_inc ( a2, b2, c2 )
      end if

      return
      end
      subroutine student_cdf_values ( n_data, c, x, fx )

c*********************************************************************72
c
cc STUDENT_CDF_VALUES returns some values of the Student CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = StudentTDistribution [ c ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 13 )

      double precision c
      double precision c_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save c_vec
      save fx_vec
      save x_vec

      data c_vec /
     &  1.0D+00,
     &  2.0D+00,
     &  3.0D+00,
     &  4.0D+00,
     &  5.0D+00,
     &  2.0D+00,
     &  5.0D+00,
     &  2.0D+00,
     &  5.0D+00,
     &  2.0D+00,
     &  3.0D+00,
     &  4.0D+00,
     &  5.0D+00 /
      data fx_vec /
     &  0.6000231200328521D+00,
     &  0.6001080279134390D+00,
     &  0.6001150934648930D+00,
     &  0.6000995134721354D+00,
     &  0.5999341989834830D+00,
     &  0.7498859393137811D+00,
     &  0.7500879487671045D+00,
     &  0.9500004222186464D+00,
     &  0.9499969138365968D+00,
     &  0.9900012348724744D+00,
     &  0.9900017619355059D+00,
     &  0.9900004567580596D+00,
     &  0.9900007637471291D+00 /
      data x_vec /
     &  0.325D+00,
     &  0.289D+00,
     &  0.277D+00,
     &  0.271D+00,
     &  0.267D+00,
     &  0.816D+00,
     &  0.727D+00,
     &  2.920D+00,
     &  2.015D+00,
     &  6.965D+00,
     &  4.541D+00,
     &  3.747D+00,
     &  3.365D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        c = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        c = c_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function student_check ( a, b, c )

c*********************************************************************72
c
cc STUDENT_CHECK checks the parameter of the central Student T CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, shape parameters of the PDF,
c    used to transform the argument X to a shifted and scaled
c    value Y = ( X - A ) / B.  It is required that B be nonzero.
c    For the standard distribution, A = 0 and B = 1.
c
c    Input, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical student_check

      if ( b .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'STUDENT_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B must be nonzero.'
        student_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'STUDENT_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C must be greater than 0.'
        student_check = .false.
        return
      end if

      student_check = .true.

      return
      end
      subroutine student_mean ( a, b, c, mean )

c*********************************************************************72
c
cc STUDENT_MEAN returns the mean of the central Student T PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, shape parameters of the PDF,
c    used to transform the argument X to a shifted and scaled
c    value Y = ( X - A ) / B.  It is required that B be nonzero.
c    For the standard distribution, A = 0 and B = 1.
c
c    Input, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean

      mean = a

      return
      end
      subroutine student_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc STUDENT_PDF evaluates the central Student T PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = Gamma ( (C+1)/2 ) /
c      ( Gamma ( C / 2 ) * Sqrt ( PI * C )
c      * ( 1 + ((X-A)/B)^2/C )^(C + 1/2 ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, shape parameters of the PDF,
c    used to transform the argument X to a shifted and scaled
c    value Y = ( X - A ) / B.  It is required that B be nonzero.
c    For the standard distribution, A = 0 and B = 1.
c
c    Input, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_gamma
      double precision x
      double precision y

      y = ( x - a ) / b

      pdf = r8_gamma ( 0.5D+00 * ( c + 1.0D+00 ) ) / ( sqrt ( pi * c )  
     &   * r8_gamma ( 0.5D+00 * c )     * sqrt ( ( 1.0D+00 + y * y / c )
     &**( 2 * c + 1.0D+00 ) ) )

      return
      end
      subroutine student_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc STUDENT_SAMPLE samples the central Student T PDF.
c
c  Discussion:
c
c    For the sampling algorithm, it is necessary that 2 .lt. C.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, shape parameters of the PDF,
c    used to transform the argument X to a shifted and scaled
c    value Y = ( X - A ) / B.  It is required that B be nonzero.
c    For the standard distribution, A = 0 and B = 1.
c
c    Input, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision a2
      double precision b
      double precision b2
      double precision c
      integer seed
      double precision x
      double precision x2
      double precision x3

      if ( c .lt. 3.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'STUDENT_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  Sampling fails for C .le. 2.'
        return
      end if

      a2 = 0.0D+00
      b2 = c / ( c - 2 )

      call normal_sample ( a2, b2, seed, x2 )

      call chi_square_sample ( c, seed, x3 )
      x3 = x3 * c / ( c - 2.0D+00 )

      x = a + b * x2 * sqrt ( c ) / x3

      return
      end
      subroutine student_variance ( a, b, c, variance )

c*********************************************************************72
c
cc STUDENT_VARIANCE returns the variance of the central Student T PDF.
c
c  Discussion:
c
c    The variance is not defined unless 2 .lt. C.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, shape parameters of the PDF,
c    used to transform the argument X to a shifted and scaled
c    value Y = ( X - A ) / B.  It is required that B be nonzero.
c    For the standard distribution, A = 0 and B = 1.
c
c    Input, double precision C, is usually called the number of
c    degrees of freedom of the distribution.  C is typically an
c    integer, but that is not essential.  It is required that
c    C be strictly positive.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision variance

      if ( c .le. 2.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'STUDENT_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  Variance not defined for C .le. 2.'
        stop
      end if

      variance = b * b * c / ( c - 2.0D+00 )

      return
      end
      subroutine student_noncentral_cdf ( x, idf, d, cdf )

c*********************************************************************72
c
cc STUDENT_NONCENTRAL_CDF evaluates the noncentral Student T CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
c
c  Author:
c
c    Original FORTRAN77 version by B E Cooper.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    BE Cooper,
c    Algorithm AS 5:
c    The Integral of the Non-Central T-Distribution,
c    Applied Statistics,
c    Volume 17, 1968, page 193.
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, integer IDF, the number of degrees of freedom.
c
c    Input, double precision D, the noncentrality parameter.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      integer a_max
  
      parameter ( a_max = 100 )
      double precision ak
      double precision b
      double precision cdf
      double precision cdf2
      double precision d
      double precision drb
      double precision emin
      parameter ( emin = 12.5D+00 )
      double precision f
      double precision fk
      double precision fmkm1
      double precision fmkm2
      double precision gamma_log
      integer idf
      integer k
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision sum2
      double precision temp
      double precision tfn
      double precision x

      f = dble ( idf )

      if ( idf .eq. 1 ) then

        a = x / sqrt ( f )
        b = f / ( f + x**2 )
        drb = d * sqrt ( b )

        call normal_01_cdf ( drb, cdf2 )
        cdf = 1.0D+00 - cdf2 + 2.0D+00 * tfn ( drb, a )

      else if ( idf .le. a_max ) then

        a = x / sqrt ( f )
        b = f / ( f + x * x )
        drb = d * sqrt ( b )
        sum2 = 0.0D+00

        fmkm2 = 0.0D+00
        if ( abs ( drb ) .lt. emin ) then
          call normal_01_cdf ( a * drb, cdf2 )
          fmkm2 = a * sqrt ( b ) * exp ( - 0.5D+00 * drb**2 ) * cdf2    
     &     / sqrt ( 2.0D+00 * pi )
        end if

        fmkm1 = b * d * a * fmkm2
        if ( abs ( d ) .lt. emin ) then
          fmkm1 = fmkm1 + 0.5D+00 * b * a * exp ( - 0.5D+00 * d**2 ) / p
     &i
        end if

        if ( mod ( idf, 2 ) .eq. 0 ) then
          sum2 = fmkm2
        else
          sum2 = fmkm1
        end if

        ak = 1.0D+00

        do k = 2, idf - 2, 2

          fk = dble ( k )

          fmkm2 = b * ( d * a * ak * fmkm1 + fmkm2 ) 
     &      * ( fk - 1.0D+00 ) / fk

          ak = 1.0D+00 / ( ak * ( fk - 1.0D+00 ) )
          fmkm1 = b * ( d * a * ak * fmkm2 + fmkm1 ) * fk 
     &      / ( fk + 1.0D+00 )

          if ( mod ( idf, 2 ) .eq. 0 ) then
            sum2 = sum2 + fmkm2
          else
            sum2 = sum2 + fmkm1
          end if

          ak = 1.0D+00 / ( ak * fk )

        end do

        if ( mod ( idf, 2 ) .eq. 0 ) then
          call normal_01_cdf ( d, cdf2 )
          cdf = 1.0D+00 - cdf2 + sum2 * sqrt ( 2.0D+00 * pi )
        else
          call normal_01_cdf ( drb, cdf2 )
          cdf = 1.0D+00 - cdf2 + 2.0D+00 * ( sum2 + tfn ( drb, a ) )
        end if
c
c  Normal approximation.
c
      else

        a = sqrt ( 0.5D+00 * f ) * exp ( gamma_log ( 0.5D+00 * ( f - 1.0
     &D+00 ) )       - gamma_log ( 0.5D+00 * f ) ) * d

        temp = ( x - a ) / sqrt ( f * ( 1.0D+00 + d**2 ) / ( f - 2.0D+00
     & ) - a**2 )

        call normal_01_cdf ( temp, cdf2 )
        cdf = cdf2

      end if

      return
      end
      subroutine student_noncentral_cdf_values ( n_data, df, lambda,
     &  x, fx )

c*********************************************************************72
c
cc STUDENT_NONCENTRAL_CDF_VALUES returns values of the noncentral Student CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = NoncentralStudentTDistribution [ df, lambda ]
c      CDF [ dist, x ]
c
c    Mathematica seems to have some difficulty computing this function
c    to the desired number of digits.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer DF, double precision LAMBDA, the parameters of the
c    function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )

      integer df
      integer df_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      double precision lambda
      double precision lambda_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save df_vec
      save fx_vec
      save lambda_vec
      save x_vec

      data df_vec /
     &   1,  2,  3,
     &   1,  2,  3,
     &   1,  2,  3,
     &   1,  2,  3,
     &   1,  2,  3,
     &  15, 20, 25,
     &   1,  2,  3,
     &  10, 10, 10,
     &  10, 10, 10,
     &  10, 10, 10 /
      data fx_vec /
     &  0.8975836176504333D+00,
     &  0.9522670169D+00,
     &  0.9711655571887813D+00,
     &  0.8231218864D+00,
     &  0.9049021510D+00,
     &  0.9363471834D+00,
     &  0.7301025986D+00,
     &  0.8335594263D+00,
     &  0.8774010255D+00,
     &  0.5248571617D+00,
     &  0.6293856597D+00,
     &  0.6800271741D+00,
     &  0.20590131975D+00,
     &  0.2112148916D+00,
     &  0.2074730718D+00,
     &  0.9981130072D+00,
     &  0.9994873850D+00,
     &  0.9998391562D+00,
     &  0.168610566972D+00,
     &  0.16967950985D+00,
     &  0.1701041003D+00,
     &  0.9247683363D+00,
     &  0.7483139269D+00,
     &  0.4659802096D+00,
     &  0.9761872541D+00,
     &  0.8979689357D+00,
     &  0.7181904627D+00,
     &  0.9923658945D+00,
     &  0.9610341649D+00,
     &  0.8688007350D+00 /
      data lambda_vec /
     &  0.0D+00,
     &  0.0D+00,
     &  0.0D+00,
     &  0.5D+00,
     &  0.5D+00,
     &  0.5D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  2.0D+00,
     &  2.0D+00,
     &  2.0D+00,
     &  4.0D+00,
     &  4.0D+00,
     &  4.0D+00,
     &  7.0D+00,
     &  7.0D+00,
     &  7.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  2.0D+00,
     &  3.0D+00,
     &  4.0D+00,
     &  2.0D+00,
     &  3.0D+00,
     &  4.0D+00,
     &  2.0D+00,
     &  3.0D+00,
     &  4.0D+00 /
      data x_vec /
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &   3.00D+00,
     &  15.00D+00,
     &  15.00D+00,
     &  15.00D+00,
     &   0.05D+00,
     &   0.05D+00,
     &   0.05D+00,
     &   4.00D+00,
     &   4.00D+00,
     &   4.00D+00,
     &   5.00D+00,
     &   5.00D+00,
     &   5.00D+00,
     &   6.00D+00,
     &   6.00D+00,
     &   6.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        df = 0
        lambda = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        df = df_vec(n_data)
        lambda = lambda_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function tfn ( h, a )

c*********************************************************************72
c
cc TFN calculates the T function of Owen.
c
c  Discussion:
c
c    Owen's T function is useful for computation of the bivariate normal
c    distribution and the distribution of a skewed normal distribution.
c
c    Although it was originally formulated in terms of the bivariate
c    normal function, the function can be defined more directly as
c
c      T(H,A) = 1 / ( 2 * pi ) *
c        Integral ( 0 .le. X .le. A ) e^( -H^2 * (1+X^2) / 2 ) / (1+X^2) dX
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2004
c
c  Author:
c
c    Original FORTRAN77 version by J C Young, C E Minder.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    Donald Owen,
c    Tables for computing the bivariate normal distribution,
c    Annals of Mathematical Statistics,
c    Volume 27, pages 1075-1090, 1956.
c
c    JC Young, CE Minder,
c    Algorithm AS 76,
c    An Algorithm Useful in Calculating Non-Central T and
c    Bivariate Normal Distributions,
c    Applied Statistics,
c    Volume 23, Number 3, 1974, pages 455-457.
c
c  Parameters:
c
c    Input, double precision H, A, the arguments of the T function.
c
c    Output, double precision TFN, the value of the T function.
c
      implicit none

      integer ngauss
      parameter ( ngauss = 10 )

      double precision a
      double precision as
      double precision h
      double precision h1
      double precision h2
      double precision hs
      integer i
      double precision rt
      double precision tfn
      double precision two_pi_inverse
      parameter ( two_pi_inverse = 0.1591549430918953D+00 )
      double precision tv1
      parameter ( tv1 = 1.0D-35 )
      double precision tv2
      parameter ( tv2 = 15.0D+00 )
      double precision, parameter :: tv3 = 15.0D+00
      double precision, parameter :: tv4 = 1.0D-05
      double precision, parameter, dimension ( ngauss ) :: weight = (/ 
     &    0.666713443086881375935688098933D-01,     0.149451349150580593
     &145776339658D+00,     0.219086362515982043995534934228D+00,     0.
     &269266719309996355091226921569D+00,     0.295524224714752870173892
     &994651D+00,     0.295524224714752870173892994651D+00,     0.269266
     &719309996355091226921569D+00,     0.219086362515982043995534934228
     &D+00,     0.149451349150580593145776339658D+00,     0.666713443086
     &881375935688098933D-01 /)
      double precision x
      double precision, parameter, dimension ( ngauss ) :: xtab = (/   
     & -0.973906528517171720077964012084D+00,    -0.86506336668898451073
     &2096688423D+00,    -0.679409568299024406234327365115D+00,    -0.43
     &3395394129247190799265943166D+00,    -0.14887433898163121088482600
     &1130D+00,     0.148874338981631210884826001130D+00,     0.43339539
     &4129247190799265943166D+00,     0.679409568299024406234327365115D+
     &00,     0.865063366688984510732096688423D+00,     0.97390652851717
     &1720077964012084D+00 /)
c
c  Test for H near zero.
c
      if ( abs ( h ) .lt. tv1 ) then
        tfn = atan ( a ) * two_pi_inverse
c
c  Test for large values of abs(H).
c
      else if ( tv2 .lt. abs ( h ) ) then
        tfn = 0.0D+00
c
c  Test for A near zero.
c
      else if ( abs ( a ) .lt. tv1 ) then
        tfn = 0.0D+00
c
c  Test whether abs(A) is so large that it must be truncated.
c  If so, the truncated value of A is H2.
c
      else

        hs = - 0.5D+00 * h * h
        h2 = a
        as = a * a
c
c  Computation of truncation point by Newton iteration.
c
        if ( tv3 .le. log ( 1.0D+00 + as ) - hs * as ) then

          h1 = 0.5D+00 * a
          as = 0.25D+00 * as

10        continue

            rt = as + 1.0D+00
            h2 = h1 + ( hs * as + tv3 - log ( rt ) )           
     &        / ( 2.0D+00 * h1 * ( 1.0D+00 / rt - hs ) )
            as = h2 * h2

            if ( abs ( h2 - h1 ) .lt. tv4 ) then
              go to 20
            end if

            h1 = h2

          go to 10

20        continue

        end if
c
c  Gaussian quadrature on the interval [0,H2].
c
        rt = 0.0D+00
        do i = 1, ngauss
          x = 0.5D+00 * h2 * ( xtab(i) + 1.0D+00 )
          rt = rt + weight(i) * exp ( hs * ( 1.0D+00 + x * x ) )        
     & / ( 1.0D+00 + x * x )
        end do

        tfn = rt * ( 0.5D+00 * h2 ) * two_pi_inverse

      end if

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
      subroutine triangle_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc TRIANGLE_CDF evaluates the Triangle CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x

      if ( x .le. a ) then

        cdf = 0.0D+00

      else if ( x .le. b ) then

        if ( a .eq. b ) then
          cdf = 0.0D+00
        else
          cdf = ( x - a ) * ( x - a ) / ( b - a ) / ( c - a )
        end if

      else if ( x .le. c ) then

        cdf = ( b - a ) / ( c - a )         + ( 2.0D+00 * c - b - x ) * 
     &( x - b ) / ( c - b ) / ( c - a )

      else

        cdf = 1.0D+00

      end if

      return
      end
      subroutine triangle_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc TRIANGLE_CDF_INV inverts the Triangle CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision cdf_mid
      double precision d
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      d = 2.0D+00 / ( c - a )
      cdf_mid = 0.5D+00 * d * ( b - a )

      if ( cdf .le. cdf_mid ) then
        x = a + sqrt ( cdf * ( b - a ) * ( c - a ) )
      else
        x = c - sqrt ( ( c - b ) * ( ( c - b ) - ( cdf - cdf_mid ) * ( c
     & - a ) ) )
      end if

      return
      end
      function triangle_check ( a, b, c )

c*********************************************************************72
c
cc TRIANGLE_CHECK checks the parameters of the Triangle CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Output, logical TRIANGLE_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical triangle_check

      if ( b .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. A.'
        triangle_check = .false.
        return
      end if

      if ( c .lt. b ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .lt. B.'
        triangle_check = .false.
        return
      end if

      if ( a .eq. c ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .eq. C.'
        triangle_check = .false.
        return
      end if

      triangle_check = .true.

      return
      end
      subroutine triangle_mean ( a, b, c, mean )

c*********************************************************************72
c
cc TRIANGLE_MEAN returns the mean of the Triangle PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Output, double precision MEAN, the mean of the discrete uniform PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean

      mean = a + ( c + b - 2.0D+00 * a ) / 3.0D+00

      return
      end
      subroutine triangle_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc TRIANGLE_PDF evaluates the Triangle PDF.
c
c  Discussion:
c
c    Given points A .le. B .le. C, the probability is 0 to the left of A,
c    rises linearly to a maximum of 2/(C-A) at B, drops linearly to zero
c    at C, and is zero for all values greater than C.
c
c    PDF(A,B,C;X)
c      = 2 * ( X - A ) / ( B - A ) / ( C - A ) for A .le. X .le. B
c      = 2 * ( C - X ) / ( C - B ) / ( C - A ) for B .le. X .le. C.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision x

      if ( x .le. a ) then

        pdf = 0.0D+00

      else if ( x .le. b ) then

        if ( a .eq. b ) then
          pdf = 0.0D+00
        else
          pdf = 2.0D+00 * ( x - a ) / ( b - a ) / ( c - a )
        end if

      else if ( x .le. c ) then

        if ( b .eq. c ) then
          pdf = 0.0D+00
        else
          pdf = 2.0D+00 * ( c - x ) / ( c - b ) / ( c - a )
        end if

      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine triangle_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc TRIANGLE_SAMPLE samples the Triangle PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call triangle_cdf_inv ( cdf, a, b, c, x )

      return
      end
      subroutine triangle_variance ( a, b, c, variance )

c*********************************************************************72
c
cc TRIANGLE_VARIANCE returns the variance of the Triangle PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    A .le. B .le. C and A .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision variance

      variance = ( ( c - a ) * ( c - a )              - ( c - a ) * ( b 
     &- a )              + ( b - a ) * ( b - a ) ) / 18.0D+00

      return
      end
      subroutine triangular_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc TRIANGULAR_CDF evaluates the Triangular CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .le. a ) then
        cdf = 0.0D+00
      else if ( x .le. 0.5D+00 * ( a + b ) ) then
        cdf = 2.0D+00 * ( x**2 - 2.0D+00 * a * x + a**2 ) / ( b - a )**2
      else if ( x .le. b ) then
        cdf = 0.5D+00 + ( - 2.0D+00 * x**2 + 4.0D+00 * b * x + 0.5D+00 *
     & a**2       - a * b - 1.5D+00 * b**2 ) / ( b - a )**2
      else
        cdf = 1.0D+00
      end if

      return
      end
      subroutine triangular_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc TRIANGULAR_CDF_INV inverts the Triangular CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
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
        write ( *, '(a)' ) 'TRIANGULAR_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .le. 0.5D+00 ) then
        x = a + 0.5D+00 * ( b - a ) * sqrt ( 2.0D+00 * cdf )
      else
        x = b - 0.5D+00 * ( b - a ) * sqrt ( 2.0D+00 * ( 1.0D+00 - cdf )
     & )
      end if

      return
      end
      function triangular_check ( a, b )

c*********************************************************************72
c
cc TRIANGULAR_CHECK checks the parameters of the Triangular CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, logical TRIANGULAR_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical triangular_check

      if ( b .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGULAR_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. A.'
        triangular_check = .false.
        return
      end if

      triangular_check = .true.

      return
      end
      subroutine triangular_mean ( a, b, mean )

c*********************************************************************72
c
cc TRIANGULAR_MEAN returns the mean of the Triangular PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = 0.5D+00 * ( a + b )

      return
      end
      subroutine triangular_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc TRIANGULAR_PDF evaluates the Triangular PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = 4 * ( X - A ) / ( B - A )^2 for A .le. X .le. (A+B)/2
c               = 4 * ( B - X ) / ( B - A )^2 for (A+B)/2 .le. X .le. B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .le. a ) then
        pdf = 0.0D+00
      else if ( x .le. 0.5D+00 * ( a + b ) ) then
        pdf = 4.0D+00 * ( x - a ) / ( b - a )**2
      else if ( x .le. b ) then
        pdf = 4.0D+00 * ( b - x ) / ( b - a )**2
      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine triangular_sample ( a, b, seed, x )

c*********************************************************************72
c
cc TRIANGULAR_SAMPLE samples the Triangular PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
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

      call triangular_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine triangular_variance ( a, b, variance )

c*********************************************************************72
c
cc TRIANGULAR_VARIANCE returns the variance of the Triangular PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = ( b - a )**2 / 24.0D+00

      return
      end
      function trigamma ( x )

c*********************************************************************72
c
cc TRIGAMMA calculates the TriGamma function.
c
c  Discussion:
c
c    TriGamma(x) = d^2 log ( Gamma ( x ) ) / dx^2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2000
c
c  Author:
c
c    FORTRAN77 original version by B Schneider
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    BE Schneider,
c    Algorithm AS 121:
c    Trigamma Function,
c    Applied Statistics,
c    Volume 27, Number 1, page 97-99, 1978.
c
c  Parameters:
c
c    Input, double precision X, the argument of the trigamma function.
c    0 .lt. X.
c
c    Output, double precision TRIGAMMA, the value of the
c    trigamma function at X.
c
      implicit none

      double precision a
      parameter ( a = 0.0001D+00 )
      double precision b
      parameter ( b = 5.0D+00 )
      double precision, parameter :: b2 =   1.0D+00 / 6.0D+00
      double precision, parameter :: b4 = - 1.0D+00 / 30.0D+00
      double precision, parameter :: b6 =   1.0D+00 / 42.0D+00
      double precision, parameter :: b8 = - 1.0D+00 / 30.0D+00
      double precision trigamma
      double precision x
      double precision y
      double precision z
c
c  1): If X is not positive, fail.
c
      if ( x .le. 0.0D+00 ) then

        trigamma = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIGAMMA - Fatal error!'
        write ( *, '(a)' ) '  X .le. 0.'
        stop
c
c  2): If X is smaller than A, use a small value approximation.
c
      else if ( x .le. a ) then

        trigamma = 1.0D+00 / x**2
c
c  3): Otherwise, increase the argument to B .le. ( X + I ).
c
      else

        z = x
        trigamma = 0.0D+00

10      continue

        if ( z .lt. b ) then
          trigamma = trigamma + 1.0D+00 / z**2
          z = z + 1.0D+00
          go to 10
        end if
c
c  ...and then apply an asymptotic formula.
c
        y = 1.0D+00 / z**2

        trigamma = trigamma + 0.5D+00 * y + ( 1.0D+00       
     &    + y * ( b2
     &    + y * ( b4
     &    + y * ( b6          
     &    + y *   b8 )))) / z

      end if

      return
      end
      subroutine uniform_01_cdf ( x, cdf )

c*********************************************************************72
c
cc UNIFORM_01_CDF evaluates the Uniform 01 CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision cdf
      double precision x

      if ( x .lt. 0.0D+00 ) then
        cdf = 0.0D+00
      else if ( 1.0D+00 .lt. x ) then
        cdf = 1.0D+00
      else
        cdf = x
      end if

      return
      end
      subroutine uniform_01_cdf_inv ( cdf, x )

c*********************************************************************72
c
cc UNIFORM_01_CDF_INV inverts the Uniform 01 CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_01_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = cdf

      return
      end
      subroutine uniform_01_mean ( mean )

c*********************************************************************72
c
cc UNIFORM_01_MEAN returns the mean of the Uniform 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision mean

      mean = 0.5D+00

      return
      end
      subroutine uniform_01_order_sample ( n, seed, x )

c*********************************************************************72
c
cc UNIFORM_01_ORDER_SAMPLE samples the Uniform 01 Order PDF.
c
c  Discussion:
c
c    In effect, this routine simply generates N samples of the
c    Uniform 01 PDF; but it generates them in order.  (Actually,
c    it generates them in descending order, but stores them in
c    the array in ascending order).  This saves the work of
c    sorting the results.  Moreover, if the order statistics
c    for another PDF are desired, and the inverse CDF is available,
c    then the desired values may be generated, presorted, by
c    calling this routine and using the results as input to the
c    inverse CDF routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jerry Banks, editor,
c    Handbook of Simulation,
c    Engineering and Management Press Books, 1998, page 168.
c
c  Parameters:
c
c    Input, integer N, the number of elements in the sample.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(N), N samples of the Uniform 01 PDF, in
c    ascending order.
c
      implicit none

      integer n

      double precision r8_uniform_01
      integer i
      integer seed
      double precision u
      double precision v
      double precision x(n)

      v = 1.0D+00
      do i = n, 1, -1
        u = r8_uniform_01 ( seed )
        v = v * u**( 1.0D+00 / dble ( i ) )
        x(i) = v
      end do

      return
      end
      subroutine uniform_01_pdf ( x, pdf )

c*********************************************************************72
c
cc UNIFORM_01_PDF evaluates the Uniform 01 PDF.
c
c  Discussion:
c
c    PDF(X) = 1 for 0 .le. X .le. 1
c           = 0 otherwise
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    0.0D+00 .le. X .le. 1.0.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision x

      if ( x .lt. 0.0D+00 .or. 1.0D+00 .lt. x ) then
        pdf = 0.0D+00
      else
        pdf = 1.0D+00
      end if

      return
      end
      function uniform_01_sample ( seed )

c*********************************************************************72
c
cc UNIFORM_01_SAMPLE is a portable random number generator.
c
c  Discussion:
c
c    SEED = SEED * (7^5) mod ( 2^31 - 1 )
c    UNIFORM_01_SAMPLE = SEED * / ( 2^31 - 1 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 February 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, the integer "seed" used to
c    generate the output random number, and updated in preparation for the
c    next one.  SEED should not be zero.
c
c    Output, double precision UNIFORM_01_SAMPLE, a random value between 0
c    and 1.
c
c  Local parameters:
c
c    IA = 7**5
c    IB = 2**15
c    IB16 = 2**16
c    IP = 2**31-1
c
      implicit none

      integer ia
      parameter ( ia = 16807 )
      integer ib15
      parameter ( ib15 = 32768 )
      integer ib16
      parameter ( ib16 = 65536 )
      integer ip
      parameter ( ip = 2147483647 )
      integer iprhi
      integer ixhi
      integer k
      integer leftlo
      integer loxa
      integer seed
      double precision uniform_01_sample
c
c  Don't let SEED be 0 or IP
c
      if ( seed .eq. 0 .or. seed .eq. ip ) then
        seed = ip / 2
      end if
c
c  Get the 15 high order bits of SEED.
c
      ixhi = seed / ib16
c
c  Get the 16 low bits of SEED and form the low product.
c
      loxa = ( seed - ixhi * ib16 ) * ia
c
c  Get the 15 high order bits of the low product.
c
      leftlo = loxa / ib16
c
c  Form the 31 highest bits of the full product.
c
      iprhi = ixhi * ia + leftlo
c
c  Get overflow past the 31st bit of full product.
c
      k = iprhi / ib15
c
c  Assemble all the parts and presubtract IP.  The parentheses are essential.
c
      seed = ( ( ( loxa - leftlo * ib16 ) - ip ) + ( iprhi - k * ib15 ) 
     &* ib16 )     + k
c
c  Add IP back in if necessary.
c
      if ( seed .lt. 0 ) then
        seed = seed + ip
      end if
c
c  Multiply by 1 / (2^31-1).
c
      uniform_01_sample = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine uniform_01_variance ( variance )

c*********************************************************************72
c
cc UNIFORM_01_VARIANCE returns the variance of the Uniform 01 PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision variance

      variance = 1.0D+00 / 12.0D+00

      return
      end
      subroutine uniform_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc UNIFORM_CDF evaluates the Uniform CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x

      if ( x .lt. a ) then
        cdf = 0.0D+00
      else if ( b .lt. x ) then
        cdf = 1.0D+00
      else
        cdf = ( x - a ) / ( b - a )
      end if

      return
      end
      subroutine uniform_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc UNIFORM_CDF_INV inverts the Uniform CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
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
        write ( *, '(a)' ) 'UNIFORM_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a + ( b - a ) * cdf

      return
      end
      function uniform_check ( a, b )

c*********************************************************************72
c
cc UNIFORM_CHECK checks the parameters of the Uniform CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, logical UNIFORM_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      logical uniform_check

      if ( b .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. A.'
        uniform_check = .false.
        return
      end if

      uniform_check = .true.

      return
      end
      subroutine uniform_mean ( a, b, mean )

c*********************************************************************72
c
cc UNIFORM_MEAN returns the mean of the Uniform PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = 0.5D+00 * ( a + b )

      return
      end
      subroutine uniform_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc UNIFORM_PDF evaluates the Uniform PDF.
c
c  Discussion:
c
c    The Uniform PDF is also known as the "Rectangular" or "de Moivre" PDF.
c
c    PDF(A,B;X) = 1 / ( B - A ) for A .le. X .le. B
c               = 0 otherwise
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      double precision x

      if ( x .lt. a .or. b .lt. x ) then
        pdf = 0.0D+00
      else
        pdf = 1.0D+00 / ( b - a )
      end if

      return
      end
      subroutine uniform_sample ( a, b, seed, x )

c*********************************************************************72
c
cc UNIFORM_SAMPLE samples the Uniform PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
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

      call uniform_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine uniform_variance ( a, b, variance )

c*********************************************************************72
c
cc UNIFORM_VARIANCE returns the variance of the Uniform PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    A .lt. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision variance

      variance = ( b - a )**2 / 12.0D+00

      return
      end
      subroutine uniform_discrete_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_CDF evaluates the Uniform Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the CDF.
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      integer a
      integer b
      double precision cdf
      integer x

      if ( x .lt. a ) then
        cdf = 0.0D+00
      else if ( b .lt. x ) then
        cdf = 1.0D+00
      else
        cdf = dble ( x + 1 - a ) / dble ( b + 1 - a )
      end if

      return
      end
      subroutine uniform_discrete_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_CDF_INV inverts the Uniform Discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Output, integer X, the smallest argument whose CDF is greater
c    than or equal to CDF.
c
      implicit none

      integer a
      double precision a2
      integer b
      double precision b2
      double precision cdf
      integer x
      double precision x2

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_DISCRETE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      a2 = dble ( a ) - 0.5D+00
      b2 = dble ( b ) + 0.5D+00
      x2 = a + cdf * ( b2 - a2 )

      x = nint ( x2 )

      x = max ( x, a )
      x = min ( x, b )

      return
      end
      function uniform_discrete_check ( a, b )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_CHECK checks the parameters of the Uniform discrete CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Output, logical UNIFORM_DISCRETE_CHECK, is true if the parameters
c    are legal.
c
      implicit none

      integer a
      integer b
      logical uniform_discrete_check

      if ( b .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'UNIFORM_DISCRETE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. A.'
        uniform_discrete_check = .false.
        return
      end if

      uniform_discrete_check = .true.

      return
      end
      subroutine uniform_discrete_mean ( a, b, mean )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_MEAN returns the mean of the Uniform discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      integer a
      integer b
      double precision mean

      mean = 0.5D+00 * dble ( a + b )

      return
      end
      subroutine uniform_discrete_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_PDF evaluates the Uniform discrete PDF.
c
c  Discussion:
c
c    The Uniform Discrete PDF is also known as the "Rectangular"
c    Discrete PDF.
c
c    PDF(A,B;X) = 1 / ( B + 1 - A ) for A .le. X .le. B.
c
c    The parameters define the interval of integers
c    for which the PDF is nonzero.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      integer a
      integer b
      double precision pdf
      integer x

      if ( x .lt. a .or. b .lt. x ) then
        pdf = 0.0D+00
      else
        pdf = 1.0D+00 / dble ( b + 1 - a )
      end if

      return
      end
      subroutine uniform_discrete_sample ( a, b, seed, x )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_SAMPLE samples the Uniform discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      integer a
      integer b
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call uniform_discrete_cdf_inv ( cdf, a, b, x )

      return
      end
      subroutine uniform_discrete_variance ( a, b, variance )

c*********************************************************************72
c
cc UNIFORM_DISCRETE_VARIANCE returns the variance of the Uniform discrete PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 January 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, B, the parameters of the PDF.
c    A .le. B.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      integer a
      integer b
      double precision variance

      variance = dble ( ( b + 1 - a )**2 - 1 ) / 12.0D+00

      return
      end
      subroutine uniform_nsphere_sample ( n, seed, x )

c*********************************************************************72
c
cc UNIFORM_NSPHERE_SAMPLE samples the Uniform Unit Sphere PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 December 2001
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jerry Banks, editor,
c    Handbook of Simulation,
c    Engineering and Management Press Books, 1998, page 168.
c
c  Parameters:
c
c    Input, integer N, the dimension of the sphere.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(N), a point on the unit N sphere, chosen
c    with a uniform probability.
c
      implicit none

      integer n

      integer i
      integer seed
      double precision x(n)
      double precision x_norm

      do i = 1, n
        call normal_01_sample ( seed, x(i) )
      end do

      x_norm = 0.0D+00
      do i = 1, n
        x_norm = x_norm + x(i)**2
      end do
      x_norm = sqrt ( x_norm )

      do i = 1, n
        x(i) = x(i) / x_norm
      end do

      return
      end
      subroutine von_mises_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc VON_MISES_CDF evaluates the von Mises CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2005
c
c  Author:
c
c    Original FORTRAN77 version by Geoffrey Hill.
c    FORTRAN90 version by John Burkardt
c
c  Reference:
c
c    Geoffrey Hill,
c    Algorithm 518,
c    Incomplete Bessel Function I0: The von Mises Distribution,
c    ACM Transactions on Mathematical Software,
c    Volume 3, Number 3, September 1977, pages 279-284.
c
c    Kanti Mardia, Peter Jupp,
c    Directional Statistics,
c    Wiley, 2000,
c    QA276.M335
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    A - PI .le. X .le. A + PI.
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision a1
      parameter ( a1 = 12.0D+00 )
      double precision a2
      parameter ( a2 = 0.8D+00 )
      double precision a3
      parameter ( a3 = 8.0D+00 )
      double precision a4
      parameter ( a4 = 1.0D+00 )
      double precision arg
      double precision b
      double precision c
      double precision c1
      parameter ( c1 = 56.0D+00 )
      double precision cdf
      double precision ck
      parameter ( ck = 10.5D+00 )
      double precision cn
      double precision error_f
      double precision erfx
      integer ip
      integer n
      double precision p
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision s
      double precision sn
      double precision u
      double precision v
      double precision x
      double precision y
      double precision z
c
c  We expect -PI .le. X - A .le. PI.
c
      if ( x - a .le. -pi ) then
        cdf = 0.0D+00
        return
      end if

      if ( pi .le. x - a ) then
        cdf = 1.0D+00
        return
      end if
c
c  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
c
      z = b

      u = mod ( x - a + pi, 2.0D+00 * pi )

      if ( u .lt. 0.0D+00 ) then
        u = u + 2.0D+00 * pi
      end if

      y = u - pi
c
c  For small B, sum IP terms by backwards recursion.
c
      if ( z .le. ck ) then

        v = 0.0D+00

        if ( 0.0D+00 .lt. z ) then

          ip = int ( z * a2 - a3 / ( z + a4 ) + a1 )
          p = dble ( ip )
          s = sin ( y )
          c = cos ( y )
          y = p * y
          sn = sin ( y )
          cn = cos ( y )
          r = 0.0D+00
          z = 2.0D+00 / z

          do n = 2, ip
            p = p - 1.0D+00
            y = sn
            sn = sn * c - cn * s
            cn = cn * c + y * s
            r = 1.0D+00 / ( p * z + r )
            v = ( sn / p + v ) * r
          end do

        end if

        cdf = ( u * 0.5D+00 + v ) / pi
c
c  For large B, compute the normal approximation and left tail.
c
      else

        c = 24.0D+00 * z
        v = c - c1
        r = sqrt ( ( 54.0D+00 / ( 347.0D+00 / v + 26.0D+00 - c ) - 6.0D+
     &00 + c )       / 12.0D+00 )
        z = sin ( 0.5D+00 * y ) * r
        s = 2.0D+00 * z**2
        v = v - s + 3.0D+00
        y = ( c - s - s - 16.0D+00 ) / 3.0D+00
        y = ( ( s + 1.75D+00 ) * s + 83.5D+00 ) / v - y
        arg = z * ( 1.0D+00 - s / y**2 )
        erfx = error_f ( arg )
        cdf = 0.5D+00 * erfx + 0.5D+00

      end if

      cdf = max ( cdf, 0.0D+00 )
      cdf = min ( cdf, 1.0D+00 )

      return
      end
      subroutine von_mises_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc VON_MISES_CDF_INV inverts the von Mises CDF.
c
c  Discussion:
c
c    A simple bisection method is used on the interval [ A - PI, A + PI ].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c
c    Output, double precision X, the corresponding argument of the CDF.
c    A - PI .le. X .le. A + PI.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision cdf1
      double precision cdf2
      double precision cdf3
      integer it
      integer it_max
      parameter ( it_max = 100 )
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision tol
      parameter ( tol = 0.000001D+00 )
      double precision x
      double precision x1
      double precision x2
      double precision x3

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'VON_MISES_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      if ( cdf .eq. 0.0D+00 ) then
        x = a - pi
        return
      else if ( 1.0D+00 .eq. cdf ) then
        x = a + pi
        return
      end if

      x1 = a - pi
      cdf1 = 0.0D+00

      x2 = a + pi
      cdf2 = 1.0D+00
c
c  Now use bisection.
c
      it = 0

10    continue

        it = it + 1

        x3 = 0.5D+00 * ( x1 + x2 )
        call von_mises_cdf ( x3, a, b, cdf3 )

        if ( abs ( cdf3 - cdf ) .lt. tol ) then
          x = x3
          go to 20
        end if

        if ( it_max .lt. it ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'VON_MISES_CDF_INV - Fatal error!'
          write ( *, '(a)' ) '  Iteration limit exceeded.'
          stop
        end if

        if ( sign ( 1.0D+00, cdf3 - cdf ) .eq. 
     &       sign ( 1.0D+00, cdf1 - cdf ) ) then
          x1 = x3
          cdf1 = cdf3
        else
          x2 = x3
          cdf2 = cdf3
        end if

      go to 10

20    continue

      return
      end
      subroutine von_mises_cdf_values ( n_data, a, b, x, fx )

c*********************************************************************72
c
cc VON_MISES_CDF_VALUES returns some values of the von Mises CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Kanti Mardia, Peter Jupp,
c    Directional Statistics,
c    Wiley, 2000,
c    LC: QA276.M335
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI <= A <= PI.
c
c    Output, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 <= B.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 23 )

      double precision a
      double precision a_vec(n_max)
      double precision b
      double precision b_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save b_vec
      save fx_vec
      save x_vec

      data a_vec /
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &  -0.2D+01,
     &  -0.1D+01,
     &   0.0D+01,
     &   0.1D+01,
     &   0.2D+01,
     &   0.3D+01,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00 /
      data b_vec /
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.1D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.2D+01,
     &   0.3D+01,
     &   0.3D+01,
     &   0.3D+01,
     &   0.3D+01,
     &   0.3D+01,
     &   0.3D+01,
     &   0.0D+00,
     &   0.1D+01,
     &   0.2D+01,
     &   0.3D+01,
     &   0.4D+01,
     &   0.5D+01 /
      data fx_vec /
     &  0.2535089956281180D-01,
     &  0.1097539041177346D+00,
     &  0.5000000000000000D+00,
     &  0.8043381312498558D+00,
     &  0.9417460124555197D+00,
     &  0.5000000000000000D+00,
     &  0.6018204118446155D+00,
     &  0.6959356933122230D+00,
     &  0.7765935901304593D+00,
     &  0.8410725934916615D+00,
     &  0.8895777369550366D+00,
     &  0.9960322705517925D+00,
     &  0.9404336090170247D+00,
     &  0.5000000000000000D+00,
     &  0.5956639098297530D-01,
     &  0.3967729448207649D-02,
     &  0.2321953958111930D-03,
     &  0.6250000000000000D+00,
     &  0.7438406999109122D+00,
     &  0.8369224904294019D+00,
     &  0.8941711407897124D+00,
     &  0.9291058600568743D+00,
     &  0.9514289900655436D+00 /
      data x_vec /
     &  -0.2617993977991494D+01,
     &  -0.1570796326794897D+01,
     &   0.0000000000000000D+00,
     &   0.1047197551196598D+01,
     &   0.2094395102393195D+01,
     &   0.1000000000000000D+01,
     &   0.1200000000000000D+01,
     &   0.1400000000000000D+01,
     &   0.1600000000000000D+01,
     &   0.1800000000000000D+01,
     &   0.2000000000000000D+01,
     &   0.0000000000000000D+00,
     &   0.0000000000000000D+00,
     &   0.0000000000000000D+00,
     &   0.0000000000000000D+00,
     &   0.0000000000000000D+00,
     &   0.0000000000000000D+00,
     &   0.7853981633974483D+00,
     &   0.7853981633974483D+00,
     &   0.7853981633974483D+00,
     &   0.7853981633974483D+00,
     &   0.7853981633974483D+00,
     &   0.7853981633974483D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0.0D+00
        b = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        a = a_vec(n_data)
        b = b_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function von_mises_check ( a, b )

c*********************************************************************72
c
cc VON_MISES_CHECK checks the parameters of the von Mises PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c    Output, logical VON_MISES_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      logical von_mises_check

      if ( a .lt. -pi .or. pi .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'VON_MISES_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. -PI or PI .lt. A.'
        von_mises_check = .false.
        return
      end if

      if ( b .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'VON_MISES_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .lt. 0.0'
        von_mises_check = .false.
        return
      end if

      von_mises_check = .true.

      return
      end
      subroutine von_mises_circular_variance ( a, b, circular_variance )

c*********************************************************************72
c
cc VON_MISES_CIRCULAR_VARIANCE: circular variance of the von Mises PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c    Output, double precision CIRCULAR_VARIANCE, the circular variance
c    of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision bessel_i0
      double precision bessel_i1
      double precision circular_variance

      circular_variance = 1.0D+00 - bessel_i1 ( b ) / bessel_i0 ( b )

      return
      end
      subroutine von_mises_mean ( a, b, mean )

c*********************************************************************72
c
cc VON_MISES_MEAN returns the mean of the von Mises PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision mean

      mean = a

      return
      end
      subroutine von_mises_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc VON_MISES_PDF evaluates the von Mises PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = EXP ( B * COS ( X - A ) ) / ( 2 * PI * I0(B) )
c
c    where:
c
c      I0(*) is the modified Bessel function of the first
c      kind of order 0.
c
c    The von Mises distribution for points on the unit circle is
c    analogous to the normal distribution of points on a line.
c    The variable X is interpreted as a deviation from the angle A,
c    with B controlling the amount of dispersion.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Jerry Banks, editor,
c    Handbook of Simulation,
c    Engineering and Management Press Books, 1998, page 160.
c
c    Donald Best, Nicholas Fisher,
c    Efficient Simulation of the von Mises Distribution,
c    Applied Statistics,
c    Volume 28, Number 2, pages 152-157.
c
c    Merran Evans, Nicholas Hastings, Brian Peacock,
c    Statistical Distributions,
c    Wiley, 2000,
c    LC: QA273.6.E92, pages 189-191.
c
c    Kanti Mardia, Peter Jupp,
c    Directional Statistics,
c    Wiley, 2000,
c    LC: QA276.M335
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A - PI .le. X .le. A + PI.
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision bessel_i0
      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      if ( x .lt. a - pi ) then
        pdf = 0.0D+00
      else if ( x .le. a + pi ) then
        pdf = exp ( b * cos ( x - a ) ) / ( 2.0D+00 * pi * bessel_i0 ( b
     & ) )
      else
        pdf = 0.0D+00
      end if

      return
      end
      subroutine von_mises_sample ( a, b, seed, x )

c*********************************************************************72
c
cc VON_MISES_SAMPLE samples the von Mises PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Best, Nicholas Fisher,
c    Efficient Simulation of the von Mises Distribution,
c    Applied Statistics,
c    Volume 28, Number 2, pages 152-157.
c
c  Parameters:
c
c    Input, double precision A, a parameter of the PDF.
c    A is the preferred direction, in radians.
c    -PI .le. A .le. PI.
c
c    Input, double precision B, a parameter of the PDF.
c    B measures the "concentration" of the distribution around the
c    angle A.  B = 0 corresponds to a uniform distribution
c    (no concentration).  Higher values of B cause greater concentration
c    of probability near A.
c    0.0D+00 .le. B.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision r8_uniform_01
      double precision f
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision rho
      integer seed
      double precision tau
      double precision u1
      double precision u2
      double precision u3
      double precision x
      double precision z

      tau = 1.0D+00 + sqrt ( 1.0D+00 + 4.0D+00 * b * b )
      rho = ( tau - sqrt ( 2.0D+00 * tau ) ) / ( 2.0D+00 * b )
      r = ( 1.0D+00 + rho**2 ) / ( 2.0D+00 * rho )

10    continue
 
        u1 = r8_uniform_01 ( seed )
        z = cos ( pi * u1 )
        f = ( 1.0D+00 + r * z ) / ( r + z )
        c = b * ( r - f )

        u2 = r8_uniform_01 ( seed )

        if ( u2 .lt. c * ( 2.0D+00 - c ) ) then
          go to 20
        end if

        if ( c .le. log ( c / u2 ) + 1.0D+00 ) then
          go to 20
        end if

      go to 10

20    continue

      u3 = r8_uniform_01 ( seed )

      x = a + sign ( 1.0D+00, u3 - 0.5D+00 ) * acos ( f )

      return
      end
      subroutine weibull_cdf ( x, a, b, c, cdf )

c*********************************************************************72
c
cc WEIBULL_CDF evaluates the Weibull CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c    A .le. X.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x
      double precision y

      if ( x .lt. a ) then
        cdf = 0.0D+00
      else
        y = ( x - a ) / b
        cdf = 1.0D+00 - 1.0D+00 / exp ( y**c )
      end if

      return
      end
      subroutine weibull_cdf_inv ( cdf, a, b, c, x )

c*********************************************************************72
c
cc WEIBULL_CDF_INV inverts the Weibull CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .lt. CDF .lt. 1.0.
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision X, the corresponding argument of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEIBULL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = a + b * ( - log ( 1.0D+00 - cdf ) )**( 1.0D+00 / c )

      return
      end
      subroutine weibull_cdf_values ( n_data, alpha, beta, x, fx )

c*********************************************************************72
c
cc WEIBULL_CDF_VALUES returns some values of the Weibull CDF.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      Needs["Statistics`ContinuousDistributions`"]
c      dist = WeibullDistribution [ alpha, beta ]
c      CDF [ dist, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, double precision ALPHA, the first parameter of the distribution.
c
c    Output, double precision BETA, the second parameter of the distribution.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision alpha
      double precision alpha_vec(n_max)
      double precision beta
      double precision beta_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save alpha_vec
      save beta_vec
      save fx_vec
      save x_vec

      data alpha_vec /
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01 /
      data beta_vec /
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01 /
      data fx_vec /
     &  0.8646647167633873D+00,
     &  0.9816843611112658D+00,
     &  0.9975212478233336D+00,
     &  0.9996645373720975D+00,
     &  0.6321205588285577D+00,
     &  0.4865828809674080D+00,
     &  0.3934693402873666D+00,
     &  0.3296799539643607D+00,
     &  0.8946007754381357D+00,
     &  0.9657818816883340D+00,
     &  0.9936702845725143D+00,
     &  0.9994964109502630D+00 /
      data x_vec /
     &  0.1000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01,
     &  0.3000000000000000D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        alpha = 0.0D+00
        beta = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        alpha = alpha_vec(n_data)
        beta = beta_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      function weibull_check ( a, b, c )

c*********************************************************************72
c
cc WEIBULL_CHECK checks the parameters of the Weibull CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, logical WEIBULL_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical weibull_check

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEIBULL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        weibull_check = .false.
        return
      end if

      if ( c .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEIBULL_CHECK - Fatal error!'
        write ( *, '(a)' ) '  C .le. 0.'
        weibull_check = .false.
        return
      end if

      weibull_check = .true.

      return
      end
      subroutine weibull_mean ( a, b, c, mean )

c*********************************************************************72
c
cc WEIBULL_MEAN returns the mean of the Weibull PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision MEAN, the mean of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision mean
      double precision r8_gamma

      mean = b * r8_gamma ( ( c + 1.0D+00 ) / c ) + a

      return
      end
      subroutine weibull_pdf ( x, a, b, c, pdf )

c*********************************************************************72
c
cc WEIBULL_PDF evaluates the Weibull PDF.
c
c  Discussion:
c
c    PDF(A,B,C;X) = ( C / B ) * ( ( X - A ) / B )^( C - 1 )
c     * EXP ( - ( ( X - A ) / B )^C ).
c
c    The Weibull PDF is also known as the Frechet PDF.
c
c    WEIBULL_PDF(A,B,1;X) is the Exponential PDF.
c
c    WEIBULL_PDF(0,1,2;X) is the Rayleigh PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c    A .le. X
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pdf
      double precision x
      double precision y

      if ( x .lt. a ) then

        pdf = 0.0D+00

      else

        y = ( x - a ) / b

        pdf = ( c / b ) * y**( c - 1.0D+00 )  / exp ( y**c )

      end if

      return
      end
      subroutine weibull_sample ( a, b, c, seed, x )

c*********************************************************************72
c
cc WEIBULL_SAMPLE samples the Weibull PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision cdf
      double precision r8_uniform_01
      integer seed
      double precision x

      cdf = r8_uniform_01 ( seed )

      call weibull_cdf_inv ( cdf, a, b, c, x )

      return
      end
      subroutine weibull_variance ( a, b, c, variance )

c*********************************************************************72
c
cc WEIBULL_VARIANCE returns the variance of the Weibull PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the parameters of the PDF.
c    0.0D+00 .lt. B,
c    0.0D+00 .lt. C.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision g1
      double precision g2
      double precision r8_gamma
      double precision variance

      g1 = r8_gamma ( ( c + 2.0D+00 ) / c )
      g2 = r8_gamma ( ( c + 1.0D+00 ) / c )

      variance = b * b * ( g1 - g2 * g2 )

      return
      end
      subroutine weibull_discrete_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc WEIBULL_DISCRETE_CDF evaluates the Discrete Weibull CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the CDF.
c    0 .le. X.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A .le. 1.0D+00,
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      integer x

      if ( x .lt. 0 ) then
        cdf = 0.0D+00
      else
        cdf = 1.0D+00 - ( 1.0D+00 - a )**((x+1)**b)
      end if

      return
      end
      subroutine weibull_discrete_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc WEIBULL_DISCRETE_CDF_INV inverts the Discrete Weibull CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 October 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A .le. 1.0D+00,
c    0.0D+00 .lt. B.
c
c    Output, integer X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      integer r8_ceiling
      integer x

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEIBULL_DISCRETE_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      x = r8_ceiling (     ( log ( 1.0D+00 - cdf ) / log ( 1.0D+00 - a )
     & )**( 1.0D+00 / b ) - 1.0D+00 )

      return
      end
      function weibull_discrete_check ( a, b )

c*********************************************************************72
c
cc WEIBULL_DISCRETE_CHECK checks the parameters of the discrete Weibull CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A .le. 1.0D+00,
c    0.0D+00 .lt. B.
c
c    Output, logical WEIBULL_DISCRETE_CHECK, is true if the parameters
c    are legal.
c
      implicit none

      double precision a
      double precision b
      logical weibull_discrete_check

      if ( a .lt. 0.0D+00 .or. 1.0D+00 .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEIBULL_DISCRETE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .lt. 0 or 1 .lt. A.'
        weibull_discrete_check = .false.
        return
      end if

      if ( b .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEIBULL_DISCRETE_CHECK - Fatal error!'
        write ( *, '(a)' ) '  B .le. 0.'
        weibull_discrete_check = .false.
        return
      end if

      weibull_discrete_check = .true.

      return
      end
      subroutine weibull_discrete_pdf ( x, a, b, pdf )

c*********************************************************************72
c
cc WEIBULL_DISCRETE_PDF evaluates the discrete Weibull PDF.
c
c  Discussion:
c
c    PDF(A,B;X) = ( 1 - A )^X^B - ( 1 - A )^(X+1)^B.
c
c    WEIBULL_DISCRETE_PDF(A,1;X) = GEOMETRIC_PDF(A;X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c    0 .le. X
c
c    Input, double precision A, B, the parameters that define the PDF.
c    0 .le. A .le. 1,
c    0 .lt. B.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision pdf
      integer x

      if ( x .lt. 0 ) then
        pdf = 0.0D+00
      else
        pdf = ( 1.0D+00 - a )**(x**b) - ( 1.0D+00 - a )**((x+1)**b)
      end if

      return
      end
      subroutine weibull_discrete_sample ( a, b, seed, x )

c*********************************************************************72
c
cc WEIBULL_DISCRETE_SAMPLE samples the discrete Weibull PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .le. A .le. 1.0D+00,
c    0.0D+00 .lt. B.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision r8_uniform_01
      integer seed
      integer x

      cdf = r8_uniform_01 ( seed )

      call weibull_discrete_cdf_inv ( cdf, a, b, x )

      return
      end
      function zeta ( p )

c*********************************************************************72
c
cc ZETA estimates the Riemann Zeta function.
c
c  Discussion:
c
c    For 1 .lt. P, the Riemann Zeta function is defined as:
c
c      ZETA ( P ) = Sum ( 1 .le. N .lt. Infinity ) 1 / N**P
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, double precision P, the power to which the integers are raised.
c    P must be greater than 1.  For integral P up to 20, a
c    precomputed value of ZETA is returned; otherwise the infinite
c    sum is approximated.
c
c    Output, double precision ZETA, an approximation to the Riemann
c    Zeta function.
c
      implicit none

      integer n
      double precision p
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision zsum
      double precision zsum_old
      double precision zeta

      if ( p .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ZETA - Fatal error!'
        write ( *, '(a)' ) '  Exponent P .le. 1.0.'
        stop
      else if ( p .eq. 2.0D+00 ) then
        zeta = pi**2 / 6.0D+00
      else if ( p .eq. 3.0D+00 ) then
        zeta = 1.2020569032D+00
      else if ( p .eq. 4.0D+00 ) then
        zeta = pi**4 / 90.0D+00
      else if ( p .eq. 5.0D+00 ) then
        zeta = 1.0369277551D+00
      else if ( p .eq. 6.0D+00 ) then
        zeta = pi**6 / 945.0D+00
      else if ( p .eq. 7.0D+00 ) then
        zeta = 1.0083492774D+00
      else if ( p .eq. 8.0D+00 ) then
        zeta = pi**8 / 9450.0D+00
      else if ( p .eq. 9.0D+00 ) then
        zeta = 1.0020083928D+00
      else if ( p .eq. 10.0D+00 ) then
        zeta = pi**10 / 93555.0D+00
      else if ( p .eq. 11.0D+00 ) then
        zeta = 1.0004941886D+00
      else if ( p .eq. 12.0D+00 ) then
        zeta = 1.0002460866D+00
      else if ( p .eq. 13.0D+00 ) then
        zeta = 1.0001227133D+00
      else if ( p .eq. 14.0D+00 ) then
        zeta = 1.0000612482D+00
      else if ( p .eq. 15.0D+00 ) then
        zeta = 1.0000305882D+00
      else if ( p .eq. 16.0D+00 ) then
        zeta = 1.0000152823D+00
      else if ( p .eq. 17.0D+00 ) then
        zeta = 1.0000076372D+00
      else if ( p .eq. 18.0D+00 ) then
        zeta = 1.0000038173D+00
      else if ( p .eq. 19.0D+00 ) then
        zeta = 1.0000019082D+00
      else if ( p .eq. 20.0D+00 ) then
        zeta = 1.0000009540D+00
      else

        zsum = 0.0D+00
        n = 0

10      continue

          n = n + 1
          zsum_old = zsum
          zsum = zsum + 1.0D+00 / ( dble ( n ) )**p
          if ( zsum .le. zsum_old ) then
            go to 20
          end if

        go to 10

20      continue

        zeta = zsum

      end if

      return
      end
      subroutine zipf_cdf ( x, a, cdf )

c*********************************************************************72
c
cc ZIPF_CDF evaluates the Zipf CDF.
c
c  Discussion:
c
c    Simple summation is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c    1 .le. N
c
c    Input, double precision A, the parameter of the PDF.
c    1.0D+00 .lt. A.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision asave
      double precision c
      double precision cdf
      double precision pdf
      integer x
      integer y
      double precision zeta

      save asave
      save c

      data asave / 0.0D+00 /
      data c / 0.0D+00 /

      if ( x .lt. 1 ) then

        cdf = 0.0D+00

      else

        if ( a .ne. asave ) then

          c = zeta ( a )
          asave = a

        end if

        cdf = 0.0D+00
        do y = 1, x
          pdf = ( 1.0D+00 / dble ( y )**a ) / c
          cdf = cdf + pdf
        end do

      end if

      return
      end
      function zipf_check ( a )

c*********************************************************************72
c
cc ZIPF_CHECK checks the parameter of the Zipf PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    1.0D+00 .lt. A.
c
c    Output, logical ZIPF_CHECK, is true if the parameters are legal.
c
      implicit none

      double precision a
      logical zipf_check

      if ( a .le. 1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ZIPF_CHECK - Fatal error!'
        write ( *, '(a)' ) '  A .le. 1.'
        zipf_check = .false.
        return
      end if

      zipf_check = .true.

      return
      end
      subroutine zipf_mean ( a, mean )

c*********************************************************************72
c
cc ZIPF_MEAN returns the mean of the Zipf PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    1.0D+00 .lt. A.
c
c    Output, double precision MEAN, the mean of the PDF.
c    The mean is only defined for 2 .lt. A.
c
      implicit none

      double precision a
      double precision mean
      double precision zeta

      if ( a .le. 2.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ZIPF_MEAN - Fatal error!'
        write ( *, '(a)' ) '  No mean defined for A .le. 2.'
        stop
      end if

      mean = zeta ( a - 1.0D+00 ) / zeta ( a )

      return
      end
      subroutine zipf_pdf ( x, a, pdf )

c*********************************************************************72
c
cc ZIPF_PDF evaluates the Zipf PDF.
c
c  Discussion:
c
c    PDF(A;X) = ( 1 / X**A ) / C
c
c    where the normalizing constant is chosen so that
c
c    C = Sum ( 1 .le. I .lt. Infinity ) 1 / I**A.
c
c    From observation, the frequency of different words in long
c    sequences of text seems to follow the Zipf PDF, with
c    parameter A slightly greater than 1.  The Zipf PDF is sometimes
c    known as the "discrete Pareto" PDF.
c
c    Lotka's law is a version of the Zipf PDF in which A is 2 or approximately
c    2.  Lotka's law describes the frequency of publications by authors in a
c    given field, and estimates that the number of authors with X papers is
c    about 1/X^A of the number of authors with 1 paper.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Alfred Lotka,
c    The frequency distribution of scientific productivity,
c    Journal of the Washington Academy of Sciences,
c    Volume 16, Number 12, 1926, pages 317-324.
c
c  Parameters:
c
c    Input, integer X, the argument of the PDF.
c    1 .le. N
c
c    Input, double precision A, the parameter of the PDF.
c    1.0D+00 .lt. A.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision a
      double precision asave
      double precision c
      double precision pdf
      integer x
      double precision zeta

      save asave
      save c

      data asave / 0.0D+00 /
      data c / 0.0D+00 /

      if ( x .lt. 1 ) then

        pdf = 0.0D+00

      else

        if ( a .ne. asave ) then

          c = zeta ( a )
          asave = a

        end if

        pdf = ( 1.0D+00 / dble ( x )**a ) / c

      end if

      return
      end
      subroutine zipf_sample ( a, seed, x )

c*********************************************************************72
c
cc ZIPF_SAMPLE samples the Zipf PDF.
c
c  Discussion:
c
c    I am concerned that there may be a discrepancy in the results
c    of this routine, which do not seem to have the predicted variances.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Luc Devroye,
c    Non-Uniform Random Variate Generation,
c    Springer Verlag, 1986, pages 550-551.
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    1.0D+00 .lt. A.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer X, a sample of the PDF.
c
      implicit none

      double precision a
      double precision b
      integer i4_huge
      double precision r8_uniform_01
      integer seed
      double precision t
      double precision test
      double precision u
      double precision v
      double precision w
      integer x

      test = dble ( i4_huge ( ) )

      b = 2.0D+00**( a - 1.0D+00 )

10    continue

        u = r8_uniform_01 ( seed )
        v = r8_uniform_01 ( seed )
        w = aint ( 1.0D+00 / u**( 1.0D+00 / ( a - 1.0D+00 ) ) )
c
c  Very small values of U can cause W to be very large,
c  bigger than the largest integer...
c
        if ( test .lt. w ) then
          go to 10
        end if

        t = ( ( w + 1.0D+00 ) / w )**( a - 1.0D+00 )

        if ( v * w * ( t - 1.0D+00 ) * b .le. t * ( b - 1.0D+00 ) ) then
          go to 20
        end if

      go to 10

20    continue

      x = int ( w )

      return
      end
      subroutine zipf_variance ( a, variance )

c*********************************************************************72
c
cc ZIPF_VARIANCE returns the variance of the Zipf PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the parameter of the PDF.
c    1.0D+00 .lt. A.
c
c    Output, double precision VARIANCE, the variance of the PDF.
c    The variance is only defined for 3 .lt. A.
c
      implicit none

      double precision a
      double precision mean
      double precision variance
      double precision zeta

      if ( a .le. 3.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ZIPF_VARIANCE - Fatal error!'
        write ( *, '(a)' ) '  No variance defined for A .le. 3.0.'
        stop
      end if

      call zipf_mean ( a, mean )

      variance = zeta ( a - 2.0D+00 ) / zeta ( a ) - mean * mean

      return
      end
