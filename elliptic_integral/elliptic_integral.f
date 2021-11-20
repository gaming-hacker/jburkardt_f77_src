      function elliptic_ea ( a )

c*********************************************************************72
c
cc ELLIPTIC_EA evaluates the complete elliptic integral E(A).
c
c  Discussion:
c
c    The value is computed using Carlson elliptic integrals:
c
c      E(a) = RF ( 0, 1-sin^2(a), 1 ) - 1/3 sin^2(a) RD ( 0, 1-sin^2(a), 1 ).
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
c  Parameters:
c
c    Input, double precision A, the argument.
c
c    Output, double precision ELLIPTIC_EA, the function value.
c
      implicit none

      double precision a
      double precision elliptic_ea
      double precision errtol
      integer ierr
      double precision k
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rd
      double precision rf
      double precision value
      double precision x
      double precision y
      double precision z

      k = sin ( a * r8_pi / 180.0D+00 )

      x = 0.0D+00
      y = ( 1.0D+00 - k ) * ( 1.0D+00 + k )
      z = 1.0D+00
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr ) 
     &  - k * k * rd ( x, y, z, errtol, ierr ) / 3.0D+00

      elliptic_ea = value

      return
      end
      subroutine elliptic_ea_values ( n_data, x, fx )

c*********************************************************************72
c
cc ELLIPTIC_EA_VALUES: values of the complete elliptic integral E(ALPHA).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the second kind.
c
c    The function is defined by the formula:
c
c      E(ALPHA) = integral ( 0 <= T <= PI/2 )
c        sqrt ( 1 - sin ( ALPHA )^2 * sin ( T )^2 ) dT
c
c    In Mathematica, the function can be evaluated by:
c
c      EllipticE[(Sin[Pi*alpha/180])^2]
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
c    Output, double precision X, the argument of the function, measured
c    in degrees.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 18 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save x_vec

      data fx_vec /
     &  1.570796326794897D+00,
     &  1.567809073977622D+00,
     &  1.558887196601596D+00,
     &  1.544150496914673D+00,
     &  1.523799205259774D+00,
     &  1.498114928422116D+00,
     &  1.467462209339427D+00,
     &  1.432290969306756D+00,
     &  1.393140248523812D+00,
     &  1.350643881047676D+00,
     &  1.305539094297794D+00,
     &  1.258679624779997D+00,
     &  1.211056027568459D+00,
     &  1.163827964493139D+00,
     &  1.118377737969864D+00,
     &  1.076405113076403D+00,
     &  1.040114395706010D+00,
     &  1.012663506234396D+00 /
      data x_vec /
     &   0.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  15.0D+00,
     &  20.0D+00,
     &  25.0D+00,
     &  30.0D+00,
     &  35.0D+00,
     &  40.0D+00,
     &  45.0D+00,
     &  50.0D+00,
     &  55.0D+00,
     &  60.0D+00,
     &  65.0D+00,
     &  70.0D+00,
     &  75.0D+00,
     &  80.0D+00,
     &  85.0D+00 /

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
      function elliptic_ek ( k )

c*********************************************************************72
c
cc ELLIPTIC_EK evaluates the complete elliptic integral E(K).
c
c  Discussion:
c
c    The value is computed using Carlson elliptic integrals:
c
c      E(k) = RF ( 0, 1-k^2, 1 ) - 1/3 k^2 RD ( 0, 1-k^2, 1 ).
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
c  Parameters:
c
c    Input, double precision K, the argument.
c
c    Output, double precision ELLIPTIC_EK, the function value.
c
      implicit none

      double precision elliptic_ek
      double precision errtol
      integer ierr
      double precision k
      double precision rd
      double precision rf
      double precision value
      double precision x
      double precision y
      double precision z

      x = 0.0D+00
      y = ( 1.0D+00 - k ) * ( 1.0D+00 + k )
      z = 1.0D+00
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr ) 
     &  - k * k * rd ( x, y, z, errtol, ierr ) / 3.0D+00

      elliptic_ek = value

      return
      end
      subroutine elliptic_ek_values ( n_data, x, fx )

c*********************************************************************72
c
cc ELLIPTIC_EK_VALUES returns values of the complete elliptic integral E(K).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the second kind.
c
c    The function is defined by the formula:
c
c      E(K) = integral ( 0 <= T <= PI/2 )
c        sqrt ( 1 - K^2 * sin ( T )^2 ) dT
c
c    In Mathematica, the function can be evaluated by:
c
c      EllipticE[k^2]
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
     &  1.570796326794897D+00,
     &  1.550973351780472D+00,
     &  1.530757636897763D+00,
     &  1.510121832092819D+00,
     &  1.489035058095853D+00,
     &  1.467462209339427D+00,
     &  1.445363064412665D+00,
     &  1.422691133490879D+00,
     &  1.399392138897432D+00,
     &  1.375401971871116D+00,
     &  1.350643881047676D+00,
     &  1.325024497958230D+00,
     &  1.298428035046913D+00,
     &  1.270707479650149D+00,
     &  1.241670567945823D+00,
     &  1.211056027568459D+00,
     &  1.178489924327839D+00,
     &  1.143395791883166D+00,
     &  1.104774732704073D+00,
     &  1.060473727766278D+00 /
      data x_vec /
     &  0.0000000000000000D+00,
     &  0.2236067977499790D+00,
     &  0.3162277660168379D+00,
     &  0.3872983346207417D+00,
     &  0.4472135954999579D+00,
     &  0.5000000000000000D+00,
     &  0.5477225575051661D+00,
     &  0.5916079783099616D+00,
     &  0.6324555320336759D+00,
     &  0.6708203932499369D+00,
     &  0.7071067811865476D+00,
     &  0.7416198487095663D+00,
     &  0.7745966692414834D+00,
     &  0.8062257748298550D+00,
     &  0.8366600265340756D+00,
     &  0.8660254037844386D+00,
     &  0.8944271909999159D+00,
     &  0.9219544457292888D+00,
     &  0.9486832980505138D+00,
     &  0.9746794344808963D+00 /

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
      function elliptic_em ( m )

c*********************************************************************72
c
cc ELLIPTIC_EM evaluates the complete elliptic integral E(M).
c
c  Discussion:
c
c    The value is computed using Carlson elliptic integrals:
c
c      E(m) = RF ( 0, 1-m, 1 ) - 1/3 m RD ( 0, 1-m, 1 ).
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
c  Parameters:
c
c    Input, double precision M, the argument.
c
c    Output, double precision ELLIPTIC_EM, the function value.
c
      implicit none

      double precision elliptic_em
      double precision errtol
      integer ierr
      double precision m
      double precision rd
      double precision rf
      double precision value
      double precision x
      double precision y
      double precision z

      x = 0.0D+00
      y = 1.0D+00 - m
      z = 1.0D+00
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr ) 
     &  - m * rd ( x, y, z, errtol, ierr ) / 3.0D+00

      elliptic_em = value

      return
      end
      subroutine elliptic_em_values ( n_data, x, fx )

c*********************************************************************72
c
cc ELLIPTIC_EM_VALUES returns values of the complete elliptic integral E(M).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the second kind.
c
c    The function is defined by the formula:
c
c      E(M) = integral ( 0 <= T <= PI/2 )
c        sqrt ( 1 - M * sin ( T )^2 ) dT
c
c    In Mathematica, the function can be evaluated by:
c
c      EllipticE[m]
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
     &  1.570796326794897D+00,
     &  1.550973351780472D+00,
     &  1.530757636897763D+00,
     &  1.510121832092819D+00,
     &  1.489035058095853D+00,
     &  1.467462209339427D+00,
     &  1.445363064412665D+00,
     &  1.422691133490879D+00,
     &  1.399392138897432D+00,
     &  1.375401971871116D+00,
     &  1.350643881047676D+00,
     &  1.325024497958230D+00,
     &  1.298428035046913D+00,
     &  1.270707479650149D+00,
     &  1.241670567945823D+00,
     &  1.211056027568459D+00,
     &  1.178489924327839D+00,
     &  1.143395791883166D+00,
     &  1.104774732704073D+00,
     &  1.060473727766278D+00 /
      data x_vec /
     &  0.00D+00,
     &  0.05D+00,
     &  0.10D+00,
     &  0.15D+00,
     &  0.20D+00,
     &  0.25D+00,
     &  0.30D+00,
     &  0.35D+00,
     &  0.40D+00,
     &  0.45D+00,
     &  0.50D+00,
     &  0.55D+00,
     &  0.60D+00,
     &  0.65D+00,
     &  0.70D+00,
     &  0.75D+00,
     &  0.80D+00,
     &  0.85D+00,
     &  0.90D+00,
     &  0.95D+00  /

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
      function elliptic_fa ( a )

c*********************************************************************72
c
cc ELLIPTIC_FA evaluates the complete elliptic integral F(A).
c
c  Discussion:
c
c    The value is computed using Carlson elliptic integrals:
c
c      F(a) = RF ( 0, 1-sin^2(a), 1 ).
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
c  Parameters:
c
c    Input, double precision A, the argument.
c
c    Output, double precision ELLIPTIC_FA, the function value.
c
      implicit none

      double precision a
      double precision elliptic_fa
      double precision errtol
      integer ierr
      double precision k
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rf
      double precision value
      double precision x
      double precision y
      double precision z

      k = sin ( a * r8_pi / 180.0D+00 )
      x = 0.0D+00
      y = ( 1.0D+00 - k ) * ( 1.0D+00 + k )
      z = 1.0D+00
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr )

      elliptic_fa = value

      return
      end
      subroutine elliptic_fa_values ( n_data, x, fx )

c*********************************************************************72
c
cc ELLIPTIC_FA_VALUES: values of the complete elliptic integral F(ALPHA).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the first kind.
c
c    The function is defined by the formula:
c
c      F(ALPHA) = integral ( 0 <= T <= PI/2 )
c        dT / sqrt ( 1 - sin ( ALPHA )^2 * sin ( T )^2 )
c
c    In Mathematica, the function can be evaluated by:
c
c      EllipticK[(Sin[alpha*Pi/180])^2]
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
c    Output, double precision X, the argument of the function, measured
c    in degrees.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 18 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save x_vec

      data fx_vec /
     &  0.1570796326794897D+01,
     &  0.1573792130924768D+01,
     &  0.1582842804338351D+01,
     &  0.1598142002112540D+01,
     &  0.1620025899124204D+01,
     &  0.1648995218478530D+01,
     &  0.1685750354812596D+01,
     &  0.1731245175657058D+01,
     &  0.1786769134885021D+01,
     &  0.1854074677301372D+01,
     &  0.1935581096004722D+01,
     &  0.2034715312185791D+01,
     &  0.2156515647499643D+01,
     &  0.2308786798167196D+01,
     &  0.2504550079001634D+01,
     &  0.2768063145368768D+01,
     &  0.3153385251887839D+01,
     &  0.3831741999784146D+01 /
      data x_vec /
     &   0.0D+00,
     &   5.0D+00,
     &  10.0D+00,
     &  15.0D+00,
     &  20.0D+00,
     &  25.0D+00,
     &  30.0D+00,
     &  35.0D+00,
     &  40.0D+00,
     &  45.0D+00,
     &  50.0D+00,
     &  55.0D+00,
     &  60.0D+00,
     &  65.0D+00,
     &  70.0D+00,
     &  75.0D+00,
     &  80.0D+00,
     &  85.0D+00 /

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
      function elliptic_fk ( k )

c*********************************************************************72
c
cc ELLIPTIC_FK evaluates the complete elliptic integral F(K).
c
c  Discussion:
c
c    The value is computed using Carlson elliptic integrals:
c
c      F(k) = RF ( 0, 1-k^2, 1 ).
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
c  Parameters:
c
c    Input, double precision K, the argument.
c
c    Output, double precision ELLIPTIC_FK, the function value.
c
      implicit none

      double precision elliptic_fk
      double precision errtol
      integer ierr
      double precision k
      double precision rf
      double precision value
      double precision x
      double precision y
      double precision z

      x = 0.0D+00
      y = ( 1.0D+00 - k ) * ( 1.0D+00 + k )
      z = 1.0D+00
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr )

      elliptic_fk = value

      return
      end
      subroutine elliptic_fk_values ( n_data, x, fx )

c*********************************************************************72
c
cc ELLIPTIC_FK_VALUES returns values of the complete elliptic integral F(K).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the first kind.
c
c    The function is defined by the formula:
c
c      F(K) = integral ( 0 <= T <= PI/2 )
c        dT / sqrt ( 1 - K^2 * sin ( T )^2 )
c
c    In Mathematica, the function can be evaluated by:
c
c      EllipticK[k^2]
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
     &  1.570796326794897D+00,
     &  1.591003453790792D+00,
     &  1.612441348720219D+00,
     &  1.635256732264580D+00,
     &  1.659623598610528D+00,
     &  1.685750354812596D+00,
     &  1.713889448178791D+00,
     &  1.744350597225613D+00,
     &  1.777519371491253D+00,
     &  1.813883936816983D+00,
     &  1.854074677301372D+00,
     &  1.898924910271554D+00,
     &  1.949567749806026D+00,
     &  2.007598398424376D+00,
     &  2.075363135292469D+00,
     &  2.156515647499643D+00,
     &  2.257205326820854D+00,
     &  2.389016486325580D+00,
     &  2.578092113348173D+00,
     &  2.908337248444552D+00 /
      data x_vec /
     &  0.0000000000000000D+00,
     &  0.2236067977499790D+00,
     &  0.3162277660168379D+00,
     &  0.3872983346207417D+00,
     &  0.4472135954999579D+00,
     &  0.5000000000000000D+00,
     &  0.5477225575051661D+00,
     &  0.5916079783099616D+00,
     &  0.6324555320336759D+00,
     &  0.6708203932499369D+00,
     &  0.7071067811865476D+00,
     &  0.7416198487095663D+00,
     &  0.7745966692414834D+00,
     &  0.8062257748298550D+00,
     &  0.8366600265340756D+00,
     &  0.8660254037844386D+00,
     &  0.8944271909999159D+00,
     &  0.9219544457292888D+00,
     &  0.9486832980505138D+00,
     &  0.9746794344808963D+00 /

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
      function elliptic_fm ( m )

c*********************************************************************72
c
cc ELLIPTIC_FM evaluates the complete elliptic integral F(M).
c
c  Discussion:
c
c    The value is computed using Carlson elliptic integrals:
c
c      F(m) = RF ( 0, 1-m, 1 ).
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
c  Parameters:
c
c    Input, double precision M, the argument.
c
c    Output, double precision ELLIPTIC_FM, the function value.
c
      implicit none

      double precision elliptic_fm
      double precision errtol
      integer ierr
      double precision m
      double precision rf
      double precision value
      double precision x
      double precision y
      double precision z

      x = 0.0D+00
      y = 1.0D+00 - m
      z = 1.0D+00
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr )

      elliptic_fm = value

      return
      end
      subroutine elliptic_fm_values ( n_data, x, fx )

c*********************************************************************72
c
cc ELLIPTIC_FM_VALUES returns values of the complete elliptic integral F(M).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the first kind.
c
c    The function is defined by the formula:
c
c      F(M) = integral ( 0 <= T <= PI/2 )
c        dT / sqrt ( 1 - M * sin ( T )^2 )
c
c    In Mathematica, the function can be evaluated by:
c
c      EllipticK[m]
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
     &  1.570796326794897D+00,
     &  1.591003453790792D+00,
     &  1.612441348720219D+00,
     &  1.635256732264580D+00,
     &  1.659623598610528D+00,
     &  1.685750354812596D+00,
     &  1.713889448178791D+00,
     &  1.744350597225613D+00,
     &  1.777519371491253D+00,
     &  1.813883936816983D+00,
     &  1.854074677301372D+00,
     &  1.898924910271554D+00,
     &  1.949567749806026D+00,
     &  2.007598398424376D+00,
     &  2.075363135292469D+00,
     &  2.156515647499643D+00,
     &  2.257205326820854D+00,
     &  2.389016486325580D+00,
     &  2.578092113348173D+00,
     &  2.908337248444552D+00 /
      data x_vec /
     &   0.00D+00,
     &   0.05D+00,
     &   0.10D+00,
     &   0.15D+00,
     &   0.20D+00,
     &   0.25D+00,
     &   0.30D+00,
     &   0.35D+00,
     &   0.40D+00,
     &   0.45D+00,
     &   0.50D+00,
     &   0.55D+00,
     &   0.60D+00,
     &   0.65D+00,
     &   0.70D+00,
     &   0.75D+00,
     &   0.80D+00,
     &   0.85D+00,
     &   0.90D+00,
     &   0.95D+00 /

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
      function elliptic_pia ( n, a )

c*********************************************************************72
c
cc ELLIPTIC_PIA evaluates the complete elliptic integral Pi(N,A).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the third kind.
c
c    The function is defined by the formula:
c
c      Pi(N,A) = integral ( 0 <= T <= PI/2 )
c        dT / (1 - N sin^2(T) ) sqrt ( 1 - sin^2(A) * sin ( T )^2 )
c
c    In MATLAB, the function can be evaluated by:
c
c      ellipticPi(n,(sin(a*pi/180)^2)
c
c    The value is computed using Carlson elliptic integrals:
c
c      k = sin ( a * pi / 180 )
c      Pi(n,k) = RF ( 0, 1 - k^2, 1 ) + 1/3 n RJ ( 0, 1 - k^2, 1, 1 - n )
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
c  Parameters:
c
c    Input, double precision N, A, the arguments.
c
c    Output, double precision ELLIPTIC_PIA, the function value.
c
      implicit none

      double precision a
      double precision elliptic_pia
      double precision errtol
      integer ierr
      double precision k
      double precision n
      double precision p
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rf
      double precision rj
      double precision value
      double precision x
      double precision y
      double precision z

      k = sin ( a * r8_pi / 180.0D+00 )
      x = 0.0D+00
      y = ( 1.0D+00 - k ) * ( 1.0D+00 + k )
      z = 1.0D+00
      p = 1.0D+00 - n
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr )
     &  + n * rj ( x, y, z, p, errtol, ierr ) / 3.0D+00

      elliptic_pia = value

      return
      end
      subroutine elliptic_pia_values ( n_data, n, a, pia )

c*********************************************************************72
c
cc ELLIPTIC_PIA_VALUES returns values of the complete elliptic integral Pi(N,A).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the third kind.
c
c    The function is defined by the formula:
c
c      Pi(N,A) = integral ( 0 <= T <= PI/2 )
c        dT / (1 - N sin^2(T) ) sqrt ( 1 - sin^2(A) * sin ( T )^2 )
c
c    In MATLAB, the function can be evaluated by:
c
c      ellipticPi(n,(sin(A*pi/180))^2)
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
c    Output, double precision N, A, the arguments of the function.
c
c    Output, double precision PIA, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision a
      double precision a_vec(n_max)
      double precision n
      integer n_data
      double precision n_vec(n_max)
      double precision pia
      double precision pia_vec(n_max)

      save a_vec
      save n_vec
      save pia_vec

      data a_vec /
     & 30.00000000000000D+00,
     & 45.00000000000000D+00,
     & 60.00000000000000D+00,
     & 77.07903361841643D+00,
     & 30.00000000000000D+00,
     & 45.00000000000000D+00,
     & 60.00000000000000D+00,
     & 77.07903361841643D+00,
     & 30.00000000000000D+00,
     & 45.00000000000000D+00,
     & 60.00000000000000D+00,
     & 77.07903361841643D+00,
     & 30.00000000000000D+00,
     & 45.00000000000000D+00,
     & 60.00000000000000D+00,
     & 77.07903361841643D+00,
     & 30.00000000000000D+00,
     & 45.00000000000000D+00,
     & 60.00000000000000D+00,
     & 77.07903361841643D+00 /

      data n_vec /
     & -10.0D+00,
     & -10.0D+00,
     & -10.0D+00,
     & -10.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00 /

      data pia_vec /
     & 0.4892245275965397D+00,
     & 0.5106765677902629D+00,
     & 0.5460409271920561D+00,
     & 0.6237325893535237D+00,
     & 0.823045542660675D+00,
     & 0.8760028274011437D+00,
     & 0.9660073560143946D+00,
     & 1.171952391481798D+00,
     & 1.177446843000566D+00,
     & 1.273127366749682D+00,
     & 1.440034318657551D+00,
     & 1.836472172302591D+00,
     & 1.685750354812596D+00,
     & 1.854074677301372D+00,
     & 2.156515647499643D+00,
     & 2.908337248444552D+00,
     & 2.413671504201195D+00,
     & 2.701287762095351D+00,
     & 3.234773471249465D+00,
     & 4.633308147279891D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        a = 0.0D+00
        n = 0.0D+00
        pia = 0.0D+00
      else
        a = a_vec(n_data)
        n = n_vec(n_data)
        pia = pia_vec(n_data)
      end if

      return
      end
      function elliptic_pik ( n, k )

c*********************************************************************72
c
cc ELLIPTIC_PIK evaluates the complete elliptic integral Pi(N,K).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the third kind.
c
c    The function is defined by the formula:
c
c      Pi(N,K) = integral ( 0 <= T <= PI/2 )
c        dT / (1 - N sin^2(T) ) sqrt ( 1 - K^2 * sin ( T )^2 )
c
c    In MATLAB, the function can be evaluated by:
c
c      ellipticPi(n,k^2)
c
c    The value is computed using Carlson elliptic integrals:
c
c      Pi(n,k) = RF ( 0, 1 - k^2, 1 ) + 1/3 n RJ ( 0, 1 - k^2, 1, 1 - n )
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
c  Parameters:
c
c    Input, double precision N, K, the arguments.
c
c    Output, double precision ELLIPTIC_PIK, the function value.
c
      implicit none

      double precision elliptic_pik
      double precision errtol
      integer ierr
      double precision k
      double precision n
      double precision p
      double precision rf
      double precision rj
      double precision value
      double precision x
      double precision y
      double precision z

      x = 0.0D+00
      y = ( 1.0D+00 - k ) * ( 1.0D+00 + k )
      z = 1.0D+00
      p = 1.0D+00 - n
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr )
     &  + n * rj ( x, y, z, p, errtol, ierr ) / 3.0D+00

      elliptic_pik = value

      return
      end
      subroutine elliptic_pik_values ( n_data, n, k, pik )

c*********************************************************************72
c
cc ELLIPTIC_PIK_VALUES returns values of the complete elliptic integral Pi(N,K).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the third kind.
c
c    The function is defined by the formula:
c
c      Pi(N,K) = integral ( 0 <= T <= PI/2 )
c        dT / (1 - N sin^2(T) ) sqrt ( 1 - K^2 * sin ( T )^2 )
c
c    In MATLAB, the function can be evaluated by:
c
c      ellipticPi(n,k^2)
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
c    Output, double precision N, K, the arguments of the function.
c
c    Output, double precision PIK, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision k
      double precision k_vec(n_max)
      double precision n
      integer n_data
      double precision n_vec(n_max)
      double precision pik
      double precision pik_vec(n_max)

      save k_vec
      save n_vec
      save pik_vec

      data k_vec /
     & 0.5000000000000000D+00,
     & 0.7071067811865476D+00,
     & 0.8660254037844386D+00,
     & 0.9746794344808963D+00,
     & 0.5000000000000000D+00,
     & 0.7071067811865476D+00,
     & 0.8660254037844386D+00,
     & 0.9746794344808963D+00,
     & 0.5000000000000000D+00,
     & 0.7071067811865476D+00,
     & 0.8660254037844386D+00,
     & 0.9746794344808963D+00,
     & 0.5000000000000000D+00,
     & 0.7071067811865476D+00,
     & 0.8660254037844386D+00,
     & 0.9746794344808963D+00,
     & 0.5000000000000000D+00,
     & 0.7071067811865476D+00,
     & 0.8660254037844386D+00,
     & 0.9746794344808963D+00 /

      data n_vec /
     & -10.0D+00,
     & -10.0D+00,
     & -10.0D+00,
     & -10.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00 /

      data pik_vec /
     & 0.4892245275965397D+00,
     & 0.5106765677902629D+00,
     & 0.5460409271920561D+00,
     & 0.6237325893535237D+00,
     & 0.823045542660675D+00,
     & 0.8760028274011437D+00,
     & 0.9660073560143946D+00,
     & 1.171952391481798D+00,
     & 1.177446843000566D+00,
     & 1.273127366749682D+00,
     & 1.440034318657551D+00,
     & 1.836472172302591D+00,
     & 1.685750354812596D+00,
     & 1.854074677301372D+00,
     & 2.156515647499643D+00,
     & 2.908337248444552D+00,
     & 2.413671504201195D+00,
     & 2.701287762095351D+00,
     & 3.234773471249465D+00,
     & 4.633308147279891D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        k = 0.0D+00
        n = 0.0D+00
        pik = 0.0D+00
      else
        k = k_vec(n_data)
        n = n_vec(n_data)
        pik = pik_vec(n_data)
      end if

      return
      end
      function elliptic_pim ( n, m )

c*********************************************************************72
c
cc ELLIPTIC_PIM evaluates the complete elliptic integral Pi(N,M).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the third kind.
c
c    The function is defined by the formula:
c
c      Pi(N,M) = integral ( 0 <= T <= PI/2 )
c        dT / (1 - N sin^2(T) ) sqrt ( 1 - M * sin ( T )^2 )
c
c    In MATLAB, the function can be evaluated by:
c
c      ellipticPi(n,m)
c
c    The value is computed using Carlson elliptic integrals:
c
c      Pi(n,m) = RF ( 0, 1 - m, 1 ) + 1/3 n RJ ( 0, 1 - m, 1, 1 - n )
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
c  Parameters:
c
c    Input, double precision N, M, the arguments.
c
c    Output, double precision ELLIPTIC_PIM, the function value.
c
      implicit none

      double precision elliptic_pim
      double precision errtol
      integer ierr
      double precision m
      double precision n
      double precision p
      double precision rf
      double precision rj
      double precision value
      double precision x
      double precision y
      double precision z

      x = 0.0D+00
      y = 1.0D+00 - m
      z = 1.0D+00
      p = 1.0D+00 - n
      errtol = 1.0D-03

      value = rf ( x, y, z, errtol, ierr )
     &  + n * rj ( x, y, z, p, errtol, ierr ) / 3.0D+00

      elliptic_pim = value

      return
      end
      subroutine elliptic_pim_values ( n_data, n, m, pim )

c*********************************************************************72
c
cc ELLIPTIC_PIM_VALUES returns values of the complete elliptic integral Pi(N,M).
c
c  Discussion:
c
c    This is one form of what is sometimes called the complete elliptic
c    integral of the third kind.
c
c    The function is defined by the formula:
c
c      Pi(N,M) = integral ( 0 <= T <= PI/2 )
c        dT / (1 - N sin^2(T) ) sqrt ( 1 - M * sin ( T )^2 )
c
c    In MATLAB, the function can be evaluated by:
c
c      ellipticPi(n,m)
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
c    Output, double precision N, M, the arguments of the function.
c
c    Output, double precision PIM, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision m
      double precision m_vec(n_max)
      double precision n
      integer n_data
      double precision n_vec(n_max)
      double precision pim
      double precision pim_vec(n_max)

      save m_vec
      save n_vec
      save pim_vec

      data m_vec /
     & 0.25D+00,
     & 0.50D+00,
     & 0.75D+00,
     & 0.95D+00,
     & 0.25D+00,
     & 0.50D+00,
     & 0.75D+00,
     & 0.95D+00,
     & 0.25D+00,
     & 0.50D+00,
     & 0.75D+00,
     & 0.95D+00,
     & 0.25D+00,
     & 0.50D+00,
     & 0.75D+00,
     & 0.95D+00,
     & 0.25D+00,
     & 0.50D+00,
     & 0.75D+00,
     & 0.95D+00 /

      data n_vec /
     & -10.0D+00,
     & -10.0D+00,
     & -10.0D+00,
     & -10.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -3.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &  -1.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.0D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00,
     &   0.5D+00 /

      data pim_vec /
     & 0.4892245275965397D+00,
     & 0.5106765677902629D+00,
     & 0.5460409271920561D+00,
     & 0.6237325893535237D+00,
     & 0.823045542660675D+00,
     & 0.8760028274011437D+00,
     & 0.9660073560143946D+00,
     & 1.171952391481798D+00,
     & 1.177446843000566D+00,
     & 1.273127366749682D+00,
     & 1.440034318657551D+00,
     & 1.836472172302591D+00,
     & 1.685750354812596D+00,
     & 1.854074677301372D+00,
     & 2.156515647499643D+00,
     & 2.908337248444552D+00,
     & 2.413671504201195D+00,
     & 2.701287762095351D+00,
     & 3.234773471249465D+00,
     & 4.633308147279891D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        m = 0.0D+00
        n = 0.0D+00
        pim = 0.0D+00
      else
        m = m_vec(n_data)
        n = n_vec(n_data)
        pim = pim_vec(n_data)
      end if

      return
      end
      function rc ( x, y, errtol, ierr )

c*********************************************************************72
c
cc RC computes the elementary integral RC(X,Y).
c
c  Discussion:
c
c    This function computes the elementary integral
c
c      RC(X,Y) = Integral ( 0 <= T < oo )
c
c                              -1/2     -1
c                    (1/2)(T+X)    (T+Y)  DT,
c
c    where X is nonnegative and Y is positive.  The duplication
c    theorem is iterated until the variables are nearly equal,
c    and the function is then expanded in Taylor series to fifth
c    order.  
c
c    Logarithmic, inverse circular, and inverse hyperbolic 
c    functions can be expressed in terms of RC.  
c
c    Check by addition theorem: 
c
c      RC(X,X+Z) + RC(Y,Y+Z) = RC(0,Z),
c      where X, Y, and Z are positive and X * Y = Z * Z.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    Relative error due to truncation is less than
c      16 * ERRTOL ^ 6 / (1 - 2 * ERRTOL).
c    Sample choices:  
c      ERRTOL   Relative truncation error less than
c      1.D-3    2.D-17
c      3.D-3    2.D-14
c      1.D-2    2.D-11
c      3.D-2    2.D-8
c      1.D-1    2.D-5
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision c1
      double precision c2
      double precision errtol
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision rc
      double precision s
      double precision sn
      double precision uplim
      double precision x
      double precision xn
      double precision y
      double precision yn
c
c  LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
c  LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
c  UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
c
      save lolim
      save uplim

      data lolim /3.D-78/
      data uplim /1.D+75/

      if ( 
     &  x .lt. 0.0d0 .or.  
     &  y .le. 0.0d0 .or.
     &  ( x + y ) .lt. lolim .or.
     &  uplim .lt. x .or.
     &  uplim .lt. y ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RC - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a)' ) ''
        ierr = 1
        rc = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y

      do

        mu = ( xn + yn + yn ) / 3.0d0
        sn = ( yn + mu ) / mu - 2.0d0

        if ( abs ( sn ) .lt. errtol ) then
          c1 = 1.0d0 / 7.0d0
          c2 = 9.0d0 / 22.0d0
          s = sn * sn * ( 0.3d0 
     &      + sn * ( c1 + sn * ( 0.375d0 + sn * c2 ) ) )
          rc = ( 1.0d0 + s ) / sqrt ( mu )
          return
        end if

        lamda = 2.0d0 * sqrt ( xn ) * sqrt ( yn ) + yn
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0

      end do

      end
      function rd ( x, y, z, errtol, ierr )

c*********************************************************************72
c
cc RD computes an incomplete elliptic integral of the second kind, RD(X,Y,Z).
c
c  Discussion:
c
c    This function computes an incomplete elliptic integral of the second kind.
c
c    RD(X,Y,Z) = Integral ( 0 <= T < oo )
c
c                                -1/2     -1/2     -3/2
c                      (3/2)(T+X)    (T+Y)    (T+Z)    DT,
c
c    where X and Y are nonnegative, X + Y is positive, and Z is positive.
c
c    If X or Y is zero, the integral is complete.
c
c    The duplication theorem is iterated until the variables are
c    nearly equal, and the function is then expanded in Taylor
c    series to fifth order.  
c
c    Check: 
c
c      RD(X,Y,Z) + RD(Y,Z,X) + RD(Z,X,Y) = 3 / sqrt ( X * Y * Z ), 
c      where X, Y, and Z are positive.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    The relative error due to truncation is less than
c      3 * ERRTOL ^ 6 / (1-ERRTOL) ^ 3/2.
c    Sample choices:
c      ERRTOL   Relative truncation error less than
c      1.D-3    4.D-18
c      3.D-3    3.D-15
c      1.D-2    4.D-12
c      3.D-2    3.D-9
c      1.D-1    4.D-6
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision ea
      double precision eb
      double precision ec
      double precision ed
      double precision ef
      double precision epslon
      double precision errtol
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision power4
      double precision rd
      double precision sigma
      double precision s1
      double precision s2
      double precision uplim
      double precision x
      double precision xn
      double precision xndev
      double precision xnroot
      double precision y
      double precision yn
      double precision yndev
      double precision ynroot
      double precision z
      double precision zn
      double precision zndev
      double precision znroot
C
C  LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C  LOLIM IS NOT LESS THAN 2 / (MACHINE MAXIMUM) ^ (2/3).
C  UPLIM IS NOT GREATER THAN (0.1 * ERRTOL / MACHINE
C  MINIMUM) ^ (2/3), WHERE ERRTOL IS DESCRIBED BELOW.
C  IN THE FOLLOWING TABLE IT IS ASSUMED THAT ERRTOL WILL
C  NEVER BE CHOSEN SMALLER THAN 1.D-5.
C
      save lolim
      save uplim

      data lolim /6.D-51/
      data uplim /1.D+48/

      if ( 
     &  x .lt. 0.0D+00 .or.
     &  y .lt. 0.0D+00 .or.
     &  x + y .lt. lolim .or.
     &  z .lt. lolim .or.
     &  x .gt. uplim .or.
     &  y .gt. uplim .or.
     &  z .gt. uplim ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RD - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a,d23.16)' ) '  Z = ', z
        write ( *, '(a)' ) ''
        ierr = 1
        rd = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y
      zn = z
      sigma = 0.0d0
      power4 = 1.0d0

      do

        mu = ( xn + yn + 3.0d0 * zn ) * 0.2d0
        xndev = ( mu - xn ) / mu
        yndev = ( mu - yn ) / mu
        zndev = ( mu - zn ) / mu
        epslon = max ( abs ( xndev ), abs ( yndev ), abs ( zndev ) )

        if ( epslon .lt. errtol ) then
          c1 = 3.0d0 / 14.0d0
          c2 = 1.0d0 / 6.0d0
          c3 = 9.0d0 / 22.0d0
          c4 = 3.0d0 / 26.0d0
          ea = xndev * yndev
          eb = zndev * zndev
          ec = ea - eb
          ed = ea - 6.0d0 * eb
          ef = ed + ec + ec
          s1 = ed 
     &      * ( - c1 + 0.25d0 * c3 * ed - 1.5d0 * c4 * zndev * ef )
          s2 = zndev 
     &      * ( c2 * ef + zndev * ( - c3 * ec + zndev * c4 * ea ) )
          rd = 3.0d0 * sigma 
     &      + power4 * ( 1.0d0 + s1 + s2 ) / ( mu * sqrt ( mu ) )

          return
        end if

        xnroot = sqrt ( xn )
        ynroot = sqrt ( yn )
        znroot = sqrt ( zn )
        lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot
        sigma = sigma + power4 / ( znroot * ( zn + lamda ) )
        power4 = power4 * 0.25d0
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0
        zn = ( zn + lamda ) * 0.25d0

      end do

      end
      function rf ( x, y, z, errtol, ierr )

c*********************************************************************72
c
cc RF computes an incomplete elliptic integral of the first kind, RF(X,Y,Z).
c
c  Discussion:
c
c    This function computes the incomplete elliptic integral of the first kind.
c
c    RF(X,Y,Z) = Integral ( 0 <= T < oo )
c
c                                -1/2     -1/2     -1/2
c                      (1/2)(T+X)    (T+Y)    (T+Z)    DT,
c
c    where X, Y, and Z are nonnegative and at most one of them is zero.
c
c    If X or Y or Z is zero, the integral is complete.
c
c    The duplication theorem is iterated until the variables are
c    nearly equal, and the function is then expanded in Taylor
c    series to fifth order.  
c
c    Check by addition theorem: 
c
c      RF(X,X+Z,X+W) + RF(Y,Y+Z,Y+W) = RF(0,Z,W), 
c      where X, Y, Z, W are positive and X * Y = Z * W.
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    Relative error due to truncation is less than
c      ERRTOL ^ 6 / (4 * (1 - ERRTOL)).
c    Sample choices:
c      ERRTOL   Relative truncation error less than
c      1.D-3    3.D-19
c      3.D-3    2.D-16
c      1.D-2    3.D-13
c      3.D-2    2.D-10
c      1.D-1    3.D-7
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision e2
      double precision e3
      double precision epslon
      double precision errtol
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision rf
      double precision s
      double precision uplim
      double precision x
      double precision xn
      double precision xndev
      double precision xnroot
      double precision y
      double precision yn
      double precision yndev
      double precision ynroot
      double precision z
      double precision zn
      double precision zndev
      double precision znroot
c
c  LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
c  LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
c  UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
c
      save lolim
      save uplim

      data lolim /3.D-78/
      data uplim /1.D+75/

      if (
     &  x .lt. 0.0D+00 .or.
     &  y .lt. 0.0D+00 .or.
     &  z .lt. 0.0D+00 .or.
     &  x + y .lt. lolim .or.
     &  x + z .lt. lolim .or.
     &  y + z .lt. lolim .or.
     &  uplim .le. x .or.
     &  uplim .le. y .or.
     &  uplim .le. z ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RF - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a,d23.16)' ) '  Z = ', z
        write ( *, '(a)' ) ''
        ierr = 1
        rf = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y
      zn = z

      do

        mu = ( xn + yn + zn ) / 3.0d0
        xndev = 2.0d0 - ( mu + xn ) / mu
        yndev = 2.0d0 - ( mu + yn ) / mu
        zndev = 2.0d0 - ( mu + zn ) / mu
        epslon = max ( abs ( xndev ), abs ( yndev ), abs ( zndev ) )

        if ( epslon .lt. errtol ) then
          c1 = 1.0d0 / 24.0d0
          c2 = 3.0d0 / 44.0d0
          c3 = 1.0d0 / 14.0d0
          e2 = xndev * yndev - zndev * zndev
          e3 = xndev * yndev * zndev
          s = 1.0d0 + ( c1 * e2 - 0.1d0 - c2 * e3 ) * e2 + c3 * e3
          rf = s / sqrt ( mu ) 
          return
        end if

        xnroot = sqrt ( xn )
        ynroot = sqrt ( yn )
        znroot = sqrt ( zn )
        lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0
        zn = ( zn + lamda ) * 0.25d0

      end do

      end
      function rj ( x, y, z, p, errtol, ierr )

c*********************************************************************72
c
cc RJ computes an incomplete elliptic integral of the third kind, RJ(X,Y,Z,P).
c
c  Discussion:
c
c    This function computes an incomplete elliptic integral of the third kind.
c
c    RJ(X,Y,Z,P) = Integral ( 0 <= T < oo )
c
c                                  -1/2     -1/2     -1/2     -1
c                        (3/2)(T+X)    (T+Y)    (T+Z)    (T+P)  DT,
c
c    where X, Y, and Z are nonnegative, at most one of them is
c    zero, and P is positive.
c
c    If X or Y or Z is zero, then the integral is complete.
c
c    The duplication theorem is iterated until the variables are nearly equal, 
c    and the function is then expanded in Taylor series to fifth order.  
c
c    Check by addition theorem: 
c
c      RJ(X,X+Z,X+W,X+P)
c      + RJ(Y,Y+Z,Y+W,Y+P) + (A-B) * RJ(A,B,B,A) + 3 / sqrt ( A)
c      = RJ(0,Z,W,P), where X,Y,Z,W,P are positive and X * Y
c      = Z * W,  A = P * P * (X+Y+Z+W),  B = P * (P+X) * (P+Y),
c      and B - A = P * (P-Z) * (P-W).  
c
c      The sum of the third and fourth terms on the left side is 3 * RC(A,B).
c
c  Modified:
c
c    27 May 2018
c
c  Author:
c
c    Bille Carlson, Elaine Notis
c
c  Reference:
c
c    Bille Carlson,
c    Computing Elliptic Integrals by Duplication,
c    Numerische Mathematik,
c    Volume 33, 1979, pages 1-16.
c
c    Bille Carlson, Elaine Notis,
c    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 7, Number 3, pages 398-403, September 1981.
c
c  Parameters:
c
c    Input, double precision X, Y, Z, P, the arguments in the integral.
c
c    Input, double precision ERRTOL, the error tolerance.
c    Relative error due to truncation of the series for rj
c    is less than 3 * ERRTOL ^ 6 / (1 - ERRTOL) ^ 3/2.
c    An error tolerance (ETOLRC) will be passed to the subroutine
c    for RC to make the truncation error for RC less than for RJ.
c    Sample choices:
c      ERRTOL   Relative truncation error less than
c      1.D-3    4.D-18
c      3.D-3    3.D-15
c      1.D-2    4.D-12
c      3.D-2    3.D-9
c      1.D-1    4.D-6
c
c    Output, integer IERR, the error flag.
c    0, no error occurred.
c    1, abnormal termination.
c
      implicit none

      double precision alfa
      double precision beta
      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision ea
      double precision eb
      double precision ec
      double precision e2
      double precision e3
      double precision epslon
      double precision errtol
      double precision etolrc
      integer ierr
      double precision lamda
      double precision lolim
      double precision mu
      double precision p
      double precision pn
      double precision pndev
      double precision power4
      double precision rc
      double precision rj
      double precision sigma
      double precision s1
      double precision s2
      double precision s3
      double precision uplim
      double precision x
      double precision xn
      double precision xndev
      double precision xnroot
      double precision y
      double precision yn
      double precision yndev
      double precision ynroot
      double precision z
      double precision zn
      double precision zndev
      double precision znroot
c
c  LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
c  LOLIM IS NOT LESS THAN THE CUBE ROOT OF THE VALUE
c  OF LOLIM USED IN THE SUBROUTINE FOR RC.
c  UPLIM IS NOT GREATER THAN 0.3 TIMES THE CUBE ROOT OF
c  THE VALUE OF UPLIM USED IN THE SUBROUTINE FOR RC.
c
      save lolim
      save uplim

      data lolim /2.D-26/
      data uplim /3.D+24/

      if ( 
     &  x .lt. 0.0D+00 .or.
     &  y .lt. 0.0D+00 .or.
     &  z .lt. 0.0D+00 .or.
     &  x + y .lt. lolim .or.
     &  x + z .lt. lolim .or.
     &  y + z .lt. lolim .or.
     &  p .lt. lolim .or.
     &  uplim .lt. x .or.
     &  uplim .lt. y .or.
     &  uplim .lt. z .or.
     &  uplim .lt. p ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RJ - Error!'
        write ( *, '(a)' ) '  Invalid input arguments.'
        write ( *, '(a,d23.16)' ) '  X = ', x
        write ( *, '(a,d23.16)' ) '  Y = ', y
        write ( *, '(a,d23.16)' ) '  Z = ', z
        write ( *, '(a,d23.16)' ) '  P = ', p
        write ( *, '(a)' ) ''
        ierr = 1
        rj = 0.0D+00
        return
      end if

      ierr = 0
      xn = x
      yn = y
      zn = z
      pn = p
      sigma = 0.0d0
      power4 = 1.0d0
      etolrc = 0.5d0 * errtol

      do

        mu = ( xn + yn + zn + pn + pn ) * 0.2d0
        xndev = ( mu - xn ) / mu
        yndev = ( mu - yn ) / mu
        zndev = ( mu - zn ) / mu
        pndev = ( mu - pn ) / mu
        epslon = max ( abs ( xndev ), abs ( yndev ), abs ( zndev ), 
     &    abs ( pndev ) )

        if ( epslon .lt. errtol ) then
          c1 = 3.0d0 / 14.0d0
          c2 = 1.0d0 / 3.0d0
          c3 = 3.0d0 / 22.0d0
          c4 = 3.0d0 / 26.0d0
          ea = xndev * ( yndev + zndev ) + yndev * zndev
          eb = xndev * yndev * zndev
          ec = pndev * pndev
          e2 = ea - 3.0d0 * ec
          e3 = eb + 2.0d0 * pndev * ( ea - ec )
          s1 = 1.0d0 
     &      + e2 * ( - c1 + 0.75d0 * c3 * e2 - 1.5d0 * c4 * e3 )
          s2 = eb * ( 0.5d0 * c2 + pndev * ( - c3 - c3 + pndev * c4 ) )
          s3 = pndev * ea * ( c2 - pndev * c3 ) - c2 * pndev * ec
          rj = 3.0d0 * sigma 
     &      + power4 * ( s1 + s2 + s3 ) / ( mu * sqrt ( mu ) )
          return
        end if

        xnroot = sqrt ( xn )
        ynroot = sqrt ( yn )
        znroot = sqrt ( zn )
        lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot
        alfa = pn * ( xnroot + ynroot + znroot ) 
     &    + xnroot * ynroot * znroot
        alfa = alfa * alfa
        beta = pn * ( pn + lamda ) * ( pn + lamda )
        sigma = sigma + power4 * rc ( alfa, beta, etolrc, ierr )

        if ( ierr .ne. 0 ) then
          rj = 0.0D+00
          return
        end if

        power4 = power4 * 0.25d0
        xn = ( xn + lamda ) * 0.25d0
        yn = ( yn + lamda ) * 0.25d0
        zn = ( zn + lamda ) * 0.25d0
        pn = ( pn + lamda ) * 0.25d0

      end do

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
c    12 June 2014
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end

