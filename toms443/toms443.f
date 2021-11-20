      subroutine lambert_w_values ( n_data, x, fx )

c*********************************************************************72
c
cc LAMBERT_W_VALUES returns some values of the Lambert W function.
c
c  Discussion:
c
c    The function W(X) is defined implicitly by:
c
c      W(X) * e^W(X) = X
c
c    The function is also known as the "Omega" function.
c
c    In Mathematica, the function can be evaluated by:
c
c      W = ProductLog [ X ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    R M Corless, G H Gonnet, D E Hare, D J Jeffrey, Donald Knuth,
c    On the Lambert W Function,
c    Advances in Computational Mathematics,
c    Volume 5, 1996, pages 329-359.
c
c    Brian Hayes,
c    "Why W?",
c    The American Scientist,
c    Volume 93, March-April 2005, pages 104-108.
c
c    Eric Weisstein,
c    "Lambert's W-Function",
c    CRC Concise Encyclopedia of Mathematics,
c    CRC Press, 1998.
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
      parameter ( n_max = 22 )

      double precision fx
      double precision fx_vec(n_max)
      integer n_data
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save x_vec

      data fx_vec /
     &  0.0000000000000000D+00,
     &  0.3517337112491958D+00,
     &  0.5671432904097839D+00,
     &  0.7258613577662263D+00,
     &  0.8526055020137255D+00,
     &  0.9585863567287029D+00,
     &  0.1000000000000000D+01,
     &  0.1049908894964040D+01,
     &  0.1130289326974136D+01,
     &  0.1202167873197043D+01,
     &  0.1267237814307435D+01,
     &  0.1326724665242200D+01,
     &  0.1381545379445041D+01,
     &  0.1432404775898300D+01,
     &  0.1479856830173851D+01,
     &  0.1524345204984144D+01,
     &  0.1566230953782388D+01,
     &  0.1605811996320178D+01,
     &  0.1745528002740699D+01,
     &  0.3385630140290050D+01,
     &  0.5249602852401596D+01,
     &  0.1138335808614005D+02 /
      data x_vec /
     &  0.0000000000000000D+00,
     &  0.5000000000000000D+00,
     &  0.1000000000000000D+01,
     &  0.1500000000000000D+01,
     &  0.2000000000000000D+01,
     &  0.2500000000000000D+01,
     &  0.2718281828459045D+01,
     &  0.3000000000000000D+01,
     &  0.3500000000000000D+01,
     &  0.4000000000000000D+01,
     &  0.4500000000000000D+01,
     &  0.5000000000000000D+01,
     &  0.5500000000000000D+01,
     &  0.6000000000000000D+01,
     &  0.6500000000000000D+01,
     &  0.7000000000000000D+01,
     &  0.7500000000000000D+01,
     &  0.8000000000000000D+01,
     &  0.1000000000000000D+02,
     &  0.1000000000000000D+03,
     &  0.1000000000000000D+04,
     &  0.1000000000000000D+07 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        x = 0.0
        fx = 0.0
      else
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
c
c  Modified:
c
c    16 September 2005
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

      character ( len = 8 ) date
      character ( len = 10 ) time

      call date_and_time ( date, time )

      write ( *, '(a8,2x,a10)' ) date, time

      return
      end
      function wew_a ( x, en )

c*********************************************************************72
c
cc WEW_A estimates Lambert's W function.
c
c  Discussion:
c
c    For a given X, this routine estimates the solution W of Lambert's 
c    equation:
c
c      X = W * EXP ( W )
c
c    This routine has higher accuracy than WEW_B.
c
c  Modified:
c
c    07 June 2014
c
c  Reference:
c
c    Fred Fritsch, R Shafer, W Crowley,
c    Algorithm 443: Solution of the transcendental equation w e^w = x,
c    Communications of the ACM,
c    October 1973, Volume 16, Number 2, pages 123-124.
c
c  Parameters:
c
c    Input, double precision X, the argument of W(X)
c
c    Output, double precision WEW_A, the estimated value of W(X).
c
c    Output, double precision EN, the last relative correction to W(X).
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision en
      double precision f
      integer newe
      double precision temp
      double precision temp2
      double precision wew_a
      double precision wn
      double precision x
      double precision y
      double precision zn

      save c1
      save c2
      save c3
      save c4
      save newe

      data newe / 1 /
c
c  Set constants.
c
      if ( newe .ne. 0 ) then
        newe = 0
        c1 = 4.0D+00 / 3.0D+00
        c2 = 7.0D+00 / 3.0D+00
        c3 = 5.0D+00 / 6.0D+00
        c4 = 2.0D+00 / 3.0D+00
      end if
c
c  Initial guess.
c
      f = dlog ( x )

      if ( x .le. 6.46D+00 ) then

        wn = x * ( 1.0D+00 + c1 * x ) 
     &    / ( 1.0D+00 + x * ( c2 + c3 * x ) )
        zn = f - wn - dlog ( wn )

      else

        wn = f
        zn = - dlog ( wn )

      end if
c
c  Iteration 1.
c
      temp = 1.0D+00 + wn
      y = 2.0D+00 * temp * ( temp + c4 * zn ) - zn
      wn = wn * ( 1.0D+00 + zn * y / ( temp * ( y - zn ) ) )
c
c  Iteration 2.
c
      zn = f - wn - dlog ( wn )
      temp = 1.0D+00 + wn
      temp2 = temp + c4 * zn
      en = zn * temp2 / ( temp * temp2 - 0.5D+00 * zn )
      wn = wn * ( 1.0D+00 + en )

      wew_a = wn

      return
      end
      function wew_b ( x, en )

c*********************************************************************72
c
cc WEW_B estimates Lambert's W function.
c
c  Discussion:
c
c    For a given X, this routine estimates the solution W of Lambert's 
c    equation:
c
c      X = W * EXP ( W )
c
c    This routine has lower accuracy than WEW_A.
c
c  Modified:
c
c    07 June 2014
c
c  Reference:
c
c    Fred Fritsch, R Shafer, W Crowley,
c    Algorithm 443: Solution of the transcendental equation w e^w = x,
c    Communications of the ACM,
c    October 1973, Volume 16, Number 2, pages 123-124.
c
c  Parameters:
c
c    Input, double precision X, the argument of W(X)
c
c    Output, double precision WEW_B, the estimated value of W(X).
c
c    Output, double precision EN, the last relative correction to W(X).
c
      implicit none

      double precision c1
      double precision c2
      double precision c3
      double precision c4
      double precision en
      double precision f
      integer newe
      double precision temp
      double precision wew_b
      double precision wn
      double precision x
      double precision y
      double precision zn

      save c1
      save c2
      save c3
      save c4
      save newe

      data newe / 1 /
c
c  Set constants.
c
      if ( newe .ne. 0 ) then
        newe = 0
        c1 = 4.0D+00 / 3.0D+00
        c2 = 7.0D+00 / 3.0D+00
        c3 = 5.0D+00 / 6.0D+00
        c4 = 2.0D+00 / 3.0D+00
      end if
c
c  Initial guess.
c
      f = dlog ( x )

      if ( x .le. 0.7385D+00 ) then
        wn = x * ( 1.0D+00 + c1 * x ) 
     &    / ( 1.0D+00 + x * ( c2 + c3 * x ) )
      else
        wn = f - 24.0D+00 * ( ( f + 2.0D+00 ) * f - 3.0D+00 ) 
     &    / ( ( 0.7D+00 * f + 58.0D+00 ) * f + 127.0D+00 )
      end if
c
c  Iteration 1.
c
      zn = f - wn - dlog ( wn )
      temp = 1.0D+00 + wn
      y = 2.0D+00 * temp * ( temp + c4 * zn ) - zn
      en = zn * y / ( temp * ( y - zn ) )
      wn = wn * ( 1.0D+00 + en )

      wew_b = wn

      return
      end
