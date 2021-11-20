      subroutine h_integral ( n, value )

c*********************************************************************72
c
cc H_INTEGRAL evaluates the integral of H(i,x).
c
c  Discussion:
c
c    H(i,x) is the physicist's Hermite polynomial of degree I.
c
c    The integral computed is:
c
c      integral ( -oo < x < +oo ) H(i,x) exp(-x^2) dx
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
c    Input, integer N, the order of the integral.  
c    0 <= N.
c
c    Output, double precision VALUE, the value of the integral.
c
      implicit none

      integer n
      double precision r8_factorial2
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision value

      if ( mod ( n, 2 ) .eq. 1 ) then

        value = 0.0D+00

      else

        value = r8_factorial2 ( n - 1 ) * sqrt ( r8_pi ) 
     &    / 2.0D+00 ** ( n / 2 )

      end if

      return
      end
      subroutine h_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc H_POLYNOMIAL_COEFFICIENTS: coefficients of H(i,x).
c
c  Discussion:
c
c    H(i,x) is the physicist's Hermite polynomial of degree I.
c
c  First terms:
c
c    N/K     0     1      2      3       4     5      6    7      8    9   10
c
c     0      1
c     1      0     2
c     2     -2     0      4
c     3      0   -12      0      8
c     4     12     0    -48      0      16
c     5      0   120      0   -160       0    32
c     6   -120     0    720      0    -480     0     64
c     7      0 -1680      0   3360       0 -1344      0   128
c     8   1680     0 -13440      0   13440     0  -3584     0    256
c     9      0 30240      0 -80640       0 48384      0 -9216      0 512
c    10 -30240     0 302400      0 -403200     0 161280     0 -23040   0 1024 
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
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Output, double precision C(0:N,0:N), the coefficients of the polynomials
c    of orders 0 through N.
c
      implicit none

      integer n

      double precision c(0:n,0:n)
      integer i
      integer j

      if ( n .lt. 0 ) then
        return
      end if

      do j = 0, n
        do i = 0, n
          c(i,j) = 0.0D+00
        end do
      end do

      c(0,0) = 1.0D+00

      if ( n .eq. 0 ) then
        return
      end if

      c(1,1) = 2.0D+00
     
      do i = 2, n
        c(i,0)     =  -2.0D+00 * dble ( i - 1 ) * c(i-2,0)
        do j = 1, i - 2
          c(i,j) =   2.0D+00                  * c(i-1,j-1)  
     &              -2.0D+00 * dble ( i - 1 ) * c(i-2,j)
        end do
        c(i,i-1) = 2.0D+00 * c(i-1,i-2)
        c(i,i) = 2.0D+00 * c(i-1,i-1)
      end do
     
      return
      end
      subroutine h_polynomial_value ( m, n, x, p )

c*********************************************************************72
c
cc H_POLYNOMIAL_VALUE evaluates H(i,x).
c
c  Discussion:
c
c    H(i,x) is the physicist's Hermite polynomial of degree I.
c
c  Differential equation:
c
c    Y'' - 2 X Y' + 2 N Y = 0
c
c  First terms:
c
c      1
c      2 X
c      4 X^2     -  2
c      8 X^3     - 12 X
c     16 X^4     - 48 X^2     + 12
c     32 X^5    - 160 X^3    + 120 X
c     64 X^6    - 480 X^4    + 720 X^2    - 120
c    128 X^7   - 1344 X^5   + 3360 X^3   - 1680 X
c    256 X^8   - 3584 X^6  + 13440 X^4  - 13440 X^2   + 1680
c    512 X^9   - 9216 X^7  + 48384 X^5  - 80640 X^3  + 30240 X
c   1024 X^10 - 23040 X^8 + 161280 X^6 - 403200 X^4 + 302400 X^2 - 30240
c
c  Recursion:
c
c    H(0,X) = 1,
c    H(1,X) = 2*X,
c    H(N,X) = 2*X * H(N-1,X) - 2*(N-1) * H(N-2,X)
c
c  Norm:
c
c    Integral ( -oo .lt. X .lt. oo ) exp ( - X^2 ) * H(N,X)^2 dX
c    = sqrt ( PI ) * 2^N * N!
c
c    H(N,X) = (-1)^N * exp ( X^2 ) * dn/dXn ( exp(-X^2 ) )
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Larry Andrews,
c    Special Functions of Mathematics for Engineers,
c    Second Edition, 
c    Oxford University Press, 1998.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision P(M,0:N), the values of the first N+1 Hermite
c    polynomials at the point X.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision p(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        p(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        p(i,1) = 2.0D+00 * x(i)
      end do
   
      do j = 2, n
        do i = 1, m
          p(i,j) = 2.0D+00 * x(i) * p(i,j-1) 
     &           - 2.0D+00 * dble ( j - 1 ) * p(i,j-2)
        end do
      end do
     
      return
      end
      subroutine h_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc H_POLYNOMIAL_VALUES: tabulated values of H(i,x).
c
c  Discussion:
c
c    H(i,x) is the physicist's Hermite polynomial of degree I.
c
c    In Mathematica, the function can be evaluated by:
c
c      HermiteH[n,x]
c
c  Differential equation:
c
c    Y'' - 2 X Y' + 2 N Y = 0
c
c  First terms:
c
c      1
c      2 X
c      4 X^2     -  2
c      8 X^3     - 12 X
c     16 X^4     - 48 X^2     + 12
c     32 X^5    - 160 X^3    + 120 X
c     64 X^6    - 480 X^4    + 720 X^2    - 120
c    128 X^7   - 1344 X^5   + 3360 X^3   - 1680 X
c    256 X^8   - 3584 X^6  + 13440 X^4  - 13440 X^2   + 1680
c    512 X^9   - 9216 X^7  + 48384 X^5  - 80640 X^3  + 30240 X
c   1024 X^10 - 23040 X^8 + 161280 X^6 - 403200 X^4 + 302400 X^2 - 30240
c
c  Recursion:
c
c    H(0,X) = 1,
c    H(1,X) = 2*X,
c    H(N,X) = 2*X * H(N-1,X) - 2*(N-1) * H(N-2,X)
c
c  Norm:
c
c    Integral ( -oo .lt. X .lt. +oo ) exp ( - X^2 ) * H(N,X)^2 dX
c    = sqrt ( PI ) * 2^N * N!
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
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the polynomial.
c
c    Output, double precision X, the point where the polynomial is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 18 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &    0.1000000000000000D+01, 
     &    0.1000000000000000D+02, 
     &    0.9800000000000000D+02, 
     &    0.9400000000000000D+03, 
     &    0.8812000000000000D+04, 
     &    0.8060000000000000D+05, 
     &    0.7178800000000000D+06, 
     &    0.6211600000000000D+07, 
     &    0.5206568000000000D+08, 
     &    0.4212712000000000D+09, 
     &    0.3275529760000000D+10, 
     &    0.2432987360000000D+11, 
     &    0.1712370812800000D+12, 
     &    0.0000000000000000D+00, 
     &    0.4100000000000000D+02, 
     &   -0.8000000000000000D+01, 
     &    0.3816000000000000D+04, 
     &    0.3041200000000000D+07 /
      data n_vec /
     &   0,  1,  2, 
     &   3,  4,  5, 
     &   6,  7,  8, 
     &   9, 10, 11, 
     &  12,  5,  5, 
     &   5,  5, 5 /
      data x_vec /
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  0.0D+00, 
     &  0.5D+00, 
     &  1.0D+00, 
     &  3.0D+00, 
     &  1.0D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine h_polynomial_zeros ( nt, z )

c*********************************************************************72
c
cc H_POLYNOMIAL_ZEROS: zeros of H(i,x).
c
c  Discussion:
c
c    H(i,x) is the physicist's Hermite polynomial of degree I.
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
c    Input, integer NT, the degree of the polynomial.
c
c    Output, double precision Z(NT), the zeros of the polynomial.
c
      implicit none

      integer nt

      double precision bj(nt)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision wts(nt)
      double precision z(nt)

      do i = 1, nt
        z(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = sqrt ( dble ( i ) / 2.0D+00 )
      end do

      wts(1) = sqrt ( sqrt ( r8_pi ) )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, z, bj, wts )

      return
      end
      subroutine h_quadrature_rule ( nt, t, wts )

c*********************************************************************72
c
cc H_QUADRATURE_RULE: quadrature for H(i,x).
c
!  Discussion:
!
!    H(i,x) is the physicist's Hermite polynomial of degree I.
!
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
c    Input, integer NT, the order of the rule.
c
c    Output, double precision T(NT), WTS(NT), the points and weights 
c    of the rule.
c
      integer nt

      double precision bj(nt)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision wts(nt)

      do i = 1, nt
        t(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = sqrt ( dble ( i ) / 2.0D+00 )
      end do

      wts(1) = sqrt ( sqrt ( r8_pi ) )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, t, bj, wts )

      do i = 1, nt
        wts(i) = wts(i) ** 2
      end do

      return
      end
      function he_double_product_integral ( i, j )

c*********************************************************************72
c
cc HE_DOUBLE_PRODUCT_INTEGRAL: integral of He(i,x)*He(j,x)*e^(-x^2/2).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
c
c    VALUE = integral ( -oo < x < +oo ) He(i,x)*He(j,x) exp(-x^2/2) dx
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
c  Reference:
c
c    Dongbin Xiu,
c    Numerical Methods for Stochastic Computations: A Spectral Method Approach,
c    Princeton, 2010,
c    ISBN13: 978-0-691-14212-8,
c    LC: QA274.23.X58.
c
c  Parameters:
c
c    Input, integer I, J, the polynomial indices.
c
c    Output, double precision HE_DOUBLE_PRODUCT_INTEGRAL, the value of 
c    the integral.
c
      implicit none

      double precision he_double_product_integral
      integer i
      integer j
      double precision r8_factorial
      double precision value

      if ( i .eq. j ) then
        value = r8_factorial ( i )
      else
        value = 0.0D+00
      end if

      he_double_product_integral = value

      return
      end
      subroutine he_integral ( n, value )

c*********************************************************************72
c
cc HE_INTEGRAL evaluates the integral of He(i,x).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
c
c    The integral computed is:
c
c      integral ( -oo < x < +oo ) He(i,x) exp(-x^2/2) dx
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
c    Input, integer N, the order of the integral.  
c    0 <= N.
c
c    Output, double precision VALUE, the value of the integral.
c
      implicit none

      integer n
      double precision r8_factorial2
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision value

      if ( mod ( n, 2 ) .eq. 1 ) then

        value = 0.0D+00

      else

        value = r8_factorial2 ( n - 1 ) * sqrt ( 2.0D+00 * r8_pi )

      end if

      return
      end
      subroutine he_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc HE_POLYNOMIAL_COEFFICIENTS: coefficients of He(i,x).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
c
c  First terms:
c
c    N/K     0     1      2      3       4     5      6    7      8    9   10
c
c     0      1
c     1      0     1
c     2     -1     0      1
c     3      0    -3      0      1
c     4      3     0     -6      0       1
c     5      0    15      0    -10       0     1
c     6    -15     0     45      0     -15     0      1
c     7      0  -105      0    105       0   -21      0     1
c     8    105     0   -420      0     210     0    -28     0      1
c     9      0   945      0  -1260       0   378      0   -36      0   1
c    10   -945     0   4725      0   -3150     0    630     0    -45   0    1
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
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Output, double precision C(0:N,0:N), the coefficients of the polynomials
c    of orders 0 through N.
c
      implicit none

      integer n

      double precision c(0:n,0:n)
      integer i
      integer j

      if ( n .lt. 0 ) then
        return
      end if

      do j = 0, n
        do i = 0, n
          c(i,j) = 0.0D+00
        end do
      end do

      c(0,0) = 1.0D+00

      if ( n .eq. 0 ) then
        return
      end if

      c(1,1) = 1.0D+00
     
      do i = 2, n
        c(i,0) = - dble ( i - 1 ) * c(i-2,0)
        do j = 1, i - 2
          c(i,j) = c(i-1,j-1) - dble ( i - 1 ) * c(i-2,j)
        end do
        c(i,i-1) = c(i-1,i-2)
        c(i,i) = c(i-1,i-1)
      end do
     
      return
      end
      subroutine he_polynomial_value ( m, n, x, p )

c*********************************************************************72
c
cc HE_POLYNOMIAL_VALUE evaluates He(i,x).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
c
c  Differential equation:
c
c    ( exp ( - 0.5 * x^2 ) * He(n,x)' )' + n * exp ( - 0.5 * x^2 ) * He(n,x) = 0
c
c  First terms:
c
c   1
c   X
c   X^2  -  1
c   X^3  -  3 X
c   X^4  -  6 X^2 +   3
c   X^5  - 10 X^3 +  15 X
c   X^6  - 15 X^4 +  45 X^2 -   15
c   X^7  - 21 X^5 + 105 X^3 -  105 X
c   X^8  - 28 X^6 + 210 X^4 -  420 X^2 +  105
c   X^9  - 36 X^7 + 378 X^5 - 1260 X^3 +  945 X
c   X^10 - 45 X^8 + 630 X^6 - 3150 X^4 + 4725 X^2 - 945
c
c  Recursion:
c
c    He(0,X) = 1,
c    He(1,X) = X,
c    He(N,X) = X * He(N-1,X) - (N-1) * He(N-2,X)
c
c  Orthogonality:
c
c    Integral ( -oo .lt. X .lt. +oo ) exp ( - 0.5 * X^2 ) * He(M,X) He(N,X) dX 
c    = sqrt ( 2 * pi ) * N! * delta ( N, M )
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Frank Olver, Daniel Lozier, Ronald Boisvert, Charles Clark,
c    NIST Handbook of Mathematical Functions,
c    Cambridge University Press, 2010,
c    ISBN: 978-0521192255,
c    LC: QA331.N57.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision P(M,0:N), the values of the probabilist's 
c    Hermite polynomials of index 0 through N.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision p(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        p(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        p(i,1) = x(i)
      end do
   
      do j = 2, n
        do i = 1, m
          p(i,j) = x(i) * p(i,j-1) - dble ( j - 1 ) * p(i,j-2)
        end do
      end do
     
      return
      end
      subroutine he_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc HE_POLYNOMIAL_VALUES: tabulated values of He(i,x).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
c
c    In Mathematica, the function can be evaluated by:
c
c      He(n,x) = HermiteH[n,x/Sqrt[2]] / Sqrt [ 2^n ] 
c
c  First terms:
c
c   1
c   X
c   X^2  -  1
c   X^3  -  3 X
c   X^4  -  6 X^2 +   3
c   X^5  - 10 X^3 +  15 X
c   X^6  - 15 X^4 +  45 X^2 -   15
c   X^7  - 21 X^5 + 105 X^3 -  105 X
c   X^8  - 28 X^6 + 210 X^4 -  420 X^2 +  105
c   X^9  - 36 X^7 + 378 X^5 - 1260 X^3 +  945 X
c   X^10 - 45 X^8 + 630 X^6 - 3150 X^4 + 4725 X^2 - 945
c
c  Recursion:
c
c    He(0,X) = 1,
c    He(1,X) = X,
c    He(N,X) = X * He(N-1,X) - (N-1) * He(N-2,X)
c
c  Norm:
c
c    Integral ( -oo .lt. X .lt. +oo ) exp ( - 0.5 * X^2 ) * He(M,X) He(N,X) dX 
c    = sqrt ( 2 * pi ) * N! * delta ( N, M )
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
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the polynomial.
c
c    Output, double precision X, the point where the polynomial is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 18 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &  1.000000000000000D+00, 
     &  5.000000000000000D+00, 
     &  24.00000000000000D+00, 
     &  110.0000000000000D+00, 
     &  478.0000000000000D+00, 
     &  1950.000000000000D+00, 
     &  7360.000000000000D+00, 
     &  25100.00000000000D+00, 
     &  73980.00000000000D+00, 
     &  169100.0000000000D+00, 
     &  179680.0000000000D+00, 
     & -792600.0000000000D+00, 
     & -5939480.000000000D+00, 
     &  0.000000000000000D+00, 
     &  6.281250000000000D+00, 
     &  6.000000000000000D+00, 
     &  18.00000000000000D+00, 
     &  90150.00000000000D+00 /
      data n_vec /
     &   0,  1,  2, 
     &   3,  4,  5, 
     &   6,  7,  8, 
     &   9, 10, 11, 
     &  12,  5,  5, 
     &   5,  5,  5 /
      data x_vec /
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  5.0D+00, 
     &  0.0D+00, 
     &  0.5D+00,
     &  1.0D+00, 
     &  3.0D+00, 
     &  1.0D+01 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine he_polynomial_zeros ( nt, z )

c*********************************************************************72
c
cc HE_POLYNOMIAL_ZEROS: zeros of He(i,x).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
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
c    Input, integer NT, the degree of the polynomial.
c
c    Output, double precision Z(NT), the zeros of the polynomial.
c
      implicit none

      integer nt

      double precision bj(nt)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision wts(nt)
      double precision z(nt)

      do i = 1, nt
        z(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = sqrt ( dble ( i ) / 2.0D+00 )
      end do

      wts(1) = sqrt ( sqrt ( r8_pi ) )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, z, bj, wts )

      do i = 1, nt
        z(i) = z(i) * sqrt ( 2.0D+00 )
      end do

      return
      end
      subroutine he_quadrature_rule ( nt, t, wts )

c*********************************************************************72
c
cc HE_QUADRATURE_RULE: quadrature for He(i,x).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
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
c    Input, integer NT, the order of the rule.
c
c    Output, double precision T(NT), WTS(NT), the points and weights 
c    of the rule.
c
      implicit none

      integer nt

      double precision bj(nt)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision wts(nt)

      do i = 1, nt
        t(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = sqrt ( dble ( i ) / 2.0D+00 )
      end do

      wts(1) = sqrt ( sqrt ( r8_pi ) )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, t, bj, wts )

      do i = 1, nt
        t(i) = t(i) * sqrt ( 2.0D+00 )
      end do

      do i = 1, nt
        wts(i) = wts(i) ** 2 * sqrt ( 2.0D+00 )
      end do

      return
      end
      function he_triple_product_integral ( i, j, k )

c*********************************************************************72
c
cc HE_TRIPLE_PRODUCT_INTEGRAL: integral of He(i,x)*He(j,x)*He(k,x)*e^(-x^2/2).
c
c  Discussion:
c
c    He(i,x) represents the probabilist's Hermite polynomial.
c
c    VALUE = integral ( -oo .lt. x .lt. +oo ) He(i,x)*He(j,x)*He(k,x) exp(-x^2/2) dx
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
c  Reference:
c
c    Dongbin Xiu,
c    Numerical Methods for Stochastic Computations: A Spectral Method Approach,
c    Princeton, 2010,
c    ISBN13: 978-0-691-14212-8,
c    LC: QA274.23.X58.
c
c  Parameters:
c
c    Input, integer I, J, K, the polynomial indices.
c
c    Output, double precision HE_TRIPLE_PRODUCT_INTEGRAL, the value 
c    of the integral.
c
      implicit none

      double precision he_triple_product_integral
      integer i
      integer j
      integer k
      double precision r8_factorial
      integer s
      double precision value

      s = ( i + j + k ) / 2

      if ( s .lt. max ( i, j, k ) ) then
        value = 0.0D+00
      else if ( mod ( i + j + k, 2 ) .ne. 0 ) then
        value = 0.0D+00
      else
        value = r8_factorial ( i ) / r8_factorial ( s - i ) 
     &        * r8_factorial ( j ) / r8_factorial ( s - j ) 
     &        * r8_factorial ( k ) / r8_factorial ( s - k )
      end if

      he_triple_product_integral = value

      return
      end
      subroutine hen_exponential_product ( p, b, table )

c*********************************************************************72
c
cc HEN_EXPONENTIAL_PRODUCT: exponential product exp(b*x)*Hen(i,x)*Hen(j,x).
c
c  Discussion:
c
c    Hen(i,x) is the normalized probabilist's Hermite polynomial of degree I. 
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of exp(B*X) with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -oo .lt. X .lt. +oo ) 
c        exp(B*X) * Hen(I,X) * Hen(J,X) exp(-0.5*X*X) dx
c
c    We will estimate these integrals using Gauss-Hermite quadrature.
c    Because of the exponential factor exp(B*X), the quadrature will not 
c    be exact.
c
c    However, when B = 0, the quadrature is exact, and moreoever, the
c    table will be the identity matrix.
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
c    Input, integer P, the maximum degree of the 
c    polyonomial factors.  0 <= P.
c
c    Input, double precision B, the coefficient of X in the exponential factor.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the weighted integral of 
c    exp(B*X) * Hen(I,X) * Hen(J,X).
c
      implicit none

      integer p

      double precision b
      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(( 3 * p + 4 ) / 2)
      double precision x
      double precision x_table(( 3 * p + 4 ) / 2)

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = ( 3 * p + 4 ) / 2

      call he_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k);
        call hen_polynomial_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        do j = 0, p
          do i = 0, p
            table(i,j) = table(i,j) 
     &        + w_table(k) * exp ( b * x ) * h_table(i) * h_table(j)
          end do
        end do

      end do

      return
      end
      subroutine hen_polynomial_value ( m, n, x, p )

c*********************************************************************72
c
cc HEN_POLYNOMIAL_VALUE evaluates Hen(i,x).
c
c  Discussion:
c
c    Hen(i,x) is the normalized probabilist's Hermite polynomial of degree I.
c
c    These polynomials satisfy the orthonormality condition:
c
c      Integral ( -oo .lt. X .lt. +oo ) exp ( - 0.5 * X^2 ) * Hen(M,X) Hen(N,X) dX 
c      = delta ( N, M )
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Frank Olver, Daniel Lozier, Ronald Boisvert, Charles Clark,
c    NIST Handbook of Mathematical Functions,
c    Cambridge University Press, 2010,
c    ISBN: 978-0521192255,
c    LC: QA331.N57.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision P(M,0:N), the values of the polynomials of 
c    index 0 through N.
c
      implicit none

      integer m
      integer n

      double precision fact
      integer i
      integer j
      double precision p(m,0:n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(m)

      do i = 1, m
        p(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        p(i,1) = x(i)
      end do
   
      do j = 2, n
        do i = 1, m
          p(i,j) = x(i) * p(i,j-1) - dble ( j - 1 ) * p(i,j-2)
        end do
      end do
c
c  Normalize.
c
      fact = 1.0D+00
      do j = 0, n
        do i = 1, m
          p(i,j) = p(i,j) / sqrt ( fact * sqrt ( 2.0D+00 * r8_pi ) )
        end do
        fact = fact * dble ( j + 1 )
      end do

      return
      end
      subroutine hen_power_product ( p, e, table )

c*********************************************************************72
c
cc HEN_POWER_PRODUCT: power products, x^e*Hen(i,x)*Hen(j,x).
c
c  Discussion:
c
c    Hen(i,x) is the normalized probabilist's Hermite polynomial of degree I.
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of X with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -oo .lt. X .lt. +oo ) 
c        X^E * Hen(I,X) * Hen(J,X) exp(-0.5*X*X) dx
c
c    We will estimate these integrals using Gauss-Hermite quadrature.
c
c    When E is 0, the computed table should be the identity matrix.
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
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 <= P.
c
c    Input, integer E, the exponent of X in the integrand.
c    0 <= E.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the weighted integral of 
c    X^E * Hen(I,X) * Hen(J,X).
c
      implicit none

      integer e
      integer p

      double precision b
      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(p + 1 + ( ( e + 1 ) / 2 ))
      double precision x
      double precision x_table(p + 1 + ( ( e + 1 ) / 2 ))

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = p + 1 + ( ( e + 1 ) / 2 )

      call he_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call hen_polynomial_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        if ( e .eq. 0 ) then
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) + w_table(k) * h_table(i) 
     &          * h_table(j)
            end do
          end do
        else
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) 
     &          + w_table(k) * x ** e * h_table(i) * h_table(j)
            end do
          end do
        end if

      end do

      return
      end
      subroutine hf_exponential_product ( p, b, table )

c*********************************************************************72
c
cc HF_EXPONENTIAL_PRODUCT: exponential products, exp(b*x)*Hf(i,x)*Hf(j,x).
c
c  Discussion:
c
c    Hf(I,X) represents the Hermite function of "degree" I.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of exp(B*X) with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -oo .lt. X .lt. +oo ) exp(B*X) * Hf(I,X) * Hf(J,X) dx
c
c    We will estimate these integrals using Gauss-Hermite quadrature.
c    Because of the exponential factor exp(B*X), the quadrature will not 
c    be exact.
c
c    However, when B = 0, the quadrature is exact, and moreoever, the
c    table will be the identity matrix.
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
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 <= P.
c
c    Input, double precision B, the coefficient of X in the exponential factor.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the integral of exp(B*X) * Hf(I,X) * Hf(J,X).
c
      implicit none

      integer p

      double precision b
      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(( 3 * p + 4 ) / 2)
      double precision x
      double precision x_table(( 3 * p + 4 ) / 2)

      do j = 0, p
        do i = 0, p
          table(0:p,0:p) = 0.0D+00
        end do
      end do

      order = ( 3 * p + 4 ) / 2

      call hf_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call hf_function_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        do j = 0, p
          do i = 0, p
            table(i,j) = table(i,j) 
     &        + w_table(k) * exp ( b * x ) * h_table(i) * h_table(j)
          end do
        end do

      end do

      return
      end
      subroutine hf_function_value ( m, n, x, f )

c*********************************************************************72
c
cc HF_FUNCTION_VALUE evaluates Hf(i,x).
c
c  Discussion:
c
c    Hf(I,X) represents the Hermite function of "degree" I.  
c
c    The Hermite function of degree n is related to the physicist's
c    Hermite polynomial H(n,x):
c
c      Hf(n,x) = H(n,x) * exp ( - 0.5 * x^2 ) / sqrt ( 2^n n! sqrt ( pi ) )
c
c    The Hermite functions are orthonormal:
c
c      Integral ( -oo .lt. x .lt. +oo ) Hf(m,x) Hf(n,x) dx = delta ( m, n )
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Frank Olver, Daniel Lozier, Ronald Boisvert, Charles Clark,
c    NIST Handbook of Mathematical Functions,
c    Cambridge University Press, 2010,
c    ISBN: 978-0521192255,
c    LC: QA331.N57.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision F(M,0:N), the values of the Hermite functions 
c    of index 0 through N at the evaluation points.
c
      implicit none

      integer m
      integer n

      double precision f(m,0:n)
      integer i
      integer j
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(m)

      do i = 1, m
        f(i,0) = exp ( - 0.5D+00 * x(i) ** 2 ) / sqrt ( sqrt ( r8_pi ) )
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        f(i,1) = 2.0D+00 * exp ( - 0.5D+00 * x(i) ** 2 ) * x(i) 
     &    / sqrt ( 2.0D+00 * sqrt ( r8_pi ) )
      end do

      do j = 2, n
        do i = 1, m
          f(i,j) = ( sqrt ( 2.0D+00 ) * x(i) * f(i,j-1) 
     &      - sqrt ( dble ( j - 1 ) ) * f(i,j-2) ) 
     &      / sqrt ( dble ( j ) )
        end do
      end do
     
      return
      end
      subroutine hf_function_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc HF_FUNCTION_VALUES: tabulated values of Hf(i,x).
c
c  Discussion:
c
c    Hf(I,X) represents the Hermite function of "degree" I.  
c
c    In Mathematica, the function can be evaluated by:
c
c      Hf(n,x) = HermiteH[n,x] 
c        * Exp [ -1/2 * x^2] / Sqrt [ 2^n * n! * Sqrt[Pi] ]
c
c    The Hermite functions are orthonormal:
c
c      Integral ( -oo .lt. x .lt. +oo ) Hf(m,x) Hf(n,x) dx = delta ( m, n )
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
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the polynomial.
c
c    Output, double precision X, the point where the polynomial is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 23 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &  0.7511255444649425D+00,
     &  0.0000000000000000D+00,
     & -0.5311259660135985D+00,  
     &  0.0000000000000000D+00,
     &  0.4599685791773266D+00,
     &  0.0000000000000000D+00, 
     &  0.4555806720113325D+00,
     &  0.6442883651134752D+00,
     &  0.3221441825567376D+00, 
     & -0.2630296236233334D+00,
     & -0.4649750762925110D+00,
     & -0.5881521185179581D-01, 
     &  0.3905052515434106D+00,
     &  0.2631861423064045D+00,
     & -0.2336911435996523D+00, 
     & -0.3582973361472840D+00,
     &  0.6146344487883041D-01,
     &  0.3678312067984882D+00, 
     &  0.9131969309166278D-01,
     &  0.4385750950032321D+00,
     & -0.2624689527931006D-01, 
     &  0.5138426125477819D+00,
     &  0.9355563118061758D-01 /
      data n_vec /
     &  0,  1,  2,  
     &  3,  4,  5,  
     &  0,  1,  2,  
     &  3,  4,  5,  
     &  6,  7,  8,  
     &  9, 10, 11,  
     & 12,  5,  5,  
     &  5,  5  /
      data x_vec /
     &  0.0D+00, 0.0D+00, 0.0D+00, 
     &  0.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 0.5D+00, 2.0D+00, 
     &  3.0D+00, 4.0D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine hf_power_product ( p, e, table )

c*********************************************************************72
c
cc HF_POWER_PRODUCT: power products x^e*Hf(i,x)*Hf(j,x).
c
c  Discussion:
c
c    Hf(I,X) represents the Hermite function of "degree" I.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of X with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -oo .lt. X .lt. +oo ) X^E * Hf(I,X) * Hf(J,X) dx
c
c    We will estimate these integrals using Gauss-Hermite quadrature.
c
c    When E is 0, the computed table should be the identity matrix.
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
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 <= P.
c
c    Input, integer E, the exponent of X in the integrand.
c    0 <= E.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the integral of X^E * Hf(I,X) * Hf(J,X).
c
      implicit none

      integer p

      double precision b
      integer e
      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(p + 1 + ( ( e + 1 ) / 2 ))
      double precision x
      double precision x_table(p + 1 + ( ( e + 1 ) / 2 ))

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = p + 1 + ( ( e + 1 ) / 2 )

      call hf_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call hf_function_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        if ( e .eq. 0 ) then
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) + w_table(k) * h_table(i) 
     &          * h_table(j)
            end do
          end do
        else
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) 
     &          + w_table(k) * x ** e * h_table(i) * h_table(j)
            end do
          end do
        end if

      end do

      return
      end
      subroutine hf_quadrature_rule ( nt, t, wts )

c*********************************************************************72
c
cc HF_QUADRATURE_RULE: quadrature for Hf(i,x).
c
c  Discussion:
c
c    Hf(I,X) represents the Hermite function of "degree" I.  
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
c    Input, integer NT, the order of the rule.
c
c    Output, double precision T(NT), WTS(NT), the points and weights
c    of the rule.
c
      implicit none

      integer nt

      double precision bj(nt)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision wts(nt)

      do i = 1, nt
        t(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = sqrt ( dble ( i ) / 2.0D+00 )
      end do

      wts(1) = sqrt ( sqrt ( r8_pi ) )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, t, bj, wts )

      do i = 1, nt
        wts(i) = wts(i) ** 2 * exp ( t(i) ** 2 )
      end do

      return
      end
      subroutine hn_exponential_product ( p, b, table )

c*********************************************************************72
c
cc HN_EXPONENTIAL_PRODUCT: exponential products exp(b*x)*Hn(i,x)*Hn(j,x).
c
c  Discussion:
c
c    Hn(I,X) is the normalized physicist's Hermite polynomial of degree I.   
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of exp(B*X) with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -oo .lt. X .lt. +oo ) 
c        exp(B*X) * Hn(I,X) * Hn(J,X) exp(-X*X) dx
c
c    We will estimate these integrals using Gauss-Hermite quadrature.
c    Because of the exponential factor exp(B*X), the quadrature will not 
c    be exact.
c
c    However, when B = 0, the quadrature is exact, and moreoever, the
c    table will be the identity matrix.
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
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 <= P.
c
c    Input, double precision B, the coefficient of X in the exponential factor.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the weighted integral of 
c    exp(B*X) * Hn(I,X) * Hn(J,X).
c
      implicit none

      integer p

      double precision b
      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(( 3 * p + 4 ) / 2)
      double precision x
      double precision x_table(( 3 * p + 4 ) / 2)

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = ( 3 * p + 4 ) / 2

      call h_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call hn_polynomial_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        do j = 0, p
          do i = 0, p
            table(i,j) = table(i,j) 
     &        + w_table(k) * exp ( b * x ) * h_table(i) * h_table(j)
          end do
        end do

      end do

      return
      end
      subroutine hn_polynomial_value ( m, n, x, p )

c*********************************************************************72
c
cc HN_POLYNOMIAL_VALUE evaluates Hn(i,x).
c
c  Discussion:
c
c    Hn(I,X) is the normalized physicist's Hermite polynomial of degree I.
c
c    These polynomials satisfy the orthonormality condition:
c
c      Integral ( -oo .lt. X .lt. +oo ) 
c        exp ( - X^2 ) * Hn(M,X) Hn(N,X) dX = delta ( N, M )
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
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Frank Olver, Daniel Lozier, Ronald Boisvert, Charles Clark,
c    NIST Handbook of Mathematical Functions,
c    Cambridge University Press, 2010,
c    ISBN: 978-0521192255,
c    LC: QA331.N57.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision P(M,0:N), the values of the polynomials of 
c    index 0 through N.
c
      implicit none

      integer m
      integer n

      double precision fact
      integer i
      integer j
      double precision p(m,0:n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision two_power
      double precision x(m)

      do i = 1, m
        p(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        p(i,1) = 2.0D+00 * x(i)
      end do
     
      do j = 2, n
        do i = 1, m
          p(i,j) = 2.0D+00 * x(i) * p(i,j-1) 
     &      - 2.0D+00 * dble ( j - 1 ) * p(i,j-2)
        end do
      end do
c
c  Normalize.
c
      fact = 1.0D+00
      two_power = 1.0D+00
      do j = 0, n
        do i = 1, m
          p(i,j) = p(i,j) / sqrt ( fact * two_power * sqrt ( r8_pi ) )
        end do
        fact = fact * dble ( j + 1 )
        two_power = two_power * 2.0D+00
      end do

      return
      end
      subroutine hn_power_product ( p, e, table )

c*********************************************************************72
c
cc HN_POWER_PRODUCT: power products x^e*Hn(i,x)*Hn(j,x).
c
c  Discussion:
c
c    Hn(I,X) is the normalized physicist's Hermite polynomial of degree I.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of X with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -oo .lt. X .lt. +oo ) X^E * Hn(I,X) * Hn(J,X) exp(-X*X) dx
c
c    We will estimate these integrals using Gauss-Hermite quadrature.
c
c    When E is 0, the computed table should be the identity matrix.
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
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 <= P.
c
c    Input, integer E, the exponent of X in the integrand.
c    0 <= E.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the weighted integral of X^E * Hn(I,X) * Hn(J,X).
c
      implicit none

      integer p

      double precision b
      integer e
      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(p + 1 + ( ( e + 1 ) / 2 ))
      double precision x
      double precision x_table(p + 1 + ( ( e + 1 ) / 2 ))

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = p + 1 + ( ( e + 1 ) / 2 )

      call h_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call hn_polynomial_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        if ( e .eq. 0 ) then
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) + w_table(k) * h_table(i) 
     &          * h_table(j)
            end do
          end do
        else
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) 
     &          + w_table(k) * x ** e * h_table(i) * h_table(j)
            end do
          end do
        end if

      end do

      return
      end
      subroutine imtqlx ( n, d, e, z )

c*********************************************************************72
c
cc IMTQLX diagonalizes a symmetric tridiagonal matrix.
c
c  Discussion:
c
c    This routine is a slightly modified version of the EISPACK routine to 
c    perform the implicit QL algorithm on a symmetric tridiagonal matrix. 
c
c    The authors thank the authors of EISPACK for permission to use this
c    routine. 
c
c    It has been modified to produce the product Q' * Z, where Z is an input 
c    vector and Q is the orthogonal matrix diagonalizing the input matrix.  
c    The changes consist (essentially) of applying the orthogonal 
c    transformations directly to Z as they are generated.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Sylvan Elhay, Jaroslav Kautsky,
c    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
c    Interpolatory Quadrature,
c    ACM Transactions on Mathematical Software,
c    Volume 13, Number 4, December 1987, pages 399-415.
c
c    Roger Martin, James Wilkinson,
c    The Implicit QL Algorithm,
c    Numerische Mathematik,
c    Volume 12, Number 5, December 1968, pages 377-383.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input/output, double precision D(N), the diagonal entries of the matrix.
c    On output, the information in D has been overwritten.
c
c    Input/output, double precision E(N), the subdiagonal entries of the 
c    matrix, in entries E(1) through E(N-1).  On output, the information in
c    E has been overwritten.
c
c    Input/output, double precision Z(N).  On input, a vector.  On output,
c    the value of Q' * Z, where Q is the matrix that diagonalizes the
c    input symmetric tridiagonal matrix.
c
      implicit none

      integer n

      double precision b
      double precision c
      double precision d(n)
      double precision e(n)
      double precision f
      double precision g
      integer i
      integer ii
      integer itn
      parameter ( itn = 30 )
      integer j
      integer k
      integer l
      integer m
      integer mml
      double precision p
      double precision prec
      double precision r
      double precision r8_epsilon
      double precision s
      double precision z(n)

      prec = r8_epsilon ( )

      if ( n .eq. 1 ) then
        return
      end if

      e(n) = 0.0D+00

      do l = 1, n

        j = 0

10      continue

          do m = l, n

            if ( m .eq. n ) then
              go to 20
            end if

            if ( abs ( e(m) ) .le. 
     &        prec * ( abs ( d(m) ) + abs ( d(m+1) ) ) ) then
              go to 20
            end if

          end do

20        continue

          p = d(l)

          if ( m .eq. l ) then
            go to 30
          end if

          if ( itn .le. j ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'IMTQLX - Fatal error!'
            write ( *, '(a)' ) '  Iteration limit exceeded.'
            write ( *, '(a,i8)' ) '  J = ', j
            write ( *, '(a,i8)' ) '  L = ', l
            write ( *, '(a,i8)' ) '  M = ', m
            write ( *, '(a,i8)' ) '  N = ', n
            stop
          end if

          j = j + 1
          g = ( d(l+1) - p ) / ( 2.0D+00 * e(l) )
          r =  sqrt ( g * g + 1.0D+00 )
          g = d(m) - p + e(l) / ( g + sign ( r, g ) )
          s = 1.0D+00
          c = 1.0D+00
          p = 0.0D+00
          mml = m - l

          do ii = 1, mml

            i = m - ii
            f = s * e(i)
            b = c * e(i)

            if ( abs ( g ) .le. abs ( f ) ) then
              c = g / f
              r =  sqrt ( c * c + 1.0D+00 )
              e(i+1) = f * r
              s = 1.0D+00 / r
              c = c * s
            else
              s = f / g
              r =  sqrt ( s * s + 1.0D+00 )
              e(i+1) = g * r
              c = 1.0D+00 / r
              s = s * c
            end if

            g = d(i+1) - p
            r = ( d(i) - g ) * s + 2.0D+00 * c * b
            p = s * r
            d(i+1) = g + p
            g = c * r - b
            f = z(i+1)
            z(i+1) = s * z(i) + c * f
            z(i) = c * z(i) - s * f

          end do

          d(l) = d(l) - p
          e(l) = g
          e(m) = 0.0D+00

        go to 10

30      continue

      end do
c
c  Sorting.
c
      do ii = 2, n

        i = ii - 1
        k = i
        p = d(i)

        do j = ii, n
          if ( d(j) .lt. p ) then
            k = j
            p = d(j)
          end if
        end do

        if ( k .ne. i ) then
          d(k) = d(i)
          d(i) = p
          p = z(i)
          z(i) = z(k)
          z(k) = p
        end if

      end do

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
      function r8_factorial ( n )

c*********************************************************************72
c
cc R8_FACTORIAL computes the factorial of N.
c
c  Discussion:
c
c    factorial ( N ) = product ( 1 <= I <= N ) I
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the factorial function.
c    If N is less than 1, the function value is returned as 1.
c
c    Output, double precision R8_FACTORIAL, the factorial of N.
c
      implicit none

      integer i
      integer n
      double precision r8_factorial

      r8_factorial = 1.0D+00

      do i = 1, n
        r8_factorial = r8_factorial * dble ( i )
      end do

      return
      end
      function r8_factorial2 ( n )

c*********************************************************************72
c
cc R8_FACTORIAL2 computes the double factorial function.
c
c  Discussion:
c
c    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
c                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
c
c  Example:
c
c     N   Value
c
c     0     1
c     1     1
c     2     2
c     3     3
c     4     8
c     5    15
c     6    48
c     7   105
c     8   384
c     9   945
c    10  3840
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the double factorial
c    function.  If N is less than 1, R8_FACTORIAL2 is returned as 1.0.
c
c    Output, double precision R8_FACTORIAL2, the value.
c
      implicit none

      integer n
      double precision r8_factorial2
      double precision r8_n

      if ( n .lt. 1 ) then
        r8_factorial2 = 1.0D+00
        return
      end if

      r8_n = dble ( n )
      r8_factorial2 = 1.0D+00

10    continue

      if ( 1.0D+00 .lt. r8_n ) then
        r8_factorial2 = r8_factorial2 * r8_n
        r8_n = r8_n - 2.0D+00
        go to 10
      end if

      return
      end
      function r8_sign ( x )

c*********************************************************************72
c
cc R8_SIGN returns the sign of an R8.
c
c  Discussion:
c
c    value = -1 if X < 0;
c    value = +1 if X => 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the number whose sign is desired.
c
c    Output, double precision R8_SIGN, the sign of X.
c
      implicit none

      double precision r8_sign
      double precision x

      if ( x .lt. 0.0D+00 ) then
        r8_sign = -1.0D+00
      else
        r8_sign = +1.0D+00
      end if

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
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
c    20 May 2004
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
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

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
      subroutine r8vec2_print ( n, a1, a2, title )

c*********************************************************************72
c
cc R8VEC2_PRINT prints an R8VEC2.
c
c  Discussion:
c
c    An R8VEC2 is a dataset consisting of N pairs of R8s, stored
c    as two separate vectors A1 and A2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A1(N), A2(N), the vectors to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a1(i), a2(i)
      end do

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
