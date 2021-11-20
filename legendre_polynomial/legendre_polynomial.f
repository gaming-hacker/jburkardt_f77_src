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
            stop 1
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
      subroutine p_exponential_product ( p, b, table )

c*********************************************************************72
c
cc P_EXPONENTIAL_PRODUCT: exponential products for P(n,x).
c
c  Discussion:
c
c    Let P(n,x) represent the Legendre polynomial of degree n.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of exp(B*X) with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -1.0 .le. X .le. +1.0 ) exp(B*X) * P(I,X) * P(J,X) dx
c
c    We will estimate these integrals using Gauss-Legendre quadrature.
c    Because of the exponential factor exp(B*X), the quadrature will not 
c    be exact.
c
c    However, when B = 0, the quadrature is exact.
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 .le. P.
c
c    Input, double precision B, the coefficient of X in the exponential factor.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
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
      double precision x(1)
      double precision x_table(( 3 * p + 4 ) / 2)

      table(0:p,0:p) = 0.0D+00

      order = ( 3 * p + 4 ) / 2

      call p_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x(1) = x_table(k)
        call p_polynomial_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        do j = 0, p
          do i = 0, p
            table(i,j) = table(i,j) + w_table(k) * exp ( b * x(1) ) 
     &        * h_table(i) * h_table(j)
          end do
        end do

      end do

      return
      end
      subroutine p_integral ( n, value )

c*********************************************************************72
c
cc P_INTEGRAL evaluates a monomial integral associated with P(n,x).
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 .le. x .lt. +1 ) x^n dx
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the exponent.
c    0 .le. N.
c
c    Output, double precision VALUE, the value of the integral.
c
      implicit none

      integer n
      double precision value

      if ( mod ( n, 2 ) .eq. 1 ) then
        value = 0.0D+00
      else
        value = 2.0D+00 / dble ( n + 1 )
      end if

      return
      end
      subroutine p_polynomial_value ( m, n, x, v )

c*********************************************************************72
c
cc P_POLYNOMIAL_VALUE evaluates the Legendre polynomials P(n,x).
c
c  Discussion:
c
c    P(n,1) = 1.
c    P(n,-1) = (-1)^N.
c    | P(n,x) | .le. 1 in [-1,1].
c
c    The N zeroes of P(n,x) are the abscissas used for Gauss-Legendre
c    quadrature of the integral of a function F(X) with weight function 1
c    over the interval [-1,1].
c
c    The Legendre polynomials are orthogonal under the inner product defined
c    as integration from -1 to 1:
c
c      Integral ( -1 .le. X .le. 1 ) P(I,X) * P(J,X) dX 
c        = 0 if I =/= J
c        = 2 / ( 2*I+1 ) if I = J.
c
c    Except for P(0,X), the integral of P(I,X) from -1 to 1 is 0.
c
c    A function F(X) defined on [-1,1] may be approximated by the series
c      C0*P(0,x) + C1*P(1,x) + ... + CN*P(n,x)
c    where
c      C(I) = (2*I+1)/(2) * Integral ( -1 .le. X .le. 1 ) F(X) P(I,x) dx.
c
c    The formula is:
c
c      P(n,x) = (1/2^N) * sum ( 0 .le. M .le. N/2 ) C(N,M) C(2N-2M,N) X^(N-2*M)
c
c  Differential equation:
c
c    (1-X*X) * P(n,x)'' - 2 * X * P(n,x)' + N * (N+1) = 0
c
c  First terms:
c
c    P( 0,x) =      1
c    P( 1,x) =      1 X
c    P( 2,x) = (    3 X^2 -       1)/2
c    P( 3,x) = (    5 X^3 -     3 X)/2
c    P( 4,x) = (   35 X^4 -    30 X^2 +     3)/8
c    P( 5,x) = (   63 X^5 -    70 X^3 +    15 X)/8
c    P( 6,x) = (  231 X^6 -   315 X^4 +   105 X^2 -     5)/16
c    P( 7,x) = (  429 X^7 -   693 X^5 +   315 X^3 -    35 X)/16
c    P( 8,x) = ( 6435 X^8 - 12012 X^6 +  6930 X^4 -  1260 X^2 +   35)/128
c    P( 9,x) = (12155 X^9 - 25740 X^7 + 18018 X^5 -  4620 X^3 +  315 X)/128
c    P(10,x) = (46189 X^10-109395 X^8 + 90090 X^6 - 30030 X^4 + 3465 X^2-63)/256
c
c  Recursion:
c
c    P(0,x) = 1
c    P(1,x) = x
c    P(n,x) = ( (2*n-1)*x*P(n-1,x)-(n-1)*P(n-2,x) ) / n
c
c    P'(0,x) = 0
c    P'(1,x) = 1
c    P'(N,x) = ( (2*N-1)*(P(N-1,x)+X*P'(N-1,x)-(N-1)*P'(N-2,x) ) / N
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision V(M,0:N), the values of the Legendre polynomials 
c    of order 0 through N at the points X.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = x(i)
      end do
   
      do j = 2, n
        do i = 1, m

          v(i,j) = ( dble ( 2 * j - 1 ) * x(i) * v(i,j-1)   
     &             - dble (     j - 1 ) *        v(i,j-2) ) 
     &             / dble (     j     )

         end do
      end do
     
      return
      end
      subroutine p_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc P_POLYNOMIAL_COEFFICIENTS: coefficients of Legendre polynomials P(n,x).
c
c  First terms:
c
c     1
c     0     1
c    -1/2   0      3/2
c     0    -3/2    0     5/2
c     3/8   0    -30/8   0     35/8
c     0    15/8    0   -70/8    0     63/8
c    -5/16  0    105/16  0   -315/16   0    231/16
c     0   -35/16   0   315/16   0   -693/16   0    429/16
c
c     1.00000
c     0.00000  1.00000
c    -0.50000  0.00000  1.50000
c     0.00000 -1.50000  0.00000  2.5000
c     0.37500  0.00000 -3.75000  0.00000  4.37500
c     0.00000  1.87500  0.00000 -8.75000  0.00000  7.87500
c    -0.31250  0.00000  6.56250  0.00000 -19.6875  0.00000  14.4375
c     0.00000 -2.1875   0.00000  19.6875  0.00000 -43.3215  0.00000  26.8125
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Output, double precision C(0:N,0:N), the coefficients of the 
c    Legendre polynomials of degree 0 through N.
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

      if ( n .le. 0 ) then
        return
      end if

      c(1,1) = 1.0D+00
     
      do i = 2, n
        do j = 0, i - 2
          c(i,j) =  dble ( - i + 1 ) * c(i-2,j) / dble ( i )
        end do
        do j = 1, i
          c(i,j) = c(i,j) + dble ( i + i - 1 ) * c(i-1,j-1) / dble ( i )
        end do
      end do
     
      return
      end
      subroutine p_polynomial_prime ( m, n, x, vp )

c*********************************************************************72
c
cc P_POLYNOMIAL_PRIME evaluates the derivative of Legendre polynomials P(n,x).
c
c  Discussion:
c
c    P(0,X) = 1
c    P(1,X) = X
c    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
c
c    P'(0,X) = 0
c    P'(1,X) = 1
c    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision VP(M,0:N), the values of the derivatives of the
c    Legendre polynomials of order 0 through N.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(m,0:n)
      double precision vp(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
        vp(i,0) = 0.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = x(i)
        vp(i,1) = 1.0D+00
      end do
 
      do j = 2, n
        do i = 1, m

          v(i,j) = ( dble ( 2 * j - 1 ) * x(i) * v(i,j-1)   
     &             - dble (     j - 1 ) *          v(i,j-2) ) 
     &             / dble (     j     )
     
          vp(i,j) = ( dble ( 2 * j - 1 ) * ( v(i,j-1) 
     &                            + x(i) * vp(i,j-1) ) 
     &              - dble (     j - 1 ) *   vp(i,j-2) ) 
     &              / dble (     j     )

        end do 
      end do
     
      return
      end
      subroutine p_polynomial_prime2 ( m, n, x, vpp )

c*********************************************************************72
c
cc P_POLYNOMIAL_PRIME2: second derivative of Legendre polynomials P(n,x).
c
c  Discussion:
c
c    P(0,X) = 1
c    P(1,X) = X
c    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
c
c    P'(0,X) = 0
c    P'(1,X) = 1
c    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
c
c    P"(0,X) = 0
c    P"(1,X) = 0
c    P"(N,X) = ( (2*N-1)*(2*P(N-1,X)+X*P"(N-1,X)-(N-1)*P"(N-2,X) ) / N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 May 2013
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision VPP(M,0:N), the second derivative of the
c    Legendre polynomials of order 0 through N.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(m,0:n)
      double precision vp(m,0:n)
      double precision vpp(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
        vp(i,0) = 0.0D+00
        vpp(i,0) = 0.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = x(i)
        vp(i,1) = 1.0D+00
        vpp(i,1) = 0.0D+00
      end do

      do j = 2, n
        do i = 1, m

          v(i,j) = 
     &      ( dble ( 2 * j - 1 ) * x(i) * v(i,j-1)   
     &      - dble (     j - 1 ) *          v(i,j-2) ) 
     &      / dble (     j     )
     
          vp(i,j) = 
     &      ( dble ( 2 * j - 1 ) * ( v(i,j-1) + x(i) * vp(i,j-1) ) 
     &      - dble (     j - 1 ) *  vp(i,j-2)               ) 
     &      / dble (     j     )

          vpp(i,j) = 
     &      ( dble ( 2 * j - 1 ) * ( 2.0D+00 * vp(i,j-1) 
     &                             + x(i) * vpp(i,j-1) ) 
     &      - dble (     j - 1 ) *   vpp(i,j-2)               ) 
     &      / dble (     j     )

        end do
      end do
     
      return
      end
      subroutine p_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc P_POLYNOMIAL_VALUES returns values of the Legendre polynomials P(n,x).
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
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 22 )

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
     &   0.1000000000000000D+01,
     &   0.2500000000000000D+00,
     &  -0.4062500000000000D+00,
     &  -0.3359375000000000D+00,
     &   0.1577148437500000D+00,
     &   0.3397216796875000D+00,
     &   0.2427673339843750D-01,
     &  -0.2799186706542969D+00,
     &  -0.1524540185928345D+00,
     &   0.1768244206905365D+00,
     &   0.2212002165615559D+00,
     &   0.0000000000000000D+00,
     &  -0.1475000000000000D+00,
     &  -0.2800000000000000D+00,
     &  -0.3825000000000000D+00,
     &  -0.4400000000000000D+00,
     &  -0.4375000000000000D+00,
     &  -0.3600000000000000D+00,
     &  -0.1925000000000000D+00,
     &   0.8000000000000000D-01,
     &   0.4725000000000000D+00,
     &   0.1000000000000000D+01 /
      data n_vec /
     &   0,  1,  2,
     &   3,  4,  5,
     &   6,  7,  8,
     &   9, 10,  3,
     &   3,  3,  3,
     &   3,  3,  3,
     &   3,  3,  3,
     &   3 /
      data x_vec /
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.00D+00,
     &  0.10D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.50D+00,
     &  0.60D+00,
     &  0.70D+00,
     &  0.80D+00,
     &  0.90D+00,
     &  1.00D+00 /

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
      subroutine p_polynomial_zeros ( nt, t )

c*********************************************************************72
c
cc P_POLYNOMIAL_ZEROS: zeros of Legendre function P(n,x).
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NT, the order of the rule.
c
c    Output, double precision T(NT), the zeros.
c
      implicit none

      integer nt

      double precision bj(nt)
      integer i
      double precision t(nt)
      double precision wts(nt)

      do i = 1, nt
        t(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = dble ( i * i ) / dble ( 4 * i * i - 1 )
      end do

      do i = 1, nt
        bj(i) = sqrt ( bj(i) )
      end do

      wts(1) = sqrt ( 2.0D+00 )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, t, bj, wts )

      return
      end
      subroutine p_power_product ( p, e, table )

c*********************************************************************72
c
cc P_POWER_PRODUCT: power products for Legendre polynomial P(n,x).
c
c  Discussion:
c
c    Let P(n,x) represent the Legendre polynomial of degree n.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of X with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( -1.0 .le. X .le. +1.0 ) X^E * P(i,x) * P(j,x) dx
c
c    We will estimate these integrals using Gauss-Legendre quadrature.
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 .le. P.
c
c    Input, integer E, the exponent of X in the integrand.
c    0 .le. E.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c
      implicit none

      integer e
      integer p

      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(p + 1 + ( ( e + 1 ) / 2 ))
      double precision x(1)
      double precision x_table(p + 1 + ( ( e + 1 ) / 2 ))

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = p + 1 + ( ( e + 1 ) / 2 )

      call p_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x(1) = x_table(k)
        call p_polynomial_value ( 1, p, x, h_table )
c
c  The following formula is an outer product in H_TABLE.
c
        if ( e .eq. 0 ) then
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) + w_table(k) 
     &          * h_table(i) * h_table(j)
            end do
          end do
        else
          do i = 0, p
            do j = 0, p
              table(i,j) = table(i,j) 
     &          + w_table(k) * x(1) ** e * h_table(i) * h_table(j)
            end do
          end do
        end if

      end do

      return
      end
      subroutine p_quadrature_rule ( nt, t, wts )

c*********************************************************************72
c
cc P_QUADRATURE_RULE: quadrature for Legendre function P(n,x).
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
      double precision t(nt)
      double precision wts(nt)

      do i = 1, nt
        t(i) = 0.0D+00
      end do

      do i = 1, nt
        bj(i) = dble ( i * i ) / dble ( 4 * i * i - 1 )
      end do

      do i = 1, nt
        bj(i) = sqrt ( bj(i) )
      end do

      wts(1) = sqrt ( 2.0D+00 )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do

      call imtqlx ( nt, t, bj, wts )

      do i = 1, nt
        wts(i) = wts(i) ** 2
      end do

      return
      end
      subroutine pm_polynomial_value ( mm, n, m, x, cx )

c*********************************************************************72
c
cc PM_POLYNOMIAL_VALUE evaluates the Legendre polynomials Pm(n,m,x).
c
c  Differential equation:
c
c    (1-X*X) * Y'' - 2 * X * Y + ( N (N+1) - (M*M/(1-X*X)) * Y = 0
c
c  First terms:
c
c    M = 0  ( = Legendre polynomials of first kind P(N,X) )
c
c    Pm(0,0,x) =    1
c    Pm(1,0,x) =    1 X
c    Pm(2,0,x) = (  3 X^2 -   1)/2
c    Pm(3,0,x) = (  5 X^3 -   3 X)/2
c    Pm(4,0,x) = ( 35 X^4 -  30 X^2 +   3)/8
c    Pm(5,0,x) = ( 63 X^5 -  70 X^3 +  15 X)/8
c    Pm(6,0,x) = (231 X^6 - 315 X^4 + 105 X^2 -  5)/16
c    Pm(7,0,x) = (429 X^7 - 693 X^5 + 315 X^3 - 35 X)/16
c
c    M = 1
c
c    Pm(0,1,x) =   0
c    Pm(1,1,x) =   1 * SQRT(1-X^2)
c    Pm(2,1,x) =   3 * SQRT(1-X^2) * X
c    Pm(3,1,x) = 1.5 * SQRT(1-X^2) * (5*X^2-1)
c    Pm(4,1,x) = 2.5 * SQRT(1-X^2) * (7*X^3-3*X)
c
c    M = 2
c
c    Pm(0,2,x) =   0
c    Pm(1,2,x) =   0
c    Pm(2,2,x) =   3 * (1-X^2)
c    Pm(3,2,x) =  15 * (1-X^2) * X
c    Pm(4,2,x) = 7.5 * (1-X^2) * (7*X^2-1)
c
c    M = 3
c
c    Pm(0,3,x) =   0
c    Pm(1,3,x) =   0
c    Pm(2,3,x) =   0
c    Pm(3,3,x) =  15 * (1-X^2)^1.5
c    Pm(4,3,x) = 105 * (1-X^2)^1.5 * X
c
c    M = 4
c
c    Pm(0,4,x) =   0
c    Pm(1,4,x) =   0
c    Pm(2,4,x) =   0
c    Pm(3,4,x) =   0
c    Pm(4,4,x) = 105 * (1-X^2)^2
c
c  Recursion:
c
c    if N .lt. M:
c      Pm(N,M,x) = 0
c    if N = M:
c      Pm(N,M,x) = (2*M-1)cc * (1-X*X)^(M/2) where Ncc means the product of
c      all the odd integers less than or equal to N.
c    if N = M+1:
c      Pm(N,M,x) = X*(2*M+1)*Pm(M,M,x)
c    if M+1 .lt. N:
c      Pm(N,M,x) = ( X*(2*N-1)*Pm(N-1,M,x) - (N+M-1)*Pm(N-2,M,x) )/(N-M)
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
c    Input, integer MM, the number of evaluation points.
c
c    Input, integer N, the maximum first index of the Legendre
c    function, which must be at least 0.
c
c    Input, integer M, the second index of the Legendre function,
c    which must be at least 0, and no greater than N.
c
c    Input, double precision X(MM), the point at which the function is to be
c    evaluated.
c
c    Output, double precision CX(MM,0:N), the function values.
c
      implicit none

      integer mm
      integer n

      double precision cx(mm,0:n)
      double precision fact
      integer i
      integer j
      integer m
      double precision x(mm)

      do j = 0, n
        do i = 1, mm
          cx(i,j) = 0.0D+00
        end do
      end do
c
c  J = M is the first nonzero function.
c
      if ( m .le. n ) then

        do i = 1, mm
          cx(i,m) = 1.0D+00
        end do

        fact = 1.0D+00
        do j = 1, m
          do i = 1, mm
            cx(i,m) = - cx(i,m) * fact * sqrt ( 1.0D+00 - x(i)**2 )
          end do
          fact = fact + 2.0D+00
        end do

      end if
c
c  J = M + 1 is the second nonzero function.
c
      if ( m + 1 .le. n ) then
        do i = 1, mm
          cx(i,m+1) = x(i) * dble ( 2 * m + 1 ) * cx(i,m)
        end do
      end if
c
c  Now we use a three term recurrence.
c
      do j = m + 2, n
        do i = 1, mm
          cx(i,j) = ( dble ( 2 * j     - 1 ) * x(i) * cx(i,j-1) 
     &              + dble (   - j - m + 1 ) *        cx(i,j-2) ) 
     &              / dble (     j - m     )
        end do
      end do

      return
      end
      subroutine pm_polynomial_values ( n_data, n, m, x, fx )

c*********************************************************************72
c
cc PM_POLYNOMIAL_VALUES returns values of Legendre polynomials Pm(n,m,x).
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
c    Output, integer N, integer M, double precision X,
c    the arguments of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision fx
      double precision fx_vec(n_max)
      integer m
      integer m_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save m_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   0.0000000000000000D+00,
     &  -0.5000000000000000D+00,
     &   0.0000000000000000D+00,
     &   0.3750000000000000D+00,
     &   0.0000000000000000D+00,
     &  -0.8660254037844386D+00,
     &  -0.1299038105676658D+01,
     &  -0.3247595264191645D+00,
     &   0.1353164693413185D+01,
     &  -0.2800000000000000D+00,
     &   0.1175755076535925D+01,
     &   0.2880000000000000D+01,
     &  -0.1410906091843111D+02,
     &  -0.3955078125000000D+01,
     &  -0.9997558593750000D+01,
     &   0.8265311444100484D+02,
     &   0.2024442836815152D+02,
     &  -0.4237997531890869D+03,
     &   0.1638320624828339D+04,
     &  -0.2025687389227225D+05 /
      data m_vec /
     &  0, 0, 0, 0,
     &  0, 1, 1, 1,
     &  1, 0, 1, 2,
     &  3, 2, 2, 3,
     &  3, 4, 4, 5 /
      data n_vec /
     &  1,  2,  3,  4,
     &  5,  1,  2,  3,
     &  4,  3,  3,  3,
     &  3,  4,  5,  6,
     &  7,  8,  9, 10 /
      data x_vec /
     &  0.00D+00,
     &  0.00D+00,
     &  0.00D+00,
     &  0.00D+00,
     &  0.00D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.20D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        m = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        m = m_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine pmn_polynomial_value ( mm, n, m, x, cx )

c*********************************************************************72
c
cc PMN_POLYNOMIAL_VALUE: normalized Legendre polynomial Pmn(n,m,x).
c
c  Discussion:
c
c    The unnormalized associated Legendre functions P_N^M(X) have
c    the property that
c
c      Integral ( -1 .le. X .le. 1 ) ( P_N^M(X) )^2 dX 
c      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
c
c    By dividing the function by the square root of this term,
c    the normalized associated Legendre functions have norm 1.
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
c    Input, integer MM, the number of evaluation points.
c
c    Input, integer N, the maximum first index of the Legendre
c    function, which must be at least 0.
c
c    Input, integer M, the second index of the Legendre function,
c    which must be at least 0, and no greater than N.
c
c    Input, double precision X(MM), the evaluation points.
c
c    Output, double precision CX(MM,0:N), the function values.
c
      implicit none

      integer mm
      integer n

      double precision cx(mm,0:n)
      double precision factor
      integer i
      integer j
      integer m
      double precision r8_factorial
      double precision x(mm)

      if ( m .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PMN_POLYNOMIAL_VALUE - Fatal error!'
        write ( *, '(a,i8)' ) '  Input value of M is ', m
        write ( *, '(a)' ) '  but M must be nonnegative.'
        stop 1
      end if
     
      if ( n .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PMN_POLYNOMIAL_VALUE - Fatal error!'
        write ( *, '(a,i8)' ) '  Input value of M = ', m
        write ( *, '(a,i8)' ) '  Input value of N = ', n
        write ( *, '(a)' ) '  but M must be less than or equal to N.'
        stop 1
      end if

      do j = 0, n
        do i = 1, mm
          cx(i,j) = 0.0D+00
        end do
      end do

      if ( m .le. n ) then
        do i = 1, mm
          cx(i,m) = 1.0D+00
        end do
        factor = 1.0D+00
        do j = 1, m
          do i = 1, mm
            cx(i,m) = - cx(i,m) * factor * sqrt ( 1.0D+00 - x(i)**2 )
          end do
          factor = factor + 2.0D+00
        end do
      end if

      if ( m + 1 .le. n ) then
        do i = 1, mm
          cx(i,m+1) = x(i) * dble ( 2 * m + 1 ) * cx(i,m)
        end do
      end if

      do j = m + 2, n
        do i = 1, mm
          cx(i,j) = ( dble ( 2 * j     - 1 ) * x(i) * cx(i,j-1) 
     &              + dble (   - j - m + 1 ) *        cx(i,j-2) )
     &              / dble (     j - m     )
        end do
      end do
c
c  Normalization.
c
      do j = m, n
        factor = sqrt ( ( dble ( 2 * j + 1 ) * r8_factorial ( j - m ) ) 
     &    / ( 2.0D+00 * r8_factorial ( j + m ) ) )
        do i = 1, mm
          cx(i,j) = cx(i,j) * factor
        end do
      end do

      return
      end
      subroutine pmn_polynomial_values ( n_data, n, m, x, fx )

c*********************************************************************72
c
cc PMN_POLYNOMIAL_VALUES: normalized Legendre polynomial Pmn(n,m,x).
c
c  Discussion:
c
c    In Mathematica, the unnormalized function can be evaluated by:
c
c      LegendreP [ n, m, x ]
c
c    The function is normalized by dividing by the factor:
c
c      sqrt ( 2 * ( n + m )! / ( 2 * n + 1 ) / ( n - m )! )
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
c    Output, integer N, integer M, double precision X,
c    the arguments of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 21 )

      double precision fx
      double precision fx_vec(n_max)
      integer m
      integer m_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save m_vec
      save n_vec
      save x_vec

      data fx_vec /
     &    0.7071067811865475D+00,
     &    0.6123724356957945D+00, 
     &   -0.7500000000000000D+00, 
     &   -0.1976423537605237D+00, 
     &   -0.8385254915624211D+00, 
     &    0.7261843774138907D+00, 
     &   -0.8184875533567997D+00, 
     &   -0.1753901900050285D+00, 
     &    0.9606516343087123D+00, 
     &   -0.6792832849776299D+00, 
     &   -0.6131941618102092D+00, 
     &    0.6418623720763665D+00, 
     &    0.4716705890038619D+00, 
     &   -0.1018924927466445D+01, 
     &    0.6239615396237876D+00, 
     &    0.2107022704608181D+00, 
     &    0.8256314721961969D+00, 
     &   -0.3982651281554632D+00, 
     &   -0.7040399320721435D+00, 
     &    0.1034723155272289D+01, 
     &   -0.5667412129155530D+00 /
      data m_vec /
     &  0, 0, 1, 0,
     &  1, 2, 0, 1,
     &  2, 3, 0, 1,
     &  2, 3, 4, 0,
     &  1, 2, 3, 4,
     &  5 /
      data n_vec /
     &  0,  1,  1,  2,
     &  2,  2,  3,  3,
     &  3,  3,  4,  4,
     &  4,  4,  4,  5,
     &  5,  5,  5,  5,
     &  5 /
      data x_vec /
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        m = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        m = m_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine pmns_polynomial_value ( mm, n, m, x, cx )

c*********************************************************************72
c
cc PMNS_POLYNOMIAL_VALUE: sphere-normalized Legendre polynomial Pmns(n,m,x).
c
c  Discussion:
c
c    The unnormalized associated Legendre functions P_N^M(X) have
c    the property that
c
c      Integral ( -1 .le. X .le. 1 ) ( P_N^M(X) )^2 dX 
c      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
c
c    By dividing the function by the square root of this term,
c    the normalized associated Legendre functions have norm 1.
c
c    However, we plan to use these functions to build spherical
c    harmonics, so we use a slightly different normalization factor of
c
c      sqrt ( ( ( 2 * N + 1 ) * ( N - M )! ) / ( 4 * pi * ( N + M )! ) ) 
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
c    Input, integer MM, the number of evaluation points.
c
c    Input, integer N, the maximum first index of the Legendre
c    function, which must be at least 0.
c
c    Input, integer M, the second index of the Legendre function,
c    which must be at least 0, and no greater than N.
c
c    Input, double precision X(MM), the evaluation points.
c
c    Output, double precision CX(MM,0:N), the function values.
c
      implicit none

      integer mm
      integer n

      double precision cx(mm,0:n)
      double precision factor
      integer i
      integer j
      integer m
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_factorial
      double precision x(mm)

      do j = 0, n
        do i = 1, mm
          cx(i,j) = 0.0D+00
        end do
      end do

      if ( m .le. n ) then
        do i = 1, mm
          cx(1:mm,m) = 1.0D+00
        end do
        factor = 1.0D+00
        do j = 1, m
          do i = 1, mm
            cx(i,m) = - cx(i,m) * factor * sqrt ( 1.0D+00 - x(i)**2 )
          end do
          factor = factor + 2.0D+00
        end do
      end if

      if ( m + 1 .le. n ) then
        do i = 1, mm
          cx(i,m+1) = x(i) * dble ( 2 * m + 1 ) * cx(i,m)
        end do
      end if

      do j = m + 2, n
        do i = 1, mm
          cx(i,j) = ( dble ( 2 * j     - 1 ) * x(i) * cx(i,j-1) 
     &              + dble (   - j - m + 1 ) *        cx(i,j-2) ) 
     &              / dble (     j - m     )
        end do
      end do
c
c  Normalization.
c
      do j = m, n
        factor = sqrt ( ( dble ( 2 * j + 1 ) * r8_factorial ( j - m ) ) 
     &    / ( 4.0D+00 * pi * r8_factorial ( j + m ) ) )
        do i = 1, mm
          cx(i,j) = cx(i,j) * factor
        end do
      end do

      return
      end
      subroutine pmns_polynomial_values ( n_data, n, m, x, fx )

c*********************************************************************72
c
cc PMNS_POLYNOMIAL_VALUES: sphere-normalized Legendre polynomial Pmns(n,m,x).
c
c  Discussion:
c
c    In Mathematica, the unnormalized function can be evaluated by:
c
c      LegendreP [ n, m, x ]
c
c    The function is normalized by dividing by the factor:
c
c      sqrt ( 4 * pi * ( n + m )! / ( 2 * n + 1 ) / ( n - m )! )
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
c    Output, integer N, integer M, double precision X,
c    the arguments of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 21 )

      double precision fx
      double precision fx_vec(n_max)
      integer m
      integer m_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save m_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   0.2820947917738781D+00,
     &   0.2443012559514600D+00,
     &  -0.2992067103010745D+00,
     &  -0.07884789131313000D+00,
     &  -0.3345232717786446D+00,
     &   0.2897056515173922D+00,
     &  -0.3265292910163510D+00,
     &  -0.06997056236064664D+00,
     &   0.3832445536624809D+00,
     &  -0.2709948227475519D+00,
     &  -0.2446290772414100D+00,
     &   0.2560660384200185D+00,
     &   0.1881693403754876D+00,
     &  -0.4064922341213279D+00,
     &   0.2489246395003027D+00,
     &   0.08405804426339821D+00,
     &   0.3293793022891428D+00,
     &  -0.1588847984307093D+00,
     &  -0.2808712959945307D+00,
     &   0.4127948151484925D+00,
     &  -0.2260970318780046D+00 /
      data m_vec /
     &  0, 0, 1, 0,
     &  1, 2, 0, 1,
     &  2, 3, 0, 1,
     &  2, 3, 4, 0,
     &  1, 2, 3, 4,
     &  5 /
      data n_vec /
     &  0,  1,  1,  2,
     &  2,  2,  3,  3,
     &  3,  3,  4,  4,
     &  4,  4,  4,  5,
     &  5,  5,  5,  5,
     &  5 /
      data x_vec /
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00,
     &  0.50D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        m = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        m = m_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine pn_pair_product ( p, table )

c*********************************************************************72
c
cc PN_PAIR_PRODUCT: pair products for normalized Legendre polynomial Pn(n,x).
c
c  Discussion:
c
c    Let Pn(n,x) represent the normalized Legendre polynomial of degree n.  
c
c    To check orthonormality, we compute
c
c      Tij = Integral ( -1.0 .le. X .le. +1.0 ) Pn(i,x) * Pn(j,x) dx
c
c    We will estimate these integrals using Gauss-Legendre quadrature.
c
c    The computed table should be the identity matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P, the maximum degree of the polyonomial 
c    factors.  0 .le. P.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c
      implicit none

      integer p

      double precision h_table(0:p)
      integer i
      integer j
      integer k
      integer order
      double precision table(0:p,0:p)
      double precision w_table(p+1)
      double precision x(1)
      double precision x_table(p+1)

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = p + 1

      call p_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x(1) = x_table(k)
        call pn_polynomial_value ( 1, p, x, h_table )

        do i = 0, p
          do j = 0, p
            table(i,j) = table(i,j) 
     &        + w_table(k) * h_table(i) * h_table(j)
          end do
        end do

      end do

      return
      end
      subroutine pn_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc PN_POLYNOMIAL_COEFFICIENTS: coefficients of normalized Legendre Pn(n,x).
c
c  Discussion:
c
c    Pn(n,x) = P(n,x) * sqrt ( (2n+1)/2 )
c
c          1       x       x^2     x^3     x^4      x^5    x^6     x^7
c
c    0   0.707
c    1   0.000   1.224
c    2  -0.790   0.000   2.371
c    3   0.000  -2.806   0.000   4.677
c    4   0.795   0.000  -7.954   0.000   9.280
c    5   0.000   4.397   0.000 -20.520   0.000   18.468
c    6  -0.796   0.000  16.731   0.000 -50.193    0.000  36.808
c    7   0.000  -5.990   0.000  53.916   0.000 -118.616   0.000  73.429 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    17 October 2014
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Output, double precision C(0:N,0:N), the coefficients of the 
c    normalized Legendre polynomials of degree 0 through N.
c
      implicit none

      integer n

      double precision c(0:n,0:n)
      integer i
      integer j
      double precision t
c
c  Compute P(i,x) coefficients.
c
      if ( n .lt. 0 ) then
        return
      end if

      do j = 0, n
        do i = 0, n
          c(i,j) = 0.0D+00
        end do
      end do

      c(0,0) = 1.0D+00

      if ( 0 .lt. n ) then
        c(1,1) = 1.0D+00
      end if
   
      do i = 2, n
        do j = 0, i - 2
          c(i,j) =  dble ( - i + 1 ) * c(i-2,j) / dble ( i )
        end do
        do j = 1, i
          c(i,j) = c(i,j) + dble ( i + i - 1 ) * c(i-1,j-1) / dble ( i )
        end do
      end do
c
c  Normalize them.
c
      do i = 0, n
        t = sqrt ( dble ( 2 * i + 1 ) / 2.0D+00 )
        do j = 0, i
          c(i,j) = c(i,j) * t
        end do
      end do
      return
      end
      subroutine pn_polynomial_value ( m, n, x, v )

c*********************************************************************72
c
cc PN_POLYNOMIAL_VALUE evaluates normalized Legendre polynomials Pn(n,x).
c
c  Discussion:
c
c    The normalized Legendre polynomials are orthonormal under the inner product
c    defined as integration from -1 to 1:
c
c      Integral ( -1 .le. x .le. +1 ) Pn(i,x) * Pn(j,x) dx = delta(i,j)
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision V(M,0:N), the values of the Legendre polynomials 
c    of order 0 through N at the points X.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision norm
      double precision v(m,0:n)
      double precision x(m)

      call p_polynomial_value ( m, n, x, v )

      do j = 0, n
        norm = sqrt ( 2.0D+00 / dble ( 2 * j + 1 ) )
        do i = 1, m
          v(i,j) = v(i,j) / norm
        end do
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
c    factorial ( N ) = product ( 1 .le. I .le. N ) I
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
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

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
