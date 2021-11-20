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
      subroutine l_exponential_product ( p, b, table )

c*********************************************************************72
c
cc L_EXPONENTIAL_PRODUCT: exponential product table for L(n,x).
c
c  Discussion:
c
c    Let L(n,x) represent the Laguerre polynomial of degree n.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of exp(B*X) with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( 0 <= X .lt. +oo ) exp(b*x) * L(i,x) * L(j,x) * exp (-x) dx
c
c    Because of the exponential factor, the quadrature will not be exact.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c    TABLE(I,J) represents the weighted integral of exp(B*X) * L(i,x) * L(j,x).
c
      implicit none

      integer p

      double precision b
      integer i
      integer j
      integer k
      double precision l_table(0:p)
      integer order
      double precision table(0:p,0:p)
      double precision w_table((3*p+4)/2)
      double precision x
      double precision x_table((3*p+4)/2)

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = ( 3 * p + 4 ) / 2

      call l_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call l_polynomial ( 1, p, x, l_table )

        do j = 0, p
          do i = 0, p
            table(i,j) = table(i,j) 
     &        + w_table(k) * exp ( b * x ) * l_table(i) * l_table(j)
          end do
        end do

      end do

      return
      end
      subroutine l_integral ( n, exact )

c*********************************************************************72
c
cc L_INTEGRAL evaluates a monomial integral associated with L(n,x).
c
c  Discussion:
c
c    The integral:
c
c      integral ( 0 <= x .lt. +oo ) x^n * exp ( -x ) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the exponent.
c    0 <= N.
c
c    Output, double precision EXACT, the value of the integral.
c
      implicit none

      double precision exact
      integer n
      double precision r8_factorial

      exact = r8_factorial ( n )

      return
      end
      subroutine l_polynomial ( m, n, x, v )

c*********************************************************************72
c
cc L_POLYNOMIAL evaluates the Laguerre polynomial L(n,x).
c
c  First terms:
c
c      1
c     -X     +  1
c   (  X^2 -  4 X      +  2 ) / 2
c   ( -X^3 +  9 X^2 -  18 X    +    6 ) / 6
c   (  X^4 - 16 X^3 +  72 X^2 -   96 X +      24 ) / 24
c   ( -X^5 + 25 X^4 - 200 X^3 +  600 X^2 -   600 X    +  120 ) / 120
c   (  X^6 - 36 X^5 + 450 X^4 - 2400 X^3 +  5400 X^2 -  4320 X     + 720 ) 
c     / 720
c   ( -X^7 + 49 X^6 - 882 X^5 + 7350 X^4 - 29400 X^3 + 52920 X^2 - 35280 X 
c     + 5040 ) / 5040
c
c  Recursion:
c
c    L(0,X) = 1
c    L(1,X) = 1 - X
c    L(N,X) = (2*N-1-X)/N * L(N-1,X) - (N-1)/N * L(N-2,X)
c
c  Orthogonality:
c
c    Integral ( 0 <= X .lt. oo ) exp ( - X ) * L(N,X) * L(M,X) dX = delta ( M, N )
c
c  Relations:
c
c    L(N,X) = (-1)^N / N! * exp ( x ) * (d/dx)^n ( exp ( - x ) * x^n )  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision V(M,0:N), the function values.
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

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        v(i,1) = 1.0D+00 - x(i)
      end do
   
      do j = 2, n
        do i = 1, m

          v(i,j) = ( ( dble ( 2 * j - 1 ) - x(i) ) * v(i,j-1)   
     &               - dble (     j - 1 )          * v(i,j-2) ) 
     &               / dble (     j     )

        end do
      end do

      return
      end
      subroutine l_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc L_POLYNOMIAL_COEFFICIENTS: coefficients of the Laguerre polynomial L(n,x).
c
c  First terms:
c
c    0: 1
c    1: 1  -1
c    2: 1  -2  1/2
c    3: 1  -3  3/2  1/6
c    4: 1  -4  4   -2/3  1/24
c    5: 1  -5  5   -5/3  5/24  -1/120
c
c  Recursion:
c
c    L(0) = ( 1,  0, 0, ..., 0 )
c    L(1) = ( 1, -1, 0, ..., 0 )
c    L(N) = (2*N-1-X) * L(N-1) - (N-1) * L(N-2) / N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
c    Output, double precision C(0:N,0:N), the coefficients of the
c    Laguerre polynomials of degree 0 through N.  Each polynomial 
c    is stored as a row.
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

      do i = 0, n
        c(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      c(1,1) = -1.0D+00
     
      do i = 2, n
        do j = 1, n

          c(i,j) = ( 
     &        dble ( 2 * i - 1 ) * c(i-1,j)     
     &      + dble (   - i + 1 ) * c(i-2,j)     
     &      -                      c(i-1,j-1) ) 
     &      / dble (     i )

        end do
      end do

      return
      end
      subroutine l_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc L_POLYNOMIAL_VALUES: some values of the Laguerre polynomial L(n,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      LaguerreL[n,x]
c
c  Differential equation:
c
c    X * Y'' + (1-X) * Y' + N * Y = 0
c
c  First terms:
c
c      1
c     -X    +  1
c   (  X^2 -  4 X     +  2 ) / 2
c   ( -X^3 +  9 X^2 -  18 X    +    6 ) / 6
c   (  X^4 - 16 X^3 +  72 X^2 -   96 X +      24 ) / 24
c   ( -X^5 + 25 X^4 - 200 X^3 +  600 X^2 -  600 x    +  120 ) / 120
c   (  X^6 - 36 X^5 + 450 X^4 - 2400 X^3 + 5400 X^2 - 4320 X + 720 ) / 720
c   ( -X^7 + 49 X^6 - 882 X^5 + 7350 X^4 - 29400 X^3 + 52920 X^2 - 35280 X 
c     + 5040 ) / 5040
c
c  Recursion:
c
c    L(0,X) = 1,
c    L(1,X) = 1-X,
c    N * L(N,X) = (2*N-1-X) * L(N-1,X) - (N-1) * L(N-2,X)
c
c  Orthogonality:
c
c    Integral ( 0 <= X .lt. oo ) exp ( - X ) * L(N,X) * L(M,X) dX
c    = 0 if N /= M
c    = 1 if N .eq. M
c
c  Special values:
c
c    L(N,0) = 1.
c
c  Relations:
c
c    L(N,X) = (-1)^N / N! * exp ( x ) * (d/dx)^n ( exp ( - x ) * x^n )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
      parameter ( n_max = 17 )

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
     &   0.0000000000000000D+00,
     &  -0.5000000000000000D+00,
     &  -0.6666666666666667D+00,
     &  -0.6250000000000000D+00,
     &  -0.4666666666666667D+00,
     &  -0.2569444444444444D+00,
     &  -0.4047619047619048D-01,
     &   0.1539930555555556D+00,
     &   0.3097442680776014D+00,
     &   0.4189459325396825D+00,
     &   0.4801341790925124D+00,
     &   0.4962122235082305D+00,
     &  -0.4455729166666667D+00,
     &   0.8500000000000000D+00,
     &  -0.3166666666666667D+01,
     &   0.3433333333333333D+02 /
      data n_vec /
     &   0,  1,  2,
     &   3,  4,  5,
     &   6,  7,  8,
     &   9, 10, 11,
     &  12,  5,  5,
     &   5,  5 /
      data x_vec /
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  1.0D+00,
     &  0.5D+00,
     &  3.0D+00,
     &  5.0D+00,
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
      subroutine l_polynomial_zeros ( n, x )

c*********************************************************************72
c
cc L_POLYNOMIAL_ZEROS: zeros of the Laguerre polynomial L(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Output, double precision X(N), the zeros.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = 1.0D+00
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        bj(i) = dble ( i )
      end do

      do i = 1, n
        x(i) = dble ( 2 * i - 1 )
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      return
      end
      subroutine l_power_product ( p, e, table )

c*********************************************************************72
c
cc L_POWER_PRODUCT: power product table for L(n,x).
c
c  Discussion:
c
c    Let L(n,x) represent the Laguerre polynomial of degree n.  
c
c    For polynomial chaos applications, it is of interest to know the
c    value of the integrals of products of X^E with every possible pair
c    of basis functions.  That is, we'd like to form
c
c      Tij = Integral ( 0 <= X .lt. +oo ) x^e * L(i,x) * L(j,x) * exp (-x) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c    Input, integer E, the exponent of X.
c    0 <= E.
c
c    Output, double precision TABLE(0:P,0:P), the table of integrals.  
c    TABLE(I,J) represents the weighted integral of x^E * L(i,x) * L(j,x).
c
      implicit none

      integer e
      integer p

      integer i
      integer j
      integer k
      double precision l_table(0:p)
      integer order
      double precision table(0:p,0:p)
      double precision w_table(p+1+(e+1)/2)
      double precision x
      double precision x_table(p+1+(e+1)/2)

      do j = 0, p
        do i = 0, p
          table(i,j) = 0.0D+00
        end do
      end do

      order = p + 1 + ( e + 1 ) / 2

      call l_quadrature_rule ( order, x_table, w_table )

      do k = 1, order

        x = x_table(k)
        call l_polynomial ( 1, p, x, l_table )

        if ( e .eq. 0 ) then

          do j = 0, p
            do i = 0, p
              table(i,j) = table(i,j) 
     &          + w_table(k) * l_table(i) * l_table(j)
            end do
          end do

        else

          do j = 0, p
            do i = 0, p
              table(i,j) = table(i,j) 
     &          + w_table(k) * ( x ** e ) * l_table(i) * l_table(j)
            end do
          end do

        end if

      end do

      return
      end
      subroutine l_quadrature_rule ( n, x, w )

c*********************************************************************72
c
cc L_QUADRATURE_RULE: Gauss-Laguerre quadrature based on L(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c  Parameters:
c
c    Input, integer N, the order.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = 1.0D+00
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        bj(i) = dble ( i )
      end do

      do i = 1, n
        x(i) = dble ( 2 * i - 1 )
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      do i = 1, n
        w(i) = w(i)**2
      end do

      return
      end
      subroutine lf_integral ( n, alpha, exact )

c*********************************************************************72
c
cc LF_INTEGRAL evaluates a monomial integral associated with Lf(n,alpha,x).
c
c  Discussion:
c
c    The integral:
c
c      integral ( 0 <= x .lt. +oo ) x^n * x^alpha * exp ( -x ) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the exponent.
c    0 <= N.
c
c    Input, double precision ALPHA, the exponent of X in the weight function.
c
c    Output, double precision EXACT, the value of the integral.
c
      implicit none

      double precision alpha
      double precision arg
      double precision exact
      integer n
      double precision r8_gamma

      arg = alpha + dble ( n + 1 )

      exact = r8_gamma ( arg )

      return
      end
      subroutine lf_function ( m, n, alpha, x, cx )

c*********************************************************************72
c
cc LF_FUNCTION evaluates the Laguerre function Lf(n,alpha,x).
c
c  Recursion:
c
c    Lf(0,ALPHA,X) = 1
c    Lf(1,ALPHA,X) = 1+ALPHA-X
c
c    Lf(N,ALPHA,X) = (2*N-1+ALPHA-X)/N * Lf(N-1,ALPHA,X) 
c                      - (N-1+ALPHA)/N * Lf(N-2,ALPHA,X)
c
c  Restrictions:
c
c    -1 .lt. ALPHA
c
c  Special values:
c
c    Lf(N,0,X) = L(N,X).
c    Lf(N,ALPHA,X) = LM(N,ALPHA,X) for ALPHA integral.
c
c  Norm:
c
c    Integral ( 0 <= X .lt. +oo ) exp ( - X ) * Lf(N,ALPHA,X)^2 dX
c    = Gamma ( N + ALPHA + 1 ) / N!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order function to compute.
c
c    Input, double precision ALPHA, the parameter.  -1 .lt. ALPHA is required.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision CX(1:M,0:N), the functions of 
c    degrees 0 through N evaluated at the points X.
c
      implicit none

      integer m
      integer n

      double precision alpha
      double precision cx(1:m,0:n)
      integer i
      integer j
      double precision x(1:m)

      if ( alpha .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LF_FUNCTION - Fatal error!'
        write ( *, '(a,g14.6)' ) '  The input value of ALPHA is ', alpha
        write ( *, '(a)' ) '  but ALPHA must be greater than -1.'
        stop
      end if
     
      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        cx(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, m
        cx(i,1) = 1.0D+00 + alpha - x(i)
      end do

      do j = 2, n
        do i = 1, m
          cx(i,j) = ( ( dble ( 2 * j - 1 ) 
     &              + alpha - x(i) ) * cx(i,j-1)   
     &              + ( dble ( - j + 1 ) 
     &              - alpha          ) * cx(i,j-2) ) 
     &              / dble ( j )
        end do
      end do

      return
      end
      subroutine lf_function_values ( n_data, n, a, x, fx )

c*********************************************************************72
c
cc LF_FUNCTION_VALUES returns values of the Laguerre function Lf(n,alpha,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      LaguerreL[n,a,x]
c
c    The functions satisfy the following differential equation:
c
c      X * Y'' + (ALPHA+1-X) * Y' + N * Y = 0
c
c    Function values can be generated by the recursion:
c
c      Lf(0,ALPHA,X) = 1
c      Lf(1,ALPHA,X) = 1+ALPHA-X
c
c      Lf(N,ALPHA,X) = ( (2*N-1+ALPHA-X) * Lf(N-1,ALPHA,X)
c                          - (N-1+ALPHA) * Lf(N-2,ALPHA,X) ) / N
c
c    The parameter ALPHA is required to be greater than -1.
c
c    For ALPHA = 0, the generalized Laguerre function Lf(N,ALPHA,X)
c    is equal to the Laguerre polynomial L(N,X).
c
c    For ALPHA integral, the generalized Laguerre function
c    Lf(N,ALPHA,X) equals the associated Laguerre polynomial Lm(N,ALPHA,X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision A, the parameter.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision a
      double precision a_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save fx_vec
      save n_vec
      save x_vec

      data a_vec /
     &  0.00D+00,
     &  0.25D+00,
     &  0.50D+00,
     &  0.75D+00,
     &  1.50D+00,
     &  2.50D+00,
     &  5.00D+00,
     &  1.20D+00,
     &  1.20D+00,
     &  1.20D+00,
     &  1.20D+00,
     &  1.20D+00,
     &  1.20D+00,
     &  5.20D+00,
     &  5.20D+00,
     &  5.20D+00,
     &  5.20D+00,
     &  5.20D+00,
     &  5.20D+00,
     &  5.20D+00 /
      data fx_vec /
     &   0.3726399739583333D-01,
     &   0.3494791666666667D+00,
     &   0.8710042317708333D+00,
     &   0.1672395833333333D+01,
     &   0.6657625325520833D+01,
     &   0.2395726725260417D+02,
     &   0.2031344319661458D+03,
     &   0.1284193996800000D+02,
     &   0.5359924801587302D+01,
     &   0.9204589064126984D+00,
     &  -0.1341585114857143D+01,
     &  -0.2119726307555556D+01,
     &  -0.1959193658349206D+01,
     &   0.1000000000000000D+01,
     &   0.5450000000000000D+01,
     &   0.1720125000000000D+02,
     &   0.4110393750000000D+02,
     &   0.8239745859375000D+02,
     &   0.1460179186171875D+03,
     &   0.2359204608298828D+03 /
      data n_vec /
     &   5,
     &   5,
     &   5,
     &   5,
     &   5,
     &   5,
     &   5,
     &   8,
     &   8,
     &   8,
     &   8,
     &   8,
     &   8,
     &   0,
     &   1,
     &   2,
     &   3,
     &   4,
     &   5,
     &   6 /
      data x_vec /
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.00D+00,
     &  0.20D+00,
     &  0.40D+00,
     &  0.60D+00,
     &  0.80D+00,
     &  1.00D+00,
     &  0.75D+00,
     &  0.75D+00,
     &  0.75D+00,
     &  0.75D+00,
     &  0.75D+00,
     &  0.75D+00,
     &  0.75D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        a = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        a = a_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine lf_function_zeros ( n, alpha, x )

c*********************************************************************72
c
cc LF_FUNCTION_ZEROS returns the zeros of Lf(n,alpha,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c  Parameters:
c
c    Input, integer N, the order.
c
c    Input, double precision ALPHA, the exponent of the X factor.
c    ALPHA must be nonnegative.
c
c    Output, double precision X(N), the zeros.
c
      implicit none

      integer n

      double precision alpha
      double precision bj(n)
      integer i
      double precision i_r8
      double precision r8_gamma
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = r8_gamma ( alpha + 1.0D+00 )
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        i_r8 = dble ( i )
        bj(i) = i_r8 * ( i_r8 + alpha )
      end do

      do i = 1, n
        bj(i) = sqrt ( bj(i) )
      end do

      do i = 1, n
        i_r8 = dble ( i )
        x(i) = 2.0D+00 * i_r8 - 1.0D+00 + alpha
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      return
      end
      subroutine lf_quadrature_rule ( n, alpha, x, w )

c*********************************************************************72
c
cc LF_QUADRATURE_RULE: Gauss-Laguerre quadrature rule for Lf(n,alpha,x);
c
c  Discussion:
c
c    The integral:
c
c      integral ( 0 <= x .lt. +oo ) exp ( - x ) * x^alpha * f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c  Parameters:
c
c    Input, integer N, the order.
c
c    Input, double precision ALPHA, the exponent of the X factor.
c    ALPHA must be nonnegative.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision alpha
      double precision bj(n)
      integer i
      double precision i_r8
      double precision r8_gamma
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = r8_gamma ( alpha + 1.0D+00 )
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        i_r8 = dble ( i )
        bj(i) = i_r8 * ( i_r8 + alpha )
      end do

      do i = 1, n
        bj(i) = sqrt ( bj(i) )
      end do

      do i = 1, n
        i_r8 = dble ( i )
        x(i) = 2.0D+00 * i_r8 - 1.0D+00 + alpha
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      do i = 1, n
        w(i) = w(i)**2
      end do

      return
      end
      subroutine lm_integral ( n, m, exact )

c*********************************************************************72
c
cc LM_INTEGRAL evaluates a monomial integral associated with Lm(n,m,x).
c
c  Discussion:
c
c    The integral:
c
c      integral ( 0 <= x .lt. +oo ) x^n * x^m * exp ( -x ) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the exponent.
c    0 <= N.
c
c    Input, integer M, the parameter.
c    0 <= M.
c
c    Output, double precision EXACT, the value of the integral.
c
      implicit none

      double precision exact
      integer m
      integer n
      double precision r8_factorial

      exact = r8_factorial ( n + m )

      return
      end
      subroutine lm_polynomial ( mm, n, m, x, cx )

c*********************************************************************72
c
cc LM_POLYNOMIAL evaluates Laguerre polynomials Lm(n,m,x).
c
c  First terms:
c
c    M = 0
c
c    Lm(0,0,X) =   1
c    Lm(1,0,X) =  -X   +  1
c    Lm(2,0,X) =   X^2 -  4 X   +  2
c    Lm(3,0,X) =  -X^3 +  9 X^2 -  18 X   +    6
c    Lm(4,0,X) =   X^4 - 16 X^3 +  72 X^2 -   96 X +     24
c    Lm(5,0,X) =  -X^5 + 25 X^4 - 200 X^3 +  600 X^2 -  600 x   +  120
c    Lm(6,0,X) =   X^6 - 36 X^5 + 450 X^4 - 2400 X^3 + 5400 X^2 - 4320 X + 720
c
c    M = 1
c
c    Lm(0,1,X) =    0
c    Lm(1,1,X) =   -1,
c    Lm(2,1,X) =    2 X - 4,
c    Lm(3,1,X) =   -3 X^2 + 18 X - 18,
c    Lm(4,1,X) =    4 X^3 - 48 X^2 + 144 X - 96
c
c    M = 2
c
c    Lm(0,2,X) =    0
c    Lm(1,2,X) =    0,
c    Lm(2,2,X) =    2,
c    Lm(3,2,X) =   -6 X + 18,
c    Lm(4,2,X) =   12 X^2 - 96 X + 144
c
c    M = 3
c
c    Lm(0,3,X) =    0
c    Lm(1,3,X) =    0,
c    Lm(2,3,X) =    0,
c    Lm(3,3,X) =   -6,
c    Lm(4,3,X) =   24 X - 96
c
c    M = 4
c
c    Lm(0,4,X) =    0
c    Lm(1,4,X) =    0
c    Lm(2,4,X) =    0
c    Lm(3,4,X) =    0
c    Lm(4,4,X) =   24
c
c  Recursion:
c
c    Lm(0,M,X)   = 1 
c    Lm(1,M,X)   = (M+1-X)
c
c    if 2 <= N:
c
c      Lm(N,M,X)   = ( (M+2*N-1-X) * Lm(N-1,M,X) 
c                   +   (1-M-N)    * Lm(N-2,M,X) ) / N
c
c  Special values:
c
c    For M = 0, the associated Laguerre polynomials Lm(N,M,X) are equal 
c    to the Laguerre polynomials L(N,X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Input, integer M, the parameter.  M must be nonnegative.
c
c    Input, double precision X(MM), the evaluation points.
c
c    Output, double precision CX(MM,0:N), the associated Laguerre polynomials 
c    of degrees 0 through N evaluated at the evaluation points.
c
      implicit none

      integer mm
      integer n

      double precision cx(mm,0:n)
      integer i
      integer j
      integer m
      double precision x(mm)

      if ( m .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LM_POLYNOMIAL - Fatal error!'
        write ( *, '(a,i8)' ) '  Input value of M = ', m
        write ( *, '(a)' ) '  but M must be nonnegative.'
        stop
      end if

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, mm
        cx(i,0) = 1.0D+00
      end do

      if ( n .eq. 0 ) then
        return
      end if

      do i = 1, mm
        cx(i,1) = dble ( m + 1 ) - x(i)
      end do

      do j = 2, n
        do i = 1, mm
          cx(i,j) = 
     &      ( ( dble (   m + 2 * j - 1 ) - x(i) ) * cx(i,j-1) 
     &        + dble ( - m     - j + 1 )          * cx(i,j-2) ) 
     &        / dble (           j )
        end do
      end do

      return
      end
      subroutine lm_polynomial_coefficients ( n, m, c )

c*********************************************************************72
c
cc LM_POLYNOMIAL_COEFFICIENTS: coefficients of Laguerre polynomial Lm(n,m,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 August 2013
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
c    Input, integer M, the parameter.
c
c    Output, double precision C(0:N,0:N), the coefficients of the
c    Laguerre polynomials of degree 0 through N.
c
      implicit none

      integer n

      double precision c(0:n,0:n)
      integer i
      integer j
      integer m

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

      c(1,0) = dble ( m + 1 )
      c(1,1) = -1.0D+00
     
      do i = 2, n

        do j = 0, i
          c(i,j) = ( dble (   m + 2 * i - 1 ) * c(i-1,j)   
     &             + dble ( - m     - i + 1 ) * c(i-2,j) ) 
     &             / dble (           i )
        end do

        do j = 1, i
          c(i,j) = c(i,j) - c(i-1,j-1) / dble ( i )
        end do

      end do

      return
      end
      subroutine lm_polynomial_values ( n_data, n, m, x, fx )

c*********************************************************************72
c
cc LM_POLYNOMIAL_VALUES returns values of Laguerre polynomials Lm(n,m,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      LaguerreL[n,m,x]
c
c    The associated Laguerre polynomials may be generalized so that the
c    parameter M is allowed to take on arbitrary noninteger values.
c    The resulting function is known as the generalized Laguerre function.
c
c    The polynomials satisfy the differential equation:
c
c      X * Y'' + (M+1-X) * Y' + (N-M) * Y = 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, integer M, the parameter.
c
c    Output, double precision X, the point where the function is evaluated.
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
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1000000000000000D+01,
     &  0.1500000000000000D+01,
     &  0.1625000000000000D+01,
     &  0.1479166666666667D+01,
     &  0.1148437500000000D+01,
     &  0.4586666666666667D+00,
     &  0.2878666666666667D+01,
     &  0.8098666666666667D+01,
     &  0.1711866666666667D+02,
     &  0.1045328776041667D+02,
     &  0.1329019368489583D+02,
     &  0.5622453647189670D+02,
     &  0.7484729341779436D+02,
     &  0.3238912982762806D+03,
     &  0.4426100000097533D+03,
     &  0.1936876572288250D+04 /
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
      subroutine lm_polynomial_zeros ( n, m, x )

c*********************************************************************72
c
cc LM_POLYNOMIAL_ZEROS returns the zeros for Lm(n,m,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c  Parameters:
c
c    Input, integer N, the order.
c
c    Input, integer M, the parameter.
c    0 <= M.
c
c    Output, double precision X(N), the zeros.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      integer m
      double precision r8_factorial
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = r8_factorial ( m )
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        bj(i) = dble ( i * ( i + m ) )
      end do

      do i = 1, n
        bj(i) = sqrt ( bj(i) )
      end do

      do i = 1, n
        x(i) = dble ( 2 * i - 1 + m )
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      return
      end
      subroutine lm_quadrature_rule ( n, m, x, w )

c*********************************************************************72
c
cc LM_QUADRATURE_RULE: Gauss-Laguerre quadrature rule for Lm(n,m,x);
c
c  Discussion:
c
c    The integral:
c
c      integral ( 0 <= x .lt. +oo ) exp ( - x ) * x^m * f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2013
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
c  Parameters:
c
c    Input, integer N, the order.
c
c    Input, integer M, the parameter.
c    0 <= M.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      integer m
      double precision r8_factorial
      double precision w(n)
      double precision x(n)
      double precision zemu
c
c  Define the zero-th moment.
c
      zemu = r8_factorial ( m )
c
c  Define the Jacobi matrix.
c
      do i = 1, n
        bj(i) = dble ( i * ( i + m ) )
      end do

      do i = 1, n
        bj(i) = sqrt ( bj(i) )
      end do

      do i = 1, n
        x(i) = dble ( 2 * i - 1 + m )
      end do

      w(1) = sqrt ( zemu )
      do i = 2, n
        w(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( n, x, bj, w )

      do i = 1, n
        w(i) = w(i)**2
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
      function r8_gamma ( x )

c*********************************************************************72
c
cc R8_GAMMA evaluates Gamma(X) for a real argument.
c
c  Discussion:
c
c    This function was originally named DGAMMA.
c
c    However, a number of Fortran compilers now include a library 
c    function of this name.  To avoid conflicts, this function was
c    renamed R8_GAMMA.
c
c    This routine calculates the GAMMA function for a real argument X.
c    Computation is based on an algorithm outlined in reference 1.
c    The program uses rational functions that approximate the GAMMA
c    function to at least 20 significant decimal digits.  Coefficients
c    for the approximation over the interval (1,2) are unpublished.
c    Those for the approximation for 12 <= X are from reference 2.
c
c  Modified:
c
c    18 January 2008
c
c  Author:
c
c    William Cody, Laura Stoltz
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
      double precision half
      integer i
      integer n
      double precision one
      double precision p(8)
      logical parity
      double precision pi
      double precision q(8)
      double precision r8_gamma
      double precision res
      double precision sqrtpi
      double precision sum
      double precision twelve
      double precision two
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
      double precision zero
c
c  Mathematical constants
c
      data one /1.0D+00 /
      data half /0.5D+00/
      data twelve /12.0D+00/
      data two /2.0D+00 /
      data zero /0.0D+00/
      data sqrtpi /0.9189385332046727417803297D+00/
      data pi /3.1415926535897932384626434D+00/
c
c  Machine dependent parameters
c
      data xbig / 171.624D+00 /
      data xminin / 2.23D-308 /
      data eps /2.22D-16/
      data xinf /1.79D+308/
c
c  Numerator and denominator coefficients for rational minimax
c  approximation over (1,2).
c
      data p/
     & -1.71618513886549492533811d+00,
     &  2.47656508055759199108314d+01,
     & -3.79804256470945635097577d+02,
     &  6.29331155312818442661052d+02,
     &  8.66966202790413211295064d+02,
     & -3.14512729688483675254357d+04,
     & -3.61444134186911729807069d+04,
     &  6.64561438202405440627855d+04/

      data q/
     & -3.08402300119738975254353D+01,
     &  3.15350626979604161529144D+02,
     & -1.01515636749021914166146D+03,
     & -3.10777167157231109440444D+03,
     &  2.25381184209801510330112D+04,
     &  4.75584627752788110767815D+03,
     & -1.34659959864969306392456D+05,
     & -1.15132259675553483497211D+05/
c
c  Coefficients for minimax approximation over (12, INF).
c
      data c/
     & -1.910444077728D-03,
     &  8.4171387781295D-04,
     & -5.952379913043012D-04,
     &  7.93650793500350248D-04,
     & -2.777777777777681622553D-03,
     &  8.333333333333333331554247D-02,
     &  5.7083835261D-03/

      parity = .false.
      fact = one
      n = 0
      y = x
c
c  Argument is negative.
c
      if ( y .le. zero ) then

        y = - x
        y1 = aint ( y )
        res = y - y1

        if ( res .ne. zero ) then

          if ( y1 .ne. aint ( y1 * half ) * two ) then
            parity = .true.
          end if

          fact = - pi / sin ( pi * res )
          y = y + one

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
c  Argument .lt. EPS.
c
        if ( xminin .le. y ) then
          res = one / y
        else
          res = xinf
          r8_gamma = res
          return
        end if

      else if ( y .lt. twelve ) then

        y1 = y
c
c  0.0 .lt. argument .lt. 1.0.
c
        if ( y .lt. one ) then

          z = y
          y = y + one
c
c  1.0 .lt. argument .lt. 12.0.
c  Reduce argument if necessary.
c
        else

          n = int ( y ) - 1
          y = y - dble ( n )
          z = y - one

        end if
c
c  Evaluate approximation for 1.0 .lt. argument .lt. 2.0.
c
        xnum = zero
        xden = one
        do i = 1, 8
          xnum = ( xnum + p(i) ) * z
          xden = xden * z + q(i)
        end do

        res = xnum / xden + one
c
c  Adjust result for case  0.0 .lt. argument .lt. 1.0.
c
        if ( y1 .lt. y ) then

          res = res / y1
c
c  Adjust result for case 2.0 .lt. argument .lt. 12.0.
c
        else if ( y .lt. y1 ) then

          do i = 1, n
            res = res * y
            y = y + one
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
          sum = sum + ( y - half ) * log ( y )
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

      if ( fact .ne. one ) then
        res = fact / res
      end if

      r8_gamma = res

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
