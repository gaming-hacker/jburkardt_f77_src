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
      function j_double_product_integral ( i, j, a, b )

c*********************************************************************72
c
cc J_DOUBLE_PRODUCT_INTEGRAL: integral of J(i,x)*J(j,x)*(1-x)^a*(1+x)^b.
c
c  Discussion:
c
c    VALUE = integral ( -1 <= x <= +1 ) J(i,x)*J(j,x)*(1-x)^a*(1+x)^b dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the polynomial indices.
c
c    Input, double precision A, B, the parameters.
c    -1 < A, B.
c
c    Output, double precision VALUE, the value of the integral.
c
      implicit none

      double precision a
      double precision b
      integer i
      double precision i_r8
      integer j
      double precision j_double_product_integral
      double precision r8_factorial
      double precision value

      if ( i .ne. j ) then

        value = 0.0D+00

      else

        i_r8 = dble ( i )

        value = 2.0D+00 ** ( a + b + 1.0D+00 ) 
     &    / ( 2.0D+00 * i_r8 + a + b + 1.0D+00 ) 
     &    * gamma ( i_r8 + a + 1.0D+00 ) 
     &    * gamma ( i_r8 + b + 1.0D+00 ) 
     &    / r8_factorial ( i ) 
     &    / gamma ( i_r8 + a + b + 1.0D+00 )

      end if

      j_double_product_integral = value

      return
      end
      function j_integral ( n )

c*********************************************************************72
c
cc J_INTEGRAL evaluates a monomial integral associated with J(n,a,b,x).
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x < +1 ) x^n dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
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
c    Output, double precision J_INTEGRAL, the value of the integral.
c
      implicit none

      double precision j_integral
      integer n
      double precision value

      if ( mod ( n, 2 ) .eq. 1 ) then
        value = 0.0D+00
      else
        value = 2.0D+00 / dble ( n + 1 )
      end if

      j_integral = value

      return
      end
      subroutine j_polynomial ( m, n, alpha, beta, x, cx )

c*********************************************************************72
c
cc J_POLYNOMIAL evaluates the Jacobi polynomials J(n,a,b,x).
c
c  Differential equation:
c
c    (1-X*X) Y'' + (BETA-ALPHA-(ALPHA+BETA+2) X) Y' + N (N+ALPHA+BETA+1) Y = 0
c
c  Recursion:
c
c    P(0,ALPHA,BETA,X) = 1,
c
c    P(1,ALPHA,BETA,X) = ( (2+ALPHA+BETA)*X + (ALPHA-BETA) ) / 2
c
c    P(N,ALPHA,BETA,X)  = 
c      ( 
c        (2*N+ALPHA+BETA-1) 
c        * ((ALPHA^2-BETA^2)+(2*N+ALPHA+BETA)*(2*N+ALPHA+BETA-2)*X) 
c        * P(N-1,ALPHA,BETA,X)
c        -2*(N-1+ALPHA)*(N-1+BETA)*(2*N+ALPHA+BETA) * P(N-2,ALPHA,BETA,X)
c      ) / 2*N*(N+ALPHA+BETA)*(2*N-2+ALPHA+BETA)
c
c  Restrictions:
c
c    -1 < ALPHA
c    -1 < BETA
c
c  Norm:
c
c    Integral ( -1 <= X <= 1 ) ( 1 - X )^ALPHA * ( 1 + X )^BETA 
c      * P(N,ALPHA,BETA,X)^2 dX 
c    = 2^(ALPHA+BETA+1) * Gamma ( N + ALPHA + 1 ) * Gamma ( N + BETA + 1 ) /
c      ( 2 * N + ALPHA + BETA ) * N! * Gamma ( N + ALPHA + BETA + 1 )
c
c  Special values:
c
c    P(N,ALPHA,BETA,1) = (N+ALPHA)!/(N!*ALPHA!) for integer ALPHA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 August 2013
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
c    Input, double precision ALPHA, one of the parameters defining the Jacobi
c    polynomials, ALPHA must be greater than -1.
c
c    Input, double precision BETA, the second parameter defining the Jacobi
c    polynomials, BETA must be greater than -1.
c
c    Input, double precision X(M), the point at which the polynomials are 
c    to be evaluated.
c
c    Output, double precision CX(M,0:N), the values of the first N+1 Jacobi
c    polynomials at the point X.
c
      implicit none

      integer m
      integer n

      double precision alpha
      double precision beta
      double precision cx(1:m,0:n)
      double precision c1
      double precision c2
      double precision c3
      double precision c4
      integer i
      integer j
      double precision r_j
      double precision x(m)

      if ( alpha .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'J_POLYNOMIAL - Fatal error!'
        write ( *, '(a,g14.6)' ) 
     &    '  Illegal input value of ALPHA = ', alpha
        write ( *, '(a)' ) '  But ALPHA must be greater than -1.'
        stop
      end if
     
      if ( beta .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'J_POLYNOMIAL - Fatal error!'
        write ( *, '(a,g14.6)' ) 
     &    '  Illegal input value of BETA = ', beta
        write ( *, '(a)' ) '  But BETA must be greater than -1.'
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
        cx(i,1) = ( 1.0D+00 + 0.5D+00 * ( alpha + beta ) ) * x(i) 
     &    + 0.5D+00 * ( alpha - beta )
      end do
     
      do j = 2, n

        r_j = dble ( j ) 

        c1 = 2.0D+00 * r_j * ( r_j + alpha + beta ) 
     &     * ( 2.0D+00 * r_j - 2.0D+00 + alpha + beta )

        c2 = ( 2.0D+00 * r_j - 1.0D+00 + alpha + beta ) 
     &     * ( 2.0D+00 * r_j  + alpha + beta ) 
     &     * ( 2.0D+00 * r_j - 2.0D+00 + alpha + beta )

        c3 = ( 2.0D+00 * r_j - 1.0D+00 + alpha + beta ) 
     &    * ( alpha + beta ) * ( alpha - beta )

        c4 = - 2.0D+00 * ( r_j - 1.0D+00 + alpha ) 
     &    * ( r_j - 1.0D+00 + beta )  * ( 2.0D+00 * r_j + alpha + beta )

        do i = 1, m
          cx(i,j) = ( ( c3 + c2 * x(i) ) * cx(i,j-1) 
     &      + c4 * cx(i,j-2) ) / c1
        end do

      end do

      return
      end
      subroutine j_polynomial_values ( n_data, n, a, b, x, fx )

c*********************************************************************72
c
cc J_POLYNOMIAL_VALUES returns some values of the Jacobi polynomial.
c
c  Discussion:
c
c    In Mathematica, the function
c
c      JacobiP[ n, a, b, x ]
c
c    returns the value of the Jacobi polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 August 2013
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
c    Output, integer N, the degree of the polynomial.
c
c    Output, double precision A, B, parameters of the function.
c
c    Output, double precision X, the argument of the function.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 26 )

      double precision a
      double precision a_vec(n_max)
      double precision b
      double precision b_vec(n_max)
      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save a_vec
      save b_vec
      save fx_vec
      save n_vec
      save x_vec

      data a_vec /
     &   0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     &   0.0D+00, 0.0D+00, 1.0D+00, 2.0D+00, 
     &   3.0D+00, 4.0D+00, 5.0D+00, 0.0D+00, 
     &   0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     &   0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     &   0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     &   0.0D+00, 0.0D+00 /
      data b_vec /
     &  1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 
     &  3.0D+00, 4.0D+00, 5.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 
     &  1.0D+00, 1.0D+00 /
      data fx_vec /
     &    1.000000000000000D+00, 
     &    0.250000000000000D+00, 
     &   -0.375000000000000D+00, 
     &   -0.484375000000000D+00, 
     &   -0.132812500000000D+00, 
     &    0.275390625000000D+00, 
     &   -0.164062500000000D+00, 
     &   -1.174804687500000D+00, 
     &   -2.361328125000000D+00, 
     &   -2.616210937500000D+00, 
     &    0.117187500000000D+00, 
     &    0.421875000000000D+00, 
     &    0.504882812500000D+00, 
     &    0.509765625000000D+00, 
     &    0.430664062500000D+00, 
     &   -6.000000000000000D+00, 
     &    0.038620000000000D+00, 
     &    0.811840000000000D+00, 
     &    0.036660000000000D+00, 
     &   -0.485120000000000D+00, 
     &   -0.312500000000000D+00, 
     &    0.189120000000000D+00, 
     &    0.402340000000000D+00, 
     &    0.012160000000000D+00, 
     &   -0.439620000000000D+00, 
     &    1.000000000000000D+00 /
      data n_vec /
     &   0, 1, 2, 3, 
     &   4, 5, 5, 5, 
     &   5, 5, 5, 5, 
     &   5, 5, 5, 5, 
     &   5, 5, 5, 5, 
     &   5, 5, 5, 5, 
     &   5, 5 /
      data x_vec /
     &   0.5D+00,  0.5D+00,  0.5D+00,  0.5D+00, 
     &   0.5D+00,  0.5D+00,  0.5D+00,  0.5D+00, 
     &   0.5D+00,  0.5D+00,  0.5D+00,  0.5D+00, 
     &   0.5D+00,  0.5D+00,  0.5D+00, -1.0D+00, 
     &  -0.8D+00, -0.6D+00, -0.4D+00, -0.2D+00, 
     &   0.0D+00,  0.2D+00,  0.4D+00,  0.6D+00, 
     &   0.8D+00,  1.0D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        a = 0.0D+00
        b = 0.0D+00
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        a = a_vec(n_data)
        b = b_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine j_polynomial_zeros ( n, alpha, beta, x )

c*********************************************************************72
c
cc J_POLYNOMIAL_ZEROS: zeros of Jacobi polynomial J(n,a,b,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt.
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
c    Input, integer, N, the order.
c
c    Input, double precision, ALPHA, BETA, the parameters.
c    -1 < ALPHA, BETA.
c
c    Output, double precision X(N), the zeros.
c
      implicit none

      integer n

      double precision a2b2
      double precision ab
      double precision abi
      double precision alpha
      double precision beta
      double precision bj(n)
      integer i
      double precision i_r8
      double precision w(n)
      double precision x(n)
      double precision zemu

      ab = alpha + beta
      abi = 2.0D+00 + ab
c
c  Define the zero-th moment.
c
      zemu = 2.0D+00 ** ( ab + 1.0D+00 ) * gamma ( alpha + 1.0D+00 ) 
     &  * gamma ( beta + 1.0D+00 ) / gamma ( abi )
c
c  Define the Jacobi matrix.
c
      x(1) = ( beta - alpha ) / abi
      do i = 2, n
        x(i) = 0.0D+00
      end do

      bj(1) = 4.0D+00 * ( 1.0D+00 + alpha ) * ( 1.0D+00 + beta ) 
     &  / ( ( abi + 1.0D+00 ) * abi * abi )
      do i = 2, n
        bj(i) = 0.0D+00
      end do

      a2b2 = beta * beta - alpha * alpha

      do i = 2, n
        i_r8 = dble ( i )
        abi = 2.0D+00 * i_r8 + ab
        x(i) = a2b2 / ( ( abi - 2.0D+00 ) * abi )
        abi = abi ** 2
        bj(i) = 4.0D+00 * i_r8 * ( i_r8 + alpha ) * ( i_r8 + beta ) 
     &    * ( i_r8 + ab ) / ( ( abi - 1.0D+00 ) * abi )
      end do

      do i = 1, n
        bj(i) =  sqrt ( bj(i) )
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
      subroutine j_quadrature_rule ( n, alpha, beta, x, w )

c*********************************************************************72
c
cc J_QUADRATURE_RULE: Gauss-Jacobi quadrature based on J(n,a,b,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2013
c
c  Author:
c
c    John Burkardt.
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
c    Input, integer, N, the order.
c
c    Input, double precision, ALPHA, BETA, the parameters.
c    -1 < ALPHA, BETA.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision a2b2
      double precision ab
      double precision abi
      double precision alpha
      double precision beta
      double precision bj(n)
      integer i
      double precision i_r8
      double precision w(n)
      double precision x(n)
      double precision zemu

      ab = alpha + beta
      abi = 2.0D+00 + ab
c
c  Define the zero-th moment.
c
      zemu = 2.0D+00 ** ( ab + 1.0D+00 ) * gamma ( alpha + 1.0D+00 ) 
     &  * gamma ( beta + 1.0D+00 ) / gamma ( abi )
c
c  Define the Jacobi matrix.
c
      x(1) = ( beta - alpha ) / abi
      do i = 2, n
        x(i) = 0.0D+00
      end do

      bj(1) = 4.0D+00 * ( 1.0D+00 + alpha ) * ( 1.0D+00 + beta ) 
     &  / ( ( abi + 1.0D+00 ) * abi * abi )
      do i = 2, n
        bj(i) = 0.0D+00
      end do

      a2b2 = beta * beta - alpha * alpha

      do i = 2, n
        i_r8 = dble ( i )
        abi = 2.0D+00 * i_r8 + ab
        x(i) = a2b2 / ( ( abi - 2.0D+00 ) * abi )
        abi = abi ** 2
        bj(i) = 4.0D+00 * i_r8 * ( i_r8 + alpha ) * ( i_r8 + beta ) 
     &    * ( i_r8 + ab ) / ( ( abi - 1.0D+00 ) * abi )
      end do

      do i = 1, n
        bj(i) =  sqrt ( bj(i) )
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
        w(i) = w(i) ** 2
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
