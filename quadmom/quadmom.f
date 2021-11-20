      subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, 
     &  rot_num )

c*********************************************************************72
c
cc JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
c
c  Discussion:
c
c    This function computes the eigenvalues and eigenvectors of a
c    real symmetric matrix, using Rutishauser's modfications of the classical
c    Jacobi rotation method with threshold pivoting.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix, which must be square, real,
c    and symmetric.
c
c    Input, integer IT_MAX, the maximum number of iterations.
c
c    Output, double precision V(N,N), the matrix of eigenvectors.
c
c    Output, double precision D(N), the eigenvalues, in descending order.
c
c    Output, integer IT_NUM, the total number of iterations.
c
c    Output, integer ROT_NUM, the total number of rotations.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision bw(n)
      double precision c
      double precision d(n)
      double precision g
      double precision gapq
      double precision h
      integer i
      integer it_max
      integer it_num
      integer j
      integer k
      integer l
      integer m
      integer p
      integer q
      integer rot_num
      double precision s
      double precision t
      double precision tau
      double precision term
      double precision termp
      double precision termq
      double precision theta
      double precision thresh
      double precision v(n,n)
      double precision w(n)
      double precision zw(n)

      do j = 1, n 
        do i = 1, n 
          v(i,j) = 0.0D+00
        end do
        v(j,j) = 1.0D+00
      end do

      do i = 1, n
        d(i) = a(i,i)
      end do

      do i = 1, n
        bw(i) = d(i)
        zw(i) = 0.0D+00
      end do

      it_num = 0
      rot_num = 0

10    continue

      if ( it_num .lt. it_max ) then

        it_num = it_num + 1
c
c  The convergence threshold is based on the size of the elements in
c  the strict upper triangle of the matrix.
c
        thresh = 0.0D+00
        do j = 1, n
          do i = 1, j - 1
            thresh = thresh + a(i,j) ** 2
          end do
        end do

        thresh = sqrt ( thresh ) / dble ( 4 * n )

        if ( thresh .eq. 0.0D+00 ) then
          go to 20
        end if

        do p = 1, n
          do q = p + 1, n

            gapq = 10.0D+00 * abs ( a(p,q) )
            termp = gapq + abs ( d(p) )
            termq = gapq + abs ( d(q) )
c
c  Annihilate tiny offdiagonal elements.
c
            if ( 4 .lt. it_num .and.
     &           termp .eq. abs ( d(p) ) .and.
     &           termq .eq. abs ( d(q) ) ) then

              a(p,q) = 0.0D+00
c
c  Otherwise, apply a rotation.
c
            else if ( thresh .le. abs ( a(p,q) ) ) then

              h = d(q) - d(p)
              term = abs ( h ) + gapq

              if ( term .eq. abs ( h ) ) then
                t = a(p,q) / h
              else
                theta = 0.5D+00 * h / a(p,q)
                t = 1.0D+00 / 
     &            ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
                if ( theta .lt. 0.0D+00 ) then
                  t = - t
                end if
              end if

              c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
              s = t * c
              tau = s / ( 1.0D+00 + c )
              h = t * a(p,q)
c
c  Accumulate corrections to diagonal elements.
c
              zw(p) = zw(p) - h
              zw(q) = zw(q) + h
              d(p) = d(p) - h
              d(q) = d(q) + h

              a(p,q) = 0.0D+00
c
c  Rotate, using information from the upper triangle of A only.
c
              do j = 1, p - 1
                g = a(j,p)
                h = a(j,q)
                a(j,p) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = p + 1, q - 1
                g = a(p,j)
                h = a(j,q)
                a(p,j) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = q + 1, n
                g = a(p,j)
                h = a(q,j)
                a(p,j) = g - s * ( h + g * tau )
                a(q,j) = h + s * ( g - h * tau )
              end do
c
c  Accumulate information in the eigenvector matrix.
c
              do j = 1, n
                g = v(j,p)
                h = v(j,q)
                v(j,p) = g - s * ( h + g * tau )
                v(j,q) = h + s * ( g - h * tau )
              end do

              rot_num = rot_num + 1

            end if

          end do
        end do

        do i = 1, n
          bw(i) = bw(i) + zw(i)
          d(i) = bw(i)
          zw(i) = 0.0D+00
        end do

        go to 10

      end if

20    continue
c
c  Restore upper triangle of input matrix.
c
      do j = 1, n
        do i = 1, j - 1
          a(i,j) = a(j,i)
        end do
      end do
c
c  Ascending sort the eigenvalues and eigenvectors.
c
      do k = 1, n - 1

        m = k

        do l = k + 1, n
          if ( d(l) .lt. d(m) ) then
            m = l
          end if
        end do

        if ( m .ne. k ) then

          t    = d(m)
          d(m) = d(k)
          d(k) = t

          do i = 1, n
            w(i)   = v(i,m)
            v(i,m) = v(i,k)
            v(i,k) = w(i)
          end do

        end if

      end do

      return
      end
      subroutine moment_method ( n, moment, x, w )

c*********************************************************************72
c
cc MOMENT_METHOD computes a quadrature rule by the method of moments.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
c  Parameters:
c
c    Input, integer N, the order of the quadrature rule.
c
c    Input, double precision MOMENT(2*N+1), moments 0 through 2*N.
c
c    Output, double precision X(N), W(N), the points and weights of the
c    quadrature rule.
c
      implicit none

      integer n

      double precision alpha(n)
      double precision beta(n-1)
      logical debug
      integer flag
      double precision h(0:n,0:n)
      integer i
      integer it_max
      integer it_num
      integer j
      double precision jacobi(n,n)
      double precision moment(0:2*n)
      double precision r(n+1,n+1)
      integer rot_num
      double precision v(n,n)
      double precision w(n)
      double precision x(n)

      debug = .false.

      if ( debug ) then
        call r8vec_print ( 2 * n + 1, moment, '  Moments:' )
      end if
c
c  Define the N+1 by N+1 Hankel matrix H(I,J) = moment(I+J).
c
      do i = 0, n
        do j = 0, n
          h(i,j) = moment(i+j)
        end do
      end do

      if ( debug ) then
        call r8mat_print ( n + 1, n + 1, h, '  Hankel matrix:' )
      end if
c
c  Compute R, the upper triangular Cholesky factor of H.
c
      call r8mat_cholesky_factor_upper ( n + 1, h, r, flag )

      if ( flag .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUADMOM_PRB01 - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  R8MAT_CHOLESKY_FACTOR_UPPER returned FLAG = ', flag
        stop 1
      end if
c
c  Compute ALPHA and BETA from R, using Golub and Welsch's formula.
c
      alpha(1) = r(1,2) / r(1,1)
      do i = 2, n
        alpha(i) = r(i,i+1) / r(i,i) - r(i-1,i) / r(i-1,i-1)
      end do

      do i = 1, n - 1
        beta(i) = r(i+1,i+1) / r(i,i)
      end do
c
c  Compute the points and weights from the moments.
c
      do j = 1, n
        do i = 1, n
          jacobi(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        jacobi(i,i) = alpha(i)
      end do

      do i = 1, n - 1
        jacobi(i,i+1) = beta(i)
        jacobi(i+1,i) = beta(i)
      end do

      if ( debug ) then
        call r8mat_print ( n, n, jacobi, '  The Jacobi matrix:' )
      end if
c
c  Get the eigendecomposition of the Jacobi matrix.
c
      it_max = 100

      call jacobi_eigenvalue ( n, jacobi, it_max, v, x, it_num, 
     &  rot_num )

      if ( debug ) then
        call r8mat_print ( n, n, v, '  Eigenvector' )
      end if

      do j = 1, n
        w(j) = moment(0) * v(1,j) ** 2
      end do

      return
      end
      subroutine moments_laguerre ( m, w )

c*********************************************************************72
c
cc MOMENTS_LAGUERRE returns moments of the Laguerre distribution.
c
c  Discussion:
c
c    pdf(x) = exp ( -x )
c    mu(k) = integral ( 0 <= x < +oo ) x^k pdf(x) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      integer k
      double precision r8_factorial
      double precision w(0:m-1)

      do k = 0, m - 1
        w(k) = r8_factorial ( k )
      end do

      return
      end
      subroutine moments_legendre ( m, a, b, w )

c*********************************************************************72
c
cc MOMENTS_LEGENDRE returns moments of the Legendre weight on [A,B].
c
c  Discussion:
c
c    mu(k) = integral ( a <= x <= b ) x^k dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision A, B, the left and right endpoints
c    of the interval.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision a
      double precision ak
      double precision b
      double precision bk
      integer k
      double precision w(0:m-1)

      bk = 1.0D+00
      ak = 1.0D+00
      do k = 0, m - 1
        bk = bk * b
        ak = ak * a
        w(k) = ( bk - ak ) / dble ( k + 1 )
      end do

      return
      end
      subroutine moments_normal_01 ( m, w )

c*********************************************************************72
c
cc MOMENTS_NORMAL_01 returns moments of the standard Normal distribution.
c
c  Discussion:
c
c    pdf(x) = exp ( -x^2/2 ) / sqrt ( pi * 2 )
c    mu(k) = integral ( -oo < x < +oo ) x^k pdf(x) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      integer k
      double precision r8_factorial2
      double precision w(0:m-1)

      w(0) = 1.0D+00

      do k = 2, m - 1, 2
        w(k) = r8_factorial2 ( k - 1 )
      end do

      do k = 1, m - 1, 2
        w(k) = 0.0D+00
      end do

      return
      end
      subroutine moments_normal ( m, mu, sigma, w )

c*********************************************************************72
c
cc MOMENTS_NORMAL returns moments of the standard Normal distribution.
c
c  Discussion:
c
c    pdf(x) = exp ( -((x-mu)/sigma)^2/2 ) / sigma / sqrt ( pi * 2 )
c    mu(k) = integral ( -oo < x < +oo ) x^k pdf(x) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      integer j
      integer j_hi
      integer k
      double precision mu
      double precision r8_choose
      double precision r8_factorial2
      double precision sigma
      double precision t
      double precision w(0:m-1)

      do k = 0, m - 1
        t = 0.0D+00
        j_hi = k / 2
        do j = 0, j_hi
          t = t + r8_choose ( k, 2 * j ) * r8_factorial2 ( 2 * j - 1 )  
     &       * sigma ** ( 2 * j ) * mu ** ( k - 2 * j )
        end do
        w(k) = t
      end do

      return
      end
      subroutine moments_truncated_normal_ab ( m, mu, sigma, a, b, w )

c*********************************************************************72
c
cc MOMENTS_TRUNCATED_NORMAL_AB: moments of the truncated Normal distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision a
      double precision b
      double precision mu
      integer order
      double precision sigma
      double precision w(0:m-1)

      do order = 0, m - 1
        call truncated_normal_ab_moment ( order, mu, sigma, a, b, 
     &    w(order) )
      end do

      return
      end
      subroutine moments_truncated_normal_a ( m, mu, sigma, a, w )

c*********************************************************************72
c
cc MOMENTS_TRUNCATED_NORMAL_A: moments of the lower truncated Normal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision a
      double precision mu
      integer order
      double precision sigma
      double precision w(0:m-1)

      do order = 0, m - 1
        call truncated_normal_a_moment ( order, mu, sigma, a, 
     &    w(order) )
      end do

      return
      end
      subroutine moments_truncated_normal_b ( m, mu, sigma, b, w )

c*********************************************************************72
c
cc MOMENTS_TRUNCATED_NORMAL_B: moments of the upper truncated Normal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision b
      double precision mu
      integer order
      double precision sigma
      double precision w(0:m-1)

      do order = 0, m - 1
        call truncated_normal_b_moment ( order, mu, sigma, b, 
     &    w(order) )
      end do

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

        q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 
     &    / ( y + a5 + a6 / ( y + a7 ) ) ) )
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
      function r8_choose ( n, k )

c*********************************************************************72
c
cc R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in R8 arithmetic.
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
c    07 June 2008
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
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, double precision R8_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer k
      integer mn
      integer mx
      integer n
      double precision r8_choose
      double precision value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0.0D+00

      else if ( mn .eq. 0 ) then

        value = 1.0D+00

      else

        mx = max ( k, n - k )
        value = dble ( mx + 1 )

        do i = 2, mn
          value = ( value * dble ( mx + i ) ) / dble ( i )
        end do

      end if

      r8_choose = value

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
c     N Value
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
      function r8_mop ( i )

c*********************************************************************72
c
cc R8_MOP returns the I-th power of -1 as an R8.
c
c  Discussion:
c
c    An R8 is a double precision real value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the power of -1.
c
c    Output, double precision R8_MOP, the I-th power of -1.
c
      implicit none

      integer i
      double precision r8_mop

      if ( mod ( i, 2 ) .eq. 0 ) then
        r8_mop = + 1.0D+00
      else
        r8_mop = - 1.0D+00
      end if

      return
      end
      subroutine r8mat_cholesky_factor_upper ( n, a, c, flag )

!*********************************************************************72
!
!! R8MAT_CHOLESKY_FACTOR_UPPER: upper Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    The lower Cholesky factor is a lower triangular matrix L such that
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of rows and columns of
!    the matrix A.
!
!    Input, double precision A(N,N), the N by N matrix.
!
!    Output, double precision C(N,N), the N by N upper triangular
!    Cholesky factor.
!
!    Output, integer FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!    2, the matrix is not nonnegative definite.
!
      implicit none

      integer n

      double precision a(n,n)
      double precision c(n,n)
      integer flag
      integer i
      integer j
      integer k
      double precision sum2
      double precision tol

      flag = 0

      do j = 1, n
        do i = 1, n
          c(i,j) = a(i,j)
        end do
      end do

      do j = 1, n

        do i = 1, j - 1
          c(j,i) = 0.0D+00
       end do

        do i = j, n

          sum2 = c(i,j)
          do k = 1, j - 1
            sum2 = sum2 - c(k,j) * c(k,i)
          end do

          if ( i .eq. j ) then
            if ( sum2 .le. 0.0D+00 ) then
              flag = 1
              return
            else
              c(j,i) = sqrt ( sum2 )
            end if
          else
            if ( c(j,j) .ne. 0.0D+00 ) then
              c(j,i) = sum2 / c(j,j)
            else
              c(j,i) = 0.0D+00
            end if
          end if

        end do

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
      subroutine truncated_normal_ab_moment ( order, mu, s, a, b, 
     &  moment )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_MOMENT: moments of the truncated Normal PDF.
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
c  Reference:
c
c    Phoebus Dhrymes,
c    Moments of Truncated Normal Distributions,
c    May 2005.
c
c  Parameters:
c
c    Input, integer ORDER, the order of the moment.
c    0 <= ORDER.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c    0.0 < S.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision MOMENT, the moment of the PDF.
c
      implicit none

      double precision a
      double precision a_h
      double precision a_cdf
      double precision a_pdf
      double precision b
      double precision b_h
      double precision b_cdf
      double precision b_pdf
      double precision ir
      double precision irm1
      double precision irm2
      double precision moment
      double precision mu
      integer order
      integer r
      double precision r8_choose
      double precision s

      if ( order .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  ORDER < 0.'
        stop 1
      end if

      if ( s .le. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  S <= 0.0.'
        stop 1
      end if

      if ( b .le. a ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  B <= A.'
        stop 1
      end if

      a_h = ( a - mu ) / s
      call normal_01_pdf ( a_h, a_pdf )
      call normal_01_cdf ( a_h, a_cdf )

      if ( a_cdf .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) 
     &    '  PDF/CDF ratio fails, because A_CDF is too small.'
        write ( *, '(a,g14.6)' ) '  A_PDF = %g\n', a_pdf
        write ( *, '(a,g14.6)' ) '  A_CDF = %g\n', a_cdf
        stop 1
      end if

      b_h = ( b - mu ) / s
      call normal_01_pdf ( b_h, b_pdf )
      call normal_01_cdf ( b_h, b_cdf )

      if ( b_cdf .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) 
     &    '  PDF/CDF ratio fails, because B_CDF is too small.'
        write ( *, '(a,g14.6)' ) '  B_PDF = %g\n', b_pdf
        write ( *, '(a,g14.6)' ) '  B_CDF = %g\n', b_cdf
        stop 1
      end if

      moment = 0.0D+00
      irm2 = 0.0D+00
      irm1 = 0.0D+00

      do r = 0, order

        if ( r .eq. 0 ) then
          ir = 1.0D+00
        else if ( r .eq. 1 ) then
          ir = - ( b_pdf - a_pdf ) / ( b_cdf - a_cdf )
        else
          ir = dble ( r - 1 ) * irm2 
     &      - ( b_h ** ( r - 1 ) * b_pdf - a_h ** ( r - 1 ) * a_pdf ) 
     &      / ( b_cdf - a_cdf )
        end if

        moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) 
     &    * ( s ** r ) * ir

        irm2 = irm1
        irm1 = ir

      end do

      return
      end
      subroutine truncated_normal_a_moment ( order, mu, s, a, moment )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_MOMENT: moments of the lower truncated Normal PDF.
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
c  Reference:
c
c    Phoebus Dhrymes,
c    Moments of Truncated Normal Distributions,
c    May 2005.
c
c  Parameters:
c
c    Input, integer ORDER, the order of the moment.
c    0 <= ORDER.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c    0.0 < S.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision MOMENT, the moment of the PDF.
c
      implicit none

      double precision a
      double precision moment
      double precision mu
      integer order
      double precision r8_mop
      double precision s

      call truncated_normal_b_moment ( order, - mu, s, - a, moment )

      moment = r8_mop ( order ) * moment

      return
      end
      subroutine truncated_normal_b_moment ( order, mu, s, b, moment )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_MOMENT: moments of the upper truncated Normal PDF.
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
c  Reference:
c
c    Phoebus Dhrymes,
c    Moments of Truncated Normal Distributions,
c    May 2005.
c
c  Parameters:
c
c    Input, integer ORDER, the order of the moment.
c    0 <= ORDER.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c    0.0 < S.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision MOMENT, the moment of the PDF.
c
      implicit none

      double precision b
      double precision f
      double precision h
      double precision h_cdf
      double precision h_pdf
      double precision ir
      double precision irm1
      double precision irm2
      double precision moment
      double precision mu
      integer order
      integer r
      double precision r8_choose
      double precision s

      if ( order .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  ORDER < 0.'
        stop 1
      end if

      h = ( b - mu ) / s
      call normal_01_pdf ( h, h_pdf )
      call normal_01_cdf ( h, h_cdf )

      if ( h_cdf .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  CDF((B-MU)/S) = 0.'
        stop 1
      end if

      f = h_pdf / h_cdf

      moment = 0.0D+00
      irm2 = 0.0D+00
      irm1 = 0.0D+00

      do r = 0, order

        if ( r .eq. 0 ) then
          ir = 1.0D+00
        else if ( r .eq. 1 ) then
          ir = - f
        else
          ir = - h ** ( r - 1 ) * f + dble ( r - 1 ) * irm2
        end if

        moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) 
     &    * ( s ** r ) * ir

        irm2 = irm1
        irm1 = ir

      end do

      return
      end
