      subroutine cheby_van1 ( m, a, b, n, x, v )

c*********************************************************************72
c
cc CHEBY_VAN1 returns the CHEBY_VAN1 matrix.
c
c  Discussion:
c
c    Normally, the Chebyshev polynomials are defined on -1 <= XI <= +1.
c    Here, we assume the Chebyshev polynomials have been defined on the
c    interval A <= X <= B, using the mapping
c      XI = ( - ( B - X ) + ( X - A ) ) / ( B - A )
c    so that
c      ChebyAB(A,B;X) = Cheby(XI).
c
c    if ( I == 1 ) then
c      V(1,1:N) = 1;
c    elseif ( I == 2 ) then
c      V(2,1:N) = XI(1:N);
c    else
c      V(I,1:N) = 2.0 * XI(1:N) * V(I-1,1:N) - V(I-2,1:N);
c
c  Example:
c
c    M = 5, A = -1, B = +1, N = 5, X = ( 1, 2, 3, 4, 5 )
c
c    1  1   1    1    1
c    1  2   3    4    5
c    1  7  17   31   49
c    1 26  99  244  485
c    1 97 577 1921 4801
c
c  Properties:
c
c    V is generally not symmetric: V' /= V.
c
c    V(I,J) = T(I-1) ( X(J) ) where T(I-1) is a Chebyshev polynomial.
c
c    V will be singular if the X values are not distinct.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Stability analysis of algorithms for solving confluent
c    Vandermonde-like systems,
c    SIAM Journal on Matrix Analysis and Applications,
c    Volume 11, 1990, pages 23-41.
c
c  Parameters:
c
c    Input, integer M, the number of rows of the matrix.
c
c    Input, double precision A, B, the interval.
c
c    Input, integer N, the number of columns of the matrix.
c
c    Input, double precision X(N), the vector that defines the matrix.
c
c    Output, double precision V(M,N), the matrix.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer i
      integer j
      double precision v(m,n)
      double precision x(n)
      double precision xi

      do j = 1, n

        xi = ( - 1.0D+00 * ( b - x(j)     )   
     &         + 1.0D+00 * (     x(j) - a ) ) 
     &       /             ( b          - a )

        do i = 1, m

          if ( i .eq. 1 ) then
            v(i,j) = 1.0D+00
          else if ( i .eq. 2 ) then
            v(i,j) = xi
          else
            v(i,j) = 2.0D+00 * xi * v(i-1,j) - v(i-2,j)
          end if

        end do
      end do

      return
      end
      subroutine legendre_van ( m, a, b, n, x, v )

c*********************************************************************72
c
cc LEGENDRE_VAN returns the LEGENDRE_VAN matrix.
c
c  Discussion:
c
c    The LEGENDRE_VAN matrix is the Legendre Vandermonde-like matrix.
c
c    Normally, the Legendre polynomials are defined on -1 <= XI <= +1.
c    Here, we assume the Legendre polynomials have been defined on the
c    interval A <= X <= B, using the mapping
c      XI = ( - ( B - X ) + ( X - A ) ) / ( B - A )
c    so that
c      Lab(A,B;X) = L(XI).
c
c    if ( I = 1 ) then
c      V(1,1:N) = 1
c    else if ( I = 2 ) then
c      V(2,1:N) = XI(1:N)
c    else
c      V(I,1:N) = ( (2*I-1) * XI(1:N) * V(I-1,1:N) - (I-1)*V(I-2,1:N) ) / I
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows of the matrix.
c
c    Input, double precision A, B, the interval.
c
c    Input, integer N, the number of columns of the matrix.
c
c    Input, double precision X(N), the vector that defines the matrix.
c
c    Output, double precision V(M,N), the matrix.
c
      implicit none

      integer m
      integer n
 
      double precision a
      double precision b
      integer i
      integer j
      double precision v(m,n)
      double precision x(n)
      double precision xi

      do j = 1, n
        xi = ( - ( b - x(j) ) + ( x(j) - a ) ) / ( b - a )
        do i = 1, m

          if ( i .eq. 1 ) then
            v(i,j) = 1.0D+00
          else if ( i .eq. 2 ) then
            v(i,j) = xi
          else
            v(i,j) = ( dble ( 2 * i - 1 ) * xi * v(i-1,j) + 
     &                 dble (   - i + 1 ) *      v(i-2,j) ) 
     &               / dble (     i )
          end if

        end do
      end do

      return
      end
      subroutine line_fekete_chebyshev ( m, a, b, n, x, nf, xf, wf )

c*********************************************************************72
c
cc LINE_FEKETE_CHEBYSHEV: approximate Fekete points in an interval [A,B].
c
c  Discussion:
c
c    We use the Chebyshev basis.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Len Bos, Norm Levenberg,
c    On the calculation of approximate Fekete points: the univariate case,
c    Electronic Transactions on Numerical Analysis, 
c    Volume 30, pages 377-397, 2008.
c    
c  Parameters:
c
c    Input, integer M, the number of basis polynomials.
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the number of sample points.
c    M <= N.
c
c    Input, double precision X(N), the coordinates of the sample points.
c
c    Output, integer NF, the number of Fekete points.
c    If the computation is successful, NF = M.
c
c    Output, double precision XF(NF), the coordinates of the Fekete points.
c
c    Output, double precision WF(NF), the weights of the Fekete points.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer i
      integer j
      double precision mom(m)
      integer nf
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision v(m,n)
      double precision w(n)
      double precision wf(m)
      double precision x(n)
      double precision xf(*)

      if ( n .lt. m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'LINE_FEKETE_CHEBYSHEV - Fatal error!'
        write ( *, '(a)' ) '  N < M.'
        stop 1
      end if
c
c  Compute the Chebyshev-Vandermonde matrix.
c
      call cheby_van1 ( m, a, b, n, x, v )
c
c  MOM(I) = Integral ( A <= x <= B ) Tab(A,B,I;x) dx
c
      mom(1) = r8_pi * ( b - a ) / 2.0D+00
      do i = 2, m
        mom(i) = 0.0D+00
      end do
c
c  Solve the system for the weights W.
c
      call qr_solve ( m, n, v, mom, w )
c
c  Extract the data associated with the nonzero weights.
c
      nf = 0
      do j = 1, n
        if ( w(j) .ne. 0.0D+00 ) then
          if ( nf .lt. m ) then
            nf = nf + 1
            xf(nf) = x(j)
            wf(nf) = w(j)
          end if
        end if
      end do

      return
      end
      subroutine line_fekete_legendre ( m, a, b, n, x, nf, xf, wf )

c*********************************************************************72
c
cc LINE_FEKETE_LEGENDRE computes approximate Fekete points in an interval [A,B].
c
c  Discussion:
c
c    We use the uniform weight and the Legendre basis:
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Len Bos, Norm Levenberg,
c    On the calculation of approximate Fekete points: the univariate case,
c    Electronic Transactions on Numerical Analysis, 
c    Volume 30, pages 377-397, 2008.
c    
c  Parameters:
c
c    Input, integer M, the number of basis polynomials.
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the number of sample points.
c    M <= N.
c
c    Input, double precision X(N), the coordinates of the sample points.
c
c    Output, integer NF, the number of Fekete points.
c    If the computation is successful, NF = M.
c
c    Output, double precision XF(NF), the coordinates of the Fekete points.
c
c    Output, double precision WF(NF), the weights of the Fekete points.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer i
      integer j
      double precision mom(m)
      integer nf
      double precision v(m,n)
      double precision w(n)
      double precision wf(m)
      double precision x(n)
      double precision xf(m)

      if ( n .lt. m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'LINE_FEKETE_LEGENDRE - Fatal error!'
        write ( *, '(a)' ) '  N < M.'
        stop 1
      end if
c
c  Compute the Legendre-Vandermonde matrix.
c
      call legendre_van ( m, a, b, n, x, v )
c
c  MOM(i) = integral ( A <= X <= B ) Lab(A,B,I;X) dx
c
      mom(1) = b - a
      do i = 2, m
        mom(i) = 0.0D+00
      end do
c
c  Solve the system for the weights W.
c
      call qr_solve ( m, n, v, mom, w )
c
c  Extract the data associated with the nonzero weights.
c
      nf = 0
      do j = 1, n
        if ( w(j) .ne. 0.0D+00 ) then
          if ( nf .lt. m ) then
            nf = nf + 1
            xf(nf) = x(j)
            wf(nf) = w(j)
          end if
        end if
      end do

      return
      end
      subroutine line_fekete_monomial ( m, a, b, n, x, nf, xf, wf )

c*********************************************************************72
c
cc LINE_FEKETE_MONOMIAL computes approximate Fekete points in an interval [A,B].
c
c  Discussion:
c
c    We use the uniform weight and the monomial basis:
c
c      P(j) = x^(j-1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Alvise Sommariva, Marco Vianello,
c    Computing approximate Fekete points by QR factorizations of Vandermonde 
c    matrices,
c    Computers and Mathematics with Applications,
c    Volume 57, 2009, pages 1324-1336.
c    
c  Parameters:
c
c    Input, integer M, the number of basis polynomials.
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer N, the number of sample points.
c    M <= N.
c
c    Input, double precision X(N), the coordinates of the sample points.
c
c    Output, integer NF, the number of Fekete points.
c    If the computation is successful, NF = M.
c
c    Output, double precision XF(NF), the coordinates of the Fekete points.
c
c    Output, double precision WF(NF), the weights of the Fekete points.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision ai
      double precision b
      double precision bi
      integer i
      integer j
      double precision mom(m)
      integer nf
      double precision v(m,n)
      double precision w(n)
      double precision wf(m)
      double precision x(n)
      double precision xf(m)

      if ( n .lt. m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'LINE_FEKETE_MONOMIAL - Fatal errorc'
        write ( *, '(a)' ) '  N < M.'
        stop 1
      end if
c
c  Form the moments.
c
      call line_monomial_moments ( a, b, m, mom )
c
c  Form the rectangular Vandermonde matrix V for the polynomial basis.
c
      do j = 1, n
        v(1,j) = 1.0D+00
        do i = 2, m
          v(i,j) = v(i-1,j) * x(j)
        end do
      end do
c
c  Solve the system for the weights W.
c
      call qr_solve ( m, n, v, mom, w )
c
c  Extract the data associated with the nonzero weights.
c
      nf = 0
      do j = 1, n
        if ( w(j) .ne. 0.0D+00 ) then
          if ( nf .lt. m ) then
            nf = nf + 1
            xf(nf) = x(j)
            wf(nf) = w(j)
          end if
        end if
      end do

      return
      end
      subroutine line_monomial_moments ( a, b, m, mom )

c*********************************************************************72
c
cc LINE_MONOMIAL_MOMENTS computes monomial moments in [A,B].
c
c  Discussion:
c
c    We use the uniform weight and the shifted and scaled monomial basis:
c
c      P(a,b,i;x) = xi(a,b;x)^(i-1)
c       xi(a,b;x) = ( - ( b - x ) + ( x - a ) ) / ( b - a )
c
c    The i-th moment is
c
c      mom(i) = integral ( a <= x <= b ) P(a,b,i;x) dx
c             = integral ( a <= x <= b ) xi(a,b;x)^(i-1) dx
c             = 0.5 * ( b - a ) * integral ( -1 <= xi <= +1 ) xi^(i-1) dxi
c             = 0.5 * ( b - a ) * xi^i / i | ( -1 <= xi <= +1 )
c             = 0.5 * ( b - a ) * ( 1 - (-1)^i ) / i
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, integer M, the number of basis polynomials.
c
c    Output, double precision MOM(M), the moments.
c
      implicit none

      integer m

      double precision a
      double precision b
      integer i
      double precision mom(m)

      do i = 1, m
        mom(i) = ( b - a ) * dble ( mod ( i, 2 ) ) / dble ( i )
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
      subroutine r8vec_linspace ( n, a, b, x )

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
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
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
