      subroutine bivand1 ( n, alpha, beta, a )

c*********************************************************************72
c
cc BIVAND1 returns a bidimensional Vandermonde1 matrix.
c
c  Discussion:
c
c    N = 3, ALPHA = ( 1, 2, 3 ), BETA = ( 10, 20, 30 )
c
c    (x,y)   | (1,10)  (2,10)  (3,10)  (1,20)  (2,20)  (1,30)
c    --------+-----------------------------------------------
c    1       |     1       1       1       1       1       1  
c    x       |     1       2       3       1       2       1
c       y    |    10      10      10      20      20      30
c    x^2     |     1       4       9       1       4       1
c    x  y    |    10      20      30      20      40      30
c    x^2y^2  |   100     100     100     400     400     900
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the data vectors.
c
c    Input, double precision ALPHA(N), BETA(N), the values that define A.
c
c    Output, double precision A(((N+1)*N)/2,((N+1)*N)/2), the matrix.
c
      implicit none

      integer n

      double precision a(((n+1)*n)/2,((n+1)*n)/2)
      double precision alpha(n)
      double precision beta(n)
      integer e
      integer e1
      integer e2
      integer i1
      integer i2
      integer ii
      integer j1
      integer j2
      integer jj
      integer n2

      n2 = ( n * ( n + 1 ) ) / 2

      e1 = 0
      e2 = 0
      e = 0

      do ii = 1, n2

        j1 = 1
        j2 = 1

        do jj = 1, n2

          if ( ii .eq. 1 ) then
            a(ii,jj) = 1.0D+00
          else
            a(ii,jj) = alpha(j1) ** e1 * beta(j2) ** e2
          end if

          if ( j1 + j2 .lt. n + 1 ) then
            j1 = j1 + 1
          else
            j1 = 1
            j2 = j2 + 1
          end if

        end do

        if ( e2 .lt. e ) then
          e1 = e1 - 1
          e2 = e2 + 1
        else
          e = e + 1
          e1 = e
          e2 = 0
        end if

      end do

      return
      end
      subroutine bivand2 ( n, alpha, beta, a )

c*********************************************************************72
c
cc BIVAND2 returns a bidimensional Vandermonde1 matrix.
c
c  Discussion:
c
c    N = 3, ALPHA = ( 1, 2, 3 ), BETA = ( 10, 20, 30 )
c
c    (x,y)   | (1,10) (2,10) (3,10) (1,20) (2,20) (3,20) (1,30) (2,30) (3,30)
c    --------+---------------------------------------------------------------
c    1       |     1      1      1      1      1      1      1      1      1  
c    x       |     1      2      3      1      2      1      1      2      3
c    x^2     |     1      4      9      1      4      1      1      4      9
c       y    |    10     10     10     20     20     20     30     30     30
c    x  y    |    10     20     30     20     40     60     30     60     90
c    x^2y    |    10     40     90     20     80    180     30    120    270
c       y^2  |   100    100    100    400    400    400    900    900    900
c    x  y^2  |   100    200    300    400    800   1200    900   1800   2700
c    x^2y^2  |   100    400    900    400   1600   3600    900   3600   8100
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the data vectors.
c
c    Input, double precision ALPHA(N), BETA(N), the values that define A.
c
c    Output, double precision A(N^2,N^2), the matrix.
c
      implicit none

      integer n

      double precision a(n*n,n*n)
      double precision alpha(n)
      double precision beta(n)
      integer i
      integer ix
      integer iy
      integer j
      integer jx
      integer jy

      i = 0
      do iy = 1, n
        do ix = 1, n
          i = i + 1
          j = 0
          do jy = 1, n
            do jx = 1, n
              j = j + 1
              a(i,j) = alpha(jx) ** ( ix - 1 ) * beta(jy) ** ( iy - 1 )
            end do
          end do
        end do
      end do

      return
      end
      subroutine dvand ( n, alpha, b, x )

c*********************************************************************72
c
cc DVAND solves a Vandermonde system A' * x = b.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Ake Bjorck, Victor Pereyra,
c    Solution of Vandermonde Systems of Equations,
c    Mathematics of Computation,
c    Volume 24, Number 112, October 1970, pages 893-903.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision ALPHA(N), the parameters that define the matrix.
c    The values should be distinct.
c
c    Input, double precision B(N), the right hand side of the linear system.
c
c    Output, double precision X(N), the solution of the linear system.
c
      implicit none

      integer n

      double precision alpha(n)
      double precision b(n)
      integer j
      integer k
      double precision x(n)

      do j = 1, n
        x(j) = b(j)
      end do

      do k = 1, n - 1
        do j = n, k + 1, -1
          x(j) = ( x(j) - x(j-1) ) / ( alpha(j) - alpha(j-k) )
        end do
      end do

      do k = n - 1, 1, -1
        do j = k, n - 1
          x(j) = x(j) - alpha(k) * x(j+1)
        end do
      end do

      return
      end
      subroutine dvandprg ( n, alpha, b, x, c, m )

c*********************************************************************72
c
cc DVANDPRG solves a Vandermonde system A' * x = f progressively.
c
c  Discussion:
c
c    This function receives the solution to the system of equations A' * x = f
c    where A is a Vandermonde matrix for alpha(0) through alpha(n-1),
c    and new values alpha(n) and f(n).  It updates the solution.
c
c    To solve a system of Nbig equations, this function may be called 
c    repeatedly, with N = 1, 2, ..., Nbig.  Each time, a solution to the 
c    current subsystem is returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Ake Bjorck, Victor Pereyra,
c    Solution of Vandermonde Systems of Equations,
c    Mathematics of Computation,
c    Volume 24, Number 112, October 1970, pages 893-903.
c
c  Parameters:
c
c    Input, integer N, the new order of the matrix, which is 1 
c    larger than on the previous call.  For the first call, N must be 1.
c
c    Input, double precision ALPHA(N), the parameters that define the matrix.
c    The values should be distinct.  The value ALPHA(N) has just been
c    added to the system.
c
c    Input, double precision B(N), the right hand side of the linear system.
c
c    Input/output, double precision X(N).  On input, the first N-1 entries 
c    contain the solution of the N-1xN-1 linear system.  On output, the 
c    solution to the NxN linear system.
c
c    Input/output, double precision C(N), M(N).  On input, the first N-1 
c    entries contain factorization data for the N-1xN-1 linear system.  On 
c    output, factorization data for the NxN linear system.
c
      implicit none

      integer n

      double precision alpha(n)
      double precision b(n)
      double precision c(n)
      double precision cn
      integer j
      double precision m(n)
      double precision x(n)

      c(n) = b(n)
      do j = n - 1, 1, -1
        c(j) = ( c(j+1) - c(j) ) / ( alpha(n) - alpha(j) )
      end do

      if ( n .eq. 1 ) then
        m(n) = 1.0D+00
      else
        m(n) = 0.0D+00
      end if

      cn = c(1)
      x(n) = c(1)

      do j = n - 1, 1, -1
        m(j+1) = m(j+1) - alpha(n-1) * m(j)
        x(n-j) = x(n-j) + m(j+1) * cn
      end do

      return
      end
      subroutine pvand ( n, alpha, b, x )

c*********************************************************************72
c
cc PVAND solves a Vandermonde system A * x = b.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Ake Bjorck, Victor Pereyra,
c    Solution of Vandermonde Systems of Equations,
c    Mathematics of Computation,
c    Volume 24, Number 112, October 1970, pages 893-903.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision ALPHA(N), the parameters that define the matrix.
c    The values should be distinct.
c
c    Input, double precision B(N), the right hand side of the linear system.
c
c    Output, double precision X(N), the solution of the linear system.
c
      implicit none

      integer n

      double precision alpha(n)
      double precision b(n)
      integer j
      integer k
      double precision x(n)

      do j = 1, n
        x(j) = b(j)
      end do

      do k = 1, n - 1
        do j = n, k + 1, -1
          x(j) = x(j) - alpha(k) * x(j-1)
        end do
      end do

      do k = n - 1, 1, -1
        do j = k + 1, n
          x(j) = x(j) / ( alpha(j) - alpha(j-k) )
        end do
        do j = k, n - 1
          x(j) = x(j) - x(j+1)
        end do
      end do

      return
      end
      subroutine pvandprg ( n, alpha, b, x, d, u )

c*****************************************************************************80
c
cc PVANDPRG solves a Vandermonde system A * x = f progressively.
c
c  Discussion:
c
c    This function receives the solution to the system of equations A * x = f
c    where A is a Vandermonde matrix for alpha(0) through alpha(n-1),
c    and new values alpha(n) and f(n).  It updates the solution.
c
c    To solve a system of Nbig equations, this function may be called 
c    repeatedly, with N = 1, 2, ..., Nbig.  Each time, a solution to the 
c    current subsystem is returned.
c
c    Note that the reference, which lists an Algol version of this algorithm, 
c    omits a minus sign, writing
c      u[j] := u[j] x delta;
c    where
c      u[j] := - u[j] x delta;
c    is actually necessary.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Ake Bjorck, Victor Pereyra,
c    Solution of Vandermonde Systems of Equations,
c    Mathematics of Computation,
c    Volume 24, Number 112, October 1970, pages 893-903.
c
c  Parameters:
c
c    Input, integer N, the new order of the matrix, which is 1 
c    larger than on the previous call.  For the first call, N must be 1.
c
c    Input, double precision ALPHA(N), the parameters that define the matrix.
c    The values should be distinct.  The value ALPHA(N) has just been
c    added to the system.
c
c    Input, double precision B(N), the right hand side of the linear system.
c
c    Input/output, double precision X(N); on input, the solution of the 
c    N-1xN-1 linear system.  On output, the solution of the NxN linear system.
c
c    Input/output, double precision D(N), U(N); on input, factorization data 
c    for the N-1xN-1 linear system.  On output, factorization data for the
c    NxN linear system.
c
      implicit none

      integer n

      double precision alpha(n)
      double precision b(n)
      double precision d(n)
      double precision delta
      double precision dn
      integer j
      double precision u(n)
      double precision x(n)

      d(n) = b(n)
      do j = n - 1, 1, -1
        d(j) = d(j+1) - alpha(n-j) * d(j)
      end do

      dn = d(1)
      u(n) = 1.0D+00

      do j = 1, n - 1
        delta = alpha(n) - alpha(j)
        u(j) = - u(j) * delta
        u(n) = u(n) * delta
        x(j) = x(j) + dn / u(j)
      end do

      x(n) = dn / u(n)

      return
      end
      subroutine r8mat_mtv ( m, n, a, x, y )

c*****************************************************************************80
c
cc R8MAT_MTV multiplies a transposed matrix times a vector
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of
c    the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(M), the vector to be multiplied by A.
c
c    Output, double precision Y(N), the product A'*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(m)
      double precision y(n)
      double precision y1(n)

      do i = 1, n
        y1(i) = 0.0D+00
        do j = 1, m
          y1(i) = y1(i) + a(j,i) * x(j)
        end do
      end do

      do i = 1, n
        y(i) = y1(i)
      end do

      return
      end
      subroutine r8mat_mv ( m, n, a, x, y )

c*********************************************************************72
c
cc R8MAT_MV multiplies a matrix times a vector.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    In FORTRAN90, this operation can be more efficiently carried
c    out by the command
c
c      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision Y(M), the product A*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(n)
      double precision y(m)
      double precision y1(m)

      do i = 1, m
        y1(i) = 0.0D+00
        do j = 1, n
          y1(i) = y1(i) + a(i,j) * x(j)
        end do
      end do

      do i = 1, m
        y(i) = y1(i)
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
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
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
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
c    17 July 2006
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
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

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
      subroutine vand1 ( n, x, a )

c*********************************************************************72
c
cc VAND1 returns the Vandermonde1 matrix A with 1's on the first row.
c
c  Formula:
c
c    A(I,J) = X(J)^(I-1)
c
c  Example:
c
c    N = 5, X = ( 2, 3, 4, 5, 6 )
c
c    1  1   1   1   1
c    2  3   4   5   6
c    4  9  16  25  36
c    8 27  64 125  216
c   16 81 256 625 1296
c
c  Properties:
c
c    A is generally not symmetric: A' /= A.
c
c    A is nonsingular if, and only if, the X values are distinct.
c
c    det ( A ) = product ( 1 <= I <= N ) ( 1 <= J .lt. I ) ( X(I) - X(J) ).
c             = product ( 1 <= J <= N ) X(J)
c             * product ( 1 <= I .lt. J ) ( X(J) - X(I) ).
c
c    A is generally ill-conditioned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Gregory, David Karney,
c    A Collection of Matrices for Testing Computational Algorithms,
c    Wiley, 1969, page 27,
c    LC: QA263.G68.
c
c    Nicholas Higham,
c    Stability analysis of algorithms for solving confluent
c    Vandermonde-like systems,
c    SIAM Journal on Matrix Analysis and Applications,
c    Volume 11, 1990, pages 23-41.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix desired.
c
c    Input, double precision X(N), the values that define A.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer j
      double precision x(n)

      do i = 1, n

        do j = 1, n

          if ( i .eq. 1 .and. x(j) .eq. 0.0D+00 ) then
            a(i,j) = 1.0D+00
          else
            a(i,j) = x(j) ** ( i - 1 )
          end if

        end do
      end do

      return
      end
