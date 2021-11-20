      subroutine qwv_2d ( t, n, a, b, c, d, x, y, w )

c*********************************************************************72
c
cc QWV_2D computes 2D quadrature weights using the Vandermonde matrix.
c
c  Discussion:
c
c    We assume that the quadrature formula approximates integrals of the form:
c
c      I(F) = Integral ( C <= Y <= D ) Integral ( A <= X <= B ) F(X,Y) dX dY
c
c    by specifying N points (X,Y) and weights W such that
c
c      Q(F) = Sum ( 1 <= I <= N ) W(I) * F(X(I),Y(I))
c
c    Now let us assume that the points (X,Y) have been specified, but that the
c    corresponding values W remain to be determined.
c
c    If we require that the quadrature rule with N points integrates the first
c    N monomials exactly, then we have N conditions on the weights W.
c
c    The K-th condition, for the monomial X^I*Y^J, J = 0 to T, I = 0 to T - J,
c    has the form:
c
c      W(1)*X(1)^I*Y(1)^J + W(2)*X(2)^I*Y(2)^j+...+W(N)*X(N)^I*Y(N)^J = 
c      = (B^(I+1)-A^(I+1))*(D^(J+1)-C(J+1))/(I+1)/(J+1)
c
c    The corresponding matrix is known as the 2D Vandermonde matrix.  It is
c    theoretically guaranteed to be nonsingular as long as the (X,Y) are
c    distinct, but its condition number grows quickly.  Therefore,
c    this simple, direct approach is often abandoned when more accuracy
c    or high order rules are needed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer T, the desired total degree.
c    0 <= T.
c
c    Input, integer N, the number of points in the rule.
c    It should be the case that T = (N+1)*(N+2)/2.
c
c    Input, double precision A, B, C, D the endpoints of the X and Y intervals.
c
c    Input, double precision X(N), Y(N), the quadrature points.
c
c    Output, double precision W(N), the quadrature weights.
c
      implicit none

      integer n

      double precision a
      double precision b
      double precision c
      double precision d
      integer i
      integer ierror
      integer j
      integer k
      integer l
      double precision rhs(n)
      integer t
      double precision v(n,n)
      double precision w(n)
      double precision x(n)
      double precision y(n)
c
c  Define the Vandermonde matrix for X.
c
      k = 0
      do j = 0, t
        do i = 0, t - j
          k = k + 1
          do l = 1, n
            v(k,l) = x(l) ** i * y(l) ** j
          end do
          rhs(k) = ( b ** ( i + 1 ) - a ** ( i + 1 ) ) / dble ( i + 1 ) 
     &           * ( d ** ( j + 1 ) - c ** ( j + 1 ) ) / dble ( j + 1 )
        end do
      end do

      if ( .false. ) then
        call r8mat_print ( n, n, v, '  Matrix:' )
      end if

      if ( .false. ) then
        call r8vec_print ( n, rhs, '  Right hand side:' )
      end if
c
c  Solve V * W = RHS to get the weights.
c
      call r8mat_solve2 ( n, v, rhs, w, ierror )

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
      subroutine r8mat_solve2 ( n, a, b, x, ierror )

c*********************************************************************72
c
cc R8MAT_SOLVE2 computes the solution of an N by N linear system.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    The linear system may be represented as
c
c      A*X = B
c
c    If the linear system is singular, but consistent, then the routine will
c    still produce a solution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of equations.
c
c    Input/output, double precision A(N,N).
c    On input, A is the coefficient matrix to be inverted.
c    On output, A has been overwritten.
c
c    Input/output, double precision B(N).
c    On input, B is the right hand side of the system.
c    On output, B has been overwritten.
c
c    Output, double precision X(N), the solution of the linear system.
c
c    Output, integer IERROR.
c    0, no error detected.
c    1, consistent singularity.
c    2, inconsistent singularity.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision amax
      double precision b(n)
      integer i
      integer ierror
      integer imax
      integer ipiv(n)
      integer j
      integer k
      double precision x(n)

      ierror = 0

      do i = 1, n
        ipiv(i) = 0
      end do

      do i = 1, n
        x(i) = 0.0D+00
      end do
c
c  Process the matrix.
c
      do k = 1, n
c
c  In column K:
c    Seek the row IMAX with the properties that:
c      IMAX has not already been used as a pivot;
c      A(IMAX,K) is larger in magnitude than any other candidate.
c
        amax = 0.0D+00
        imax = 0
        do i = 1, n
          if ( ipiv(i) .eq. 0 ) then
            if ( amax .lt. abs ( a(i,k) ) ) then
              imax = i
              amax = abs ( a(i,k) )
            end if
          end if
        end do
c
c  If you found a pivot row IMAX, then,
c    eliminate the K-th entry in all rows that have not been used for pivoting.
c
        if ( imax .ne. 0 ) then

          ipiv(imax) = k
          do j = k + 1, n
            a(imax,j) = a(imax,j) / a(imax,k)
          end do
          b(imax) = b(imax) / a(imax,k)
          a(imax,k) = 1.0D+00

          do i = 1, n

            if ( ipiv(i) .eq. 0 ) then
              do j = k + 1, n
                a(i,j) = a(i,j) - a(i,k) * a(imax,j)
              end do
              b(i) = b(i) - a(i,k) * b(imax)
              a(i,k) = 0.0D+00
            end if

          end do

        end if

      end do
c
c  Now, every row with nonzero IPIV begins with a 1, and
c  all other rows are all zero.  Begin solution.
c
      do j = n, 1, -1

        imax = 0
        do k = 1, n
          if ( ipiv(k) .eq. j ) then
            imax = k
          end if
        end do

        if ( imax .eq. 0 ) then

          x(j) = 0.0D+00

          if ( b(j) .eq. 0.0D+00 ) then
            ierror = 1
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'R8MAT_SOLVE2 - Warning:'
            write ( *, '(a,i8)' )
     &        '  Consistent singularity, equation = ', j
          else
            ierror = 2
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'R8MAT_SOLVE2 - Error:'
            write ( *, '(a,i8)' )
     &        '  Inconsistent singularity, equation = ', j
          end if

        else

          x(j) = b(imax)

          do i = 1, n
            if ( i .ne. imax ) then
              b(i) = b(i) - a(i,j) * x(j)
            end if
          end do

        end if

      end do

      return
      end
      subroutine r8vec_even ( n, alo, ahi, a )

c*********************************************************************72
c
cc R8VEC_EVEN returns an R8VEC of evenly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    If N is 1, then the midpoint is returned.
c
c    Otherwise, the two endpoints are returned, and N-2 evenly
c    spaced points between them.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values.
c
c    Input, double precision ALO, AHI, the low and high values.
c
c    Output, double precision A(N), N evenly spaced values.
c    Normally, A(1) = ALO and A(N) = AHI.
c    However, if N = 1, then A(1) = 0.5*(ALO+AHI).
c
      implicit none

      integer n

      double precision a(n)
      double precision ahi
      double precision alo
      integer i

      if ( n .eq. 1 ) then

        a(1) = 0.5D+00 * ( alo + ahi )

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * alo
     &           + dble (     i - 1 ) * ahi )
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
      subroutine r8vec_print_16 ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT_16 prints an R8VEC to 16 decimal places.
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
c    29 May 2014
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
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g24.16)' ) i, ':', a(i)
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
