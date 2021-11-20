      subroutine i4_to_pascal ( k, i, j )

c*********************************************************************72
c
cc I4_TO_PASCAL converts a linear index to Pascal triangle coordinates.
c
c  Discussion:
c
c    We describe the grid points in Pascal's triangle in two ways:
c
c    As a linear index K:
c
c                     1
c                   2   3
c                 4   5   6
c               7   8   9   10
c
c    As elements (I,J) of Pascal's triangle:
c
c                     0,0
c                  1,0   0,1
c               2,0   1,1    0,2
c            3,0   2,1   1,2    0,3
c
c  Example:
c
c     K  I  J
c
c     1  0  0
c     2  1  0
c     3  0  1
c     4  2  0
c     5  1  1
c     6  0  2
c     7  3  0
c     8  2  1
c     9  1  2
c    10  0  3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer K, the linear index of the (I,J) element.
c    1 <= K.
c
c    Output, integer I, J, the Pascal indices.
c
      implicit none

      integer d
      integer i
      integer j
      integer k

      if ( k .le. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'I4_TO_PASCAL - Fatal error!'
        write ( *, '(a)' ) '  K must be positive.'
        stop 1
      end if

      call i4_to_pascal_degree ( k, d )

      j = k - ( d * ( d + 1 ) ) / 2 - 1
      i = d - j

      return
      end
      subroutine i4_to_pascal_degree ( k, d )

c*********************************************************************72
c
cc I4_TO_PASCAL_DEGREE converts a linear index to a Pascal triangle degree.
c
c  Discussion:
c
c    We describe the grid points in Pascal's triangle in two ways:
c
c    As a linear index K:
c
c                     1
c                   2   3
c                 4   5   6
c               7   8   9   10
c
c    As elements (I,J) of Pascal's triangle:
c
c                     0,0
c                  1,0   0,1
c               2,0   1,1    0,2
c            3,0   2,1   1,2    0,3
c
c    The quantity D represents the "degree" of the corresponding monomial,
c    that is, D = I + J.
c
c    We can compute D directly from K using the quadratic formula.
c
c  Example:
c
c     K  I  J  D
c
c     1  0  0  0
c
c     2  1  0  1
c     3  0  1  1
c
c     4  2  0  2
c     5  1  1  2
c     6  0  2  2
c
c     7  3  0  3
c     8  2  1  3
c     9  1  2  3
c    10  0  3  3
c
c    11  4  0  4
c    12  3  1  4
c    13  2  2  4
c    14  1  3  4
c    15  0  4  4
c
c    16  5  0  5
c    17  4  1  5
c    18  3  2  5
c    19  2  3  5
c    20  1  4  5
c    21  0  5  5
c
c    22  6  0  6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer K, the linear index of the (I,J) element.
c    1 <= K.
c
c    Output, integer D, the degree (sum) of the corresponding Pascal indices.
c
      implicit none

      double precision arg
      integer d
      integer k

      if ( k .le. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'I4_TO_PASCAL_DEGREE - Fatal error!'
        write ( *, '(a)' ) '  K must be positive.'
        stop 1
      end if

      arg = dble ( 1 + 8 * ( k - 1 ) )

      d = int ( 0.5D+00 * ( -1.0D+00 + sqrt ( arg ) ) )

      return
      end
      subroutine pascal_to_i4 ( i, j, k )

c*********************************************************************72
c
cc PASCAL_TO_I4 converts Pacal triangle coordinates to a linear index.
c
c  Discussion:
c
c    We describe the grid points in a Pascal triangle in two ways:
c
c    As a linear index K:
c
c                     1
c                   2   3
c                 4   5   6
c               7   8   9   10
c
c    As elements (I,J) of Pascal's triangle:
c
c                     0,0
c                  1,0   0,1
c               2,0   1,1    0,2
c            3,0   2,1   1,2    0,3
c
c  Example:
c
c     K  I  J
c
c     1  0  0
c     2  1  0
c     3  0  1
c     4  2  0
c     5  1  1
c     6  0  2
c     7  3  0
c     8  2  1
c     9  1  2
c    10  0  3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the row and column indices.  I and J 
c    must be nonnegative.
c
c    Output, integer K, the linear index of the (I,J) element.
c
      implicit none

      integer d
      integer i
      integer j
      integer k

      if ( i .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PASCAL_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  I < 0.'
        write ( *, '(a,i8)' ) '  I = ', i
        stop 1
      else if ( j .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PASCAL_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  J < 0.'
        write ( *, '(a,i8)' ) '  J = ', j
        stop 1
      end if

      d = i + j

      k = ( d * ( d + 1 ) ) / 2 + j + 1

      return
      end
      subroutine poly_power ( d1, p1, n, d2, p2 )

c*********************************************************************72
c
cc POLY_POWER computes a power of a polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer D1, the degree of the polynomial.
c
c    Input, double precision P1(M1), the polynomial coefficients.
c    M1 = ((D1+1)*(D1+2))/2.
c
c    Input, integer N, the nonnegative integer power.
c
c    Input, integer D2, the degree of the power polynomial.
c    D2 = N * D1.
c
c    Output, double precision P2(M2), the polynomial power.
c    M2 = ((D2+1)*(D2+2))/2.
c
      implicit none

      integer d1
      integer d2
      integer n

      integer d3
      integer d4
      integer i
      integer k
      integer m2
      integer m3
      integer m4
      double precision p1(( (     d1 + 1 ) * (     d1 + 2 ) ) / 2)
      double precision p2(( (     d2 + 1 ) * (     d2 + 2 ) ) / 2)
      double precision p3(( ( n * d1 + 1 ) * ( n * d1 + 2 ) ) / 2)
      double precision p4(( ( n * d1 + 1 ) * ( n * d1 + 2 ) ) / 2)

      m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
c
c  Create P3, a polynomial representation of 1, that is
c  big enough to store the final result.
c
      d3 = n * d1
      m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
      p3(1) = 1.0D+00
      do k = 2, m3
        p3(k) = 0.0D+00
      end do
c
c  Create P4, big enough to hold the result.
c
      d4 = n * d1
      m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2
      do k = 1, m4
        p4(k) = 0.0D+00
      end do
c
c  Now set D3 to 0, to indicate that P3 currently contains only
c  a constant term.
c
      d3 = 0
c
c  Iterate N times:
c    P <= P1 * P
c
      do i = 1, n
        d4 = d1 + d3
        call poly_product ( d1, p1, d3, p3, d4, p4 )
        d3 = d4
        do k = 1, m3
          p3(k) = p4(k)
        end do
      end do
c
c  Copy the result to the user.
c
      do k = 1, m2
        p2(k) = p3(k)
      end do

      return
      end
      subroutine poly_power_linear ( d1, p1, n, d2, p2 )

c*********************************************************************72
c
cc POLY_POWER_LINEAR computes the polynomial ( a + b*x + c*y ) ^ n.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer D1, the degree of the linear polynomial,
c    which should be 1 (or possibly 0).
c
c    Input, double precision P1(M1), the coefficients of the linear polynomial.
c    M1 = ( (D1+1)*(D1+2) ) / 2, which should be 3.
c
c    Input, integer N, the power to which the polynomial is to be 
c    raised.  0 <= N.
c
c    Input, integer D2, the degree of the power polyynomial.
c    D2 = N * D1 = N.
c
c    Output, double precision P2(M2), the coefficients of the power polynomial.
c    M2 = ( (D2+1)*(D2+2) ) / 2, which should be ((N+1)*(N+2))/2.
c
      implicit none

      integer d1
      integer d2
      integer n

      integer i
      integer j
      integer k
      integer l
      double precision p1(( ( d1 + 1 ) * ( d1 + 2 ) ) / 2 )
      double precision p2(( ( d2 + 1 ) * ( d2 + 2 ) ) / 2 )
      integer trinomial

      if ( d1 .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'POLY_POWER_LINEAR - Fatal error!'
        write ( *, '(a)' ) '  D1 < 0.'
        stop 1
      end if

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'POLY_POWER_LINEAR - Fatal error!'
        write ( *, '(a)' ) '  N < 0.'
        stop 1
      end if

      if ( d1 .eq. 0 ) then
        p2(1) = p1(1) ** n
        return
      end if

      if ( n .eq. 0 ) then
        p2(1) = 1.0D+00
        return
      end if
c
c  Use the Trinomial formula.
c
      do i = 0, n
        do j = 0, n - i
          do k = 0, n - i - j
c
c  We store X^J Y^K in location L.
c
            call pascal_to_i4 ( j, k, l )
            p2(l) = dble ( trinomial ( i, j, k ) ) 
     &        * p1(1) ** i * p1(2) ** j * p1(3) ** k
          end do
        end do
      end do

      return
      end
      subroutine poly_print ( d, p, title )

c*********************************************************************72
c
cc POLY_PRINT prints an XY polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer D, the degree of the polynomial.
c
c    Output, double precision P(M), the coefficients of all monomials of 
c    degree 0 through D.  P must contain ((D+1)*(D+2))/2 entries.
c
c    Input, character * ( * ) TITLE, a title string.
c
      implicit none

      integer d

      logical all_zero
      integer i
      integer j
      integer k
      integer m
      double precision p(( ( d + 1 ) * ( d + 2 ) ) / 2)
      character * ( * ) title

      m = ( ( d + 1 ) * ( d + 2 ) ) / 2

      all_zero = .true.
      do k = 1, m
        if ( p(k) .ne. 0.0D+00 ) then
          all_zero = .false.
        end if
      end do

      if ( all_zero ) then

        write ( *, '(a)' ) trim ( title ) // ' = 0'

      else

        write ( *, '(a)' ) trim ( title ) // ' = '

        do k = 1, m

          call i4_to_pascal ( k, i, j )

          if ( p(k) .ne. 0.0D+00 ) then

            if ( p(k) .lt. 0.0D+00 ) then
              write ( *, '(2x,a,g14.6)', advance = 'no' ) 
     &          '-', abs ( p(k) )
            else
              write ( *, '(2x,a,g14.6)', advance = 'no' ) '+', p(k)
            end if

            if ( i + j .ne. 0 ) then
              write ( *, '(a)', advance = 'no' ) ' '
            end if

            if ( i .eq. 0 ) then
            else if ( i .eq. 1 ) then
              write ( *, '(a)', advance = 'no' ) 'x'
            else
              write ( *, '(a,i2)', advance = 'no' ) 'x^', i
            end if

            if ( j .eq. 0 ) then
            else if ( j .eq. 1 ) then
              write ( *, '(a)', advance = 'no' ) 'y'
            else
              write ( *, '(a,i2)', advance = 'no' ) 'y^', j
            end if

            write ( *, '(a)' )

          end if

        end do

      end if

      return
      end
      subroutine poly_product ( d1, p1, d2, p2, d3, p3 )

c*********************************************************************72
c
cc POLY_PRODUCT computes P3(x,y) = P1(x,y) * P2(x,y) for polynomials.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer D1, the degree of factor 1.
c
c    Input, double precision P1(M1), the factor 1 coefficients.
c    M1 = ((D1+1)*(D1+2))/2.
c
c    Input, integer D2, the degree of factor 2.
c
c    Input, double precision P2(M2), the factor2 coefficients.
c    M2 = ((D2+1)*(D2+2))/2.
c
c    Input, integer D3, the degree of the result.
c    D3 = D1 + D2.
c
c    Output, double precision P3(M3), the result coefficients.
c    M3 = ((D3+1)*(D3+2))/2.
c
      implicit none

      integer d1
      integer d2
      integer d3

      integer i1
      integer i2
      integer i3
      integer j1
      integer j2
      integer j3
      integer k1
      integer k2
      integer k3
      integer m1
      integer m2
      integer m3
      double precision p1(( ( d1 + 1 ) * ( d1 + 2 ) ) / 2)
      double precision p2(( ( d2 + 1 ) * ( d2 + 2 ) ) / 2)
      double precision p3(( ( d3 + 1 ) * ( d3 + 2 ) ) / 2)

      m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2
      m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2
      m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
c
c  Consider each entry in P1:
c    P1(K1) * X^I1 * Y^J1
c  and multiply it by each entry in P2:
c    P2(K2) * X^I2 * Y^J2
c  getting 
c    P3(K3) = P3(K3) + P1(K1) * P2(X2) * X^(I1+I2) * Y(J1+J2)
c
      do k3 = 1, m3
        p3(k3) = 0.0D+00
      end do

      do k1 = 1, m1
        call i4_to_pascal ( k1, i1, j1 )
        do k2 = 1, m2
          call i4_to_pascal ( k2, i2, j2 )
          i3 = i1 + i2
          j3 = j1 + j2
          call pascal_to_i4 ( i3, j3, k3 )
          p3(k3) = p3(k3) + p1(k1) * p2(k2)
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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( * ) title

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
c    Input, character * ( * ) TITLE, a title.
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
      subroutine rs_to_xy_map ( t, a, b, c, d, e, f )

c*********************************************************************72
c
cc RS_TO_XY_MAP returns the linear map from reference to physical triangle.
c
c  Discussion:
c
c    This function returns the coefficients of the linear map that sends
c    the vertices of the reference triangle, (0,0), (1,0) and (0,1), to
c    the vertices of a physical triangle T, of the form:
c
c      X = A + B * R + C * S;
c      Y = D + E * R + F * S.
c
c  Reference Element:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the coordinates of the vertices.  The
c    vertices are assumed to be the images of (0,0), (1,0) and (0,1) 
c    respectively.
c
c    Output, double precision A, B, C, D, E, F, the mapping coefficients.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision d
      double precision e
      double precision f
      double precision t(2,3)

      a = t(1,1)
      b = t(1,2) - t(1,1)
      c = t(1,3) - t(1,1)

      d = t(2,1)
      e = t(2,2) - t(2,1)
      f = t(2,3) - t(2,1)

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
      function triangle01_monomial_integral ( i, j )

c*********************************************************************72
c
cc TRIANGLE01_MONOMIAL_INTEGRAL: monomial integrals in the unit triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the exponents.  
c    Each exponent must be nonnegative.
c
c    Output, double precision TRIANGLE01_MONOMIAL_INTEGRAL, the integral.
c
      implicit none

      integer i
      integer j
      integer k
      integer l
      double precision q
      double precision triangle01_monomial_integral

      k = 0
      q = 1.0D+00

      do l = 1, i
        k = k + 1
        q = q * dble ( l ) / dble ( k )
      end do

      do l = 1, j
        k = k + 1
        q = q * dble ( l ) / dble ( k )
      end do

      do l = 1, 2
        k = k + 1
        q = q / dble ( k )
      end do

      triangle01_monomial_integral = q

      return
      end
      function triangle01_poly_integral ( d, p )

c*********************************************************************72
c
cc TRIANGLE01_POLY_INTEGRAL: polynomial integral over the unit triangle.
c
c  Discussion:
c
c    The unit triangle is T = ( (0,0), (1,0), (0,1) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer D, the degree of the polynomial.
c
c    Input, double precision P(M), the polynomial coefficients.
c    M = ((D+1)*(D+2))/2.
c
c    Output, double precision TRIANGLE01_POLY_INTEGRAL, the integral.
c
      implicit none

      integer d

      integer i
      integer j
      integer k
      integer m
      double precision p(( ( d + 1 ) * ( d + 2 ) ) / 2)
      double precision q
      double precision triangle01_poly_integral
      double precision triangle01_monomial_integral

      m = ( ( d + 1 ) * ( d + 2 ) ) / 2

      q = 0.0D+00
      do k = 1, m
        call i4_to_pascal ( k, i, j )
        q = q + p(k) * triangle01_monomial_integral ( i, j )
      end do

      return
      end
      function triangle_area ( t )

c*********************************************************************72
c
cc TRIANGLE_AREA returns the area of a triangle.
c
c  Discussion:
c
c    If the vertices are given in counter clockwise order, the area
c    will be positive.
c
c  Licensing:
c
c    This code is distributed under the GNU GPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision T(2,3), the vertices of the triangle.
c
c    Output, double precision TRIANGLE_AREA, the area of the triangle.
c
      implicit none

      double precision t(2,3)
      double precision triangle_area

      triangle_area = 0.5D+00 * 
     &  ( 
     &      ( t(1,2) - t(1,1) ) * ( t(2,3) - t(2,1) ) 
     &    - ( t(1,3) - t(1,1) ) * ( t(2,2) - t(2,1) ) 
     &  )

      return
      end
      function triangle_monomial_integral ( i, j, t )

c*********************************************************************72
c
cc TRIANGLE_MONOMIAL_INTEGRAL integrates a monomial over an arbitrary triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the exponents of X and Y in the monomial.
c    0 <= I, J.
c
c    Input, double precision T(2,3), the vertices of the triangle.
c
c    Output, double precision TRIANGLE_MONOMIAL_INTEGRAL, the integral 
c    of X^I * Y^J over triangle T.
c
      implicit none

      integer d1
      integer d2
      integer d3
      integer d4
      integer d5
      integer i
      integer j
      integer m1
      integer m2
      integer m3
      integer m4
      integer m5
      double precision t(2,3)
      double precision triangle_monomial_integral
      double precision triangle_monomial_integral_sub
c
c  Because of the limitations of FORTRAN77, we need to allocate memory
c  in a roundabout way.
c
      d1 = 1
      m1 = ( ( d1 + 1 ) * ( d1 + 2 ) ) / 2

      d2 = 1
      m2 = ( ( d2 + 1 ) * ( d2 + 2 ) ) / 2

      d3 = i * d1
      m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2

      d4 = j * d2
      m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2

      d5 = d3 + d4
      m5 = ( ( d5 + 1 ) * ( d5 + 2 ) ) / 2

      triangle_monomial_integral = triangle_monomial_integral_sub (
     &  i, j, t, d1, m1, d2, m2, d3, m3, d4, m4, d5, m5 )

      return
      end
      function triangle_monomial_integral_sub ( i, j, t, d1, m1, 
     &  d2, m2, d3, m3, d4, m4, d5, m5 )

c*********************************************************************72
c
cc TRIANGLE_MONOMIAL_INTEGRAL_SUB works for TRIANGLE_MONOMIAL_INTEGRAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the exponents of X and Y in the monomial.
c    0 <= I, J.
c
c    Input, double precision T(2,3), the vertices of the triangle.
c
c    Input, integer D1, M1, the degree and size of polynomial P1.
c
c    Input, integer D2, M2, the degree and size of polynomial P2.
c
c    Input, integer D3, M3, the degree and size of polynomial P3.
c
c    Input, integer D4, M4, the degree and size of polynomial P4.
c
c    Input, integer D5, M5, the degree and size of polynomial P5.
c
c    Output, double precision TRIANGLE_MONOMIAL_INTEGRAL_SUB, the integral 
c    of X^I * Y^J over triangle T.
c
      implicit none

      integer m1
      integer m2
      integer m3
      integer m4
      integer m5

      double precision a
      double precision b
      double precision c
      double precision d
      integer d1
      integer d2
      integer d3
      integer d4
      integer d5
      double precision e
      double precision f
      integer i
      integer j
      double precision p1(m1)
      double precision p2(m2)
      double precision p3(m3)
      double precision p4(m4)
      double precision p5(m5)
      double precision q
      double precision t(2,3)
      double precision triangle_area
      double precision triangle_monomial_integral_sub
      double precision triangle01_poly_integral
c
c  Get map coefficients from reference RS triangle to general XY triangle.
c    R = a+b*X+c*Y
c    S = d+e*X+f*Y
c
      call rs_to_xy_map ( t, a, b, c, d, e, f )
c
c  Set
c    P1(R,S) = a+b*R+c*S
c    P2(R,S) = d+e*R+f*S
c
      p1(1) = a
      p1(2) = b
      p1(3) = c

      p2(1) = d
      p2(2) = e
      p2(3) = f
c
c  Exponentiate:
c    P3(R,S) = P1(R,S)^i
c    P4(R,S) = P2(R,S)^j
c
      d3 = i * d1
      m3 = ( ( d3 + 1 ) * ( d3 + 2 ) ) / 2
      call poly_power_linear ( d1, p1, i, d3, p3 )

      d4 = j * d2
      m4 = ( ( d4 + 1 ) * ( d4 + 2 ) ) / 2
      call poly_power_linear ( d2, p2, j, d4, p4 )
c
c  Compute the product 
c    P5(R,S) = P3(R,S) * P4(R,S)
c
      d5 = d3 + d4
      m5 = ( ( d5 + 1 ) * ( d5 + 2 ) ) / 2

      call poly_product ( d3, p3, d4, p4, d5, p5 )
c
c  Compute the integral of P5(R,S) over the reference triangle.
c
      q = triangle01_poly_integral ( d5, p5 )
c
c  Multiply by the area of the physical triangle T(X,Y) divided by
c  the area of the reference triangle.
c
      q = q * triangle_area ( t ) / 0.5D+00

      triangle_monomial_integral_sub = q

      return
      end
      function triangle_poly_integral ( d, p, t )

c*********************************************************************72
c
cc TRIANGLE_POLY_INTEGRAL: polynomial integral over a triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer D, the degree of the polynomial.
c
c    Input, double precision P(M), the polynomial coefficients.
c    M = ((D+1)*(D+2))/2.
c
c    Input, double precision T(2,3), the vertices of the triangle.
c
c    Output, double precision TRIANGLE_POLY_INTEGRAL, the integral.
c
      implicit none

      integer d

      integer i
      integer j
      integer k
      integer m
      double precision p(( ( d + 1 ) * ( d + 2 ) ) / 2)
      double precision q
      double precision t(2,3)
      double precision triangle_monomial_integral
      double precision triangle_poly_integral

      m = ( ( d + 1 ) * ( d + 2 ) ) / 2

      q = 0.0D+00
      do k = 1, m
        call i4_to_pascal ( k, i, j )
        q = q + p(k) * triangle_monomial_integral ( i, j, t )
      end do

      return
      end
      function triangle_xy_integral ( x1, y1, x2, y2, x3, y3 )

c*********************************************************************72
c
cc TRIANGLE_XY_INTEGRAL computes the integral of XY over a triangle.
c
c  Discussion:
c
c    This function was written as a special test case for the general
c    problem of integrating a monomial x^alpha * y^beta over a general 
c    triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X1, Y1, X2, Y2, X3, Y3, the coordinates of the
c    triangle vertices.
c
c    Output, double precision TRIANGLE_XY_INTEGRAL, the integral of X*Y 
c    over the triangle.
c
      implicit none

      double precision det
      double precision p00
      double precision p01
      double precision p02
      double precision p10
      double precision p11
      double precision p20
      double precision q
      double precision triangle01_monomial_integral
      double precision triangle_xy_integral
      double precision x1
      double precision x2
      double precision x3
      double precision y1
      double precision y2
      double precision y3
c
c  x = x1 * ( 1 - xi - eta )
c    + x2 *       xi
c    + x3 *            eta
c
c  y = y1 * ( 1 - xi - eta )
c    + y2 *       xi
c    + y3 *            eta
c
c  Rewrite as linear polynomials in (xi,eta):
c
c  x = x1 + ( x2 - x1 ) * xi + ( x3 - x1 ) * eta
c  y = y1 + ( y2 - y1 ) * xi + ( y3 - y1 ) * eta
c
c  Jacobian:
c
c    J = [ ( x2 - x1 )  ( x3 - x1 ) ]
c        [ ( y2 - y1 )  ( y3 - y1 ) ]
c
c    det J = ( x2 - x1 ) * ( y3 - y1 ) - ( y2 - y1 ) * ( x3 - x1 )
c
c  Integrand
c
c    x * y = ( x1 + ( x2 - x1 ) * xi + ( x3 - x1 ) * eta )
c          * ( y1 + ( y2 - y1 ) * xi + ( y3 - y1 ) * eta )
c
c  Rewrite as linear combination of monomials:
c
c    x * y = 1      * x1 * y1
c          + eta    * ( x1 * ( y3 - y1 ) + ( x3 - x1 ) * y1 )
c          + xi     * ( x1 * ( y2 - y1 ) + ( x2 - x1 ) * y1 )
c          + eta^2  * ( x3 - x1 ) * ( y3 - y1 )
c          + xi*eta * ( ( x2 - x1 ) * ( y3 - y1 ) + ( x3 - x1 ) * ( y2 - y1 ) )
c          + xi^2   * ( x2 - x1 ) * ( y2 - y1 )
c
      det = ( x2 - x1 ) * ( y3 - y1 ) - ( y2 - y1 ) * ( x3 - x1 )

      p00 = x1 * y1

      p01 = x1 * ( y3 - y1 ) + ( x3 - x1 ) * y1
      p10 = x1 * ( y2 - y1 ) + ( x2 - x1 ) * y1

      p02 = ( x3 - x1 ) * ( y3 - y1 )
      p11 = ( x2 - x1 ) * ( y3 - y1 ) + ( x3 - x1 ) * ( y2 - y1 )
      p20 = ( x2 - x1 ) * ( y2 - y1 )

      q = 0.0D+00
      q = q + p00 * triangle01_monomial_integral ( 0, 0 )
      q = q + p10 * triangle01_monomial_integral ( 1, 0 )
      q = q + p01 * triangle01_monomial_integral ( 0, 1 )
      q = q + p20 * triangle01_monomial_integral ( 2, 0 )
      q = q + p11 * triangle01_monomial_integral ( 1, 1 )
      q = q + p02 * triangle01_monomial_integral ( 0, 2 )

      q = q * det

      triangle_xy_integral = q

      return
      end
      function trinomial ( i, j, k )

c*********************************************************************72
c
cc TRINOMIAL computes a trinomial coefficient.
c
c  Discussion:
c
c    The trinomial coefficient is a generalization of the binomial
c    coefficient.  It may be interpreted as the number of combinations of
c    N objects, where I objects are of type 1, J of type 2, and K of type 3.
c    and N = I + J + K.
c
c    T(I,J,K) = N! / ( I! J! K! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, K, the factors.
c    All should be nonnegative.
c
c    Output, integer TRINOMIAL, the trinomial coefficient.
c
      implicit none

      integer i
      integer j
      integer k
      integer l
      integer t
      integer trinomial
      integer value
c
c  Each factor must be nonnegative.
c
      if ( i .lt. 0 .or. j .lt. 0 .or. k .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRINOMIAL - Fatal error!'
        write ( *, '(a)' ) '  Negative factor encountered.'
        stop 1
      end if

      value = 1

      t = 1

      do l = 1, i
c   value = value * t / l
        t = t + 1
      end do

      do l = 1, j
        value = value * t / l
        t = t + 1
      end do

      do l = 1, k
        value = value * t / l
        t = t + 1
      end do
      
      trinomial = value

      return
      end
      subroutine xy_to_rs_map ( t, a, b, c, d, e, f )

c*********************************************************************72
c
cc XY_TO_RS_MAP returns the linear map from physical to reference triangle.
c
c  Discussion:
c
c    Given the vertices T of an arbitrary triangle in the (X,Y) coordinate
c    system, this function returns the coefficients of the linear map
c    that sends the vertices of T to (0,0), (1,0) and (0,1) respectively
c    in the reference triangle with coordinates (R,S):
c
c      R = A + B * X + C * Y;
c      S = D + E * X + F * Y.
c
c  Reference Element T3:
c
c    |
c    1  3
c    |  |\
c    |  | \
c    S  |  \
c    |  |   \
c    |  |    \
c    0  1-----2
c    |
c    +--0--R--1-->
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the X and Y coordinates
c    of the vertices.  The vertices are assumed to be the images of
c    (0,0), (1,0) and (0,1) respectively.
c
c    Output, double precision A, B, C, D, E, F, the mapping coefficients.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision d
      double precision e
      double precision f
      double precision g
      double precision t(2,3)

      g =   ( ( t(2,3) - t(2,1) ) * ( t(1,2) - t(1,1) )   
     &      - ( t(1,3) - t(1,1) ) * ( t(2,2) - t(2,1) ) )

      a = ( - ( t(2,3) - t(2,1) ) * t(1,1)  
     &      + ( t(1,3) - t(1,1) ) * t(2,1) ) / g

      b =     ( t(2,3) - t(2,1) ) / g

      c =   - ( t(1,3) - t(1,1) ) / g

      d = (   ( t(2,2) - t(2,1) ) * t(1,1) 
     &      - ( t(1,2) - t(1,1) ) * t(2,1) ) / g

      e =   - ( t(2,2) - t(2,1) ) / g

      f =     ( t(1,2) - t(1,1) ) / g

      return
      end
