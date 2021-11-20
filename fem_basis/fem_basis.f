      subroutine fem_basis_1d ( i, j, x, lij )

c*********************************************************************72
c
cc FEM_BASIS_1D evaluates an arbitrary 1D basis function.
c
c  Discussion:
c
c    Given the maximum degree D for the polynomial basis defined
c    on a reference interval, we have D + 1 monomials
c    of degree at most D.  In each barycentric coordinate, we define
c    D+1 points, so that 0 <= I, J <= D and I+J = D, with
c    (I,J) corresponding to
c    * the basis point X(I,J) = ( I/D );
c    * the basis monomial P(I,J)(X) = X^I.
c
c    For example, with D = 2, we have simply:
c
c      A---B---C
c
c    with
c
c       I J    X      P(I,J)(X)
c
c    A (0 2) ( 0.0 )  1
c    B (1 1) ( 0.5 )  x
c    C (2 0) ( 1.0 )  x^2
c
c    Now instead of the monomials P(I,J)(X), we want a set of
c    polynomials L(I,J)(X) which span the same space, but have
c    the Lagrange property, namely L(I,J) (X) is 1 if X is
c    equal to X(I,J), and 0 if X is equal to any other
c    of the basis points.
c
c    This is easily arranged.  Given an index (I,J), we compute
c    1) I factors of the form (   X -0/D) * (   X -1/D) * ... * (   X -(I-1)/D);
c    2) J factors of the form ((1-X)-0/D) * ((1-X)-1/D) * ... * ((1-X)-(J-1)/D).
c
c    This results in the product of I+J linear factors, in other words,
c    a polynomial of degree D.  This polynomial is 0 at all basis points
c    except X(I,J).  If we divide this polynomial by its value at
c    the basis point, we arrive at the desired Lagrange polynomial
c    L(I,J)(X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the integer barycentric coordinates of
c    the basis function, 0 <= I, J.  The polynomial degree D = I + J.
c
c    Input, double precision X, the evaluation point.
c
c    Output, double precision LIJ, the value of the basis function at X.
c
      implicit none

      double precision c
      integer d
      integer i
      integer j
      double precision lij
      integer p
      double precision w
      double precision x

      d = i + j
      lij = 1.0D+00
      c = 1.0D+00
      do p = 0, i - 1
        lij = lij * ( d * x - p )
        c = c     * (     i - p )
      end do
      w = 1.0D+00 - x
      do p = 0, j - 1
        lij = lij * ( d * w - p )
        c = c     * (     j - p )
      end do

      lij = lij / c

      return
      end
      subroutine fem_basis_2d ( i, j, k, x, y, lijk )

c*********************************************************************72
c
cc FEM_BASIS_2D evaluates an arbitrary triangular basis function.
c
c  Discussion:
c
c    Given the maximum degree D for the polynomial basis defined
c    on a reference triangle, we have ( ( D + 1 ) * ( D + 2 ) ) / 2 monomials
c    of degree at most D.  In each barycentric coordinate, we define
c    D+1 planes, so that 0 <= I, J, K <= D and I+J+K = D, with
c    (I,J,K) corresponding to
c    * the basis point (X,Y)(I,J,K) = ( I/D, J/D );
c    * the basis monomial P(I,J,K)(X,Y) = X^I Y^J.
c
c    For example, with D = 2, we have simply:
c
c    F
c    |\
c    C-E
c    |\|\
c    A-B-D
c
c    with
c
c       I J K    X    Y    P(I,J,K)(X,Y)
c
c    A (0 0 2) (0.0, 0.0)  1
c    B (1 0 1) (0.5, 0.0)  x
c    C (0 1 1) (0.0, 0.5)  y
c    D (2 0 0) (1.0, 0.0)  x^2
c    E (1 1 0) (0.5, 0.5)  x y
c    F (0 2 0) (0.0, 1.0)  y^2
c
c    Now instead of the monomials P(I,J,K)(X,Y), we want a set of
c    polynomials L(I,J,K)(X,Y) which span the same space, but have
c    the Lagrange property, namely L(I,J,K) (X,Y) is 1 if (X,Y) is
c    equal to (X,Y)(I,J,K), and 0 if (X,Y) is equal to any other
c    of the basis points.
c
c    This is easily arranged.  Given an index (I,J,K), we compute
c    1) I factors of the form (X-0)   * (X-1/D)   * ... * (X-(I-1)/D);
c    2) J factors of the form (Y-0)   * (Y-1/D)   * ... * (Y-(J-1)/D);
c    3) K factors of the form ((1-X-Y)-0/D) * ((1-X-Y)-1/D) * ...
c       * ((1-X-Y)-(K-1)/D).
c
c    This results in the product of I+J+K linear factors, in other words,
c    a polynomial of degree D.  This polynomial is 0 at all basis points
c    except (X,Y)(I,J,K).  If we divide this polynomial by its value at
c    the basis point, we arrive at the desired Lagrange polynomial
c    L(I,J,K)(X,Y).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, K, the integer barycentric coordinates of
c    the basis function, 0 <= I, J, K.  The polynomial degree D = I + J + K.
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision LIJK, the value of the basis function at (X,Y).
c
      implicit none

      double precision c
      integer d
      integer i
      integer j
      integer k
      double precision lijk
      integer p
      double precision w
      double precision x
      double precision y

      d = i + j + k
      lijk = 1.0D+00
      c = 1.0D+00
      do p = 0, i - 1
        lijk = lijk * ( d * x - p )
        c = c       * (     i - p )
      end do
      do p = 0, j - 1
        lijk = lijk * ( d * y - p )
        c = c       * (     j - p )
      end do
      w = 1.0D+00 - x - y
      do p = 0, k - 1
        lijk = lijk * ( d * w - p )
        c = c       * (     k - p )
      end do

      lijk = lijk / c

      return
      end
      subroutine fem_basis_3d ( i, j, k, l, x, y, z, lijkl )

c*********************************************************************72
c
cc FEM_BASIS_3D evaluates an arbitrary tetrahedral basis function.
c
c  Discussion:
c
c    Given the maximum degree D for the polynomial basis defined
c    on a reference tetrahedron, we have
c    ( D + 1 ) * ( D + 2 ) * ( D + 3 ) / 6 monomials
c    of degree at most D.  In each barycentric coordinate, we define
c    D+1 planes, so that 0 <= I, J, K, L <= D and I+J+K+L = D, with
c    (I,J,K,L) corresponding to
c    * the basis point (X,Y,Z)(I,J,K,L) = ( I/D, J/D, K/D );
c    * the basis monomial P(I,J,K,L)(X,Y,Z) = X^I Y^J Z^K.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, K, L, the integer barycentric
c    coordinates of the basis function, 0 <= I, J, K, L.
c    The polynomial degree D = I + J + K + L.
c
c    Input, double precision X, Y, Z, the evaluation point.
c
c    Output, double precision LIJKL, the value of the basis function
c    at (X,Y,Z).
c
      implicit none

      double precision c
      integer d
      integer i
      integer j
      integer k
      integer l
      double precision lijkl
      integer p
      double precision w
      double precision x
      double precision y
      double precision z

      d = i + j + k + l
      lijkl = 1.0D+00
      c = 1.0D+00
      do p = 0, i - 1
        lijkl = lijkl * ( d * x - p )
        c = c         * (     i - p )
      end do
      do p = 0, j - 1
        lijkl = lijkl * ( d * y - p )
        c = c         * (     j - p )
      end do
      do p = 0, k - 1
        lijkl = lijkl * ( d * z - p )
        c = c         * (     k - p )
      end do
      w = 1.0D+00 - x - y - z
      do p = 0, l - 1
        lijkl = lijkl * ( d * w - p )
        c = c         * (     l - p )
      end do

      lijkl = lijkl / c

      return
      end
      subroutine fem_basis_md ( m, i, x, l )

c*********************************************************************72
c
cc FEM_BASIS_MD evaluates an arbitrary M-dimensional basis function.
c
c  Discussion:
c
c    This routine evaluates the generalization of the formula used for
c    the 1D, 2D and 3D cases.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer I(M+1), the integer barycentric
c    coordinates of the basis function, 0 <= I(1:M+1).
c    The polynomial degree D = sum(I(1:M+1)).
c
c    Input, double precision X(M), the evaluation point.
c
c    Output, double precision L, the value of the basis function at X.
c
      implicit none

      integer m

      double precision c
      integer d
      integer i(m+1)
      integer i4vec_sum
      double precision l
      integer p
      integer q
      double precision r8vec_sum
      double precision w
      double precision x(m)

      d = i4vec_sum ( m + 1, i )

      l = 1.0D+00
      c = 1.0D+00

      do q = 1, m
        do p = 0, i(q) - 1
          l = l * ( d * x(q) - p )
          c = c * (     i(q) - p )
        end do
      end do

      w = 1.0D+00 - r8vec_sum ( m, x )

      do p = 0, i(m+1) - 1
        l = l * ( d * w      - p )
        c = c * (     i(m+1) - p )
      end do

      l = l / c

      return
      end
      subroutine fem_basis_prism_triangle ( i, j, xyz, b )

c*********************************************************************72
c
cc FEM_BASIS_PRISM_TRIANGLE evaluates a triangular prism basis function.
c
c  Discussion:
c
c    The element is a 3D prism, formed from a triangular base in the
c    XY plane that is extended vertically in the Z direction.
c
c    I(1:3) are the integer barycentric coordinates of a point in the
c    triangle.  I(1) + I(2) + I(3) = DI, the degree of the triangular
c    basis function BI.  X = I(1) / DI, Y = I(2) / DI.
c    The triangle is assumed to be the unit reference
c    triangle 0 <= X <= 1, 0 <= Y <= 1, 0 <= X + Y <= 1.
c
c    J(1:2) are the integer barycentric coordinates of a point in the
c    line segment.  J(1) + J(2) = DJ, the degree of the linear basis
c    function BJ.  Z = J(1) / DJ.
c    The line is assumed to be the unit line 0 <= Z <= 1.
c
c    The degree of the basis function B = BI * BJ is D = DI + DJ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I(3), the integer barycentric coordinates of
c    the triangular basis function, 0 <= I(*).
c    The polynomial degree DI = I(1) + I(2) + I(3).
c
c    Input, integer J(2), the integer barycentric coordinates of
c    the linear basis function, 0 <= J(*).
c    The polynomial degree DJ = J(1) + J(2).
c
c    Input, double precision XYZ(3), the evaluation point.
c
c    Output, double precision B, the value of the basis function at XYZ.
c
      implicit none

      double precision b
      double precision bi
      double precision bj
      integer i(3)
      integer j(2)
      double precision xyz(3)

      call fem_basis_2d ( i(1), i(2), i(3), xyz(1), xyz(2), bi )

      call fem_basis_1d ( j(1), j(2), xyz(3), bj )

      b = bi * bj

      return
      end
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

      return
      end
      function r8_fraction ( i, j )

c*********************************************************************72
c
cc R8_FRACTION uses real arithmetic on an integer ratio.
c
c  Discussion:
c
c    Given integer variables I and J, both FORTRAN and C will evaluate
c    an expression such as "I/J" using what is called "integer division",
c    with the result being an integer.  It is often convenient to express
c    the parts of a fraction as integers but expect the result to be computed
c    using real arithmetic.  This function carries out that operation.
c
c  Example:
c
c       I     J   I/J  R8_FRACTION
c
c       1     2     0  0.5
c       7     4     1  1.75
c       8     4     2  2.00
c       9     4     2  2.25
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the arguments.
c
c    Output, double precision R8_FRACTION, the value of the ratio.
c
      implicit none

      integer i
      integer j
      double precision r8_fraction

      r8_fraction = dble ( i ) / dble ( j )

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
