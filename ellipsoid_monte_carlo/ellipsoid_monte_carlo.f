      subroutine ellipsoid_sample ( m, n, a, v, r, seed, x )

c*********************************************************************72
c
cc ELLIPSOID_SAMPLE samples points uniformly from an ellipsoid.
c
c  Discussion:
c
c    The points X in the ellipsoid are described by a M by M
c    positive definite symmetric matrix A, a "center" V, and
c    a "radius" R, such that
c
c      (X-V)' * A * (X-V) <= R * R
c
c    The algorithm computes the Cholesky factorization of A:
c
c      A = U' * U.
c
c    A set of uniformly random points Y is generated, satisfying:
c
c      Y' * Y <= R * R.
c
c    The appropriate points in the ellipsoid are found by solving
c
c      U * X = Y
c      X = X + V
c
c    Thanks to Dr Karl-Heinz Keil for pointing out that the original
c    coding was actually correct only if A was replaced by its inverse.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer M, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input, double precision A(M,M), the matrix that describes
c    the ellipsoid.
c
c    Input, double precision V(M), the "center" of the ellipsoid.
c
c    Input, double precision R, the "radius" of the ellipsoid.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(M,N), the points.
c
      implicit none

      integer m
      integer n

      double precision a(m,m)
      integer i
      integer info
      integer j
      double precision r
      integer seed
      double precision u(m,m)
      double precision v(m)
      double precision x(m,n)
c
c  Get the Cholesky factor U.
c
      call r8mat_copy ( m, m, a, u )

      call r8po_fa ( m, u, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ELLIPSOID_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  R8PO_FA reports that the matrix A '
        write ( *, '(a)' ) '  is not positive definite symmetric.'
        stop 1
      end if
c
c  Get the points Y that satisfy Y' * Y <= 1.
c
      call uniform_in_sphere01_map ( m, n, seed, x )
c
c  Get the points Y that satisfy Y' * Y <= R * R.
c
      do j = 1, n
        do i = 1, m
          x(i,j) = r * x(i,j)
        end do
      end do
c
c  Solve U * X = Y.
c
      do j = 1, n
        call r8po_sl ( m, u, x(1:m,j) )
      end do
c
c  X = X + V.
c
      do j = 1, n
        do i = 1, m
          x(i,j) = x(i,j) + v(i)
        end do
      end do

      return
      end
      function ellipsoid_volume ( m, a, v, r )

c*********************************************************************72
c
cc ELLIPSOID_VOLUME returns the volume of an ellipsoid.
c
c  Discussion:
c
c    The points X in the ellipsoid are described by an M by M
c    positive definite symmetric matrix A, an M-dimensional point V,
c    and a "radius" R, such that
c      (X-V)' * A * (X-V) <= R * R
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, double precision A(M,M), the matrix that describes
c    the ellipsoid.  A must be symmetric and positive definite.
c
c    Input, double precision V(M), the "center" of the ellipse.
c    The value of V is not actually needed by this function.
c
c    Input, double precision R, the "radius" of the ellipse.
c
c    Output, double precision ELLIPSOID_VOLUME, the volume of the ellipsoid.
c
      implicit none

      integer m

      double precision a(m,m)
      double precision ellipsoid_volume
      double precision hypersphere_unit_volume
      integer i
      integer info
      double precision r
      double precision sqrt_det
      double precision u(m,m)
      double precision v(m)

      call r8mat_copy ( m, m, a, u )

      call r8po_fa ( m, u, info )

      sqrt_det = 1.0D+00
      do i = 1, m
        sqrt_det = sqrt_det * u(i,i)
      end do

      ellipsoid_volume = r ** m * hypersphere_unit_volume ( m ) 
     &  / sqrt_det

      return
      end
      function hypersphere_unit_volume ( m )

c*********************************************************************72
c
cc HYPERSPHERE_UNIT_VOLUME: volume of a unit hypersphere in M dimensions.
c
c  Discussion:
c
c    The unit hypersphere in M dimensions satisfies:
c
c      sum ( 1 <= I <= M ) X(I) * X(I) = 1
c
c    Results for the first few values of DIM_NUM are:
c
c     M  Volume
c
c     1    2
c     2    1        * PI
c     3  ( 4 /   3) * PI
c     4  ( 1 /   2) * PI^2
c     5  ( 8 /  15) * PI^2
c     6  ( 1 /   6) * PI^3
c     7  (16 / 105) * PI^3
c     8  ( 1 /  24) * PI^4
c     9  (32 / 945) * PI^4
c    10  ( 1 / 120) * PI^5
c
c    For the unit sphere, Volume(M) = 2 * PI * Volume(M-2)/ M
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision HYPERSPHERE_UNIT_VOLUME, the volume.
c
      implicit none

      double precision hypersphere_unit_volume
      integer i
      integer m
      integer m2
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision value

      if ( mod ( m, 2 ) .eq. 0 ) then
        m2 = m / 2
        value = r8_pi ** m2
        do i = 1, m2
          value = value / dble ( i )
        end do
      else
        m2 = ( m - 1 ) / 2
        value = r8_pi ** m2 * 2.0D+00 ** m
        do i = m2 + 1, 2 * m2 + 1
          value = value / dble ( i )
        end do
      end if

      hypersphere_unit_volume = value

      return
      end
      subroutine monomial_value ( m, n, e, x, value )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 <= i <= m ) x(i)^e(i)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points at which the
c    monomial is to be evaluated.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the point coordinates.
c
c    Output, double precision VALUE(N), the value of the monomial.
c
      implicit none

      integer m
      integer n

      integer e(m)
      integer i
      integer j
      double precision value(n)
      double precision x(m,n)

      do j = 1, n
        value(j) = 1.0D+00
        do i = 1, m
          if ( 0 .ne. e(i) ) then
            value(j) = value(j) * x(i,j) ** e(i)
          end if
        end do
      end do

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
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
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
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
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      double precision r8_uniform_01
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
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
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
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
c    28 April 2008
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
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
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
c    28 April 2008
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
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
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

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine r8po_fa ( n, a, info )

c*********************************************************************72
c
cc R8PO_FA factors an R8PO matrix.
c
c  Discussion:
c
c    The R8PO storage format is used for a symmetric positive definite 
c    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
c    upper triangular matrix, so it will be in R8GE storage format.)
c
c    Only the diagonal and upper triangle of the square array are used.
c    This same storage scheme is used when the matrix is factored by
c    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
c    is set to zero.
c
c    R8PO storage is used by LINPACK and LAPACK.
c
c    The positive definite symmetric matrix A has a Cholesky factorization
c    of the form:
c
c      A = R' * R
c
c    where R is an upper triangular matrix with positive elements on
c    its diagonal.  This routine overwrites the matrix A with its
c    factor R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2003
c
c  Author:
c
c    Original FORTRAN77 version by Dongarra, Bunch, Moler, Stewart.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input/output, double precision A(N,N).
c    On input, the matrix in R8PO storage.
c    On output, the Cholesky factor R in R8GE storage.
c
c    Output, integer INFO, error flag.
c    0, normal return.
c    K, error condition.  The principal minor of order K is not
c    positive definite, and the factorization was not completed.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer info
      integer j
      integer k
      double precision s

      do j = 1, n

        do k = 1, j - 1
          a(k,j) = ( a(k,j) - sum ( a(1:k-1,k) * a(1:k-1,j) ) ) / a(k,k)
        end do

        s = a(j,j) - sum ( a(1:j-1,j)**2 )

        if ( s .le. 0.0D+00 ) then
          info = j
          return
        end if

        a(j,j) = sqrt ( s )

      end do

      info = 0
c
c  Since the Cholesky factor is stored in R8GE format, be sure to
c  zero out the lower triangle.
c
      do i = 1, n
        do j = 1, i-1
          a(i,j) = 0.0D+00
        end do
      end do

      return
      end
      subroutine r8po_sl ( n, a_lu, b )

c*********************************************************************72
c
cc R8PO_SL solves an R8PO system factored by R8PO_FA.
c
c  Discussion:
c
c    The R8PO storage format is used for a symmetric positive definite 
c    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
c    upper triangular matrix, so it will be in R8GE storage format.)
c
c    Only the diagonal and upper triangle of the square array are used.
c    This same storage scheme is used when the matrix is factored by
c    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
c    is set to zero.
c
c    R8PO storage is used by LINPACK and LAPACK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 August 2014
c
c  Author:
c
c    Original FORTRAN77 version by Dongarra, Bunch, Moler, Stewart.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A_LU(N,N), the Cholesky factor from R8PO_FA.
c
c    Input/output, double precision B(N).
c    On input, the right hand side.
c    On output, the solution vector.
c
      implicit none

      integer n

      double precision a_lu(n,n)
      double precision b(n)
      integer i
      integer k
      double precision t
c
c  Solve R' * y = b.
c
      do k = 1, n
        t = 0.0D+00
        do i = 1, k - 1
          t = t + b(i) * a_lu(i,k)
        end do
        b(k) = ( b(k) - t ) / a_lu(k,k)
      end do
c
c  Solve R * x = y.
c
      do k = n, 1, -1
        b(k) = b(k) / a_lu(k,k)
        do i = 1, k - 1
          b(i) = b(i) - a_lu(i,k) * b(k)
        end do
      end do

      return
      end
      function r8vec_norm ( n, a )

c*********************************************************************72
c
cc R8VEC_NORM returns the L2 norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
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
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), the vector whose L2 norm is desired.
c
c    Output, double precision R8VEC_NORM, the L2 norm of A.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_norm
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + a(i) * a(i)
      end do
      value = sqrt ( value )

      r8vec_norm = value

      return
      end
      subroutine r8vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.  If N is negative,
c    then the code will flush its internal memory; in particular,
c    if there is a saved value to be used on the next call, it is
c    instead discarded.  This is useful if the user has reset the
c    random number seed, for instance.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
      implicit none

      integer n

      integer i
      integer m
      double precision r(n+1)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  Maybe we don't need any more values.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop 1
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * r8_pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        m = ( x_hi_index - x_lo_index + 1 ) / 2

        call r8vec_uniform_01 ( 2 * m, seed, r )

        do i = 1, 2 * m, 2

          x(x_lo_index+i-1) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * cos ( 2.0D+00 * r8_pi * r(i+1) )

          x(x_lo_index+i) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * sin ( 2.0D+00 * r8_pi * r(i+1) )

        end do
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        x_hi_index = x_hi_index - 1

        m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

        call r8vec_uniform_01 ( 2 * m, seed, r )

        do i = 1, 2 * m - 3, 2

          x(x_lo_index+i-1) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * cos ( 2.0D+00 * r8_pi * r(i+1) )

          x(x_lo_index+i) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * sin ( 2.0D+00 * r8_pi * r(i+1) )

        end do

        x(n) = sqrt ( -2.0D+00 * log ( r(2*m-1) ) )
     &    * cos ( 2.0D+00 * r8_pi * r(2*m) )

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
      character * ( * ) title

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
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
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
      subroutine uniform_in_sphere01_map ( dim_num, n, seed, x )

c*********************************************************************72
c
cc UNIFORM_IN_SPHERE01_MAP maps uniform points into the unit sphere.
c
c  Discussion:
c
c    The sphere has center 0 and radius 1.
c
c    This routine is valid for any spatial dimension DIM_NUM.
c
c    We first generate a point ON the sphere, and then distribute it
c    IN the sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Russell Cheng,
c    Random Variate Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998, pages 168.
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the space.
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,N), the points.
c
      implicit none

      integer dim_num
      integer n

      double precision exponent
      integer i
      integer j
      double precision norm
      double precision r
      double precision r8_uniform_01
      double precision r8vec_norm
      integer seed
      double precision x(dim_num,n)

      exponent = 1.0D+00 / dble ( dim_num )

      do j = 1, n
c
c  Fill a vector with normally distributed values.
c
        call r8vec_normal_01 ( dim_num, seed, x(1,j) )
c
c  Compute the length of the vector.
c
        norm = r8vec_norm ( dim_num, x(1,j) )
c
c  Normalize the vector.
c
        do i = 1, dim_num
          x(i,j) = x(i,j) / norm
        end do
c
c  Now compute a value to map the point ON the sphere INTO the sphere.
c
        r = r8_uniform_01 ( seed )

        do i = 1, dim_num
          x(i,j) = r ** exponent * x(i,j)
        end do

      end do

      return
      end