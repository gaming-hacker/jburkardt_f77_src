      subroutine comp_next_grlex ( kc, xc )

c*********************************************************************72
c
cc COMP_NEXT_GRLEX returns the next composition in grlex order.
c
c  Discussion:
c
c    Example:
c
c    KC = 3
c
c    #   XC(1  XC(2) XC(3)  Degree
c      +------------------------
c    1 |  0     0     0        0
c      |
c    2 |  0     0     1        1
c    3 |  0     1     0        1
c    4 |  1     0     0        1
c      |
c    5 |  0     0     2        2
c    6 |  0     1     1        2
c    7 |  0     2     0        2
c    8 |  1     0     1        2
c    9 |  1     1     0        2
c   10 |  2     0     0        2
c      |
c   11 |  0     0     3        3
c   12 |  0     1     2        3
c   13 |  0     2     1        3
c   14 |  0     3     0        3
c   15 |  1     0     2        3
c   16 |  1     1     1        3
c   17 |  1     2     0        3
c   18 |  2     0     1        3
c   19 |  2     1     0        3
c   20 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer KC, the number of parts of the composition.
c    1 <= KC.
c
c    Input/output, integer XC(KC), the current composition.
c    Each entry of XC must be nonnegative.
c    On return, XC has been replaced by the next composition in the
c    grlex order.
c
      implicit none

      integer kc

      integer i
      integer im1
      integer j
      integer t
      integer xc(kc)
c
c  Ensure that 1 <= KC.
c
      if ( kc .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  KC .lt. 1'
        stop 1
      end if
c
c  Ensure that 0 <= XC(I).
c
      do i = 1, kc
        if ( xc(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
          write ( *, '(a)' ) '  XC(I) .lt. 0'
          stop 1
        end if
      end do
c
c  Find I, the index of the rightmost nonzero entry of X.
c
      i = 0
      do j = kc, 1, -1
        if ( 0 .lt. xc(j) ) then
          i = j
          go to 10
        end if
      end do

10    continue
c
c  set T = X(I)
c  set XC(I) to zero,
c  increase XC(I-1) by 1,
c  increment XC(KC) by T-1.
c
      if ( i == 0 ) then
        xc(kc) = 1
        return
      else if ( i == 1 ) then
        t = xc(1) + 1
        im1 = kc
      else if ( 1 .lt. i ) then
        t = xc(i)
        im1 = i - 1
      end if

      xc(i) = 0
      xc(im1) = xc(im1) + 1
      xc(kc) = xc(kc) + t - 1

      return
      end
      subroutine comp_random ( n, k, seed, a )

c*********************************************************************72
c
cc COMP_RANDOM selects a random composition of the integer N into K parts.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2007
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the integer to be decomposed.
c
c    Input, integer K, the number of parts in the composition.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, integer A(K), the parts of the composition.
c
      implicit none

      integer k

      integer a(k)
      integer i
      integer l
      integer m
      integer n
      integer seed

      call ksub_random ( n+k-1, k-1, seed, a )

      a(k) = n + k
      l = 0

      do i = 1, k
        m = a(i)
        a(i) = a(i) - l - 1
        l = m
      end do

      return
      end
      function i4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM_AB, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_uniform_ab
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r = real ( seed ) * 4.656612875E-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0E+00 - r ) * ( real ( min ( a, b ) ) - 0.5E+00 )
     &  +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform_ab = value

      return
      end
      subroutine i4mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    39 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      character * ( * ) title

      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &   jhi, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*8 ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer  j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )  title

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
          write ( ctemp(i2), '(i8)' ) i
        end do

        write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc

            i = i2lo - 1 + i2

            write ( ctemp(i2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine ksub_random ( n, k, seed, a )

c*********************************************************************72
c
cc KSUB_RANDOM selects a random subset of size K from a set of size N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2007
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the size of the set from which subsets are drawn.
c
c    Input, integer K, number of elements in desired subsets.  K must
c    be between 0 and N.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, integer A(K).  A(I) is the I-th element of the
c    output set.  The elements of A are in order.
c
      implicit none

      integer k

      integer a(k)
      integer i
      integer i4_uniform_ab
      integer ids
      integer ihi
      integer ip
      integer ir
      integer is
      integer ix
      integer l
      integer ll
      integer m
      integer m0
      integer n
      integer seed

      if ( k .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KSUB_RANDOM - Fatal error!'
        write ( *, '(a,i8)' ) '  K = ', k
        write ( *, '(a)' ) '  but 0 <= K is required!'
        stop 1
      else if ( n .lt. k ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KSUB_RANDOM - Fatal error!'
        write ( *, '(a,i8)' ) '  N = ', n
        write ( *, '(a,i8)' ) '  K = ', k
        write ( *, '(a)' ) '  K <= N is required!'
        stop 1
      end if

      if ( k .eq. 0 ) then
        return
      end if

      do i = 1, k
        a(i) = ( ( i - 1 ) * n ) / k
      end do

      do i = 1, k

10      continue

          ix = i4_uniform_ab ( 1, n, seed )

          l = 1 + ( ix * k - 1 ) / n

          if ( a(l) .lt. ix ) then
            go to 20
          end if

        go to 10

20      continue

        a(l) = a(l) + 1

      end do

      ip = 0
      is = k

      do i = 1, k

        m = a(i)
        a(i) = 0

        if ( m .ne. ( ( i - 1 ) * n ) / k ) then
          ip = ip + 1
          a(ip) = m
        end if

      end do

      ihi = ip

      do i = 1, ihi
        ip = ihi + 1 - i
        l = 1 + ( a(ip) * k - 1 ) / n
        ids = a(ip) - ( ( l - 1 ) * n ) / k
        a(ip) = 0
        a(is) = l
        is = is - ids
      end do

      do ll = 1, k

        l = k + 1 - ll

        if ( a(l) .ne. 0 ) then
          ir = l
          m0 = 1 + ( ( a(l) - 1 ) * n ) / k
          m = ( a(l) * n ) / k - m0 + 1
        end if

        ix = i4_uniform_ab ( m0, m0 + m - 1, seed )

        i = l + 1

30      continue

        if ( i .le. ir ) then

          if ( ix .lt. a(i) ) then
            go to 40
          end if

          ix = ix + 1
          a(i-1) = a(i)
          i = i + 1

          go to 30

        end if

40      continue

        a(i-1) = ix
        m = m - 1

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
      subroutine simplex_grid_index_all ( m, n, ng, grid )

c*********************************************************************72
c
cc SIMPLEX_GRID_INDEX_ALL returns all the simplex grid indices.
c
c  Discussion:
c
c    The number of grid indices can be determined by calling
c      ng = simplex_grid_size ( m, n )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer NG, the number of values in the grid.
c
c    Output, integer GRID(M+1,NG), the current, and then the next,
c    grid index.
c
      implicit none

      integer m
      integer ng

      integer g(m+1)
      integer grid(m+1,ng)
      integer i
      integer k
      integer n

      do i = 1, m
        g(i) = 0
      end do
      g(m+1) = n

      k = 1
      grid(1:m+1,k) = g(1:m+1)

10    continue

      if ( k < ng ) then
        call comp_next_grlex ( m + 1, g )
        k = k + 1
        grid(1:m+1,k) = g(1:m+1)
        go to 10
      end if

      return
      end
      subroutine simplex_grid_index_next ( m, n, g )

c*********************************************************************72
c
cc SIMPLEX_GRID_INDEX_NEXT returns the next simplex grid index.
c
c  Discussion:
c
c    The vector G has dimension M+1.  The first M entries may be regarded
c    as grid coordinates.  These coordinates must have a sum between 0 and N.
c    The M+1 entry contains the remainder, that is N minus the sum of the
c    first M coordinates.
c
c    Each time the function is called, it is given a current grid index, and
c    computes the next one.  The very first index is all zero except for a
c    final value of N, and the very last index has all zero except for an'
c    intial value of N.
c
c    For example, here are the coordinates in order for M = 3, N = 3:
c
c     0  0 0 0 3
c     1  0 0 1 2
c     2  0 0 2 1
c     3  0 0 3 0
c     4  0 1 0 2
c     5  0 1 1 1
c     6  0 1 2 0
c     7  0 2 0 1
c     8  0 2 1 0
c     9  0 3 0 0
c    10  1 0 0 2
c    11  1 0 1 1
c    12  1 0 2 0
c    13  1 1 0 1
c    14  1 1 1 0
c    15  1 2 0 0
c    16  2 0 0 1
c    17  2 0 1 0
c    18  2 1 0 0
c    19  3 0 0 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of subintervals.
c
c    Input/output, integer G(M+1), the current, and then the next,
c    grid index.
c
      implicit none

      integer m

      integer g(m+1)
      integer n

      call comp_next_grlex ( m + 1, g )

      return
      end
      subroutine simplex_grid_index_sample ( m, n, seed, g )

c*********************************************************************72
c
cc SIMPLEX_GRID_INDEX_SAMPLE returns a random simplex grid index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of subintervals in
c    each dimension.
c
c    Input, integer SEED, a seed for the random number generator.
c
c    Output, integer G(M+1), a randomly selected index in the
c    simplex grid.
c
c    Output, integer SEED, the updated random number seed.
c
      implicit none

      integer m

      integer g(m+1)
      integer n
      integer seed

      call comp_random ( n, m + 1, seed, g )

      return
      end
      subroutine simplex_grid_index_to_point ( m, n, ng, g, v, x )

c*********************************************************************72
c
cc SIMPLEX_GRID_INDEX_TO_POINT returns  points corresponding to simplex indices.
c
c  Discussion:
c
c    The M-dimensional simplex is defined by M+1 vertices.
c
c    Given a regular grid that uses N subintervals along the edge between
c    each pair of vertices, a simplex grid index G is a set of M+1 values
c    each between 0 and N, and summing to N.
c
c    This function determines the coordinates X of the point corresponding
c    to the index G.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of subintervals.
c
c    Input, integer NG, the number of grid indices to be converted.
c
c    Input, integer G(M+1,NG), the grid indices of 1
c    or more points.
c
c    Input, double precision V(M,M+1), the coordinates of the vertices
c    of the simplex.
c
c    Output, double precision X(M,NG), the coordinates of one or more points.
c
      implicit none

      integer m
      integer n
      integer ng

      integer g(m+1,ng)
      integer i
      integer j
      integer k
      double precision v(m,m+1)
      double precision x(m,ng)

      do j = 1, ng
        do i = 1, m
          x(i,j) = 0.0D+00
          do k = 1, m + 1
            x(i,j) = x(i,j) + v(i,k) * dble ( g(k,j) )
          end do
          x(i,j) = x(i,j) / dble ( n )
        end do
      end do

      return
      end
      subroutine simplex_grid_size ( m, n, ng )

c*********************************************************************72
c
cc SIMPLEX_GRID_SIZE counts the grid points inside a simplex.
c
c  Discussion:
c
c    The size of a grid with parameters M, N is C(M+N,N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of subintervals.
c
c    Output, integer NG, the number of grid points.
c
      implicit none

      integer i
      integer m
      integer n
      integer ng

      ng = 1

      do i = 1, m
        ng = ( ng * ( n + i ) ) / i
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
