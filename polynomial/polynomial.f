      function i4_choose ( n, k )

c*********************************************************************72
c
cc I4_CHOOSE computes the binomial coefficient C(N,K).
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in integer arithmetic.
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
c    02 June 2007
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
c    Output, integer I4_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer i4_choose
      integer k
      integer mn
      integer mx
      integer n
      integer value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0

      else if ( mn .eq. 0 ) then

        value = 1

      else

        mx = max ( k, n - k )
        value = mx + 1

        do i = 2, mn
          value = ( value * ( mx + i ) ) / i
        end do

      end if

      i4_choose = value

      return
      end
      function i4_fall ( x, n )

c*********************************************************************72
c
cc I4_FALL computes the falling factorial function [X]_N.
c
c  Discussion:
c
c    Note that the number of "injections" or 1-to-1 mappings from
c    a set of N elements to a set of M elements is [M]_N.
c
c    The number of permutations of N objects out of M is [M]_N.
c
c    Moreover, the Stirling numbers of the first kind can be used
c    to convert a falling factorial into a polynomial, as follows:
c
c      [X]_N = S^0_N + S^1_N * X + S^2_N * X^2 + ... + S^N_N X^N.
c
c    The formula used is:
c
c      [X]_N = X * ( X - 1 ) * ( X - 2 ) * ... * ( X - N + 1 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the argument of the falling factorial function.
c
c    Input, integer N, the order of the falling factorial function.
c    If N = 0, FALL = 1, if N = 1, FALL = X.  Note that if N is
c    negative, a "rising" factorial will be computed.
c
c    Output, integer I4_FALL, the value of the falling 
c    factorial function.
c
      implicit none

      integer arg
      integer i
      integer i4_fall
      integer n
      integer value
      integer x

      value = 1

      arg = x

      if ( 0 .lt. n ) then

        do i = 1, n
          value = value * arg
          arg = arg - 1
        end do

      else if ( n .lt. 0 ) then

        do i = -1, n, -1
          value = value * arg
          arg = arg + 1
        end do

      end if

      i4_fall = value

      return
      end
      subroutine i4vec_concatenate ( n1, a, n2, b, c )

c*********************************************************************72
c
cc I4VEC_CONCATENATE concatenates two I4VEC's.
c
c  Discussion:
c
c    An I4VEC is a vector of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, the number of entries in the first vector.
c
c    Input, integer A(N1), the first vector.
c
c    Input, integer N2, the number of entries in the second vector.
c
c    Input, integer B(N2), the second vector.
c
c    Output, integer C(N1+N2), the concatenation of A and B.
c
      implicit none

      integer n1
      integer n2

      integer a(n1)
      integer b(n2)
      integer c(n1+n2)
      integer i

      do i = 1, n1
        c(i) = a(i)
      end do

      do i = 1, n2
        c(n1+i) = b(i)
      end do

      return
      end
      subroutine i4vec_permute ( n, p, a )

c*********************************************************************72
c
cc I4VEC_PERMUTE permutes an I4VEC in place.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    This routine permutes an array of integer "objects", but the same
c    logic can be used to permute an array of objects of any arithmetic
c    type, or an array of objects of any complexity.  The only temporary
c    storage required is enough to store a single object.  The number
c    of data movements made is N + the number of cycles of order 2 or more,
c    which is never more than N + N/2.
c
c  Example:
c
c    Input:
c
c      N = 5
c      P = (   2,   4,   5,   1,   3 )
c      A = (   1,   2,   3,   4,   5 )
c
c    Output:
c
c      A    = (   2,   4,   5,   1,   3 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of objects.
c
c    Input, integer P(N), the permutation.  P(I) = J means
c    that the I-th element of the output array should be the J-th
c    element of the input array.
c
c    Input/output, integer A(N), the array to be permuted.
c
      implicit none

      integer n

      integer a(n)
      integer a_temp
      integer i
      integer ierror
      integer iget
      integer iput
      integer istart
      integer p(n)

      call perm_check1 ( n, p )
c
c  Search for the next element of the permutation that has not been used.
c
      do istart = 1, n

        if ( p(istart) .lt. 0 ) then

          go to 20

        else if ( p(istart) .eq. istart ) then

          p(istart) = - p(istart)
          go to 20

        else

          a_temp = a(istart)
          iget = istart
c
c  Copy the new value into the vacated entry.
c
10        continue

            iput = iget
            iget = p(iget)

            p(iput) = - p(iput)

            if ( iget .lt. 1 .or. n .lt. iget ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
              write ( *, '(a)' ) '  An index is out of range.'
              write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
              stop 1
            end if

            if ( iget .eq. istart ) then
              a(iput) = a_temp
              go to 20
            end if

            a(iput) = a(iget)

          go to 10

        end if

20      continue

      end do
c
c  Restore the signs of the entries.
c
      do i = 1, n
        p(i) = - p(i)
      end do

      return
      end
      subroutine i4vec_sort_heap_index_a ( n, a, indx )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    The sorting is not actually carried out.  Rather an index array is
c    created which defines the sorting.  This array may be used to sort
c    or index the array, or to sort or index related arrays keyed on the
c    original array.
c
c    Once the index array is computed, the sorting can be carried out
c    "implicitly:
c
c      A(INDX(1:N)) is sorted,
c
c    or explicitly, by the call
c
c      call i4vec_permute ( n, indx, a )
c
c    after which A(1:N) is sorted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), an array to be index-sorted.
c
c    Output, integer INDX(N), the sort index.  The
c    I-th element of the sorted array is A(INDX(I)).
c
      implicit none

      integer n

      integer a(n)
      integer aval
      integer i
      integer indx(n)
      integer indxt
      integer ir
      integer j
      integer l

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, n
        indx(i) = i
      end do

      if ( n .eq. 1 ) then
        return
      end if

      l = n / 2 + 1
      ir = n

10    continue

        if ( 1 .lt. l ) then

          l = l - 1
          indxt = indx(l)
          aval = a(indxt)

        else

          indxt = indx(ir)
          aval = a(indxt)
          indx(ir) = indx(1)
          ir = ir - 1

          if ( ir .eq. 1 ) then
            indx(1) = indxt
            go to 30
          end if

        end if

        i = l
        j = l + l

20      continue

        if ( j .le. ir ) then

          if ( j .lt. ir ) then
            if ( a(indx(j)) .lt. a(indx(j+1)) ) then
              j = j + 1
            end if
          end if

          if ( aval .lt. a(indx(j)) ) then
            indx(i) = indx(j)
            i = j
            j = j + j
          else
            j = ir + 1
          end if

          go to 20

        end if

        indx(i) = indxt

      go to 10

30    continue

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
      subroutine mono_next_grlex ( m, x )

c*********************************************************************72
c
cc MONO_NEXT_GRLEX returns the next monomial in grlex order.
c
c  Discussion:
c
c    Example:
c
c    M = 3
c
c    #  X(1)  X(2)  X(3)  Degree
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
c    10 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer X(M), the current monomial.
c    The first element is X = [ 0, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer im1
      integer j
      integer t
      integer x(m)
c
c  Ensure that 1 <= M.
c
      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1'
        stop 1
      end if
c
c  Ensure that 0 <= X(I).
c
      do i = 1, m
        if ( x(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
          write ( *, '(a)' ) '  X(I) .lt. 0'
          stop 1
        end if
      end do
c
c  Find I, the index of the rightmost nonzero entry of X.
c
      i = 0
      do j = m, 1, -1

        if ( 0 .lt. x(j) ) then
          i = j
          go to 10
        end if

      end do

10    continue
c
c  set T = X(I)
c  set X(I) to zero,
c  increase X(I-1) by 1,
c  increment X(M) by T-1.
c
      if ( i .eq. 0 ) then
        x(m) = 1
        return
      else if ( i .eq. 1 ) then
        t = x(1) + 1
        im1 = m
      else if ( 1 .lt. i ) then
        t = x(i)
        im1 = i - 1
      end if

      x(i) = 0
      x(im1) = x(im1) + 1
      x(m) = x(m) + t - 1

      return
      end
      subroutine mono_rank_grlex ( m, x, rank )

c*********************************************************************72
c
cc MONO_RANK_GRLEX computes the graded lexicographic rank of a monomial.
c
c  Discussion:
c
c    The graded lexicographic ordering is used, over all monomials of
c    dimension D, with degree NM = 0, then 1, then 2, ...
c
c    For example, if M = 3, the ranking begins:
c
c    Rank  Sum    1  2  3
c    ----  ---   -- -- --
c       1    0    0  0  0
c
c       2    1    0  0  1
c       3    1    0  1  0
c       4    1    1  0  1
c
c       5    2    0  0  2
c       6    2    0  1  1
c       7    2    0  2  0
c       8    2    1  0  1
c       9    2    1  1  0
c      10    2    2  0  0
c
c      11    3    0  0  3
c      12    3    0  1  2
c      13    3    0  2  1
c      14    3    0  3  0
c      15    3    1  0  2
c      16    3    1  1  1
c      17    3    1  2  0
c      18    3    2  0  1
c      19    3    2  1  0
c      20    3    3  0  0
c
c      21    4    0  0  4
c      ..   ..   .. .. ..
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 <= M.
c
c    Input, integer X(M), the composition.
c    For each 1 <= I <= M, we have 0 <= X(I).
c
c    Output, integer RANK, the rank of the composition.
c
      implicit none

      integer m

      integer i
      integer i4_choose
      integer i4vec_sum
      integer j
      integer ks
      integer n
      integer nm
      integer ns
      integer rank
      integer tim1
      integer x(m)
      integer xs(m-1)
c
c  Ensure that 1 <= M.
c
      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1'
        stop 1
      end if
c
c  Ensure that 0 <= X(I).
c
      do i = 1, m
        if ( x(i) .lt. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
          write ( *, '(a)' ) '  X(I) .lt. 0'
          stop 1
        end if
      end do
c
c  NM = sum ( X )
c
      nm = i4vec_sum ( m, x )
c
c  Convert to KSUBSET format.
c
      ns = nm + m - 1
      ks = m - 1

      xs(1) = x(1) + 1
      do i = 2, ks
        xs(i) = xs(i-1) + x(i) + 1
      end do
c
c  Compute the rank.
c
      rank = 1

      do i = 1, ks

        if ( i == 1 ) then
          tim1 = 0
        else
          tim1 = xs(i-1)
        end if

        if ( tim1 + 1 .le. xs(i) - 1 ) then
          do j = tim1 + 1, xs(i) - 1
            rank = rank + i4_choose ( ns - j, ks - i )
          end do
        end if

      end do

      do n = 0, nm - 1
        rank = rank + i4_choose ( n + m - 1, n )
      end do

      return
      end
      subroutine mono_total_next_grlex ( m, n, x )

c*********************************************************************72
c
cc MONO_TOTAL_NEXT_GRLEX: grlex next monomial with total degree equal to N.
c
c  Discussion:
c
c    We consider all monomials in a M dimensional space, with total degree N.
c
c    For example:
c
c    M = 3
c    N = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     3        3
c    2 |  0     1     2        3
c    3 |  0     2     1        3
c    4 |  0     3     0        3
c    5 |  1     0     2        3
c    6 |  1     1     1        3
c    7 |  1     2     0        3
c    8 |  2     0     1        3
c    9 |  2     1     0        3
c   10 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the degree.
c    0 <= N.
c
c    Input/output, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, N ].
c    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer im1
      integer j
      integer n
      integer t
      integer x(m)

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .ne. n ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X does not sum to N.'
        stop 1
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n ) then
        x(1) = 0
        x(m) = n
      else
        call mono_next_grlex ( m, x )
      end if

      return
      end
      subroutine mono_unrank_grlex ( m, rank, x )

c*********************************************************************72
c
cc MONO_UNRANK_GRLEX computes the monomial of given grlex rank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 <= D.
c
c    Input, integer RANK, the rank of the monomial.
c    1 <= RANK.
c
c    Output, integer X(M), the composition of the given rank.
c    For each I, 0 <= X(I) <= NM, and 
c    sum ( 1 <= I <= M ) X(I) = NM.
c
      implicit none

      integer m

      integer i
      integer i4_choose
      integer j
      integer ks
      integer nksub
      integer nm
      integer ns
      integer r
      integer rank
      integer rank1
      integer rank2
      integer x(m)
      integer xs(m-1)
c
c  Ensure that 1 <= M.
c
      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1'
        stop 1
      end if
c
c  Ensure that 1 <= RANK.
c
      if ( rank .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  RANK .lt. 1'
        stop 1
      end if
c
c  Special case M == 1.
c
      if ( m .eq. 1 ) then
        x(1) = rank - 1
        return
      end if
c
c  Determine the appropriate value of NM.
c  Do this by adding up the number of compositions of sum 0, 1, 2, 
c  ..., without exceeding RANK.  Moreover, RANK - this sum essentially
c  gives you the rank of the composition within the set of compositions
c  of sum NM.  And that's the number you need in order to do the
c  unranking.
c
      rank1 = 1
      nm = -1

10    continue

        nm = nm + 1
        r = i4_choose ( nm + m - 1, nm )
        if ( rank .lt. rank1 + r ) then
          go to 20
        end if
        rank1 = rank1 + r
      go to 10

20    continue

      rank2 = rank - rank1
c
c  Convert to KSUBSET format.
c  Apology: an unranking algorithm was available for KSUBSETS,
c  but not immediately for compositions.  One day we will come back
c  and simplify all this.
c
      ks = m - 1
      ns = nm + m - 1

      nksub = i4_choose ( ns, ks )

      j = 1

      do i = 1, ks

        r = i4_choose ( ns - j, ks - i )

30      continue

        if ( r <= rank2 .and. 0 .lt. r ) then
          rank2 = rank2 - r
          j = j + 1
          r = i4_choose ( ns - j, ks - i )
          go to 30
        end if

        xs(i) = j
        j = j + 1

      end do
c
c  Convert from KSUBSET format to COMP format.
c
      x(1) = xs(1) - 1
      do i = 2, m - 1
        x(i) = xs(i) - xs(i-1) - 1
      end do
      x(m) = ns - xs(ks)

      return
      end
      function mono_upto_enum ( m, n )

c*********************************************************************72
c
cc MONO_UPTO_ENUM enumerates monomials in M dimensions of degree up to N.
c
c  Discussion:
c
c    For M = 2, we have the following values:
c
c    N  VALUE
c
c    0    1
c    1    3
c    2    6
c    3   10
c    4   15
c    5   21
c
c    In particular, VALUE(2,3) = 10 because we have the 10 monomials:
c
c      1, x, y, x^2, xy, y^2, x^3, x^2y, xy^2, y^3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the maximum degree.
c
c    Output, integer MONO_UPTO_ENUM, the number of monomials in
c    M variables, of total degree N or less.
c
      implicit none

      integer m
      integer i4_choose
      integer mono_upto_enum
      integer n
      integer value

      value = i4_choose ( n + m, n )

      mono_upto_enum = value

      return
      end
      subroutine mono_value ( m, n, f, x, v )

c*********************************************************************72
c
cc MONO_VALUE evaluates a monomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, integer F(M), the exponents of the monomial.
c
c    Input, double precision X(M,N), the coordinates of the evaluation points.
c
c    Output, double precision V(N), the value of the monomial at X.
c
      implicit none

      integer m
      integer n

      integer f(m)
      integer i
      integer j
      double precision v(n)
      double precision x(m,n)
  
      do j = 1, n
        v(j) = 1.0D+00
        do i = 1, m
          v(j) = v(j) * x(i,j) ** f(i)
        end do
      end do

      return
      end
      subroutine perm_check0 ( n, p )

c*********************************************************************72
c
cc PERM_CHECK0 checks a 0-based permutation.
c
c  Discussion:
c
c    The routine verifies that each of the integers from 0 to
c    to N-1 occurs among the N entries of the permutation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries.
c
c    Input, integer P(N), the array to check.
c
      implicit none

      integer n

      integer ierror
      integer location
      integer p(n)
      integer value

      do value = 0, n - 1

        ierror = 1

        do location = 1, n
          if ( p(location) .eq. value ) then
            ierror = 0
            go to 10
          end if
        end do

10      continue

        if ( ierror .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'PERM_CHECK0 - Fatal error!'
          write ( *, '(a,i4)' ) '  Permutation is missing value ', value
          stop 1
        end if

      end do

      return
      end
      subroutine perm_check1 ( n, p )

c*********************************************************************72
c
cc PERM_CHECK1 checks a 1-based permutation.
c
c  Discussion:
c
c    The routine verifies that each of the integers from 1 to
c    to N occurs among the N entries of the permutation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries.
c
c    Input, integer P(N), the array to check.
c
      implicit none

      integer n

      integer ierror
      integer location
      integer p(n)
      integer value

      do value = 1, n

        ierror = 1

        do location = 1, n
          if ( p(location) .eq. value ) then
            ierror = 0
            go to 10
          end if
        end do

10      continue

        if ( ierror .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'PERM_CHECK1 - Fatal error!'
          write ( *, '(a,i4)' ) '  Permutation is missing value ', value
          stop 1
        end if

      end do

      return
      end
      subroutine polynomial_add ( o1, c1, e1, o2, c2, e2, o, c, e )

c*********************************************************************72
c
cc POLYNOMIAL_ADD adds two polynomials.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer O1, the "order" of polynomial 1.
c
c    Input, double precision C1(O1), the coefficients of polynomial 1.
c
c    Input, integer E1(O1), the indices of the exponents of
c    polynomial 1.
c
c    Input, integer O2, the "order" of polynomial 2.
c
c    Input, double precision C2(O2), the coefficients of polynomial 2.
c
c    Input, integer E2(O2), the indices of the exponents of
c    polynomial 2.
c
c    Output, integer O, the "order" of the polynomial sum.
c
c    Output, double precision C(O), the coefficients of the polynomial sum.
c
c    Output, integer E(O), the indices of the exponents of
c    the polynomial sum.
c
      implicit none

      integer o1
      integer o2

      double precision c(o1+o2)
      double precision c1(o1)
      double precision c2(o2)
      integer e(o1+o2)
      integer e1(o1)
      integer e2(o2)
      integer o

      o = o1 + o2
      call r8vec_concatenate ( o1, c1, o2, c2, c )
      call i4vec_concatenate ( o1, e1, o2, e2, e )

      call polynomial_sort ( o, c, e )
      call polynomial_compress ( o, c, e, o, c, e )

      return
      end
      subroutine polynomial_axpy ( s, o1, c1, e1, o2, c2, e2, o, c, e )

c*********************************************************************72
c
cc POLYNOMIAL_AXPY adds a multiple of one polynomial to another.
c
c  Discussion:
c
c    P(X) = P2(X) + S * P1(X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision S, the multiplier for the first polynomial.
c
c    Input, integer O1, the "order" of polynomial 1.
c
c    Input, double precision C1(O1), the coefficients of polynomial 1.
c
c    Input, integer E1(O1), the indices of the exponents of
c    polynomial 1.
c
c    Input, integer O2, the "order" of polynomial 2.
c
c    Input, double precision C2(O2), the coefficients of polynomial 2.
c
c    Input, integer E2(O2), the indices of the exponents of
c    polynomial 2.
c
c    Output, integer O, the "order" of the polynomial sum.
c
c    Output, double precision C(O), the coefficients of the polynomial sum.
c
c    Output, integer E(O), the indices of the exponents of
c    the polynomial sum.
c
      implicit none

      integer o1
      integer o2

      double precision c(*)
      double precision c1(o1)
      double precision c2(o2)
      double precision c3(o1+o2)
      integer e(*)
      integer e1(o1)
      integer e2(o2)
      integer e3(o1+o2)
      integer i
      integer o
      integer o3
      double precision s
      double precision sc1(o1)

      o3 = o1 + o2
      do i = 1, o1
        sc1(i) = s * c1(i)
      end do
      call r8vec_concatenate ( o1, sc1, o2, c2, c3 )
      call i4vec_concatenate ( o1, e1, o2, e2, e3 )

      call polynomial_sort ( o3, c3, e3 )
      call polynomial_compress ( o3, c3, e3, o, c, e )

      return
      end
      subroutine polynomial_compress ( o1, c1, e1, o2, c2, e2 )

c*********************************************************************72
c
cc POLYNOMIAL_COMPRESS compresses a polynomial.
c
c  Discussion:
c
c    The function polynomial_sort() must be called first, or else
c    the E1 vector should be in ascending sorted order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer O1, the "order" of the polynomial.
c
c    Input, double precision C1(O1), the coefficients of the polynomial.
c
c    Input, integer E1(O1), the indices of the exponents of
c    the polynomial.
c
c    Output, integer O2, the "order" of the polynomial.
c
c    Output, double precision C2(O2), the coefficients of the polynomial.
c
c    Output, integer E2(O2), the indices of the exponents of
c    the polynomial.
c
      implicit none

      integer o1
      integer o2

      double precision c1(o1)
      double precision c2(o2)
      integer e1(o1)
      integer e2(o2)
      integer get
      integer put
      double precision r8_epsilon_sqrt
      parameter ( r8_epsilon_sqrt = 0.1490116119384766D-07 )
c
c  Add coefficients associated with the same exponent.
c
      get = 0
      put = 0

10    continue

      if ( get .lt. o1 ) then

        get = get + 1

        if ( 0 .eq. put ) then

          put = put + 1
          c2(put) = c1(get)
          e2(put) = e1(get)

        else

          if ( e2(put) .eq. e1(get) ) then
            c2(put) = c2(put) + c1(get)
          else
            put = put + 1
            c2(put) = c1(get)
            e2(put) = e1(get)
          end if

        end if

        go to 10

      end if

      o2 = put
c
c  Clear out zeros and tiny coefficients.
c
      get = 0
      put = 0

20    continue

      if ( get .lt. o2 ) then

        get = get + 1

        if ( r8_epsilon_sqrt < abs ( c2(get) ) ) then
          put = put + 1
          c2(put) = c2(get)
          e2(put) = e2(get)
        end if

        go to 20

      end if

      o2 = put

      return
      end
      subroutine polynomial_dif ( m, o1, c1, e1, dif, o2, c2, e2 )

c*********************************************************************72
c
cc POLYNOMIAL_DIF differentiates a polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer O1, the "order" of polynomial 1.
c
c    Input, double precision C1(O1), the coefficients of polynomial 1.
c
c    Input, integer E1(O1), the indices of the exponents of
c    polynomial 1.
c
c    Input, integer DIF(M), indicates the number of
c    differentiations in each component.
c
c    Output, integer O2, the "order" of the polynomial derivative.
c
c    Output, double precision C2(O2), the coefficients of the polynomial
c    derivative.
c
c    Output, integer E2(O2), the indices of the exponents of the
c    polynomial derivative.
c
      implicit none

      integer m
      integer o1

      double precision c1(o1)
      double precision c2(o1)
      integer dif(m)
      integer e1(o1)
      integer e2(o1)
      integer f1(m)
      integer i
      integer i4_fall
      integer j
      integer o2

      o2 = o1
      do j = 1, o1
        c2(j) = c1(j)
      end do

      do j = 1, o1
        call mono_unrank_grlex ( m, e1(j), f1 )
        do i = 1, m
          c2(j) = c2(j) * i4_fall ( f1(i), dif(i) )
          f1(i) = max ( f1(i) - dif(i), 0 )
        end do
        call mono_rank_grlex ( m, f1, e2(j) )
      end do

      o2 = o1
      call polynomial_sort ( o2, c2, e2 )

      call polynomial_compress ( o2, c2, e2, o2, c2, e2 )

      return
      end
      subroutine polynomial_mul ( m, o1, c1, e1, o2, c2, e2, o, c, e )

c*********************************************************************72
c
cc POLYNOMIAL_MUL multiplies two polynomials.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer O1, the "order" of polynomial 1.
c
c    Input, double precision C1(O1), the coefficients of polynomial 1.
c
c    Input, integer E1(O1), the indices of the exponents of
c    polynomial 1.
c
c    Input, integer O2, the "order" of polynomial 2.
c
c    Input, double precision C2(O2), the coefficients of polynomial 2.
c
c    Input, integer E2(O2), the indices of the exponents of
c    polynomial 2.
c
c    Output, integer O, the "order" of the polynomial product.
c
c    Output, double precision C(O), the coefficients of the polynomial product.
c
c    Output, integer E(O), the indices of the exponents of the
c    polynomial product.
c
      implicit none

      integer m
      integer o1
      integer o2

      double precision c(o1*o2)
      double precision c1(o1)
      double precision c2(o2)
      integer e(o1*o2)
      integer e1(o1)
      integer e2(o2)
      integer f(m)
      integer f1(m)
      integer f2(m)
      integer i
      integer j
      integer k
      integer o

      o = 0
      do j = 1, o2
        do i = 1, o1
          o = o + 1
          c(o) = c1(i) * c2(j)
          call mono_unrank_grlex ( m, e1(i), f1 )
          call mono_unrank_grlex ( m, e2(j), f2 )
          do k = 1, m
            f(k) = f1(k) + f2(k)
          end do
          call mono_rank_grlex ( m, f, e(o) )
        end do
      end do

      call polynomial_sort ( o, c, e )
      call polynomial_compress ( o, c, e, o, c, e )

      return
      end
      subroutine polynomial_print ( m, o, c, e, title )

c*********************************************************************72
c
cc POLYNOMIAL_PRINT prints a polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer O, the "order" of the polynomial, that is,
c    simply the number of terms.
c
c    Input, double precision C(O), the coefficients.
c
c    Input, integer E(O), the indices of the exponents.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer o

      double precision c(o)
      integer e(o)
      integer f(m)
      integer i
      integer j
      character * ( * ) title

      write ( *, '(a)' ) trim ( title )

      if ( o .eq. 0 ) then
        write ( *, '(a)' ) '      0.'
      else
        do j = 1, o
          write (  *, '(a)', advance = 'no' ) '    '
          if ( c(j) .lt. 0.0D+00 ) then
            write (  *, '(a)', advance = 'no' ) '- '
          else
            write (  *, '(a)', advance = 'no' ) '+ '
          end if
          write ( *, '(g14.6,a)', advance = 'no' ) 
     &      abs ( c(j) ), ' * x^('
          call mono_unrank_grlex ( m, e(j), f )
          do i = 1, m
            write ( *, '(i2)', advance = 'no' ) f(i)
            if ( i .lt. m ) then
              write (  *, '(a)', advance = 'no' ) ','
            else
              write (  *, '(a)', advance = 'no' ) ')'
            end if
          end do
          if ( j .eq. o ) then
            write (  *, '(a)', advance = 'no' ) '.'
          end if
          write (  *, '(a)' ) ''
        end do
      end if

      return
      end
      subroutine polynomial_scale ( s, m, o, c, e )

c*********************************************************************72
c
cc POLYNOMIAL_SCALE scales a polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision S, the scale factor.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer O, the "order" of the polynomial.
c
c    Input/output, double precision C(O), the coefficients of the polynomial.
c
c    Input, integer E(O), the indices of the exponents of
c    the polynomial.
c
      implicit none

      integer o

      double precision c(o)
      integer m
      integer e(o)
      integer i
      double precision s

      do i = 1, o
        c(i) = c(i) * s
      end do

      return
      end
      subroutine polynomial_sort ( o, c, e )

c*********************************************************************72
c
cc POLYNOMIAL_SORT sorts the information in a polynomial.
c
c  Discussion
c
c    The coefficients C and exponents E are rearranged so that
c    the elements of E are in ascending order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer O, the "order" of the polynomial.
c
c    Input/output, double precision C(O), the coefficients of the polynomial.
c
c    Input/output, integer E(O), the indices of the exponents of
c    the polynomial.
c
      implicit none

      integer o

      double precision c(o)
      integer e(o)
      integer indx(o)

      call i4vec_sort_heap_index_a ( o, e, indx )

      call i4vec_permute ( o, indx, e )
      call r8vec_permute ( o, indx, c )

      return
      end
      subroutine polynomial_value ( m, o, c, e, n, x, p )

c*********************************************************************72
c
cc POLYNOMIAL_VALUE evaluates a polynomial.
c
c  Discussion:
c
c    The polynomial is evaluated term by term, and no attempt is made to
c    use an approach such as Horner's method to speed up the process.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer O, the "order" of the polynomial.
c
c    Input, double precision C(O), the coefficients of the polynomial.
c
c    Input, integer E(O), the indices of the exponents 
c    of the polynomial.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(D,NX), the coordinates of the evaluation points.
c
c    Output, double precision P(NX), the value of the polynomial at X.
c
      implicit none

      integer m
      integer n
      integer o

      double precision c(o)
      integer e(o)
      integer f(m)
      integer j
      integer k
      double precision p(n)
      double precision v(n)
      double precision x(m,n)

      do k = 1, n
        p(k) = 0.0D+00
      end do

      do j = 1, o
        call mono_unrank_grlex ( m, e(j), f )
        call mono_value ( m, n, f, x, v )
        do k = 1, n
          p(k) = p(k) + c(j) * v(k)
        end do
      end do

      return
      end
      subroutine r8vec_concatenate ( n1, a, n2, b, c )

c*********************************************************************72
c
cc R8VEC_CONCATENATE concatenates two R8VEC's.
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
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, the number of entries in the first vector.
c
c    Input, double precision A(N1), the first vector.
c
c    Input, integer N2, the number of entries in the second vector.
c
c    Input, double precision B(N2), the second vector.
c
c    Output, double precision C(N1+N2), the concatenation of A and B.
c
      implicit none

      integer n1
      integer n2

      double precision a(n1)
      double precision b(n2)
      double precision c(n1+n2)
      integer i

      do i = 1, n1
        c(i) = a(i)
      end do

      do i = 1, n2
        c(n1+i) = b(i)
      end do

      return
      end
      subroutine r8vec_permute ( n, p, a )

c*********************************************************************72
c
cc R8VEC_PERMUTE permutes an R8VEC in place.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    This routine permutes an array of real "objects", but the same
c    logic can be used to permute an array of objects of any arithmetic
c    type, or an array of objects of any complexity.  The only temporary
c    storage required is enough to store a single object.  The number
c    of data movements made is N + the number of cycles of order 2 or more,
c    which is never more than N + N/2.
c
c    P(I) = J means that the I-th element of the output array should be
c    the J-th element of the input array.  P must be a legal permutation
c    of the integers from 1 to N, otherwise the algorithm will
c    fail catastrophically.
c
c  Example:
c
c    Input:
c
c      N = 5
c      P = (   2,   4,   5,   1,   3 )
c      A = ( 1.0, 2.0, 3.0, 4.0, 5.0 )
c
c    Output:
c
c      A    = ( 2.0, 4.0, 5.0, 1.0, 3.0 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of objects.
c
c    Input, integer P(N), the permutation.
c
c    Input/output, double precision A(N), the array to be permuted.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_temp
      integer ierror
      integer iget
      integer iput
      integer istart
      integer p(n)

      call perm_check1 ( n, p )
c
c  Search for the next element of the permutation that has not been used.
c
      do istart = 1, n

        if ( p(istart) .lt. 0 ) then

          go to 20

        else if ( p(istart) .eq. istart ) then

          p(istart) = - p(istart)
          go to 20

        else

          a_temp = a(istart)
          iget = istart
c
c  Copy the new value into the vacated entry.
c
10        continue

            iput = iget
            iget = p(iget)

            p(iput) = - p(iput)

            if ( iget .lt. 1 .or. n .lt. iget ) then
             write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
              write ( *, '(a)' ) '  An index is out of range.'
              write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
              stop 1
            end if

            if ( iget .eq. istart ) then
              a(iput) = a_temp
              go to 20
            end if

            a(iput) = a(iget)

          go to 10

        end if

20      continue

      end do
c
c  Restore the signs of the entries.
c
      p(1:n) = - p(1:n)

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
     &  m, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
