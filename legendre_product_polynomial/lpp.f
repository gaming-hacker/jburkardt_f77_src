      subroutine comp_enum ( n, k, number )

c*********************************************************************72
c
cc COMP_ENUM returns the number of compositions of the integer N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to N.  The compositions (1,2,1)
c    and (1,1,2) are considered to be distinct.
c
c    The 28 compositions of 6 into three parts are:
c
c      6 0 0,  5 1 0,  5 0 1,  4 2 0,  4 1 1,  4 0 2,
c      3 3 0,  3 2 1,  3 1 2,  3 0 3,  2 4 0,  2 3 1,
c      2 2 2,  2 1 3,  2 0 4,  1 5 0,  1 4 1,  1 3 2,
c      1 2 3,  1 1 4,  1 0 5,  0 6 0,  0 5 1,  0 4 2,
c      0 3 3,  0 2 4,  0 1 5,  0 0 6.
c
c    The formula for the number of compositions of N into K parts is
c
c      Number = ( N + K - 1 )! / ( N! * ( K - 1 )! )
c
c    (Describe the composition using N '1's and K-1 dividing lines '|'.
c    The number of distinct permutations of these symbols is the number
c    of compositions.  This is equal to the number of permutations of 
c    N+K-1 things, with N identical of one kind and K-1 identical of another.)
c
c    Thus, for the above example, we have:
c
c      Number = ( 6 + 3 - 1 )! / ( 6! * (3-1)! ) = 28
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2007
c
c  Author:
c
c    John Burkardt
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
c    Input, integer N, the integer whose compositions are desired.
c
c    Input, integer K, the number of parts in the composition.
c
c    Output, integer NUMBER, the number of compositions of N into K parts.
c
      implicit none

      integer i4_choose
      integer k
      integer n
      integer number

      number = i4_choose ( n + k - 1, n )

      return
      end
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
      subroutine comp_random_grlex ( kc, rank1, rank2, seed, xc, rank )

c*********************************************************************72
c
cc COMP_RANDOM_GRLEX: random composition with degree less than or equal to NC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer KC, the number of parts in the composition.
c
c    Input, integer RANK1, RANK2, the minimum and maximum ranks.
c    1 <= RANK1 <= RANK2.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, integer XC(KC), the random composition.
c
c    Output, integer RANK, the rank of the composition.
c
      implicit none

      integer kc

      integer i4_uniform_ab
      integer nc
      integer rank
      integer rank1
      integer rank2
      integer seed
      integer xc(kc)
c
c  Ensure that 1 <= KC.
c
      if ( kc .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COMP_RANDOM_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  KC < 1'
        stop 1
      end if
c
c  Ensure that 1 <= RANK1.
c
      if ( rank1 .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COMP_RANDOM_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  RANK1 < 1'
        stop 1
      end if
c
c  Ensure that RANK1 <= RANK2.
c
      if ( rank2 .lt. rank1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COMP_RANDOM_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  RANK2 < RANK1'
        stop 1
      end if
c
c  Select the rank.
c
      rank = i4_uniform_ab ( rank1, rank2, seed )
c
c  Recover the corresponding composition.
c
      call comp_unrank_grlex ( kc, rank, xc )

      return
      end
      subroutine comp_rank_grlex ( kc, xc, rank )

c*********************************************************************72
c
cc COMP_RANK_GRLEX computes the graded lexicographic rank of a composition.
c
c  Discussion:
c
c    The graded lexicographic ordering is used, over all KC-compositions
c    for NC = 0, 1, 2, ...
c
c    For example, if KC = 3, the ranking begins:
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
c    11 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer KC, the number of parts in the composition.
c    1 <= KC.
c
c    Input, integer XC(KC), the composition.
c    For each 1 <= I <= KC, we have 0 <= XC(I).
c
c    Output, integer RANK, the rank of the composition.
c
      implicit none

      integer kc

      integer i
      integer i4_choose
      integer i4vec_sum
      integer j
      integer ks
      integer n
      integer nc
      integer ns
      integer rank
      integer tim1
      integer xc(kc)
      integer xs(kc-1)
c
c  Ensure that 1 <= KC.
c
      if ( kc .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COMP_RANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  KC .lt. 1'
        stop 1
      end if
c
c  Ensure that 0 <= XC(I).
c
      do i = 1, kc
        if ( xc(i) .lt. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'COMP_RANK_GRLEX - Fatal error!'
          write ( *, '(a)' ) '  XC(I) .lt. 0'
          stop 1
        end if
      end do
c
c  NC = sum ( XC )
c
      nc = i4vec_sum ( kc, xc )
c
c  Convert to KSUBSET format.
c
      ns = nc + kc - 1
      ks = kc - 1

      xs(1) = xc(1) + 1
      do i = 2, kc - 1
        xs(i) = xs(i-1) + xc(i) + 1
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

      do n = 0, nc - 1
        rank = rank + i4_choose ( n + kc - 1, n )
      end do

      return
      end
      subroutine comp_unrank_grlex ( kc, rank, xc )

c*********************************************************************72
c
cc COMP_UNRANK_GRLEX computes the composition of given grlex rank.
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
c    Input, integer RANK, the rank of the composition.
c    1 <= RANK.
c
c    Output, integer XC(KC), the composition of the given rank.
c    For each I, 0 <= XC(I) <= NC, and 
c    sum ( 1 <= I <= KC ) XC(I) = NC.
c
      implicit none

      integer kc

      integer i
      integer i4_choose
      integer j
      integer ks
      integer nc
      integer nksub
      integer ns
      integer r
      integer rank
      integer rank1
      integer rank2
      integer xc(kc)
      integer xs(kc-1)
c
c  Ensure that 1 <= KC.
c
      if ( kc .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COMP_UNRANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  KC .lt. 1'
        stop 1
      end if
c
c  Ensure that 1 <= RANK.
c
      if ( rank .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COMP_UNRANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  RANK .lt. 1'
        stop 1
      end if
c
c  Determine the appropriate value of NC.
c  Do this by adding up the number of compositions of sum 0, 1, 2, 
c  ..., without exceeding RANK.  Moreover, RANK - this sum essentially
c  gives you the rank of the composition within the set of compositions
c  of sum NC.  And that's the number you need in order to do the
c  unranking.
c
      rank1 = 1
      nc = -1

10    continue

        nc = nc + 1
        r = i4_choose ( nc + kc - 1, nc )
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
      ks = kc - 1
      ns = nc + kc - 1

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
      xc(1) = xs(1) - 1
      do i = 2, kc - 1
        xc(i) = xs(i) - xs(i-1) - 1
      end do
      xc(kc) = ns - xs(ks)

      return
      end
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
        stop 1
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
c    01 December 2013
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
      subroutine i4vec_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_PRINT prints an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i12)' ) i, ':', a(i)
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
c    An I4VEC is a vector of integer values.
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
      subroutine i4vec_uniform_ab ( n, a, b, seed, x )

c*********************************************************************72
c
cc I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    The pseudorandom numbers should be uniformly distributed
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
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer X(N), a vector of numbers between A and B.
c
      implicit none

      integer n

      integer a
      integer b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      real r
      integer seed
      integer value
      integer x(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

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
     &    +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
        value = nint ( r )

        value = max ( value, min ( a, b ) )
        value = min ( value, max ( a, b ) )

        x(i) = value

      end do

      return
      end
      subroutine lp_coefficients ( n, o, c, f )

c*********************************************************************72
c
cc LP_COEFFICIENTS: coefficients of Legendre polynomials P(n,x).
c
c  Discussion:
c
c    The Legendre polynomial with index N will have O = 1 + (N/2) terms.
c    The monomials of orders N, N-2, N-2, ... will have nonzero coefficients.
c
c  First terms:
c
c     1
c     0     1
c    -1/2   0      3/2
c     0    -3/2    0     5/2
c     3/8   0    -30/8   0     35/8
c     0    15/8    0   -70/8    0     63/8
c    -5/16  0    105/16  0   -315/16   0    231/16
c     0   -35/16   0   315/16   0   -693/16   0    429/16
c
c     1.00000
c     0.00000  1.00000
c    -0.50000  0.00000  1.50000
c     0.00000 -1.50000  0.00000  2.5000
c     0.37500  0.00000 -3.75000  0.00000  4.37500
c     0.00000  1.87500  0.00000 -8.75000  0.00000  7.87500
c    -0.31250  0.00000  6.56250  0.00000 -19.6875  0.00000  14.4375
c     0.00000 -2.1875   0.00000  19.6875  0.00000 -43.3215  0.00000  26.8125
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 February 2014
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Output, integer O, the number of coefficients.
c
c    Output, double precision C((N+2)/2), the coefficients of the Legendre
c    polynomial of degree N.
c
c    Output, integer F((N+2)/2), the exponents.
c
      implicit none

      integer n

      double precision c((n+2)/2)
      double precision ctable(0:n,0:n)
      integer f((n+2)/2)
      integer i
      integer j
      integer k
      integer o

      do j = 0, n
        do i = 0, n
         ctable(i,j) = 0.0D+00
        end do
      end do

      ctable(0,0) = 1.0D+00

      if ( 0 .lt. n ) then

        ctable(1,1) = 1.0D+00

        do i = 2, n

          do j = 0, i - 2
            ctable(i,j) =
     &          dble (   - i + 1 ) * ctable(i-2,j)
     &        / dble (     i     )
          end do

          do j = 1, i
            ctable(i,j) = ctable(i,j)
     &        + dble ( i + i - 1 ) * ctable(i-1,j-1)
     &        / dble (     i     )
          end do

        end do

      end if
c
c  Extract the nonzero data from the alternating columns of the last row.
c
      o = ( n + 2 ) / 2

      k = o
      do j = n, 0, -2
        c(k) = ctable(n,j)
        f(k) = j
        k = k - 1
      end do

      return
      end
      subroutine lp_value ( n, o, x, v )

c*********************************************************************72
c
cc LP_VALUE evaluates the Legendre polynomials P(n,x).
c
c  Discussion:
c
c    P(n,1) = 1.
c    P(n,-1) = (-1)^N.
c    | P(n,x) | <= 1 in [-1,1].
c
c    The N zeroes of P(n,x) are the abscissas used for Gauss-Legendre
c    quadrature of the integral of a function F(X) with weight function 1
c    over the interval [-1,1].
c
c    The Legendre polynomials are orthogonal under the inner product defined
c    as integration from -1 to 1:
c
c      Integral ( -1 <= X <= 1 ) P(I,X) * P(J,X) dX
c        = 0 if I =/= J
c        = 2 / ( 2*I+1 ) if I = J.
c
c    Except for P(0,X), the integral of P(I,X) from -1 to 1 is 0.
c
c    A function F(X) defined on [-1,1] may be approximated by the series
c      C0*P(0,x) + C1*P(1,x) + ... + CN*P(n,x)
c    where
c      C(I) = (2*I+1)/(2) * Integral ( -1 <= X <= 1 ) F(X) P(I,x) dx.
c
c    The formula is:
c
c      P(n,x) = (1/2^N) * sum ( 0 <= M <= N/2 ) C(N,M) C(2N-2M,N) X^(N-2*M)
c
c  Differential equation:
c
c    (1-X*X) * P(n,x)'' - 2 * X * P(n,x)' + N * (N+1) = 0
c
c  First terms:
c
c    P( 0,x) =      1
c    P( 1,x) =      1 X
c    P( 2,x) = (    3 X^2 -       1)/2
c    P( 3,x) = (    5 X^3 -     3 X)/2
c    P( 4,x) = (   35 X^4 -    30 X^2 +     3)/8
c    P( 5,x) = (   63 X^5 -    70 X^3 +    15 X)/8
c    P( 6,x) = (  231 X^6 -   315 X^4 +   105 X^2 -     5)/16
c    P( 7,x) = (  429 X^7 -   693 X^5 +   315 X^3 -    35 X)/16
c    P( 8,x) = ( 6435 X^8 - 12012 X^6 +  6930 X^4 -  1260 X^2 +   35)/128
c    P( 9,x) = (12155 X^9 - 25740 X^7 + 18018 X^5 -  4620 X^3 +  315 X)/128
c    P(10,x) = (46189 X^10-109395 X^8 + 90090 X^6 - 30030 X^4 + 3465 X^2-63)/256
c
c  Recursion:
c
c    P(0,x) = 1
c    P(1,x) = x
c    P(n,x) = ( (2*n-1)*x*P(n-1,x)-(n-1)*P(n-2,x) ) / n
c
c    P'(0,x) = 0
c    P'(1,x) = 1
c    P'(N,x) = ( (2*N-1)*(P(N-1,x)+X*P'(N-1,x)-(N-1)*P'(N-2,x) ) / N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2012
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
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer N, the number of evaluation points.
c
c    Input, integer O, the degree of the polynomial.
c
c    Input, double precision X(N), the evaluation points.
c
c    Output, double precision V(N), the values of the Legendre polynomials
c    of order O at the points X.
c
      implicit none

      integer n
      integer o

      integer i
      integer j
      double precision v(n)
      double precision vtable(n,0:o)
      double precision x(n)

      do i = 1, n
        vtable(i,0) = 1.0D+00
      end do

      if ( 1 .le. o ) then

        do i = 1, n

          vtable(i,1) = x(i)

            do j = 2, o

              vtable(i,j) =
     &          ( dble ( 2 * j - 1 ) * x(i) * vtable(i,j-1)
     &          - dble (     j - 1 ) *        vtable(i,j-2) )
     &          / dble (     j     )

          end do

        end do

      end if

      do i = 1, n
        v(i) = vtable(i,o)
      end do

      return
      end
      subroutine lp_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc LP_VALUES returns values of the Legendre polynomials P(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2012
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
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 22 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   0.1000000000000000D+01,
     &   0.2500000000000000D+00,
     &  -0.4062500000000000D+00,
     &  -0.3359375000000000D+00,
     &   0.1577148437500000D+00,
     &   0.3397216796875000D+00,
     &   0.2427673339843750D-01,
     &  -0.2799186706542969D+00,
     &  -0.1524540185928345D+00,
     &   0.1768244206905365D+00,
     &   0.2212002165615559D+00,
     &   0.0000000000000000D+00,
     &  -0.1475000000000000D+00,
     &  -0.2800000000000000D+00,
     &  -0.3825000000000000D+00,
     &  -0.4400000000000000D+00,
     &  -0.4375000000000000D+00,
     &  -0.3600000000000000D+00,
     &  -0.1925000000000000D+00,
     &   0.8000000000000000D-01,
     &   0.4725000000000000D+00,
     &   0.1000000000000000D+01 /
      data n_vec /
     &   0,  1,  2,
     &   3,  4,  5,
     &   6,  7,  8,
     &   9, 10,  3,
     &   3,  3,  3,
     &   3,  3,  3,
     &   3,  3,  3,
     &   3 /
      data x_vec /
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.25D+00,
     &  0.00D+00,
     &  0.10D+00,
     &  0.20D+00,
     &  0.30D+00,
     &  0.40D+00,
     &  0.50D+00,
     &  0.60D+00,
     &  0.70D+00,
     &  0.80D+00,
     &  0.90D+00,
     &  1.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine lpp_to_polynomial ( m, l, o_max, o, c, e )

c*********************************************************************72
c
cc LPP_TO_POLYNOMIAL writes a Legendre Product Polynomial as a polynomial.
c
c  Discussion:
c
c    For example, if
c      M = 3,
c      L = ( 1, 0, 2 ),
c    then
c      L(1,0,2)(X,Y,Z)
c      = L(1)(X) * L(0)(Y) * L(2)(Z)
c      = X * 1 * ( 3Z^2-1)/2
c      = - 1/2 X + (3/2) X Z^2
c    so
c      O = 2 (2 nonzero terms)
c      C = -0.5
c           1.5
c      E = 4    <-- index in 3-space of exponent (1,0,0)
c          15   <-- index in 3-space of exponent (1,0,2)
c
c    The output value of O is no greater than
c      O_MAX = product ( 1 <= I <= M ) (L(I)+2)/2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer L(M), the index of each Legendre product
c    polynomial factor.  0 <= L(*).
c
c    Input, integer O_MAX, an upper limit on the size of the
c    output arrays.
c      O_MAX = product ( 1 <= I <= M ) (L(I)+2)/2.
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
      integer o_max

      double precision c(o_max)
      double precision c1(o_max)
      double precision c2(o_max)
      integer e(o_max)
      integer e1(o_max)
      integer e2(o_max)
      integer f2(o_max)
      integer i
      integer j1
      integer j2
      integer l(m)
      integer o
      integer o1
      integer o2
      integer p(m)

      o1 = 1
      c1(1) = 1.0D+00
      e1(1) = 1
c
c  Implicate one factor at a time.
c
      do i = 1, m

        call lp_coefficients ( l(i), o2, c2, f2 )

        o = 0

        do j2 = 1, o2
          do j1 = 1, o1
            o = o + 1
            c(o) = c1(j1) * c2(j2)
            if ( 1 .lt. i ) then
              call mono_unrank_grlex ( i - 1, e1(j1), p(1:i-1) )
            end if
            p(i) = f2(j2)
            call mono_rank_grlex ( i, p, e(o) )
          end do
        end do

        call polynomial_sort ( o, c, e )
        call polynomial_compress ( o, c, e, o, c, e )

        o1 = o
        do j1 = 1, o1
          c1(j1) = c(j1)
          e1(j1) = e(j1)
        end do

      end do

      return
      end
      subroutine lpp_value ( m, n, o, x, v )

c*********************************************************************72
c
cc LPP_VALUE evaluates a Legendre Product Polynomial at several points X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 September 2014
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
c    Input, integer O(M), the degree of the polynomial factors.
c    0 <= O(*).
c
c    Input, double precision X(M,N), the evaluation points.
c
c    Output, double precision VALUE(N), the value of the Legendre Product
c    Polynomial of degree O at the points X.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer o(m)
      double precision v(n)
      double precision vi(n)
      double precision x(m,n)

      do j = 1, n
        v(j) = 1.0D+00
      end do

      do i = 1, m

        call lp_value ( n, o(i), x(i,1:n), vi )

        do j = 1, n
          v(j) = v(j) * vi(j)
        end do

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
c    07 December 2013
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
      subroutine mono_print ( m, f, title )

c*********************************************************************72
c
cc MONO_PRINT prints a monomial.
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
c    Input, integer F(M), the exponents.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m

      integer f(m)
      integer i
      character * ( * ) title

      write ( *, '(a)', advance = 'no' ) title
      write ( *, '(a)', advance = 'no' ) 'x^('
      do i = 1, m
        write ( *, '(i2)', advance = 'no' ) f(i)
        if ( i .lt. m ) then
          write (  *, '(a)', advance = 'no' ) ','
        else
          write (  *, '(a)', advance = 'yes' ) ').'
        end if
      end do

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
c    dimension M, with degree NM = 0, then 1, then 2, ...
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
c    11 December 2013
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
c    D variables, of total degree N or less.
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
      subroutine mono_upto_next_grlex ( m, n, x )

c*********************************************************************72
c
cc MONO_UPTO_NEXT_GRLEX: grlex next monomial with total degree up to N.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total
c    degree up to N.
c
c    For example:
c
c    M = 3
c    N = 3
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
c    Input, integer N, the maximum degree.
c    0 <= N.
c
c    Input/output, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, 0 ].
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
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to less than 0.'
        stop 1
      end if

      if ( n .lt. i4vec_sum ( m, x ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to more than N.'
        stop 1
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n ) then
        x(1) = 0
      else
        call mono_next_grlex ( m, x )
      end if

      return
      end
      subroutine mono_upto_random ( m, n, seed, rank, x )

c*********************************************************************72
c
cc MONO_UPTO_RANDOM: random monomial with total degree less than or equal to N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 November 2013
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
c    Input/output, integer SEED, the random number seed.
c
c    Output, integer RANK, the rank of the monomial.
c
c    Output, integer X(M), the random monomial.
c
      implicit none

      integer m

      integer i4_uniform_ab
      integer mono_upto_enum
      integer n
      integer rank
      integer rank_max
      integer rank_min
      integer seed
      integer x(m)

      rank_min = 1
      rank_max = mono_upto_enum ( m, n )
      rank = i4_uniform_ab ( rank_min, rank_max, seed )
      call mono_unrank_grlex ( m, rank, x )

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
      subroutine perm_uniform ( n, seed, p )

c*********************************************************************72
c
cc PERM_UNIFORM selects a random permutation of N objects.
c
c  Discussion:
c
c    The routine assumes the objects are labeled 1, 2, ... N.
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
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the number of objects to be permuted.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, integer P(N), a permutation of ( 1, 2, ..., N ), in standard
c    index form.
c
      implicit none

      integer n

      integer i
      integer i4_uniform_ab
      integer j
      integer p(n)
      integer pk
      integer seed

      do i = 1, n
        p(i) = i
      end do

      do i = 1, n
        j = i4_uniform_ab ( i, n, seed )
        pk = p(i)
        p(i) = p(j)
        p(j) = pk
      end do

      return
      end
      subroutine polynomial_compress ( o1, c1, e1, o2, c2, e2 )

c*********************************************************************72
c
cc POLYNOMIAL_COMPRESS compresses a polynomial.
c
c  Discussion:
c
c    The function polynomial_sort() must be called first.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2014
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

      get = 0
      put = 0

10    continue

      if ( get .lt. o1 ) then

        get = get + 1

        if ( abs ( c1(get) ) .le. r8_epsilon_sqrt ) then
          go to 10
        end if

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
      subroutine r8vec_uniform_ab ( n, a, b, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
c
c  Discussion:
c
c    Each dimension ranges from A to B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 January 2005
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
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

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
