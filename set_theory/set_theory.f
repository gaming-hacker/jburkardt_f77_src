      subroutine b4set_colex_rank ( n, t, rank )

c*********************************************************************72
c
cc B4SET_COLEX_RANK computes the colexicographic rank of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T, the set.
c
c    Output, integer RANK, the rank of the set.
c
      implicit none

      integer n

      integer i
      integer rank
      integer t

      rank = 0

      do i = 0, n - 1

        if ( btest ( t, i ) ) then
          rank = rank + 2 ** i
        end if

      end do

      return
      end
      subroutine b4set_colex_successor ( n, t, rank )

c*********************************************************************72
c
cc B4SET_COLEX_SUCCESSOR computes the colexicographic successor of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input/output, integer T, describes a set.  
c    On input, T describes a set.
c    On output, T describes the next set in the ordering.
c    If the input T was the last in the ordering, then the output T
c    will be the first.
c
c    Input/output, integer RANK, the rank.
c    If RANK = -1 on input, then the routine understands that this is
c    the first call, and that the user wishes the routine to supply
c    the first element in the ordering, which has RANK = 0.
c    In general, the input value of RANK is increased by 1 for output,
c    unless the very last element of the ordering was input, in which
c    case the output value of RANK is 0.
c
      implicit none

      integer n

      integer i
      integer rank
      integer t
c
c  Return the first element.
c
      if ( rank .eq. -1 ) then
        t = 0
        rank = 0
        return
      end if

      do i = 0, n - 1

        if ( .not. btest ( t, i ) ) then
          t = ibset ( t, i )
          rank = rank + 1
          return
        else
          t = ibclr ( t, i )
        end if

      end do

      rank = 0

      return
      end
      subroutine b4set_colex_unrank ( rank, n, t )

c*********************************************************************72
c
cc B4SET_COLEX_UNRANK computes the B4SET of given colexicographic rank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer RANK, the rank of the set.
c
c    Input, integer N, the order of the master set.
c
c    Output, integer T, the set of the given rank.
c
      implicit none

      integer n

      integer i
      integer rank
      integer rank_copy
      integer sub_num
      integer t
c
c  Check.
c
      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'B4SET_COLEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  Input N is illegal.'
        stop
      end if

      call b4set_enum ( n, sub_num )

      if ( rank .lt. 0 .or. sub_num .lt. rank ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'B4SET_COLEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  The input rank is illegal.'
        stop
      end if

      rank_copy = rank

      t = 0

      do i = 0, n - 1

        if ( mod ( rank_copy, 2 ) .eq. 1 ) then
          t = ibset ( t, i )
        else
          t = ibclr ( t, i )
        end if

        rank_copy = rank_copy / 2

      end do

      return
      end
      subroutine b4set_complement ( n, a, b )

c*********************************************************************72
c
cc B4SET_COMPLEMENT computes the complement of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, the set.
c
c    Output, integer B, the complement of A.
c
      implicit none

      integer a
      integer b
      integer i
      integer n

      do i = 1, n
        if ( .not. btest ( a, i - 1 ) ) then
          b = ibset ( b, i - 1 )
        else
          b = ibclr ( b, i - 1 )
        end if
      end do

      return
      end
      subroutine b4set_complement_relative ( n, a, b, c )

c*********************************************************************72
c
cc B4SET_COMPLEMENT_RELATIVE computes the relative complement of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, the set.
c
c    Input, integer B, the set with respect to which 
c    the complement is taken.
c
c    Output, integer C, the complement of A with respect to B.
c
      implicit none

      integer a
      integer b
      integer c
      integer i
      integer n

      do i = 1, n
        if (  btest ( a, i - 1 ) .and. .not. btest ( b, i - 1 ) ) then
          c = ibset ( c, i - 1 )
        else
          c = ibclr ( c, i - 1 )
        end if
      end do

      return
      end
      subroutine b4set_delete ( n, a, t )

c*********************************************************************72
c
cc B4SET_DELETE deletes an element from a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, an item.
c
c    Input/output, integer T, a set.
c
      implicit none

      integer a
      integer n
      integer t

      if ( a .lt. 1 .or. n .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'B4SET_DELETE - Fatal error!'
        write ( *, '(a)' ) '  1 .le. A .le. N fails.'
        stop
      end if

      t = ibclr ( t, a - 1 )

      return
      end
      subroutine b4set_distance ( n, t1, t2, dist )

c*********************************************************************72
c
cc B4SET_DISTANCE computes the Hamming distance between two B4SET's.
c
c  Discussion:
c
c    The sets T1 and T2 are assumed to be subsets of a set of N elements.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T1, T2, two sets.
c
c    Output, integer DIST, the Hamming distance between T1 and T2,
c    defined as the number of elements of the master set which are
c    in either T1 or T2 but not both.
c
      implicit none

      integer dist
      integer i
      integer n
      integer t1
      integer t2

      dist = 0

      do i = 1, n

        if ( btest ( t1, i - 1 ) .neqv. btest ( t2, i - 1 ) ) then
          dist = dist + 1
        end if

      end do

      return
      end
      subroutine b4set_enum ( n, set_num )

c*********************************************************************72
c
cc B4SET_ENUM enumerates the B4SET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Output, integer SET_NUM, the number of distinct sets.
c
      implicit none

      integer n
      integer set_num

      set_num = 2 ** n

      return
      end
      function b4set_index ( n, a, t )

c*********************************************************************72
c
cc B4SET_INDEX returns the index of an element of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, the item.
c
c    Input, integer T, a set.
c
c    Output, integer B4SET_INDEX, the index of the item in the set,
c    or -1 if the item is not in the set.
c
      implicit none

      integer a
      integer b4set_index
      integer i
      integer n
      integer t
      integer value

      if ( a .lt. 1 .or. n .lt. a ) then
        value = -1
      else
        value = 0
        do i = 1, a
          if ( btest ( t, i - 1 ) ) then
            value = value + 1
          end if
        end do
      end if

      b4set_index = value

      return
      end
      subroutine b4set_insert ( n, a, t )

c*********************************************************************72
c
cc B4SET_INSERT inserts an item into a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, the item.
c    1 .le. A .le. N.
c
c    Input/output, integer T, a set.
c
      implicit none

      integer a
      integer n
      integer t

      if ( a .lt. 1 .or. n .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'B4SET_INSERT - Fatal error!'
        write ( *, '(a)' ) '  1 .le. A .le. N fails.'
        stop
      end if

      t = ibset ( t, a - 1 )

      return
      end
      subroutine b4set_intersect ( n, a, b, c )

c*********************************************************************72
c
cc B4SET_INTERSECT computes the intersection of two B4SET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, B, two sets.
c
c    Output, integer C, the intersection of A and B.
c
      implicit none

      integer a
      integer b
      integer c
      integer i
      integer n

      c = 0

      do i = 1, n
        if ( btest ( a, i - 1 ) .and. btest ( b, i - 1 ) ) then
          c = ibset ( c, i - 1 )
        else
          c = ibclr ( c, i - 1 )
        end if
      end do

      return
      end
      function b4set_is_empty ( n, t )

c*********************************************************************72
c
cc B4SET_IS_EMPTY determines if a B4SET is empty.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T, a set.
c
c    Output, logical B4SET_IS_EMPTY is TRUE if T is empty.
c
      implicit none

      logical b4set_is_empty
      integer n
      integer t

      b4set_is_empty = ( t .eq. 0 )

      return
      end
      function b4set_is_equal ( n, t1, t2 )

c*********************************************************************72
c
cc B4SET_IS_EQUAL determines if two B4SET's are equal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T1, T2, two sets.
c
c    Output, logical B4SET_IS_EQUAL, is TRUE if T1 equals T2.
c
      implicit none

      logical b4set_is_equal
      integer n
      integer t1
      integer t2

      b4set_is_equal = ( t1 .eq. t2 )

      return
      end
      function b4set_is_member ( n, a, t )

c*********************************************************************72
c
cc B4SET_IS_MEMBER determines if an item is a member of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, an item.
c
c    Input, integer T, a set.
c
c    Output, logical B4SET_IS_MEMBER, is TRUE if A is an element of T.
c
      implicit none

      integer a
      logical b4set_is_member
      integer n
      integer t

      if ( 1 .le. a .and. a .le. n ) then
        b4set_is_member = btest ( t, a - 1 )
      else
        b4set_is_member = .false.
      end if

      return
      end
      function b4set_is_subset ( n, t1, t2 )

c*********************************************************************72
c
cc B4SET_IS_SUBSET determines if one B4SET is a subset of another.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T1, T2, two sets.
c
c    Output, logical B4SET_IS_SUBSET, is TRUE if T1 is a subset of T2.
c
      implicit none

      logical b4set_is_subset
      integer i
      integer n
      integer t1
      integer t2

      b4set_is_subset = .true.

      do i = 1, n
        if ( btest ( t1, i - 1 ) .and. .not. btest ( t2, i - 1 ) ) then
          b4set_is_subset = .false.
          return
        end if
      end do

      return
      end
      subroutine b4set_lex_rank ( n, t, rank )

c*********************************************************************72
c
cc B4SET_LEX_RANK computes the lexicographic rank of a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T, the set.
c
c    Output, integer RANK, the rank of the set.
c
      implicit none

      integer n

      integer i
      integer rank
      integer t

      rank = 0

      do i = 0, n - 1

        if ( btest ( t, i ) ) then
          rank = rank + 2 ** ( n - i - 1 )
        end if

      end do

      return
      end
      subroutine b4set_lex_successor ( n, t, rank )

c*********************************************************************72
c
cc B4SET_LEX_SUCCESSOR computes the lexicographic successor of a B4SET.
c
c  Discussion:
c
c    In the original code, there is a last element with no successor.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input/output, integer T, describes a set.
c    On input, T describes a set.
c    On output, T describes the next set in the ordering.
c    If the input T was the last in the ordering, then the output T
c    will be the first.
c
c    Input/output, integer RANK, the rank.
c    If RANK = -1 on input, then the routine understands that this is
c    the first call, and that the user wishes the routine to supply
c    the first element in the ordering, which has RANK = 0.
c    In general, the input value of RANK is increased by 1 for output,
c    unless the very last element of the ordering was input, in which
c    case the output value of RANK is 0.
c
      implicit none

      integer n

      integer i
      integer rank
      integer t
c
c  Return the first element.
c
      if ( rank .eq. -1 ) then
        t = 0
        rank = 0
        return
      end if

      do i = n - 1, 0, -1

        if ( .not. btest ( t, i ) ) then
          t = ibset ( t, i )
          rank = rank + 1
          return
        else
          t = ibclr ( t, i )
        end if

      end do

      rank = 0

      return
      end
      subroutine b4set_lex_unrank ( rank, n, t )

c*********************************************************************72
c
cc B4SET_LEX_UNRANK computes the B4SET of given lexicographic rank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer RANK, the rank of the set.
c
c    Input, integer N, the order of the master set.
c
c    Output, integer T, the set of the given rank.
c
      implicit none

      integer n

      integer i
      integer nsub
      integer rank
      integer rank_copy
      integer set_num
      integer t
c
c  Check.
c
      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'B4SET_LEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  Input N is illegal.'
        stop
      end if

      call b4set_enum ( n, set_num )

      if ( rank .lt. 0 .or. set_num .lt. rank ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'B4SET_LEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  The input rank is illegal.'
        stop
      end if

      rank_copy = rank

      t = 0

      do i = n - 1, 0, -1

        if ( mod ( rank_copy, 2 ) .eq. 1 ) then
          t = ibset ( t, i )
        else
          t = ibclr ( t, i )
        end if

        rank_copy = rank_copy / 2

      end do

      return
      end
      subroutine b4set_to_lset ( n, t, a )

c*********************************************************************72
c
cc B4SET_TO_LSET converts a B4SET to an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T, the set.
c
c    Input, logical A(N), the LSET version of the set.
c
      implicit none

      integer n

      logical a(n)
      integer i
      integer t

      do i = 1, n
        a(i) = btest ( t, i - 1 )
      end do

      return
      end
      subroutine b4set_transpose_print ( n, t, title )

c*********************************************************************72
c
cc B4SET_TRANSPOSE_PRINT prints a B4SET "transposed".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer T, the set.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer i
      integer n
      integer s
      integer t
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( t .eq. 0 ) then
        write ( *, '(a)' ) '  (Empty set)'
      else
        s = 0
        do i = 1, n
          if ( btest ( t, i - 1 ) ) then
            write ( *, '(2x,i2)', advance = 'no' ) i
            s = s + 4
          end if
          if ( 76 .lt. s .or. ( 0 .lt. s .and. i .eq. n ) ) then
            write ( *, '(1x)', advance = 'yes' )
            s = 0
          end if
       end do
      end if

      return
      end
      subroutine b4set_random ( n, seed, a )

c*********************************************************************72
c
cc B4SET_RANDOM sets a rondom B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, integer A, the random B4SET.
c
      implicit none

      integer n

      integer a
      integer a_logical(n)
      integer seed

      call lset_random ( n, seed, a_logical )
      call lset_to_b4set ( n, a_logical, a )

      return
      end
      subroutine b4set_union ( n, a, b, c )

c*********************************************************************72
c
cc B4SET_UNION computes the union of two B4SET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, B, two sets.
c
c    Output, integer C, the union of A and B.
c
      implicit none

      integer a
      integer b
      integer c
      integer i
      integer n

      do i = 1, n
        if ( btest ( a, i - 1 ) .or. btest ( b, i - 1 ) ) then
          c = ibset ( c, i - 1 )
        else
          c = ibclr ( c, i - 1 )
        end if
      end do

      return
      end
      subroutine b4set_weight ( n, t, weight )

c*********************************************************************72
c
cc B4SET_WEIGHT computes the Hamming weight of a B4SET.
c
c  Discussion:
c
c    The Hamming weight is simply the number of elements in the set.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set..
c
c    Input, integer T, the set.
c
c    Output, integer WEIGHT, the Hamming weight of the set T.
c
      implicit none

      integer i
      integer n
      integer t
      integer weight

      weight = 0

      do i = 1, n
        if ( btest ( t, i - 1 ) ) then
          weight = weight + 1
        end if
      end do

      return
      end
      subroutine b4set_xor ( n, a, b, c )

c*********************************************************************72
c
cc B4SET_XOR computes the symmetric difference of two B4SET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, B, two sets.
c
c    Output, integer C, the symmetric difference of A and B.
c
      implicit none

      integer a
      integer b
      integer c
      integer i
      integer n

      do i = 1, n
        if ( btest ( a, i - 1 ) .neqv. btest ( b, i - 1 ) ) then
          c = ibset ( c, i - 1 )
        else
          c = ibclr ( c, i - 1 )
        end if
      end do

      return
      end
      subroutine digit_to_ch ( digit, ch )

c*********************************************************************72
c
cc DIGIT_TO_CH returns the character representation of a decimal digit.
c
c  Discussion:
c
c    Instead of CHAR, we now use the ACHAR function, which
c    guarantees the ASCII collating sequence.
c
c  Example:
c
c    DIGIT   CH
c    -----  ---
c      0    '0'
c      1    '1'
c    ...    ...
c      9    '9'
c     17    '*'
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIGIT, the digit value between 0 and 9.
c
c    Output, character CH, the corresponding character.
c
      implicit none

      character ch
      integer digit

      if ( 0 .le. digit .and. digit .le. 9 ) then

        ch = achar ( digit + 48 )

      else

        ch = '*'

      end if

      return
      end
      subroutine i4_to_s_right ( intval, s )

c*********************************************************************72
c
cc I4_TO_S_RIGHT converts an I4 to a right justified string.
c
c  Discussion:
c
c    An I4 is an integer.
c
c  Example:
c
c    Assume that S is 6 characters long:
c
c    INTVAL       S
c
c         1       1
c        -1      -1
c         0       0
c      1952    1952
c    123456  123456
c   1234567  ******  <-- Not enough roomc
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INTVAL, an integer to be converted.
c
c    Output, character * ( * ) S, the representation of the integer.
c    The integer will be right-justified.  If there is not enough space,
c    the string will be filled with stars.
c
      implicit none

      character c
      integer i
      integer idig
      integer ihi
      integer ilo
      integer intval
      integer ipos
      integer ival
      character * ( * )  s

      s = ' '

      ilo = 1
      ihi = len ( s )

      if ( ihi .le. 0 ) then
        return
      end if
c
c  Make a copy of the integer.
c
      ival = intval
c
c  Handle the negative sign.
c
      if ( ival .lt. 0 ) then

        if ( ihi .le. 1 ) then
          s(1:1) = '*'
          return
        end if

        ival = -ival
        s(1:1) = '-'
        ilo = 2

      end if
c
c  The absolute value of the integer goes into S(ILO:IHI).
c
      ipos = ihi
c
c  Find the last digit of IVAL, strip it off, and stick it into the string.
c
10    continue

        idig = mod ( ival, 10 )
        ival = ival / 10

        if ( ipos .lt. ilo ) then
          do i = 1, ihi
            s(i:i) = '*'
          end do
          return
        end if

        call digit_to_ch ( idig, c )

        s(ipos:ipos) = c
        ipos = ipos - 1

        if ( ival .eq. 0 ) then
          go to 20
        end if

      go to 10

20    continue
c
c  Shift the minus sign, if any.
c
      if ( s(1:1) .eq. '-' ) then
        if ( ipos .ne. 1 ) then
          s(1:1) = ' '
          s(ipos:ipos) = '-'
        end if
      end if

      return
      end
      subroutine i4vec_to_b4set ( n_num, a_num, n, a )

c*********************************************************************72
c
cc I4VEC_TO_B4SET converts an I4VEC to a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N_NUM, the number of numeric entries.
c
c    Input, integer A_NUM(N_NUM), the numeric vector.
c    Entries of A_NUM should be between 1 and 32.
c
c    Input, integer N, the order of the master set.
c    N .le. 32.
c
c    Output, integer A, the corresponding B4SET.
c
      implicit none

      integer n
      integer n_num

      integer a
      integer a_num(n_num)
      integer i
      integer pos
      integer pos_max

      a = 0
      pos_max = min ( bit_size ( a ), n )

      do i = 1, n_num
        pos = a_num(i)
        if ( 1 .le. pos .and. pos .le. pos_max ) then
          a = ibset ( a, pos - 1 )
        end if
      end do

      return
      end
      subroutine i4vec_to_lset ( n_num, a_num, n, a )

c*********************************************************************72
c
cc I4VEC_TO_LSET converts an I4VEC to an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N_NUM, the number of numeric entries.
c
c    Input, integer A_NUM(N_NUM), the numeric vector.
c
c    Input, integer N, the order of the master set.
c
c    Output, logical A(N), the corresponding LSET.
c
      implicit none

      integer n
      integer n_num

      logical a(n)
      integer a_num(n_num)
      integer i

      do i = 1, n
        a(i) = .false.
      end do

      do i = 1, n_num
        call lset_insert ( n, a_num(i), a )
      end do

      return
      end
      subroutine lset_colex_rank ( n, t, rank )

c*********************************************************************72
c
cc LSET_COLEX_RANK computes the colexicographic rank of an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T(N), the set.
c
c    Output, integer RANK, the rank of the set.
c
      implicit none

      integer n

      integer i
      integer rank
      logical t(n)

      rank = 0

      do i = 1, n

        if ( t(i) ) then
          rank = rank + 2 ** ( i - 1 )
        end if

      end do

      return
      end
      subroutine lset_colex_successor ( n, t, rank )

c*********************************************************************72
c
cc LSET_COLEX_SUCCESSOR computes the colexicographic successor of an LSET.
c
c  Discussion:
c
c    In the original code, there is a last element with no successor.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input/output, logical T(N), describes a set.  
c    On input, T describes a set.
c    On output, T describes the next set in the ordering.
c    If the input T was the last in the ordering, then the output T
c    will be the first.
c
c    Input/output, integer RANK, the rank.
c    If RANK = -1 on input, then the routine understands that this is
c    the first call, and that the user wishes the routine to supply
c    the first element in the ordering, which has RANK = 0.
c    In general, the input value of RANK is increased by 1 for output,
c    unless the very last element of the ordering was input, in which
c    case the output value of RANK is 0.
c
      implicit none

      integer n

      integer i
      integer rank
      logical t(n)
c
c  Return the first element.
c
      if ( rank .eq. -1 ) then
        do i = 1, n
          t(i) = .false.
        end do
        rank = 0
        return
      end if

      do i = 1, n

       if ( .not. t(i) ) then
          t(i) = .true.
          rank = rank + 1
          return
        else
          t(i) = .false.
        end if

      end do

      rank = 0

      return
      end
      subroutine lset_colex_unrank ( rank, n, t )

c*********************************************************************72
c
cc LSET_COLEX_UNRANK computes the LSET of given colexicographic rank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer RANK, the rank of the set.
c
c    Input, integer N, the order of the master set.
c
c    Output, logical T(N), the set of the given rank.
c
      implicit none

      integer n

      integer i
      integer rank
      integer rank_copy
      integer sub_num
      logical t(n)
c
c  Check.
c
      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_COLEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  Input N is illegal.'
        stop
      end if

      call lset_enum ( n, sub_num )

      if ( rank .lt. 0 .or. sub_num .lt. rank ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_COLEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  The input rank is illegal.'
        stop
      end if

      rank_copy = rank

      do i = 1, n
        if ( mod ( rank_copy, 2 ) .eq. 1 ) then
          t(i) = .true.
        else
          t(i) = .false.
        end if

        rank_copy = rank_copy / 2

      end do

      return
      end
      subroutine lset_complement ( n, a, b )

c*********************************************************************72
c
cc LSET_COMPLEMENT computes the complement of an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical A(N), the set.
c
c    Output, logical B(N), the complement of A.
c
      implicit none

      integer n

      logical a(n)
      logical b(n)
      integer i

      do i = 1, n
        b(i) = .not. a(i)
      end do

      return
      end
      subroutine lset_complement_relative ( n, a, b, c )

c*********************************************************************72
c
cc LSET_COMPLEMENT_RELATIVE computes the relative complement of an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical A(N), the set.
c
c    Input, logical B(N), the set with respect to which the complement is taken.
c
c    Output, logical C(N), the complement of A with respect to B.
c
      implicit none

      integer n

      logical a(n)
      logical b(n)
      logical c(n)
      integer i

      do i = 1, n
        c(i) = a(i) .and. ( .not. b(i) )
      end do

      return
      end
      subroutine lset_delete ( n, a, t )

c*********************************************************************72
c
cc LSET_DELETE deletes an element from an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, an item.
c
c    Input/output, logical T(N), a set.
c    On output, T(A) = FALSE.
c
      implicit none

      integer n

      integer a
      logical t(n)

      if ( a .lt. 1 .or. n .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_DELETE - Fatal error!'
        write ( *, '(a)' ) '  1 .le. A .le. N fails.'
        stop
      end if

      t(a) = .false.

      return
      end
      subroutine lset_distance ( n, t1, t2, dist )

c*********************************************************************72
c
cc LSET_DISTANCE computes the Hamming distance between two LSET's.
c
c  Discussion:
c
c    The sets T1 and T2 are assumed to be subsets of a set of N elements.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T1(N), T2(N), two sets.
c
c    Output, integer DIST, the Hamming distance between T1 and T2,
c    defined as the number of elements of the master set which are
c    in either T1 or T2 but not both.
c
      implicit none

      integer n

      integer dist
      integer i
      logical t1(n)
      logical t2(n)

      dist = 0

      do i = 1, n

        if ( (         t1(i)   .and. ( .not. t2(i) ) ) .or. 
     &       ( ( .not. t1(i) ) .and.         t2(i)   )  ) then
          dist = dist + 1
        end if

      end do

      return
      end
      subroutine lset_enum ( n, set_num )

c*********************************************************************72
c
cc LSET_ENUM enumerates the LSET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Output, integer SET_NUM, the number of distinct sets.
c
      implicit none

      integer n
      integer set_num

      set_num = 2 ** n

      return
      end
      function lset_index ( n, a, t )

c*********************************************************************72
c
cc LSET_INDEX returns the index of an element of an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, the item.
c
c    Input, logical T(N), a set.
c
c    Output, integer LSET_INDEX, the index of the item in the set,
c    or -1 if the item is not in the set.
c
      implicit none

      integer n

      integer a
      integer i
      integer lset_index
      logical t(n)
      integer value

      if ( a .lt. 1 .or. n .lt. a ) then
        value = -1
      else
        value = 0
        do i = 1, a
          if ( t(i) ) then
            value = value + 1
          end if
        end do
      end if

      lset_index = value

      return
      end
      subroutine lset_insert ( n, a, t )

c*********************************************************************72
c
cc LSET_INSERT inserts an item into an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, the item.
c    1 .le. A .le. N.
c
c    Input/output, logical T(N), a set.
c    On output, T(A) = TRUE.
c
      implicit none

      integer n

      integer a
      logical t(n)

      if ( a .lt. 1 .or. n .lt. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_INSERT - Fatal error!'
        write ( *, '(a)' ) '  1 .le. A .le. N fails.'
        stop
      end if

      t(a) = .true.

      return
      end
      subroutine lset_intersect ( n, a, b, c )

c*********************************************************************72
c
cc LSET_INTERSECT computes the intersection of two LSET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical A(N), B(N), two sets.
c
c    Output, logical C(N), the intersection of A and B.
c
      implicit none

      integer n

      logical a(n)
      logical b(n)
      logical c(n)
      integer i

      do i = 1, n
        c(i) = a(i) .and. b(i)
      end do

      return
      end
      function lset_is_empty ( n, t )

c*********************************************************************72
c
cc LSET_IS_EMPTY determines if an LSET is empty.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T(N), a set.
c
c    Output, logical LSET_IS_EMPTY is TRUE if T is empty.
c
      implicit none

      integer n

      integer i
      logical lset_is_empty
      logical t(n)

      lset_is_empty = .true.

      do i = 1, n
        if ( t(i) ) then
          lset_is_empty = .false.
          return
        end if
      end do

      return
      end
      function lset_is_equal ( n, t1, t2 )

c*********************************************************************72
c
cc LSET_IS_EQUAL determines if two LSET's are equal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T1(N), T2(N), two sets.
c
c    Output, logical LSET_IS_EQUAL, is TRUE if T1 equals T2.
c
      implicit none

      integer n

      integer i
      logical lset_is_equal
      logical t1(n)
      logical t2(n)

      lset_is_equal = .true.

      do i = 1, n
        if ( (       t1(i) .and. .not. t2(i) ) .or.
     &       ( .not. t1(i) .and.       t2(i) ) ) then
          lset_is_equal = .false.
          return
        end if
      end do

      return
      end
      function lset_is_member ( n, a, t )

c*********************************************************************72
c
cc LSET_IS_MEMBER determines if an item is a member of an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, integer A, an item.
c
c    Input, logical T(N), a set.
c
c    Output, logical LSET_IS_MEMBER, is TRUE if A is an element of T.
c
      implicit none

      integer n

      integer a
      logical lset_is_member
      logical t(n)

      if ( 1 .le. a .and. a .le. n ) then
        lset_is_member = t(a)
      else
        lset_is_member = .false.
      end if

      return
      end
      function lset_is_subset ( n, t1, t2 )

c*********************************************************************72
c
cc LSET_IS_SUBSET determines if one LSET is a subset of another.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T1(N), T2(N), two sets.
c
c    Output, logical LSET_IS_SUBSET, is TRUE if T1 is a subset of T2.
c
      implicit none

      integer n

      integer i
      logical lset_is_subset
      logical t1(n)
      logical t2(n)

      lset_is_subset = .true.

      do i = 1, n
        if ( t1(i) .and. .not. t2(i) ) then
          lset_is_subset = .false.
          return
        end if
      end do

      return
      end
      subroutine lset_lex_rank ( n, t, rank )

c*********************************************************************72
c
cc LSET_LEX_RANK computes the lexicographic rank of an LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T(N), the set.
c
c    Output, integer RANK, the rank of the set.
c
      implicit none

      integer n

      integer i
      integer rank
      logical t(n)

      rank = 0

      do i = 1, n

        if ( t(i) ) then
          rank = rank + 2 ** ( n - i )
        end if

      end do

      return
      end
      subroutine lset_lex_successor ( n, t, rank )

c*********************************************************************72
c
cc LSET_LEX_SUCCESSOR computes the lexicographic successor of an LSET.
c
c  Discussion:
c
c    In the original code, there is a last element with no successor.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input/output, logical T(N), describes a set.
c    On input, T describes a set.
c    On output, T describes the next set in the ordering.
c    If the input T was the last in the ordering, then the output T
c    will be the first.
c
c    Input/output, integer RANK, the rank.
c    If RANK = -1 on input, then the routine understands that this is
c    the first call, and that the user wishes the routine to supply
c    the first element in the ordering, which has RANK = 0.
c    In general, the input value of RANK is increased by 1 for output,
c    unless the very last element of the ordering was input, in which
c    case the output value of RANK is 0.
c
      implicit none

      integer n

      integer i
      integer rank
      logical t(n)
c
c  Return the first element.
c
      if ( rank .eq. -1 ) then
        do i = 1, n
          t(i) = .false.
        end do
        rank = 0
        return
      end if

      do i = n, 1, -1

        if ( .not. t(i) ) then
          t(i) = .true.
          rank = rank + 1
          return
        else
          t(i) = .false.
        end if

      end do

      rank = 0

      return
      end
      subroutine lset_lex_unrank ( rank, n, t )

c*********************************************************************72
c
cc LSET_LEX_UNRANK computes the LSET of given lexicographic rank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer RANK, the rank of the set.
c
c    Input, integer N, the order of the master set.
c
c    Output, logical T(N), the set of the given rank.
c
      implicit none

      integer n

      integer i
      integer nsub
      integer rank
      integer rank_copy
      integer set_num
      logical t(n)
c
c  Check.
c
      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_LEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  Input N is illegal.'
        stop
      end if

      call lset_enum ( n, set_num )

      if ( rank .lt. 0 .or. set_num .lt. rank ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_LEX_UNRANK - Fatal error!'
        write ( *, '(a)' ) '  The input rank is illegal.'
        stop
      end if

      rank_copy = rank

      do i = n, 1, -1

        if ( mod ( rank_copy, 2 ) .eq. 1 ) then
          t(i) = .true.
        else
          t(i) = .false.
        end if

        rank_copy = rank_copy / 2

      end do

      return
      end
      subroutine lset_random ( n, seed, a )

c*********************************************************************72
c
cc LSET_RANDOM sets a rondom LSET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, logical A(N).
c
      implicit none

      integer n

      logical a(n)
      integer i4_huge
      parameter ( i4_huge      = 2147483647 )
      integer i4_huge_half
      parameter ( i4_huge_half = 1073741823 )
      integer i
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LSET_RANDOM - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        a(i) = ( i4_huge_half .lt. seed )

      end do

      return
      end
      subroutine lset_to_b4set ( n, a_log, a )

c*********************************************************************72
c
cc LSET_TO_B4SET converts an I4VEC to a B4SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c    N .le. 32.
c
c    Input, logical A_LOG(N), the logical representation of the set.
c
c    Output, integer A, the corresponding B4SET.
c
      implicit none

      integer n

      integer a
      logical a_log(n)
      integer i

      a = 0

      do i = 1, n
        if ( a_log(i) ) then
          a = ibset ( a, i - 1 )
        end if
      end do

      return
      end
      subroutine lset_transpose_print ( n, t, title )

c*********************************************************************72
c
cc LSET_TRANSPOSE_PRINT prints an LSET "transposed".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T(N), the set.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      integer i
      logical lset_is_empty
      integer s
      logical t(n)
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( lset_is_empty ( n, t ) ) then
        write ( *, '(a)' ) '  (Empty set)'
      else
        s = 0
        do i = 1, n
          if ( t(i) ) then
            write ( *, '(2x,i2)', advance = 'no' ) i
            s = s + 4
          end if
          if ( 76 .lt. s .or. ( 0 .lt. s .and. i .eq. n ) ) then
            write ( *, '(1x)', advance = 'yes' )
            s = 0
          end if
        end do
      end if

      return
      end
      subroutine lset_union ( n, a, b, c )

c*********************************************************************72
c
cc LSET_UNION computes the union of two LSET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical A(N), B(N), two sets.
c
c    Output, logical C(N), the union of A and B.
c
      implicit none

      integer n

      logical a(n)
      logical b(n)
      logical c(n)
      integer i

      do i = 1, n
        c(i) = a(i) .or. b(i)
      end do

      return
      end
      subroutine lset_weight ( n, t, weight )

c*********************************************************************72
c
cc LSET_WEIGHT computes the Hamming weight of an LSET.
c
c  Discussion:
c
c    The Hamming weight is simply the number of elements in the set.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T(N), the set.
c
c    Output, integer WEIGHT, the Hamming weight of the set T.
c
      implicit none

      integer n

      integer i
      logical t(n)
      integer weight

      weight = 0

      do i = 1, n
        if ( t(i) ) then
          weight = weight + 1
        end if
      end do

      return
      end
      subroutine lset_xor ( n, a, b, c )

c*********************************************************************72
c
cc LSET_XOR computes the symmetric difference of two LSET's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical A(N), B(N), two sets.
c
c    Output, logical C(N), the symmetric difference of A and B.
c
      implicit none

      integer n

      logical a(n)
      logical b(n)
      logical c(n)
      integer i

      do i = 1, n
        c(i) =  (         a(i)   .and. ( .not. b(i) ) ) .or. 
     &          ( ( .not. a(i) ) .and.         b(i)   )
      end do

      return
      end
      subroutine lvec_transpose_print ( n, t, title )

c*********************************************************************72
c
cc LVEC_TRANSPOSE_PRINT prints an LVEC "transposed".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the order of the master set.
c
c    Input, logical T(N), the set.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      integer i
      integer ihi
      integer ilo
      logical t(n)
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '

      do ilo = 1, n, 80
        ihi = min ( ilo + 80 - 1, n )
        write ( *, '(80l1)' ) ( t(i), i = ilo, ihi )
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
