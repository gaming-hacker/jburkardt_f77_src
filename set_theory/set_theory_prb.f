      program main

c*********************************************************************72
c
cc MAIN is the main program for SET_THEORY_PRB.
c
c  Discussion:
c
c    SET_THEORY_PRB tests the SET_THEORY library.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SET_THEORY_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SET_THEORY library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SET_THEORY_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' )  ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests the B4SET routines.
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
      implicit none

      integer b_num
      parameter ( b_num = 16 )
      integer n
      parameter ( n = 32 )
      integer w_num
      parameter ( w_num = 5 )
      integer y_num
      parameter ( y_num = 4 )

      integer a
      integer a_num
      integer a_numeric(n)
      integer b
      integer b_numeric(b_num)
      integer c
      integer d
      integer e
      integer f
      integer g
      integer h
      integer i
      logical b4set_is_member
      logical b4set_is_subset
      integer u
      integer u_numeric(n)
      integer w
      integer w_numeric(w_num)
      integer x
      integer y
      integer y_numeric(y_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Test the set theory functions'
      write ( *, '(a)' ) '  with the B4SET representation of a set.'
c
c  Define the universal set.
c
      do i = 1, n
        u_numeric(i) = i
      end do
      call i4vec_to_b4set ( n, u_numeric, n, u )
c
c  Define the set A by a numeric property.
c
      a_num = 0
      do i = 1, n
        if ( mod ( i, 5 ) .eq. 0 ) then
          a_num = a_num + 1
          a_numeric(a_num) = i
        end if
      end do
      call i4vec_to_b4set ( a_num, a_numeric, n, a )
      call b4set_transpose_print ( n, a, '  A: ' );
c
c  Define the set by starting with a numeric list of entries.
c
      b_numeric(1) = 3
      b_numeric(2) = 6
      b_numeric(3) = 9
      b_numeric(4) = 12
      b_numeric(5) = 15
      b_numeric(6) = 18
      b_numeric(7) = 21
      b_numeric(8) = 24
      b_numeric(9) = 27
      b_numeric(10) = 30
      b_numeric(11) = 33
      b_numeric(12) = 36
      b_numeric(13) = 39
      b_numeric(14) = 42
      b_numeric(15) = 45
      b_numeric(16) = 48

      call i4vec_to_b4set ( b_num, b_numeric, n, b )
      call b4set_transpose_print ( n, b, '  B: ' )
c
c  C is the complement of B (with respect to the universal set).
c
      call b4set_complement ( n, b, c )
      call b4set_transpose_print ( n, c, '  C = ~ B:' )
c
c  D is the intersection of A and B.
c
      call b4set_intersect ( n, a, b, d )
      call b4set_transpose_print ( n, d, '  D = A intersect B:' )
c
c  E is the intersection of A and B.
c
      call b4set_union ( n, a, b, e )
      call b4set_transpose_print ( n, e, '  E = A union B:' )
c
c  F is the symmetric difference of A and B.
c
      call b4set_xor ( n, a, b, f )
      call b4set_transpose_print ( n, f, '  F = A xor B:' )
c
c  G is the complement of B with respect to A.
c  H is the complement of A with respect to B.
c
      call b4set_complement_relative ( n, a, b, g )
      call b4set_transpose_print ( n, g, '  G = A ~ B:' )

      call b4set_complement_relative ( n, b, a, h )
      call b4set_transpose_print ( n, h, '  H = B ~ A:' )
c
c  B4SET_IS_MEMBER checks if an element is in a set.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  B4SET_IS_MEMBER ( i, A ) reports whether i is a member of A'
      write ( *, '(a)' ) ' '

      do i = 10, 20
        if ( b4set_is_member ( n, i, a ) ) then
          write ( *, '(2x,i2,a)' ) i, ' is a member of A.'
        else
          write ( *, '(2x,i2,a)' ) i, ' is not a member of A.'
        end if
      end do
c
c  B4SET_IS_SUBSET checks whether a set is a subset.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  B4SET_IS_SUBSET ( D, A ) reports whether D is a subset of A'
      write ( *, '(a)' ) ' '

      call b4set_intersect ( n, a, b, d );

      if ( b4set_is_subset ( n, d, a ) ) then
        write ( *, '(a)' ) '  ( A intersect B ) is a subset of A.'
      else
        write ( *, '(a)' ) '  ( A intersect B)  is not a subset of A.'
      end if
c
c  B4SET_INSERT adds an item to a set.
c
      w_numeric(1) = 1
      w_numeric(2) = 11
      w_numeric(3) = 21
      w_numeric(4) = 31
      w_numeric(5) = 41
      call i4vec_to_b4set ( w_num, w_numeric, n, w )
      call b4set_transpose_print ( n, w, '  W: ' )

      x = 6
      call b4set_insert ( n, x, w )
      call b4set_transpose_print ( n, w, '  W := W + 6:' );

      x = 31
      call b4set_delete ( n, x, w )
      call b4set_transpose_print ( n, w, '  W := W - 31:' );

      y_numeric(1) = 4
      y_numeric(2) = 5
      y_numeric(3) = 6
      y_numeric(4) = 7
      call i4vec_to_b4set ( y_num, y_numeric, n, y )
      call b4set_union ( n, w, y, w )

      call b4set_transpose_print ( n, w, 
     &  '  W := W union [ 4, 5, 6, 7 ]:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests B4SET_COLEX_RANK, _COLEX_SUCCESSOR, _COLEX_UNRANK, _ENUM.
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
      implicit none

      integer n
      parameter ( n = 5 )

      integer nsub
      integer rank
      integer rank_old
      integer t
      character * ( 80 ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  All subsets of a set,'
      write ( *, '(a)' ) '  using the colexicographic ordering'
      write ( *, '(a)' ) '  with the B4SET representation of a set.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  B4SET_COLEX_RANK ranks,'
      write ( *, '(a)' ) '  B4SET_COLEX_SUCCESSOR lists,'
      write ( *, '(a)' ) '  B4SET_COLEX_UNRANK unranks.'
      write ( *, '(a)' ) '  B4SET_ENUM enumerates.'
c
c  Enumerate.
c
      call b4set_enum ( n, nsub )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  For N = ', n
      write ( *, '(a,i8)' ) '  the number of subsets is ', nsub
      write ( *, '(a)' ) ' '
c
c  List
c
      rank = -1

10    continue

        rank_old = rank

        call b4set_colex_successor ( n, t, rank )

        if ( rank .le. rank_old ) then
          go to 20
        end if

        write ( title, '(a,i4)' ) '  Rank: ', rank
        call b4set_transpose_print ( n, t, title )

      go to 10

20    continue
c
c  Unrank.
c
      rank = nsub / 3

      call b4set_colex_unrank ( rank, n, t )

      write ( title, '(2x,a,i4)' ) 'The element of rank ', rank
      call b4set_transpose_print ( n, t, title )
c
c  Rank.
c
      call b4set_colex_rank ( n, t, rank )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) 
     &  '  The rank of this element is computed as ', rank

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests B4SET_LEX_RANK, _LEX_SUCCESSOR, _LEX_UNRANK, _ENUM.
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
      implicit none

      integer n
      parameter ( n = 5 )

      integer nsub
      integer rank
      integer rank_old
      integer t
      character * ( 80 ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  All subsets of a set,'
      write ( *, '(a)' ) '  using the lexicographic ordering,'
      write ( *, '(a)' ) '  with the B4SET representation of a set.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  B4SET_LEX_RANK ranks,'
      write ( *, '(a)' ) '  B4SET_LEX_SUCCESSOR lists,'
      write ( *, '(a)' ) '  B4SET_LEX_UNRANK unranks.'
      write ( *, '(a)' ) '  B4SET_ENUM enumerates.'
c
c  Enumerate.
c
      call b4set_enum ( n, nsub )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  For N = ', n
      write ( *, '(a,i8)' ) '  the number of subsets is ', nsub
      write ( *, '(a)' ) ' '
c
c  List
c
      rank = -1

10    continue

        rank_old = rank

        call b4set_lex_successor ( n, t, rank )

        if ( rank .le. rank_old ) then
          go to 20
        end if

        write ( title, '(a,i4)' ) '  Rank: ', rank
        call b4set_transpose_print ( n, t, title )

      go to 10

20    continue
c
c  Unrank.
c
      rank = nsub / 3

      call b4set_lex_unrank ( rank, n, t )

      write ( title, '(2x,a,i4)' ) 'The element of rank ', rank
      call b4set_transpose_print ( n, t, title )
c
c  Rank.
c
      call b4set_lex_rank ( n, t, rank )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) 
     &  '  The rank of this element is computed as ', rank

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests the LSET routines.
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
      implicit none

      integer b_num
      parameter ( b_num = 16 )
      integer n 
      parameter ( n = 50 )
      integer w_num
      parameter ( w_num = 5 )
      integer y_num
      parameter ( y_num = 4 )

      logical a(n)
      logical b(n)
      integer b_numeric(b_num)
      logical c(n)
      logical d(n)
      logical e(n)
      logical f(n)
      logical g(n)
      logical h(n)
      integer i
      logical lset_is_member
      logical lset_is_subset
      logical u(n)
      integer u_numeric(n)
      logical w(n)
      integer w_numeric(w_num)
      integer x
      logical y(n)
      integer y_numeric(y_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Test the set theory functions'
      write ( *, '(a)' ) '  with the LSET representation of a set.'
c
c  Define the universal set.
c
      do i = 1, n
        u_numeric(i) = i
      end do
      do i = 1, n
        u(i) = .true.
      end do
c
c  Define the set A by a numeric property.
c
      a(1:n) = ( mod ( u_numeric(1:n), 5 ) == 0 )
      call lset_transpose_print ( n, a, '  A: ' );
c
c  Define the set by starting with a numeric list of entries.
c
      b_numeric(1) = 3
      b_numeric(2) = 6
      b_numeric(3) = 9
      b_numeric(4) = 12
      b_numeric(5) = 15
      b_numeric(6) = 18
      b_numeric(7) = 21
      b_numeric(8) = 24
      b_numeric(9) = 27
      b_numeric(10) = 30
      b_numeric(11) = 33
      b_numeric(12) = 36
      b_numeric(13) = 39
      b_numeric(14) = 42
      b_numeric(15) = 45
      b_numeric(16) = 48

      call i4vec_to_lset ( b_num, b_numeric, n, b )
      call lset_transpose_print ( n, b, '  B: ' )
c
c  C is the complement of B (with respect to the universal set).
c
      call lset_complement ( n, b, c )
      call lset_transpose_print ( n, c, '  C = ~ B:' )
c
c  D is the intersection of A and B.
c
      call lset_intersect ( n, a, b, d )
      call lset_transpose_print ( n, d, '  D = A intersect B:' )
c
c  E is the intersection of A and B.
c
      call lset_union ( n, a, b, e )
      call lset_transpose_print ( n, e, '  E = A union B:' )
c
c  F is the symmetric difference of A and B.
c
      call lset_xor ( n, a, b, f )
      call lset_transpose_print ( n, f, '  F = A xor B:' )
c
c  G is the complement of B with respect to A.
c  H is the complement of A with respect to B.
c
      call lset_complement_relative ( n, a, b, g )
      call lset_transpose_print ( n, g, '  G = A ~ B:' )

      call lset_complement_relative ( n, b, a, h )
      call lset_transpose_print ( n, h, '  H = B ~ A:' )
c
c  LSET_IS_MEMBER checks if an element is in a set.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  LSET_IS_MEMBER ( i, A ) reports whether i is a member of A'
      write ( *, '(a)' ) ' '

      do i = 10, 20
        if ( lset_is_member ( n, i, a ) ) then
          write ( *, '(2x,i2,a)' ) i, ' is a member of A.'
        else
          write ( *, '(2x,i2,a)' ) i, ' is not a member of A.'
        end if
      end do
c
c  LSET_IS_SUBSET checks whether a set is a subset.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  LSET_IS_SUBSET ( D, A ) reports whether D is a subset of A'
      write ( *, '(a)' ) ' '

      call lset_intersect ( n, a, b, d );

      if ( lset_is_subset ( n, d, a ) ) then
        write ( *, '(a)' ) '  ( A intersect B ) is a subset of A.'
      else
        write ( *, '(a)' ) '  ( A intersect B)  is not a subset of A.'
      end if
c
c  LSET_INSERT adds an item to a set.
c
      w_numeric(1) = 1
      w_numeric(2) = 11
      w_numeric(3) = 21
      w_numeric(4) = 31
      w_numeric(5) = 41
      call i4vec_to_lset ( w_num, w_numeric, n, w )
      call lset_transpose_print ( n, w, '  W: ' )

      x = 6
      call lset_insert ( n, x, w )
      call lset_transpose_print ( n, w, '  W := W + 6:' );

      x = 31
      call lset_delete ( n, x, w )
      call lset_transpose_print ( n, w, '  W := W - 31:' );

      y_numeric(1) = 16
      y_numeric(2) = 26
      y_numeric(3) = 36
      y_numeric(4) = 46
      call i4vec_to_lset ( y_num, y_numeric, n, y )
      call lset_union ( n, w, y, w )

      call lset_transpose_print ( n, w, 
     &  '  W := W union [16, 26, 36, 46]:' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests LSET_COLEX_RANK, _COLEX_SUCCESSOR, _COLEX_UNRANK, _ENUM.
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
      implicit none

      integer n
      parameter ( n = 5 )

      integer nsub
      integer rank
      integer rank_old
      logical t(n)
      character * ( 80 ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  All subsets of a set,'
      write ( *, '(a)' ) '  using the colexicographic ordering'
      write ( *, '(a)' ) '  with the LSET representation of a set.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  LSET_COLEX_RANK ranks,'
      write ( *, '(a)' ) '  LSET_COLEX_SUCCESSOR lists,'
      write ( *, '(a)' ) '  LSET_COLEX_UNRANK unranks.'
      write ( *, '(a)' ) '  LSET_ENUM enumerates.'
c
c  Enumerate.
c
      call lset_enum ( n, nsub )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  For N = ', n
      write ( *, '(a,i8)' ) '  the number of subsets is ', nsub
      write ( *, '(a)' ) ' '
c
c  List
c
      rank = -1

10    continue

        rank_old = rank

        call lset_colex_successor ( n, t, rank )

        if ( rank .le. rank_old ) then
          go to 20
        end if

        write ( title, '(a,i4)' ) '  Rank: ', rank
        call lset_transpose_print ( n, t, title )

      go to 10

20    continue
c
c  Unrank.
c
      rank = nsub / 3

      call lset_colex_unrank ( rank, n, t )

      write ( title, '(2x,a,i4)' ) 'The element of rank ', rank
      call lset_transpose_print ( n, t, title )
c
c  Rank.
c
      call lset_colex_rank ( n, t, rank )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) 
     &  '  The rank of this element is computed as ', rank

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests LSET_LEX_RANK, _LEX_SUCCESSOR, _LEX_UNRANK, _ENUM.
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
      implicit none

      integer n
      parameter ( n = 5 )

      integer nsub
      integer rank
      integer rank_old
      logical t(n)
      character * ( 80 ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  All subsets of a set,'
      write ( *, '(a)' ) '  using the lexicographic ordering,'
      write ( *, '(a)' ) '  with the LSET representation of a set.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  LSET_LEX_RANK ranks,'
      write ( *, '(a)' ) '  LSET_LEX_SUCCESSOR lists,'
      write ( *, '(a)' ) '  LSET_LEX_UNRANK unranks.'
      write ( *, '(a)' ) '  LSET_ENUM enumerates.'
c
c  Enumerate.
c
      call lset_enum ( n, nsub )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  For N = ', n
      write ( *, '(a,i8)' ) '  the number of subsets is ', nsub
      write ( *, '(a)' ) ' '
c
c  List
c
      rank = -1

10    continue

        rank_old = rank

        call lset_lex_successor ( n, t, rank )

        if ( rank .le. rank_old ) then
          go to 20
        end if

        write ( title, '(a,i4)' ) '  Rank: ', rank
        call lset_transpose_print ( n, t, title )

      go to 10

20    continue
c
c  Unrank.
c
      rank = nsub / 3

      call lset_lex_unrank ( rank, n, t )

      write ( title, '(2x,a,i4)' ) 'The element of rank ', rank
      call lset_transpose_print ( n, t, title )
c
c  Rank.
c
      call lset_lex_rank ( n, t, rank )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) 
     &  '  The rank of this element is computed as ', rank

      return
      end
