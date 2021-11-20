      program main

c*********************************************************************72
c
cc MAIN is the main program for TREEPACK.
c
c  Discussion:
c
c    TREEPACK_PRB tests the TREEPACK library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TREEPACK_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TREEPACK library.'

      call test005 ( )
      call test006 ( )
      call test01 ( )
      call test02 ( )
      call test025 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )

      call test10 ( )
      call test11 ( )
      call test12 ( )
      call test13 ( )
      call test14 ( )
      call test15 ( )
      call test16 ( )
      call test17 ( )
      call test18 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TREEPACK_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test005 ( )

c*********************************************************************72
c
cc TEST005 tests CATALAN and CATALAN_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2(0:10)
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST005'
      write ( *, '(a)' ) '  CATALAN computes Catalan numbers.'
      write ( *, '(a)' ) '  CATALAN_VALUES returns some exact values.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  N  exact C(I)  computed C(I)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call catalan_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call catalan ( n, c2 )

        write ( *, '(2x,i4,2i8)' ) n, c, c2(n)

      go to 10

20    continue

      return
      end
      subroutine test006 ( )

c*********************************************************************72
c
cc TEST006 tests CBT_TRAVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer depth
      parameter ( depth = 4 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST006'
      write ( *, '(a)' ) 
     &  '  CBT_TRAVERSE traverses a complete binary tree.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  For this demonstration, we simply print our path.'
      write ( *, '(a,i4)' ) '  The tree depth is ', depth
      write ( *, '(a)' ) ' '

      call cbt_traverse ( depth )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests PRUEFER_TO_TREE_ARC.
c
c  Discussion:
c
c    The tree is
c
c          5
c          |
c    2-3-6-8-1-9
c      |   |
c      7   4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 9 )

      integer code(nnode-2)
      integer inode(nnode-1)
      integer jnode(nnode-1)

      save code

      data code /
     &  1, 3, 8, 8, 3, 6, 8 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  PRUEFER_TO_TREE_ARC computes a tree from its Pruefer code.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          5'
      write ( *, '(a)' ) '          |'
      write ( *, '(a)' ) '    2-3-6-8-1-9'
      write ( *, '(a)' ) '      |   |'
      write ( *, '(a)' ) '      7   4'

      call i4vec_print ( nnode-2, code, '  The Pruefer code:' )

      call pruefer_to_tree_arc ( nnode, code, inode, jnode )
     
      call graph_arc_print ( nnode-1, inode, jnode, '  The graph:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests PRUEFER_TO_TREE_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 9 )

      integer code(nnode)
      integer itree(nnode)

      save code

      data code /
     &  1, 3, 8, 8, 3, 6, 8, 0, 0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  PRUEFER_TO_TREE_2 produces a tree from its Pruefer code'

      call i4vec_print ( nnode-2, code, '  The Pruefer code:' )

      call pruefer_to_tree_2 ( nnode, code, itree )
     
      call i4vec_print ( nnode-1, itree, 
     &  '  The edge list of the tree:' )
     
      return
      end
      subroutine test025 ( )

c*********************************************************************72
c
cc TEST025 tests PRUEFER_TO_TREE_2.
c
c  Discussion:
c
c    This example is used to illustrate the Nijenhuis and Wilf algorithm
c    LBLTRE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 4 )

      integer code(nnode)
      integer i
      integer itree(nnode)
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST025'
      write ( *, '(a)' ) 
     &  '  PRUEFER_TO_TREE_2 produces a tree from its Pruefer code'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Code      Tree'
      write ( *, '(a)' ) ' '
      do j = 1, nnode
        code(2) = j
        do i = 1, nnode
          code(1) = i
          call pruefer_to_tree_2 ( nnode, code, itree )
          write ( *, '(2x,i2,2x,i2,4x,i2,2x,i2,2x,i2)' ) 
     &      code(1:2), itree(1:3)
        end do
      end do
     
      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests TREE_ARC_TO_PRUEFER.
c
c  Discussion:
c
c    The tree is
c
c          5
c          |
c    2-3-6-8-1-9
c      |   |
c      7   4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode 
      parameter ( nnode = 9 )

      integer code(nnode-2)
      integer inode(nnode-1)
      integer jnode(nnode-1)

      save inode
      save jnode

      data inode /
     &  2, 3, 3, 6, 8, 8, 8, 1 /
      data jnode /
     &  3, 7, 6, 8, 4, 5, 1, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  TREE_ARC_TO_PRUEFER: Tree => Pruefer code'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          5'
      write ( *, '(a)' ) '          |'
      write ( *, '(a)' ) '    2-3-6-8-1-9'
      write ( *, '(a)' ) '      |   |'
      write ( *, '(a)' ) '      7   4'

      call graph_arc_print ( nnode-1, inode, jnode, '  The graph:' )
     
      call tree_arc_to_pruefer ( nnode, inode, jnode, code )

      call i4vec_print ( nnode-2, code, '  The Pruefer code:' )
     
      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests TREE_ARC_CENTER.
c
c  Discussion:
c
c    The tree is
c
c    2---3---6---8---1---9
c       /       / \
c      7       5   4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 9 )

      integer center(2)
      integer eccent
      integer i
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer parity

      save inode
      save jnode

      data inode /
     &  2, 3, 3, 6, 8, 8, 8, 1 /
      data jnode /
     &  3, 7, 6, 8, 4, 5, 1, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  TREE_ARC_CENTER computes the center of a tree.'

      call graph_arc_print ( nnode-1, inode, jnode, 
     &  '  The edge list of the tree:' )

      call tree_arc_center ( nnode, inode, jnode, center, eccent, 
     &  parity )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Parity = ', parity
      write ( *, '(a,i8)' ) '  Eccentricity is ', eccent

      if ( parity .eq. 0 ) then
        write ( *, '(a)' ) '  No center node (degenerate case).'
      else if ( parity .eq. 1 ) then
        write ( *, '(a,i8)' ) '  Center node: ', center(1)
      else
        write ( *, '(a,2i8)' ) '  Center nodes: ', center(1), center(2)
      end if

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests TREE_ARC_CENTER.
c
c  Discussion:
c
c    Compare:
c
c    2--1--3
c
c    1--2--3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 3 )

      integer center(2)
      integer eccent
      integer i
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer parity

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  TREE_ARC_CENTER computes the center of a tree.'

      inode(1) = 1
      inode(2) = 1
      jnode(1) = 2
      jnode(2) = 3

      call graph_arc_print ( nnode-1, inode, jnode, 
     &  '  The edge list of the tree:' )

      call tree_arc_center ( nnode, inode, jnode, center, eccent, 
     &  parity )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Parity = ', parity
      write ( *, '(a,i8)' ) '  Eccentricity is ', eccent

      if ( parity .eq. 0 ) then
        write ( *, '(a)' ) '  No center node (degenerate case).'
      else if ( parity .eq. 1 ) then
        write ( *, '(a,i8)' ) '  Center node: ', center(1)
      else
        write ( *, '(a,2i8)' ) '  Center nodes: ', center(1), center(2)
      end if

      inode(1) = 2
      inode(2) = 2
      jnode(1) = 1
      jnode(2) = 3

      call graph_arc_print ( nnode-1, inode, jnode, 
     &  '  The edge list of the tree:' )

      call tree_arc_center ( nnode, inode, jnode, center, eccent, 
     &  parity )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Parity = ', parity
      write ( *, '(a,i8)' ) '  Eccentricity is ', eccent

      if ( parity .eq. 0 ) then
        write ( *, '(a)' ) '  No center node (degenerate case).'
      else if ( parity .eq. 1 ) then
        write ( *, '(a,i8)' ) '  Center node: ', center(1)
      else
        write ( *, '(a,2i8)' ) '  Center nodes: ', center(1), center(2)
      end if

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests TREE_ARC_CENTER.
c
c  Discussion:
c
c    The tree is
c
c     1-----2-----3
c    /|\   / \   /|\
c   4 5 6 8  10 7 9 11
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 11 )

      integer center(2)
      integer eccent
      integer i
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer parity

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) 
     &  '  TREE_ARC_CENTER computes the center of a tree.'

      inode(1) = 1
      jnode(1) = 4
      inode(2) = 1
      jnode(2) = 5
      inode(3) = 1
      jnode(3) = 6

      inode(4) = 2
      jnode(4) = 8
      inode(5) = 2
      jnode(5) = 10

      inode(6) = 3
      jnode(6) = 7
      inode(7) = 3
      jnode(7) = 9
      inode(8) = 3
      jnode(8) = 11

      inode(9) = 1
      jnode(9) = 2
      inode(10) = 2
      jnode(10) = 3

      call graph_arc_print ( nnode-1, inode, jnode, 
     &  '  The edge list of the tree:' )

      call tree_arc_center ( nnode, inode, jnode, center, eccent, 
     &  parity )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Parity = ', parity
      write ( *, '(a,i8)' ) '  Eccentricity is ', eccent

      if ( parity .eq. 0 ) then
        write ( *, '(a)' ) '  No center node (degenerate case).'
      else if ( parity .eq. 1 ) then
        write ( *, '(a,i8)' ) '  Center node: ', center(1)
      else
        write ( *, '(a,2i8)' ) '  Center nodes: ', center(1), center(2)
      end if

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests TREE_ARC_DIAM.
c
c  Discussion:
c
c    The tree is:
c
c    2---3---6---8---1---9
c       /       / \
c      7       5   4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      integer nnode
      parameter ( nnode = 9 )

      integer diam
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer label(nnode)
      integer nnode1
      integer nnode2

      save inode
      save jnode

      data inode /
     &  2, 3, 3, 6, 8, 8, 8, 1 /
      data jnode /
     &  3, 7, 6, 8, 4, 5, 1, 9 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) 
     &  '  TREE_ARC_DIAM computes the diameter of a tree.'

      call graph_arc_print ( nnode-1, inode, jnode, 
     &  '  The edge list of the tree:' )

      call tree_arc_diam ( nnode, inode, jnode, diam, label, nnode1, 
     &  nnode2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  This tree has a diameter of ', diam
      write ( *, '(a,i8,a,i8)' ) 
     &  '  between nodes ', nnode1, ' and ', nnode2

      call i4vec_print ( nnode, label, '  Nodes and labels:' )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests TREE_ARC_RANDOM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 4 )

      integer i
      integer icode(nnode-2)
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  TREE_ARC_RANDOM produces a random labeled'
      write ( *, '(a)' ) '  tree and its Pruefer code.'
      write ( *, '(a)' ) ' '
     
      do i = 1, 5

        call tree_arc_random ( nnode, seed, icode, inode, jnode )

        call graph_arc_print ( nnode-1, inode, jnode, 
     &    '  The random tree:' )

        call i4vec_print ( nnode-2, icode, '  The Pruefer code:' )

      end do
     
      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 tests TREE_ENUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      integer num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) 
     &  '  TREE_ENUM enumerates the labeled trees on a given'
      write ( *, '(a)' ) '  number of nodes.'
      write ( *, '(a)' ) ' '

      do nnode = 0, 10

        call tree_enum ( nnode, num )

        write ( *, '(2x,i8,i10)' ) nnode, num

      end do
     
      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 tests TREE_PARENT_NEXT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 4 )

      integer iarray(nnode)
      integer icode(nnode)
      integer itree(nnode)
      logical more

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) 
     &  '  TREE_PARENT_NEXT finds all labeled trees of a given '
      write ( *, '(a)' ) '  order, and their Pruefer codes.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Pruefer code     Tree'
      write ( *, '(a)' ) ' '
     
      more = .false.
     
10    continue
     
        call tree_parent_next ( nnode, iarray, icode, itree, more )
     
        write ( *, '(2x,2i2,14x,3i2)' ) 
     &    icode(1:nnode-2), itree(1:nnode-1)

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue
     
      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 tests TREE_RB_ENUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      integer num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) 
     &  '  TREE_RB_ENUM enumerates the rooted binary trees on a '
      write ( *, '(a)' ) '  given number of nodes.'
      write ( *, '(a)' ) ' '

      do nnode = 0, 11

        call tree_rb_enum ( nnode, num )

        write ( *, '(2x,i8,2x,i8)' ) nnode, num

      end do
     
      return
      end
      subroutine test12 ( )

c*********************************************************************72
c
cc TEST12 tests TREE_RB_LEX_NEXT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 11 )

      integer a(n)
      integer i
      logical more

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST12'
      write ( *, '(a)' ) 
     &  '  TREE_RB_LEX_NEXT produces all rooted binary trees with'
      write ( *, '(a)' ) 
     &  '  a given number of nodes, in lexicographic order, using'
      write ( *, '(a)' ) '  the preorder traversal representation.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of nodes N = ', n
      write ( *, '(a)' ) ' '

      more = .false.
      i = 0

10    continue

        call tree_rb_lex_next ( n, a, more )

        if ( .not. more ) then 
          go to 20
        end if

        i = i + 1
        write ( *, '(2x,i2,2x,11i1)' ) i, a(1:n)

      go to 10

20    continue

      return
      end
      subroutine test13 ( )

c*********************************************************************72
c
cc TEST13 tests TREE_RB_LEX_NEXT, TREE_RB_TO_PARENT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 11 )

      integer a(n)
      integer i
      logical more
      integer parent(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST13'
      write ( *, '(a)' ) 
     &  '  TREE_RB_LEX_NEXT produces all rooted binary trees with'
      write ( *, '(a)' ) 
     &  '  a given number of nodes, in lexicographic order,'
      write ( *, '(a)' ) 
     &  '  using the preorder traversal representation.'
      write ( *, '(a)' ) 
     &  '  TREE_RB_TO_PARENT converts the preorder traversal form'
      write ( *, '(a)' ) 
     &  '  to the more comprehensible parent node representation.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The number of nodes N = ', n
      write ( *, '(a)' ) ' '

      more = .false.
      i = 0

10    continue

        call tree_rb_lex_next ( n, a, more )

        if ( .not. more ) then 
          go to 20
        end if

        call tree_rb_to_parent ( n, a, parent )

        i = i + 1
        write ( *, '(2x,i2,2x,11i3)' ) i, parent(1:n)

      go to 10

20    continue

      return
      end
      subroutine test14 ( )

c*********************************************************************72
c
cc TEST14 tests TREE_RB_YULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )

      integer a(n_max)
      integer i
      integer n
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST14'
      write ( *, '(a)' ) 
     &  '  TREE_RB_YULE carries out one step of the Yule model'
      write ( *, '(a)' ) 
     &  '  on a rooted binary tree stored in preorder traversal form.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Each call adds two children to an arbitary leaf.'

      do i = 1, 5

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Simulation ', i
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Nodes  Preorder code'
        write ( *, '(a)' ) ' '

        n = 0

10      continue

          call tree_rb_yule ( n, seed, a )

          write ( *, '(2x,i2,2x,11i1)' ) n, a(1:n)

          if ( n_max .lt. n + 2 ) then
            go to 20
          end if

        go to 10

20      continue

      end do

      return
      end
      subroutine test15 ( )

c*********************************************************************72
c
cc TEST15 tests TREE_ROOTED_CODE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 12 )

      integer code(2*nnode)
      integer parent(nnode)

      save parent

      data parent /
     &  0, 1, 1, 2, 2, 2, 3, 3, 5, 5, 6, 10 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST15'
      write ( *, '(a)' ) '  TREE_ROOTED_CODE: code of a rooted tree.'
      write ( *, '(a)' ) ' '

      call i4vec_print ( nnode, parent, '  Parent vector for tree:' )

      call tree_rooted_code ( nnode, parent, code )

      call i4vec_print ( 2*nnode, code, '  The tree code:' )

      return
      end
      subroutine test16 ( )

c*********************************************************************72
c
cc TEST16 tests TREE_ROOTED_DEPTH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 12 )

      integer depth
      integer depth_node(nnode)
      integer parent(nnode)

      save parent

      data parent /
     & 0, 1, 1, 2, 2, 2, 3, 3, 5, 5, 6, 10 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST16'
      write ( *, '(a)' ) '  TREE_ROOTED_DEPTH: depth of a rooted tree.'

      call i4vec_print ( nnode, parent, '  Parent vector for tree:' )

      call tree_rooted_depth ( nnode, parent, depth, depth_node )

      call i4vec_print ( nnode, depth_node, 
     &  '  Individual node depths:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Overall rooted tree depth: ', depth

      return
      end
      subroutine test17 ( )

c*********************************************************************72
c
cc TEST17 tests TREE_ROOTED_ENUM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 10 )

      integer ntree(nnode)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST17'
      write ( *, '(a)' ) 
     &  '  TREE_ROOTED_ENUM counts unlabeled rooted trees.'

      call tree_rooted_enum ( nnode, ntree )

      call i4vec_print ( nnode, ntree, 
     &  '  Number of trees with given number of nodes:' )

      return
      end
      subroutine test18 ( )

c*********************************************************************72
c
cc TEST18 tests TREE_ROOTED_RANDOM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnode
      parameter ( nnode = 5 )

      integer i
      integer itree(nnode)
      integer j
      integer ntree(nnode)
      integer seed

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST18'
      write ( *, '(a)' ) 
     &  '  TREE_ROOTED_RANDOM: random unlabeled rooted trees.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Selecting random trees, rooted at 1'
      write ( *, '(a,i4)' ) '  Number of nodes is NNODE = ', nnode
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Each tree is described by the nodes that'
      write ( *, '(a)' ) '  connect nodes 2 through NNODE.'
      write ( *, '(a)' ) ' '
      do i = 1, 5

        call tree_rooted_random ( nnode, seed, ntree, itree )

        write ( *, '(19i4)' ) itree(2:nnode)

      end do
     
      return
      end
