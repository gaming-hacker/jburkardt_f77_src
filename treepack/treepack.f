      subroutine catalan ( n, c )

c*********************************************************************72
c
cc CATALAN computes the Catalan numbers, from C(0) to C(N).
c
c  First values:
c
c     C(0)     1
c     C(1)     1
c     C(2)     2
c     C(3)     5
c     C(4)    14
c     C(5)    42
c     C(6)   132
c     C(7)   429
c     C(8)  1430
c     C(9)  4862
c    C(10) 16796
c
c  Formula:
c
c    C(N) = (2*N)c / ( (N+1) * (Nc) * (Nc) )
c         = 1 / (N+1) * COMB ( 2N, N )
c         = 1 / (2N+1) * COMB ( 2N+1, N+1).
c
c  Recursion:
c
c    C(N) = 2 * (2*N-1) * C(N-1) / (N+1)
c    C(N) = SUM ( I = 1 to N-1 ) C(I) * C(N-I)
c
c  Comments:
c
c    The Catalan number C(N) counts:
c
c    1) the number of binary trees on N vertices;
c    2) the number of ordered trees on N+1 vertices;
c    3) the number of full binary trees on 2N+1 vertices;
c    4) the number of well formed sequences of 2N parentheses;
c    5) number of ways 2N ballots can be counted, in order,
c       with N positive and N negative, so that the running sum
c       is never negative;
c    6) the number of standard tableaus in a 2 by N rectangular Ferrers diagram;
c    7) the number of monotone functions from [1..N} to [1..N} which
c       satisfy f(i) .le. i for all i,
c    8) the number of ways to triangulate a polygon with N+2 vertices.
c
c  Example:
c
c    N = 3
c
c    ()()()
c    ()(())
c    (()())
c    (())()
c    ((()))
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 August 1998
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Dennis Stanton, Dennis White,
c    Constructive Combinatorics,
c    Springer Verlag, New York, 1986.
c
c  Parameters:
c
c    Input, integer N, the number of Catalan numbers desired.
c
c    Output, integer C(0:N), the Catalan numbers from C(0) to C(N).
c
      implicit none

      integer n

      integer i
      integer c(0:n)

      c(0) = 1
c
c  The extra parentheses ensure that the integer division is
c  done AFTER the integer multiplication.
c
      do i = 1, n
        c(i) = ( c(i-1) * 2 * ( 2 * i - 1 ) ) / ( i + 1 )
      end do

      return
      end
      subroutine catalan_values ( n_data, n, c )

c*********************************************************************72
c
cc CATALAN_VALUES returns some values of the Catalan numbers for testing.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2007
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
c  Parameters:
c
c    Input/output, integer N_DATA.
c    On input, if N_DATA is 0, the first test data is returned, and N_DATA
c    is set to 1.  On each subsequent call, the input value of N_DATA is
c    incremented and that test data item is returned, if available.  When
c    there is no more test data, N_DATA is set to 0.
c
c    Output, integer N, the order of the Catalan number.
c
c    Output, integer C, the value of the Catalan number.
c
      implicit none

      integer n_max
      parameter ( n_max = 11 )

      integer c
      integer c_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)

      save c_vec
      save n_vec

      data c_vec /
     &  1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796 /

      data n_vec /
     &   0,  1,  2,  3,  4, 5,  6,  7,  8,  9,  10 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        c = 0
      else
        n = n_vec(n_data)
        c = c_vec(n_data)
      end if

      return
      end
      subroutine cbt_traverse ( depth )

c*********************************************************************72
c
cc CBT_TRAVERSE traverses a complete binary tree of given depth.
c
c  Discussion:
c
c    There will be 2^DEPTH terminal nodes of the complete binary tree.
c
c    This function traverses the tree, and prints out a binary code of 0's
c    and 1's each time it encounters a terminal node.  This results in a
c    printout of the binary digits from 0 to 2^DEPTH - 1.
c
c    The function is intended as a framework to be used to traverse a binary
c    tree.  Thus, in practice, a user would insert some action when a terminal
c    node is encountered.
c
c    Another use would occur when a combinatorial search is being made, for
c    example in a knapsack problem.  Each binary string then represents which
c    objects are to be included in the knapsack.  In that case, the traversal
c    could be speeded up by noticing cases where a nonterminal node has been
c    reached, but the knapsack is already full, in which case the only solution
c    uses none of the succeeding items, or overfull, in which case no solutions
c    exist that include this initial path segment.
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
c  Parameters:
c
c    Input, integer DEPTH, the depth of the tree.
c
      implicit none

      integer depth

      integer b(depth)
      integer direction
      integer DOWNLEFT
      parameter ( DOWNLEFT = 1 )
      integer i
      integer k
      integer p
      integer UP
      parameter ( UP = 3 )
      integer UPDOWNRIGHT
      parameter ( UPDOWNRIGHT = 2 )

      if ( depth .lt. 1 ) then
        return
      end if

      do i = 1, depth
        b(i) = 0
      end do
      p = 0
      direction = DOWNLEFT
      k = 0

10    continue
c
c  Try going in direction DOWNLEFT.
c
        if ( direction .eq. DOWNLEFT ) then
          p = p + 1
          b(p) = 0
          if ( p .lt. depth ) then
            write ( *, '(2x,a1,2x,4x,2x,10i1)' ) ' ', b(1:p)
          else
            write ( *, '(2x,a1,2x,i4,2x,10i1)' ) '(', k, b(1:depth)
            k = k + 1
            direction = UPDOWNRIGHT
          end if
        end if
c
c  Try going in direction UPDOWNRIGHT.
c
        if ( direction .eq. UPDOWNRIGHT ) then
          b(p) = + 1
          if ( p .lt. depth ) then
            write ( *, '(2x,a1,2x,4x,2x,10i1)' ) ' ', b(1:p)
            direction = DOWNLEFT
          else
            write ( *, '(2x,a1,2x,i4,2x,10i1)' ) ')', k, b(1:depth)
            k = k + 1
            direction = UP
          end if
        end if
c
c  Try going in direction UP.
c
        if ( direction .eq. UP ) then
          p = p - 1
          if ( 1 .le. p ) then
            write ( *, '(2x,a1,2x,4x,2x,10i1)' ) ' ', b(1:p)
            if ( b(p) .eq. 0 ) then
              direction = UPDOWNRIGHT
            end if
          else
            go to 20
          end if
        end if

      go to 10

20    continue

      return
      end
      subroutine graph_adj_edge_count ( adj, nnode, nedge )

c*********************************************************************72
c
cc GRAPH_ADJ_EDGE_COUNT counts the number of edges in a graph.
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
c  Parameters:
c
c    Input, integer ADJ(NNODE,NNODE), the adjacency information.
c    ADJ(I,J) is 1 if there is an edge from node I to node J.
c
c    Input, integer NNODE, the number of nodes.
c
c    Output, integer NEDGE, the number of edges in the graph.
c
      implicit none

      integer nnode

      integer adj(nnode,nnode)
      integer i
      integer j
      integer nedge

      nedge = 0

      do i = 1, nnode
        do j = 1, nnode

          if ( i .eq. j ) then
            nedge = nedge + 2 * adj(i,j)
          else
            nedge = nedge + adj(i,j)
          end if

        end do
      end do

      nedge = nedge / 2

      return
      end
      subroutine graph_adj_is_node_connected ( adj, nnode, result )

c*********************************************************************72
c
cc GRAPH_ADJ_IS_NODE_CONNECTED determines if a graph is nodewise connected.
c
c  Discussion:
c
c    A graph is nodewise connected if, from every node, there is a path
c    to any other node.
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
c  Parameters:
c
c    Input, integer ADJ(NNODE,NNODE), the adjacency matrix for the 
c    graph.  ADJ(I,J) is nonzero if there is an edge from node I to node J.
c
c    Input, integer NNODE, the number of nodes.
c
c    Output, integer RESULT.
c    0, the graph is not nodewise connected.
c    1, the graph is nodewise connected.
c
      implicit none

      integer nnode

      integer adj(nnode,nnode)
      integer found(nnode)
      integer i
      integer ihi
      integer ii
      integer ilo
      integer j
      integer jhi
      integer jlo
      integer k
      integer list(nnode)
      integer result
c
c  FOUND(I) is 1 if node I has been reached.
c  LIST(I) contains a list of the nodes as they are reached.
c
      do k = 1, nnode
        list(k) = 0
      end do

      do k = 1, nnode
        found(k) = 0
      end do
c
c  Start at node 1.
c
      found(1) = 1
      list(1) = 1
      ilo = 1
      ihi = 1
c
c  From the batch of nodes found last time, LIST(ILO:IHI),
c  look for unfound neighbors, and store their indices in LIST(JLO:JHI).
c
10    continue

        jlo = ihi + 1
        jhi = ihi

        do ii = ilo, ihi

          i = list(ii)

          do j = 1, nnode

            if ( adj(i,j) .ne. 0 .or. adj(j,i) .ne. 0 ) then

              if ( found(j) .eq. 0 ) then
                jhi = jhi + 1
                list(jhi) = j
                found(j) = 1
              end if

            end if

          end do

        end do
c
c  If no neighbors were found, exit.
c
        if ( jhi .lt. jlo ) then
          go to 20
        end if
c
c  If neighbors were found, then go back and find THEIR neighbors.
c
        ilo = jlo
        ihi = jhi
        
      go to 10

20    continue
c
c  No more neighbors were found.  Have we reached all nodes?
c
      if ( ihi .eq. nnode ) then
        result = 1
      else
        result = 0
      end if

      return
      end
      subroutine graph_adj_is_tree ( adj, nnode, result )

c*********************************************************************72
c
cc GRAPH_ADJ_IS_TREE determines whether a graph is a tree.
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
c  Parameters:
c
c    Input, integer ADJ(NNODE,NNODE), the adjacency matrix for the 
c    graph.  ADJ(I,J) is nonzero if there is an edge from node I to node J.
c
c    Input, integer NNODE, the number of nodes.
c
c    Output, integer RESULT.
c    0, the graph is not a tree.
c    1, the graph is a tree.
c
      implicit none

      integer nnode

      integer adj(nnode,nnode)
      integer nedge
      integer result

      if ( nnode .le. 1 ) then
        result = 1
        return
      end if
c
c  Every node must be connected to every other node.
c
      call graph_adj_is_node_connected ( adj, nnode, result )

      if ( result .eq. 0 ) then
        return
      end if
c
c  There must be exactly NNODE-1 edges.
c
      call graph_adj_edge_count ( adj, nnode, nedge )

      if ( nedge .eq. nnode - 1 ) then
        result = 1
      else
        result = 0
      end if

      return
      end
      subroutine graph_arc_degree ( nnode, nedge, inode, jnode, degree )

c*********************************************************************72
c
cc GRAPH_ARC_DEGREE determines the degree of the nodes of a graph.
c
c  Discussion:
c
c    The degree of a node is the number of edges that include the node.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer NEDGE, the number of edges.
c
c    Input, integer INODE(NEDGE), JNODE(NEDGE), the pairs of nodes
c    that form the edges.
c
c    Output, integer DEGREE(NNODE), the degree of each node, 
c    that is, the number of edges that include the node.
c
      implicit none

      integer nedge
      integer nnode

      integer degree(nnode)
      integer i
      integer inode(nedge)
      integer j
      integer jnode(nedge)
      integer n

      do j = 1, nnode
        degree(j) = 0
      end do

      do i = 1, nedge

        n = inode(i)
        if ( 1 .le. n .and. n .le. nnode ) then
          degree(n) = degree(n) + 1
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'GRAPH_ARC_DEGREE - Fatal error!'
          write ( *, '(a,i8)' ) '  Out-of-range node value = ', n
          stop
        end if

        n = jnode(i)
        if ( 1 .le. n .and. n .le. nnode ) then
          degree(n) = degree(n) + 1
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'GRAPH_ARC_DEGREE - Fatal error!'
          write ( *, '(a,i8)' ) '  Out-of-range node value = ', n
          stop
        end if

      end do

      return
      end
      subroutine graph_arc_is_tree ( nedge, inode, jnode, nnode, 
     &  result )

c*********************************************************************72
c
cc GRAPH_ARC_IS_TREE determines whether a graph is a tree.
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
c  Parameters:
c
c    Input, integer INODE(NEDGE), JNODE(NEDGE).  INODE(I) and 
c    JNODE(I) are the start and end nodes of the I-th edge of the graph G.
c
c    Input, integer NEDGE, the number of edges in the graph G.
c
c    Input, integer NNODE, the number of nodes.
c
c    Output, integer RESULT.
c    0, the graph is not a tree.
c    1, the graph is a tree.
c
      implicit none

      integer nedge
      integer nnode

      integer adj(nnode,nnode)
      integer inode(nedge)
      integer jnode(nedge)
      integer nnode2
      integer result

      call graph_arc_to_graph_adj ( nedge, inode, jnode, adj, nnode2 )

      call graph_adj_is_tree ( adj, nnode, result )

      return
      end
      subroutine graph_arc_node_count ( nedge, inode, jnode, nnode )

c*********************************************************************72
c
cc GRAPH_ARC_NODE_COUNT counts the number of nodes in a graph.
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
c  Parameters:
c
c    Input, integer NEDGE, the number of edges.
c
c    Input, integer INODE(NEDGE), JNODE(NEDGE).  INODE(I) and 
c    JNODE(I) are the start and end nodes of the I-th edge.
c
c    Output, integer NNODE, the number of distinct nodes.
c
      implicit none

      integer nedge

      integer i
      integer i4vec_max
      integer iedge
      integer inode(nedge)
      integer jnode(nedge)
      integer knode(2*nedge)
      integer nnode
c
c  Copy all the node labels into KNODE,
c  sort KNODE,
c  count the unique entries.  
c
c  That's NNODE.
c
      do i = 1, nedge
        knode(i) = inode(i)
      end do

      do i = 1, nedge
        knode(nedge+i) = jnode(i)
      end do

      call i4vec_sort_heap_a ( 2*nedge, knode )

      call i4vec_sorted_unique_count ( 2*nedge, knode, nnode )

      return
      end
      subroutine graph_arc_node_max ( nedge, inode, jnode, node_max ) 

c*********************************************************************72
c
cc GRAPH_ARC_NODE_MAX determines the maximum node label.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NEDGE, the number of edges.
c
c    Input, integer INODE(NEDGE), JNODE(NEDGE).  INODE(I) and 
c    JNODE(I) are the start and end nodes of the I-th edge.
c
c    Output, integer NODE_MAX, the maximum node index.
c
      implicit none

      integer nedge

      integer i4vec_max
      integer inode(nedge)
      integer jnode(nedge)
      integer node_max

      node_max = max ( i4vec_max ( nedge, inode ),
     &                 i4vec_max ( nedge, jnode ) )

      return
      end
      subroutine graph_arc_print ( nedge, inode, jnode, title )

c*********************************************************************72
c
cc GRAPH_ARC_PRINT prints out a graph from an edge list.
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
c  Parameters:
c
c    Input, integer NEDGE, the number of edges.
c
c    Input, integer INODE(NEDGE), JNODE(NEDGE), the beginning 
c    and end nodes of the edges.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer nedge

      integer i
      integer inode(nedge)
      integer jnode(nedge)
      character * ( * ) title

      if ( len_trim ( title ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) trim ( title )
      end if

      write ( *, '(a)' ) ' '

      do i = 1, nedge
        write ( *, '(i8,4x,2i8)' ) i, inode(i), jnode(i)
      end do

      return
      end
      subroutine graph_arc_to_graph_adj ( nedge, inode, jnode, adj,
     &  nnode )

c*********************************************************************72
c
cc GRAPH_ARC_TO_GRAPH_ADJ converts an arc list graph to an adjacency graph.
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
c  Parameters:
c
c    Input, integer NEDGE, the number of edges.
c
c    Input, integer INODE(NEDGE), JNODE(NEDGE), the edge array for 
c    an undirected graph.  The I-th edge connects nodes INODE(I) and JNODE(I).
c
c    Output, integer ADJ(NNODE,NNODE), the adjacency information.
c
c    Output, integer NNODE, the number of nodes.
c
      implicit none

      integer nedge

      integer adj(nedge+1,nedge+1)
      integer i
      integer inode(nedge)
      integer j
      integer jnode(nedge)
      integer k
      integer nnode
c
c  Determine the number of nodes.
c
      call graph_arc_node_count ( nedge, inode, jnode, nnode )

      do j = 1, nnode
        do i = 1, nnode
          adj(i,j) = 0
        end do
      end do

      do k = 1, nedge
        i = inode(k)
        j = jnode(k)
        adj(i,j) = 1
        adj(j,i) = 1
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
      subroutine i4mat_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_PRINT prints an I4MAT.
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
c    30 June 2003
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
c    Input, integer A(M,N), the matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer ihi
      integer ilo
      integer jhi
      integer jlo
      character*(*) title

      ilo = 1
      ihi = m
      jlo = 1
      jhi = n

      call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

      return
      end
      subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc I4MAT_PRINT_SOME prints some of an I4MAT.
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
c    04 November 2003
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
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*(8) ctemp(incx)
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
      character*(*) title

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
          write ( ctemp(j2), '(i8)' ) j
        end do

        write ( *, '(''  Col '',10a8)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine i4vec_heap_d ( n, a )

c*********************************************************************72
c
cc I4VEC_HEAP_D reorders an I4VEC into an descending heap.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    A descending heap is an array A with the property that, for every index J,
c    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
c    2*J and 2*J+1 are legal).
c
c                  A(1)
c                /      \
c            A(2)         A(3)
c          /     \        /  \
c      A(4)       A(5)  A(6) A(7)
c      /  \       /   \
c    A(8) A(9) A(10) A(11)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2008
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
c    Input, integer N, the size of the input array.
c
c    Input/output, integer A(N).
c    On input, an unsorted array.
c    On output, the array has been reordered into a heap.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer ifree
      integer key
      integer m
c
c  Only nodes N/2 down to 1 can be "parent" nodes.
c
      do i = n/2, 1, -1
c
c  Copy the value out of the parent node.
c  Position IFREE is now "open".
c
        key = a(i)
        ifree = i

10      continue
c
c  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
c  IFREE.  (One or both may not exist because they exceed N.)
c
          m = 2 * ifree
c
c  Does the first position exist?
c
          if ( n .lt. m ) then
            go to 20
          end if
c
c  Does the second position exist?
c
          if ( m + 1 .le. n ) then
c
c  If both positions exist, take the larger of the two values,
c  and update M if necessary.
c
            if ( a(m) .lt. a(m+1) ) then
              m = m + 1
            end if

          end if
c
c  If the large descendant is larger than KEY, move it up,
c  and update IFREE, the location of the free position, and
c  consider the descendants of THIS position.
c
          if ( a(m) .le. key ) then
            go to 20
          end if

          a(ifree) = a(m)
          ifree = m

        go to 10
c
c  Once there is no more shifting to do, KEY moves into the free spot IFREE.
c
20      continue

        a(ifree) = key

      end do

      return
      end
      subroutine i4vec_indicator ( n, a )

c*********************************************************************72
c
cc I4VEC_INDICATOR sets an I4VEC to the indicator vector.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements of A.
c
c    Output, integer A(N), the array to be initialized.
c
      implicit none

      integer n

      integer a(n)
      integer i

      do i = 1, n
        a(i) = i
      end do

      return
      end
      function i4vec_max ( n, a )

c*********************************************************************72
c
cc I4VEC_MAX computes the maximum element of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2007
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
c    Output, integer AMAX, the value of the largest entry.
c
      implicit none

      integer n

      integer a(n)
      integer amax
      integer i
      integer i4vec_max

      amax = a(1)

      do i = 2, n
        amax = max ( amax, a(i) )
      end do

      i4vec_max = amax

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
      subroutine i4vec_sort_heap_a ( n, a )

c*********************************************************************72
c
cc I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2008
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
c    Input, integer N, the number of entries in the array.
c
c    Input/output, integer A(N).
c    On input, the array to be sorted;
c    On output, the array has been sorted.
c
      implicit none

      integer n

      integer a(n)
      integer n1
      integer t

      if ( n .le. 1 ) then
        return
      end if
c
c  1: Put A into descending heap form.
c
      call i4vec_heap_d ( n, a )
c
c  2: Sort A.
c
c  The largest object in the heap is in A(1).
c  Move it to position A(N).
c
      t    = a(1)
      a(1) = a(n)
      a(n) = t
c
c  Consider the diminished heap of size N1.
c
      do n1 = n - 1, 2, -1
c
c  Restore the heap structure of A(1) through A(N1).
c
        call i4vec_heap_d ( n1, a )
c
c  Take the largest object from A(1) and move it to A(N1).
c
        t    = a(1)
        a(1) = a(n1)
        a(n1) = t

      end do

      return
      end
      subroutine i4vec_sorted_unique_count ( n, a, unique_num )

c*********************************************************************72
c
cc I4VEC_SORTED_UNIQUE_COUNT counts the unique elements in a sorted I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    Because the array is sorted, this algorithm is O(N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements of A.
c
c    Input, integer A(N), the sorted array to examine.
c
c    Output, integer UNIQUE_NUM, the number of unique elements of A.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer unique_num

      if ( n .lt. 1 ) then
        unique_num = 0
        return
      end if

      unique_num = 1

      do i = 2, n

        if ( a(i-1) .ne. a(i) ) then
          unique_num = unique_num + 1
        end if

      end do

      return
      end
      subroutine pruefer_to_tree_arc ( nnode, iarray, inode, jnode )

c*********************************************************************72
c
cc PRUEFER_TO_TREE_ARC is given a Pruefer code, and computes the tree.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Reference:
c
c    Dennis Stanton, Dennis White,
c    Constructive Combinatorics,
c    Springer Verlag, New York, 1986.
c
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer IARRAY(NNODE-2), the Pruefer code of the tree.
c
c    Output, integer INODE(NNODE-1), JNODE(NNODE-1), the edge
c    array of the tree.  The I-th edge joins nodes INODE(I) and JNODE(I).
c
      implicit none

      integer nnode

      integer i
      integer iarray(nnode-2)
      integer ii
      integer inode(nnode-1)
      integer iwork(nnode)
      integer j
      integer jnode(nnode-1)
c
c  Initialize IWORK(I) to count the number of neighbors of node I.
c  The Pruefer code uses each node one less time than its total
c  number of neighbors.
c
      do i = 1, nnode
        iwork(i) = 1
      end do
   
      do i = 1, nnode-2
        iwork(iarray(i)) = iwork(iarray(i)) + 1
      end do
c
c  Now process each entry in the Pruefer code.
c
      do i = 1, nnode - 2
 
        ii = 0
        do j = 1, nnode
          if ( iwork(j) .eq. 1 ) then
            ii = j
          end if
        end do
     
        inode(i) = ii
        jnode(i) = iarray(i)
        iwork(ii) = 0
        iwork(iarray(i)) = iwork(iarray(i)) - 1
     
      end do
     
      inode(nnode-1) = iarray(nnode-2)
     
      if ( iarray(nnode-2) .ne. 1 ) then
        jnode(nnode-1) = 1
      else
        jnode(nnode-1) = 2
      end if
     
      return
      end
      subroutine pruefer_to_tree_2 ( nnode, iarray, itree )

c*********************************************************************72
c
cc PRUEFER_TO_TREE_2 produces the edge list of a tree from its Pruefer code.
c
c  Discussion:
c
c    One can thus exhibit all trees on N nodes, produce
c    one at random, find the M-th one on the list, etc, by
c    manipulating the Pruefer codes.
c
c    For every labeled tree on N nodes, there is a unique N-2 tuple
c    of integers A1 through AN-2, with each A between 1 and N.  There
c    are N^(N-2) such sequences, and each one is associated with exactly
c    one tree.
c
c    This routine apparently assumes that the Pruefer code is
c    generated by taking the LOWEST labeled terminal node each time.
c    This is not consistent with PRUEFER_TO_TREE and TREE_TO_PRUEFER.
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
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis. Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer NNODE, number of nodes in desired tree.
c
c    Input, integer IARRAY(NNODE).  IARRAY(I), I = 1, NNODE-2 
c    is the Pruefer code for the tree.
c
c    Output, integer ITREE(NNODE); the I-th edge of the tree
c    joins nodes I and ITREE(I).
c
      implicit none

      integer nnode

      integer i
      integer ir
      integer itree(nnode)
      integer iarray(nnode)
      integer j
      integer k
      integer kp
      integer l

      do i = 1, nnode
        itree(i) = 0
      end do
   
      do i = nnode-2, 1, -1
     
        l = iarray(i)
     
        if ( itree(l) .eq. 0 ) then
          iarray(i) = - l
          itree(l) = - 1
        end if
     
      end do
     
      iarray(nnode-1) = nnode
c
c  Find next index K so that ITREE(K) is 0.
c
      k = 1
      j = 0

10    continue

      if ( itree(k) .ne. 0 ) then
        k = k + 1
        go to 10
      end if
     
      kp = k

20    continue
     
        j = j + 1
        ir = abs ( iarray(j) )
        itree(kp) = ir
     
        if ( j .eq. nnode - 1 ) then
          go to 50
        end if
     
        if ( 0 .lt. iarray(j) ) then
30        continue
          if ( itree(k) .ne. 0 ) then
            k = k + 1
            go to 30
          end if
          kp = k
          go to 10
        end if
     
        if ( k .lt. ir ) then
          itree(ir) = 0
40        continue
          if ( itree(k) .ne. 0 ) then
            k = k + 1
            go to 40
          end if
          kp = k
          go to 10
        end if
     
        kp = ir

      go to 20

50    continue
c
c  Restore the signs of IARRAY.
c
      do i = 1, nnode - 2
        iarray(i) = abs ( iarray(i) )
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

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

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
      subroutine tree_arc_center ( nnode, inode, jnode, center, eccent, 
     &  parity )

c*********************************************************************72
c
cc TREE_ARC_CENTER computes the center, eccentricity, and parity of a tree.
c
c  Discussion:
c
c    A tree is an undirected graph of N nodes, which uses N-1 edges,
c    and is connected.  
c
c    A graph with N-1 edges is not guaranteed to be a tree, and so this
c    routine must first check that condition before proceeding.
c
c    The edge distance between two nodes I and J is the minimum number of
c    edges that must be traversed in a path from I and J.
c
c    The eccentricity of a node I is the maximum edge distance between
c    node I and the other nodes J in the graph.
c
c    The radius of a graph is the minimum eccentricity over all nodes
c    in the graph.
c
c    The diameter of a graph is the maximum eccentricity over all nodes
c    in the graph.
c
c    The center of a graph is the set of nodes whose eccentricity is 
c    equal to the radius, that is, the set of nodes of minimum eccentricity.
c
c    For a tree, the center is either a single node, or a pair of
c    neighbor nodes.
c
c    The parity of the tree is 1 if the center is a single node, or 2 if
c    the center is 2 nodes.
c
c    The center of a tree can be found by removing all "leaves", that is,
c    nodes of degree 1.  This step is repeated until only 1 or 2 nodes
c    are left.
c
c    Thanks to Alexander Sax for pointing out that a previous version of the
c    code was failing when the tree had an odd parity, that is, a single
c    center node, 15 April 2013.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer INODE(NNODE-1), JNODE(NNODE-1), the edges of
c    the tree.  Edge I connects nodes INODE(I) and JNODE(I).
c
c    Output, integer CENTER(2).  CENTER(1) is the index of the
c    first node in the center.  CENTER(2) is 0 if there is only one node
c    in the center, or else the index of the second node.
c
c    Output, integer ECCENT, the eccentricity of the nodes in 
c    the center, and the radius of the the tree.
c
c    Output, integer PARITY, the parity of the tree, which is
c    normally 1 or 2.
c
      implicit none

      integer nnode

      integer center(2)
      integer degree(nnode)
      integer eccent
      integer i
      integer iedge
      integer ileaf
      integer inode(nnode-1)
      integer j
      integer jnode(nnode-1)
      integer list(nnode)
      integer nedge
      integer nleaf
      integer nnode2
      integer parity
      integer result

      eccent = 0
      center(1) = 0
      center(2) = 0
      parity = 0

      if ( nnode .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ARC_CENTER - Fatal error!'
        write ( *, '(a)' ) '  NNODE .le. 0.'
        stop
      else if ( nnode .eq. 1 ) then
        eccent = 0
        center(1) = 1
        center(2) = 0
        parity = 1
        return
      else if ( nnode .eq. 2 ) then
        eccent = 1
        center(1) = 1
        center(2) = 2
        parity = 2
        return
      end if
c
c  Is this graph really a tree?
c
      nedge = nnode - 1
      call graph_arc_is_tree ( nedge, inode, jnode, nnode, result )

      if ( result .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ARC_CENTER - Fatal error!'
        write ( *, '(a)' ) '  This graph is NOT a tree.'
        stop
      end if
c
c  Compute the degrees.
c
      call graph_arc_degree ( nnode, nedge, inode, jnode, degree )
c
c  Defoliate the tree.
c
      nnode2 = nnode

10    continue

        eccent = eccent + 1
c
c  Find and mark the leaves.
c
        nleaf = 0

        do i = 1, nnode

          if ( degree(i) .eq. 1 ) then
            nleaf = nleaf + 1
            list(nleaf) = i
          end if

        end do
c
c  Delete the leaves.
c
        do ileaf = 1, nleaf

          i = list(ileaf)

          iedge = 0
          j = 0

20        continue

            iedge = iedge + 1

            if ( nedge .lt. iedge ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'TREE_ARC_CENTER - Fatal error!'
              write ( *, '(a)' ) '  Data or algorithm failure.'
              stop
            end if

            if ( inode(iedge) .eq. i ) then
              j = jnode(iedge)
              inode(iedge) = - inode(iedge)
              jnode(iedge) = - jnode(iedge)
            else if ( jnode(iedge) .eq. i ) then
              j = inode(iedge)
              inode(iedge) = - inode(iedge)
              jnode(iedge) = - jnode(iedge)
            end if

            if ( j .ne. 0 ) then
              go to 30
            end if

          go to 20

30        continue

          degree(i) = -1
          nnode2 = nnode2 - 1
          degree(j) = degree(j) - 1
c
c  If the other node has degree 0, we must have just finished
c  stripping all leaves from the tree, leaving a single node.
c  Don't kill it here.  It is our odd center.
c
c         if ( degree(j) .eq. 0 ) then
c           nnode2 = nnode2 - 1
c         end if

        end do
c
c  Find the remaining nodes.
c
        nnode2 = 0

        do i = 1, nnode

          if ( 0 .le. degree(i) ) then
            nnode2 = nnode2 + 1
            list(nnode2) = i
          end if

        end do
c
c  If at least 3, more pruning is required.
c
        if ( nnode2 .lt. 3 ) then
          go to 40
        end if

      go to 10

40    continue
c
c  If only one or two nodes left, we are done.
c
      parity = nnode2

      do i = 1, nnode2
        center(i) = list(i)
      end do

      do i = 1, nedge
        inode(i) = abs ( inode(i) )
      end do

      do i = 1, nedge
        jnode(i) = abs ( jnode(i) )
      end do

      return
      end
      subroutine tree_arc_diam ( nnode, inode, jnode, diam, label, 
     &  n1, n2 )

c*********************************************************************72
c
cc TREE_ARC_DIAM computes the "diameter" of a tree.
c
c  Discussion:
c
c    A tree is an undirected graph of N nodes, which uses N-1 edges,
c    and is connected.  
c
c    A graph with N-1 edges is not guaranteed to be a tree, and so this
c    routine must first check that condition before proceeding.
c
c    The diameter of a graph is the length of the longest possible
c    path that never repeats an edge.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer INODE(NNODE-1), JNODE(NNODE-1), the edges 
c    of the tree.  Edge I connects nodes INODE(I) and JNODE(I).
c
c    Output, integer DIAM, the length of the longest path 
c    in the tree.
c
c    Output, integer LABEL(NNODE), marks the path between 
c    nodes N1 and N2.  Node I is in this path if LABEL(I) is 1.
c
c    Output, integer N1, N2, the indices of two nodes in the 
c    tree which are separated by DIAM edges.
c
      implicit none

      integer nnode

      integer degree(nnode)
      integer diam
      integer i
      integer inode(nnode-1)
      integer invals
      integer j
      integer jnode(nnode-1)
      integer k
      integer kstep
      integer label(nnode)
      integer n1
      integer n2
      integer nabe
      integer nedge
      integer result

      if ( nnode .le. 0 ) then
        diam = 0
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ARC_DIAM - Fatal error!'
        write ( *, '(a)' ) '  NNODE .le. 0.'
        stop
      end if

      if ( nnode .eq. 1 ) then
        diam = 0
        n1 = 1
        n2 = 1
        return
      end if
c
c  Is this graph really a tree?
c
      nedge = nnode - 1
      call graph_arc_is_tree ( nedge, inode, jnode, nnode, result )

      if ( result .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ARC_DIAM - Fatal error!'
        write ( *, '(a)' ) '  This graph is NOT a tree.'
        stop
      end if

      call i4vec_indicator ( nnode, label )
c
c  On step K:
c
c    Identify the terminal and interior nodes.
c
c    If there are no interior nodes left, 
c
c      then there are just two nodes left at all.  The diameter is 2*K-1, 
c      and a maximal path extends between the nodes whose labels are 
c      contained in the two remaining terminal nodes.
c
c    Else
c
c      The label of each terminal node is passed to its interior neighbor.
c      If more than one label arrives, take any one.
c
c      The terminal nodes are removed.
c
      kstep = 0

10    continue

        kstep = kstep + 1
c
c  Compute the degree of each node.
c
        do i = 1, nnode
          degree(i) = 0        
        end do

        do j = 1, nedge

          k = inode(j)
          if ( 0 .lt. k ) then
            degree(k) = degree(k) + 1
          end if

          k = jnode(j)
          if ( 0 .lt. k ) then
            degree(k) = degree(k) + 1
          end if

        end do
c
c  Count the number of interior nodes.
c
        invals = 0
        do i = 1, nnode
          if ( 1 .lt. degree(i) ) then
            invals = invals + 1
          end if
        end do
c
c  If there are 1 or 0 interior nodes, it's time to stop.
c
        if ( invals .eq. 1 ) then

          diam = 2 * kstep
          go to 20
        
        else if ( invals .eq. 0 ) then

          diam = 2 * kstep - 1
          go to 20

        end if
c
c  If there are at least two interior nodes, then chop off the 
c  terminal nodes and pass their labels inward.
c
        do k = 1, nnode

          if ( degree(k) .eq. 1 ) then

            do j = 1, nedge

              if ( inode(j) .eq. k ) then
                nabe = jnode(j)
                label(nabe) = label(k)
                inode(j) = - inode(j)
                jnode(j) = - jnode(j)
              else if ( jnode(j) .eq. k ) then
                nabe = inode(j)
                label(nabe) = label(k)
                inode(j) = - inode(j)
                jnode(j) = - jnode(j)
              end if

            end do

          end if

        end do

      go to 10

20    continue
c
c  Now get the labels from two of the remaining terminal nodes.
c  The nodes represented by these labels will be a diameter apart.
c
      n1 = 0
      n2 = 0

      do i = 1, nnode
        if ( degree(i) .eq. 1 ) then
          if ( n1 .eq. 0 ) then
            n1 = label(i)
          else if ( n2 .eq. 0 ) then
            n2 = label(i)
          end if
        end if
      end do
c
c  Set the labels of the interior node (if any) and nodes marked
c  N1 and N2 to 1, and all others to 0.  This will label the nodes on the path.
c
      if ( invals .eq. 1 ) then

        do i = 1, nnode
          if ( 1 .lt. degree(i) ) then
            label(i) = 1
          end if
        end do

      end if

      do i = 1, nnode
        if ( label(i) .eq. n1 .or. label(i) .eq. n2 ) then
          label(i) = 1
        else
          label(i) = 0
        end if
      end do
c
c  Clean up the arrays.
c
      do j = 1, nedge
        inode(j) = abs ( inode(j) )
        k = inode(j)
        jnode(j) = abs ( jnode(j) )
        k = jnode(j)
      end do

      return
      end
      subroutine tree_arc_random ( nnode, seed, code, inode, jnode )

c*********************************************************************72
c
cc TREE_ARC_RANDOM selects a random labeled tree and its Pruefer code.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input/output, integer SEED, a seed for the random number 
c    generator.
c
c    Output, integer CODE(NNODE-2), the Pruefer code for the 
c    labeled tree.
c
c    Output, integer INODE(NNODE-1), JNODE(NNODE-1), the edge 
c    array for the tree.
c
      implicit none

      integer nnode

      integer code(nnode-2)
      integer i
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer seed

      if ( nnode .le. 0  ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ARC_RANDOM - Fatal error!'
        write ( *, '(a,i8)' ) '  NNODE = ', nnode
        write ( *, '(a)' ) '  but NNODE must be at least 1.'
        stop
      end if

      if ( nnode .le. 2 ) then
        return
      end if

      call vec_random ( nnode-2, nnode, seed, code )
     
      do i = 1, nnode - 2
        code(i) = code(i) + 1
      end do
   
      call pruefer_to_tree_arc ( nnode, code, inode, jnode )
     
      return
      end
      subroutine tree_arc_to_pruefer ( nnode, inode, jnode, code )

c*********************************************************************72
c
cc TREE_ARC_TO_PRUEFER is given a labeled tree, and computes its Pruefer code.
c
c  Discussion:
c
c    A tree is an undirected graph of N nodes, which uses N-1 edges,
c    and is connected.  
c
c    A graph with N-1 edges is not guaranteed to be a tree, and so this
c    routine must first check that condition before proceeding.
c
c    The Pruefer code is a correspondence between all labeled trees of
c    N nodes, and all list of N-2 integers between 1 and N (with repetition
c    allowed).  The number of labeled trees on N nodes is therefore N**(N-2).
c
c    The Pruefer code is constructed from the tree as follows:
c
c    A terminal node on the tree is defined as a node with only one neighbor.
c
c    Consider the set of all terminal nodes on the tree.  Take the one
c    with the highest label, I.  Record the label of its neighbor, J.
c    Delete node I and the edge between node I and J.
c
c    J is the first entry in the Pruefer code for the tree.   Repeat
c    the operation a total of N-2 times to get the complete code.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Reference:
c
c    Dennis Stanton, Dennis White,
c    Constructive Combinatorics,
c    Springer Verlage, New York, 1986.
c
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer INODE(NNODE-1), JNODE(NNODE-1), the edge array 
c    of the tree.  The I-th edge joins nodes INODE(I) and JNODE(I).
c
c    Output, integer CODE(NNODE-2), the Pruefer code of the tree.
c
      implicit none

      integer nnode

      integer code(nnode-2)
      integer degree(nnode)
      integer i
      integer i2
      integer iterm
      integer inode(nnode-1)
      integer j
      integer jnode(nnode-1)
      integer jsave
      integer nedge
      integer result
c
c  Is this graph really a tree?
c
      nedge = nnode - 1
      call graph_arc_is_tree ( nedge, inode, jnode, nnode, result )

      if ( result .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ARC_TO_PRUEFER - Fatal error!'
        write ( *, '(a)' ) '  This graph is NOT a tree.'
        stop
      end if
c
c  Compute the degree of each node.
c
      nedge = nnode - 1
      call graph_arc_degree ( nnode, nedge, inode, jnode, degree )
c
c  Compute the next term of the Pruefer code.
c
      do i = 1, nnode - 2
c
c  Find the terminal node with the highest label.
c
        iterm = 0
     
        do j = 1, nnode
          if ( degree(j) .eq. 1 ) then
            iterm = j
          end if
        end do
c
c  Find the edge that includes this node, and note the
c  index of the other node.
c
        do j = 1, nnode - 1

          jsave = j
     
          if ( inode(j) .eq. iterm ) then
            i2 = 2
            go to 10
          else if ( jnode(j) .eq. iterm ) then
            i2 = 1
            go to 10
          end if
     
        end do

10      continue
c
c  Delete the edge from the tree.
c
        degree(inode(jsave)) = degree(inode(jsave)) - 1
        degree(jnode(jsave)) = degree(jnode(jsave)) - 1
c
c  Add the neighbor of the node to the Pruefer code.
c
        if ( i2 .eq. 1 ) then
          code(i) = inode(jsave)
        else
          code(i) = jnode(jsave)
        end if
c
c  Negate the nodes in the edge list to mark them as used.
c
        inode(jsave) = - inode(jsave)
        jnode(jsave) = - jnode(jsave)
     
      end do
c
c  Before returning, restore the original form of the edge list.
c
      do i = 1, nnode - 1
        inode(i) = abs ( inode(i) )
      end do

      do i = 1, nnode - 1
        jnode(i) = abs ( jnode(i) )
      end do
   
      return
      end
      subroutine tree_enum ( nnode, ntree )

c*********************************************************************72
c
cc TREE_ENUM enumerates the labeled trees on NNODE nodes.
c
c  Discussion:
c
c    The formula is due to Cauchy.
c
c  Example:
c
c    NNODE      NTREE
c
c    0              1
c    1              1
c    2              1
c    3              3
c    4             16
c    5            125
c    6           1296
c    7          16807
c    8         262144
c    9        4782969
c   10      100000000
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes in each tree.
c    NNODE must normally be at least 3, but for this routine,
c    any value of NNODE is allowed.  Values of NNODE greater than 10
c    will probably overflow.
c
c    Output, integer NTREE, the number of distinct labeled trees.
c
      implicit none

      integer nnode
      integer ntree

      if ( nnode .lt. 0 ) then
        ntree = 0
      else if ( nnode .eq. 0 ) then
        ntree = 1
      else if ( nnode .eq. 1 ) then
        ntree = 1
      else if ( nnode .eq. 2 ) then
        ntree = 1
      else
        ntree = nnode ** ( nnode - 2 )
      end if

      return
      end
      subroutine tree_parent_next ( nnode, iarray, code, itree, more )

c*********************************************************************72
c
cc TREE_PARENT_NEXT generates, one at a time, all labeled trees.
c
c  Discussion:
c
c    The routine also returns the corresponding Pruefer codes.
c
c  Formula:
c
c    There are N^(N-2) labeled trees on N nodes (Cayley's formula).
c
c    The number of trees in which node I has degree D(I) is the
c    multinomial coefficient: ( N-2; D(1)-1, D(2)-1, ..., D(N)-1 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Parameters:
c
c    Input, integer NNODE, the number of nodes to be used in 
c    the trees.
c
c    Workspace, integer IARRAY(NNODE).
c
c    Output, integer CODE(NNODE).  The first NNODE-2 entries 
c    of CODE contain the Pruefer code for the given labeled tree.
c
c    Output, integer ITREE(NNODE).  The first NNODE-1 entries 
c    of ITREE describe the edges that go between the nodes.  Each pair
c    (I, ITREE(I)) represents an edge.  Thus if ITREE(5) = 3,
c    there is an edge from node 3 to node 5.
c
c    Input/output, logical MORE.  On the first call only, the
c    user is required to set MORE = .FALSE.  Then call the routine, and
c    it will return information about the first tree
c    as well as setting MORE to the value .TRUE.
c    Keep calling to get another tree until MORE is .FALSE.
c    on return, at which point there are no more trees.
c
      implicit none

      integer nnode

      integer code(nnode)
      integer i
      integer iarray(nnode)
      integer itree(nnode)
      logical more

      call vec_next ( nnode-2, nnode, iarray, more )
     
      do i = 1, nnode - 2
        code(i) = iarray(i) + 1
      end do
     
      call pruefer_to_tree_2 ( nnode, code, itree )
     
      return
      end
      subroutine tree_parent_to_arc ( nnode, parent, nedge, inode, 
     &  jnode )

c*********************************************************************72
c
cc TREE_PARENT_TO_ARC converts a tree from parent to arc representation.
c
c  Discussion:
c
c    Parent representation lists the parent node of each node.  For a
c    tree of N nodes, one node has a parent of 0, representing a null link.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes in the tree.
c
c    Input, integer PARENT(NNODE), the parent node representation 
c    of the tree.
c
c    Output, integer NEDGE, the number of edges, normally NNODE-1.
c
c    Output, integer INODE(NEDGE), JNODE(NEDGE), pairs of nodes
c    that define the links.
c
      implicit none

      integer nnode

      integer i
      integer inode(nnode-1)
      integer jnode(nnode-1)
      integer nedge
      integer parent(nnode)

      nedge = 0

      do i = 1, nnode

        if ( parent(i) .ne. 0 ) then
          nedge = nedge + 1
          inode(nedge) = i
          jnode(nedge) = parent(i)
        end if

      end do

      return
      end
      subroutine tree_rb_enum ( n, num )

c*********************************************************************72
c
cc TREE_RB_ENUM returns the number of rooted binary trees with N nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 August 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of nodes in the rooted 
c    binary tree.  N should be odd.
c
c    Output, integer NUM, the number of rooted binary trees 
c    with N nodes.
c
      implicit none

      integer n

      integer c(0:(n-1)/2)
      integer num

      if ( n .lt. 0 ) then

        num = 0

      else if ( n .eq. 0 ) then

        num = 1

      else if ( mod ( n, 2 ) .eq. 0 ) then

        num = 0

      else

        call catalan ( ( n - 1 ) / 2, c )

        num = c((n-1)/2)

      end if

      return
      end
      subroutine tree_rb_lex_next ( n, a, more )

c*********************************************************************72
c
cc TREE_RB_LEX_NEXT generates rooted binary trees in lexicographic order.
c
c  Discussion:
c
c    The information definining the tree of N nodes is stored in a vector 
c    of 0's and 1's, in preorder traversal form.  Essentially, the
c    shape of the tree is traced out with a pencil that starts at the root,
c    and stops at the very last null leaf.  The first time that a (non-null) 
c    node is encountered, a 1 is added to the vector, and the left 
c    descendant of the node is visited next.  When the path returns from
c    the first descendant, the second descendant is visited.  When then path
c    returns again, the path passes back up from the node to its parent.
c    A null leaf is encountered only once, and causes a zero to be added to 
c    the vector, and the path goes back up to the parent node.  
c
c    The lexicographic order is used to order the vectors of 1's and 0's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 June 2013
c
c  Reference:
c
c    Frank Ruskey,
c    Combinatorial Generation,
c    To appear.
c
c  Parameters:
c
c    Input, integer N, the number of nodes in the rooted binary
c    tree.  N should be odd.
c
c    Input/output, integer A(N), the preorder traversal form for
c    the previous/next rooted binary tree.
c
c    Output, logical MORE, is TRUE if the next rooted binary tree was
c    returned on this call, or FALSE if there are no more rooted binary
c    trees, and the output of the previous call was the last one.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer k
      logical more
      integer p
      integer q

      if ( .not. more ) then
        do i = 1, n - 2, 2
          a(i) = 1
        end do
        do i = 2, n - 1, 2
          a(i) = 0
        end do
        a(n) = 0
        more = .true.
        return
      end if
c
c  Find the last 1 in A.
c
      k = n

10    continue

      if ( a(k) .eq. 0 ) then
        k = k - 1
        go to 10
      end if

      q = n - k - 1
c
c  Find the last 0 preceding the last 1 in A.
c  If there is none, then we are done, because 11...1100..00 
c  is the final element.
c
20    continue

        if ( k .eq. 1 ) then
          more = .false.
          return
        end if

        if ( a(k) .eq. 0 ) then
          go to 30
        end if

        k = k - 1

      go to 20

30    continue
	
      p = n - k - q - 1
      a(k) = 1
      do i = k + 1, n - 2 * p + 1
        a(i) = 0
      end do
      do i = n - 2 * p + 2, n - 2, 2
        a(i) = 1
      end do
      do i = n - 2 * p + 3, n - 1, 2
        a(i) = 0
      end do
      a(n) = 0

      return
      end
      subroutine tree_rb_to_parent ( n, a, parent )

c*********************************************************************72
c
cc TREE_RB_TO_PARENT converts rooted binary tree to parent node representation.
c
c  Discussion:
c
c    Parent node representation of a tree assigns to each node a "parent" node,
c    which represents the first link of the path between the node and the 
c    root node.  The root node itself is assigned a parent of 0.
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
c  Parameters:
c
c    Input, integer N, the number of nodes in the tree.
c
c    Input, integer A(N), the preorder traversal form for the
c    rooted binary tree.
c
c    Output, integer PARENT(N), the parent node representation 
c    of the tree.
c
      implicit none

      integer n

      integer a(n)
      integer dad
      integer k
      integer node
      integer node_num
      integer parent(n)
      integer use(n)

      node = 0
      node_num = 0

      do k = 1, n

        dad = node
        node_num = node_num + 1
        node = node_num
        parent(node) = dad

        if ( a(k) .eq. 1 ) then

          use(node) = 0

        else

          use(node) = 2

10        continue

          if ( use(node) .eq. 2 ) then
            node = dad
            if ( node .eq. 0 ) then
              go to 20
            end if
            use(node) = use(node) + 1
            dad = parent(node)
            go to 10
          end if

20        continue

        end if

      end do

      return
      end
      subroutine tree_rb_yule ( n, seed, a )

c*********************************************************************72
c
cc TREE_RB_YULE adds two nodes to a rooted binary tree using the Yule model.
c
c  Discussion:
c
c    The Yule model is a simulation of how an evolutionary family tree
c    develops.  We start with a root node.  The internal nodes of the tree 
c    are inactive and never change.  Each pendant or leaf node of the
c    tree represents a biological family that can spontaneously "fission",
c    developing two new distinct sub families.  In graphical terms, the node
c    becomes internal, with two new leaf nodes depending from it.
c
c    The tree is stored in inorder traversal form.
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
c  Parameters:
c
c    Input/output, integer N, the number of nodes in the input
c    tree.  On output, this number has been increased, usually by 2.
c
c    Input/output, integer SEED, a seed for the random number
c    generator.
c
c    Input/output, integer A(*), the preorder traversal form 
c    for the rooted binary tree.  The number of entries in A is N.
c
      implicit none

      integer a(*)
      integer i
      integer i4_uniform_ab
      integer ileaf
      integer j
      integer jleaf
      integer n
      integer nleaf
      integer seed

      if ( n .le. 0 ) then
        n = 1
        a(1) = 0
        return
      end if
c
c  Count the expected number of leaves, which are the 0 values.
c
      nleaf = ( n + 1 ) / 2
c
c  Choose a random number between 1 and NLEAF.
c
      ileaf = i4_uniform_ab ( 1, nleaf, seed )
c
c  Locate leaf number ILEAF.
c
      j = 0
      jleaf = 0
      do i = 1, n
        if ( a(i) .eq. 0 ) then
          jleaf = jleaf + 1
        end if
        if ( jleaf .eq. ileaf ) then
          j = i
          go to 10
        end if
      end do

10    continue
c
c  Replace '0' by '100'
c
      do i = n + 2, j + 2, -1
        a(i) = a(i-2)
      end do

      a(j) = 1
      a(j+1) = 0
      n = n + 2

      return
      end
      subroutine tree_rooted_code ( nnode, parent, code )

c*********************************************************************72
c
cc TREE_ROOTED_CODE returns the code of a rooted tree.
c
c  Discussion:
c
c    This code for a rooted tree depends on the node ordering, so it's actually
c    the code for a labeled rooted tree.  To eliminate the effects of node
c    labeling, one could choose as the code for a tree the maximum of all
c    the codes associated with the different possible labelings of the tree.
c    There are more effective ways of arriving at this code than simply
c    generating all possible codes and comparing them.  
c
c    For a tree with NNODES, the code is a list of 2*NNODE 0's and 1's,
c    describing a traversal of the tree starting at an imaginary node 0,
c    moving "down" to the root (a code entry of 1), and then moving
c    "down" (1) or "up" (0) as the tree is traversed in a depth first
c    manner.  The final move must be from the root up to the imaginary
c    node 0.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer PARENT(NNODE), is the parent node of each node.
c    The node with parent 0 is the root.
c
c    Output, integer CODE(2*NNODE), the code for the tree.
c
      implicit none

      integer nnode

      integer code(2*nnode)
      integer father
      integer i
      integer k
      integer parent(nnode)
      integer son
c
c  Find the root.
c
      father = 0

      do i = 1, nnode

        if ( parent(i) .eq. 0 ) then
          k = 1
          code(1) = 1
          father = i
          go to 10
        end if

      end do

10    continue

      if ( father .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ROOTED_CODE - Fatal error!'
        write ( *, '(a)' ) '  Could not find the root.'
        stop
      end if

20    continue

      if ( father .ne. 0 ) then

        k = k + 1
        code(k) = 0

        do son = 1, nnode
          if ( parent(son) .eq. father ) then
            code(k) = 1
            father = son
            go to 30
          end if
        end do

30      continue

        if ( code(k) .eq. 0 ) then
          parent(father) = - parent(father)
          father = - parent(father)
        end if

        go to 20

      end if

      do i = 1, nnode
        parent(i) = - parent(i)
      end do

      return
      end
      subroutine tree_rooted_code_compare ( nnode, npart, code1, code2, 
     &  result )

c*********************************************************************72
c
cc TREE_ROOTED_CODE_COMPARE compares a portion of the code for two rooted trees.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer NPART, the number of nodes for which the code
c    has been determined.  This determines the portion of the codes to be
c    compared.  We expect 0 .le. NPART .le. NNODE.
c
c    Input, integer CODE1(2*NNODE), CODE2(2*NNODE), the two 
c    rooted tree codes to be compared.
c
c    Output, integer RESULT, the result of the comparison.
c    -1, CODE1 .lt. CODE2,
c     0, CODE1 = CODE2,
c    +1, CODE1 > CODE2.
c
      implicit none

      integer nnode

      integer code1(2*nnode)
      integer code2(2*nnode)
      integer i
      integer ihi
      integer npart
      integer result

      result = 0

      if ( npart .le. 0 ) then
        return
      end if

      ihi = 2 * min ( npart, nnode )

      do i = 1, ihi

        if ( code1(i) .lt. code2(i) ) then
          result = -1
          return
        else if ( code2(i) .lt. code1(i) ) then
          result = +1
          return
        end if

      end do

      return
      end
      subroutine tree_rooted_depth ( nnode, parent, depth, depth_node )

c*********************************************************************72
c
cc TREE_ROOTED_DEPTH returns the depth of a rooted tree.
c
c  Discussion:
c
c    The depth of any node of a rooted tree is the number of edges in 
c    the shortest path from the root to the node.
c
c    The depth of the rooted tree is the maximum of the depths
c    of all the nodes.
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
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input, integer PARENT(NNODE), is the parent node of each node.
c    The node with parent 0 is the root.
c
c    Output, integer DEPTH, the depth of the tree.
c
c    Output, integer DEPTH_NODE(NNODE), the depth of each node.
c
      implicit none

      integer nnode

      integer depth
      integer depth_node(nnode)
      integer i
      integer i4vec_max
      integer j
      integer parent(nnode)
      integer root
c
c  Find the root.
c
      root = 0
      do i = 1, nnode
        if ( parent(i) .eq. 0 ) then
          root = i
          go to 10
        end if
      end do

10    continue

      if ( root .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ROOTED_DEPTH - Fatal error!'
        write ( *, '(a)' ) '  Could not find the root.'
        stop
      end if
c
c  Determine the depth of each node by moving towards the node.
c  If you reach a node whose depth is already known, stop early.
c
      do i = 1, nnode
        depth_node(i) = 0
      end do

      do i = 1, nnode

        j = i

20      continue

        if ( j .ne. root ) then

          depth_node(i) = depth_node(i) + 1
          j = parent(j)

          if ( 0 .lt. depth_node(j) ) then
            depth_node(i) = depth_node(i) + depth_node(j)
            go to 30
          end if

          go to 20

        end if

30      continue

      end do
c
c  Determine the maximum depth.
c
      depth = i4vec_max ( nnode, depth_node )

      return
      end
      subroutine tree_rooted_enum ( nnode, ntree )

c*********************************************************************72
c
cc TREE_ROOTED_ENUM counts the number of unlabeled rooted trees.
c
c  Example:
c
c    Input    Output
c
c      1         1
c      2         1
c      3         2
c      4         4
c      5         9
c      6        20
c      7        48
c      8       115
c      9       286
c     10       719
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
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Output, integer NTREE(NNODE).  NTREE(I) is the number of 
c    rooted, unlabeled trees on I nodes, for I = 1, 2, ... NNODE.
c
      implicit none

      integer nnode

      integer i
      integer id
      integer isum
      integer itd
      integer j
      integer nlast
      integer ntree(nnode)

      ntree(1) = 1
     
      do nlast = 2, nnode
     
        isum = 0
     
        do id = 1, nlast - 1
     
          i = nlast
          itd = ntree(id) * id
     
          do j = 1, nlast - 1

            i = i - id

            if ( i .le. 0 ) then
              go to 10
            end if

            isum = isum + ntree(i) * itd

          end do

10        continue
     
        end do
     
        ntree(nlast) = isum / ( nlast - 1 )
     
      end do

      return
      end
      subroutine tree_rooted_random ( nnode, seed, ntree, itree )

c*********************************************************************72
c
cc TREE_ROOTED_RANDOM selects a random unlabeled rooted tree.
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
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer NNODE, the number of nodes.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, integer NTREE(NNODE).  NTREE(I) is the number of 
c    rooted, unlabeled trees on I nodes, for I = 1, 2, ... NNODE.
c
c    Output, integer ITREE(NNODE).  (I,ITREE(I)) is the I-th edge
c    of the output tree for I = 2,NNODE.  ITREE(1)=0.
c
      implicit none

      integer nnode

      integer i
      integer id
      integer is1
      integer is2
      integer itd
      integer itree(nnode)
      integer iz
      integer j
      integer l
      integer ll
      integer ls
      integer m
      integer ntree(nnode)
      integer nval
      double precision r
      double precision r8_uniform_01
      integer seed
      integer stack(2,nnode)

      if ( nnode .le. 0  ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TREE_ROOTED_RANDOM - Fatal error!'
        write ( *, '(a,i8)' ) '  NNODE = ', nnode
        write ( *, '(a)' ) '  but NNODE must be at least 1.'
        stop
      end if
c
c  Compute a table of the number of such trees for a given number of nodes.
c
      call tree_rooted_enum ( nnode, ntree )
c
c  Now select one such tree at random.
c
      l = 0

      nval = nnode
      is1 = 0
      is2 = 0

10    continue

20      continue

        if ( 2 .lt. nval ) then
     
          r = r8_uniform_01 ( seed )

          iz = int ( ( nval - 1 ) * ntree(nval) * r )

          id = 0
      
          id = id + 1
          itd = id * ntree(id)
          m = nval
          j = 0

30        continue
     
            j = j + 1
            m = m - id

            if ( m .lt. 1 ) then
              id = id + 1
              itd = id * ntree(id)
              m = nval
              j = 0
              go to 30
            end if

            iz = iz - ntree(m) * itd

            if ( iz .lt. 0 ) then
              go to 40
            end if

          go to 30

40        continue

          is1 = is1 + 1
          stack(1,is1) = j
          stack(2,is1) = id
          nval = m
     
          go to 20
  
        end if
   
        itree(is2+1) = l
        l = is2 + 1
        is2 = is2 + nval

        if ( 1 .lt. nval ) then
          itree(is2) = is2 - 1
        end if
     
50      continue
     
          nval = stack(2,is1)
     
          if ( nval .ne. 0 ) then
            stack(2,is1) = 0
            go to 60
          end if
     
          j = stack(1,is1)
          is1 = is1 - 1
          m = is2 - l + 1
          ll = itree(l)
          ls = l + ( j - 1 ) * m - 1
     
          if ( j .ne. 1 ) then
            do i = l, ls
              itree(i+m) = itree(i) + m
              if ( mod(i-l,m) .eq. 0 ) then
                itree(i+m) = ll
              end if
            end do
          end if
     
          is2 = ls + m
     
          if ( is2 .eq. nnode ) then
            return
          end if

          l = ll
     
        go to 50

60      continue

      go to 10

      return
      end
      subroutine vec_next ( n, ibase, iarray, more )

c*********************************************************************72
c
cc VEC_NEXT generates all N-vectors of integers modulo a given base.
c
c  Discussion:
c
c    The items are produced one at a time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 1999
c
c  Parameters:
c
c    Input, integer N, the size of the vectors to be used.
c
c    Input, integer IBASE, the base to be used.  IBASE = 2 will
c    give vectors of 0's and 1's, for instance.
c
c    Input/output, integer IARRAY(N).  On each return from VECNEX,
c    IARRAY will contain entries in the range 0 to IBASE-1.
c
c    Input/output, logical MORE.  Set this variable .FALSE. before
c    the first call.  Normally, MORE will be returned .TRUE. but
c    once all the vectors have been generated, MORE will be
c    reset .FALSE. and you should stop calling the program.
c
      implicit none

      integer n

      integer i
      integer iarray(n)
      integer ibase
      integer kount
      integer last
      logical more
      integer nn

      save kount
      save last

      if ( .not. more ) then
     
        kount = 1
        last = ibase ** n
        more = .true.
        do i = 1, n
          iarray(i) = 0
        end do
   
      else
     
        kount = kount + 1

        if ( kount .eq. last ) then
          more = .false.
        end if

        iarray(n) = iarray(n) + 1
     
        do i = 1, n

          nn = n - i

          if ( iarray(nn+1) .lt. ibase ) then
            return
          end if

          iarray(nn+1) = 0

          if ( nn .ne. 0 ) then
            iarray(nn) = iarray(nn) + 1
          end if

        end do
     
      end if
     
      return
      end
      subroutine vec_random ( n, base, seed, iarray )

c*********************************************************************72
c
cc VEC_RANDOM selects a random N-vector of integers modulo a given base.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    15 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the vector to be generated.
c
c    Input, integer BASE, the base to be used.
c
c    Input/output, integer SEED, a seed for the random 
c    number generator.
c
c    Output, integer IARRAY(N), a list of N random values between
c    0 and IBASE-1.
c
      implicit none

      integer n

      integer base
      integer i
      integer i4_uniform_ab
      integer iarray(n)
      integer ival
      integer seed

      do i = 1, n
        ival = i4_uniform_ab ( 0, base-1, seed )
        iarray(i) = ival
      end do
     
      return
      end
