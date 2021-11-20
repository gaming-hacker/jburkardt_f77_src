      program main

c*********************************************************************72
c
cc MAIN is the main program for TET_MESH_PRB.
c
c  Discussion:
c
c    TET_MESH_PRB tests the TET_MESH library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TET_MESH_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TET_MESH library.'

      call test001 ( )
      call test002 ( )
      call test003 ( )
      call test004 ( )
      call test005 ( )
      call test006 ( )
      call test007 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TET_MESH_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test001 ( )

c*********************************************************************72
c
cc TEST001 tests R8MAT_SOLVE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )
      integer rhs_num
      parameter ( rhs_num = 2 )
c
c  Each row of this definition is a COLUMN of the matrix.
c
      double precision a(n,n+rhs_num)
      integer info

      save a

      data a /
     &  1.0D+00,  4.0D+00,  7.0D+00,
     &  2.0D+00,  5.0D+00,  8.0D+00, 
     &  3.0D+00,  6.0D+00,  0.0D+00,
     & 14.0D+00, 32.0D+00, 23.0D+00,
     &  7.0D+00, 16.0D+00,  7.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST001'
      write ( *, '(a)' ) '  R8MAT_SOLVE solves linear systems.'
c
c  Print out the matrix to be inverted.
c
      call r8mat_print ( n, n+rhs_num, a, '  The linear system:' )
c
c  Solve the systems.
c
      call r8mat_solve ( n, rhs_num, a, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The input matrix was singular.'
        write ( *, '(a)' ) '  The solutions could not be computed.'
        write ( *, '(a)' ) ' '
        return
      end if

      call r8mat_print ( n, rhs_num, a(1:n,n+1:n+rhs_num),
     &  '  The computed solutions' )

      return
      end
      subroutine test002 ( )

c*********************************************************************72
c
cc TEST002 tests TETRAHEDRON_ORDER4_PHYSICAL_TO_REFERENCE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      integer i
      integer j
      double precision phy(3,n)
      double precision r8_uniform_01
      double precision ref(3,n)
      double precision ref2(3,n)
      integer seed
      double precision tet(3,4)

      save tet

      data tet /
     &  5.0D+00, 0.0D+00, 0.0D+00,
     &  8.0D+00, 0.0D+00, 0.0D+00,
     &  5.0D+00, 2.0D+00, 0.0D+00,
     &  6.0D+00, 1.0D+00, 2.0D+00 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST002'
      write ( *, '(a)' ) '  For an order 4 tetrahedron,'
      write ( *, '(a)' ) '  TETRAHEDRON_ORDER4_PHYSICAL_TO_REFERENCE '
      write ( *, '(a)' ) '  maps a physical point to a reference point.'
      write ( *, '(a)' ) '  TETRAHEDRON_ORDER4_REFERENCE_TO_PHYSICAL '
      write ( *, '(a)' ) '  maps a reference point to a physical point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     ( R, S, T )          ==>  ( X, Y, Z )  ' //
     &  '         ==> ( R2, S2, T2 )'
      write ( *, '(a)' ) ' '

      call tetrahedron_reference_sample ( n, seed, ref )

      call tetrahedron_order4_reference_to_physical ( tet, n, ref, phy )
      call tetrahedron_order4_physical_to_reference ( tet, n, phy, 
     &  ref2 )

      do j = 1, n

        write ( *, '(2x,3f8.4,2x,3f8.4,2x,3f8.4)' ) 
     &    ( ref(i,j), i = 1, 3 ), 
     &    ( phy(i,j), i = 1, 3 ),
     &    ( ref2(i,j), i = 1, 3 )

      end do

      return
      end
      subroutine test003 ( )

c*********************************************************************72
c
cc TEST003 tests TETRAHEDRON_ORDER10_TO_ORDER4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer node_num1
      integer node_num2
      double precision node_xyz(3,27)
      integer tet_node1(10,6)
      integer tet_node2(4,48)
      integer tet_num1
      integer tet_num2
      integer tet_order1
      parameter ( tet_order1 = 10 )
      integer tet_order2 
      parameter ( tet_order2 = 4 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST003'
      write ( *, '(a)' ) '  For an order 10 tet mesh,'
      write ( *, '(a)' ) '  TETRAHEDRON_ORDER10_TO_ORDER4 '
      write ( *, '(a)' ) '  makes a linear (order 4) tet mesh by using'
      write ( *, '(a)' ) '  the existing nodes, and replacing each'
      write ( *, '(a)' ) '  quadratic tetrahedron by 8 linear ones.'

      call tet_mesh_order10_example_size ( node_num1, tet_num1 )

      call tet_mesh_order10_example_set ( node_num1, tet_num1,
     &  node_xyz, tet_node1 )

      call i4mat_transpose_print_some ( tet_order1, tet_num1, tet_node1,
     &  1, 1, tet_order1, 5, '  First 5 quadratic tetrahedrons:' )

      call tet_mesh_order10_to_order4_size ( node_num1, tet_num1,
     &  node_num2, tet_num2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Quadratic mesh size is       ', tet_num1
      write ( *, '(a,i8)' ) '  Linearized mesh size will be ', tet_num2

      call tet_mesh_order10_to_order4_compute ( tet_num1, tet_node1,    
     &  tet_num2, tet_node2 )

      call i4mat_transpose_print_some ( tet_order2, tet_num2, tet_node2,
     &  1, 1, tet_order2, 5, '  First 5 linear tetrahedrons:' )

      return
      end
      subroutine test004 ( )

c*********************************************************************72
c
cc TEST004 tests TET_MESH_NODE_ORDER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4vec_sum
      integer node_num
      integer node_order(27)
      double precision node_xyz(3,27)
      integer tet_node(10,6)
      integer tet_num
      integer tet_order
      parameter ( tet_order = 10 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST004'
      write ( *, '(a)' ) 
     &  '  TET_MESH_NODE_ORDER determines the order of '
      write ( *, '(a)' ) '  each node in a tet mesh.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The order of a node is the number of tetrahedrons'
      write ( *, '(a)' ) 
     &  '  that use the node as part of their definition.'

      call tet_mesh_order10_example_size ( node_num, tet_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  This mesh has tetrahedron order ', 
     &  tet_order
      write ( *, '(a,i8)' ) '  The number of tetrahedrons is   ', 
     &  tet_num

      call tet_mesh_order10_example_set ( node_num, tet_num,
     &  node_xyz, tet_node )

      call i4mat_transpose_print ( tet_order, tet_num, tet_node,
     &  '  The tet mesh:' )

      call tet_mesh_node_order ( tet_order, tet_num, tet_node,
     &  node_num, node_order )

      call i4vec_print ( node_num, node_order, '  Node orders:' );

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Check that the following are equal:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of tetrahedrons * order = ', 
     &  tet_num * tet_order
      write ( *, '(a,i8)' ) '  Sum of node orders             = ', 
     &  i4vec_sum ( node_num, node_order )

      return
      end
      subroutine test005 ( )

c*********************************************************************72
c
cc TEST005 tests TETRAHEDRON_BARYCENTRIC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 August 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision c1(4)
      double precision c1_sum
      double precision c2(4)
      integer i
      double precision p(3)
      double precision r8vec_sum
      integer seed
      integer test1
      integer test1_num
      parameter ( test1_num = 3 )
      integer test2
      integer test2_num
      parameter ( test2_num = 5 )
      double precision tet_xyz(3,4)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST005'
      write ( *, '(a)' ) 
     &  '  TETRAHEDRON_BARYCENTRIC computes the barycentric'
      write ( *, '(a)' ) '  coordinates of a point.'
c
c  Choose a random tetrahedron.
c
      do test1 = 1, test1_num

        call r8mat_uniform_01 ( 3, 4, seed, tet_xyz )

        call r8mat_transpose_print ( 3, 4, tet_xyz, 
     &    '  Random tetrahedron:' )
c
c  Choose barycentric coordinates C1 at random.
c
c  Define a point P.
c
c  Have TETRAHEDRON_BARYCENTRIC compute C2, the barycentric coordinates of P.
c
        do test2 = 1, test2_num

          call r8vec_uniform_01 ( 4, seed, c1 )
          c1_sum = r8vec_sum ( 4, c1 )

          do i = 1, 4
            c1(i) = c1(i) / c1_sum
          end do

          call r8mat_mv ( 3, 4, tet_xyz, c1, p )

          call tetrahedron_barycentric ( tet_xyz, p, c2 )

          write ( *, '(a)' ) ' '
          write ( *, '(2x,a,4(2x,g14.6))' ) 'C1 = ', c1(1:4)
          write ( *, '(2x,a,4(2x,g14.6))' ) 'C2 = ', c2(1:4)

        end do
      end do

      return
      end
      subroutine test006 ( )

c*********************************************************************72
c
cc TEST006 tests TET_MESH_TET_NEIGHBORS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer node_num
      double precision node_xyz(3,63)
      integer tet_neighbor(4,144)
      integer tet_node(4,144)
      integer tet_num
      integer tet_order
      parameter ( tet_order = 4 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST006'
      write ( *, '(a)' ) 
     &  '  TET_MESH_TET_NEIGHBORS computes the 4 neighboring'
      write ( *, '(a)' ) 
     &  '  tetrahedrons of each tetrahedron in a tet mesh.'
      write ( *, '(a)' ) '  containing a point.'
c
c  Set up the example tetrahedron mesh.
c
      call tet_mesh_order4_example_size ( node_num, tet_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  This mesh has tetrahedron order ', tet_order
      write ( *, '(a,i8)' ) 
     &  '  The number of tetrahedrons is   ', tet_num

      call tet_mesh_order4_example_set ( node_num, tet_num, node_xyz, 
     &  tet_node )
c
c  Print the tets.
c
      call i4mat_transpose_print_some ( tet_order, tet_num, tet_node,   
     &  1, 1, tet_order, 10, '  First 10 Tets:' )
c
c  The TET_NEIGHBOR array is needed by TET_MESH_DELAUNAY_SEARCH.
c
      call tet_mesh_neighbor_tets ( tet_order, tet_num, tet_node,
     &  tet_neighbor )

      call i4mat_transpose_print_some ( 4, tet_num, tet_neighbor,
     &  1, 1, 4, 10, '  First 10 Tet Neighbors:' )

      return
      end
      subroutine test007 ( )

c*********************************************************************72
c
cc TEST007 tests TET_MESH_SEARCH_NAIVE and TET_MESH_SEARCH_DELAUNAY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 August 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha(4)
      integer face
      integer i
      integer i4_uniform_ab
      integer j
      integer node_num
      double precision node_xyz(3,63)
      double precision p(3)
      integer seed
      integer step_num
      double precision t(3,4)
      integer test
      integer test_num
      parameter ( test_num = 5 )
      integer tet1
      integer tet2
      integer tet3
      integer tet_neighbor(4,144)
      integer tet_node(4,144)
      integer tet_num
      integer tet_order
      parameter ( tet_order = 4 )

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST007'
      write ( *, '(a)' ) 
     &  '  TET_MESH_SEARCH_NAIVE uses a naive algorithm'
      write ( *, '(a)' ) 
     &  '  to search a tetrahedral mesh for the tetrahedron'
      write ( *, '(a)' ) '  containing a point.'
      write ( *, '(a)' ) 
     &  '  TET_MESH_SEARCH_DELAUNAY uses the Delaunay search algorithm'
      write ( *, '(a)' ) 
     &  '  to search a Delaunay tetrahedral mesh for the tetrahedron'
      write ( *, '(a)' ) '  containing a point.'
c
c  Set up the example tetrahedron mesh.
c
      call tet_mesh_order4_example_size ( node_num, tet_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  This mesh has tetrahedron order ', tet_order
      write ( *, '(a,i8)' ) 
     &  '  The number of tetrahedrons is   ', tet_num

      call tet_mesh_order4_example_set ( node_num, tet_num, node_xyz, 
     &  tet_node )
c
c  Initializing TET3 to -1 helps TET_MESH_DELAUNAY_SEARCH get started.
c
      tet3 = - 1
c
c  The TET_NEIGHBOR array is needed by TET_MESH_DELAUNAY_SEARCH.
c
      call tet_mesh_neighbor_tets ( tet_order, tet_num, tet_node,
     &  tet_neighbor )

      do test = 1, test_num
c
c  Choose a tetrahedron at random.
c
        tet1 = i4_uniform_ab ( 1, tet_num, seed )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) 
     &    '  Point was chosen from tetrahedron    ', tet1
c
c  Choose a point in the tetrahedron at random.
c
        do j = 1, 4
          do i = 1, 3
            t(i,j) = node_xyz(i,tet_node(j,tet1) )
          end do
        end do

        call tetrahedron_sample ( t, 1, seed, p )
c
c  Naive search.
c
        call tet_mesh_search_naive ( node_num, node_xyz, tet_order, 
     &    tet_num, tet_node, p, tet2, step_num )

        write ( *, '(a,i8,a,i8)' ) 
     &    '  Naive search ended in tetrahedron    ', tet2,
     &    ', number of steps = ', step_num
c
c  Delaunay search.
c
        call tet_mesh_search_delaunay ( node_num, node_xyz, tet_order,  
     &     tet_num, tet_node, tet_neighbor, p, tet3, face, step_num )

        write ( *, '(a,i8,a,i8)' ) 
     &    '  Delaunay search ended in tetrahedron ', tet3,
     &    ', number of steps = ', step_num

      end do

      return
      end
