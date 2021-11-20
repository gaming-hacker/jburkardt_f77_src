      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM2D_PACK_PRB.
c
c  Discussion:
c
c    FEM2D_PACK_PRB tests the FEM2D_PACK library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM2D_PACK_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FEM2D_PACK library.'

      call test01 ( )
      call test03 ( )
      call test04 ( )

      call test105 ( )
      call test12 ( )
      call test13 ( )
      call test135 ( )
      call test14 ( )
      call test15 ( )
      call test16 ( )
      call test18 ( )
      call test19 ( )

      call test23 ( )
      call test24 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM2D_PACK_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests BANDWIDTH_MESH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element_node(6,24)
      integer element_num
      integer element_order
      integer m
      integer ml
      integer mu
      integer nelemx
      integer nelemy

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  BANDWIDTH_MESH computes the geometric bandwidth:'
      write ( *, '(a)' ) '  of a finite element mesh.'

      nelemx = 2
      nelemy = 6

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  NELEMX = ', nelemx
      write ( *, '(a,i8)' ) '  NELEMY = ', nelemy

      element_order = 6
      call grid_element_num ( 'T6', nelemx, nelemy, element_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  ELEMENT_ORDER = ', element_order
      write ( *, '(a,i8)' ) '  ELEMENT_NUM   = ', element_num

      call grid_t6_element ( nelemx, nelemy, element_node )

      call grid_print ( element_order, element_num, element_node )

      call bandwidth_mesh ( element_order, element_num, element_node, 
     &  ml, mu, m )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Lower bandwidth ML = ', ml
      write ( *, '(a,i8)' ) '  Upper bandwidth MU = ', mu
      write ( *, '(a,i8)' ) '  Total bandwidth M  = ', m

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests BASIS_11_**_TEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 June 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Test the computation of ONE basis function'
      write ( *, '(a)' ) '  at ONE point in a given element:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  BASIS_11_Q4_TEST : Q4 element.'
      write ( *, '(a)' ) '  BASIS_11_T3_TEST : T3 element.'
      write ( *, '(a)' ) '  BASIS_11_T4_TEST : T4 element.'
      write ( *, '(a)' ) '  BASIS_11_T6_TEST : T6 element.'

      call basis_11_q4_test ( )

      call basis_11_t3_test ( )

      call basis_11_t4_test ( )

      call basis_11_t6_test ( )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests BASIS_MN_**_TEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  Test the computation of all M basis functions'
      write ( *, '(a)' ) '  at N points in a given element:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  BASIS_MN_Q4_TEST : Q4 element.'
      write ( *, '(a)' ) '  BASIS_MN_T3_TEST : T3 element.'
      write ( *, '(a)' ) '  BASIS_MN_T4_TEST : T4 element.'
      write ( *, '(a)' ) '  BASIS_MN_T6_TEST : T6 element.'

      call basis_mn_q4_test ( )

      call basis_mn_t3_test ( )

      call basis_mn_t4_test ( )

      call basis_mn_t6_test ( )

      return
      end
      subroutine test105 ( )

c*********************************************************************72
c
cc TEST105 tests GRID_NODES_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer num_x
      parameter ( num_x = 5 )
      integer num_y
      parameter ( num_y = 3 )

      integer node_num
      parameter ( node_num = num_x * num_y )

      integer i
      integer j
      integer node
      double precision node_xy(2,node_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST105'
      write ( *, '(a)' ) 
     &  '  GRID_NODES_01 creates a regular grid in the unit square.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  NUM_X =    ', num_x
      write ( *, '(a,i8)' ) '  NUM_Y =    ', num_y
      write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
      write ( *, '(a)' ) ' '

      call grid_nodes_01 ( num_x, num_y, node_xy )

      do node = 1, node_num
        write ( *, '(2x,i8,2x,f14.6,2x,f14.6)' ) 
     &    node, node_xy(1:2,node)
      end do

      return
      end
      subroutine test12 ( )

c********************************************************************72
c
cc TEST12 tests INTERP_TEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST12'
      write ( *, '(a)' ) '  INTERP_TEST tests the interpolating power'
      write ( *, '(a)' ) '  of the element.'

      call interp_test ( 'Q4' )

      call interp_test ( 'Q8' )

      call interp_test ( 'Q9' )

      call interp_test ( 'Q12' )

      call interp_test ( 'Q16' )

      call interp_test ( 'QL' )

      call interp_test ( 'T3' )

      call interp_test ( 'T4' )

      call interp_test ( 'T6' )

      call interp_test ( 'T10' )

      return
      end
      subroutine test13 ( )

c*********************************************************************72
c
cc TEST13 tests MAP_TEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST13'
      write ( *, '(a)' ) '  MAP_TEST tests the map routines.'

      call map_test ( 'Q4' )

      call map_test ( 'Q8' )

      call map_test ( 'Q9' )

      call map_test ( 'Q12' )

      call map_test ( 'Q16' )

      call map_test ( 'QL' )

      call map_test ( 'T3' )

      call map_test ( 'T4' )

      call map_test ( 'T6' )

      call map_test ( 'T10' )

      return
      end
      subroutine test135 ( )

c*********************************************************************72
c
cc TEST135 demonstrates MASS_MATRIX_T3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element_num
      parameter ( element_num = 8 )
      integer element_order
      parameter ( element_order = 3 )
      integer node_num
      parameter ( node_num = 9 )

      double precision a(node_num,node_num)
      integer element_node(element_order,element_num )
      double precision node_xy(2,node_num)

      save element_node
      save node_xy

      data element_node /
     &  1, 4, 2, 
     &  5, 2, 4, 
     &  4, 7, 5, 
     &  8, 5, 7, 
     &  2, 5, 3, 
     &  6, 3, 5, 
     &  5, 8, 6, 
     &  9, 6, 8 /
      data node_xy /
     &  0.0D+00, 0.0D+00, 
     &  0.0D+00, 0.5D+00, 
     &  0.0D+00, 1.0D+00, 
     &  0.5D+00, 0.0D+00, 
     &  0.5D+00, 0.5D+00, 
     &  0.5D+00, 1.0D+00, 
     &  1.0D+00, 0.0D+00, 
     &  1.0D+00, 0.5D+00, 
     &  1.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST135'
      write ( *, '(a)' ) 
     &  '  MASS_MATRIX_T3 computes the mass matrix for'
      write ( *, '(a)' ) '  a finite element system using T3 elements'
      write ( *, '(a)' ) '  (linear triangles).'

      call mass_matrix_t3 ( node_num, element_num, element_node,
     &  node_xy, a )

      call r8mat_print ( node_num, node_num, a, 
     &  '  The T3 mass matrix:' )

      return
      end
      subroutine test14 ( )

c*********************************************************************72
c
cc TEST14 demonstrates MASS_MATRIX_T6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element_num
      parameter ( element_num = 2 )
      integer element_order
      parameter ( element_order = 6 )
      integer node_num
      parameter ( node_num = 9 )
 
      double precision a(node_num,node_num)
      integer element_node(element_order,element_num )
      double precision node_xy(2,node_num)

      save element_node
      save node_xy

      data element_node /
     &  1, 3, 7, 2, 5, 4,
     &  9, 7, 3, 8, 5, 6 /
      data node_xy /
     &  0.0D+00, 0.0D+00, 
     &  0.0D+00, 0.5D+00, 
     &  0.0D+00, 1.0D+00, 
     &  0.5D+00, 0.0D+00, 
     &  0.5D+00, 0.5D+00, 
     &  0.5D+00, 1.0D+00, 
     &  1.0D+00, 0.0D+00, 
     &  1.0D+00, 0.5D+00, 
     &  1.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST14'
      write ( *, '(a)' ) '  MASS_MATRIX_T6 computes the mass matrix for'
      write ( *, '(a)' ) '  a finite element system using T6 elements'
      write ( *, '(a)' ) '  (quadratic triangles).'

      call mass_matrix_t6 ( node_num, element_num, element_node,
     &  node_xy, a )

      call r8mat_print ( node_num, node_num, a, 
     &  '  The T6 mass matrix:' )

      return
      end
      subroutine test15 ( )

c*********************************************************************72
c
cc TEST15 tests PHYSICAL_TO_REFERENCE_T3 and REFERENCE_TO_PHYSICAL_T3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 January 2013
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
      double precision phy(2,n)
      double precision r8_uniform_01
      double precision ref(2,n)
      double precision ref2(2,n)
      integer seed
      double precision t(2,3)

      save t

      data t /
     &  1.0D+00, 1.0D+00, 
     &  3.0D+00, 1.0D+00, 
     &  2.0D+00, 5.0D+00 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST15'
      write ( *, '(a)' ) '  For an order 3 triangle,'
      write ( *, '(a)' ) '  PHYSICAL_TO_REFERENCE_T3 maps a physical'
      write ( *, '(a)' ) '  point to a reference point.'
      write ( *, '(a)' ) '  REFERENCE_TO_PHYSICAL_T3 maps a reference'
      write ( *, '(a)' ) '  point to a physical point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  (   XSI    ETA ) ==> ( X     Y  )  ==> ( XSI2   ETA2 )'
      write ( *, '(a)' ) ' '

      do j = 1, n

        do i = 1, 2
          ref(i,j) = r8_uniform_01 ( seed )
        end do

        if ( 1.0D+00 .lt. ref(1,j) + ref(2,j) ) then
          do i = 1, 2
            ref(i,j) = 1.0D+00 - ref(i,j)
          end do
        end if

      end do

      call reference_to_physical_t3 ( t, n, ref, phy )
      call physical_to_reference_t3 ( t, n, phy, ref2 )

      do j = 1, n

        write ( *, '(2x,2f8.4,2x,2f8.4,2x,2f8.4)' ) 
     &    ref(1:2,j), phy(1:2,j), ref2(1:2,j)
      end do

      return
      end
      subroutine test16 ( )

c*********************************************************************72
c
cc TEST16 tests REFERENCE_TO_PHYSICAL_T6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 16 )

      integer i
      integer j
      double precision phy(2,n)
      double precision ref(2,n)
      double precision t(2,6)

      save ref
      save t

      data ref /
     &  0.00D+00, 0.00D+00, 
     &  1.00D+00, 0.00D+00, 
     &  0.00D+00, 1.00D+00, 
     &  0.50D+00, 0.00D+00, 
     &  0.50D+00, 0.50D+00, 
     &  0.00D+00, 0.50D+00, 
     &  0.25D+00, 0.75D+00, 
     &  0.75D+00, 0.25D+00, 
     &  0.40D+00, 0.10D+00, 
     &  0.30D+00, 0.20D+00, 
     &  0.20D+00, 0.30D+00, 
     &  0.10D+00, 0.40D+00, 
     &  0.10D+00, 0.10D+00, 
     &  0.20D+00, 0.20D+00, 
     &  0.30D+00, 0.30D+00, 
     &  0.40D+00, 0.40D+00 /
      data t /
     &  0.0D+00, 0.0D+00, 
     &  2.0D+00, 0.0D+00, 
     &  0.0D+00, 4.0D+00, 
     &  1.0D+00, 0.0D+00, 
     &  1.0D+00, 1.0D+00, 
     &  0.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST16'
      write ( *, '(a)' ) '  For an order 6 triangle,'
      write ( *, '(a)' ) '  REFERENCE_TO_PHYSICAL_T6 maps a reference'
      write ( *, '(a)' ) '  point to a physical point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      XSI     ETA  ==>  X       Y'
      write ( *, '(a)' ) ' '

      call reference_to_physical_t6 ( t, n, ref, phy )

      do j = 1, n
        write ( *, '(2x,2f8.4,2x,2f8.4)' ) ref(1:2,j), phy(1:2,j)
      end do

      return
      end
      subroutine test18 ( )

c*********************************************************************72
c
cc TEST18 tests SHAPE_TEST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST18'
      write ( *, '(a)' ) '  SHAPE_TEST tests the shape routines.'

      call shape_test ( 'Q4' )

      call shape_test ( 'Q8' )

      call shape_test ( 'Q9' )

      call shape_test ( 'Q12' )

      call shape_test ( 'Q16' )

      call shape_test ( 'QL' )

      call shape_test ( 'T3' )

      call shape_test ( 'T4' )

      call shape_test ( 'T6' )

      call shape_test ( 'T10' )

      return
      end
      subroutine test19 ( )

c*********************************************************************72
c
cc TEST19 tests SPHERE_GRID_Q4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element
      integer element_node(4,64)
      integer element_num
      integer element_order
      parameter ( element_order = 4 )
      integer nelemx
      parameter ( nelemx = 8 )
      integer nelemy
      parameter ( nelemy = 8 )
      integer node
      integer node_num
      double precision node_xyz(3,58)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST19'
      write ( *, '(a)' ) '  SPHERE_GRID_Q4_ELEMENT sets up a grid of '
      write ( *, '(a)' ) '  Q4 quadrilaterals on a sphere.'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRID_Q4_ELEMENT_NUM returns the number'
      write ( *, '(a)' ) '  of elements in the grid'
      write ( *, '(a)' ) '  SPHERE_GRID_Q4_NODE_NUM returns the number'
      write ( *, '(a)' ) '  of nodes in the grid.'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRID_Q4_NODE_XYZ returns the coordinates'
      write ( *, '(a)' ) '  of nodes in the grid.'

      call sphere_grid_q4_element_num ( nelemx, nelemy, element_num )
      call sphere_grid_q4_node_num ( nelemx, nelemy, node_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Expected number of nodes =    ', node_num
      write ( *, '(a,i8)' ) 
     &  '  Expected number of elements = ', element_num

      call sphere_grid_q4_element ( nelemx, nelemy, element_node )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The elements and their nodes:'
      write ( *, '(a)' ) ' '

      do element = 1, element_num
        write ( *, '(i4,2x,4i4)' ) element, 
     &    element_node(1:element_order,element)
      end do

      call sphere_grid_q4_node_xyz ( nelemx, nelemy, node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The node coordinates:'
      write ( *, '(a)' ) ' '

      do node = 1, node_num
        write ( *, '(2x,i4,2x,3g14.6)' ) node, node_xyz(1:3,node)
      end do
c
c  Write the elements and nodes to files.
c
      call r8mat_write ( 'sphere_q4_nodes.txt', 3, node_num, node_xyz )

      call i4mat_write ( 'sphere_q4_elements.txt', element_order, 
     &  element_num, element_node )

      return
      end
      subroutine test23 ( )

c*********************************************************************72
c
cc TEST23 tests SPHERE_GRID_T6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element_num_max
      parameter ( element_num_max = 18 )
      integer node_num_max
      parameter ( node_num_max = 44 )

      integer element
      integer element_node(6,element_num_max)
      integer element_num
      integer element_order
      parameter ( element_order = 6 )
      integer nelemx
      parameter ( nelemx = 3 )
      integer nelemy
      parameter ( nelemy = 4 )
      integer node
      integer node_num
      double precision node_xyz(3,node_num_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST23'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRID_T6_ELEMENT sets up a grid of T6 triangles'
      write ( *, '(a)' ) '  on a sphere.'
      write ( *, '(a)' ) '  SPHERE_GRID_T6_ELEMENT_NUM returns the'
      write ( *, '(a)' ) '  number of elements in the grid'
      write ( *, '(a)' ) '  SPHERE_GRID_T6_NODE_NUM returns the number'
      write ( *, '(a)' ) '  of nodes in the grid.'
      write ( *, '(a)' ) 
     &  '  SPHERE_GRID_T6_NODE_XYZ returns the coordinates'
      write ( *, '(a)' ) '  of nodes in the grid.'

      call sphere_grid_t6_element_num ( nelemx, nelemy, element_num )
      call sphere_grid_t6_node_num ( nelemx, nelemy, node_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Expected number of nodes =    ', node_num
      write ( *, '(a,i8)' ) 
     &  '  Expected number of elements = ', element_num

      call sphere_grid_t6_element ( nelemx, nelemy, element_node )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The elements and their nodes:'
      write ( *, '(a)' ) ' '

      do element = 1, element_num
        write ( *, '(i4,2x,6i4)' ) element, 
     &    element_node(1:element_order,element)
      end do

      call sphere_grid_t6_node_xyz ( nelemx, nelemy, node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The node coordinates:'
      write ( *, '(a)' ) ' '

      do node = 1, node_num
        write ( *, '(2x,i4,2x,3g14.6)' ) node, node_xyz(1:3,node)
      end do
c
c  Write the elements and nodes to files.
c
      call r8mat_write ( 'sphere_t6_nodes.txt', 3, node_num, node_xyz )

      call i4mat_write ( 'sphere_t6_elements.txt', element_order, 
     &  element_num, element_node )

      return
      end
      subroutine test24 ( )

c*********************************************************************72
c
cc TEST24 tests TRIANGLE_UNIT_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer order_max
      parameter ( order_max = 64 )

      integer a
      integer b
      double precision coef
      double precision err
      double precision exact
      integer i
      integer order
      double precision quad
      integer rule
      integer rule_max
      parameter ( rule_max = 20 )
      integer triangle_unit_size
      double precision value
      double precision weight(order_max)
      double precision x
      double precision xtab(order_max)
      double precision y
      double precision ytab(order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST24'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_SET sets up a quadrature '
      write ( *, '(a)' ) '  in the unit triangle,'
      write ( *, '(a)' ) ' '

      do a = 0, 10

        do b = 0, 10 - a

          coef = dble ( a + b + 2 ) * dble ( a + b + 1 )
          do i = 1, b
            coef = coef * dble ( a + i ) / dble ( i )
          end do

          write ( *, '(a)' ) ' '
          write ( *, '(a,i8,a,i8)' ) '  A = ', a, '  B = ', b
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Rule       QUAD           ERROR'
          write ( *, '(a)' ) ' '

          do rule = 1, rule_max

            order = triangle_unit_size ( rule )

            call triangle_unit_set ( rule, xtab, ytab, weight )

            quad = 0.0D+00

            do i = 1, order

              x = xtab(i)
              y = ytab(i)

              if ( a .eq. 0 .and. b .eq. 0 ) then
                value = coef
              else if ( a .eq. 0 .and. b .ne. 0 ) then
                value = coef              * ytab(i)**b
              else if ( a .ne. 0 .and. b .eq. 0 ) then
                value = coef * xtab(i)**a
              else if ( a .ne. 0 .and. b .ne. 0 ) then
                value = coef * xtab(i)**a * ytab(i)**b
              end if

              quad = quad + 0.5D+00 * weight(i) * value

            end do

            exact = 1.0D+00
            err = abs ( exact - quad )

            write ( *, '(2x,i4,2x,g14.6,2x,f14.8)' ) rule, quad, err

          end do

        end do

      end do

      return
      end
