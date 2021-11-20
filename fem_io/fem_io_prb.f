      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM_IO_PRB.
c
c  Discussion:
c
c    FEM_IO_PRB tests the FEM_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM_IO_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FEM_IO library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM_IO_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests FEM_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      character * ( 80 ) element_file_name
      integer element_node(3,96)
      integer element_num
      integer element_order
      double precision node_coord(2,65)
      character * ( 80 ) :: node_coord_file_name
      double precision node_data(1,65)
      character ( len = 80 ) node_data_file_name
      integer node_data_num
      integer node_num

      element_file_name = 'ell_elements.txt'
      node_coord_file_name = 'ell_nodes.txt'
      node_data_file_name = 'ell_values.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  FEM_READ reads finite element data from files.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The node coordinate file name is "' //
     &  trim ( node_coord_file_name ) // '".'
      write ( *, '(a)' ) '  The element file name is "' //
     &  trim ( element_file_name ) // '".'
      write ( *, '(a)' ) '  The node data file name is "' //
     &  trim ( node_data_file_name ) // '".'

      call fem_header_read ( node_coord_file_name, element_file_name,   
     &  node_data_file_name, dim_num, node_num, element_num,
     &  element_order, node_data_num )

      call fem_data_read ( node_coord_file_name, element_file_name,     
     &  node_data_file_name, dim_num, node_num, element_num,
     &  element_order, node_data_num, node_coord, element_node, 
     &  node_data )

      call fem_header_print ( dim_num, node_num, element_order, 
     &  element_num, node_data_num )

      call r8mat_transpose_print_some ( dim_num, node_num, node_coord, 
     &  1, 1, dim_num,  10, '  First 10 node coordindates:' )

      call i4mat_transpose_print_some ( element_order, element_num,     
     &element_node, 1, 1, element_order, 10, '  First 10 elements' )

      call r8mat_transpose_print_some ( node_data_num, node_num, 
     &  node_data, 1, 1, node_data_num, 10, 
     &  '  First 10 node data sets:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc FEM_IO_TEST02 tests FEM_WRITE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer node_num
      parameter ( node_num = 5 )
      integer element_num
      parameter ( element_num = 3 )
      integer element_order
      parameter ( element_order = 3 )
      integer node_data_num
      parameter ( node_data_num = 2 )

      character * ( 80 ) element_file_name
      integer element_node(element_order,element_num)
      double precision node_coord(dim_num,node_num)
      character * ( 80 ) node_coord_file_name
      double precision node_data(node_data_num,node_num)
      character * ( 80 ) node_data_file_name

      save element_node

      data element_node /
     &  1, 2, 4,
     &  5, 4, 2,
     &  2, 3, 5 /
      data node_coord /
     &  0.0D+00, 0.0D+00,
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 0.0D+00,
     &  0.0D+00, 1.0D+00,
     &  1.0D+00, 1.0D+00 /
      data node_data /
     &  1.0D+00, 0.0D+00,
     &  0.8D+00, 0.2D+00,   
     &  0.6D+00, 0.4D+00,
     &  0.9D+00, 0.1D+00,
     &  0.5D+00, 0.5D+00 /

      element_file_name = 'tiny_elements.txt'
      node_coord_file_name = 'tiny_nodes.txt'
      node_data_file_name = 'tiny_values.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM_TEST02'
      write ( *, '(a)' ) 
     &  '  Demonstrate the use of FEM_WRITE to write finite'
      write ( *, '(a)' ) '  element data to files.'

      write ( *, '(a)' ) ' '

      write ( *, '(a)' ) '  The node coordinate file name is "' //
     &  trim ( node_coord_file_name ) // '".'
      write ( *, '(a)' ) '  The element file name is "' //
     &  trim ( element_file_name ) // '".'
      write ( *, '(a)' ) '  The node data file name is "' //
     &  trim ( node_data_file_name ) // '".'

      call fem_header_print ( dim_num, node_num, element_order, 
     &  element_num, node_data_num )

      call r8mat_transpose_print ( dim_num, node_num, node_coord,
     &  '  Node coordindates:' )

      call i4mat_transpose_print ( element_order, element_num,
     &  element_node, '  Elements' )

      call r8mat_transpose_print ( node_data_num, node_num, node_data,  
     &   '  Node data sets:' )

      call fem_write ( node_coord_file_name, element_file_name,
     &  node_data_file_name, dim_num, node_num, element_num,
     &  element_order, node_data_num, node_coord, element_node, 
     &  node_data )

      return
      end
