      program main

c*********************************************************************72
c
cc MAIN is the main program for GMSH_IO_PRB.
c
c  Discussion:
c
c    GMSH_IO_PRB tests the GMSH_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GMSH_IO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the GMSH_IO library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GMSH_IO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 gets the example 2D data and writes it to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element_node(3,24)
      integer element_num
      integer element_order
      character * ( 255 ) gmsh_filename
      integer m
      integer node_num
      double precision node_x(2,21)

      gmsh_filename = 'example_2d.msh'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  Get example 2D data, write to a file.'
c
c  Get sizes.
c
      call gmsh_mesh2d_node_size_example ( node_num, m )

      call gmsh_mesh2d_element_size_example ( element_num, 
     &  element_order )
c
c  Print the sizes.
c
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of nodes = ', node_num
      write ( *, '(a,i4)' ) '  Spatial dimension = ', m
      write ( *, '(a,i4)' ) '  Number of elements = ', element_num
      write ( *, '(a,i4)' ) '  Order of elements = ', element_order
c
c  Get the data.
c
      call gmsh_mesh2d_node_data_example ( node_num, m, node_x )

      call gmsh_mesh2d_element_data_example ( element_num, 
     &  element_order, element_node )
c
c  Print some of the data.
c
      call r8mat_transpose_print_some ( m, node_num, node_x,
     &  1, 1, m, 10, '  Coordinates for first 10 nodes:' )

      call i4mat_transpose_print_some ( element_order, element_num, 
     &  element_node, 1, 1, element_order, 10, 
     &  '  Node connectivity of first 10 elements:' )
c
c  Write the GMSH file.
c
      call gmsh_mesh2d_write ( gmsh_filename, m, node_num, node_x,
     &  element_order, element_num, element_node )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Wrote example data to file "' //
     &  trim ( gmsh_filename ) // '"'

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 reads the example data from a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 October 2014
c
c  Author:
c
c   John Burkardt
c
      implicit none

      integer element_node(3,24)
      integer element_num
      integer element_order
      character * ( 255 ) gmsh_filename
      integer m
      integer node_num
      double precision node_x(2,21)

      gmsh_filename = 'example_2d.msh'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) '  Read data from a file.'
c
c  Get the data size.
c
      call gmsh_size_read ( gmsh_filename, node_num, m, element_num,    
     & element_order )
c
c  Print the sizes.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Node data read from file "' // trim ( gmsh_filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of nodes = ', node_num
      write ( *, '(a,i4)' ) '  Spatial dimension = ', m
      write ( *, '(a,i4)' ) '  Number of elements = ', element_num
      write ( *, '(a,i4)' ) '  Element order = ', element_order
c
c  Get the data.
c
      call gmsh_data_read ( gmsh_filename, m, node_num, node_x,
     &  element_order, element_num, element_node )
c
c  Print some of the data.
c
      call r8mat_transpose_print_some ( m, node_num, node_x,
     &  1, 1, m, 10, '  Coordinates for first 10 nodes:' )

      call i4mat_transpose_print_some ( element_order, element_num, 
     &  element_node, 1, 1, element_order, 10, 
     &  '  Connectivity for first 10 elements:' )

      return
      end

