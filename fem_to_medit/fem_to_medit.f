      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM_TO_MEDIT.
c
c  Discussion:
c
c    FEM_TO_MEDIT converts mesh data from FEM to MEDIT format.
c
c    The FEM format defines "node", "element", and "boundary_node_mask",
c    files for a mesh.  A typical set of such files might have the names
c    "suv_nodes.txt", "suv_elements.txt" and "suv_boundary_node_mask.txt".
c
c    This program reads these files and creates a MEDIT mesh file, whose
c    name might be "suv.mesh".
c
c  Usage:
c
c    fem_to_mesh prefix
c
c    reads the FEM files
c      "prefix"_nodes.txt
c      "prefix"_elements.txt and
c      "prefix"_boundary_node_mask.txt
c    and creates the MEDIT file
c      "prefix".mesh.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 255 ) boundary_node_mask_filename
      character * ( 255 ) element_filename
      integer iarg
      integer iargc
      integer ierror
      integer ilen
      integer ios
      integer ipxfargc
      character * ( 255 ) mesh_filename
      character * ( 255 ) node_filename
      integer num_arg
      character * ( 255 ) prefix

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM_TO_MEDIT'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Read a set of FEM files;'
      write ( *, '(a)' ) '  Write a corresponding MEDIT mesh file.'
c
c  Get the number of command line arguments.
c
      num_arg = iargc ( )

      if ( num_arg < 1 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Enter the filename prefix:'
        read ( *, '(a)', iostat = ios ) prefix

        if ( ios /= 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'FEM_TO_MEDIT - Fatal error!'
          write ( *, '(a)' ) '  Unexpected read error!'
          stop
        end if

      else

        iarg = 1

        call getarg ( iarg, prefix )

      end if
c
c  Set the filenames.
c
      node_filename    = trim ( prefix ) // '_nodes.txt'
      element_filename = trim ( prefix ) // '_elements.txt'
      boundary_node_mask_filename    = trim ( prefix ) // '_boundary_nod
     &e_mask.txt'
      mesh_filename    = trim ( prefix ) // '.mesh'
c
c  Now we know what to do.
c
      call fem_to_medit_handle ( node_filename, element_filename,     bo
     &undary_node_mask_filename, mesh_filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM_TO_MEDIT'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine ch_cap ( ch )

c*********************************************************************72
c
cc CH_CAP capitalizes a single character.
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
c    Input/output, character CH, the character to capitalize.
c
      implicit none

      character ch
      integer itemp

      itemp = ichar ( ch )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        ch = char ( itemp - 32 )
      end if

      return
      end
      function ch_eqi ( c1, c2 )

c*********************************************************************72
c
cc CH_EQI is a case insensitive comparison of two characters for equality.
c
c  Example:
c
c    CH_EQI ( 'A', 'a' ) is TRUE.
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
c    Input, character C1, C2, the characters to compare.
c
c    Output, logical CH_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c1_cap
      character c2
      character c2_cap
      logical ch_eqi

      c1_cap = c1
      c2_cap = c2

      call ch_cap ( c1_cap )
      call ch_cap ( c2_cap )

      if ( c1_cap .eq. c2_cap ) then
        ch_eqi = .true.
      else
        ch_eqi = .false.
      end if

      return
      end
      subroutine ch_to_digit ( c, digit )

c*********************************************************************72
c
cc CH_TO_DIGIT returns the integer value of a base 10 digit.
c
c  Example:
c
c     C   DIGIT
c    ---  -----
c    '0'    0
c    '1'    1
c    ...  ...
c    '9'    9
c    ' '    0
c    'X'   -1
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
c    Input, character C, the decimal digit, '0' through '9' or blank
c    are legal.
c
c    Output, integer DIGIT, the corresponding integer value.  If C was
c    'illegal', then DIGIT is -1.
c
      implicit none

      character c
      integer digit

      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

        digit = ichar ( c ) - 48

      else if ( c .eq. ' ' ) then

        digit = 0

      else

        digit = -1

      end if

      return
      end
      subroutine fem_data_read ( node_coord_file_name, 
     &  element_file_name, node_data_file_name, dim_num, node_num, 
     &  element_num, element_order, node_data_num, node_coord, 
     &  element_node, node_data )

c*********************************************************************72
c
cc FEM_DATA_READ reads data from a set of FEM files.
c
c  Discussion:
c
c    This program reads the node, element and data files that define
c    a finite element geometry and data based on that geometry:
c    * a set of nodes,
c    * a set of elements based on those nodes,
c    * a set of data values associated with each node.
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
c  Parameters:
c
c    Input, character * ( * ) NODE_COORD_FILE_NAME, the name of the node
c    coordinate file.
c
c    Input, character * ( * ) ELEMENT_FILE_NAME, the name of the element
c    file.  
c
c    Input, character * ( * ) NODE_DATA_FILE_NAME, the name of the node
c    data file.  
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer NODE_DATA_NUM, the number of data items
c    per node.
c
c    Output, double precision NODE_COORD(DIM_NUM,NODE_NUM), the coordinates
c    of nodes.
c
c    Output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM);
c    the global index of local node I in element J.
c
c    Output, double precision NODE_DATA(NODE_DATA_NUM,NODE_NUM), the data
c    values associated with each node.
c
      implicit none

      integer dim_num
      integer element_num
      integer element_order
      integer node_data_num
      integer node_num

      character * ( * ) element_file_name
      integer element_node(element_order,element_num)
      double precision node_coord(dim_num,node_num)
      character * ( * ) node_coord_file_name
      double precision node_data(node_data_num,node_num)
      character * ( * ) node_data_file_name

      if ( len_trim ( node_coord_file_name ) .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FEM_DATA_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  No node coordinate file name was supplied!'
        stop 1
      end if

      call r8mat_data_read ( node_coord_file_name, dim_num, node_num, 
     &  node_coord )

      if ( 0 .le. len_trim ( element_file_name ) ) then
        call i4mat_data_read ( element_file_name, element_order,
     &    element_num, element_node )
      end if

      if ( 0 .le. len_trim ( node_data_file_name ) ) then
        call r8mat_data_read ( node_data_file_name, node_data_num, 
     &    node_num, node_data )
      end if

      return
      end
      subroutine fem_header_print ( dim_num, node_num, element_num,
     &  element_order, node_data_num )

c*********************************************************************72
c
cc FEM_HEADER_PRINT prints the header to set of FEM files.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer NODE_DATA_NUM, the number of data items
c    per node.
c
      implicit none

      integer dim_num
      integer element_num
      integer element_order
      integer node_data_num
      integer node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension         = ', dim_num
      write ( *, '(a,i8)' ) '  Number of nodes           = ', node_num
      write ( *, '(a,i8)' ) 
     &  '  Number of elements        = ', element_num
      write ( *, '(a,i8)' ) 
     &  '  Element order             = ', element_order
      write ( *, '(a,i8)' ) 
     &  '  Number of node data items = ', node_data_num

      return
      end
      subroutine fem_header_read ( node_coord_file_name, 
     &  element_file_name, node_data_file_name, dim_num, node_num, 
     &  element_num, element_order, node_data_num )

c*********************************************************************72
c
cc FEM_HEADER_READ reads the sizes of arrays in a set of FEM files.
c
c  Discussion:
c
c    This program reads the node, element and data files that define
c    a finite element geometry and data based on that geometry:
c    * a set of nodes,
c    * a set of elements based on those nodes,
c    * a set of data values associated with each node.
c    and returns the sizes DIM_NUM, NODE_NUM, ELEMENT_NUM, ELEMENT_ORDER,
c    and NODE_DATA_NUM required to allocate space for these arrays.
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
c  Parameters:
c
c    Input, character * ( * ) NODE_COORD_FILE_NAME, the name of the node
c    coordinate file.  If this argument is not supplied, the program terminates.
c
c    Input, character * ( * ) ELEMENT_FILE_NAME, the name of the element
c    file.  
c
c    Input, character * ( * ) NODE_DATA_FILE_NAME, the name of the node
c    data file.  
c
c    Output, integer DIM_NUM, the spatial dimension, inferred from
c    the "shape" of the data in the node file.
c
c    Output, integer NODE_NUM, the number of nodes, inferred from
c    the number of lines of data in the node coordinate file.
c
c    Output, integer ELEMENT_NUM, the number of elements, inferred
c    from the number of lines of data in the element file.
c
c    Output, integer ELEMENT_ORDER, the order of the elements,
c    inferred from the number of items in the first line of the element file.
c
c    Output, integer NODE_DATA_NUM, the number of data items per
c    node, inferred from the number of items in the first line of the node
c    data file.
c
      implicit none

      integer dim_num
      character * ( * ) element_file_name
      integer element_num
      integer element_order
      character * ( * ) node_coord_file_name
      character * ( * ) node_data_file_name
      integer node_data_num
      integer node_num
      integer node_num2

      if ( len_trim ( node_coord_file_name ) .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FEM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The node coordinate file was not supplied!'
        stop 1
      end if

      if ( len_trim ( element_file_name ) .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FEM_HEADER_READ:'
        write ( *, '(a)' ) '  No element file name was supplied.'
        write ( *, '(a)' ) '  No element data will be returned.'
      end if

      if ( len_trim ( node_data_file_name ) .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FEM_HEADER_READ:'
        write ( *, '(a)' ) '  No node data file name was supplied!'
        write ( *, '(a)' ) '  No node data will be returned.'
      end if
c
c  Read the node coordinate file.
c
      call r8mat_header_read ( node_coord_file_name, dim_num, node_num )

      if ( 0 .lt. len_trim ( element_file_name ) ) then
        call i4mat_header_read ( element_file_name, element_order, 
     &    element_num )
      else
        element_order = 0
        element_num = 0
      end if

      if ( 0 .lt. len_trim ( node_data_file_name ) ) then

        call r8mat_header_read ( node_data_file_name, node_data_num, 
     &    node_num2 )

        if ( node_num2 .ne. node_num ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 
     &      '  The number of nodes in the node coordinate '
          write ( *, '(a,i6,a)' ) '  file is ', node_num, 
     &      ' but the number of nodes'
          write ( *, '(a,i6)' ) '  in the node data file is ', node_num2
          write ( *, '(a)' ) '  No node data will be stored.'
          node_data_num = 0
        end if

      else
        node_data_num = 0
      end if

      return
      end
      subroutine fem_to_medit_handle ( node_filename, element_filename, 
     &  boundary_node_mask_filename, mesh_filename )

c*********************************************************************72
c
cc FEM_TO_MEDIT_HANDLE copies mesh data from a FEM dataset to a MEDIT data set.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 December 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) NODE_FILENAME, the FEM node filename.
c
c    Input, character * ( * ) ELEMENT_FILENAME, the FEM element filename.
c
c    Input, character * ( * ) BOUNDARY_NODE_MASK_FILENAME, the FEM
c    boundary node mask filename.
c
c    Input, character * ( * ) MESH_FILENAME, the MESH filename.
c
      implicit none

      character * ( * ) boundary_node_mask_filename
      integer dim_num
      character * ( * ) element_filename
      integer element_num
      integer element_order
      character * ( * ) mesh_filename
      character * ( * ) node_filename
      integer node_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reading FEM mesh files:'
      write ( *, '(a)' ) '    "' // trim ( node_filename ) // '".'
      write ( *, '(a)' ) '    "' // trim ( element_filename ) // '".'
      write ( *, '(a)' ) '    "' // trim ( boundary_node_mask_filename )
     & // '".'

      call fem_header_read ( node_filename, element_filename,
     &  dim_num, node_num, element_num, element_order )

      call fem_header_print ( dim_num, node_num, element_num, 
     &  element_order )

      call fem_to_medit_handle2 ( node_filename, element_filename, 
     &  boundary_node_mask_filename, mesh_filename, dim_num, node_num, 
     &  element_num, element_order )

      return
      end
      subroutine fem_to_medit_handle2 ( node_filename, element_filename, 
     &  boundary_node_mask_filename, mesh_filename, dim_num, node_num, 
     &  element_num, element_order )

c*********************************************************************72
c
cc FEM_TO_MEDIT_HANDLE copies mesh data from a FEM dataset to a MEDIT data set.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 December 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) NODE_FILENAME, the FEM node filename.
c
c    Input, character * ( * ) ELEMENT_FILENAME, the FEM element filename.
c
c    Input, character * ( * ) BOUNDARY_NODE_MASK_FILENAME, the FEM
c    boundary node mask filename.
c
c    Input, character * ( * ) MESH_FILENAME, the MESH filename.
c
      implicit none

      integer dim_num
      integer element_num
      integer element_order
      integer node_num

      integer boundary_node_mask(node_num)
      character * ( * ) boundary_node_mask_filename
      integer dim
      integer edge_label(0)
      integer edge_vertex(2,0)
      integer edges
      character * ( * ) element_filename
      integer element_node(element_order,element_num)
      integer hexahedron_label(0)
      integer hexahedron_vertex(8,0)
      integer hexahedrons
      integer i
      integer j
      character * ( * ) mesh_filename
      double precision node_coord(dim_num,node_num)
      character * ( * ) node_filename
      integer quadrilateral_label(0)
      integer quadrilateral_vertex(4,0)
      integer quadrilaterals
      integer tetrahedron_label(element_num)
      integer tetrahedron_vertex(4,element_num)
      integer tetrahedrons
      integer triangle_label(element_num)
      integer triangle_vertex(3,element_num)
      integer triangles
      double precision vertex_coordinate(dim_num,node_num)
      integer vertex_label(node_num)
      integer vertices
c
c  Read the FEM data.
c
      call fem_data_read ( node_filename, element_filename,
     &  boundary_node_mask_filename, dim_num, node_num, element_num,
     &  element_order, node_coord, element_node, boundary_node_mask )
c
c  Set up the MEDIT data.
c
      dim = dim_num
      vertices = node_num
      edges = 0
      if ( dim .eq. 2 ) then
        triangles = element_num
      else
        triangles = 0
      end if
      quadrilaterals = 0
      if ( dim .eq. 3 ) then
        tetrahedrons = element_num
      else
        tetrahedrons = 0
      end if
      hexahedrons = 0

      do j = 1, triangles
        do i = 1, 3
          triangle_vertex(i,j) = element_node(i,j)
        end do
      end do

      do j = 1, triangles
        triangle_label(1:triangles) = 0
      end do

      do j = 1, tetrahedrons
        do i = 1, 4
          tetrahedron_vertex(i,j) = element_node(i,j)
        end do
      end do

      do j = 1, tetrahedrons
        tetrahedron_label(j) = 0
      end do

      do j = 1, vertices
        do i = 1, dim
          vertex_coordinate(i,j) = node_coord(i,j)
        end do
      end do

      do i = 1, vertices
        vertex_label(i) = boundary_node_mask(i)
      end do
c
c  Write the MEDIT file.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Writing MEDIT mesh file: "' // 
     &  trim ( mesh_filename ) // '".'

      call medit_write ( mesh_filename, dim, vertices, edges, triangles,
     &  quadrilaterals, tetrahedrons, hexahedrons, vertex_coordinate,
     &  vertex_label, edge_vertex, edge_label, triangle_vertex, 
     &  triangle_label, quadrilateral_vertex, quadrilateral_label, 
     &  tetrahedron_vertex, tetrahedron_label, hexahedron_vertex, 
     &  hexahedron_label )

      return
      end
      subroutine file_column_count ( input_filename, column_num )

c*********************************************************************72
c
cc FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
c
c  Discussion:
c
c    The file is assumed to be a simple text file.
c
c    Most lines of the file is presumed to consist of COLUMN_NUM words,
c    separated by spaces.  There may also be some blank lines, and some
c    comment lines,
c    which have a "#" in column 1.
c
c    The routine tries to find the first non-comment non-blank line and
c    counts the number of words in that line.
c
c    If all lines are blanks or comments, it goes back and tries to analyze
c    a comment line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the file.
c
c    Output, integer COLUMN_NUM, the number of columns in the file.
c
      implicit none

      integer column_num
      logical got_one
      character * ( * ) input_filename
      integer input_unit
      character * ( 255 ) line
c
c  Open the file.
c
      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old', form = 'formatted', access = 'sequential' )
c
c  Read one line, but skip blank lines and comment lines.
c
      got_one = .false.

10    continue

        read ( input_unit, '(a)', err = 20 ) line

        if ( len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        if ( line(1:1) .eq. '#' ) then
          go to 10
        end if

        got_one = .true.
        go to 20

      go to 10

20    continue

      if ( .not. got_one ) then

        rewind ( input_unit )

30      continue

          read ( input_unit, '(a)', err = 40 ) line

          if ( len_trim ( line ) .eq. 0 ) then
            go to 30
          end if

          got_one = .true.
          go to 40

        go to 30

40    continue

      end if

      close ( unit = input_unit )

      if ( .not. got_one ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Warning.'
        write ( *, '(a)' ) '  The file does not contain any data.'
        column_num = -1
        return
      end if

      call s_word_count ( line, column_num )

      return
      end
      subroutine file_row_count ( input_filename, row_num )

c*********************************************************************72
c
cc FILE_ROW_COUNT counts the number of row records in a file.
c
c  Discussion:
c
c    It does not count lines that are blank, or that begin with a
c    comment symbol '#'.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Output, integer ROW_NUM, the number of rows found.
c
      implicit none

      integer bad_num
      integer comment_num
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      character * ( 255 ) line
      integer record_num
      integer row_num

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old' )

      comment_num = 0
      row_num = 0
      record_num = 0
      bad_num = 0

10    continue

        read ( input_unit, '(a)', err = 20, end = 20 ) line

        record_num = record_num + 1

        if ( line(1:1) .eq. '#' ) then
          comment_num = comment_num + 1
          go to 10
        end if

        if ( len_trim ( line ) .eq. 0 ) then
          comment_num = comment_num + 1
          go to 10
        end if

        row_num = row_num + 1

      go to 10

20    continue

      close ( unit = input_unit )

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine i4mat_data_read ( input_filename, m, n, table )

c*********************************************************************72
c
cc I4MAT_DATA_READ reads data from an I4MAT file.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c    The file may contain more than N points, but this routine
c    will return after reading N points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Output, integer TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer i
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      integer j
      character * ( 255 ) line
      integer table(m,n)
      integer x(m)

      ierror = 0

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old' )

      j = 0

10    continue

      if ( j .lt. n ) then

        read ( input_unit, '(a)' ) line

        if ( line(1:1) .eq. '#' .or. len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        call s_to_i4vec ( line, m, x, ierror )

        if ( ierror .ne. 0 ) then
          go to 10
        end if

        j = j + 1

        do i = 1, m
          table(i,j) = x(i)
        end do

        go to 10

      end if

      close ( unit = input_unit )

      return
      end
      subroutine i4mat_header_read ( input_filename, m, n )

c*********************************************************************72
c
cc I4MAT_HEADER_READ reads the header from an integer table file.
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
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Output, integer M, spatial dimension.
c
c    Output, integer N, the number of points.
c
      implicit none

      character * ( * ) input_filename
      integer m
      integer n

      call file_column_count ( input_filename, m )

      if ( m .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4MAT_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  There was an I/O problem while'
        write ( *, '(a)' ) '  trying to count the number of data'
        write ( *, '(a,a,a)' ) '  columns in "', input_filename, '".'
        stop 1
      end if

      call file_row_count ( input_filename, n )

      if ( n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4MAT_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  There was an I/O problem while'
        write ( *, '(a)' ) '  trying to count the number of data rows'
        write ( *, '(a,a,a)' ) '  in "', input_filename, '".'
        stop 1
      end if

      return
      end
      subroutine medit_write ( filename, dim, vertices, edges,
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons,
     &  vertex_coordinate, vertex_label, edge_vertex, edge_label,
     &  triangle_vertex, triangle_label, quadrilateral_vertex,
     &  quadrilateral_label, tetrahedron_vertex, tetrahedron_label,
     &  hexahedron_vertex, hexahedron_label )

c*********************************************************************72
c
cc MEDIT_WRITE writes sizes and data to a MEDIT mesh file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Pascal Frey,
c    MEDIT: An interactive mesh visualization software,
c    Technical Report RT-0253,
c    Institut National de Recherche en Informatique et en Automatique,
c    03 December 2001.
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file to be created.
c    Ordinarily, the name should include the extension ".mesh".
c
c    Input, integer DIM, the spatial dimension, which should be 2 or 3.
c
c    Input, integer VERTICES, the number of vertices.
c
c    Input, double precision VERTEX_COORRDINATE(DIM,VERTICES), the coordinates
c    of each vertex.
c
c    Input, integer VERTEX_LABEL(VERTICES), a label for each vertex.
c
c    Input, integer EDGES, the number of edges (may be 0).
c
c    Input, integer EDGE_VERTEX(2,EDGES), the vertices that form each edge.
c
c    Input, integer EDGE_LABEL(EDGES), a label for each edge.
c
c    Input, integer TRIANGLES, the number of triangles (may be 0).
c
c    Input, integer TRIANGLE_VERTEX(3,TRIANGLES), the vertices that form
c    each triangle.
c
c    Input, integer TRIANGLE_LABEL(TRIANGLES), a label for each triangle.
c
c    Input, integer QUADRILATERALS, the number of quadrilaterals (may be 0).
c
c    Input, integer QUADRILATERAL_VERTEX(4,QUADRILATERALS), the vertices that
c    form each quadrilateral.
c
c    Input, integer QUADRILATERAL_LABEL(QUADRILATERALS), a label for
c    each quadrilateral.
c
c    Input, integer TETRAHEDRONS, the number of tetrahedrons (may be 0).
c
c    Input, integer TETRAHEDRON_VERTEX(4,TETRAHEDRONS), the vertices that
c    form each tetrahedron.
c
c    Input, integer TETRAHEDRON_LABEL(TETRAHEDRONS), a label for
c    each tetrahedron.
c
c    Input, integer HEXAHEDRONS, the number of hexahedrons (may be 0).
c
c    Input, integer HEXAHEDRON_VERTEX(8,HEXAHEDRONS), the vertices that form
c    each hexahedron.
c
c    Input, integer HEXAHEDRON_LABEL(HEXAHEDRONS), a label for each hexahedron.
c
      implicit none

      integer dim
      integer edges
      integer hexahedrons
      integer quadrilaterals
      integer tetrahedrons
      integer triangles
      integer vertices

      integer edge_label(edges)
      integer edge_vertex(2,edges)
      character * ( * ) filename
      integer fileunit
      integer hexahedron_label(hexahedrons)
      integer hexahedron_vertex(8,hexahedrons)
      integer i
      integer ios
      integer j
      integer quadrilateral_label(quadrilaterals)
      integer quadrilateral_vertex(4,quadrilaterals)
      integer tetrahedron_label(tetrahedrons)
      integer tetrahedron_vertex(4,tetrahedrons)
      integer triangle_label(triangles)
      integer triangle_vertex(3,triangles)
      double precision vertex_coordinate(dim,vertices)
      integer vertex_label(vertices)
c
c  Open the file.
c
      call get_unit ( fileunit )

      open ( unit = fileunit, file = filename, status = 'replace',
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MEDIT_WRITE - Fatal errorc'
        write ( *, '(a)' ) '  Could not open file.'
        stop
      end if

      write ( fileunit, '(a)' ) 'MeshVersionFormatted 1'
      write ( fileunit, '(a)' ) '#  Created by medit_write.f'
c
c  Dimension information.
c
      write ( fileunit, '(a)' ) ' '
      write ( fileunit, '(a)' ) 'Dimension'
      write ( fileunit, '(i8)' ) dim
c
c  Vertices.
c
      write ( fileunit, '(a)' ) ' '
      write ( fileunit, '(a)' ) 'Vertices'
      write ( fileunit, '(i8)' ) vertices
      if ( dim == 2 ) then
        do j = 1, vertices
          write ( fileunit, '(2(2x,f10.6),2x,i8)' )
     &      ( vertex_coordinate(i,j), i = 1, dim ), vertex_label(j)
        end do
      else if ( dim == 3 ) then
        do j = 1, vertices
          write ( fileunit, '(3(2x,f10.6),2x,i8)' )
     &      ( vertex_coordinate(i,j), i = 1, dim ), vertex_label(j)
        end do
      end if
c
c  Edges.
c
      if ( 0 .lt. edges ) then
        write ( fileunit, '(a)' ) ' '
        write ( fileunit, '(a)' ) 'Edges'
        write ( fileunit, '(i8)' ) edges
        do j = 1, edges
          write ( fileunit, '(2(2x,i8),2x,i8)' )
     &      ( edge_vertex(i,j), i = 1, 2 ), edge_label(j)
        end do
      end if
c
c  Triangles.
c
      if ( 0 .lt. triangles ) then
        write ( fileunit, '(a)' ) ' '
        write ( fileunit, '(a)' ) 'Triangles'
        write ( fileunit, '(i8)' ) triangles
        do j = 1, triangles
          write ( fileunit, '(3(2x,i8),2x,i8)' )
     &      ( triangle_vertex(i,j), i = 1, 3 ), triangle_label(j)
        end do
      end if
c
c  Quadrilaterals.
c
      if ( 0 .lt. quadrilaterals ) then
        write ( fileunit, '(a)' ) ' '
        write ( fileunit, '(a)' ) 'Quadrilaterals'
        write ( fileunit, '(i8)' ) quadrilaterals
        do j = 1, quadrilaterals
          write ( fileunit, '(4(2x,i8),2x,i8)' )
     &      ( quadrilateral_vertex(i,j), i = 1, 4 ),
     &      quadrilateral_label(j)
        end do
      end if
c
c  Tetrahedron.
c
      if ( 0 .lt. tetrahedrons ) then
        write ( fileunit, '(a)' ) ' '
        write ( fileunit, '(a)' ) 'Tetrahedra'
        write ( fileunit, '(i8)' ) tetrahedrons
        do j = 1, tetrahedrons
          write ( fileunit, '(4(2x,i8),2x,i8)' )
     &      ( tetrahedron_vertex(i,j), i = 1, 4 ), tetrahedron_label(j)
        end do
      end if
c
c  Hexahedron.
c
      if ( 0 .lt. hexahedrons ) then
        write ( fileunit, '(a)' ) ' '
        write ( fileunit, '(a)' ) 'Hexahedra'
        write ( fileunit, '(i8)' ) hexahedrons
        do j = 1, hexahedrons
          write ( fileunit, '(8(2x,i8),2x,i8)' )
     &      ( hexahedron_vertex(i,j), i = 1, 8 ), hexahedron_label(j)
        end do
      end if
c
c  End.
c
      write ( fileunit, '(a)' ) ' '
      write ( fileunit, '(a)' ) 'End'

      close ( unit = fileunit )

      return
      end
      subroutine r8mat_data_read ( input_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_DATA_READ reads data from an R8MAT file.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    The file may contain more than N points, but this routine will
c    return after reading N of them.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Output, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer  n

      integer i
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      integer j
      character * ( 255 ) line
      double precision table(m,n)
      double precision x(m)

      ierror = 0

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old' )

      j = 0

10    continue

      if ( j .lt. n ) then

        read ( input_unit, '(a)' ) line

        if ( line(1:1) .eq. '#' .or. len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        call s_to_r8vec ( line, m, x, ierror )

        if ( ierror .ne. 0 ) then
          go to 10
        end if

        j = j + 1

        do i = 1, m
          table(i,j) = x(i)
        end do

        go to 10

      end if

      close ( unit = input_unit )

      return
      end
      subroutine r8mat_header_read ( input_filename, m, n )

c*********************************************************************72
c
cc R8MAT_HEADER_READ reads the header from an R8MAT file.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Output, integer M, spatial dimension.
c
c    Output, integer N, the number of points.
c
      implicit none

      character * ( * ) input_filename
      integer m
      integer n

      call file_column_count ( input_filename, m )

      if ( m .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  There was an I/O problem while trying'
        write ( *, '(a)' ) '  to count the number of data columns in'
        write ( *, '(a,a,a)' )
     &    '  the file "', trim ( input_filename ), '".'
        stop 1
      end if

      call file_row_count ( input_filename, n )

      if ( n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  There was an I/O problem while trying'
        write ( *, '(a)' ) '  to count the number of data rows in'
        write ( *, '(a,a,a)' )
     &    '  the file "', trim ( input_filename ), '".'
        stop 1
      end if

      return
      end
      subroutine s_to_i4 ( s, ival, ierror, length )

c*********************************************************************72
c
cc S_TO_I4 reads an I4 from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, a string to be examined.
c
c    Output, integer IVAL, the integer value read from the string.
c    If the string is blank, then IVAL will be returned 0.
c
c    Output, integer IERROR, an error flag.
c    0, no error.
c    1, an error occurred.
c
c    Output, integer LENGTH, the number of characters of S
c    used to make IVAL.
c
      implicit none

      character c
      integer i
      integer ierror
      integer isgn
      integer istate
      integer ival
      integer length
      character * ( * ) s
      integer s_len

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0
      s_len = len_trim ( s )

      do i = 1, s_len

        c = s(i:i)
c
c  Haven't read anything.
c
        if ( istate .eq. 0 ) then

          if ( c .eq. ' ' ) then

          else if ( c .eq. '-' ) then
            istate = 1
            isgn = -1
          else if ( c .eq. '+' ) then
            istate = 1
            isgn = + 1
          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read the sign, expecting digits.
c
        else if ( istate .eq. 1 ) then

          if ( c .eq. ' ' ) then

          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read at least one digit, expecting more.
c
        else if ( istate .eq. 2 ) then

          if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            ival = 10 * ival + ichar ( c ) - ichar ( '0' )
          else
            ival = isgn * ival
            length = i - 1
            return
          end if

        end if

      end do
c
c  If we read all the characters in the string, see if we're OK.
c
      if ( istate .eq. 2 ) then
        ival = isgn * ival
        length = len_trim ( s )
      else
        ierror = 1
        length = 0
      end if

      return
      end
      subroutine s_to_i4vec ( s, n, ivec, ierror )

c*********************************************************************72
c
cc S_TO_I4VEC reads an I4VEC from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be read.
c
c    Input, integer N, the number of values expected.
c
c    Output, integer IVEC(N), the values read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    -K, could not read data for entries -K through N.
c
      implicit none

      integer n

      integer i
      integer ierror
      integer ilo
      integer ivec(n)
      integer length
      character * ( * ) s

      i = 0
      ierror = 0
      ilo = 1

10    continue

      if ( i .lt. n ) then

        i = i + 1

        call s_to_i4 ( s(ilo:), ivec(i), ierror, length )

        if ( ierror .ne. 0 ) then
          ierror = -i
          go to 20
        end if

        ilo = ilo + length

      go to 10

      end if

20    continue

      return
      end
      subroutine s_to_r8 ( s, dval, ierror, length )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 from a string.
c
c  Discussion:
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 DVAL
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision DVAL, the value read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    1, 2, 6 or 7, the input number was garbled.  The
c    value of IERROR is the last type of input successfully
c    read.  For instance, 1 means initial blanks, 2 means
c    a plus or minus sign, and so on.
c
c    Output, integer LENGTH, the number of characters read
c    to form the number, including any terminating
c    characters such as a trailing comma or blanks.
c
      implicit none

      logical ch_eqi
      character c
      double precision dval
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer nchar
      integer ndig
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s

      nchar = len_trim ( s )

      ierror = 0
      dval = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( nchar .lt. length + 1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 .lt. ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if ( ihave .lt. 11 .and. lle ( '0', c )
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          call ch_to_digit ( c, ndig )

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

        go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to NCHAR.
c
      if ( iterm .ne. 1 .and. length+1 .eq. nchar ) then
        length = nchar
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7.
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or.
     &     ihave .eq. 6 .or. ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious error!'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a,a)' ) '    ', s
        return
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      dval = dble ( isgn ) * rexp * rtop / rbot

      return
      end
      subroutine s_to_r8vec ( s, n, rvec, ierror )

c*********************************************************************72
c
cc S_TO_R8VEC reads an R8VEC from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be read.
c
c    Input, integer N, the number of values expected.
c
c    Output, double precision RVEC(N), the values read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    -K, could not read data for entries -K through N.
c
      implicit none

      integer  n

      integer i
      integer ierror
      integer ilo
      integer lchar
      double precision rvec(n)
      character * ( * ) s

      i = 0
      ierror = 0
      ilo = 1

10    continue

      if ( i .lt. n ) then

        i = i + 1

        call s_to_r8 ( s(ilo:), rvec(i), ierror, lchar )

        if ( ierror .ne. 0 ) then
          ierror = -i
          go to 20
        end if

        ilo = ilo + lchar

        go to 10

      end if

20    continue

      return
      end
      subroutine s_word_count ( s, nword )

c*********************************************************************72
c
cc S_WORD_COUNT counts the number of "words" in a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be examined.
c
c    Output, integer NWORD, the number of "words" in the string.
c    Words are presumed to be separated by one or more blanks.
c
      implicit none

      logical blank
      integer i
      integer lens
      integer nword
      character * ( * ) s

      nword = 0
      lens = len ( s )

      if ( lens .le. 0 ) then
        return
      end if

      blank = .true.

      do i = 1, lens

        if ( s(i:i) .eq. ' ' ) then
          blank = .true.
        else if ( blank ) then
          nword = nword + 1
          blank = .false.
        end if

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
