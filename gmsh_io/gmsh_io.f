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
      subroutine gmsh_data_read ( gmsh_filename, node_dim, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc GMSH_DATA_READ reads data from a GMSH file.
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
c  Parameters:
c
c    Input, character * ( * ) GMSH_FILENAME, the GMSH filename.
c
c    Input, integer NODE_DIM, the spatial dimension.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_X(NODE_DIM,NODE_NUM), the node coordinates.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
c    the nodes that make up each element.
c

      implicit none

      integer element_num
      integer element_order
      integer node_dim
      integer node_num

      integer element_node(element_order,element_num)
      character * ( * ) gmsh_filename
      integer i
      integer i4_dummy
      integer ierror
      integer indx
      integer input
      integer input_stat
      integer j
      integer k
      integer length
      integer level
      double precision node_x(node_dim,node_num)
      double precision r8_big
      parameter ( r8_big = 1.0D+30 )
      logical s_begin
      character * ( 255 ) text
      double precision x
      double precision x_max
      double precision x_min
      double precision y
      double precision y_max
      double precision y_min
      double precision z
      double precision z_max
      double precision z_min

      call get_unit ( input )

      open ( unit = input, file = gmsh_filename, status = 'old',
     &  iostat = input_stat )

      if ( input_stat .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
        write ( *, '(a)' ) '  Could not open input file "' //
     &    trim ( gmsh_filename ) // '"'
        stop 1
      end if

      level = 0

10    continue

        read ( input, '(a)', iostat = input_stat ) text

        if ( input_stat .ne. 0 ) then
          write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Error while seeking node coordinates.'
          stop 1
        end if

        if ( level .eq. 0 ) then
          if ( s_begin ( text(1:6), '$Nodes' ) ) then
            level = 1
            j = 0
          end if
        else if ( level .eq. 1 ) then
          call s_to_i4 ( text, i4_dummy, ierror, length )
          level = 2
        else if ( level .eq. 2 ) then
          if ( s_begin ( text(1:9), '$EndNodes' ) ) then
            go to 20
          else
            j = j + 1
            call s_to_i4 ( text, indx, ierror, length )
            text = text(length+1:)
            do i = 1, node_dim
              call s_to_r8 ( text, x, ierror, length )
              text = text(length+1:)
              node_x(i,j) = x
            end do
          end if
        end if

      go to 10

20    continue
c
c  Now read element information.
c
      level = 0

30    continue

        read ( input, '(a)', iostat = input_stat ) text

        if ( input_stat .ne. 0 ) then
          write ( *, '(a)' ) 'GMSH_DATA_READ - Fatal error!'
          write ( *, '(a)' ) 
     &      '  Error while seeking element connectivity.'
          stop 1
        end if

        if ( level .eq. 0 ) then
          if ( s_begin ( text(1:9), '$Elements' ) ) then
            level = 1
            j = 0
          end if
        else if ( level .eq. 1 ) then
          call s_to_i4 ( text, i4_dummy, ierror, length )
          level = 2
        else if ( level .eq. 2 ) then
          if ( s_begin ( text(1:12), '$EndElements' ) ) then
            go to 40
          else
            j = j + 1
            k = 0
            do k = 1, 5
              call s_to_i4 ( text, i4_dummy, ierror, length )
              text = text(length+1:)
            end do
            do i = 1, element_order
              call s_to_i4 ( text, k, ierror, length )
              text = text(length+1:)
              element_node(i,j) = k
            end do
          end if
        end if

      go to 30

40    continue

      close ( unit = input )

      return
      end
      subroutine gmsh_size_read ( gmsh_filename, node_num, node_dim, 
     &  element_num, element_order )

c*********************************************************************72
c
cc GMSH_SIZE_READ reads sizes from a GMSH file.
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
c  Parameters:
c
c    Input, character * ( * ) GMSH_FILENAME, the GMSH filename.
c
c    Output, integer NODE_NUM, the number of nodes.
c
c    Output, integer NODE_DIM, the spatial dimension.
c
c    Output, integer ELEMENT_NUM, the number of elements.
c
c    Output, integer ELEMENT_ORDER, the order of the elements.
c
      implicit none

      integer element_num
      integer element_order
      character * ( * ) gmsh_filename
      integer ierror
      integer indx
      integer input
      integer input_stat
      integer k
      integer length
      integer level
      integer node_dim
      integer node_num
      double precision r8_big
      parameter ( r8_big = 1.0D+30 )
      logical s_begin
      character * ( 255 ) text
      double precision x
      double precision x_max
      double precision x_min
      double precision y
      double precision y_max
      double precision y_min
      double precision z
      double precision z_max
      double precision z_min

      node_num = 0
      node_dim = 0

      x_max = - r8_big
      x_min = + r8_big
      y_max = - r8_big
      y_min = + r8_big
      z_max = - r8_big
      z_min = + r8_big

      call get_unit ( input )

      open ( unit = input, file = gmsh_filename, status = 'old',
     &  iostat = input_stat )

      if ( input_stat .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'GMSH_SIZE_READ - Fatal error!'
        write ( *, '(a)' ) '  Could not open input file "' //
     &    trim ( gmsh_filename ) // '"'
        stop 1
      end if

      level = 0

10    continue

        read ( input, '(a)', iostat = input_stat ) text

        if ( level .eq. 0 ) then
          if ( s_begin ( text(1:6), '$Nodes' ) ) then
            level = 1
          end if
        else if ( level .eq. 1 ) then
          call s_to_i4 ( text, node_num, ierror, length )
          level = 2
        else if ( level .eq. 2 ) then
          if ( s_begin ( text(1:9), '$EndNodes' ) ) then
            go to 20
          else
            call s_to_i4 ( text, indx, ierror, length )
            text = text(length+1:)
            call s_to_r8 ( text, x, ierror, length )
            x_min = min ( x_min, x )
            x_max = max ( x_max, x )
            text = text(length+1:)
c
c  Need to check that we actually were able to read an R8 here.
c
            call s_to_r8 ( text, y, ierror, length )
            y_min = min ( y_min, y )
            y_max = max ( y_max, y )
            text = text(length+1:)
            call s_to_r8 ( text, z, ierror, length )
            text = text(length+1:)
            z_min = min ( z_min, z )
            z_max = max ( z_max, z )
          end if
        end if

      go to 10

20    continue
c
c  Make a very simple guess as to the dimensionality of the data.
c
      node_dim = 3
      if ( z_max .eq. z_min ) then
        node_dim = 2
        if ( y_max .eq. y_min ) then
          node_dim = 1
        end if
      end if
c
c  Now read element information.
c
      level = 0

30    continue

        read ( input, '(a)', iostat = input_stat ) text

        if ( level .eq. 0 ) then
          if ( s_begin ( text(1:9), '$Elements' ) ) then
            level = 1
          end if
        else if ( level .eq. 1 ) then
          call s_to_i4 ( text, element_num, ierror, length )
          level = 2
        else if ( level .eq. 2 ) then
          if ( s_begin ( text(1:12), '$EndElements' ) ) then
            go to 50
          else
            k = 0
            do
              call s_to_i4 ( text, indx, ierror, length )
              text = text(length+1:)
              if ( ierror .ne. 0 ) then
                go to 40
              end if
              k = k + 1
            end do
40          continue
            element_order = k - 5
            go to 50
          end if
        end if

      go to 30

50    continue

      close ( unit = input )

      return
      end
      subroutine gmsh_mesh1d_write ( gmsh_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc GMSH_MESH1D_WRITE writes 1D mesh data as a Gmsh mesh file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Christophe Geuzaine, Jean-Francois Remacle,
c    Gmsh: a three-dimensional finite element mesh generator with
c    built-in pre- and post-processing facilities,
c    International Journal for Numerical Methods in Engineering,
c    Volume 79, Number 11, pages 1309-1331, 2009.
c
c  Parameters:
c
c    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_X(M,NODE_NUM), the node coordinates.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
c    the nodes that make up each element.
c
      implicit none

      integer element_num
      integer element_order
      integer m
      integer node_num

      integer element
      integer element_node(element_order,element_num)
      integer element_type
      character * ( * ) gmsh_filename
      integer gmsh_unit
      integer node
      double precision node_x(m,node_num)
      integer tag_num
      integer tag1
c
c  Enforce 1-based indexing of nodes.
c
      call mesh_base_one ( node_num, element_order, element_num, 
     &  element_node )
c
c  Open the file.
c
      call get_unit ( gmsh_unit )

      open ( unit = gmsh_unit, file = gmsh_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( gmsh_unit, '(a)' ) '$MeshFormat'
      write ( gmsh_unit, '(a)' ) '2.2 0 8'
      write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

      write ( gmsh_unit, '(a)' ) '$Nodes'
      write ( gmsh_unit, '(i6)' ) node_num
      do node = 1, node_num
        write ( gmsh_unit, '(i6,2x,g14.6,a)' ) 
     &    node, node_x(1:m,node), '  0.0  0.0'
      end do
      write ( gmsh_unit, '(a)' ) '$EndNodes'

      element_type = 1

      tag_num = 2
      tag1 = 0
      write ( gmsh_unit, '(a)' ) '$Elements'
      write ( gmsh_unit, '(i6)' ) element_num
      do element = 1, element_num
        write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,2(2x,i6))' ) 
     &    element, element_type, tag_num, tag1, element, 
     &    element_node(1:element_order,element)
      end do
      write ( gmsh_unit, '(a)' ) '$EndElements'

      close ( unit = gmsh_unit )

      return
      end
      subroutine gmsh_mesh2d_element_data_example ( element_num, 
     &  element_order, element_node )

c*********************************************************************72
c
cc GMSH_MESH2D_ELEMENT_DATA_EXAMPLE returns element data for the 2D example.
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
c  Parameters:
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
c    the indices of the nodes that make up each element.
c
      implicit none

      integer element_num
      integer element_order

      integer element_node(element_order,element_num)
      integer element_node_save(3,24)

      save element_node_save

      data element_node_save /
     &  1,  2,  6, 
     &  7,  6,  2, 
     &  2,  3,  7, 
     &  8,  7,  3, 
     &  3,  4,  8, 
     &  9,  8,  4, 
     &  4,  5,  9, 
     & 10,  9,  5, 
     &  6,  7, 11, 
     & 12, 11,  7, 
     &  7,  8, 12, 
     & 13, 12,  8, 
     &  8,  9, 13, 
     & 14, 13,  9, 
     &  9, 10, 14, 
     & 15, 14, 10,
     & 11, 12, 16,
     & 17, 16, 12, 
     & 12, 13, 17, 
     & 18, 17, 13, 
     & 16, 17, 19, 
     & 20, 19, 17, 
     & 17, 18, 20, 
     & 21, 20, 18 /

      call i4mat_copy ( element_order, element_num, element_node_save,  
     &  element_node )

      return
      end
      subroutine gmsh_mesh2d_element_size_example ( element_num, 
     &  element_order )

c*********************************************************************72
c
cc GMSH_MESH2D_ELEMENT_SIZE_EXAMPLE returns element sizes for the 2D example.
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
c  Parameters:
c
c    Output, integer ELEMENT_NUM, the number of elements.
c
c    Output, integer ELEMENT_ORDER, the order of the elements.
c
      implicit none

      integer element_num
      integer element_order

      element_num = 24
      element_order = 3

      return
      end
      subroutine gmsh_mesh2d_node_data_example ( node_num, node_dim, 
     &  node_x )

c*********************************************************************72
c
cc GMSH_MESH2D_NODE_DATA_EXAMPLE returns node data for the 2D example.
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
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer NODE_DIM, the spatial dimension.
c
c    Output, double precision NODE_X(NODE_DIM,NODE_NUM), the nodal
c    coordinates.
c
      implicit none

      integer node_dim
      integer node_num

      double precision node_x(node_dim,node_num)
      double precision node_x_save(2,21)

      save node_x_save

      data node_x_save /
     & 0.0, 0.0, 
     & 1.0, 0.0, 
     & 2.0, 0.0, 
     & 3.0, 0.0, 
     & 4.0, 0.0, 
     & 0.0, 1.0, 
     & 1.0, 1.0, 
     & 2.0, 1.0, 
     & 3.0, 1.0, 
     & 4.0, 1.0, 
     & 0.0, 2.0, 
     & 1.0, 2.0, 
     & 2.0, 2.0, 
     & 3.0, 2.0, 
     & 4.0, 2.0, 
     & 0.0, 3.0, 
     & 1.0, 3.0, 
     & 2.0, 3.0, 
     & 0.0, 4.0, 
     & 1.0, 4.0, 
     & 2.0, 4.0 /

      call r8mat_copy ( node_dim, node_num, node_x_save, node_x )

      return
      end
      subroutine gmsh_mesh2d_node_size_example ( node_num, node_dim )

c*********************************************************************72
c
cc GMSH_MESH2D_NODE_SIZE_EXAMPLE returns node sizes for the 2D example.
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
c  Parameters:
c
c    Output, integer NODE_NUM, the number of nodes.
c
c    Output, integer NODE_DIM, the spatial dimension.
c
      implicit none

      integer node_dim
      integer node_num

      node_num = 21
      node_dim = 2

      return
      end
      subroutine gmsh_mesh2d_write ( gmsh_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc GMSH_MESH2D_WRITE writes 2D mesh data as a Gmsh mesh file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Christophe Geuzaine, Jean-Francois Remacle,
c    Gmsh: a three-dimensional finite element mesh generator with
c    built-in pre- and post-processing facilities,
c    International Journal for Numerical Methods in Engineering,
c    Volume 79, Number 11, pages 1309-1331, 2009.
c
c  Parameters:
c
c    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_X(M,NODE_NUM), the node coordinates.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
c    the nodes that make up each element.
c
      implicit none

      integer element_num
      integer element_order
      integer m
      integer node_num

      integer element
      integer element_node(element_order,element_num)
      integer element_type
      character * ( * ) gmsh_filename
      integer gmsh_unit
      integer node
      double precision node_x(m,node_num)
      integer tag_num
      integer tag1
c
c  Enforce 1-based indexing of nodes.
c
      call mesh_base_one ( node_num, element_order, element_num, 
     &  element_node )
c
c  Open the file.
c
      call get_unit ( gmsh_unit )

      open ( unit = gmsh_unit, file = gmsh_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( gmsh_unit, '(a)' ) '$MeshFormat'
      write ( gmsh_unit, '(a)' ) '2.2 0 8'
      write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

      write ( gmsh_unit, '(a)' ) '$Nodes'
      write ( gmsh_unit, '(i6)' ) node_num
      do node = 1, node_num
        write ( gmsh_unit, '(i6,2x,g14.6,2x,g14.6,a)' ) 
     &    node, node_x(1:m,node), '  0.0'
      end do
      write ( gmsh_unit, '(a)' ) '$EndNodes'

      if ( element_order .eq. 3 ) then
        element_type = 2
      else if ( element_order .eq. 6 ) then
        element_type = 9
      end if

      tag_num = 2
      tag1 = 0
      write ( gmsh_unit, '(a)' ) '$Elements'
      write ( gmsh_unit, '(i6)' ) element_num
      do element = 1, element_num
        write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,6(2x,i6))' ) 
     &    element, element_type, tag_num, tag1, element, 
     &    element_node(1:element_order,element)
      end do
      write ( gmsh_unit, '(a)' ) '$EndElements'

      close ( unit = gmsh_unit )

      return
      end
      subroutine gmsh_mesh3d_write ( gmsh_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc GMSH_MESH3D_WRITE writes 3D mesh data as a Gmsh mesh file.
c
c  Discussion:
c
c    The node ordering for the 20 node element is not standard.
c
c    Assuming the vertices are A, B, C and D, Gmsh uses the following ordering:
c
c    1:    a
c    2:        b
c    3:            c
c    4:                d
c    5: (2*a  +b        )/3
c    6: (  a+2*b        )/3
c    7: (    2*b+  c    )/3
c    8: (      b+2*c    )/3
c    9: (  a    +2*c    )/3
c   10: (2*a    +  c    )/3
c   11: (2*a        +  d)/3
c   12: (  a        +2*d)/3
c   13: (     b     +2*d)/3
c   14: (   2*b     +  d)/3
c   15: (       +  c+2*d)/3
c   16: (       +2*c+  d)/3
c   17: (  a+  b+  c    )/3
c   18: (  a+  b    +  d)/3
c   19: (      b+  c+  d)/3
c   20: (  a+      c+  d)/3
c
c    Leo Rebholz used the following ordering:
c
c    1:    a
c    2:        b
c    3:            c
c    4:                d
c    5: (2*a  +b        )/3
c    6: (2*a    +  c    )/3
c    7: (  a+2*b        )/3
c    8: (  a    +2*c    )/3
c    9: (  a+  b+  c    )/3
c   10: (    2*b+  c    )/3
c   11: (      b+2*c    )/3
c   12: (2*a        +  d)/3
c   13: (   2*b     +  d)/3
c   14: (       +2*c+  d)/3
c   15: (  a+  b    +  d)/3
c   16: (      b+  c+  d)/3
c   17: (  a+      c+  d)/3
c   18: (  a        +2*d)/3
c   19: (     b     +2*d)/3
c   20: (       +  c+2*d)/3
c
c    Since the only 20 node data we have is from Leo, we will assume that
c    all 20 node input data is in Leo's format, and needs to be converted
c    to the Gmsh convention.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Christophe Geuzaine, Jean-Francois Remacle,
c    Gmsh: a three-dimensional finite element mesh generator with
c    built-in pre- and post-processing facilities,
c    International Journal for Numerical Methods in Engineering,
c    Volume 79, Number 11, pages 1309-1331, 2009.
c
c  Parameters:
c
c    Input, character * ( * ) GMSH_FILENAME, the name of the Gmsh file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_X(M,NODE_NUM), the node coordinates.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), 
c    the nodes that make up each element.
c
      implicit none

      integer element_num
      integer element_order
      integer m
      integer node_num

      integer element
      integer element_node(element_order,element_num)
      integer element_type
      character * ( * ) gmsh_filename
      integer gmsh_unit
      integer leo_to_gmsh(20)
      integer node
      double precision node_x(m,node_num)
      integer tag_num
      integer tag1

      save leo_to_gmsh

      data leo_to_gmsh /
     &   1,  2,  3,  4,  5, 
     &   7, 10, 11,  8,  6, 
     &  12, 18, 19, 13, 20, 
     &  14,  9, 15, 16, 17 /
c
c  Enforce 1-based indexing of nodes.
c
      call mesh_base_one ( node_num, element_order, element_num, 
     &  element_node )
c
c  Open the file.
c
      call get_unit ( gmsh_unit )

      open ( unit = gmsh_unit, file = gmsh_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( gmsh_unit, '(a)' ) '$MeshFormat'
      write ( gmsh_unit, '(a)' ) '2.2 0 8'
      write ( gmsh_unit, '(a)' ) '$EndMeshFormat'

      write ( gmsh_unit, '(a)' ) '$Nodes'
      write ( gmsh_unit, '(i6)' ) node_num
      do node = 1, node_num
        write ( gmsh_unit, '(i6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    node, node_x(1:3,node)
      end do
      write ( gmsh_unit, '(a)' ) '$EndNodes'

      if ( element_order .eq. 4 ) then
        element_type = 4
      else if ( element_order .eq. 10 ) then
        element_type = 11
      else if ( element_order .eq. 20 ) then
        element_type = 29
      end if

      tag_num = 2
      tag1 = 0
      write ( gmsh_unit, '(a)' ) '$Elements'
      write ( gmsh_unit, '(i6)' ) element_num
      do element = 1, element_num
        if ( element_order .eq. 20 ) then
          write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,6(2x,i6))' )
     &      element, element_type, tag_num, tag1, element, 
     &      element_node(leo_to_gmsh(1:element_order),element)
        else
          write ( gmsh_unit, '(i6,2x,i2,2x,i2,2x,i2,2x,i6,6(2x,i6))' )
     &      element, element_type, tag_num, tag1, element, 
     &      element_node(1:element_order,element)
        end if
      end do
      write ( gmsh_unit, '(a)' ) '$EndElements'

      close ( unit = gmsh_unit )

      return
      end
      subroutine i4mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc I4MAT_COPY copies an I4MAT.
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
c    04 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer A1(M,N), the matrix to copy.
c
c    Output, integer A2(M,N), the copy.
c
      implicit none

      integer m
      integer n

      integer a1(m,n)
      integer a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine i4mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
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
c    39 October 2007
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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      character * ( * ) title

      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
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
c    30 October 2007
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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*8 ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer  j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )  title

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8)' ) i
        end do

        write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc

            i = i2lo - 1 + i2

            write ( ctemp(i2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine mesh_base_one ( node_num, element_order, element_num, 
     &  element_node )

c*********************************************************************72
c
cc MESH_BASE_ONE ensures that the element definition is one-based.
c
c  Discussion:
c
c    The ELEMENT_NODE array contains nodes indices that form elements.
c    The convention for node indexing might start at 0 or at 1.
c    Since a FORTRAN90 program will naturally assume a 1-based indexing, it is
c    necessary to check a given element definition and, if it is actually
c    0-based, to convert it.
c
c    This function attempts to detect 0-based node indexing and correct it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, int NODE_NUM, the number of nodes.
c
c    Input, int ELEMENT_ORDER, the order of the elements.
c
c    Input, int ELEMENT_NUM, the number of elements.
c
c    Input/output, int ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), the element
c    definitions.
c
      implicit none

      integer element_num
      integer element_order

      integer element
      integer element_node(element_order,element_num)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_max
      integer i4mat_min
      integer j
      integer node
      integer node_max
      integer node_min
      integer node_num
      integer order

      node_min = + i4_huge
      node_max = - i4_huge
      do j = 1, element_num
        do i = 1, element_order
          node_min = min ( node_min, element_node(i,j) )
          node_max = max ( node_max, element_node(i,j) )
        end do
      end do

      if ( node_min .eq. 0 .and. node_max .eq. node_num - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE:'
        write ( *, '(a)' )
     &    '  The element indexing appears to be 0-based!'
        write ( *, '(a)' ) '  This will be converted to 1-based.'
        do j = 1, element_num
          do i = 1, element_order
            element_node(i,j) = element_node(i,j) + 1
          end do
        end do
      else if ( node_min .eq. 1 .and. node_max .eq. node_num ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE:'
        write ( *, '(a)' ) 
     &    '  The element indexing appears to be 1-based!'
        write ( *, '(a)' ) '  No conversion is necessary.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE - Warning!'
        write ( *, '(a)' ) 
     &    '  The element indexing is not of a recognized type.'
        write ( *, '(a,i8)' ) '  NODE_MIN = ', node_min
        write ( *, '(a,i8)' ) '  NODE_MAX = ', node_max
        write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
      end if

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
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
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
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
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title
      integer title_len

      title_len = len_trim ( title )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title(1:title_len)

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,5a14)' ) j, ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      function s_begin ( s1, s2 )

c*********************************************************************72
c
cc S_BEGIN is TRUE if one string matches the beginning of the other.
c
c  Discussion:
c
c    The strings are compared, ignoring blanks, spaces and capitalization.
c
c  Example:
c
c     S1              S2      S_BEGIN
c
c    'Bob'          'BOB'     TRUE
c    '  B  o b '    ' bo b'   TRUE
c    'Bob'          'Bobby'   TRUE
c    'Bobo'         'Bobb'    FALSE
c    ' '            'Bob'     FALSE    (Do not allow a blank to match
c                                       anything but another blank string.)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 November 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( ) S1, S2, the strings to be compared.
c
c    Output, logical S_BEGIN, is TRUE if the strings match up to
c    the end of the shorter string, ignoring case.
c
      implicit none

      logical ch_eqi
      integer i1
      integer i2
      logical s_begin
      character * ( * )  s1
      integer s1_length
      character * ( * )  s2
      integer s2_length

      s1_length = len_trim ( s1 )
      s2_length = len_trim ( s2 )
c
c  If either string is blank, then both must be blank to match.
c  Otherwise, a blank string matches anything, which is not
c  what most people want.
c
      if ( s1_length .eq. 0 .or. s2_length .eq. 0 ) then

        if ( s1_length .eq. 0 .and. s2_length .eq. 0 ) then
          s_begin = .true.
        else
          s_begin = .false.
        end if

        return

      end if

      i1 = 0
      i2 = 0
c
c  Find the next nonblank in S1.
c
10    continue

20      continue

          i1 = i1 + 1

          if ( s1_length .lt. i1 ) then
            s_begin = .true.
            return
          end if

          if ( s1(i1:i1) .ne. ' ' ) then
            go to 30
          end if

        go to 20

30      continue
c
c  Find the next nonblank in S2.
c
40      continue

          i2 = i2 + 1

          if ( s2_length .lt. i2 ) then
            s_begin = .true.
            return
          end if

          if ( s2(i2:i2) .ne. ' ' ) then
            go to 50
          end if

        go to 40

50      continue
c
c  If the characters match, get the next pair.
c
        if ( .not. ch_eqi ( s1(i1:i1), s2(i2:i2) ) ) then
          go to 60
        end if

      go to 10

60    continue

      s_begin = .false.

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
