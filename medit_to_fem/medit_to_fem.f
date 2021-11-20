      program main

c*****************************************************************************80
c
cc MAIN is the main program for MEDIT_TO_FEM.
c
c  Discussion:
c
c    TRIANGLE_TO_FEM converts mesh data from MEDIT to FEM format.
c
c  Usage:
c
c    medit_to_fem prefix
c
c    where 'prefix' is the common filename prefix:
c
c    * 'prefix'.mesh is the MEDIT filename.
c    * 'prefix'_nodes.txt will contain the FEM node coordinates.
c    * 'prefix'_elements.txt will contain the FEM element node connectivity.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer arg_num
      integer dim
      integer, allocatable :: edge_label(:)
      integer, allocatable :: edge_vertex(:,:)
      integer edges
      integer element_num
      integer element_order
      character * ( 255 ) fem_element_filename
      character * ( 255 ) fem_node_filename
      integer, allocatable :: hexahedron_label(:)
      integer, allocatable :: hexahedron_vertex(:,:)
      integer hexahedrons
      integer iarg
      integer iargc
      integer m
      character * ( 255 ) medit_filename
      integer node_num;
      character * ( 255 ) prefix
      integer, allocatable :: quadrilateral_label(:)
      integer, allocatable :: quadrilateral_vertex(:,:)
      integer quadrilaterals
      integer, allocatable :: tetrahedron_label(:)
      integer, allocatable :: tetrahedron_vertex(:,:)
      integer tetrahedrons
      integer, allocatable :: triangle_label(:)
      integer, allocatable :: triangle_vertex(:,:)
      integer triangles
      double precision, allocatable :: vertex_coordinate(:,:)
      integer, allocatable :: vertex_label(:)
      integer vertices

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MEDIT_TO_FEM'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Read a MESH file created by MEDIT:'
      write ( *, '(a)' ) '  * "prefix".mesh.'
      write ( *, '(a)' ) 
     &  '  Write two simple FEM files listing nodes and elements.'
      write ( *, '(a)' ) '  * "prefix"_nodes.txt, node coordinates.'
      write ( *, '(a)' ) 
     &  '  * "prefix"_elements.txt, element connectivity.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the filename prefix.
c
      if ( arg_num .lt. 1 ) then

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  Please enter the filename prefix.'

        read ( *, '(a)' ) prefix

      else

        iarg = 1
        call getarg ( iarg, prefix )

      end if
c
c  Create the filenames.
c
      medit_filename = trim ( prefix ) // '.mesh'
      fem_node_filename = trim ( prefix ) // '_nodes.txt'
      fem_element_filename = trim ( prefix ) // '_elements.txt'
c
c  Read MEDIT sizes.
c
      call mesh_size_read ( medit_filename, dim, vertices, edges, 
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons )
c
c  Report sizes.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Size information from MEDIT file:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of dimensions = ', dim
      write ( *, '(a,i8)' ) '  Number of vertices = ', vertices
      write ( *, '(a,i8)' ) '  Number of edges = ', edges
      write ( *, '(a,i8)' ) '  Number of triangles = ', triangles
      write ( *, '(a,i8)' ) 
     &  '  Number of quadrilaterals = ', quadrilaterals
      write ( *, '(a,i8)' ) '  Number of tetrahedrons = ', tetrahedrons
      write ( *, '(a,i8)' ) '  Number of hexahedrons = ', hexahedrons

      call main_sub ( medit_filename, fem_node_filename, 
     &  fem_element_filename, dim, vertices, edges, 
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MEDIT_TO_FEM:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine main_sub ( medit_filename, fem_node_filename, 
     &  fem_element_filename, dim, vertices, edges, 
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons )

c*********************************************************************72
c
cc MAIN_SUB is a helper routine for MAIN.
c
c  Discussion:
c
c    Since FORTRAN77 can't officially allocate arrays, we try to do so
c    by calling a subroutine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2014
c
c  Author:
c
c    John Burkardt
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
      integer element_num
      integer element_order
      character * ( 255 ) fem_element_filename
      character * ( 255 ) fem_node_filename
      integer hexahedron_label(hexahedrons)
      integer hexahedron_vertex(8,hexahedrons)
      integer m
      character * ( 255 ) medit_filename
      integer node_num
      integer quadrilateral_label(quadrilaterals)
      integer quadrilateral_vertex(4,quadrilaterals)
      integer tetrahedron_label(tetrahedrons)
      integer tetrahedron_vertex(4,tetrahedrons)
      integer triangle_label(triangles)
      integer triangle_vertex(3,triangles)
      double precision vertex_coordinate(dim,vertices)
      integer vertex_label(vertices)
c
c  Read MEDIT data.
c
      call mesh_data_read ( medit_filename, dim, vertices, edges, 
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons, 
     &  vertex_coordinate, vertex_label, edge_vertex, edge_label, 
     &  triangle_vertex, triangle_label, quadrilateral_vertex, 
     &  quadrilateral_label, tetrahedron_vertex, tetrahedron_label, 
     &  hexahedron_vertex, hexahedron_label )
c
c  Choose the FEM data.
c
c  We need to assume that there is only one element type.
c  If there are elements of multiple dimension, take the highest.
c
      m = dim
      node_num = vertices
      call r8mat_write ( fem_node_filename, dim, vertices, 
     &  vertex_coordinate )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Created node coordinate file "' //
     &  trim ( fem_node_filename ) // '"'

      if ( 0 .lt. hexahedrons .and. dim .eq. 3 ) then

        element_order = 8
        element_num = hexahedrons
        call i4mat_write ( fem_element_filename, element_order, 
     &    element_num, hexahedron_vertex )

      else if ( 0 .lt. tetrahedrons .and. dim .eq. 3 ) then

        element_order = 4
        element_num = tetrahedrons
        call i4mat_write ( fem_element_filename, element_order, 
     &    element_num, tetrahedron_vertex )

      else if ( 0 .lt. quadrilaterals .and. dim .eq. 2 ) then

        element_order = 4
        element_num = quadrilaterals
        call i4mat_write ( fem_element_filename, element_order, 
     &    element_num, quadrilateral_vertex )

      else if ( 0 .lt. triangles .and. dim .eq. 2 ) then

        element_order = 3
        element_num = triangles
        call i4mat_write ( fem_element_filename, element_order, 
     &    element_num, triangle_vertex )

      else

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MEDIT_TO_FEM - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Unexpected combination of spatial dimension'
        write ( *, '(a)' ) '  and number of nonzero objects.'
        stop 1

      end if

      write ( *, '(a)' ) '  Created element connectivity file "' //     
     &  trim ( fem_element_filename ) // '"'

      return
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
      subroutine i4mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc I4MAT_WRITE writes an I4MAT file.
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
c    31 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, integer TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      integer table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a4)' ) '(', m, 'i10)'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine i4mat_zero ( m, n, a )

c*********************************************************************72
c
cc I4MAT_ZERO zeroes out an I4MAT.
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
c    22 November 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the row and column dimensions of the matrix.
c
c    Output, integer A(M,N), a matrix of zeroes.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a(i,j) = 0
        end do
      end do

      return
      end
      subroutine i4vec_zero ( n, a )

c*********************************************************************72
c
cc I4VEC_ZERO sets the entries of an I4VEC to 0.
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
c    18 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Output, integer A(N), the vector, which has been set to zero.
c
      implicit none

      integer n

      integer a(n)
      integer i

      do i = 1, n
        a(i) = 0
      end do

      return
      end
      subroutine mesh_data_read ( filename, dim, vertices, edges,
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons,
     &  vertex_coordinate, vertex_label, edge_vertex, edge_label,
     &  triangle_vertex, triangle_label, quadrilateral_vertex,
     &  quadrilateral_label, tetrahedron_vertex, tetrahedron_label,
     &  hexahedron_vertex, hexahedron_label )

c*********************************************************************72
c
cc MESH_DATA_READ reads data from a MESH file.
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
c    Input, character * ( * ) FILENAME, the name of the MESH file.
c
c    Input, integer DIM, the spatial dimension, which should be 2 or 3.
c
c    Input, integer VERTICES, the number of vertices.
c
c    Input, integer EDGES, the number of edges (may be 0).
c
c    Input, integer TRIANGLES, the number of triangles (may be 0).
c
c    Input, integer QUADRILATERALS, the number of quadrilaterals
c    (may be 0).
c
c    Input, integer TETRAHEDRONS, the number of tetrahedrons
c    (may be 0).
c
c    Input, integer HEXAHEDRONS, the number of hexahedrons
c    (may be 0).
c
c    Output, double precision VERTEX_COORDINATE(DIM,VERTICES), the coordinates
c    of each vertex.
c
c    Output, integer VERTEX_LABEL(VERTICES), a label for
c    each vertex.
c
c    Output, integer EDGE_VERTEX(2,EDGES), the vertices that form
c    each edge.
c
c    Output, integer EDGE_LABEL(EDGES), a label for each edge.
c
c    Output, integer TRIANGLE_VERTEX(3,TRIANGLES), the vertices
c    that form each triangle.
c
c    Output, integer TRIANGLE_LABEL(TRIANGLES), a label for each
c    triangle.
c
c    Output, integer QUADRILATERAL_VERTEX(4,QUADRILATERALS), the
c    vertices that form each quadrilateral.
c
c    Output, integer QUADRILATERAL_LABEL(QUADRILATERALS), a label
c    for each quadrilateral.
c
c    Output, integer TETRAHEDRON_VERTEX(4,TETRAHEDRONS), the
c    vertices that form each tetrahedron.
c
c    Output, integer TETRAHEDRON_LABEL(TETRAHEDRONS), a label for
c    each tetrahedron.
c
c    Output, integer HEXAHEDRON_VERTEX(8,HEXAHEDRONS), the vertices
c    that form each hexahedron.
c
c    Output, integer HEXAHEDRON_LABEL(HEXAHEDRONS), a label for
c    each hexahedron.
c
      implicit none

      integer edges
      integer hexahedrons
      integer quadrilaterals
      integer tetrahedrons
      integer triangles
      integer vertices

      integer dim
      integer edge
      integer edge_label(edges)
      integer edge_vertex(2,edges)
      character * ( * ) filename
      integer fileunit
      integer hexahedron
      integer hexahedron_label(hexahedrons)
      integer hexahedron_vertex(8,hexahedrons)
      integer i
      integer i4vec(9)
      integer ierror
      integer ios
      integer j
      character * ( 80 ) keyword
      integer length
      integer line_num
      integer quadrilateral
      integer quadrilateral_label(quadrilaterals)
      integer quadrilateral_vertex(4,quadrilaterals)
      double precision r8vec(9)
      logical s_begin
      logical s_eqi
      integer s_len_trim
      integer tetrahedron
      integer tetrahedron_label(tetrahedrons)
      integer tetrahedron_vertex(4,tetrahedrons)
      character * ( 255 ) text
      integer triangle
      integer triangle_label(triangles)
      integer triangle_vertex(3,triangles)
      integer vertex
      double precision vertex_coordinate(dim,vertices)
      integer vertex_label(vertices)
c
c  Initialize everything to nothing.
c
      call r8mat_zero ( dim, vertices, vertex_coordinate )
      call i4vec_zero ( vertices, vertex_label )
      call i4mat_zero ( 2, edges, edge_vertex )
      call i4vec_zero ( edges, edge_label )
      call i4mat_zero ( 3, triangles, triangle_vertex )
      call i4vec_zero ( triangles, triangle_label )
      call i4mat_zero ( 4, quadrilaterals, quadrilateral_vertex )
      call i4vec_zero ( quadrilaterals, quadrilateral_label )
      call i4mat_zero ( 4, tetrahedrons, tetrahedron_vertex )
      call i4vec_zero ( tetrahedrons, tetrahedron_label )
      call i4mat_zero ( 8, hexahedrons, hexahedron_vertex )
      call i4vec_zero ( hexahedrons, hexahedron_label )
c
c  Open the file.
c
      call get_unit ( fileunit )

      open ( unit = fileunit, file = filename, status = 'old',
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_DATA_READ - Fatal errorc'
        write ( *, '(a)' ) '  Could not open file.'
        stop
      end if
c
c  Read lines til you get alphanumerics and determine a "mode"
c
      line_num = 0
      keyword = 'NONE'

10    continue

        read ( fileunit, '(a)', iostat = ios ) text

        if ( ios .ne. 0 ) then
          go to 20
        end if

        line_num = line_num + 1

        if ( s_len_trim ( text ) .eq. 0 ) then
          keyword = 'NONE'
          go to 10
        end if

        if ( text(1:1) .eq. '#' ) then
          go to 10
        end if
c
c  Remove initial blanks.
c
        call s_adjustl ( text )
c
c  Expecting a keyword.
c
            if ( s_eqi ( text, 'CORNERS' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'DIMENSION' ) ) then

          keyword = 'DIMENSION'

        else if ( s_eqi ( text, 'EDGES' ) ) then

          keyword = 'EDGES'

        else if ( s_eqi ( text, 'END' ) ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  END statement encountered.'
          go to 20

        else if ( s_eqi ( text, 'HEXAHEDRA' ) .or.
     &            s_eqi ( text, 'HEXAHEDRONS' ) ) then

          keyword = 'HEXAHEDRONS'

        else if ( s_begin ( text, 'MESHVERSIONFORMATTED' ) ) then

        else if ( s_eqi ( text, 'NORMALATQUADRILATERALVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'NORMALATTRIANGLEVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'NORMALATVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'NORMALS' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'QUADRILATERALS' ) ) then

          keyword = 'QUADRILATERALS'

        else if ( s_eqi ( text, 'REQUIREDEDGES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'REQUIREDVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'RIDGES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'TANGENTATEDGES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'TANGENTS' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'TETRAHEDRA' ) .or.
     &            s_eqi ( text, 'TETRAHEDRONS' ) ) then

          keyword = 'TETRAHEDRONS'

        else if ( s_eqi ( text, 'TRIANGLES' ) ) then

          keyword = 'TRIANGLES'

        else if ( s_eqi ( text, 'VERTICES' ) ) then

          keyword = 'VERTICES'
c
c  Presumably, numeric data to be processed by keyword.
c
        else if ( s_eqi ( keyword, 'DIMENSION' ) ) then

          call s_to_i4 ( text, dim, ierror, length )

          keyword = 'NONE'

        else if ( s_eqi ( keyword, 'EDGES' ) ) then

          call s_to_i4 ( text, edges, ierror, length )

          keyword = 'EDGE_VERTEX'
          edge = 0

        else if ( s_eqi ( keyword, 'EDGE_VERTEX' ) ) then

          call s_to_i4vec ( text, 3, i4vec, ierror )
          edge = edge + 1
          do i = 1, 2
            edge_vertex(i,edge) = i4vec(i)
          end do
          edge_label(edge) = i4vec(3)

        else if ( s_eqi ( keyword, 'HEXAHEDRONS' ) ) then

          call s_to_i4 ( text, hexahedrons, ierror, length )

          keyword = 'HEXAHEDRON_VERTEX'
          hexahedron = 0

        else if ( s_eqi ( keyword, 'HEXAHEDRON_VERTEX' ) ) then

          call s_to_i4vec ( text, 9, i4vec, ierror )
          hexahedron = hexahedron + 1
          do i = 1, 8
            hexahedron_vertex(i,hexahedron) = i4vec(i)
          end do
          hexahedron_label(hexahedron) = i4vec(9)

        else if ( s_eqi ( keyword, 'QUADRILATERALS' ) ) then

          call s_to_i4 ( text, quadrilaterals, ierror, length )

          keyword = 'QUADRILATERAL_VERTEX'
          quadrilateral = 0

        else if ( s_eqi ( keyword, 'QUADRILATERAL_VERTEX' ) ) then

          call s_to_i4vec ( text, 5, i4vec, ierror )
          quadrilateral = quadrilateral + 1
          do i = 1, 4
            quadrilateral_vertex(i,quadrilateral) = i4vec(i)
          end do
          quadrilateral_label(quadrilateral) = i4vec(5)

        else if ( s_eqi ( keyword, 'TETRAHEDRONS' ) ) then

          call s_to_i4 ( text, tetrahedrons, ierror, length )

          keyword = 'TETRAHEDRON_VERTEX'
          tetrahedron = 0

        else if ( s_eqi ( keyword, 'TETRAHEDRON_VERTEX' ) ) then

          call s_to_i4vec ( text, 5, i4vec, ierror )
          tetrahedron = tetrahedron + 1
          do i = 1, 4
            tetrahedron_vertex(i,tetrahedron) = i4vec(i)
          end do
          tetrahedron_label(tetrahedron) = i4vec(5)

        else if ( s_eqi ( keyword, 'TRIANGLES' ) ) then

          call s_to_i4 ( text, triangles, ierror, length )

          keyword = 'TRIANGLE_VERTEX'
          triangle = 0

        else if ( s_eqi ( keyword, 'TRIANGLE_VERTEX' ) ) then

          call s_to_i4vec ( text, 4, i4vec, ierror )
          triangle = triangle + 1
          do i = 1, 3
            triangle_vertex(i,triangle) = i4vec(i)
          end do
          triangle_label(triangle) = i4vec(4)

        else if ( s_eqi ( keyword, 'VERTICES' ) ) then

          call s_to_i4 ( text, vertices, ierror, length )

          keyword = 'VERTEX_COORDINATE'
          vertex = 0

        else if ( s_eqi ( keyword, 'VERTEX_COORDINATE' ) ) then

          call s_to_r8vec ( text, dim + 1, r8vec, ierror )
          vertex = vertex + 1
          do i = 1, dim
            vertex_coordinate(i,vertex) = r8vec(i)
          end do
          vertex_label(vertex) = int ( r8vec(dim + 1) )

        else if ( s_eqi ( keyword, 'SKIP' ) ) then

        else

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MESH_DATA_READ - Fatal error!'
          write ( *, '(a,i8)' )
     &      '  Could not find keyword while reading line ', line_num
          write ( *, '(a)' ) '"' // trim ( text ) // '".'
          stop

        end if

      go to 10

20    continue
c
c  Close the file.
c
      close ( unit = fileunit )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) '  Read ', line_num,
     &  ' lines from "' // trim ( filename ) // '".'

      return
      end
      subroutine mesh_size_read ( filename, dim, vertices, edges,
     &  triangles, quadrilaterals, tetrahedrons, hexahedrons )

c*********************************************************************72
c
cc MESH_SIZE_READ reads sizes from a MESH file.
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
c    Input, character * ( * ) FILENAME, the name of the MESH file.
c
c    Output, integer DIM, the spatial dimension, which should be 2 or 3.
c
c    Output, integer VERTICES, the number of vertices.
c
c    Output, integer EDGES, the number of edges (may be 0).
c
c    Output, integer TRIANGLES, the number of triangles (may be 0).
c
c    Output, integer QUADRILATERALS, the number of quadrilaterals
c    (may be 0).
c
c    Output, integer TETRAHEDRONS, the number of tetrahedrons
c    (may be 0).
c
c    Output, integer HEXAHEDRONS, the number of hexahedrons
c    (may be 0).
c
      implicit none

      integer dim
      integer edges
      character * ( * ) filename
      integer fileunit
      integer hexahedrons
      integer ierror
      integer ios
      character * ( 80 ) keyword
      integer length
      integer line_num
      integer quadrilaterals
      logical s_begin
      logical s_eqi
      integer s_len_trim
      integer tetrahedrons
      character * ( 255 ) text
      integer triangles
      integer vertices
c
c  Initialize everything to nothing.
c
      dim = 0
      vertices = 0
      edges = 0
      triangles = 0
      quadrilaterals = 0
      tetrahedrons = 0
      hexahedrons = 0
c
c  Open the file.
c
      call get_unit ( fileunit )

      open ( unit = fileunit, file = filename, status = 'old',
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_SIZE_READ - Fatal error!'
        write ( *, '(a)' ) '  Could not open file.'
        stop
      end if
c
c  Read lines til you get alphanumerics and determine a "mode"
c
      line_num = 0
      keyword = 'NONE'

10    continue

        read ( fileunit, '(a)', iostat = ios ) text

        if ( ios .ne. 0 ) then
          go to 20
        end if

        line_num = line_num + 1

        if ( s_len_trim ( text ) .eq. 0 ) then
          keyword = 'NONE'
          go to 10
        end if

        if ( text(1:1) .eq. '#' ) then
          go to 10
        end if
c
c  Remove initial blanks.
c
        call s_adjustl ( text )
c
c  Expecting a keyword.
c
            if ( s_eqi ( text, 'CORNERS' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'DIMENSION' ) ) then

          keyword = 'DIMENSION'

        else if ( s_eqi ( text, 'EDGES' ) ) then

          keyword = 'EDGES'

        else if ( s_eqi ( text, 'END' ) ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  END statement encountered.'
          go to 20

        else if ( s_eqi ( text, 'HEXAHEDRA' ) .or.
     &            s_eqi ( text, 'HEXAHEDRONS' ) ) then

          keyword = 'HEXAHEDRONS'

        else if ( s_begin ( text, 'MESHVERSIONFORMATTED' ) ) then

        else if ( s_eqi ( text, 'NORMALATQUADRILATERALVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'NORMALATTRIANGLEVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'NORMALATVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'NORMALS' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'QUADRILATERALS' ) ) then

          keyword = 'QUADRILATERALS'

        else if ( s_eqi ( text, 'REQUIREDEDGES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'REQUIREDVERTICES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'RIDGES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'TANGENTATEDGES' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'TANGENTS' ) ) then

          keyword = 'SKIP'

        else if ( s_eqi ( text, 'TETRAHEDRA' ) .or.
     &            s_eqi ( text, 'TETRAHEDRONS' ) ) then

          keyword = 'TETRAHEDRONS'

        else if ( s_eqi ( text, 'TRIANGLES' ) ) then

          keyword = 'TRIANGLES'

        else if ( s_eqi ( text, 'VERTICES' ) ) then

          keyword = 'VERTICES'
c
c  Presumably, numeric data to be processed by keyword.
c
        else if ( s_eqi ( keyword, 'DIMENSION' ) ) then

          call s_to_i4 ( text, dim, ierror, length )

          keyword = 'NONE'

        else if ( s_eqi ( keyword, 'EDGES' ) ) then

          call s_to_i4 ( text, edges, ierror, length )

          keyword = 'EDGE_VERTEX'

        else if ( s_eqi ( keyword, 'EDGE_VERTEX' ) ) then

        else if ( s_eqi ( keyword, 'HEXAHEDRONS' ) ) then

          call s_to_i4 ( text, hexahedrons, ierror, length )

          keyword = 'HEXAHEDRON_VERTEX'

        else if ( s_eqi ( keyword, 'HEXAHEDRON_VERTEX' ) ) then

        else if ( s_eqi ( keyword, 'QUADRILATERALS' ) ) then

          call s_to_i4 ( text, quadrilaterals, ierror, length )

          keyword = 'QUADRILATERAL_VERTEX'

        else if ( s_eqi ( keyword, 'QUADRILATERAL_VERTEX' ) ) then

        else if ( s_eqi ( keyword, 'TETRAHEDRONS' ) ) then

          call s_to_i4 ( text, tetrahedrons, ierror, length )

          keyword = 'TETRAHEDRON_VERTEX'

        else if ( s_eqi ( keyword, 'TETRAHEDRON_VERTEX' ) ) then

        else if ( s_eqi ( keyword, 'TRIANGLES' ) ) then

          call s_to_i4 ( text, triangles, ierror, length )

          keyword = 'TRIANGLE_VERTEX'

        else if ( s_eqi ( keyword, 'TRIANGLE_VERTEX' ) ) then

        else if ( s_eqi ( keyword, 'VERTICES' ) ) then

          call s_to_i4 ( text, vertices, ierror, length )

          keyword = 'VERTEX_COORDINATE'

        else if ( s_eqi ( keyword, 'VERTEX_COORDINATE' ) ) then

        else if ( s_eqi ( keyword, 'SKIP' ) ) then

        else

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MESH_SIZE_READ - Fatal error!'
          write ( *, '(a,i8)' )
     &      '  Could not find keyword while reading line ', line_num
          write ( *, '(a)' ) '"' // trim ( text ) // '".'
          stop

        end if

      go to 10

20    continue
c
c  Close the file.
c
      close ( unit = fileunit )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) '  Read ', line_num,
     &  ' lines from "' // trim ( filename ) // '".'

      return
      end
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
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
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine r8mat_zero ( m, n, a )

c*********************************************************************72
c
cc R8MAT_ZERO zeroes an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Output, double precision A(M,N), the matrix of zeroes.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a(i,j) = 0.0D+00
        end do
      end do

      return
      end
      subroutine s_adjustl ( s )

c*********************************************************************72
c
cc S_ADJUSTL flushes a string left.
c
c  Discussion:
c
c    Both blanks and tabs are treated as "white space".
c
c    This routine is similar to the FORTRAN90 ADJUSTL routine.
c
c  Example:
c
c    Input             Output
c
c    '     Hello'      'Hello     '
c    ' Hi therec  '    'Hi therec   '
c    'Fred  '          'Fred  '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 Jun3 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character * ( * ) S.
c    On input, S is a string of characters.
c    On output, any initial blank or tab characters have been cut.
c
      implicit none

      integer i
      integer nonb
      character * ( * ) s
      integer s_length
      character tab

      tab = char ( 9 )
c
c  Check the length of the string to the last nonblank.
c  If nonpositive, return.
c
      s_length = len_trim ( s )

      if ( s_length .le. 0 ) then
        return
      end if
c
c  Find NONB, the location of the first nonblank, nontab.
c
      nonb = 0

      do i = 1, s_length

        if ( s(i:i) .ne. ' ' .and. s(i:i) .ne. tab ) then
          nonb = i
          go to 10
        end if

      end do

10    continue

      if ( nonb .eq. 0 ) then
        s = ' '
        return
      end if
c
c  Shift the string left.
c
      if ( 1 .lt. nonb ) then
        do i = 1, s_length + 1 - nonb
          s(i:i) = s(i+nonb-1:i+nonb-1)
        end do
      end if
c
c  Blank out the end of the string.
c
      s(s_length+2-nonb:s_length) = ' '

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
      function s_eqi ( s1, s2 )

c*********************************************************************72
c
cc S_EQI is a case insensitive comparison of two strings for equality.
c
c  Example:
c
c    S_EQI ( 'Anjana', 'ANJANA' ) is TRUE.
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
c    Input, character*(*) S1, S2, the strings to compare.
c
c    Output, logical S_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c2
      integer i
      integer lenc
      logical s_eqi
      character*(*) s1
      integer s1_length
      character*(*) s2
      integer s2_length

      s1_length = len ( s1 )
      s2_length = len ( s2 )
      lenc = min ( s1_length, s2_length )

      s_eqi = .false.

      do i = 1, lenc

        c1 = s1(i:i)
        c2 = s2(i:i)
        call ch_cap ( c1 )
        call ch_cap ( c2 )

        if ( c1 .ne. c2 ) then
          return
        end if

      end do

      do i = lenc + 1, s1_length
        if ( s1(i:i) .ne. ' ' ) then
          return
        end if
      end do

      do i = lenc + 1, s2_length
        if ( s2(i:i) .ne. ' ' ) then
          return
        end if
      end do

      s_eqi = .true.

      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

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
      integer s_len_trim

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, s_len_trim ( s )

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
        length = s_len_trim ( s )
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
c    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
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
      integer s_len_trim

      nchar = s_len_trim ( s )

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

        if ( nchar .lt. length+1 ) then
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
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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

