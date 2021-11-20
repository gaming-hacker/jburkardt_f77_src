      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM_TO_XML.
c
c  Discussion:
c
c    FEM_TO_XML converts a mesh from FEM to DOLFIN XML format.
c
c  Usage:
c
c    fem_to_xml prefix
c
c    where 'prefix' is the common filename prefix:
c
c    * 'prefix'_nodes.txt contains the linear node coordinates,
c    * 'prefix'_elements.txt contains the linear element definitions.
c    * 'prefix'.xml will contain the DOLFIN XML version of the data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Anders Logg, Kent-Andre Mardal, Garth Wells,
c    Automated Solution of Differential Equations by the Finite Element
c    Method: The FEniCS Book,
c    Lecture Notes in Computational Science and Engineering,
c    Springer, 2011,
c    ISBN13: 978-3642230981
c
      implicit none

      integer arg_num

      integer element
      character * ( 255 ) element_filename
      integer element_num
      integer element_order
      integer i
      integer iarg
      integer iargc
      integer m
      character * ( 255 ) node_filename
      integer node_num
      character * ( 255 ) prefix
      character * ( 255 ) xml_filename

      call timestamp ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FEM_TO_XML'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Convert a 1D, 2D or 3D mesh ' //
     &  'from FEM to DOLFIN XML format.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Read "prefix"_nodes.txt, node coordinates.'
      write ( *, '(a)' ) '  Read "prefix"_elements.txt,' // 
     &  ' element node connectivity definitions.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Create "prefix".xml, a corresponding DOLFIN XML file.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Argument 1 is the common filename prefix.
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, prefix )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Please enter the filename prefix.'

        read ( *, '(a)' ) prefix

      end if
c
c  Create the filenames.
c
      node_filename = trim ( prefix ) // '_nodes.txt'
      element_filename = trim ( prefix ) // '_elements.txt'
      xml_filename = trim ( prefix ) // '.xml'
c
c  Read the node header.
c
      call r8mat_header_read ( node_filename, m, node_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the header of "' 
     &  // trim ( node_filename ) //'".'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension = ', m
      write ( *, '(a,i8)' ) '  Number of points  = ', node_num
c
c  Read the element header.
c
      call i4mat_header_read ( element_filename, element_order, 
     &  element_num )

      if ( m .eq. 1 ) then

        if ( element_order .eq. 2 ) then

        else
          write ( *, * ) ' '
          write ( *, '(a)' ) 'FEM_TO_XML - Fatal error!'
          write ( *, '(a)' ) 
     &      '  1D element data must use 2 vertices.'
          stop 1
        end if

      else if ( m .eq. 2 ) then

        if ( element_order .eq. 3 ) then
        elseif ( element_order .eq. 6 ) then
        else
          write ( *, * ) ' '
          write ( *, '(a)' ) 'FEM_TO_XML - Fatal error!'
          write ( *, '(a)' ) 
     &      ' 2d element data must use 3 vertices.'
          stop 1
        end if

       else if ( m .eq. 3 ) then

        if ( element_order .eq. 4 ) then

        else
          write ( *, * ) ' '
          write ( *, '(a)' ) 'FEM_TO_XML - Fatal error!'
          write ( *, '(a)' ) 
     &      '  3D element data must use 4 vertices.'
          stop 1
        end if

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the header of "' 
     &  // trim ( element_filename ) //'".'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Element order = ', element_order
      write ( *, '(a,i8)' ) '  Number of elements = ', element_num

      call main_sub ( m, element_filename, element_num, 
     &  element_order, node_filename, node_num, xml_filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM_TO_XML'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine main_sub ( m, element_filename, element_num, 
     &  element_order, node_filename, node_num, xml_filename )

c*********************************************************************72
c
cc MAIN_SUB makes up for the fact that FORTRAN77 can't allocate memory.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Anders Logg, Kent-Andre Mardal, Garth Wells,
c    Automated Solution of Differential Equations by the Finite Element
c    Method: The FEniCS Book,
c    Lecture Notes in Computational Science and Engineering,
c    Springer, 2011,
c    ISBN13: 978-3642230981
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, character * ( * ) ELEMENT_FILENAME, the name of the element file.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, character * ( * ) NODE_FILENAME, the name of the node file.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, character ( * ) XML_FILENAME, the name of the XML file.
c
      implicit none

      integer element_num
      integer element_order
      integer m
      integer node_num

      character * ( * ) element_filename
      integer element_node(element_order,element_num)
      character * ( * ) node_filename
      double precision node_x(m,node_num)
      character * ( * ) xml_filename
c
c  Read the node data.
c
      call r8mat_data_read ( node_filename, m, node_num, 
     &  node_x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the data in "' // 
     &  trim ( node_filename ) //'".'

      call r8mat_transpose_print_some ( m, node_num, node_x, 
     &  1, 1, m, 5, '  Initial portion of node coordinate data:' )
c
c  Read the element data.
c
      call i4mat_data_read ( element_filename, element_order, 
     &  element_num, element_node )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the data in "' 
     &  // trim ( element_filename ) //'".'

      call i4mat_transpose_print_some ( element_order, element_num, 
     &  element_node, 1, 1, element_order, 10, 
     &  '  Initial portion of element data:' )
c
c  Write the XML version of the data.
c
      if ( m .eq. 1 ) then

        call xml_mesh1d_write ( xml_filename, m, node_num, 
     &    node_x, element_order, element_num, element_node )

      else if ( m .eq. 2 ) then

        call xml_mesh2d_write ( xml_filename, m, node_num, 
     &    node_x, element_order, element_num, element_node )

      else if ( m .eq. 3 ) then

        call xml_mesh3d_write ( xml_filename, m, node_num, 
     &    node_x, element_order, element_num, element_node )

      end if

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Created XML file "' // 
     &  trim ( xml_filename ) // '".'

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
      function i4mat_max ( m, n, a )

c*********************************************************************72
c
cc I4MAT_MAX returns the maximum of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is a rectangular array of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 June 2010
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
c    Input, integer A(M,N), the M by N matrix.
c
c    Output, integer I4MAT_MAX, the maximum entry of A.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_max
      integer j

      i4mat_max = - i4_huge

      do j = 1, n
        do i = 1, m
          i4mat_max = max ( i4mat_max, a(i,j) )
        end do
      end do

      return
      end
      function i4mat_min ( m, n, a )

c*********************************************************************72
c
cc I4MAT_MIN returns the minimum of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is a rectangular array of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 June 2010
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
c    Input, integer A(M,N), the M by N matrix.
c
c    Output, integer I4MAT_MIN, the minimum entry of A.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_min
      integer j

      i4mat_min = i4_huge

      do j = 1, n
        do i = 1, m
          i4mat_min = min ( i4mat_min, a(i,j) )
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
     &   jhi, title )

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
      subroutine mesh_base_zero ( node_num, element_order, element_num, 
     &  element_node )

c*********************************************************************72
c
cc MESH_BASE_ZERO ensures that the element definition is zero-based.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input/output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), the element
c    definitions.
c
      implicit none

      integer element_num
      integer element_order

      integer element
      integer element_node(element_order,element_num)
      integer i
      integer i4mat_max
      integer i4mat_min
      integer j
      integer node
      integer node_max
      integer node_min
      integer node_num
      integer order

      node_min = i4mat_min ( element_order, element_num, element_node )
      node_max = i4mat_max ( element_order, element_num, element_node )

      if ( node_min .eq. 0 .and. node_max .eq. node_num - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ZERO:'
        write ( *, '(a)' ) 
     &    '  The element indexing appears to be 0-based!'
        write ( *, '(a)' ) '  No conversion is necessary.'
      else if ( node_min .eq. 1 .and. node_max .eq. node_num  ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ZERO:'
        write ( *, '(a)' ) 
     &    '  The element indexing appears to be 1-based!'
        write ( *, '(a)' ) '  This will be converted to 0-based.'
        do j = 1, element_num
          do i = 1, element_order
            element_node(i,j) = element_node(i,j) - 1
          end do
        end do
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ZERO - Warning!'
        write ( *, '(a)' ) 
     &    '  The element indexing is not of a recognized type.'
        write ( *, '(a,i8)' ) '  NODE_MIN = ', node_min
        write ( *, '(a,i8)' ) '  NODE_MAX = ', node_max
        write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
      end if

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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

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

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

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
      subroutine xml_mesh1d_write ( xml_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc XML_MESH1D_WRITE writes 1D mesh data as a DOLFIN XML file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Anders Logg, Kent-Andre Mardal, Garth Wells,
c    Automated Solution of Differential Equations by the Finite Element
c    Method: The FEniCS Book,
c    Lecture Notes in Computational Science and Engineering,
c    Springer, 2011,
c    ISBN13: 978-3642230981
c
c  Parameters:
c
c    Input, character * ( * ) XML_FILENAME, the name of the XML file 
c    to create.
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
      character * ( * ) xml_filename
      integer xml_unit
      integer node
      double precision node_x(m,node_num)
c
c  Force 0-based indexing.
c
      call mesh_base_zero ( node_num, element_order, element_num, 
     &  element_node )
c
c  Open the file.
c
      call get_unit ( xml_unit )

      open ( unit = xml_unit, file = xml_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( xml_unit, '(a)' ) 
     &  '<?xml version="1.0" encoding="UTF-8"?>'
      write ( xml_unit, '(a)' ) ''
      write ( xml_unit, '(a)' ) 
     &  '<dolfin xmlns:dolfin="http://www.fenics.org/dolfin/">'
      write ( xml_unit, '(a)' ) '  <mesh celltype="interval" dim="1">'

      write ( xml_unit, '(a,i6,a)' ) 
     &  '    <vertices size="', node_num, '">'
      do node = 1, node_num
        write ( xml_unit, '(a,i6,a,g14.6,a,g14.6,a)' ) 
     &    '      <vertex index ="', node - 1, 
     &    '" x ="', node_x(1,node), 
     &    '"/>'
      end do
      write ( xml_unit, '(a)' ) '    </vertices>'

      write ( xml_unit, '(a,i6,a)' ) 
     &  '    <cells size="', element_num, '">'
      do element = 1, element_num
        write ( xml_unit, '(a,i6,a,i6,a,i6,a,i6,a)' ) 
     &    '      <interval index ="', element - 1, 
     &    '" v0 ="', element_node(1,element), 
     &    '" v1 ="', element_node(2,element), '"/>'
      end do
      write ( xml_unit, '(a)' ) '    </cells>'
      write ( xml_unit, '(a)' ) '  </mesh>'
      write ( xml_unit, '(a)' ) '</dolfin>'

      close ( unit = xml_unit )

      return
      end
      subroutine xml_mesh2d_write ( xml_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc XML_MESH2D_WRITE writes 2D mesh data as a DOLFIN XML file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Anders Logg, Kent-Andre Mardal, Garth Wells,
c    Automated Solution of Differential Equations by the Finite Element
c    Method: The FEniCS Book,
c    Lecture Notes in Computational Science and Engineering,
c    Springer, 2011,
c    ISBN13: 978-3642230981
c
c  Parameters:
c
c    Input, character * ( * ) XML_FILENAME, the name of the XML file 
c    to create.
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
      character * ( * ) xml_filename
      integer xml_unit
      integer node
      double precision node_x(m,node_num)
c
c  Force 0-based indexing.
c
      call mesh_base_zero ( node_num, element_order, element_num, 
     &  element_node )
c
c  Open the file.
c
      call get_unit ( xml_unit )

      open ( unit = xml_unit, file = xml_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( xml_unit, '(a)' ) 
     &  '<?xml version="1.0" encoding="UTF-8"?>'
      write ( xml_unit, '(a)' ) ''
      write ( xml_unit, '(a)' ) 
     &  '<dolfin xmlns:dolfin="http://www.fenics.org/dolfin/">'
      write ( xml_unit, '(a)' ) '  <mesh celltype="triangle" dim="2">'

      write ( xml_unit, '(a,i6,a)' ) 
     &  '    <vertices size="', node_num, '">'
      do node = 1, node_num
        write ( xml_unit, '(a,i6,a,g14.6,a,g14.6,a)' ) 
     &    '      <vertex index ="', node - 1, 
     &    '" x ="', node_x(1,node), 
     &    '" y ="', node_x(2,node), 
     &    '" z ="0"/>'
      end do
      write ( xml_unit, '(a)' ) '    </vertices>'

      write ( xml_unit, '(a,i6,a)' ) 
     &  '    <cells size="', element_num, '">'
      do element = 1, element_num
        write ( xml_unit, '(a,i6,a,i6,a,i6,a,i6,a)' ) 
     &    '      <triangle index ="', element - 1, 
     &    '" v0 ="', element_node(1,element), 
     &    '" v1 ="', element_node(2,element), 
     &    '" v2 ="', element_node(3,element), '"/>'
      end do
      write ( xml_unit, '(a)' ) '    </cells>'
      write ( xml_unit, '(a)' ) '  </mesh>'
      write ( xml_unit, '(a)' ) '</dolfin>'

      close ( unit = xml_unit )

      return
      end
      subroutine xml_mesh3d_write ( xml_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

c*********************************************************************72
c
cc XML_MESH3D_WRITE writes 3D mesh data as a DOLFIN XML file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Anders Logg, Kent-Andre Mardal, Garth Wells,
c    Automated Solution of Differential Equations by the Finite Element
c    Method: The FEniCS Book,
c    Lecture Notes in Computational Science and Engineering,
c    Springer, 2011,
c    ISBN13: 978-3642230981
c
c  Parameters:
c
c    Input, character * ( * ) XML_FILENAME, the name of the XML file 
c    to create.
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
      character * ( * ) xml_filename
      integer xml_unit
      integer node
      double precision node_x(m,node_num)
c
c  Force 0-based indexing.
c
      call mesh_base_zero ( node_num, element_order, element_num, 
     &  element_node )
c
c  Open the file.
c
      call get_unit ( xml_unit )

      open ( unit = xml_unit, file = xml_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( xml_unit, '(a)' ) 
     &  '<?xml version="1.0" encoding="UTF-8"?>'
      write ( xml_unit, '(a)' ) ''
      write ( xml_unit, '(a)' ) 
     &  '<dolfin xmlns:dolfin="http://www.fenics.org/dolfin/">'
      write ( xml_unit, '(a)' ) 
     &  '  <mesh celltype="tetrahedron" dim="3">'

      write ( xml_unit, '(a,i6,a)' ) 
     &  '    <vertices size="', node_num, '">'
      do node = 1, node_num
        write ( xml_unit, '(a,i6,a,g14.6,a,g14.6,a,g14.6,a)' ) 
     &    '      <vertex index ="', node - 1, 
     &    '" x ="', node_x(1,node), 
     &    '" y ="', node_x(2,node), 
     &    '" z ="', node_x(3,node), '"/>'
      end do
      write ( xml_unit, '(a)' ) '    </vertices>'

      write ( xml_unit, '(a,i6,a)' ) 
     &  '    <cells size="', element_num, '">'
      do element = 1, element_num
        write ( xml_unit, '(a,i6,a,i6,a,i6,a,i6,a,i6,a)' ) 
     &    '      <tetrahedron index ="', element - 1, 
     &    '" v0 ="', element_node(1,element), 
     &    '" v1 ="', element_node(2,element), 
     &    '" v2 ="', element_node(3,element), 
     &    '" v3 ="', element_node(4,element), '"/>'
      end do
      write ( xml_unit, '(a)' ) '    </cells>'
      write ( xml_unit, '(a)' ) '  </mesh>'
      write ( xml_unit, '(a)' ) '</dolfin>'

      close ( unit = xml_unit )

      return
      end
