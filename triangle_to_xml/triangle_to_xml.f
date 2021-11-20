      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE_TO_XML.
c
c  Discussion:
c
c    TRIANGLE_TO_XML converts mesh data from TRIANGLE format to XML format.
c
c  Usage:
c
c    triangle_to_xml prefix
c
c    where 'prefix' is the common filename prefix:
c
c    * 'prefix'.node contains the triangle node coordinates,
c    * 'prefix'.ele contains the triangle element node connectivity.
c    * 'prefix'.xml will contain the XML file.
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

      integer arg_num
      integer element_att_num
      integer element_num
      integer element_order

      integer iarg
      integer iargc
      integer m
      integer node_att_num
      integer node_marker_num
      integer node_num;
      character * ( 255 ) prefix
      character * ( 255 ) triangle_element_filename
      character * ( 255 ) triangle_node_filename
      character * ( 255 ) xml_filename
      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_TO_XML'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Read a mesh description by TRIANGLE:'
      write ( *, '(a)' ) '  * "prefix".node, node coordinates.'
      write ( *, '(a)' ) '  * "prefix".ele, element connectivity.'
      write ( *, '(a)' ) '  Write a corresponding XML file.'
      write ( *, '(a)' ) '  * "prefix".xml.'
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
      triangle_node_filename = trim ( prefix ) // '.node'
      triangle_element_filename = trim ( prefix ) // '.ele'
      xml_filename = trim ( prefix ) // '.xml'
c
c  Read the triangle node size information.
c
      call triangle_node_size_read ( triangle_node_filename, node_num,
     &  m, node_att_num, node_marker_num )
c
c  Read the triangle element size information.
c
      call triangle_element_size_read ( triangle_element_filename, 
     &  element_num, element_order, element_att_num )
c
c  Report sizes.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Size information from TRIANGLE files:'
      write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
      write ( *, '(a,i4)' ) '  Number of nodes NODE_NUM = ', node_num
      write ( *, '(a,i4)' ) '  NODE_ATT_NUM = ', node_att_num
      write ( *, '(a,i4)' ) '  NODE_MARKER_NUM = ', node_marker_num
      write ( *, '(a,i4)' ) 
     &  '  Number of elements ELEMENT_NUM = ', element_num
      write ( *, '(a,i4)' ) 
     &  '  Element order ELEMENT_ORDER = ', element_order
      write ( *, '(a,i4)' ) '  ELEMENT_ATT_NUM = ', element_att_num
c
c  Call a subroutine so we can allocate memory.
c
      call triangle_to_fem_handle ( triangle_node_filename, 
     &  triangle_element_filename, xml_filename, 
     &  element_att_num, element_num, 
     &  element_order, m, node_att_num, node_marker_num, node_num )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIANGLE_TO_FEM:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine triangle_to_fem_handle ( triangle_node_filename, 
     &  triangle_element_filename, xml_filename, 
     &  element_att_num, element_num, 
     &  element_order, m, node_att_num, node_marker_num, node_num )

c*********************************************************************72
c
cc TRIANGLE_TO_FEM_HANDLER sets up arrays before processing.
c
c  Discussion:
c
c    Officially, FORTRAN77 can't allocate memory, except perhaps when
c    a subroutine is called.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer element_att_num
      integer element_num
      integer element_order
      integer m
      integer node_att_num
      integer node_marker_num
      integer node_num;

      double precision element_att(element_att_num,element_num)
      integer element_node(element_order,element_num)
      double precision node_att(node_att_num,node_num)
      integer node_marker(node_marker_num,node_num)
      double precision node_x(m,node_num)
      character * ( * ) triangle_element_filename
      character * ( * ) triangle_node_filename
      character * ( * ) xml_filename
c
c  Read TRIANGLE data.
c
      call triangle_node_data_read ( triangle_node_filename, node_num,
     &  m, node_att_num, node_marker_num, node_x, node_att, 
     &  node_marker )

      call triangle_element_data_read ( triangle_element_filename, 
     &  element_num, element_order, element_att_num, element_node, 
     &  element_att )
c
c  Write XML data.
c
      call xml_mesh2d_write ( xml_filename, m, node_num, 
     &  node_x, element_order, element_num, element_node )

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

      integer element_node(element_order,element_num)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer node_max
      integer node_min
      integer node_num

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
      subroutine triangle_element_data_read ( element_filename, 
     &  element_num, element_order, element_att_num, element_node, 
     &  element_att )

c*********************************************************************72
c
cc TRIANGLE_ELEMENT_DATA_READ reads the data from an element file.
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
c    Input, character * ( * ) ELEMENT_FILENAME, the name of the file.
c
c    Input, integer ELEMENT_NUM, the number of elements.
c
c    Input, integer ELEMENT_ORDER, the order of the elements.
c
c    Input, integer ELEMENT_ATT_NUM, number of element attributes
c    listed on each node record.
c
c    Output, integer ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
c    the indices of the nodes that make up each element.
c
c    Output, double precision ELEMENT_ATT(ELEMENT_ATT_NUM,ELEMENT_NUM), the
c    attributes of each element.
c
      implicit none

      integer element_att_num
      integer element_num
      integer element_order

      integer element
      double precision element_att(element_att_num,element_num)
      character * ( * ) element_filename
      integer element_node(element_order,element_num)
      integer i
      integer i1
      integer i2
      integer i3
      integer ierror
      integer input
      integer ios
      integer ival
      integer length
      character * ( 255 ) text
      double precision value

      element = 0

      call get_unit ( input )

      open ( unit = input, file = element_filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRIANGLE_ELEMENT_DATA_READ - Fatal error!'
        write ( *, '(a)' ) '  Unable to open file.'
        stop 1
      end if

10    continue

        read ( input, '(a)', iostat = ios ) text

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'TRIANGLE_ELEMENT_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Unexpected end of file while reading.'
          stop 1
        end if

        if ( len_trim ( text ) .eq. 0 ) then
          go to 10
        end if

        if ( text(1:1) .eq. '#' ) then
          go to 10
        end if

        if ( element .eq. 0 ) then

          call s_to_i4 ( text, i1, ierror, length )
          text = text(length+1:)
          call s_to_i4 ( text, i2, ierror, length )
          text = text(length+1:)
          call s_to_i4 ( text, i3, ierror, length )
          text = text(length+1:)

        else

          call s_to_i4 ( text, ival, ierror, length )
          text = text(length+1:)

          do i = 1, element_order
            call s_to_i4 ( text, ival, ierror, length )
            text = text(length+1:)
            element_node(i,element) = ival
          end do

          do i = 1, element_att_num
            call s_to_r8 ( text, value, length, ierror )
            text = text(length+1:)
            element_att(i,element) = value
          end do

        end if

        element = element + 1

        if ( element_num .lt. element ) then
          go to 20
        end if

      go to 10

20    continue

      close ( unit = input )

      return
      end
      subroutine triangle_element_size_read ( element_filename, 
     &  element_num, element_order, element_att_num )

c*********************************************************************72
c
cc TRIANGLE_ELEMENT_SIZE_READ reads the header information from an element file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) ELEMENT_FILENAME, the name of the
c    element file.
c
c    Output, integer ELEMENT_NUM, the number of elements.
c
c    Output, integer ELEMENT_ORDER, the order of the elements.
c
c    Output, integer ELEMENT_ATT_NUM, the number of
c    element attributes.
c
      implicit none

      integer element_att_num
      character * ( * ) element_filename
      integer element_num
      integer element_order
      integer ierror
      integer input
      integer ios
      integer length
      character * ( 255 ) text

      call get_unit ( input )

      open ( unit = input, file = element_filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRIANGLE_ELEMENT_SIZE_READ - Fatal error!'
        write ( *, '(a)' ) '  Unable to open file.'
        stop 1
      end if

10    continue

        read ( input, '(a)', iostat = ios ) text

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'TRIANGLE_ELEMENT_SIZE_READ - Fatal error!'
          write ( *, '(a)' ) '  Unexpected end of file while reading.'
          stop 1
        end if

        if ( len_trim ( text ) .eq. 0 ) then
          go to 10
        end if

        if ( text(1:1) .eq. '#' ) then
          go to 10
        end if

        call s_to_i4 ( text, element_num, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, element_order, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, element_att_num, ierror, length )
        text = text(length+1:)

        go to 20

      go to 10

20    continue

      close ( unit = input )

      return
      end
      subroutine triangle_node_data_read ( node_filename, node_num, 
     &  node_dim, node_att_num, node_marker_num, node_coord, node_att, 
     &  node_marker )

c*********************************************************************72
c
cc TRIANGLE_NODE_DATA_READ reads the data from a node file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 October 2014
c
c  Author:
c
c    John Burkardt
c
c Parameters:
c
c   Input, character * ( * ) NODE_FILENAME, the name of the node file.
c
c   Input, integer NODE_NUM, the number of nodes.
c
c   Input, integer NODE_DIM, the spatial dimension.
c
c   Input, integer NODE_ATT_NUM, number of node attributes
c   listed on each node record.
c
c   Input, integer NODE_MARKER_NUM, 1 if every node record
c   includes a final boundary marker value.
c
c   Output, double precision NODE_COORD(NODE_DIM,NODE_NUM), the nodal
c   coordinates.
c
c   Output, double precision NODE_ATT(NODE_ATT_NUM,NODE_NUM), the nodal
c   attributes.
c
c   Output, integer NODE_MARKER(NODE_MARKER_NUM,NODE_NUM), the
c   node markers.
c
      implicit none

      integer node_att_num
      integer node_dim
      integer node_marker_num
      integer node_num

      integer i
      integer i1
      integer i2
      integer i3
      integer i4
      integer ierror
      integer input
      integer ios
      integer ival
      integer length
      integer node
      double precision node_att(node_att_num,node_num)
      double precision node_coord(node_dim,node_num)
      character * ( * ) node_filename
      integer node_marker(node_marker_num,node_num)
      character * ( 255 ) text
      double precision value

      node = 0

      call get_unit ( input )

      open ( unit = input, file = node_filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRIANGLE_NODE_DATA_READ - Fatal error!'
        write ( *, '(a)' ) '  Unable to open file.'
        stop 1
      end if

10    continue

        read ( input, '(a)', iostat = ios ) text

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'TRIANGLE_NODE_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Unexpected end of file while reading.'
          stop 1
        end if

        if ( len_trim ( text ) .eq. 0 ) then
          go to 10
        end if

        if ( text(1:1) .eq. '#' ) then
          go to 10
        end if
c
c  Ignore the dimension line.
c
        if ( node .eq. 0 ) then

        else

          call s_to_i4 ( text, ival, ierror, length )
          text = text(length+1:)

          do i = 1, node_dim
            call s_to_r8 ( text, value, ierror, length )
            text = text(length+1:)
            node_coord(i,node) = value
          end do

          do i = 1, node_att_num
            call s_to_r8 ( text, value, ierror, length )
            text = text(length+1:)
            node_att(i,node) = value;
          end do

          do i = 1, node_marker_num
            call s_to_i4 ( text, ival, ierror, length )
            text = text(length+1:)
            node_marker(i,node) = ival
          end do

        end if

        node = node + 1

        if ( node_num .lt. node ) then
          go to 20
        end if

      go to 10

20    continue

      close ( unit = input )

      return
      end
      subroutine triangle_node_size_read ( node_filename, node_num, 
     &  node_dim, node_att_num, node_marker_num )

c*********************************************************************72
c
cc TRIANGLE_NODE_SIZE_READ reads the header information from a node file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) NODE_FILENAME, the name of the node file.
c
c    Output, integer NODE_NUM, the number of nodes.
c
c    Output, integer NODE_DIM, the spatial dimension.
c
c    Output, integer NODE_ATT_NUM, number of node attributes
c    listed on each node record.
c
c    Output, integer NODE_MARKER_NUM, 1 if every node record
c    includes a final boundary marker value.
c
      implicit none

      integer ierror
      integer input
      integer ios
      integer length
      integer node_att_num
      integer node_dim
      character * ( * ) node_filename
      integer node_marker_num
      integer node_num
      character * ( 255 ) text

      call get_unit ( input )

      open ( unit = input, file = node_filename, status = 'old',
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRIANGLE_NODE_SIZE_READ - Fatal error!'
        write ( *, '(a)' ) '  Unable to open file.'
        stop 1
      end if

10    continue

        read ( input, '(a)', iostat = ios ) text

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'TRIANGLE_NODE_SIZE_READ - Fatal error!'
          write ( *, '(a)' ) '  Unexpected end of file while reading.'
          stop 1
        end if

        if ( len_trim ( text ) .eq. 0 ) then
          go to 10
        end if

        if ( text(1:1) .eq. '#' ) then
          go to 10
        end if

        call s_to_i4 ( text, node_num, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, node_dim, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, node_att_num, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, node_marker_num, ierror, length )
        text = text(length+1:)

        go to 20

      go to 10

20    continue

      close ( unit = input )

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
