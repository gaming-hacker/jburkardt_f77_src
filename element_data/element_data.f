      program main

c*********************************************************************72
c
cc MAIN is the main program for ELEMENT_DATA.
c
c  Discussion:
c
c    ELEMENT_DATA organizes data on a rectangular grid into finite element data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 June 2001
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer maxelm
      integer maxnp
      integer maxnq
      integer npe

      parameter ( maxelm = 2000 )
      parameter ( maxnp = 6000 )
      parameter ( maxnq = 10 )
      parameter ( npe = 4 )

      character*80 element_file_name
      integer i
      integer iarg
      integer iargc
      integer ierror
      integer ihi
      integer ilen
      integer ilo
      integer indx(maxnp)
      integer ios
      integer ipxfargc
      character*256 names
      integer nelem
      integer nelemx
      integer nelemy
      integer node(npe,maxelm)
      character*80 node_file_name
      integer np
      integer np2
      integer nq
      integer num_arg
      integer nx
      integer ny
      real v(maxnp,maxnq)
      character*80 v_file_name

      element_file_name = 'element.txt'
      node_file_name = 'node.txt'

      write ( *, * ) ' '
      write ( *, * ) 'ELEMENT_DATA'
      write ( *, * ) '  FORTRAN77 version'
      write ( *, * ) '  Reads a simple data file which defines'
      write ( *, * ) '  elements, nodes, and nodal values, and'
      write ( *, * ) '  constructs node and element files suitable'
      write ( *, * ) '  for use with the DISPLAY4 graphics program.'
      write ( *, * ) ' '
      write ( *, * ) '  Last modified on 03 July 2001.'
c
c  Get the number of command line arguments.
c
c  Old style:
c
      num_arg = iargc ( )
c
c  New style:
c
c num_arg = ipxfargc ( )
c
c  If at least one command line argument, it's the input file name.
c
      if ( num_arg .lt. 1 ) then

        write ( *, * ) ' '
        write ( *, * ) 'Enter the input file name:'
        read ( *, '(a)', iostat = ios ) v_file_name

        if ( ios .ne. 0 ) then
          write ( *, * ) ' '
          write ( *, * ) 'ELEMENT_DATA - Fatal error!'
          write ( *, * ) '  Unexpected read error!'
          stop
        end if

      else

        iarg = 1
c
c  Old style:
c
        call getarg ( iarg, v_file_name )
c
c  New style:
c
c   call pxfgetarg ( iarg, v_file_name, ilen, ierror )
c
c   if ( ierror .ne. 0 ) then
c     write ( *, * ) ' '
c     write ( *, * ) 'ELEMENT_DATA - Fatal error!'
c     write ( *, * ) '  Could not read command line argument.'
c     stop
c   end if

      end if
c
c  Now we know what to do.
c
      write ( *, * ) ' '
      write ( *, * ) 'ELEMENT_DATA'
      write ( *, * ) '  Read input file:  ', v_file_name
c
c  Estimate the value of NP by counting the lines in the file.
c
      call file_line_count ( v_file_name, np )

      write ( *, * ) ' '
      write ( *, * ) '  The properties file seems to indicate ', 
     &  np, ' nodes.'

      if ( np .gt. maxnp ) then
        write ( *, * ) ' '
        write ( *, * ) 'ELEMENT_DATA - Fatal error!'
        write ( *, * ) '  NP > MAXNP.'
        stop
      end if
c
c  Estimate the value of NQ by counting the columns in the file.
c
      call file_column_count ( v_file_name, nq )

      write ( *, * ) ' '
      write ( *, * ) '  The properties file seems to have ', 
     &  nq, ' columns.'

      if ( nq .gt. maxnq ) then
        write ( *, * ) ' '
        write ( *, * ) 'ELEMENT_DATA - Fatal error!'
        write ( *, * ) '  NQ > MAXNQ.'
        stop
      end if
c
c  Set the names.
c
      do i = 1, nq
        ilo = 14*(i-1)+1
        ihi = 14*i
        if ( i .eq. 1 ) then
          write ( names(ilo:ihi), '(a1,13x)' ) 'X'
        else if ( i .eq. 2 ) then
          write ( names(ilo:ihi), '(a1,13x)' ) 'Y'
        else
          write ( names(ilo:ihi), '(a1,i2.2,11x)' ) 'V', i-2
        end if
      end do

      call read_elements ( maxelm, maxnp, maxnq, nelem, node, np, 
     &  npe, nq, v, v_file_name )

      write ( *, * ) ' '
      write ( *, * ) 'ELEMENT_DATA:'
      write ( *, * ) '  Number of nodes NP = ', np
      write ( *, * ) '  Number of node properties NQ = ', nq
      write ( *, * ) '  Number of elements NELEM = ', nelem
c
c  Write the element information to a file.
c
      call write_element ( element_file_name, npe, maxelm, 
     & nelem, node )
c
c  Write the node information to a file.
c
      call write_node ( node_file_name, maxnp, np, maxnq, nq, v, names )

      write ( *, * ) ' '
      write ( *, * ) 'ELEMENT_DATA'
      write ( *, * ) '  Normal end of execution.'

      stop
      end
      subroutine ch_cap ( c )

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
c    19 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character C, the character to capitalize.
c
      implicit none

      character c
      integer itemp

      itemp = ichar ( c )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        c = char ( itemp - 32 )
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
c    CH_EQI ( 'A', 'a' ) is .TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    28 July 2000
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

      logical ch_eqi
      character c1
      character c1_cap
      character c2
      character c2_cap

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
      subroutine file_column_count ( file_name, ncolumn )

c*********************************************************************72
c
cc FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
c
c  Discussion:
c
c    The file is assumed to be a simple text file.
c
c    Most lines of the file is presumed to consist of NCOLUMN words, separated
c    by spaces.  There may also be some blank lines, and some comment lines,
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
c    21 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) FILE_NAME, the name of the file.
c
c    Output, integer NCOLUMN, the number of columns assumed to be in the file.
c
      implicit none

      character*(*) file_name
      logical got_one
      integer ios
      integer iunit
      character*256 line
      integer ncolumn
c
c  Open the file.
c
      call get_unit ( iunit )

      open ( unit = iunit, file = file_name, status = 'old', 
     &  form = 'formatted', access = 'sequential', iostat = ios )

      if ( ios .ne. 0 ) then
        ncolumn = - 1
        write ( *, * ) ' '
        write ( *, * ) 'FILE_COLUMN_COUNT - Fatal error!'
        write ( *, * ) '  Could not open the file:'
        write ( *, '(a)' ) file_name
        return
      end if
c
c  Read one line, but skip blank lines and comment lines.
c
      got_one = .false.

10    continue

        read ( iunit, '(a)', iostat = ios ) line

        if ( ios .ne. 0 ) then
          go to 20
        end if

        if ( line .eq. ' ' ) then
          go to 10
        end if

        if ( line(1:1) .eq. '#' ) then
          go to 10
        end if

        got_one = .true.

20    continue

      if ( .not. got_one ) then

        rewind ( iunit )

30      continue

          read ( iunit, '(a)', iostat = ios ) line

          if ( ios .ne. 0 ) then
            go to 40
          end if

          if ( line .eq. ' ' ) then
            go to 30
          end if

          got_one = .true.

40      continue

      end if

      close ( unit = iunit )

      if ( .not. got_one ) then
        write ( *, * ) ' '
        write ( *, * ) 'FILE_COLUMN_COUNT - Warning!'
        write ( *, * ) '  The file does not seem to contain any data.'
        ncolumn = 0
        return
      end if

      call word_count ( line, ncolumn )

      return
      end
      subroutine file_line_count ( file_name, nline )

c*********************************************************************72
c
cc FILE_LINE_COUNT counts the number of lines in a file.
c
c  Discussion:
c
c    The file is assumed to be a simple text file.
c
c    Blank lines and comment lines, which begin with '#', are not counted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) FILE_NAME, the name of the file.
c
c    Output, integer NLINE, the number of lines found in the file.
c
      implicit none

      character*(*) file_name
      integer ios
      integer iunit
      character*256 line
      integer nline
c
      nline = 0
c
c  Open the file.
c
      call get_unit ( iunit )

      open ( unit = iunit, file = file_name, status = 'old', 
     &  form = 'formatted', access = 'sequential', iostat = ios )

      if ( ios .ne. 0 ) then
        nline = - 1
        write ( *, * ) ' '
        write ( *, * ) 'FILE_LINE_COUNT - Fatal error!'
        write ( *, * ) '  Could not open the file:', file_name
        return
      end if
c
c  Count the lines.
c
10    continue

        read ( iunit, '(a)', iostat = ios ) line

        if ( ios .ne. 0 ) then
          go to 20
        end if

        if ( line .eq. ' ' ) then
          go to 10
        end if

        if ( line(1:1) .eq. '#' ) then
          go to 10
        end if

        nline = nline + 1

        go to 10

20    continue

      close ( unit = iunit )

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is an integer between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 March 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5 and 6).
c
c    Otherwise, IUNIT is an integer between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
      implicit none

      integer i
      integer ios
      integer iunit
      logical lopen

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 ) then

          inquire ( unit = i, opened = lopen, iostat = ios )

          if ( ios .eq. 0 ) then
            if ( .not. lopen ) then
              iunit = i
              return
            end if
          end if

        end if

      end do

      return
      end
      subroutine r_next ( s, r, done )

c*********************************************************************72
c
cc R_NEXT "reads" real numbers from a string, one at a time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string, presumably containing real
c    numbers.  These may be separated by spaces or commas.
c
c    Output, real R.  If DONE is FALSE, then R contains the
c    "next" real value read from the string.  If DONE is TRUE, then
c    R is zero.
c
c    Input/output, logical DONE.
c    On input with a fresh string, the user should set DONE to TRUE.
c    On output, the routine sets DONE to FALSE if another real
c    value was read, or TRUE if no more reals could be read.
c
      implicit none

      logical done
      integer ierror
      integer lchar
      integer next
      real r
      character*(*) s

      save next

      data next / 1 /

      r = 0.0E+00

      if ( done ) then
        next = 1
        done = .false.
      end if

      if ( next .gt. len ( s ) ) then
        done = .true.
        return
      end if

      call s_to_r ( s(next:), r, ierror, lchar )

      if ( ierror .ne. 0 .or. lchar .eq. 0 ) then
        done = .true.
        next = 1
      else
        done = .false.
        next = next + lchar
      end if

      return
      end
      subroutine read_elements ( maxelm, maxnp, maxnq, nelem, node, np, 
     &  npe, nq, v, v_file_name )

c*********************************************************************72
c
cc READ_ELEMENTS reads Niyazi Sahin's file and constructs the elements.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 July 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MAXELM, the maximum number of elements.
c
c    Input, integer MAXNP, the maximum number of nodes.
c
c    Input, integer MAXNQ, the maximum number of node properties.
c
c    Output, integer NELEM, the number of elements.
c
c    Output, integer NODE(NPE,MAXELM), the list of nodes that form each element.
c
c    Output, integer NP, the number of nodes.
c
c    Input, integer NPE, the number of nodes per element.
c
c    Output, integer NQ, the number of node properties.
c
c    Output, real V(MAXNP,MAXNQ), the list of node property values.
c
c    Input, character ( len = 80 ) V_FILE_NAME, the name of the node
c    property file.
c
      implicit none

      integer maxelm
      integer maxnp
      integer maxnq
      integer npe

      logical blank
      logical done
      integer ierror
      integer indx(maxnp)
      integer ios
      integer iunit
      integer ival
      integer j
      integer k
      character*256 line
      integer n
      integer nelem
      integer nnode
      integer node(npe,maxelm)
      integer np
      integer nq
      character scalar
      real v(maxnp,maxnq)
      character*80 v_file_name
      real v_temp(maxnq)
      real x(np)
      real xval
      real y(np)
      real yval

      scalar = 'S'

      call get_unit ( iunit )

      open ( unit = iunit, file = v_file_name, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'READ_ELEMENTS - Fatal error!'
        write ( *, * ) '  Could not open the file ', v_file_name
        stop
      end if
c
c  Read the data, one element at a time.
c  A blank record indicates a new element.
c  Many nodes are repeated.
c
      n = 0
      nelem = 0
      blank = .true.

10    continue

        read ( iunit, '(a)', iostat = ios ) line

        if ( ios .ne. 0 ) then
          go to 20
        end if

        if ( line .eq. ' ' ) then
          blank = .true.
          go to 10
        end if

        if ( line(1:1) .eq. '#' ) then
          go to 10
        end if

        if ( blank ) then
          blank = .false.
          nelem = nelem + 1
          if ( nelem .gt. maxelm ) then
            write ( *, * ) ' '
            write ( *, * ) 'READ_ELEMENTS - Fatal error!'
            write ( *, * ) '  Too many elements.'
            stop
          end if
          nnode = 0
        end if

        done = .true.
        do j = 1, nq
          call r_next ( line, v_temp(j), done )
        end do
c
c  Extract the X, Y values, store them if they are unique, and get the index.
c
        xval = v_temp(1)
        yval = v_temp(2)

        call rrvec_index_insert_unique ( np, n, x, y, indx, xval, yval, 
     &    ival, ierror )
c
c  Store the temporary data in the correct row.
c
        v(ival,1) = v_temp(1)
        v(ival,2) = v_temp(2)
        v(ival,3) = v_temp(5)
        v(ival,4) = v_temp(6)
c
c  We only have one scalar data slot for now.  Grab pressure or stream function.
c
        if ( scalar .eq. 'P' ) then
          v(ival,5) = v_temp(3)
        else
          v(ival,5) = v_temp(4)
        end if

        nnode = nnode + 1
c
c  The first node is repeated as the last node.  Skip it.
c
        if ( nnode .gt. 4 ) then
          go to 10
        end if

        node(nnode,nelem) = ival

        go to 10

20    continue

      close ( unit = iunit )

      write ( *, * ) ' '
      write ( *, * ) 'READ_ELEMENTS:'
      write ( *, * ) '  Number of elements is ', nelem
      write ( *, * ) '  Number of unique nodes is ', n

      np = n
      nq = 5

      return
      end
      function rrvec_compare ( x1, y1, x2, y2 )

c*********************************************************************72
c
cc RRVEC_COMPARE compares two RR vectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 October 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real X1, Y1, the first vector.
c
c    Input, real X2, Y2, the second vector.
c
c    Output, character RRVEC_COMPARE: '<', '>' or '=' if the first vector
c    is less, greater or equal to the second.
c
      implicit none

      character c
      character rrvec_compare
      real x1
      real x2
      real y1
      real y2

      if ( x1 .lt. x2 ) then
        c = '<'
      else if ( x1 .gt. x2 ) then
        c = '>'
      else if ( y1 .lt. y2 ) then
        c = '<'
      else if ( y1 .gt. y2 ) then
        c = '>'
      else
        c = '='
      end if

      rrvec_compare = c

      return
      end
      subroutine rrvec_index_search ( maxn, n, x, y, indx, xval, yval, 
     &  less, equal, more )

c*********************************************************************72
c
cc RRVEC_INDEX_SEARCH searches for an RR value in an indexed sorted list.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    24 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MAXN, the maximum size of the list.
c
c    Input, integer N, the size of the current list.
c
c    Input, real X(N), Y(N), the list.
c
c    Input, integer INDX(N), the sort index of the list.
c
c    Input, real XVAL, YVAL, the value to be sought.
c
c    Output, integer LESS, EQUAL, MORE, the indexes in INDX of the
c    entries of X that are just less than, equal to, and just greater
c    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
c    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
c    is the greatest entry of X, then MORE is N+1.
c
      implicit none

      integer maxn

      character c
      integer equal
      integer hi
      integer indx(maxn)
      integer less
      integer lo
      integer mid
      integer more
      integer n
      character rrvec_compare
      real x(maxn)
      real xhi
      real xlo
      real xmid
      real xval
      real y(maxn)
      real yhi
      real ylo
      real ymid
      real yval

      if ( n .le. 0 ) then
        less = 0
        equal = 0
        more = 0
        return
      end if

      lo = 1
      hi = n

      xlo = x(indx(lo))
      ylo = y(indx(lo))

      xhi = x(indx(hi))
      yhi = y(indx(hi))

      c = rrvec_compare ( xval, yval, xlo, ylo )

      if ( c .eq. '<' ) then
        less = 0
        equal = 0
        more = 1
        return
      else if ( c .eq. '=' ) then
        less = 0
        equal = 1
        more = 2
        return
      end if

      c = rrvec_compare ( xval, yval, xhi, yhi )

      if ( c .eq. '>' ) then
        less = n
        equal = 0
        more = n + 1
        return
      else if ( c .eq. '=' ) then
        less = n - 1
        equal = n
        more = n + 1
        return
      end if

10    continue

        if ( lo + 1 .eq. hi ) then
          less = lo
          equal = 0
          more = hi
          return
        end if

        mid = ( lo + hi ) / 2
        xmid = x(indx(mid))
        ymid = y(indx(mid))

        c = rrvec_compare ( xval, yval, xmid, ymid )

        if ( c .eq. '=' ) then
          equal = mid
          less = equal - 1
          more = equal + 1
          return
        else if ( c .eq. '<' ) then
          hi = mid
        else if ( c .eq. '>' ) then
          lo = mid
        end if

      go to 10

      end
      subroutine rrvec_index_insert_unique ( maxn, n, x, y, indx, xval, 
     &  yval, ival, ierror )

c*********************************************************************72
c
cc RRVEC_INDEX_INSERT_UNIQUE inserts a unique RR value in an indexed sorted list.
c
c  Discussion:
c
c    If the input value does not occur in the current list, it is added,
c    and N, X, Y and INDX are updated.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    25 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MAXN, the maximum size of the list.
c
c    Input/output, integer N, the size of the list.
c
c    Input/output, real X(N), Y(N), the list of R2 vectors.
c
c    Input/output, integer INDX(N), the sort index of the list.
c
c    Input, real XVAL, YVAL, the value to be inserted if it is
c    not already in the list.
c
c    Output, integer IVAL, the index in X, Y corresponding to the
c    value XVAL, YVAL.
c
c    Output, integer IERROR, 0 for no error, 1 if an error occurred.
c
      implicit none

      integer maxn

      integer equal
      integer ierror
      integer indx(maxn)
      integer ival
      integer j
      integer less
      integer more
      integer n
      real x(maxn)
      real xval
      real y(maxn)
      real yval

      ierror = 0

      if ( n .le. 0 ) then

        if ( maxn .le. 0 ) then
          ierror = 1
          write ( *, * ) ' '
          write ( *, * ) 'RRVEC_INDEX_INSERT_UNIQUE - Fatal error!'
          write ( *, * ) '  Not enough space to store new data.'
          return
        end if

        n = 1
        x(1) = xval
        y(1) = yval
        indx(1) = 1
        ival = 1
        return

      end if
c
c  Does ( XVAL, YVAL ) already occur in ( X, Y )?
c
      call rrvec_index_search ( maxn, n, x, y, indx, xval, yval, less, 
     &  equal, more )

      if ( equal .eq. 0 ) then

        if ( n .ge. maxn ) then
          ierror = 1
          write ( *, * ) ' '
          write ( *, * ) 'RRVEC_INDEX_INSERT_UNIQUE - Fatal error!'
          write ( *, * ) '  Not enough space to store new data.'
          return
        end if

        x(n+1) = xval
        y(n+1) = yval
        ival = n + 1
        do j = n+1, more+1, -1
          indx(j) = indx(j-1)
        end do
        indx(more) = n + 1
        n = n + 1

      else

        ival = indx(equal)

      end if

      return
      end
      subroutine s_to_r ( s, r, ierror, lchar )

c*********************************************************************72
c
cc S_TO_R reads a real number from a string.
c
c  Discussion:
c
c    This routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the real number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 spaces
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon.
c
c    with most quantities optional.
c
c  Example:
c
c    S                 R
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
c    12 February 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, real R, the real value that was read from the string.
c
c    Output, integer IERROR, error flag.
c
c    0, no errors occurred.
c
c    1, 2, 6 or 7, the input number was garbled.  The
c    value of IERROR is the last type of input successfully
c    read.  For instance, 1 means initial blanks, 2 means
c    a plus or minus sign, and so on.
c
c    Output, integer LCHAR, the number of characters read from
c    the string to form the number, including any terminating
c    characters such as a trailing comma or blanks.
c
      implicit none

      logical ch_eqi
      character c
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer lchar
      integer nchar
      integer ndig
      real r
      real rbot
      real rexp
      real rtop
      character*(*) s
      character TAB

      TAB = char ( 9 )
      nchar = len ( s )
      ierror = 0
      r = 0.0E+00
      lchar = - 1
      isgn = 1
      rtop = 0.0E+00
      rbot = 1.0E+00
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        lchar = lchar + 1
        c = s(lchar+1:lchar+1)
c
c  Blank or TAB character.
c
        if ( c .eq. ' ' .or. c .eq. TAB ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( ihave .gt. 1 ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            lchar = lchar + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = - 1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = - 1
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
          else if ( ihave .ge. 6 .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Exponent marker.
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
        else if ( ihave .lt. 11 .and. lge ( c, '0' ) 
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
            rtop = 10.0E+00 * rtop + real ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0E+00 * rtop + real ( ndig )
            rbot = 10.0E+00 * rbot
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
        if ( iterm .eq. 1 .or. lchar+1 .ge. nchar ) then
          go to 20
        end if

      go to 10
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LCHAR is equal to NCHAR.
c
20    continue

      if ( iterm .ne. 1 .and. lchar+1 .eq. nchar ) then
        lchar = nchar
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or. ihave .eq. 6 .or. 
     &  ihave .eq. 7 ) then

        ierror = ihave

        return
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0E+00
      else

        if ( jbot .eq. 1 ) then
          rexp = 10.0E+00**( jsgn * jtop )
        else
          rexp = jsgn * jtop
          rexp = rexp / jbot
          rexp = 10.0E+00**rexp
        end if

      end if

      r = isgn * rexp * rtop / rbot

      return
      end
      subroutine word_count ( s, nword )

c*********************************************************************72
c
cc WORD_COUNT counts the number of "words" in a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 April 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, the string to be examined.
c
c    Output, integer NWORD, the number of "words" in the string.
c    Words are presumed to be separated by one or more blanks.
c
      implicit none

      logical blank
      integer i
      integer lens
      integer nword
      character*(*) s

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
      subroutine write_element ( element_file, npe, maxelm, 
     &  nelem, node )

c*********************************************************************72
c
cc WRITE_ELEMENT writes an element data file.
c
c  Discussion:
c
c    The element file contains information about the organization of
c    the nodes into elements.  The format is as follows:
c
c      Line 1: NELEM, the number of elements
c      Line 2: NPE, the number of nodes per element.
c      Line 3 through NELEM+2: the node list for each element.
c
c    Blank lines, and comments lines, which begin with a "#", may occur anywhere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) ELEMENT_FILE, the name of the element file.
c
c    Input, integer NELEM, the number of elements.
c
c    Input, integer NODE(NPE,NELEM), the global node numbers in each element.
c
c    Input, integer NPE, the number of nodes per element.
c
      implicit none

      integer maxelm
      integer npe

      character*(*) element_file
      integer i
      integer ielem
      integer ierror
      integer ios
      integer nelem
      integer node(npe,maxelm)
      integer output_unit

      ierror = 0

      call get_unit ( output_unit )
c
c  Open the data file.
c
      open ( unit = output_unit, file = element_file, 
     &  status = 'unknown', iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'WRITE_ELEMENT - Fatal error!'
        write ( *, * ) '  Could not open the element file: ',
     &    element_file
        return
      end if
c
c  Number of elements.
c
      write ( output_unit, '(i6)' ) nelem
c
c  Number of nodes per element.
c
      write ( output_unit, '(i6)' ) npe
c
c  Node numbers associated with each element.
c
      do ielem = 1, nelem
        write ( output_unit, '(6i6)' ) ( node(i,ielem), i = 1, npe )
      end do

      close ( unit = output_unit )

      write ( *, * ) ' '
      write ( *, * ) 'WRITE_ELEMENT:'
      write ( *, * ) '  The element data was written to the file: ',
     & element_file

      return
      end
      subroutine write_node ( node_file, maxnp, np, maxnq, nq, v, 
     &  names )

c*********************************************************************72
c
cc WRITE_NODE writes a node data file.
c
c  Discussion:
c
c    The node file contains the value of various quantities at the nodes.
c    The format is:
c
c      Line 1: NP, the number of nodes
c      Line 2: NQ, the number of values per node, (X, Y and the quantities).
c      Line 3: NAMES, the names of X, Y, and the quantities
c      Lines 4 through NP+3: X, Y, and the quantities for each node.
c
c    Blanks and comment lines, which have a "#" in column 1, may occur anywhere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 June 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) NODE_FILE, the node file.
c
c    Input, integer NP, the number of nodes.
c
c    Input, integer NQ, the number of quantities (including X and Y).
c
c    Input, real V(NP,NQ), X, Y, and various quantities associated with
c    the nodes.
c
c    Input, character ( len = 14 * NQ ) NAMES, names for the quantities.
c
      implicit none

      integer maxnp
      integer maxnq

      integer i
      integer ios
      integer j
      character*(*) names
      character*(*) node_file
      integer np
      integer nq
      integer output_unit
      real v(maxnp,maxnq)

      call get_unit ( output_unit )

      open ( unit = output_unit, file = node_file, 
     &  status = 'unknown', iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'WRITE_NODE - Fatal error!'
        write ( *, * ) '  Could not open the node data file:', node_file
        return
      end if

      do i = 1, np
        write ( output_unit, '(12g14.6)' ) ( v(i,j), j = 1, nq )
      end do

      close ( unit = output_unit )

      write ( *, * ) ' '
      write ( *, * ) 'WRITE_NODE:'
      write ( *, * ) '  The node data was written to the file: ',
     &  node_file

      return
      end
