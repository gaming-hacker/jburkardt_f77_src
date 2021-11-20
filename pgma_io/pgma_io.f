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
      subroutine getint ( done, ierror, inunit, ival, string )

c*********************************************************************72
c
cc GETINT reads an integer from a file.
c
c  Discussion:
c
c    The file, or at least the part read by GETINT, is assumed to
c    contain nothing but integers.  These integers may be separated
c    by spaces, or appear on separate lines.  Comments, which begin
c    with "#" and extend to the end of the line, may appear anywhere.
c
c    Each time GETINT is called, it tries to read the next integer
c    it can find.  It remembers where it was in the current line
c    of text.
c
c    The user should open a text file on FORTRAN unit INUNIT,
c    set STRING = ' ' and DONE = TRUE.  The GETINT routine will take
c    care of reading in a new STRING as necessary, and extracting
c    as many integers as possible from the line of text before 
c    reading in the next line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, logical DONE.
c    On input, if this is the first call, or the user has changed
c    STRING, then set DONE = TRUE.
c    On output, if there is no more data to be read from STRING,
c    then DONE is TRUE.
c
c    Output, integer IERROR, error flag.
c    0, no error occurred.
c    1, an error occurred while trying to read the integer.
c
c    Input, integer INUNIT, the FORTRAN unit from which to read.
c
c    Output, integer IVAL, the integer that was read.
c
c    Input/output, character * ( * ) STRING, the text of the most recently 
c    read line of the file.
c
      implicit none

      logical done
      integer i
      integer ierror
      integer inunit
      integer ios
      integer ival
      integer last
      character * ( * )  string
      character * ( 80 ) word

10    continue

        call word_next_rd ( string, word, done )

        if ( .not. done ) then
          go to 20
        end if

        read ( inunit, '(a)', err = 30 ) string

        i = index ( string, '#' )

        if ( i .ne. 0 ) then
          string(i:) = ' '
        end if

      go to 10

20    continue

      call s_to_i4 ( word, ival, ierror, last )

      if ( ierror .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GETINT - Fatal error!'
        write ( *, '(a)' ) '  Error converting string to integer.'
        stop
      end if

      return

30    continue

      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GETINT - Fatal error!'
      write ( *, '(a)' ) '  Error reading string from file.'
      stop
      end
      subroutine pgma_check_data ( row_num, col_num, g_max, g, ierror )

c*********************************************************************72
c
cc PGMA_CHECK_DATA checks ASCII PGM data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ROW_NUM,  COL_NUM, the number of rows 
c    and columns of data.
c
c    Input, integer G_MAX, the maximum gray value.
c
c    Input, integer G(ROW_NUM,COL_NUM), contains the gray data.
c
c    Output, integer IERROR, error flag.
c    0, no error detected.
c    1, the data is illegal.
c
      implicit none

      integer col_num
      integer row_num

      integer g(row_num,col_num)
      integer i
      integer ierror
      integer j 
      integer g_max

      ierror = 0

      do j = 1, col_num
        do i = 1, row_num

          if ( g(i,j) .lt. 0 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'PGMA_CHECK_DATA - Fatal error!'
            write ( *, '(a)' ) '  G(I,J) < 0.'
            ierror = 1
            stop
          end if

          if ( g_max .lt. g(i,j) ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'PGMA_CHECK_DATA - Fatal error!'
            write ( *, '(a)' ) '  G_MAX < G(I,J).'
            ierror = 1
            stop
          end if

        end do
      end do

      return
      end
      subroutine pgma_example ( row_num, col_num, g )

c*********************************************************************72
c
cc PGMA_EXAMPLE sets up sample ASCII PGM data.
c
c  Discussion:
c
c    The data is based on three periods of a sine curve.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ROW_NUM, COL_NUM, the number of rows and 
c    columns of data.  A reasonable value is 200 for ROW_NUM and 600 
c    for COL_NUM.
c
c    Output, integer G(ROW_NUM,COL_NUM), the gray scale data.
c
      implicit none

      integer row_num
      integer col_num

      integer g(row_num,col_num)
      integer i
      integer j
      integer periods
      parameter ( periods = 3 )
      double precision pi
      parameter ( pi = 3.14159265D+00 )
      double precision x
      double precision y

      do i = 1, row_num
        y = 2.0D+00 * dble ( i - 1 ) 
     &    / dble ( row_num - 1 ) - 1.0D+00
        do j = 1, col_num
          x = 2.0D+00 * pi * dble ( periods * ( j - 1 ) ) 
     &      / dble ( col_num - 1 )
          g(i,j) = int ( 20.0D+00 * ( sin ( x ) - y + 2.0D+00 ) )
        end do
      end do

      return
      end
      subroutine pgma_read_data ( file_in_unit, row_num, col_num, g )

c*********************************************************************72
c
cc PGMA_READ_DATA reads the data in an ASCII PGM file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    04 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FILE_IN_UNIT, the unit number of the file.
c
c    Input, integer ROW_NUM, COL_NUM, the number of rows and 
c    columns of data.
c
c    Output, integer G(ROW_NUM,COL_NUM), the gray data.
c
      implicit none

      integer col_num
      integer row_num

      logical done
      integer file_in_unit
      integer g(row_num,col_num)
      integer i
      integer ierror
      integer j
      character ( len = 80 ) string

      ierror = 0
      done = .true.
      string = ' '

      do i = 1, row_num
        do j = 1, col_num

          call getint ( done, ierror, file_in_unit, g(i,j), string )

          if ( ierror .ne. 0 ) then
            ierror = 5
            close ( unit = file_in_unit )
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'PGMA_READ_DATA - Fatal error!'
            write ( *, '(a)' ) '  Problem reading G data.'
            stop
          end if

        end do
      end do

      return
      end
      subroutine pgma_read_header ( file_in_unit, row_num, col_num, 
     &  g_max )

c*********************************************************************72
c
cc PGMA_READ_HEADER reads the header of an ASCII PGM file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    04 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FILE_IN_UNIT, the unit number of the file.
c
c    Output, integer ROW_NUM, COL_NUM, the number of rows and 
c    columns of data.
c
c    Output, integer G_MAX, the maximum gray value.
c
      implicit none

      logical done
      integer file_in_unit
      integer ierror
      integer ios
      character ( 2 )  magic
      integer g_max
      integer col_num
      integer row_num
      logical s_eqi
      character ( len = 80 ) string
c
c  Read the first line of data, which must begin with the magic number.
c
      read ( file_in_unit, '(a)', err = 10 ) magic

      if ( .not. s_eqi ( magic, 'P2' ) ) then
        ierror = 3
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_HEADER - Fatal error.'
        write ( *, '(a)' ) '  First two bytes not magic number "P2".'
        write ( *, '(a)' ) '  First two bytes are: "' // magic // '".'
        stop
      end if
c
c  Now search for COL_NUM, ROW_NUM, and G_MAX.
c
      done = .true.
      string = ' '

      call getint ( done, ierror, file_in_unit, col_num, string )

      if ( ierror .ne. 0 ) then
        close ( unit = file_in_unit )
        ierror = 4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_HEADER - Fatal error!'
        write ( *, '(a)' ) '  Problem reading COL_NUM.'
        stop
      end if

      call getint ( done, ierror, file_in_unit, row_num, string )

      if ( ierror .ne. 0 ) then
        ierror = 4
        close ( unit = file_in_unit )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_HEADER - Fatal error!'
        write ( *, '(a)' ) '  Problem reading ROW_NUM.'
        stop
      end if

      call getint ( done, ierror, file_in_unit, g_max, string )

      if ( ierror .ne. 0 ) then
        ierror = 4
        close ( unit = file_in_unit )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_HEADER - Fatal error!'
        write ( *, '(a)' ) '  Problem reading G_MAX.'
        stop
      end if

      return

10    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PGMA_READ_HEADER - Fatal error!'
      write ( *, '(a)' ) '  End or error while reading file.'
      ierror = 2
      stop
      end
      subroutine pgma_read_test ( file_in_name, ierror )

c*********************************************************************72
c
cc PGMA_READ_TEST tests an ASCII PGM file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_IN_NAME, the name of the file 
c    containing the ASCII PGM data.
c
c    Output, integer IERROR, an error flag which is nonzero if
c    there was an error.
c
      implicit none

      integer length_max
      parameter ( length_max = 100000 )

      character * ( * ) file_in_name
      integer file_in_unit
      integer g(length_max)
      integer ierror
      integer ios
      integer g_max
      integer col_num
      integer row_num

      call get_unit ( file_in_unit )

      open ( unit = file_in_unit, file = file_in_name, status = 'old',
     &  err = 10 )
c
c  Read the header.
c
      call pgma_read_header ( file_in_unit, row_num, col_num, g_max )
c
c  Allocate the data.
c
      if ( length_max < row_num * col_num ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_TEST - Fatal error!'
        write ( *, '(a)' ) '  Not enough internal storage for data.'
        write ( *, '(a)' ) '  Increase LENGTH_MAX.'
        stop
      end if
c
c  Read the data.
c
      call pgma_read_data ( file_in_unit, row_num, col_num, g )

      close ( unit = file_in_unit )
c
c  Check the data.
c
      call pgma_check_data ( row_num, col_num, g_max, g, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_TEST - Warning!'
        write ( *, '(a)' ) '  PGMA_CHECK_DATA did not approve the data.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_TEST:'
        write ( *, '(a)' ) '  PGMA_CHECK_DATA approved the data.'
      end if

      return

10    continue

      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PGMA_READ_TEST - Fatal error!'
      write ( *, '(a)' ) '  Could not open the file.'

      stop
      end
      subroutine pgma_write ( file_out_name, row_num, col_num, g )

c*********************************************************************72
c
cc PGMA_WRITE writes an ASCII PGM file.
c
c  Example:
c
c    P2
c    # feep.pgm
c    24 7
c    15
c    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
c    0  3  3  3  3  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15 15 15 15  0
c    0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0 15  0
c    0  3  3  3  0  0  0  7  7  7  0  0  0 11 11 11  0  0  0 15 15 15 15  0
c    0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0  0  0
c    0  3  0  0  0  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15  0  0  0  0
c    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
c    
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_OUT_NAME, the name of the file 
c    to which the data should be written.
c
c    Input, integer ROW_NUM, COL_NUM, the number of rows and 
c    columns of data.
c
c    Input, integer G(ROW_NUM,COL_NUM), the gray value of each 
c    pixel.  These should be nonnegative.
c
      implicit none

      integer col_num
      integer row_num

      logical debug
      parameter ( debug = .false. )
      character * ( * ) file_out_name
      integer file_out_unit
      integer g(row_num,col_num)
      integer g_max
      integer i
      integer ios
      integer j
c
c  Compute the maximum color value.
c
      g_max = 0
      do j = 1, col_num
        do i = 1, row_num
          g_max = max ( g_max, g(i,j) )
        end do
      end do
c
c  Open the file.
c
      call get_unit ( file_out_unit )

      open ( unit = file_out_unit, file = file_out_name, 
     &  status = 'replace',  form = 'formatted', access = 'sequential', 
     &  err = 10 )
c
c  Write the header.
c
      call pgma_write_header ( file_out_name, file_out_unit, row_num, 
     &  col_num, g_max )
c
c  Write the data.
c
      call pgma_write_data ( file_out_unit, row_num, col_num, g )
c
c  Close the file.
c
      close ( unit = file_out_unit )
c
c  Report
c
      if ( debug ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_WRITE - Note:'
        write ( *, '(a)' ) '  The data was checked and written.'
        write ( *, '(a,i8)' ) 
     &    '  Number of data rows ROW_NUM =    ', row_num
        write ( *, '(a,i8)' ) 
     &    '  Number of data columns COL_NUM = ', col_num
        write ( *, '(a,i8)' ) 
     &    '  Maximum gray value G_MAX =       ', g_max
      end if

      return

10    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PGMA_WRITE - Fatal error!'
      write ( *, '(a)' ) '  Could not open the file.'
      stop
      end
      subroutine pgma_write_data ( file_out_unit, row_num, col_num, 
     &  g )

c*********************************************************************72
c
cc PGMA_WRITE_DATA writes the data of an ASCII PGM file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FILE_OUT_UNIT, the output file unit number.
c
c    Input, integer ROW_NUM, COL_NUM, the number of rows and 
c    columns of data.
c
c    Input, integer G(ROW_NUM,COL_NUM), the gray value of each 
c    pixel.  These should be nonnegative.
c
      implicit none

      integer col_num
      integer row_num

      integer file_out_unit
      integer g(row_num,col_num)
      integer i
      integer jhi
      integer jlo

      do i = 1, row_num
        do jlo = 1, col_num, 12
          jhi = min ( jlo + 11, col_num )
          write ( file_out_unit, '(12i5)' ) g(i,jlo:jhi)
        end do
      end do

      return
      end
      subroutine pgma_write_header ( file_out_name, file_out_unit, 
     &  row_num, col_num, g_max )

c*********************************************************************72
c
cc PGMA_WRITE_HEADER writes the header of an ASCII PGM file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_OUT_NAME, the name of the output file.
c
c    Input, integer FILE_OUT_UNIT, the output file unit number.
c
c    Input, integer ROW_NUM, COL_NUM, the number of rows and 
c    columns of data.
c
c    Input, integer G_MAX, the maximum gray value.
c
      implicit none

      character * ( * )  file_out_name
      integer file_out_unit
      integer ierror
      character ( 2 ) magic
      parameter ( magic = 'P2' )
      integer g_max
      integer col_num
      integer row_num
c
c  Write the header.
c
      write ( file_out_unit, '(a2)' ) magic
      write ( file_out_unit, '(a)' ) '# ' // trim ( file_out_name ) 
     &  // ' created by PGMA_IO::PGMA_WRITE.'
      write ( file_out_unit, '(i8,2x,i8)' ) col_num, row_num
      write ( file_out_unit, '(i8)' ) g_max

      return
      end
      subroutine pgma_write_test ( file_out_name )

c*********************************************************************72
c
cc PGMA_WRITE_TEST tests the ASCII PGM write routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_OUT_NAME, the name of the file 
c    to contain the ASCII PGM data.
c
      implicit none

      integer col_num
      parameter ( col_num = 300 )
      integer row_num
      parameter ( row_num = 300 )

      character * ( * )  file_out_name
      integer g(row_num,col_num)
c
c  Set the data.
c
      call pgma_example ( row_num, col_num, g )
c
c  Write the data to the file.
c
      call pgma_write ( file_out_name, row_num, col_num, g )

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
      subroutine word_next_rd ( line, word, done )

c*********************************************************************72
c
cc WORD_NEXT_RD "reads" words from a string, one at a time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) LINE, a string, presumably containing
c    words separated by spaces.
c
c    Output, character * ( * ) WORD.
c    If DONE is FALSE,
c      WORD contains the "next" word read from LINE.
c    Else
c      WORD is blank.
c
c    Input/output, logical DONE.
c    On input, on the first call, or with a fresh value of LINE,
c      set DONE to TRUE.
c    Else
c      leave it at the output value of the previous call.
c    On output, if a new nonblank word was extracted from LINE
c      DONE is FALSE
c    ELSE
c      DONE is TRUE.
c    If DONE is TRUE, then you need to provide a new LINE of data.
c
c  Local Parameters:
c
c    NEXT is the next location in LINE that should be searched.
c
      implicit none

      logical done
      integer ilo
      integer lenl
      character * ( * )  line
      integer next
      character TAB
      parameter ( TAB = char ( 9 ) )
      character * ( * )  word

      save next

      data next / 1 /

      lenl = len_trim ( line )

      if ( done ) then
        next = 1
        done = .false.
      end if
c
c  Beginning at index NEXT, search LINE for the next nonblank.
c
      ilo = next

10    continue
c
c  ...LINE(NEXT:LENL) is blank.  Return with WORD=' ', and DONE=TRUE.
c
        if ( lenl < ilo ) then
          word = ' '
          done = .true.
          next = lenl + 1
          return
        end if
c
c  ...If the current character is blank, skip to the next one.
c
        if ( line(ilo:ilo) .ne. ' ' .and. line(ilo:ilo) .ne. TAB ) then
          go to 20
        end if

        ilo = ilo + 1

      go to 10

20    continue
c
c  To get here, ILO must be the index of the nonblank starting
c  character of the next word.
c
c  Now search for the LAST nonblank character.
c
      next = ilo + 1

30    continue

        if ( lenl .lt. next ) then
          word = line(ilo:next-1)
          return
        end if

        if ( line(next:next) .eq. ' ' .or. 
     &       line(next:next) .eq. TAB ) then
          go to 40
        end if

        next = next + 1

      go to 30

40    continue

      word = line(ilo:next-1)

      return
      end
