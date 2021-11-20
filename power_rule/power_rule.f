      program main

c*********************************************************************72
c
cc MAIN is the main program for POWER_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer arg_num
      integer dim_num
      integer dim_num_1d
      integer iarg
      integer iargc
      integer ierror
      integer last
      integer point_num
      integer point_num_1d
      integer point_num_1d2
      character * ( 255 ) quad_1d_filename
      character * ( 255 ) quad_filename
      character * ( 255 ) quad_r_filename
      character * ( 255 ) quad_r_1d_filename
      character * ( 255 ) quad_w_filename
      character * ( 255 ) quad_w_1d_filename
      character * ( 255 ) quad_x_filename
      character * ( 255 ) quad_x_1d_filename
      double precision, allocatable, dimension ( :, : ) :: r
      double precision, allocatable, dimension ( : ) :: r_1d
      character * ( 255 ) string
      double precision, allocatable, dimension ( : ) :: w
      double precision, allocatable, dimension ( : ) :: w_1d
      double precision, allocatable, dimension ( :, : ) :: x
      double precision, allocatable, dimension ( : ) :: x_1d

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POWER_RULE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Create a multidimensional power rule'
      write ( *, '(a)' ) 
     &  '  as a product of identical 1D integration rules.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the 1D quadrature file root name:
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, quad_1d_filename )
c
c  A commandline argument can't be blank.  To let a user
c  specify the shortcut names, let "D" indicate them.
c
        if ( quad_1d_filename .eq. 'D' ) then
          quad_1d_filename = ''
        end if

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE:'
        write ( *, '(a)' ) 
     &    '  Enter the "root" name of the 1D quadrature files.'

        read ( *, '(a)' ) quad_1d_filename

      end if
c
c  Create the names of:
c    the 1D quadrature X file;
c    the 1D quadrature W file;
c    the 1D quadrature R file;
c
      if ( 0 .lt. len_trim ( quad_1d_filename ) ) then
        quad_x_1d_filename = trim ( quad_1d_filename ) // '_x.txt'
        quad_w_1d_filename = trim ( quad_1d_filename ) // '_w.txt'
        quad_r_1d_filename = trim ( quad_1d_filename ) // '_r.txt'
      else
        quad_x_1d_filename = 'x.txt'
        quad_w_1d_filename = 'w.txt'
        quad_r_1d_filename = 'r.txt'
      end if
c
c  The second command line argument is the spatial dimension.
c
      if ( 2 .le. arg_num ) then

        iarg = 2
        call getarg ( iarg, string )
        call s_to_i4 ( string, dim_num, ierror, last )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE:'
        write ( *, '(a)' ) 
     &    '  Please enter the desired spatial dimension of the rule.'

        read ( *, * ) dim_num

      end if
c
c  Summarize the input.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POWER_RULE: User input:'
      write ( *, '(a)' ) '  Quadrature rule X file = "' 
     &  // trim ( quad_x_1d_filename) // '".'
      write ( *, '(a)' ) '  Quadrature rule W file = "' 
     &  // trim ( quad_w_1d_filename) // '".'
      write ( *, '(a)' ) '  Quadrature rule R file = "' 
     &  // trim ( quad_r_1d_filename) // '".'
      write ( *, '(a,i8)' ) '  Spatial dimension = ', dim_num
c
c  Read the X file.
c
      call r8mat_header_read ( quad_x_1d_filename, dim_num_1d, 
     &  point_num_1d )

      if ( dim_num_1d .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The 1D quadrature abscissa file should have'
        write ( *, '(a)' ) '  exactly one value on each line.'
        stop 1
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of points in 1D rule = ', point_num_1d
c
c  Determine size of rule.
c
      call power_rule_size ( point_num_1d, dim_num, point_num )

      write ( *, '(a,i8)' ) 
     &  '  Number of points in power rule = ', point_num
c
c  Call a subroutine, so that we can allocate memory.
c  Otherwise, FORTRAN77 may be unable to allocate memory for us.
c
      call main_sub ( dim_num, point_num, point_num_1d,
     &  quad_1d_filename, quad_filename, quad_r_filename,
     &  quad_r_1d_filename, quad_w_filename, quad_w_1d_filename,
     &  quad_x_filename, quad_x_1d_filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POWER_RULE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine main_sub ( dim_num, point_num, point_num_1d,
     &  quad_1d_filename, quad_filename, quad_r_filename,
     &  quad_r_1d_filename, quad_w_filename, quad_w_1d_filename,
     &  quad_x_filename, quad_x_1d_filename )

c*********************************************************************72
c
cc MAIN_SUB is a part of the main program using allocated memory.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      integer point_num
      integer point_num_1d

      integer dim_num_1d
      integer point_num_1d2
      character * ( 255 ) quad_1d_filename
      character * ( 255 ) quad_filename
      character * ( 255 ) quad_r_filename
      character * ( 255 ) quad_r_1d_filename
      character * ( 255 ) quad_w_filename
      character * ( 255 ) quad_w_1d_filename
      character * ( 255 ) quad_x_filename
      character * ( 255 ) quad_x_1d_filename
      double precision r(dim_num,2)
      double precision r_1d(2)
      double precision w(point_num)
      double precision w_1d(point_num_1d)
      double precision x(dim_num,point_num)
      double precision x_1d(point_num_1d)

      call r8mat_data_read ( quad_x_1d_filename, dim_num_1d, 
     &  point_num_1d, x_1d )
c
c  Read the W file.
c
      call r8mat_header_read ( quad_w_1d_filename, dim_num_1d, 
     &  point_num_1d2 )

      if ( dim_num_1d .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE - Fatal error!'
        write ( *, '(a)' ) '  The 1D quadrature weight file should have'
        write ( *, '(a)' ) '  exactly one value on each line.'
        stop 1
      end if

      if ( point_num_1d2 .ne. point_num_1d ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE - Fatal error!'
        write ( *, '(a)' ) '  The 1D quadrature weight file should have'
        write ( *, '(a)' ) 
     &    '  the same number of lines as the abscissa file.'
        stop 1
      end if

      call r8mat_data_read ( quad_w_1d_filename, dim_num_1d, 
     &  point_num_1d, w_1d )
c
c  Read the R file.
c
      call r8mat_header_read ( quad_r_1d_filename, dim_num_1d, 
     &  point_num_1d2 )

      if ( dim_num_1d .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE - Fatal error!'
        write ( *, '(a)' ) '  The 1D quadrature region file should have'
        write ( *, '(a)' ) '  exactly one value on each line.'
        stop 1
      end if

      if ( point_num_1d2 .ne. 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POWER_RULE - Fatal error!'
        write ( *, '(a)' ) '  The 1D quadrature region file should have'
        write ( *, '(a)' ) '  exactly two lines.'
        stop 1
      end if

      call r8mat_data_read ( quad_r_1d_filename, 1, 2, r_1d )
c
c  Compute the rule.
c
      call power_rule_set ( point_num_1d, x_1d, w_1d, r_1d, 
     &  dim_num, point_num, x, w, r )
c
c  Write rule to files.
c
      quad_filename = 'power'

      quad_x_filename = trim ( quad_filename ) // '_x.txt'
      quad_w_filename = trim ( quad_filename ) // '_w.txt'
      quad_r_filename = trim ( quad_filename ) // '_r.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Creating quadrature rule X file = "' 
     &  // trim ( quad_x_filename ) // '".'

      call r8mat_write ( quad_x_filename, dim_num, point_num, x )

      write ( *, '(a)' ) '  Creating quadrature rule W file = "' 
     &  // trim ( quad_w_filename ) // '".'

      call r8mat_write ( quad_w_filename, 1, point_num, w )
     
      write ( *, '(a)' ) '  Creating quadrature rule R file = "' 
     &  // trim ( quad_r_filename ) // '".'

      call r8mat_write ( quad_r_filename, dim_num, 2, r )

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
      subroutine power_rule_set ( point_num_1d, x_1d, w_1d, r_1d, 
     &  dim_num, point_num, x, w, r )

c*********************************************************************72
c
cc POWER_RULE_SET sets up a power rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer POINT_NUM_1D, the order of the 1D rule.
c
c    Input, double precision X_1D(POINT_NUM_1D), the points of the 1D rule.
c
c    Input, double precision W_1D(POINT_NUM_1D), the weights of the
c    1D rule.
c
c    Input, double precision R_1D(2), the extreme points that define
c    the range of the 1D region.
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer POINT_NUM, the number of points in the rule.
c
c    Output, double precision X(DIM_NUM,POINT_NUM), the points of the rule.
c
c    Output, double precision W(POINT_NUM), the weights of the rule.
c
c    Output, double precision R(DIM_NUM,2), the extreme points 
c    that define the range of the product rule region.
c
      implicit none

      integer dim_num
      integer point_num
      integer point_num_1d

      integer i
      integer indx(dim_num)
      integer k
      double precision r(dim_num,2)
      double precision r_1d(2)
      double precision w(point_num)
      double precision w_1d(point_num_1d)
      double precision x(dim_num,point_num)
      double precision x_1d(point_num_1d)

      k = 0
      
10    continue

        call tuple_next ( 1, point_num_1d, dim_num, k, indx )

        if ( k .eq. 0  ) then
          go to 20
        end if

        w(k) = 1.0D+00
        do i = 1, dim_num
          w(k) = w(k) * w_1d(indx(i))
        end do

        do i = 1, dim_num
          x(i,k) = x_1d(indx(i))
        end do

      go to 10

20    continue

      do i = 1, dim_num
        r(i,1) = r_1d(1)
        r(i,2) = r_1d(2)
      end do

      return
      end
      subroutine power_rule_size ( point_num_1d, dim_num, point_num )

c*********************************************************************72
c
cc POWER_RULE_SIZE returns the size of a power rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer POINT_NUM_1D, the number of points in the 
c    1D rule.
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Output, integer POINT_NUM, the number of points in the rule.
c
      implicit none

      integer dim_num
      integer point_num
      integer point_num_1d

      point_num = point_num_1d ** dim_num

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

      integer i
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
          write ( output_unit, string ) ( table(i,j), i = 1, m )
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

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

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, len_trim ( s )

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
      subroutine tuple_next ( m1, m2, n, rank, x )

c*********************************************************************72
c
cc TUPLE_NEXT computes the next element of a tuple space.
c
c  Discussion:
c
c    The elements are N vectors.  Each entry is constrained to lie
c    between M1 and M2.  The elements are produced one at a time.
c    The first element is
c      (M1,M1,...,M1),
c    the second element is
c      (M1,M1,...,M1+1),
c    and the last element is
c      (M2,M2,...,M2)
c    Intermediate elements are produced in lexicographic order.
c
c  Example:
c
c    N = 2, M1 = 1, M2 = 3
c
c    INPUT        OUTPUT
c    -------      -------
c    Rank  X      Rank   X
c    ----  ---    -----  --- 
c    0     * *    1      1 1
c    1     1 1    2      1 2
c    2     1 2    3      1 3
c    3     1 3    4      2 1
c    4     2 1    5      2 2
c    5     2 2    6      2 3
c    6     2 3    7      3 1
c    7     3 1    8      3 2
c    8     3 2    9      3 3
c    9     3 3    0      0 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M1, M2, the minimum and maximum entries.
c
c    Input, integer N, the number of components.
c
c    Input/output, integer RANK, counts the elements.
c    On first call, set RANK to 0.  Thereafter, the output value of RANK
c    will indicate the order of the element returned.  When there are no 
c    more elements, RANK will be returned as 0.
c
c    Input/output, integer X(N), on input the previous tuple.
c    On output, the next tuple.
c
      implicit none

      integer n

      integer i
      integer m1
      integer m2
      integer rank
      integer x(n)

      if ( m2 .lt. m1 ) then
        rank = 0
        return
      end if

      if ( rank .le. 0 ) then

        do i = 1, n
          x(i) = m1
        end do
        rank = 1

      else

        rank = rank + 1
        i = n

10      continue

          if ( x(i) .lt. m2 ) then
            x(i) = x(i) + 1
            go to 20
          end if

          x(i) = m1

          if ( i .eq. 1 ) then
            rank = 0
            do i = 1, n
              x(i) = m1
            end do
            go to 20
          end if

          i = i - 1

        go to 10

20      continue

      end if

      return
      end