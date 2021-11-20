      subroutine cc_data_read ( prefix, ncc, n, icc, ccc, acc )

c*********************************************************************72
c
cc CC_DATA_READ reads data about a sparse matrix in CC format.
c
c  Discussion:
c
c    Three files are presumed to exist:
c    * prefix_icc.txt contains NCC ICC values;
c    * prefix_ccc.txt contains N+1 CCC values;
c    * prefix_acc.txt contains NCC ACC values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) PREFIX, a common prefix for the filenames.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Output, integer ICC(NCC), the CC rows.
c
c    Output, integer CCC(N+1), the compressed CC columns.
c
c    Output, double precision ACC(NCC), the CC values.
c
      implicit none

      integer n
      integer ncc

      double precision acc(ncc)
      integer ccc(n+1)
      character * ( 255 ) filename_acc
      character * ( 255 ) filename_ccc
      character * ( 255 ) filename_icc
      integer icc(ncc)
      character * ( * ) prefix

      filename_icc = trim ( prefix ) // "_icc.txt"
      call i4vec_data_read ( filename_icc, ncc, icc )

      filename_ccc = trim ( prefix ) // "_ccc.txt"
      call i4vec_data_read ( filename_ccc, n + 1, ccc )

      filename_acc = trim ( prefix ) // "_acc.txt"
      call r8vec_data_read ( filename_acc, ncc, acc )

      return
      end
      subroutine cc_header_read ( prefix, ncc, n )

c*********************************************************************72
c
cc CC_HEADER_READ reads header information about a sparse matrix in CC format.
c
c  Discussion:
c
c    Three files are presumed to exist:
c    * prefix_icc.txt contains NCC ICC values;
c    * prefix_ccc.txt contains N+1 CCC values;
c    * prefix_acc.txt contains NCC ACC values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) PREFIX, a common prefix for the filenames.
c
c    Output, integer NCC, the number of CC elements.
c
c    Output, integer N, the number of columns in the matrix.
c
      implicit none

      character * ( 255 ) filename_ccc
      character * ( 255 ) filename_icc
      integer n
      integer ncc
      character * ( * ) prefix

      filename_icc = trim ( prefix ) // "_icc.txt"
      call file_row_count ( filename_icc, ncc )

      filename_ccc = trim ( prefix ) // "_ccc.txt"
      call file_row_count ( filename_ccc, n )
      n = n - 1

      return
      end
      subroutine cc_mv ( m, n, ncc, icc, ccc, acc, x, b )

c*********************************************************************72
c
cc CC_MV multiplies a CC matrix by a vector
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    October 1992
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NCC, the number of CC values.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns
c
c    Input, double precision ACC(NCC), the CC values.
c
c    Input, double precision X(N), the vector to be multiplied.
c
c    Output, double precision B(M), the product A*X.
c
      implicit none

      integer m
      integer n
      integer ncc

      double precision acc(ncc)
      double precision b(m)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer k
      double precision x(n)

      do i = 1, m
        b(i) = 0.0D+00
      end do

      do j = 1, n
        do k = ccc(j), ccc(j+1) - 1
          i = icc(k)
          b(i) = b(i) + acc(k) * x(j)
        end do
      end do

      return
      end
      subroutine cc_print ( m, n, ncc, icc, ccc, acc, title )

c*********************************************************************72
c
cc CC_PRINT prints a sparse matrix in CC format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in the matrix.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns.
c
c    Input, double precision ACC(NCC), the CC values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n
      integer ncc

      double precision acc(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer k
      integer m
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J         A'
      write ( *, '(a)' ) '  ----  ----  ----  ----------------'
      write ( *, '(a)' ) ' '

      if ( ccc(1) .eq. 0 ) then

        j = 0
        do k = 1, ncc
          i = icc(k)
10        continue
          if ( ccc(j+2) .le. k - 1 ) then
            j = j + 1
            go to 10
          end if
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) 
     &      k - 1, i, j, acc(k)
        end do

      else

        j = 1
        do k = 1, ncc
          i = icc(k)
20        continue
          if ( ccc(j+1) .le. k ) then
            j = j + 1
            go to 20
          end if
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k, i, j, acc(k)
        end do

      end if

      return
      end
      subroutine cc_print_some ( i_min, i_max, j_min, j_max, ncc, n, 
     &  icc, ccc, acc, title )

c*********************************************************************72
c
cc CC_PRINT_SOME prints some of a sparse matrix in CC format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I_MIN, IMAX, the first and last rows to print.
c
c    Input, integer J_MIN, J_MAX, the first and last columns 
c    to print.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer N, the number of columns.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns.
c
c    Input, double precision ACC(NCC), the CC values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n
      integer ncc

      double precision acc(ncc)
      integer ccc(n+1)
      integer i
      integer i_max
      integer i_min
      integer icc(ncc)
      integer j
      integer j_max
      integer j_min
      integer jnext
      integer k
      character * ( * ) title

      subroutine cc_print ( m, n, ncc, icc, ccc, acc, title )

c*********************************************************************72
c
cc CC_PRINT prints a sparse matrix in CC format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in the matrix.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns.
c
c    Input, double precision ACC(NCC), the CC values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n
      integer ncc

      double precision acc(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer k
      integer m
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J         A'
      write ( *, '(a)' ) '  ----  ----  ----  ----------------'
      write ( *, '(a)' ) ' '

      if ( ccc(1) .eq. 0 ) then

        j = 0

        do k = 1, ncc

          i = icc(k)

10        continue
          if ( ccc(j+2) .le. k - 1 ) then
            j = j + 1
            go to 10
          end if

          if ( i_min .le. i .and. i .le. i_max .and. 
     &         j_min .le. j .and. j .le. j_max ) then
            write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) 
     &        k - 1, i, j, acc(k)
          end if

        end do

      else

        j = 1
        do k = 1, ncc

          i = icc(k)

20        continue
          if ( ccc(j+1) .le. k ) then
            j = j + 1
            go to 20
          end if

          if ( i_min .le. i .and. i .le. i_max .and. 
     &         j_min .le. j .and. j .le. j_max ) then
            write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k, i, j, acc(k)
          end if

        end do

      end if

      return
      end
      subroutine cc_write ( prefix, ncc, n, icc, ccc, acc )

c*********************************************************************72
c
cc CC_WRITE writes a sparse matrix in CC format to 3 files.
c
c  Discussion:
c
c    Three files will be created:
c    * prefix_icc.txt contains NCC ICC values;
c    * prefix_ccc.txt contains N+1 CCC values;
c    * prefix_acc.txt contains NCC ACC values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) PREFIX, a common prefix for the filenames.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns.
c
c    Input, double precision ACC(NCC), the CC values.
c
      implicit none

      integer n
      integer ncc

      double precision acc(ncc)
      integer ccc(n+1)
      character * ( 255 ) filename_acc
      character * ( 255 ) filename_ccc
      character * ( 255 ) filename_icc
      character * ( * ) prefix
      integer icc(ncc)

      filename_icc = trim ( prefix ) // "_icc.txt"
      call i4vec_write ( filename_icc, ncc, icc )

      filename_ccc = trim ( prefix ) // "_ccc.txt"
      call i4vec_write ( filename_ccc, n + 1, ccc )

      filename_acc = trim ( prefix ) // "_acc.txt"
      call r8vec_write ( filename_acc, ncc, acc )

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
      subroutine i4vec_data_read ( input_filename, n, table )

c*********************************************************************72
c
cc I4VEC_DATA_READ reads data from an I4VEC file.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
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
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Input, integer N, the number of points.
c
c    Output, integer TABLE(N), the data.
c
      implicit none

      integer n

      integer i
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      integer j
      integer length
      character * ( 255 ) line
      integer table(n)
      integer x

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

        call s_to_i4 ( line, x, ierror, length )

        if ( ierror .ne. 0 ) then
          go to 10
        end if

        j = j + 1

        table(j) = x

        go to 10

      end if

      close ( unit = input_unit )

      return
      end
      subroutine i4vec_dec ( n, x )

c*********************************************************************72
c
cc I4VEC_DEC decrements an I4VEC.
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
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the array.
c
c    Input/output, integer X(N), the array to be decremented.
c
      implicit none

      integer n

      integer i
      integer x(n)

      do i = 1, n
        x(i) = x(i) - 1
      end do

      return
      end
      subroutine i4vec_inc ( n, x )

c*********************************************************************72
c
cc I4VEC_INC increments an I4VEC.
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
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the array.
c
c    Input/output, integer X(N), the array to be incremented.
c
      implicit none

      integer n

      integer i
      integer x(n)

      do i = 1, n
        x(i) = x(i) + 1
      end do

      return
      end
      subroutine i4vec_write ( output_filename, n, x )

c*********************************************************************72
c
cc I4VEC_WRITE writes an I4VEC file.
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
c    12 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer N, the number of points.
c
c    Input, integer X(N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      integer x(n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. n ) then
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, '(i8)' ) x(j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine r8vec_data_read ( input_filename, n, table )

c*********************************************************************72
c
cc R8VEC_DATA_READ reads data from an R8VEC file.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
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
c    11 June 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Input, integer N, the number of points.
c
c    Output, double precision TABLE(N), the data.
c
      implicit none

      integer n

      integer i
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      integer j
      integer length
      character * ( 255 ) line
      double precision table(n)
      double precision x

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

        call s_to_r8 ( line, x, ierror, length )

        if ( ierror .ne. 0 ) then
          go to 10
        end if

        j = j + 1

        table(j) = x

        go to 10

      end if

      close ( unit = input_unit )

      return
      end
      subroutine r8vec_write ( output_filename, n, x )

c*********************************************************************72
c
cc R8VEC_WRITE writes an R8VEC file.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 July 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      double precision x(n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. n ) then
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, '(2x,g24.16)' ) x(j)
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
