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
      subroutine mm_comment_print ( comment )

c*********************************************************************72
c
cc MM_COMMENT_PRINT prints a comment from a Matrix Market file.
c
c  Discussion:
c
c    Comment lines begin with a '%' character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) COMMENT, the comment.
c
      implicit none

      character * ( * ) comment

      write ( *, '(a)' ) trim ( comment )

      return
      end
      subroutine mm_comment_read ( input_unit, comment )

c*********************************************************************72
c
cc MM_COMMENT_READ reads a comment from a Matrix Market file.
c
c  Discussion:
c
c    This routine simply reads one line from the file.  Comment
c    lines begin with a '%' character, and must occur between the
c    header line and the size line.  It is up to the user to
c    examine the line returned by this routine, and, if it does not
c    begin with a comment character, then the user may do any of:
c    *) terminate the processing, because you simply wanted to see comments;
c    *) backspace the file, so the line can be processed by MM_SIZE_READ_FILE;
c    *) simply pass the line returned by this routine directly
c       to MM_SIZE_READ_STRING.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT_UNIT, the input unit identifier.
c
c    Output, character * ( * ) COMMENT, the next line of the file.
c
      implicit none

      character * ( * ) comment
      integer input_unit
      integer ios

      read ( input_unit, '(a)', iostat = ios ) comment

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_COMMENT_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Unexpected end of file while reading comments.'
        stop 1
      end if

      return
      end
      subroutine mm_comment_write ( output_unit, comment )

c*********************************************************************72
c
cc MM_COMMENT_WRITE writes a comment to a Matrix Market file.
c
c  Discussion:
c
c    Comments may be written AFTER the header line and BEFORE the size line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer OUTPUT_UNIT, the output unit identifier.
c
c    Input, character * ( * ) COMMENT, the comment to be written.
c    This routine will prepend a "%  " comment marker to the string.
c
      implicit none

      character * ( * ) comment
      integer output_unit

      if ( len_trim ( comment ) .eq. 0 ) then
        write ( output_unit, '(a)' ) '%'
      else
        write ( output_unit, '(a)' ) '%  ' // trim ( comment )
      end if

      return
      end
      subroutine mm_file_read ( input_unit, id, type, rep, field, symm, 
     &  nrow, ncol, nnz, nnzmax, indx, jndx, ival, rval, dval, cval )

c*********************************************************************72
c
cc MM_FILE_READ reads data from a Matrix Market file.
c
c  Discussion:
c
c    The data may be either sparse coordinate format, or dense array format.
c
c    The unit input_unit must be open, and the file will be rewound on return.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    Original FORTRAN77 version by Karin A. Remington, NIST ACMD
c    This FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT_UNIT, the input unit identifier.
c
c    Output, character * ( 14 ) ID, the Matrix Market identifier.
c    This value must be '%%MatrixMarket'.
c
c    Output, character * ( 6 ) TYPE, the Matrix Market type.
c    This value must be 'matrix'.
c
c    Output, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Output, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Output, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Output, integer NROW, the number of rows in the matrix.
c
c    Output, integer NCOL, the number of columns in the matrix.
c
c    Output, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
c    Input, integer NNZMAX, the maximum dimension of the 
c    appropriate data array.
c
c    Output, integer INDX(NNZ), the row indices for coordinate 
c    format.  Not used if REP is 'array'.
c
c    Output, integer JNDX(NNZ), the column indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Output, integer IVAL(NNZ), matrix values, if FIELD 
c    is 'integer'.
c
c    Output, real RVAL(NNZ), matrix values, if FIELD is 'real'.
c
c    Output, double precision DVAL(NNZ), matrix values, if FIELD is 'double'.
c
c    Output, complex CVAL(NNZ), matrix values, if FIELD 
c    is 'complex'.
c
      implicit none

      complex cval(*)
      double precision dval(*)
      character * ( 7 ) field
      character * ( 14 ) id
      integer indx(*)
      integer input_unit
      integer ival(*)
      integer jndx(*)
      integer ncol
      integer nnz
      integer nnzmax
      integer nrow
      character * ( 10 ) rep
      real rval(*)
      character * ( 19 ) symm
      character * ( 1024 ) tmp1
      character * ( 6 ) type
c
c  Read and check the header line.
c
      call mm_header_read ( input_unit, id, type, rep, field, symm )

      call mm_header_check ( id, type, rep, field, symm )
c
c  Read through the comment lines:
c
10    continue

        call mm_comment_read ( input_unit, tmp1 )

        if ( tmp1(1:1) .ne. '%' ) then
          go to 20
        end if

      go to 10

20    continue
c
c  Current line is not a comment.
c
c  We can either process the string directly by MM_SIZE_READ_STRING,
c  or backspace the file and have MM_SIZE_READ_FILE handle it.
c
      call mm_size_read_string ( tmp1, rep, symm, nrow, ncol, nnz )
c
c  Make sure there is enough storage space.
c
      if ( nnzmax .lt. nnz ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_FILE_READ - Fatal error!'
        write ( *, '(a)' ) '  Insufficient memory to read matrix.'
        write ( *, '(a)' ) '  Number of entries to be read = ', nnz
        write ( *, '(a)' ) '  Number of entries supplied = ',nnzmax
        stop 1
      end if
c
c  Read the data values.
c
      call mm_values_read ( input_unit, rep, field, nnz, indx, jndx, 
     &  ival, rval, dval, cval )

      return
      end
      subroutine mm_file_write ( output_unit, id, type, rep, field, 
     &  symm, nrow, ncol, nnz, indx, jndx, ival, rval, dval, cval )

c*********************************************************************72
c
cc MM_FILE_WRITE writes data to a Matrix Market file.
c
c  Discussion:
c
c    The data may be either sparse coordinate format, or dense array format.
c
c    The unit OUTPUT_UNIT must be open.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    Original FORTRAN77 version by Karin A. Remington, NIST ACMD
c    This FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer OUTPUT_UNIT, the output unit identifier.
c
c    Input, character * ( 14 ) ID, the Matrix Market identifier.
c    This value must be '%%MatrixMarket'.
c
c    Input, character * ( 6 ) TYPE, the Matrix Market type.
c    This value must be 'matrix'.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern' (for REP = 'coordinate' only)
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Input, integer NROW, the number of rows in the matrix.
c
c    Input, integer NCOL, the number of columns in the matrix.
c
c    Input, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
c    Input, integer INDX(NNZ), the row indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer JNDX(NNZ), the column indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer IVAL(NNZ), matrix values, if FIELD is
c    'integer'.
c
c    Input, real RVAL(NNZ), matrix values, if FIELD is 'real'.
c
c    Input, double precision DVAL(NNZ), matrix values, if FIELD is 'double'.
c
c    Input, complex CVAL(NNZ), matrix values, if FIELD
c    is 'complex'.
c
      implicit none

      complex cval(*)
      double precision dval(*)
      character * ( * ) field
      character * ( 14 ) id
      integer indx(*)
      integer ival(*)
      integer jndx(*)
      integer ncol
      integer nnz
      integer nnz2
      integer nrow
      integer output_unit
      character * ( * ) rep
      real rval(*)
      logical s_eqi
      character * ( * ) symm
      character * ( 6 ) type
c
c  Test the header values.
c
      call mm_header_check ( id, type, rep, field, symm )
c
c  Write the header line.
c
      call mm_header_write ( output_unit, id, type, rep, field, symm )
c
c  Write some comment lines.
c
      call mm_comment_write ( output_unit, ' ' )
      call mm_comment_write ( output_unit, 
     &  'This file created by MM_FILE_WRITE of MM_IO.F90.' )
      call mm_comment_write ( output_unit, ' ' )
c
c  Write the size line.
c
      call mm_size_write ( output_unit, rep, nrow, ncol, nnz )
c
c  Determine NNZ where necessary.
c
      call mm_nnz_set ( rep, symm, nrow, ncol, nnz2 )
c
c  Write the data.
c
      if ( s_eqi ( rep, 'coordinate' ) ) then
        call mm_values_write ( output_unit, rep, field, nnz, indx, jndx, 
     &    ival, rval, dval, cval )
      else
        call mm_values_write ( output_unit, rep, field, nnz2, indx, 
     &    jndx, ival, rval, dval, cval )
      end if

      return
      end
      function mm_header_check ( id, type, rep, field, symm )

c*********************************************************************72
c
cc MM_HEADER_CHECK checks the header strings for a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( 14 ) ID, the Matrix Market identifier.
c    This value must be '%%MatrixMarket'.
c
c    Input, character * ( 6 ) TYPE, the Matrix Market type.
c    This value must be 'matrix'.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern' (for REP = 'coordinate' only)
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Output, logical MM_HEADER_CHECK, is TRUE if the header is OK.
c
      implicit none

      character * ( * ) field
      character * ( 14 ) id
      logical mm_header_check
      character * ( * ) rep
      logical s_eqi
      logical s_neqi
      character * ( * ) symm
      character * ( 6 ) type
c
c  Test the input qualifiers.
c
      if ( s_neqi ( id, '%%MatrixMarket' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_CHECK - Fatal error!'
        write ( *, '(a)' ) '  The value of ID was illegal:'
        write ( *, '(a)' ) '    "' // trim ( id ) // '".'
        write ( *, '(a)' ) '  Legal values are:'
        write ( *, '(a)' ) '    "%%MatrixMarket"'
        mm_header_check = .false.
        return
      end if

      if ( s_neqi ( type, 'matrix' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_CHECK - Fatal error!'
        write ( *, '(a)' ) '  The value of TYPE was illegal:'
        write ( *, '(a)' ) '    "' // trim ( type ) // '".'
        write ( *, '(a)' ) '  Legal values are:'
        write ( *, '(a)' ) '    "matrix"'
        mm_header_check = .false.
        return
      end if

      if ( 
     &  s_neqi ( rep, 'coordinate' ) .and. 
     &  s_neqi ( rep, 'array'      ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_CHECK - Fatal error!'
        write ( *, '(a)' ) '  The value of REP was illegal:'
        write ( *, '(a)' ) '    "' // trim ( rep ) // '".'
        write ( *, '(a)' ) '  Legal values are:'
        write ( *, '(a)' ) '    "array"'
        write ( *, '(a)' ) '    "coordinate"'
        mm_header_check = .false.
        return
      end if

      if ( s_eqi ( rep, 'coordinate' ) ) then

        if ( 
     &    s_neqi ( field, 'integer' ) .and. 
     &    s_neqi ( field, 'real'    ) .and. 
     &    s_neqi ( field, 'double'  ) .and. 
     &    s_neqi ( field, 'complex' ) .and. 
     &    s_neqi ( field, 'pattern' ) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_HEADER_CHECK - Fatal error!'
          write ( *, '(a)' ) '  The value of FIELD was illegal:'
          write ( *, '(a)' ) '    "' // trim ( field ) // '".'
          mm_header_check = .false.
          return
        end if

      else if ( s_eqi ( rep, 'array' ) ) then

        if ( 
     &    s_neqi ( field, 'integer' ) .and. 
     &    s_neqi ( field, 'real'    ) .and. 
     &    s_neqi ( field, 'double'  ) .and. 
     &    s_neqi ( field, 'complex' ) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_HEADER_CHECK - Fatal error!'
          write ( *, '(a)' ) '  The value of FIELD was illegal:'
          write ( *, '(a)' ) '    "' // trim ( field ) // '".'
          mm_header_check = .false.
          return
        end if

      end if

      if ( 
     &  s_neqi ( symm, 'general'        ) .and. 
     &  s_neqi ( symm, 'symmetric'      ) .and. 
     &  s_neqi ( symm, 'hermitian'      ) .and. 
     &  s_neqi ( symm, 'skew-symmetric' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_CHECK - Fatal error!'
        write ( *, '(a)' ) '  The value of SYMM was illegal:'
        write ( *, '(a)' ) '    "' // trim ( symm ) // '".'
        mm_header_check = .false.
        return
      end if

      mm_header_check = .true.

      return
      end
      subroutine mm_header_print ( input_file, id, type, rep, field, 
     &  symm )

c*********************************************************************72
c
cc MM_HEADER_PRINT prints header information from a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILE, the input file name.
c
c    Input, character * ( 14 ) ID, the Matrix Market identifier.
c    This value must be '%%MatrixMarket'.
c
c    Input, character * ( 6 ) TYPE, the Matrix Market type.
c    This value must be 'matrix'.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
      implicit none

      character * ( 14 ) id
      character * ( 7 ) field
      character * ( * ) input_file
      character * ( 10 ) rep
      character * ( 19 ) symm
      character * ( 6 ) type

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MM_HEADER_PRINT:'
      write ( *, '(a)' ) 
     &  '  Header information from Matrix Market file "' 
     &  // trim ( input_file ) // '".'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix Market ID = "' 
     &  // trim ( id ) // '".'
      write ( *, '(a)' ) '    "%%MatrixMarket" is only allowed value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix Market TYPE = "' 
     &  // trim ( type ) // '".'
      write ( *, '(a)' ) '    "matrix" is only allowed value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Representation type REP = "' 
     &  // trim ( rep ) // '".'
      write ( *, '(a)' ) '    "coordinate" for sparse matrices,'
      write ( *, '(a)' ) '    "array"      for dense matrices,'
      write ( *, '(a)' ) 
     &  '    "elemental"  for unassembled finite element matrices.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Numeric FIELD = "' 
     &  // trim ( field ) // '".'
      write ( *, '(a)' ) '    "integer" for integer values,'
      write ( *, '(a)' ) '    "real"    for real values,'
      write ( *, '(a)' ) 
     &  '    "double"  for double precision real values,'
      write ( *, '(a)' ) '    "complex" for complex values,'
      write ( *, '(a)' ) '    "pattern" for nonzero pattern only.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Symmetry type SYMM = "' 
     &  // trim ( symm ) // '".'
      write ( *, '(a)' ) '    "general"         no symmetry,'
      write ( *, '(a)' ) '    "symmetric"       A(I,J) = A(J,I),'
      write ( *, '(a)' ) 
     &  '                      input only lower triangle.'
      write ( *, '(a)' ) '    "skew-symmetric"  A(I,J) = - A(J,I),'
      write ( *, '(a)' ) 
     &  '                      input only strict lower triangle.'
      write ( *, '(a)' ) '    "Hermitian"       A(I,J) = A*(J,I),'
      write ( *, '(a)' ) 
     &  '                      input only lower triangle.'

      return
      end
      subroutine mm_header_read ( input_unit, id, type, rep, field, 
     &  symm )

c*********************************************************************72
c
cc MM_HEADER_READ reads the header line from a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    Original FORTRAN77 version by Karin A. Remington, NIST ACMD
c    This FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT_UNIT, the input unit identifier.
c
c    Output, character * ( 14 ) ID, the Matrix Market identifier.
c    This value must be '%%MatrixMarket'.
c
c    Output, character * ( 6 ) TYPE, the Matrix Market type.
c    This value must be 'matrix'.
c
c    Output, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Output, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Output, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
      implicit none

      logical done
      character * ( 7 ) field
      character * ( 14 ) id
      integer input_unit
      integer ios
      character * ( 10 ) rep
      character * ( 1024 ) s
      character * ( 19 ) symm
      character * ( 6 ) type
c
c  Read the header line.
c
      read ( input_unit, '(a)', iostat = ios ) s

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  I/O error reading the header line.'
        stop 1
      end if

      if ( len_trim ( s ) .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The header line does not contain any data.'
        stop 1
      end if
c
c  Parse the blank-delimited words from the header line:
c
      done = .true.

      call s_word_next ( s, id, done )

      if ( done ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The header line does not contain enough data.'
        write ( *, '(a)' ) '  Could not read the ID field.'
        stop 1
      end if

      call s_word_next ( s, type, done )

      if ( done ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The header line does not contain enough data.'
        write ( *, '(a)' ) '  Could not read the TYPE field.'
        stop 1
      end if

      call s_word_next ( s, rep, done )

      if ( done ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The header line does not contain enough data.'
        write ( *, '(a)' ) '  Could not read the REP field.'
        stop 1
      end if

      call s_word_next ( s, field, done )

      if ( done ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The header line does not contain enough data.'
        write ( *, '(a)' ) '  Could not read the FIELD field.'
        stop 1
      end if

      call s_word_next ( s, symm, done )

      if ( done ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The header line does not contain enough data.'
        write ( *, '(a)' ) '  Could not read the SYMM field.'
        stop 1
      end if

      return
      end
      subroutine mm_header_write ( output_unit, id, type, rep, field, 
     &  symm )

c*********************************************************************72
c
cc MM_HEADER_WRITE prints header information to a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer OUTPUT_UNIT, the output unit identifier.
c
c    Input, character * ( 14 ) ID, the Matrix Market identifier.
c    This value must be '%%MatrixMarket'.
c
c    Input, character * ( 6 ) TYPE, the Matrix Market type.
c    This value must be 'matrix'.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
      implicit none

      character * ( 7 ) field
      character * ( 14 ) id
      integer output_unit
      character * ( 10 ) rep
      character * ( 19 ) symm
      character * ( 6 ) type

      write ( output_unit, '(a,1x,a,1x,a,1x,a,1x,a)' ) 
     &  trim ( id ), trim ( type ), trim ( rep ), trim ( field ), 
     &  trim ( symm )

      return
      end
      subroutine mm_nnz_set ( rep, symm, nrow, ncol, nnz )

c*********************************************************************72
c
cc MM_NNZ_SET sets the value of NNZ for the ARRAY representation.
c
c  Discussion:
c
c    If the representation is not "ARRAY", then NNZ is returned as 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Input, integer NROW, the number of rows in the matrix.
c
c    Input, integer NCOL, the number of columns in the matrix.
c
c    Output, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
      implicit none

      integer ncol
      integer nrow
      integer nnz
      character * ( 10 ) rep
      logical s_eqi
      character * ( 19 ) symm

      nnz = 0

      if ( s_eqi ( rep, 'coordinate' ) ) then

      else if ( s_eqi ( rep, 'array' ) ) then

        if ( s_eqi ( symm, 'general' ) ) then
          nnz = nrow * ncol
        else if ( s_eqi ( symm, 'symmetric' ) .or. 
     &            s_eqi ( symm, 'hermitian' ) ) then
          nnz = ( nrow * ncol - nrow ) / 2 + nrow
        else if ( s_eqi ( symm, 'skew-symmetric' ) ) then
          nnz = ( nrow * ncol - nrow ) / 2
        end if

      end if

      return
      end
      subroutine mm_size_print ( input_file, rep, symm, nrow, ncol, 
     &  nnz )

c*********************************************************************72
c
cc MM_SIZE_PRINT prints size information from a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILE, the input file name.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Input, integer NROW, the number of rows in the matrix.
c
c    Input, integer NCOL, the number of columns in the matrix.
c
c    Input, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
      implicit none

      character * ( * ) input_file
      integer ncol
      integer nnz
      integer nnz2
      integer nrow
      character * ( 10 ) rep
      logical s_eqi
      character * ( 17 ) symm

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MM_SIZE_PRINT:'
      write ( *, '(a)' ) 
     &  '  Size information from Matrix Market file "' 
     &  // trim ( input_file ) // '".'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of rows    NROW = ', nrow
      write ( *, '(a,i8)' ) '  Number of columns NCOL = ', ncol

      if ( s_eqi ( rep, 'coordinate' ) ) then
        write ( *, '(a,i8)' ) 
     &    '  Declared number of nonzeros NNZ = ', nnz
      else if ( s_eqi ( rep, 'array' ) ) then
        write ( *, '(a,i8)' ) '  Dummy number of nonzeros NNZ = ', nnz
        call mm_nnz_set ( rep, symm, nrow, ncol, nnz2 )
        write ( *, '(a,i8)' ) 
     &    '  Inferred number of nonzeros NNZ2 = ', nnz2
      end if

      return
      end
      subroutine mm_size_read_file ( input_unit, rep, symm, nrow, 
     &  ncol, nnz )

c*********************************************************************72
c
cc MM_SIZE_READ_FILE reads size information from a Matrix Market file.
c
c  Discussion:
c
c    This routine assumes that the next line of the file is the size
c    record.  However, a Matrix Market file may contain comment lines
c    between the first record and the size record.  In that case, you
c    might prefer using MM_COMMENT_READ to read each line until you
c    find the first noncomment (comment lines must begin with the
c    percent character) and then passing that line to MM_SIZE_READ_STRING.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT_FILE, the input file unit.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Output, integer NROW, the number of rows in the matrix.
c
c    Output, integer NCOL, the number of columns in the matrix.
c
c    Output, integer NNZ, the number of nonzero entries required to store
c    the matrix, if REP = 'coordinate'.
c
      implicit none

      integer input_unit
      integer ios
      integer ncol
      integer nnz
      integer nrow
      character * ( 10 ) rep
      logical s_eqi
      character * ( 19 ) symm

      if ( s_eqi ( rep, 'coordinate' ) ) then

        read ( input_unit, *, iostat = ios ) nrow, ncol, nnz

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_SIZE_READ_FILE - Fatal error!'
          write ( *, '(a)' ) 
     &      '  I/O error while reading size information.'
          write ( *, '(a)' ) '  Expecting "NROW NCOL NNZ" values.'
          stop 1
        end if

      else if ( s_eqi ( rep, 'array' ) ) then

        read ( input_unit, *, iostat = ios ) nrow, ncol

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_SIZE_READ_FILE - Fatal error!'
          write ( *, '(a)' ) 
     &      '  I/O error while reading size information.'
          write ( *, '(a)' ) '  Expecting "NROW NCOL" values.'
          stop 1
        end if

        call mm_nnz_set ( rep, symm, nrow, ncol, nnz )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_SIZE_READ_FILE - Fatal error!'
        write ( *, '(a)' ) '  Unexpected value of REP.'
        stop 1

      end if

      return
      end
      subroutine mm_size_read_string ( string, rep, symm, nrow, ncol, 
     &  nnz )

c*********************************************************************72
c
cc MM_SIZE_READ_STRING reads size information from a string.
c
c  Discussion:
c
c    We presume that the string contains the size record from a
c    Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) STRING, the string containing the size record.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 19 ) SYMM, the Matrix Market symmetry.
c    Possible values include:
c    'symmetric'
c    'hermitian'
c    'skew-symmetric'
c    'general'
c
c    Output, integer NROW, the number of rows in the matrix.
c
c    Output, integer NCOL, the number of columns in the matrix.
c
c    Output, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
      implicit none

      integer ierror
      integer ncol
      integer nnz
      integer nrow
      character * ( 10 ) rep
      logical s_eqi
      character * ( * ) string
      character * ( 19 ) symm
      integer value(3)

      if ( s_eqi ( rep, 'coordinate' ) ) then

        call s_to_i4vec ( string, 3, value, ierror )

        if ( ierror .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_SIZE_READ_STRING - Fatal error!'
          write ( *, '(a)' ) 
     &      '  I/O error while reading size information.'
          write ( *, '(a)' ) '  Expecting "NROW NCOL NNZ" values.'
          stop 1
        end if

        nrow = value(1)
        ncol = value(2)
        nnz = value(3)

      else if ( s_eqi ( rep, 'array' ) ) then

        call s_to_i4vec ( string, 2, value, ierror )

        if ( ierror .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_SIZE_READ_STRING - Fatal error!'
          write ( *, '(a)' ) 
     &      '  I/O error while reading size information.'
          write ( *, '(a)' ) '  Expecting "NROW NCOL" values.'
          stop 1
        end if

        nrow = value(1)
        ncol = value(2)

        call mm_nnz_set ( rep, symm, nrow, ncol, nnz )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_SIZE_READ_STRING - Fatal error!'
        write ( *, '(a)' ) '  Unexpected value of REP.'
        stop 1

      end if

      return
      end
      subroutine mm_size_write ( output_unit, rep, nrow, ncol, nnz )

c*********************************************************************72
c
cc MM_SIZE_WRITE writes size information to a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer OUTPUT_UNIT, the input file unit.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, integer NROW, the number of rows in the matrix.
c
c    Input, integer NCOL, the number of columns in the matrix.
c
c    Input, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
      implicit none

      integer ncol
      integer nnz
      integer nrow
      integer output_unit
      character * ( 10 ) rep
      logical s_eqi

      if ( s_eqi ( rep, 'coordinate' ) ) then
        write ( output_unit, * ) nrow, ncol, nnz
      else if ( s_eqi ( rep, 'array' ) ) then
        write ( output_unit, * ) nrow, ncol
      end if

      return
      end
      subroutine mm_values_print ( rep, field, nnz, indx, jndx, ival, 
     &  rval, dval, cval )

c*********************************************************************72
c
cc MM_VALUES_PRINT prints the matrix values of a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Input, integer NNZ, the number of nonzero entries required to
c    store the matrix, if REP = 'coordinate'.
c
c    Input, integer INDX(NNZ), the row indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer JNDX(NNZ), the column indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer IVAL(NNZ), matrix values, if FIELD 
c    is 'integer'.
c
c    Input, real RVAL(NNZ), matrix values, if FIELD is 'real'.
c
c    Input, double precision DVAL(NNZ), matrix values, if FIELD is 'double'.
c
c    Input, complex CVAL(NNZ), matrix values, if FIELD 
c    is 'complex'.
c
      implicit none

      integer nnz

      complex cval(nnz)
      double precision dval(nnz)
      character * ( 7 ) field
      integer ihi
      integer ilo
      integer indx(nnz)
      integer ival(nnz)
      integer jndx(nnz)
      character * ( 10 ) rep
      real rval(nnz)

      ilo = 1
      ihi = nnz

      call mm_values_print_some ( rep, field, nnz, indx, jndx, ival, 
     &  rval, dval, cval, ilo, ihi )

      return
      end
      subroutine mm_values_print_some ( rep, field, nnz, indx, jndx, 
     &  ival, rval, dval, cval, ilo, ihi )

c*********************************************************************72
c
cc MM_VALUES_PRINT_SOME prints some matrix values of a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Input, integer NNZ, the number of nonzero entries required to
c    store the matrix, if REP = 'coordinate'.
c
c    Input, integer INDX(NNZ), the row indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer JNDX(NNZ), the column indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer IVAL(NNZ), matrix values, if FIELD 
c    is 'integer'.
c
c    Input, real RVAL(NNZ), matrix values, if FIELD is 'real'.
c
c    Input, double precision DVAL(NNZ), matrix values, if FIELD is 'double'.
c
c    Input, complex CVAL(NNZ), matrix values, if FIELD 
c    is 'complex'.
c
c    Input, integer ILO, IHI, the minimum and maximum indices of
c    the data to print.
c
      implicit none

      integer nnz

      complex cval(nnz)
      double precision dval(nnz)
      character * ( 7 ) field
      integer i
      integer ihi
      integer ilo
      integer indx(nnz)
      integer ival(nnz)
      integer jndx(nnz)
      character * ( 10 ) rep
      real rval(nnz)
      logical s_eqi

      if ( s_eqi ( rep, 'coordinate' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Sparse array storage by coordinate.'
        write ( *, '(a,i8,a,i8)' ) 
     &    '  Listing entries ', ilo, ' through ', ihi
        write ( *, '(a)' ) ' '
      else if ( s_eqi ( rep, 'array' ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Dense array storage of all elements.'
        write ( *, '(a,i8,a,i8)' ) 
     &    '  Listing entries ', ilo, ' through ', ihi
        write ( *, '(a)' ) ' '
      end if

      do i = max ( ilo, 1 ), min ( ihi, nnz )

        if ( s_eqi ( rep, 'coordinate' ) ) then

          if ( s_eqi ( field, 'integer' ) ) then
            write ( *, '(i4,2x,3i8)' ) i, indx(i), jndx(i), ival(i)
          else if ( s_eqi ( field, 'real' ) ) then
            write ( *, '(i4,2x,2i8,g14.6)' ) 
     &        i, indx(i), jndx(i), rval(i)
          else if ( s_eqi ( field, 'double' ) ) then
            write ( *, '(i4,2x,2i8,g14.6)' ) 
     &        i, indx(i), jndx(i), dval(i)
          else if ( s_eqi ( field, 'complex' ) ) then
            write ( *, '(i4,2x,2i8,2g14.6)' ) 
     &        i, indx(i), jndx(i), cval(i)
          else if ( s_eqi ( field, 'pattern' ) ) then
            write ( *, '(i4,2x,2i8)' ) i, indx(i), jndx(i)
          end if

        else if ( s_eqi ( rep, 'array' ) ) then

          if ( s_eqi ( field, 'integer' ) ) then
            write ( *, '(i4,2x,i8)' ) i, ival(i)
          else if ( s_eqi ( field, 'real' ) ) then
            write ( *, '(i4,2x,g14.6)' ) i, rval(i)
          else if ( s_eqi ( field, 'double' ) ) then
            write ( *, '(i4,2x,g14.6)' ) i, dval(i)
          else if ( s_eqi ( field, 'complex' ) ) then
            write ( *, '(i4,2x,2g14.6)' ) i, cval(i)
          end if

        end if

      end do

      return
      end
      subroutine mm_values_read ( input_unit, rep, field, nnz, indx, 
     &  jndx, ival, rval, dval, cval )

c*********************************************************************72
c
cc MM_VALUES_READ reads matrix values from a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT_UNIT, the input unit identifier.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Input, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
c    Output, integer INDX(NNZ), the row indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Output, integer JNDX(NNZ), the column indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Output, integer IVAL(NNZ), matrix values, if FIELD 
c    is 'integer'.
c
c    Output, real RVAL(NNZ), matrix values, if FIELD is 'real'.
c
c    Output, double precision DVAL(NNZ), matrix values, if FIELD is 'double'.
c
c    Output, complex CVAL(NNZ), matrix values, if FIELD 
c    is 'complex'.
c
      implicit none

      integer nnz

      complex cval(nnz)
      double precision dval(nnz)
      character * ( 7 ) field
      integer i
      integer indx(nnz)
      integer input_unit
      integer ios
      integer ival(nnz)
      integer jndx(nnz)
      character * ( 10 ) rep
      real rval(nnz)
      logical s_eqi

      if ( s_eqi ( rep, 'coordinate' ) ) then

        if ( s_eqi ( field, 'integer' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) 
     &        indx(i), jndx(i), ival(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'real' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) 
     &        indx(i), jndx(i), rval(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'double' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) 
     &        indx(i), jndx(i), dval(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'complex' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) 
     &        indx(i), jndx(i), cval(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'pattern' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) indx(i), jndx(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
          write ( *, '(a)' ) '  Illegal value of FIELD.'
          stop 1
        end if

      else if ( s_eqi ( rep, 'array' ) ) then

        if ( s_eqi ( field, 'integer' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) ival(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'real' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) rval(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'double' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) dval(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else if ( s_eqi ( field, 'complex' ) ) then
          do i = 1, nnz
            read ( input_unit, *, iostat = ios ) cval(i)
            if ( ios .ne. 0 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
              write ( *, '(a)' ) 
     &          '  Error or end of file on value field ', i
              stop 1
            end if
          end do
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
          write ( *, '(a)' ) '  Illegal value of FIELD.'
          stop 1
        end if

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MM_VALUES_READ - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of REP.'
        stop 1

      end if

      return
      end
      subroutine mm_values_write ( output_unit, rep, field, nnz, indx, 
     &  jndx, ival, rval, dval, cval )

c*********************************************************************72
c
cc MM_VALUES_WRITE writes matrix values to a Matrix Market file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer OUTPUT_UNIT, the output unit identifier.
c
c    Input, character * ( 10 ) REP, the Matrix Market 'representation'
c    indicator.  Possible values include:
c    'coordinate'   (for sparse data)
c    'array'        (for dense data)
c    'elemental'    (to be added)
c
c    Input, character * ( 7 ) FIELD, the Matrix Market 'field'.
c    Possible values include:
c    'real'
c    'double'
c    'complex'
c    'integer'
c    'pattern'
c
c    Input, integer NNZ, the number of nonzero entries required
c    to store the matrix, if REP = 'coordinate'.
c
c    Input, integer INDX(NNZ), the row indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer JNDX(NNZ), the column indices for coordinate
c    format.  Not used if REP is 'array'.
c
c    Input, integer IVAL(NNZ), matrix values, if FIELD 
c    is 'integer'.
c
c    Input, real RVAL(NNZ), matrix values, if FIELD is 'real'.
c
c    Input, double precision DVAL(NNZ), matrix values, if FIELD is 'double'.
c
c    Input, complex CVAL(NNZ), matrix values, if FIELD 
c    is 'complex'.
c
      implicit none

      integer nnz

      complex cval(nnz)
      double precision dval(nnz)
      character * ( 7 ) field
      integer i
      integer indx(nnz)
      integer ival(nnz)
      integer jndx(nnz)
      integer output_unit
      character * ( 10 ) rep
      real rval(nnz)
      logical s_eqi

      if ( s_eqi ( rep, 'coordinate' ) ) then

        if ( s_eqi ( field, 'integer' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) indx(i), jndx(i), ival(i)
          end do
        else if ( s_eqi ( field, 'real' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) indx(i), jndx(i), rval(i)
          end do
        else if ( s_eqi ( field, 'double' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) indx(i), jndx(i), dval(i)
          end do
        else if ( s_eqi ( field, 'complex' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) indx(i), jndx(i), cval(i)
          end do
        else if ( s_eqi ( field, 'pattern' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) indx(i), jndx(i)
          end do
        end if

      else if ( s_eqi ( rep, 'array' ) ) then

        if ( s_eqi ( field, 'integer' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) ival(i)
          end do
        else if ( s_eqi ( field, 'real' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) rval(i)
          end do
        else if ( s_eqi ( field, 'double' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) dval(i)
          end do
        else if ( s_eqi ( field, 'complex' ) ) then
          do i = 1, nnz
            write ( output_unit, * ) cval(i)
          end do
        end if

      end if

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
      function s_neqi ( s1, s2 )

c*********************************************************************72
c
cc S_NEQI compares two strings for non-equality, ignoring case.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S1, S2, the strings to compare.
c
c    Output, logical S_NEQI, the result of the comparison.
c
      implicit none

      character c1
      character c2
      integer i
      integer len1
      integer len2
      integer lenc
      logical s_neqi
      character * ( * ) s1
      character * ( * ) s2

      len1 = len ( s1 )
      len2 = len ( s2 )
      lenc = min ( len1, len2 )

      s_neqi = .true.

      do i = 1, lenc

        c1 = s1(i:i)
        c2 = s2(i:i)
        call ch_cap ( c1 )
        call ch_cap ( c2 )

        if ( c1 .ne. c2 ) then
          return
        end if

      end do

      do i = lenc + 1, len1
        if ( s1(i:i) .ne. ' ' ) then
          return
        end if
      end do

      do i = lenc + 1, len2
        if ( s2(i:i) .ne. ' ' ) then
          return
        end if
      end do

      s_neqi = .false.

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
      subroutine s_word_next ( s, word, done )

c*********************************************************************72
c
cc S_WORD_NEXT "reads" words from a string, one at a time.
c
c  Special cases:
c
c    The following characters are considered to be a single word,
c    whether surrounded by spaces or not:
c
c      " ( ) { } [ ]
c
c    Also, if there is a trailing comma on the word, it is stripped off.
c    This is to facilitate the reading of lists.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, a string, presumably containing words
c    separated by spaces.
c
c    Output, character * ( * ) WORD.
c    If DONE is FALSE, then WORD contains the "next" word read.
c    If DONE is TRUE, then WORD is blank, because there was no more to read.
c
c    Input/output, logical DONE.
c    On input with a fresh string, set DONE to TRUE.
c    On output, the routine sets DONE:
c      FALSE if another word was read,
c      TRUE if no more words could be read.
c
      implicit none

      logical done
      integer ilo
      integer lenc
      integer next
      character * ( * ) s
      character TAB
      parameter ( TAB = char ( 9 ) )
      character * ( * ) word

      save lenc
      save next

      data lenc / 0 /
      data next / 1 /
c
c  We "remember" LENC and NEXT from the previous call.
c
c  An input value of DONE = TRUE signals a new line of text to examine.
c
      if ( done ) then

        next = 1
        done = .false.
        lenc = len_trim ( s )

        if ( lenc .le. 0 ) then
          done = .true.
          word = ' '
          return
        end if

      end if
c
c  Beginning at index NEXT, search the string for the next nonblank,
c  which signals the beginning of a word.
c
      ilo = next
c
c  ...S(NEXT:) is blank.  Return with WORD = ' ' and DONE = TRUE.
c
10    continue

        if ( lenc .lt. ilo ) then
          word = ' '
          done = .true.
          next = lenc + 1
          return
        end if
c
c  If the current character is blank, skip to the next one.
c
        if ( s(ilo:ilo) .ne. ' ' .and. s(ilo:ilo) .ne. TAB ) then
          go to 20
        end if

        ilo = ilo + 1

      go to 10

20    continue
c
c  ILO is the index of the next nonblank character in the string.
c
c  If this initial nonblank is a special character,
c  then that's the whole word as far as we're concerned,
c  so return immediately.
c
      if ( s(ilo:ilo) .eq. '"' .or. 
     &     s(ilo:ilo) .eq. '(' .or. 
     &     s(ilo:ilo) .eq. ')' .or. 
     &     s(ilo:ilo) .eq. '{' .or. 
     &     s(ilo:ilo) .eq. '}' .or. 
     &     s(ilo:ilo) .eq. '[' .or. 
     &     s(ilo:ilo) .eq. ']' ) then

        word = s(ilo:ilo)
        next = ilo + 1
        return

      end if
c
c  Now search for the last contiguous character that is not a
c  blank, TAB, or special character.
c
      next = ilo + 1

30    continue

      if ( next .le. lenc ) then

        if ( s(next:next) .eq. ' ' ) then
          go to 40
        else if ( s(next:next) .eq. TAB ) then
          go to 40
        else if ( s(next:next) .eq. '"' ) then
          go to 40
        else if ( s(next:next).eq. '(' ) then
          go to 40
        else if ( s(next:next) .eq. ')' ) then
          go to 40
        else if ( s(next:next) .eq. '{' ) then
          go to 40
        else if ( s(next:next) .eq. '}' ) then
          go to 40
        else if ( s(next:next) .eq. '[' ) then
          go to 40
        else if ( s(next:next) .eq. ']' ) then
          go to 40
        end if

        next = next + 1

        go to 30

      end if

40    continue
c
c  Ignore a trailing comma.
c
      if ( s(next-1:next-1) .eq. ',' ) then
        word = s(ilo:next-2)
      else
        word = s(ilo:next-1)
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