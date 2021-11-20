      program main

c*********************************************************************72
c
cc MAIN is the main program for TETRAHEDRON_PROPERTIES.
c
c  Discussion:
c
c    TETRAHEDRON_PROPERTIES reports properties of a tetrahedron.
c
c  Usage:
c
c    tetrahedron_properties filename
c
c    where "filename" is a file containing the coordinates of the vertices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision ab(3)
      double precision ac(3)
      double precision ad(3)
      integer arg_num
      double precision bc(3)
      double precision bd(3)
      double precision cd(3)
      double precision centroid(3)
      double precision circum_center(3)
      double precision circum_radius
      double precision dihedral_angles(6)
      integer dim_num
      double precision edge_length(6)
      double precision face_angles(3,4)
      double precision face_areas(4)
      integer i
      integer iarg
      integer iargc
      double precision in_center(3)
      double precision in_radius
      character ( len = 255 ) node_filename
      integer node_num
      double precision  node_xyz(3,4)
      double precision quality1
      double precision quality2
      double precision quality3
      double precision quality4
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision solid_angles(4)
      double precision volume

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_PROPERTIES:'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Determine properties of a tetrahedron.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Commandline argument #1 is the file name
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, node_filename )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TETRAHEDRON_PROPERTIES:'
        write ( *, '(a)' ) 
     &    '  Enter the name of the node coordinate file.'

        read ( *, '(a)' ) node_filename

      end if
c
c  Read the node data.
c
      call r8mat_header_read ( node_filename, dim_num, node_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the header of "' 
     &  // trim ( node_filename ) //'".'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
      write ( *, '(a,i8)' ) '  Number of points NODE_NUM = ', node_num

      if ( dim_num .ne. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TETRAHEDRON_PROPERTIES - Fatal error!'
        write ( *, '(a)' ) '  Dataset must have spatial dimension 3.'
        stop
      end if

      if ( node_num .ne. 4 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TETRAHEDRON_PROPERTIES - Fatal error!'
        write ( *, '(a)' ) '  Dataset must have 4 nodes.'
        stop
      end if

      call r8mat_data_read ( node_filename, dim_num, node_num, 
     &  node_xyz )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the data in "' 
     &  // trim ( node_filename ) //'".'

      call r8mat_transpose_print ( dim_num, node_num, node_xyz, 
     &  '  Node coordinates:' )
c
c  CENTROID
c
      call tetrahedron_centroid ( node_xyz, centroid )

      write ( *, '(a)' ) ' '
      write ( *, '(a,3g14.6)' ) '  CENTROID: ', centroid(1:3)
c
c  CIRCUMSPHERE
c
      call tetrahedron_circumsphere ( node_xyz, circum_radius, 
     &  circum_center )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  CIRCUM_RADIUS = ', circum_radius
      write ( *, '(a,3g14.6)' ) '  CIRCUM_CENTER: ', circum_center(1:3)
c
c  DIHEDRAL ANGLES
c
      call tetrahedron_dihedral_angles ( node_xyz, dihedral_angles )

      call r8vec_print ( 6, dihedral_angles, 
     &  '  DIHEDRAL_ANGLES (radians)' )

      dihedral_angles(1:6) = dihedral_angles(1:6) * 180.0D+00 / r8_pi

      call r8vec_print ( 6, dihedral_angles, 
     &  '  DIHEDRAL_ANGLES (degrees)' )
c
c  EDGES
c
      call tetrahedron_edges ( node_xyz, ab, ac, ad, bc, bd, cd )

      write ( *, '(a)' ) ''
      call r8vec_transpose_print ( 3, ab, '  EDGE AB:' )
      call r8vec_transpose_print ( 3, ac, '  EDGE AC:' )
      call r8vec_transpose_print ( 3, ad, '  EDGE AD:' )
      call r8vec_transpose_print ( 3, bc, '  EDGE BC:' )
      call r8vec_transpose_print ( 3, bd, '  EDGE BD:' )
      call r8vec_transpose_print ( 3, cd, '  EDGE CD:' )
c
c  EDGE LENGTHS
c
      call tetrahedron_edge_length ( node_xyz, edge_length )

      call r8vec_print ( 6, edge_length, '  EDGE_LENGTHS' )
c
c  FACE ANGLES
c
      call tetrahedron_face_angles ( node_xyz, face_angles )

      call r8mat_transpose_print ( 3, 4, face_angles, 
     &  '  FACE_ANGLES (radians)' )

      face_angles(1:3,1:4) = face_angles(1:3,1:4) * 180.0D+00 / r8_pi

      call r8mat_transpose_print ( 3, 4, face_angles, 
     &  '  FACE_ANGLES (degrees)' )
c
c  FACE AREAS
c
      call tetrahedron_face_areas ( node_xyz, face_areas )

      call r8vec_print ( 4, face_areas, '  FACE_AREAS' )
c
c  INSPHERE
c
      call tetrahedron_insphere ( node_xyz, in_radius, in_center )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  IN_RADIUS = ', in_radius
      write ( *, '(a,3g14.6)' ) '  IN_CENTER: ', in_center(1:3)
c
c  QUALITY1
c
      call tetrahedron_quality1 ( node_xyz, quality1 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  QUALITY1 = ', quality1
c
c  QUALITY2
c
      call tetrahedron_quality2 ( node_xyz, quality2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  QUALITY2 = ', quality2
c
c  QUALITY3
c
      call tetrahedron_quality3 ( node_xyz, quality3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  QUALITY3 = ', quality3
c
c  QUALITY4
c
      call tetrahedron_quality4 ( node_xyz, quality4 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  QUALITY4 = ', quality4
c
c  SOLID ANGLES
c
      call tetrahedron_solid_angles ( node_xyz, solid_angles )

      call r8vec_print ( 4, solid_angles, 
     &  '  SOLID_ANGLES (steradians)' )
c
c  VOLUME
c
      call tetrahedron_volume ( node_xyz, volume )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  VOLUME = ', volume
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_PROPERTIES:'
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
      function r8_acos ( c )

c*********************************************************************72
c
cc R8_ACOS computes the arc cosine function, with argument truncation.
c
c  Discussion:
c
c    If you call your system ACOS routine with an input argument that is
c    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
c    surprise (I did).
c
c    This routine simply truncates arguments outside the range.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 October 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision C, the argument.
c
c    Output, double precision R8_ACOS, an angle whose cosine is C.
c
      implicit none

      double precision c
      double precision c2
      double precision r8_acos

      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )

      r8_acos = acos ( c2 )

      return
      end
      subroutine r8_swap ( x, y )

c*********************************************************************72
c
cc R8_SWAP switches two R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, double precision X, Y.  On output, the values of X and
c    Y have been interchanged.
c
      implicit none

      double precision x
      double precision y
      double precision z

      z = x
      x = y
      y = z

      return
      end
      function r8mat_det_4d ( a )

c*********************************************************************72
c
cc R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
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
c    31 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(4,4), the matrix whose determinant is desired.
c
c    Output, double precision R8MAT_DET_4D, the determinant of the matrix.
c
      implicit none

      double precision a(4,4)
      double precision r8mat_det_4d

      r8mat_det_4d =
     &       a(1,1) * (
     &           a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) )
     &     - a(1,2) * (
     &           a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) )
     &     + a(1,3) * (
     &           a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )
     &     - a(1,4) * (
     &           a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) )
     &         + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

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
      subroutine r8mat_solve ( n, rhs_num, a, info )

c*********************************************************************72
c
cc R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
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
c    08 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer RHS_NUM, the number of right hand sides.
c    RHS_NUM must be at least 0.
c
c    Input/output, double precision A(N,N+rhs_num), contains in rows and
c    columns 1 to N the coefficient matrix, and in columns N+1 through
c    N+rhs_num, the right hand sides.  On output, the coefficient matrix
c    area has been destroyed, while the right hand sides have
c    been overwritten with the corresponding solutions.
c
c    Output, integer INFO, singularity flag.
c    0, the matrix was not singular, the solutions were computed;
c    J, factorization failed on step J, and the solutions could not
c    be computed.
c
      implicit none

      integer n
      integer rhs_num

      double precision a(n,n+rhs_num)
      double precision apivot
      double precision factor
      integer i
      integer info
      integer ipivot
      integer j
      integer k

      info = 0

      do j = 1, n
c
c  Choose a pivot row.
c
        ipivot = j
        apivot = a(j,j)

        do i = j+1, n
          if ( abs ( apivot ) .lt. abs ( a(i,j) ) ) then
            apivot = a(i,j)
            ipivot = i
          end if
        end do

        if ( apivot .eq. 0.0D+00 ) then
          info = j
          return
        end if
c
c  Interchange.
c
        do i = 1, n + rhs_num
          call r8_swap ( a(ipivot,i), a(j,i) )
        end do
c
c  A(J,J) becomes 1.
c
        a(j,j) = 1.0D+00
        do k = j + 1, n + rhs_num
          a(j,k) = a(j,k) / apivot
        end do
c
c  A(I,J) becomes 0.
c
        do i = 1, n

          if ( i .ne. j ) then

            factor = a(i,j)
            a(i,j) = 0.0D+00
            do k = j + 1, n + rhs_num
              a(i,k) = a(i,k) - factor * a(j,k)
            end do

          end if

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
      subroutine r8vec_angle_3d ( u, v, angle )

c*********************************************************************72
c
cc R8VEC_ANGLE_3D computes the angle between two vectors in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision U(3), V(3), the vectors.
c
c    Output, double precision ANGLE, the angle between the two vectors.
c
      implicit none

      double precision angle
      double precision angle_cos
      double precision r8_acos
      double precision r8vec_dot_product
      double precision r8vec_norm
      double precision u(3)
      double precision u_norm
      double precision uv_dot
      double precision v(3)
      double precision v_norm

      uv_dot = r8vec_dot_product ( 3, u, v )

      u_norm = r8vec_norm ( 3, u )

      v_norm = r8vec_norm ( 3, v )

      angle_cos = uv_dot / u_norm / v_norm

      angle = r8_acos ( angle_cos )

      return
      end
      subroutine r8vec_cross_3d ( v1, v2, v3 )

c*********************************************************************72
c
cc R8VEC_CROSS_3D computes the cross product of two vectors in 3D.
c
c  Discussion:
c
c    The cross product in 3D can be regarded as the determinant of the
c    symbolic matrix:
c
c          |  i  j  k |
c      det | x1 y1 z1 |
c          | x2 y2 z2 |
c
c      = ( y1 * z2 - z1 * y2 ) * i
c      + ( z1 * x2 - x1 * z2 ) * j
c      + ( x1 * y2 - y1 * x2 ) * k
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), the two vectors.
c
c    Output, double precision V3(3), the cross product vector.
c
      implicit none

      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
      v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
      v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

      return
      end
      function r8vec_max ( n, a )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision AMAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_max
      double precision value

      value = a(1)
      do i = 2, n
        value = max ( value, a(i) )
      end do

      r8vec_max = value

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      function r8vec_norm ( n, a )

c*********************************************************************72
c
cc R8VEC_NORM returns the L2 norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), the vector whose L2 norm is desired.
c
c    Output, double precision R8VEC_NORM, the L2 norm of A.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_norm
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + a(i) * a(i)
      end do
      value = sqrt ( value )

      r8vec_norm = value

      return
      end
      function r8vec_normsq ( n, a )

c*********************************************************************72
c
cc R8VEC_NORMSQ returns the square of the L2 norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The square of the vector L2 norm is defined as:
c
c      R8VEC_NORMSQ = sum ( 1 <= I <= N ) V(I)^2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the vector dimension.
c
c    Input, double precision A(N), the vector.
c
c    Output, double precision R8VEC_NORMSQ, the squared L2 norm.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_normsq
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + a(i) * a(i)
      end do
      r8vec_normsq = value

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
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
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec_transpose_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Example:
c
c    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
c    TITLE = 'My vector:  '
c
c    My vector:   1.0    2.1    3.2    4.3    5.4
c                 6.5    7.6    8.7    9.8   10.9
c                11.0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      integer ihi
      integer ilo
      character * ( * ) title
      integer title_length

      title_length = len_trim ( title )

      do ilo = 1, n, 5
        ihi = min ( ilo + 5 - 1, n )
        if ( ilo .eq. 1 ) then
          write ( *, '(a,2x,5g14.6)' ) title(1:title_length), a(ilo:ihi)
        else
          write ( *, '(a,2x,5g14.6)' ) 
     &      ( ' ', i = 1, title_length ), a(ilo:ihi)
        end if
      end do

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
      subroutine tetrahedron_centroid ( tetra, centroid )

c*********************************************************************72
c
cc TETRAHEDRON_CENTROID computes the centroid of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4) the tetrahedron vertices.
c
c    Output, double precision CENTROID(3), the coordinates of the centroid.
c
      implicit none

      double precision centroid(3)
      integer i
      integer j
      double precision t
      double precision tetra(3,4)

      do i = 1, 3
        t = 0.0D+00
        do j = 1, 4
          t = t + tetra(i,j)
        end do
        centroid(i) = t / 4.0D+00
      end do

      return
      end
      subroutine tetrahedron_circumsphere ( tetra, r, pc )

c*********************************************************************72
c
cc TETRAHEDRON_CIRCUMSPHERE computes the circumsphere of a tetrahedron.
c
c  Discussion:
c
c    The circumsphere, or circumscribed sphere, of a tetrahedron is the
c    sphere that passes through the four vertices.  The circumsphere is
c    not necessarily the smallest sphere that contains the tetrahedron.
c
c    Surprisingly, the diameter of the sphere can be found by solving
c    a 3 by 3 linear system.  This is because the vectors P2 - P1,
c    P3 - P1 and P4 - P1 are secants of the sphere, and each forms a
c    right triangle with the diameter through P1.  Hence, the dot product of
c    P2 - P1 with that diameter is equal to the square of the length
c    of P2 - P1, and similarly for P3 - P1 and P4 - P1.  This determines
c    the diameter vector originating at P1, and hence the radius and
c    center.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Adrian Bowyer, John Woodwark,
c    A Programmer's Geometry,
c    Butterworths, 1983.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4) the tetrahedron vertices.
c
c    Output, double precision R, PC(3), the center of the
c    circumscribed sphere, and its radius.  If the linear system is
c    singular, then R = -1, PC(1:3) = 0.
c
      implicit none

      double precision a(3,4)
      integer i
      integer info
      integer j
      double precision pc(3)
      double precision r
      double precision tetra(3,4)
c
c  Set up the linear system.
c
      do j = 1, 3
        do i = 1, 4
          a(i,j) = tetra(j,i+1)
        end do
      end do

      do j = 1, 3
        do i = 1, 3
          a(i,j) = a(i,j) - tetra(j,1)
        end do
      end do

      do i = 1, 3
        a(i,4) = 0.0D+00
        do j = 1, 3
          a(i,4) = a(i,4) + a(i,j)**2
        end do
      end do
c
c  Solve the linear system.
c
      call r8mat_solve ( 3, 1, a, info )
c
c  If the system was singular, return a consolation prize.
c
      if ( info .ne. 0 ) then
        r = -1.0D+00
        do i = 1, 3
          pc(i) = 0.0D+00
        end do
        return
      end if
c
c  Compute the radius and center.
c
      r = 0.0D+00
      do i = 1, 3
        r = r + a(i,4)**2
      end do
      r = 0.5D+00 * sqrt ( r )

      do i = 1, 3
        pc(i) = tetra(i,1) + 0.5D+00 * a(i,4)
      end do

      return
      end
      subroutine tetrahedron_dihedral_angles ( tetra, angle )

c*********************************************************************72
c
cc TETRAHEDRON_DIHEDRAL_ANGLES computes dihedral angles of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision ANGLE(6), the dihedral angles along the
c    axes AB, AC, AD, BC, BD and CD, respectively.
c
      implicit none

      double precision ab(3)
      double precision abc_normal(3)
      double precision abd_normal(3)
      double precision ac(3)
      double precision acd_normal(3)
      double precision ad(3)
      double precision angle(6)
      double precision bc(3)
      double precision bcd_normal(3)
      double precision bd(3)
      double precision cd(3)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision tetra(3,4)

      call tetrahedron_edges ( tetra, ab, ac, ad, bc, bd, cd )

      call r8vec_cross_3d ( ac, ab, abc_normal )
      call r8vec_cross_3d ( ab, ad, abd_normal )
      call r8vec_cross_3d ( ad, ac, acd_normal )
      call r8vec_cross_3d ( bc, bd, bcd_normal )

      call r8vec_angle_3d ( abc_normal, abd_normal, angle(1) )
      call r8vec_angle_3d ( abc_normal, acd_normal, angle(2) )
      call r8vec_angle_3d ( abd_normal, acd_normal, angle(3) )
      call r8vec_angle_3d ( abc_normal, bcd_normal, angle(4) )
      call r8vec_angle_3d ( abd_normal, bcd_normal, angle(5) )
      call r8vec_angle_3d ( acd_normal, bcd_normal, angle(6) )

      do i = 1, 6
        angle(i) = r8_pi - angle(i)
      end do

      return
      end
      subroutine tetrahedron_edges ( tetra, ab, ac, ad, bc, bd, cd )

c*********************************************************************72
c
cc TETRAHEDRON_EDGES computes the edges of a tetrahedron.
c
c  Discussion:
c
c    The vertices are A, B, C, D.  The edge from A to B is denoted by AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 May 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision AB(3), AC(3), AD(3), BC(3), BD(3), CD(3), 
c    vectors that represent the edges of the tetrahedron.
c
      implicit none

      double precision ab(3)
      double precision ac(3)
      double precision ad(3)
      double precision bc(3)
      double precision bd(3)
      double precision cd(3)
      integer i
      double precision tetra(3,4)

      do i = 1, 3
        ab(i) = tetra(i,2) - tetra(i,1)
        ac(i) = tetra(i,3) - tetra(i,1)
        ad(i) = tetra(i,4) - tetra(i,1)
        bc(i) = tetra(i,3) - tetra(i,2)
        bd(i) = tetra(i,4) - tetra(i,2)
        cd(i) = tetra(i,4) - tetra(i,3)
      end do

      return
      end
      subroutine tetrahedron_edge_length ( tetra, edge_length )

c*********************************************************************72
c
cc TETRAHEDRON_EDGE_LENGTH returns edge lengths of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the tetrahedron vertices.
c
c    Output, double precision EDGE_LENGTH(6), the length of the edges.
c
      implicit none

      double precision edge_length(6)
      integer i
      integer j1
      integer j2
      integer k
      double precision r8vec_norm
      double precision t(3)
      double precision tetra(3,4)

      k = 0
      do j1 = 1, 3
        do j2 = j1 + 1, 4
          k = k + 1
          do i = 1, 3
            t(i) = tetra(i,j2) - tetra(i,j1)
          end do
          edge_length(k) = r8vec_norm ( 3, t )
         end do
      end do

      return
      end
      subroutine tetrahedron_face_angles ( tetra, angles )

c*********************************************************************72
c
cc TETRAHEDRON_FACE_ANGLES returns the 12 face angles of a tetrahedron.
c
c  Discussion:
c
c    The tetrahedron has 4 triangular faces.  This routine computes the
c    3 planar angles associated with each face.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4) the tetrahedron vertices.
c
c    Output, double precision ANGLES(3,4), the face angles.
c
      implicit none

      double precision angles(3,4)
      integer i
      integer j
      double precision tri(3,3)
      double precision tetra(3,4)
c
c  Face 123
c
      do j = 1, 3
        do i = 1, 3
          tri(i,j) = tetra(i,j)
        end do
      end do

      call triangle_angles_3d ( tri, angles(1:3,1) )
c
c  Face 124
c
      do i = 1, 3
        tri(i,1) = tetra(i,1)
      end do
      do i = 1, 3
        tri(i,2) = tetra(i,2)
      end do
      do i = 1, 3
        tri(i,3) = tetra(i,4)
      end do

      call triangle_angles_3d ( tri, angles(1:3,2) )
c
c  Face 134
c
      do i = 1, 3
        tri(i,1) = tetra(i,1)
      end do
      do i = 1, 3
        tri(i,2) = tetra(i,3)
      end do
      do i = 1, 3
        tri(i,3) = tetra(i,4)
      end do

      call triangle_angles_3d ( tri, angles(1:3,3) )
c
c  Face 234
c
      do i = 1, 3
        tri(i,1) = tetra(i,2)
      end do
      do i = 1, 3
        tri(i,2) = tetra(i,3)
      end do
      do i = 1, 3
        tri(i,3) = tetra(i,4)
      end do

      call triangle_angles_3d ( tri, angles(1:3,4) )

      return
      end
      subroutine tetrahedron_face_areas ( tetra, areas )

c*********************************************************************72
c
cc TETRAHEDRON_FACE_AREAS returns the 4 face areas of a tetrahedron.
c
c  Discussion:
c
c    The tetrahedron has 4 triangular faces.  This routine computes the
c    area of each face.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4) the tetrahedron vertices.
c
c    Output, double precision AREAS(4), the face areas.
c
      implicit none

      double precision areas(4)
      integer i
      integer j
      double precision tri(3,3)
      double precision tetra(3,4)
c
c  Face 123
c
      do j = 1, 3
        do i = 1, 3
          tri(i,j) = tetra(i,j)
        end do
      end do

      call triangle_area_3d ( tri, areas(1) )
c
c  Face 124
c
      do i = 1, 3
        tri(i,1) = tetra(i,1)
      end do
      do i = 1, 3
        tri(i,2) = tetra(i,2)
      end do
      do i = 1, 3
        tri(i,3) = tetra(i,4)
      end do

      call triangle_area_3d ( tri, areas(2) )
c
c  Face 134
c
      do i = 1, 3
        tri(i,1) = tetra(i,1)
      end do
      do i = 1, 3
        tri(i,2) = tetra(i,3)
      end do
      do i = 1, 3
        tri(i,3) = tetra(i,4)
      end do

      call triangle_area_3d ( tri, areas(3) )
c
c  Face 234
c
      do i = 1, 3
        tri(i,1) = tetra(i,2)
      end do
      do i = 1, 3
        tri(i,2) = tetra(i,3)
      end do
      do i = 1, 3
        tri(i,3) = tetra(i,4)
      end do

      call triangle_area_3d ( tri, areas(4) )

      return
      end
      subroutine tetrahedron_insphere ( tetra, r, pc )

c*********************************************************************72
c
cc TETRAHEDRON_INSPHERE finds the insphere of a tetrahedron.
c
c  Discussion:
c
c    The insphere of a tetrahedron is the inscribed sphere, which touches
c    each face of the tetrahedron at a single point.
c
c    The points of contact are the centroids of the triangular faces
c    of the tetrahedron.  Therefore, the point of contact for a face
c    can be computed as the average of the vertices of that face.
c
c    The sphere can then be determined as the unique sphere through
c    the four given centroids.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Schneider, David Eberly,
c    Geometric Tools for Computer Graphics,
c    Elsevier, 2002,
c    ISBN: 1558605940,
c    LC: T385.G6974.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision R, PC(3), the radius and the center
c    of the sphere.
c
      implicit none

      double precision b(4,4)
      double precision r8mat_det_4d
      double precision r8vec_norm
      double precision gamma
      integer i
      integer j
      double precision l123
      double precision l124
      double precision l134
      double precision l234
      double precision n123(3)
      double precision n124(3)
      double precision n134(3)
      double precision n234(3)
      double precision pc(3)
      double precision r
      double precision tetra(3,4)
      double precision v21(3)
      double precision v31(3)
      double precision v41(3)
      double precision v32(3)
      double precision v42(3)
      double precision v43(3)

      call tetrahedron_edges ( tetra, v21, v31, v41, v32, v42, v43 )

      call r8vec_cross_3d ( v21, v31, n123 )
      call r8vec_cross_3d ( v41, v21, n124 )
      call r8vec_cross_3d ( v31, v41, n134 )
      call r8vec_cross_3d ( v42, v32, n234 )

      l123 = r8vec_norm ( 3, n123 )
      l124 = r8vec_norm ( 3, n124 )
      l134 = r8vec_norm ( 3, n134 )
      l234 = r8vec_norm ( 3, n234 )

      do i = 1, 3
        pc(i) = ( l234 * tetra(i,1)   
     &          + l134 * tetra(i,2)   
     &          + l124 * tetra(i,3)   
     &          + l123 * tetra(i,4) ) 
     &          / ( l234 + l134 + l124 + l123 )
      end do

      do j = 1, 4
        do i = 1, 3
          b(i,j) = tetra(i,j)
        end do
        b(4,j) = 1.0D+00
      end do

      gamma = abs ( r8mat_det_4d ( b ) )

      r = gamma / ( l234 + l134 + l124 + l123 )

      return
      end
      subroutine tetrahedron_quality1 ( tetra, quality )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY1: "quality" of a tetrahedron.
c
c  Discussion:
c
c    The quality of a tetrahedron is 3 times the ratio of the radius of
c    the inscribed sphere divided by that of the circumscribed sphere.
c
c    An equilateral tetrahredron achieves the maximum possible quality of 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the tetrahedron vertices.
c
c    Output, double precision QUALITY, the quality of the tetrahedron.
c
      implicit none

      double precision pc(3)
      double precision quality
      double precision r_in
      double precision r_out
      double precision tetra(3,4)

      call tetrahedron_circumsphere ( tetra, r_out, pc )

      call tetrahedron_insphere ( tetra, r_in, pc )

      quality = 3.0D+00 * r_in / r_out

      return
      end
      subroutine tetrahedron_quality2 ( tetra, quality2 )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY2: "quality" of a tetrahedron.
c
c  Discussion:
c
c    The quality measure #2 of a tetrahedron is:
c
c      QUALITY2 = 2 * sqrt ( 6 ) * RIN / LMAX
c
c    where
c
c      RIN = radius of the inscribed sphere;
c      LMAX = length of longest side of the tetrahedron.
c
c    An equilateral tetrahredron achieves the maximum possible quality of 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Qiang Du, Desheng Wang,
c    The Optimal Centroidal Voronoi Tesselations and the Gersho's
c    Conjecture in the Three-Dimensional Space,
c    Computers and Mathematics with Applications,
c    Volume 49, 2005, pages 1355-1373.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the tetrahedron vertices.
c
c    Output, double precision QUALITY2, the quality of the tetrahedron.
c
      implicit none

      double precision edge_length(6)
      double precision l_max
      double precision pc(3)
      double precision quality2
      double precision r_in
      double precision r8vec_max
      double precision tetra(3,4)

      call tetrahedron_edge_length ( tetra, edge_length )

      l_max = r8vec_max ( 6, edge_length )

      call tetrahedron_insphere ( tetra, r_in, pc )

      quality2 = 2.0D+00 * sqrt ( 6.0D+00 ) * r_in / l_max

      return
      end
      subroutine tetrahedron_quality3 ( tetra, quality3 )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY3 computes the mean ratio of a tetrahedron.
c
c  Discussion:
c
c    This routine computes QUALITY3, the eigenvalue or mean ratio of
c    a tetrahedron.
c
c      QUALITY3 = 12 * ( 3 * volume )^(2/3) / (sum of squares of edge lengths).
c
c    This value may be used as a shape quality measure for the tetrahedron.
c
c    For an equilateral tetrahedron, the value of this quality measure
c    will be 1.  For any other tetrahedron, the value will be between
c    0 and 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2005
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision QUALITY3, the mean ratio of the tetrahedron.
c
      implicit none

      double precision ab(3)
      double precision ac(3)
      double precision ad(3)
      double precision bc(3)
      double precision bd(3)
      double precision cd(3)
      double precision denom
      double precision lab
      double precision lac
      double precision lad
      double precision lbc
      double precision lbd
      double precision lcd
      double precision quality3
      double precision r8vec_normsq
      double precision tetra(3,4)
      double precision volume
c
c  Compute the vectors representing the sides of the tetrahedron.
c
      call tetrahedron_edges ( tetra, ab, ac, ad, bc, bd, cd )
c
c  Compute the squares of the lengths of the sides.
c
      lab = r8vec_normsq ( 3, ab )
      lac = r8vec_normsq ( 3, ac )
      lad = r8vec_normsq ( 3, ad )
      lbc = r8vec_normsq ( 3, bc )
      lbd = r8vec_normsq ( 3, bd )
      lcd = r8vec_normsq ( 3, cd )
c
c  Compute the volume.
c
      volume = abs ( 
     &    ab(1) * ( ac(2) * ad(3) - ac(3) * ad(2) ) 
     &  + ab(2) * ( ac(3) * ad(1) - ac(1) * ad(3) ) 
     &  + ab(3) * ( ac(1) * ad(2) - ac(2) * ad(1) ) ) / 6.0D+00

      denom = lab + lac + lad + lbc + lbd + lcd

      if ( denom .eq. 0.0D+00 ) then
        quality3 = 0.0D+00
      else
        quality3 = 12.0D+00 
     &    * ( 3.0D+00 * volume )**( 2.0D+00 / 3.0D+00 ) / denom
      end if

      return
      end
      subroutine tetrahedron_quality4 ( tetra, quality4 )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY4 computes the minimum solid angle of a tetrahedron.
c
c  Discussion:
c
c    This routine computes a quality measure for a tetrahedron, based
c    on the sine of half the minimum of the four solid angles.
c
c    The quality measure for an equilateral tetrahedron should be 1,
c    since the solid angles of such a tetrahedron are each equal to pi.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2005
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    This version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision QUALITY4, the value of the quality measure.
c
      implicit none

      double precision ab(3)
      double precision ac(3)
      double precision ad(3)
      double precision bc(3)
      double precision bd(3)
      double precision cd(3)
      double precision denom
      double precision l1
      double precision l2
      double precision l3
      double precision lab
      double precision lac
      double precision lad
      double precision lbc
      double precision lbd
      double precision lcd
      double precision quality4
      double precision r8vec_norm
      double precision tetra(3,4)
      double precision volume
c
c  Compute the vectors that represent the sides.
c
      call tetrahedron_edges ( tetra, ab, ac, ad, bc, bd, cd )
c
c  Compute the lengths of the sides.
c
      lab = r8vec_norm ( 3, ab )
      lac = r8vec_norm ( 3, ac )
      lad = r8vec_norm ( 3, ad )
      lbc = r8vec_norm ( 3, bc )
      lbd = r8vec_norm ( 3, bd )
      lcd = r8vec_norm ( 3, cd )
c
c  Compute the volume
c
      volume = abs ( 
     &    ab(1) * ( ac(2) * ad(3) - ac(3) * ad(2) ) 
     &  + ab(2) * ( ac(3) * ad(1) - ac(1) * ad(3) ) 
     &  + ab(3) * ( ac(1) * ad(2) - ac(2) * ad(1) ) ) / 6.0D+00

      quality4 = 1.0D+00

      l1 = lab + lac
      l2 = lab + lad
      l3 = lac + lad

      denom = ( l1 + lbc ) * ( l1 - lbc ) 
     &      * ( l2 + lbd ) * ( l2 - lbd ) 
     &      * ( l3 + lcd ) * ( l3 - lcd )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      l1 = lab + lbc
      l2 = lab + lbd
      l3 = lbc + lbd

      denom = ( l1 + lac ) * ( l1 - lac ) 
     &      * ( l2 + lad ) * ( l2 - lad ) 
     &      * ( l3 + lcd ) * ( l3 - lcd )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      l1 = lac + lbc
      l2 = lac + lcd
      l3 = lbc + lcd

      denom = ( l1 + lab ) * ( l1 - lab ) 
     &      * ( l2 + lad ) * ( l2 - lad ) 
     &      * ( l3 + lbd ) * ( l3 - lbd )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      l1 = lad + lbd
      l2 = lad + lcd
      l3 = lbd + lcd

      denom = ( l1 + lab ) * ( l1 - lab ) 
     &      * ( l2 + lac ) * ( l2 - lac ) 
     &      * ( l3 + lbc ) * ( l3 - lbc )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      quality4 = quality4 * 1.5D+00 * sqrt ( 6.0D+00 )

      return
      end
      subroutine tetrahedron_solid_angles ( tetra, angle )

c*********************************************************************72
c
cc TETRAHEDRON_SOLID_ANGLES computes solid angles of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision ANGLE(4), the solid angles.
c
      implicit none

      double precision angle(4)
      double precision dihedral_angles(6)
      double precision, parameter :: r8_pi = 3.141592653589793D+00
      double precision tetra(3,4)

      call tetrahedron_dihedral_angles ( tetra, dihedral_angles )

      angle(1) = dihedral_angles(1) 
     &         + dihedral_angles(2) 
     &         + dihedral_angles(3) - r8_pi

      angle(2) = dihedral_angles(1) 
     &         + dihedral_angles(4) 
     &         + dihedral_angles(5) - r8_pi

      angle(3) = dihedral_angles(2) 
     &         + dihedral_angles(4) 
     &         + dihedral_angles(6) - r8_pi

      angle(4) = dihedral_angles(3) 
     &         + dihedral_angles(5) 
     &         + dihedral_angles(6) - r8_pi

      return
      end
      subroutine tetrahedron_volume ( tetra, volume )

c*********************************************************************72
c
cc TETRAHEDRON_VOLUME computes the volume of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision VOLUME, the volume of the tetrahedron.
c
      implicit none

      double precision a(4,4)
      integer i
      integer j
      double precision r8mat_det_4d
      double precision tetra(3,4)
      double precision volume

      do j = 1, 4
        do i = 1, 3
          a(i,j) = tetra(i,j)
        end do
        a(4,j) = 1.0D+00
      end do

      volume = abs ( r8mat_det_4d ( a ) ) / 6.0D+00

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
      subroutine triangle_angles_3d ( t, angle )

c*********************************************************************72
c
cc TRIANGLE_ANGLES_3D computes the angles of a triangle in 3D.
c
c  Discussion:
c
c    The law of cosines is used:
c
c      C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
c
c    where GAMMA is the angle opposite side C.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(3,3), the triangle vertices.
c
c    Output, double precision ANGLE(3), the angles opposite
c    sides P1-P2, P2-P3 and P3-P1, in radians.
c
      implicit none

      double precision a
      double precision angle(3)
      double precision b
      double precision c
      integer i
      double precision r8_acos
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(3,3)
c
c  Compute the length of each side.
c
      a = 0.0D+00
      b = 0.0D+00
      c = 0.0D+00
      do i = 1, 3
        a = a + ( t(i,1) - t(i,2) )**2
        b = b + ( t(i,2) - t(i,3) )**2
        c = c + ( t(i,3) - t(i,1) )**2
      end do
      a = sqrt ( a )
      b = sqrt ( b )
      c = sqrt ( c )
c
c  Take care of a ridiculous special case.
c
      if ( a .eq. 0.0D+00 .and. 
     &     b .eq. 0.0D+00 .and. 
     &     c .eq. 0.0D+00 ) then
        do i = 1, 3
          angle(i) = 2.0D+00 * r8_pi / 3.0D+00
        end do
        return
      end if

      if ( c .eq. 0.0D+00 .or. a .eq. 0.0D+00 ) then
        angle(1) = r8_pi
      else
        angle(1) = r8_acos ( ( c * c + a * a - b * b ) 
     &    / ( 2.0D+00 * c * a ) )
      end if

      if ( a .eq. 0.0D+00 .or. b .eq. 0.0D+00 ) then
        angle(2) = r8_pi
      else
        angle(2) = r8_acos ( ( a * a + b * b - c * c ) 
     &    / ( 2.0D+00 * a * b ) )
      end if

      if ( b .eq. 0.0D+00 .or. c .eq. 0.0D+00 ) then
        angle(3) = r8_pi
      else
        angle(3) = r8_acos ( ( b * b + c * c - a * a ) 
     &    / ( 2.0D+00 * b * c ) )
      end if

      return
      end
      subroutine triangle_area_3d ( t, area )

c*********************************************************************72
c
cc TRIANGLE_AREA_3D computes the area of a triangle in 3D.
c
c  Discussion:
c
c    This routine uses the fact that the norm of the cross product
c    of two vectors is the area of the parallelogram they form.
c
c    Therefore, the area of the triangle is half of that value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Adrian Bowyer, John Woodwark,
c    A Programmer's Geometry,
c    Butterworths, 1983.
c
c  Parameters:
c
c    Input, double precision T(3,3), the triangle vertices.
c
c    Output, double precision AREA, the area of the triangle.
c
      implicit none

      double precision area
      double precision cross(3)
      double precision r8vec_norm
      double precision t(3,3)
c
c  Compute the cross product vector.
c
      cross(1) = ( t(2,2) - t(2,1) ) * ( t(3,3) - t(3,1) ) 
     &         - ( t(3,2) - t(3,1) ) * ( t(2,3) - t(2,1) )

      cross(2) = ( t(3,2) - t(3,1) ) * ( t(1,3) - t(1,1) ) 
     &         - ( t(1,2) - t(1,1) ) * ( t(3,3) - t(3,1) )

      cross(3) = ( t(1,2) - t(1,1) ) * ( t(2,3) - t(2,1) ) 
     &         - ( t(2,2) - t(2,1) ) * ( t(1,3) - t(1,1) )

      area = 0.5D+00 * r8vec_norm ( 3, cross )

      return
      end
