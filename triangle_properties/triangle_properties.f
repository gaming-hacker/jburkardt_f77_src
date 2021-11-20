      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE_PROPERTIES.
c
c  Discussion:
c
c    TRIANGLE_PROPERTIES reports properties of a triangle.
c
c  Usage:
c
c    triangle_properties filename
c
c    where "filename" is a file containing the coordinates of the vertices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision angles(3)
      double precision area
      integer arg_num
      double precision centroid(2)
      double precision circum_center(2)
      double precision circum_radius
      integer dim_num
      double precision edge_length(3)
      logical flag
      integer i
      integer iarg
      integer iargc
      double precision in_center(2)
      double precision in_radius
      character ( len = 255 ) node_filename
      integer node_num
      double precision node_xy(2,3)
      integer orientation
      double precision ortho_center(2)
      double precision quality
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      integer triangle_orientation_2d

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_PROPERTIES:'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Determine properties of a triangle.'
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
        write ( *, '(a)' ) 'TRIANGLE_PROPERTIES:'
        write ( *, '(a)' ) '  Enter name of the node coordinate file.'

        read ( *, '(a)' ) node_filename

      end if
c
c  Read the node data.
c
      call r8mat_header_read ( node_filename, dim_num, node_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Read header of "' // trim ( node_filename ) //'".'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
      write ( *, '(a,i8)' ) '  Number of points NODE_NUM = ', node_num

      if ( dim_num .ne. 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_PROPERTIES - Fatal error!'
        write ( *, '(a)' ) '  Dataset must have spatial dimension 2.'
        stop 1
      end if

      if ( node_num .ne. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE_PROPERTIES - Fatal error!'
        write ( *, '(a)' ) '  Dataset must have 3 nodes.'
        stop 1
      end if

      call r8mat_data_read ( node_filename, dim_num, node_num, node_xy )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Read the data in "' // trim ( node_filename ) //'".'

      call r8mat_transpose_print ( dim_num, node_num, node_xy,
     &  '  Node coordinates:' )
c
c ANGLES
c
      call triangle_angles_2d ( node_xy, angles )

      call r8vec_print ( 3, angles, '  ANGLES (radians):' )

      angles(1:3) = angles(1:3) * 180.0D+00 / r8_pi

      call r8vec_print ( 3, angles, '  ANGLES (degrees):' )
c
c  AREA
c
      call triangle_area_2d ( node_xy, area )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  AREA:  ', area
c
c  CENTROID
c
      call triangle_centroid_2d ( node_xy, centroid )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) '  CENTROID:  ', centroid(1:2)
c
c  CIRCUMCIRCLE
c
      call triangle_circumcircle_2d ( node_xy, circum_radius, 
     &  circum_center )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  CIRCUM_RADIUS:  ', circum_radius
      write ( *, '(a,2g14.6)' ) '  CIRCUM_CENTER: ', circum_center(1:2)
c
c  EDGE LENGTHS
c
      call triangle_edge_length_2d ( node_xy, edge_length )

      call r8vec_print ( 3, edge_length, '  EDGE_LENGTHS:' )
c
c  INCIRCLE
c
      call triangle_incircle_2d ( node_xy, in_radius, in_center )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  IN_RADIUS:  ', in_radius
      write ( *, '(a,2g14.6)' ) '  IN_CENTER: ', in_center(1:2)
c
c  ORIENTATION
c
      orientation = triangle_orientation_2d ( node_xy )

      write ( *, '(a)' ) ' '
      if ( orientation .eq. 0 ) then
        write ( *, '(a,2g14.6)' ) '  ORIENTATION: CounterClockwise.'
      else if ( orientation .eq. 1 ) then
        write ( *, '(a,2g14.6)' ) '  ORIENTATION: Clockwise.'
      else if ( orientation .eq. 2 ) then
        write ( *, '(a,2g14.6)' ) 
     &    '  ORIENTATION: Degenerate Distinct Colinear Points.'
      else if ( orientation .eq. 3 ) then
        write ( *, '(a,2g14.6)' )
     &    '  ORIENTATION: Degenerate, at least two points identical.'
      end if
c
c  ORTHOCENTER
c
      call triangle_orthocenter_2d ( node_xy, ortho_center, flag )

      if ( flag ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  ORTHO_CENTER:  Could not be computed.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a,2g14.6)' ) '  ORTHO_CENTER:  ', ortho_center(1:2)
      end if
c
c  QUALITY
c
      call triangle_quality_2d ( node_xy, quality )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  QUALITY:  ', quality
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_PROPERTIES:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      function arc_cosine ( c )

c*********************************************************************72
c
cc ARC_COSINE computes the arc cosine function, with argument truncation.
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
c    02 December 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision C, the argument.
c
c    Output, double precision ARC_COSINE, an angle whose cosine is C.
c
      implicit none

      double precision arc_cosine
      double precision c
      double precision c2

      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )

      arc_cosine = acos ( c2 )

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
      function i4_modp ( i, j )

c*********************************************************************72
c
cc I4_MODP returns the nonnegative remainder of integer division.
c
c  Discussion:
c
c    If
c      NREM = I4_MODP ( I, J )
c      NMULT = ( I - NREM ) / J
c    then
c      I = J * NMULT + NREM
c    where NREM is always nonnegative.
c
c    The MOD function computes a result with the same sign as the
c    quantity being divided.  Thus, suppose you had an angle A,
c    and you wanted to ensure that it was between 0 and 360.
c    Then mod(A,360) would do, if A was positive, but if A
c    was negative, your result would be between -360 and 0.
c
c    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
c
c  Example:
c
c        I     J     MOD I4_MODP    Factorization
c
c      107    50       7       7    107 =  2 *  50 + 7
c      107   -50       7       7    107 = -2 * -50 + 7
c     -107    50      -7      43   -107 = -3 *  50 + 43
c     -107   -50      -7      43   -107 =  3 * -50 + 43
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the number to be divided.
c
c    Input, integer J, the number that divides I.
c
c    Output, integer I4_MODP, the nonnegative remainder when I is
c    divided by J.
c
      implicit none

      integer i
      integer i4_modp
      integer j
      integer value

      if ( j .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODP - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
        stop
      end if

      value = mod ( i, j )

      if ( value .lt. 0 ) then
        value = value + abs ( j )
      end if

      i4_modp = value

      return
      end
      function i4_wrap ( ival, ilo, ihi )

c*********************************************************************72
c
cc I4_WRAP forces an I4 to lie between given limits by wrapping.
c
c  Example:
c
c    ILO = 4, IHI = 8
c
c    I  Value
c
c    -2     8
c    -1     4
c     0     5
c     1     6
c     2     7
c     3     8
c     4     4
c     5     5
c     6     6
c     7     7
c     8     8
c     9     4
c    10     5
c    11     6
c    12     7
c    13     8
c    14     4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IVAL, an integer value.
c
c    Input, integer ILO, IHI, the desired bounds for the integer value.
c
c    Output, integer I4_WRAP, a "wrapped" version of IVAL.
c
      implicit none

      integer i4_modp
      integer i4_wrap
      integer ihi
      integer ilo
      integer ival
      integer jhi
      integer jlo
      integer value
      integer wide

      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )

      wide = jhi - jlo + 1

      if ( wide .eq. 1 ) then
        value = jlo
      else
        value = jlo + i4_modp ( ival - jlo, wide )
      end if

      i4_wrap = value

      return
      end
      function line_exp_is_degenerate_nd ( dim_num, p1, p2 )

c*********************************************************************72
c
cc LINE_EXP_IS_DEGENERATE_ND finds if an explicit line is degenerate in ND.
c
c  Discussion:
c
c    The explicit form of a line in ND is:
c
c      the line through the points P1 and P2.
c
c    An explicit line is degenerate if the two defining points are equal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, double precision P1(DIM_NUM), P2(DIM_NUM), two points on the line.
c
c    Output, logical LINE_EXP_IS_DEGENERATE_ND, is TRUE if the line
c    is degenerate.
c
      implicit none

      integer dim_num

      integer i
      logical line_exp_is_degenerate_nd
      double precision p1(dim_num)
      double precision p2(dim_num)
      logical value

      value = .true.
      do i = 1, dim_num
        if ( p1(i) .ne. p2(i) ) then
          value = .false.
        end if
      end do

      line_exp_is_degenerate_nd = value

      return
      end
      subroutine line_exp_perp_2d ( p1, p2, p3, p4, flag )

c*********************************************************************72
c
cc LINE_EXP_PERP_2D computes a line perpendicular to a line and through a point.
c
c  Discussion:
c
c    The explicit form of a line in 2D is:
c
c      the line through the points P1 and P2.
c
c    The input point P3 should NOT lie on the line (P1,P2).  If it
c    does, then the output value P4 will equal P3.
c
c    P1-----P4-----------P2
c            |
c            |
c           P3
c
c    P4 is also the nearest point on the line (P1,P2) to the point P3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), two points on the line.
c
c    Input, double precision P3(2), a point (presumably not on the
c    line (P1,P2)), through which the perpendicular must pass.
c
c    Output, double precision P4(2), a point on the line (P1,P2),
c    such that the line (P3,P4) is perpendicular to the line (P1,P2).
c
c    Output, logical FLAG, is TRUE if the point could not be computed.
c
      implicit none

      double precision bot
      logical flag
      logical line_exp_is_degenerate_nd
      double precision p1(2)
      double precision p2(2)
      double precision p3(2)
      double precision p4(2)
      double precision r8_huge
      double precision t

      flag = .false.

      if ( line_exp_is_degenerate_nd ( 2, p1, p2 ) ) then
        flag = .true.
        p4(1:2) = r8_huge ( )
        return
      end if

      bot = ( p2(1) - p1(1) )**2 + ( p2(2) - p1(2) )**2
c
c  (P3-P1) dot (P2-P1) = Norm(P3-P1) * Norm(P2-P1) * Cos(Theta).
c
c  (P3-P1) dot (P2-P1) / Norm(P3-P1)**2 = normalized coordinate T
c  of the projection of (P3-P1) onto (P2-P1).
c
      t = ( ( p1(1) - p3(1) ) * ( p1(1) - p2(1) ) 
     &    + ( p1(2) - p3(2) ) * ( p1(2) - p2(2) ) ) / bot

      p4(1:2) = p1(1:2) + t * ( p2(1:2) - p1(1:2) )

      return
      end
      subroutine line_exp2imp_2d ( p1, p2, a, b, c )

c*********************************************************************72
c
cc LINE_EXP2IMP_2D converts an explicit line to implicit form in 2D.
c
c  Discussion:
c
c    The explicit form of a line in 2D is:
c
c      the line through the points P1 and P2.
c
c    The implicit form of a line in 2D is:
c
c      A * X + B * Y + C = 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), two points on the line.
c
c    Output, double precision A, B, C, the implicit form of the line.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical line_exp_is_degenerate_nd
      double precision norm
      double precision p1(2)
      double precision p2(2)
c
c  Take care of degenerate cases.
c
      if ( line_exp_is_degenerate_nd ( 2, p1, p2 ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LINE_EXP2IMP_2D - Warning!'
        write ( *, '(a)' ) '  The line is degenerate.'
      end if

      a = p2(2) - p1(2)
      b = p1(1) - p2(1)
      c = p2(1) * p1(2) - p1(1) * p2(2)

      norm = a * a + b * b + c * c

      if ( 0.0D+00 .lt. norm ) then
        a = a / norm
        b = b / norm
        c = c / norm
      end if

      if ( a .lt. 0.0D+00 ) then
        a = -a
        b = -b
        c = -c
      end if

      return
      end
      function line_imp_is_degenerate_2d ( a, b, c )

c*********************************************************************72
c
cc LINE_IMP_IS_DEGENERATE_2D finds if an implicit point is degenerate in 2D.
c
c  Discussion:
c
c    The implicit form of a line in 2D is:
c
c      A * X + B * Y + C = 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, C, the implicit line parameters.
c
c    Output, logical LINE_IMP_IS_DEGENERATE_2D, is true if the
c    line is degenerate.
c
      implicit none

      double precision a
      double precision b
      double precision c
      logical line_imp_is_degenerate_2d

      line_imp_is_degenerate_2d = ( a * a + b * b .eq. 0.0D+00 )

      return
      end
      subroutine lines_exp_int_2d ( p1, p2, q1, q2, ival, p )

c*********************************************************************72
c
cc LINES_EXP_INT_2D determines where two explicit lines intersect in 2D.
c
c  Discussion:
c
c    The explicit form of a line in 2D is:
c
c      the line through the points P1 and P2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision P1(2), P2(2), two points on the first line.
c
c    Input, double precision Q1(2), Q2(2), two points on the second line.
c
c    Output, integer IVAL, reports on the intersection:
c    0, no intersection, the lines may be parallel or degenerate.
c    1, one intersection point, returned in P.
c    2, infinitely many intersections, the lines are identical.
c
c    Output, double precision P(2), if IVAl = 1, P is
c    the intersection point.  Otherwise, P = 0.
c
      implicit none

      double precision a1
      double precision a2
      double precision b1
      double precision b2
      double precision c1
      double precision c2
      integer ival
      logical point_1
      logical point_2
      double precision p(2)
      double precision p1(2)
      double precision p2(2)
      double precision q1(2)
      double precision q2(2)

      ival = 0
      p(1:2) = 0.0D+00
c
c  Check whether either line is a point.
c
      if ( p1(1) .eq. p2(1) .and. p1(2) .eq. p2(2) ) then
        point_1 = .true.
      else
        point_1 = .false.
      end if

      if ( q1(1) .eq. q2(1) .and. q1(2) .eq. q2(2) ) then
        point_2 = .true.
      else
        point_2 = .false.
      end if
c
c  Convert the lines to ABC format.
c
      if ( .not. point_1 ) then
        call line_exp2imp_2d ( p1, p2, a1, b1, c1 )
      end if

      if ( .not. point_2 ) then
        call line_exp2imp_2d ( q1, q2, a2, b2, c2 )
      end if
c
c  Search for intersection of the lines.
c
      if ( point_1 .and. point_2 ) then
        if ( p1(1) .eq. q1(1) .and. p1(2) .eq. q1(2) ) then
          ival = 1
          p(1) = p1(1)
          p(2) = p1(2)
        end if
      else if ( point_1 ) then
        if ( a2 * p1(1) + b2 * p1(2) .eq. c2 ) then
          ival = 1
          p(1) = p1(1)
          p(2) = p1(2)
        end if
      else if ( point_2 ) then
        if ( a1 * q1(1) + b1 * q1(2) .eq. c1 ) then
          ival = 1
          p(1) = q1(1)
          p(2) = q1(2)
        end if
      else
        call lines_imp_int_2d ( a1, b1, c1, a2, b2, c2, ival, p )
      end if

      return
      end
      subroutine lines_imp_int_2d ( a1, b1, c1, a2, b2, c2, ival, p )

c*********************************************************************72
c
cc LINES_IMP_INT_2D determines where two implicit lines intersect in 2D.
c
c  Discussion:
c
c    The implicit form of a line in 2D is:
c
c      A * X + B * Y + C = 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A1, B1, C1, define the first line.
c    At least one of A1 and B1 must be nonzero.
c
c    Input, double precision A2, B2, C2, define the second line.
c    At least one of A2 and B2 must be nonzero.
c
c    Output, integer IVAL, reports on the intersection.
c
c    -1, both A1 and B1 were zero.
c    -2, both A2 and B2 were zero.
c     0, no intersection, the lines are parallel.
c     1, one intersection point, returned in P.
c     2, infinitely many intersections, the lines are identical.
c
c    Output, double precision P(2), if IVAL = 1, then P is
c    the intersection point.  Otherwise, P = 0.
c
      implicit none

      double precision a(2,3)
      double precision a1
      double precision a2
      double precision b1
      double precision b2
      double precision c1
      double precision c2
      integer info
      integer ival
      logical line_imp_is_degenerate_2d
      double precision p(2)

      p(1:2) = 0.0D+00
c
c  Refuse to handle degenerate lines.
c
      if ( line_imp_is_degenerate_2d ( a1, b1, c1 ) ) then
        ival = -1
        return
      end if

      if ( line_imp_is_degenerate_2d ( a2, b2, c2 ) ) then
        ival = -2
        return
      end if
c
c  Set up and solve a linear system.
c
      a(1,1) = a1
      a(1,2) = b1
      a(1,3) = -c1

      a(2,1) = a2
      a(2,2) = b2
      a(2,3) = -c2

      call r8mat_solve ( 2, 1, a, info )
c
c  If the inverse exists, then the lines intersect at the solution point.
c
      if ( info .eq. 0 ) then

        ival = 1
        p(1:2) = a(1:2,3)
c
c  If the inverse does not exist, then the lines are parallel
c  or coincident.  Check for parallelism by seeing if the
c  C entries are in the same ratio as the A or B entries.
c
      else

        ival = 0

        if ( a1 .eq. 0.0D+00 ) then
          if ( b2 * c1 .eq. c2 * b1 ) then
            ival = 2
          end if
        else
          if ( a2 * c1 .eq. c2 * a1 ) then
            ival = 2
          end if
        end if

      end if

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a very large R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a "huge" value.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
      subroutine r8_swap ( x, y )

c*********************************************************************72
c
cc R8_SWAP swaps two R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 December 2000
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
      double precision t

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
          t = a(ipivot,i)
          a(ipivot,i) = a(j,i)
          a(j,i) = t
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
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
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
      subroutine triangle_angles_2d ( t, angle )

c*********************************************************************72
c
cc TRIANGLE_ANGLES_2D computes the angles of a triangle in 2D.
c
c  Discussion:
c
c    The law of cosines is used:
c
c      C^2 = A^2 + B^2 - 2 * A * B * COS ( GAMMA )
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
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision ANGLE(3), the angles opposite
c    sides P1-P2, P2-P3 and P3-P1, in radians.
c
      implicit none

      double precision a
      double precision angle(3)
      double precision arc_cosine
      double precision b
      double precision c
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(2,3)
c
c  Compute the length of each side.
c
      a = sqrt ( ( t(1,1) - t(1,2) )**2 + ( t(2,1) - t(2,2) )**2 )
      b = sqrt ( ( t(1,2) - t(1,3) )**2 + ( t(2,2) - t(2,3) )**2 )
      c = sqrt ( ( t(1,3) - t(1,1) )**2 + ( t(2,3) - t(2,1) )**2 )
c
c  Take care of ridiculous special cases.
c
      if ( a .eq. 0.0D+00 .and. 
     &     b .eq. 0.0D+00 .and. 
     &     c .eq. 0.0D+00 ) then
        angle(1:3) = 2.0D+00 * r8_pi / 3.0D+00
        return
      end if

      if ( c .eq. 0.0D+00 .or. a .eq. 0.0D+00 ) then
        angle(1) = r8_pi
      else
        angle(1) = arc_cosine ( ( c * c + a * a - b * b ) 
     &    / ( 2.0D+00 * c * a ) )
      end if

      if ( a .eq. 0.0D+00 .or. b .eq. 0.0D+00 ) then
        angle(2) = r8_pi
      else
        angle(2) = arc_cosine ( ( a * a + b * b - c * c ) 
     &    / ( 2.0D+00 * a * b ) )
      end if

      if ( b .eq. 0.0D+00 .or. c .eq. 0.0D+00 ) then
        angle(3) = r8_pi
      else
        angle(3) = arc_cosine ( ( b * b + c * c - a * a ) 
     &    / ( 2.0D+00 * b * c ) )
      end if

      return
      end
      subroutine triangle_area_2d ( t, area )

c*********************************************************************72
c
cc TRIANGLE_AREA_2D computes the area of a triangle in 2D.
c
c  Discussion:
c
c    If the triangle's vertices are given in counter clockwise order,
c    the area will be positive.  If the triangle's vertices are given
c    in clockwise order, the area will be negative!
c
c    An earlier version of this routine always returned the absolute
c    value of the computed area.  I am convinced now that that is
c    a less useful result!  For instance, by returning the signed
c    area of a triangle, it is possible to easily compute the area
c    of a nonconvex polygon as the sum of the (possibly negative)
c    areas of triangles formed by node 1 and successive pairs of vertices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision AREA, the area of the triangle.
c
      implicit none

      double precision area
      double precision t(2,3)

      area = 0.5D+00 * (
     &    t(1,1) * ( t(2,2) - t(2,3) )
     &  + t(1,2) * ( t(2,3) - t(2,1) )
     &  + t(1,3) * ( t(2,1) - t(2,2) ) )

      return
      end
      subroutine triangle_centroid_2d ( t, centroid )

c*********************************************************************72
c
cc TRIANGLE_CENTROID_2D computes the centroid of a triangle in 2D.
c
c  Discussion:
c
c    The centroid of a triangle can also be considered the
c    center of gravity, or center of mass, assuming that the triangle
c    is made of a thin uniform sheet of massy material.
c
c    The centroid of a triangle is the intersection of the medians.
c
c    A median of a triangle is a line connecting a vertex to the
c    midpoint of the opposite side.
c
c    In barycentric coordinates, in which the vertices of the triangle
c    have the coordinates (1,0,0), (0,1,0) and (0,0,1), the centroid
c    has coordinates (1/3,1/3,1/3).
c
c    In geometry, the centroid of a triangle is often symbolized by "G".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 December 2004
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
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision CENTROID(2), the coordinates of the centroid.
c
      implicit none

      double precision centroid(2)
      integer i
      integer j
      double precision t(2,3)

      do i = 1, 2
        centroid(i) = 0.0D+00
        do j = 1, 3
          centroid(i) = centroid(i) + t(i,j)
        end do
        centroid(i) = centroid(i) / 3.0D+00
      end do

      return
      end
      subroutine triangle_circumcircle_2d ( t, r, pc )

c*********************************************************************72
c
cc TRIANGLE_CIRCUMCIRCLE_2D computes the circumcircle of a triangle in 2D.
c
c  Discussion:
c
c    The circumcenter of a triangle is the center of the circumcircle, the
c    circle that passes through the three vertices of the triangle.
c
c    The circumcircle contains the triangle, but it is not necessarily the
c    smallest triangle to do so.
c
c    If all angles of the triangle are no greater than 90 degrees, then
c    the center of the circumscribed circle will lie inside the triangle.
c    Otherwise, the center will lie outside the triangle.
c
c    The circumcenter is the intersection of the perpendicular bisectors
c    of the sides of the triangle.
c
c    In geometry, the circumcenter of a triangle is often symbolized by "O".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision R, PC(2), the circumradius and circumcenter
c    of the triangle.
c
      implicit none

      double precision a
      double precision b
      double precision bot
      double precision c
      double precision det
      double precision f(2)
      double precision pc(2)
      double precision r
      double precision top(2)
      double precision t(2,3)
c
c  Circumradius.
c
      a = sqrt ( ( t(1,2) - t(1,1) )**2 + ( t(2,2) - t(2,1) )**2 )
      b = sqrt ( ( t(1,3) - t(1,2) )**2 + ( t(2,3) - t(2,2) )**2 )
      c = sqrt ( ( t(1,1) - t(1,3) )**2 + ( t(2,1) - t(2,3) )**2 )

      bot = ( a + b + c ) * ( - a + b + c ) * (   a - b + c ) 
     &  * (   a + b - c )

      if ( bot .le. 0.0D+00 ) then
        r = -1.0D+00
        pc(1:2) = 0.0D+00
        return
      end if

      r = a * b * c / sqrt ( bot )
c
c  Circumcenter.
c
      f(1) = ( t(1,2) - t(1,1) )**2 + ( t(2,2) - t(2,1) )**2
      f(2) = ( t(1,3) - t(1,1) )**2 + ( t(2,3) - t(2,1) )**2

      top(1) =   ( t(2,3) - t(2,1) ) * f(1) - ( t(2,2) - t(2,1) ) * f(2)
      top(2) = - ( t(1,3) - t(1,1) ) * f(1) + ( t(1,2) - t(1,1) ) * f(2)

      det  =    ( t(2,3) - t(2,1) ) * ( t(1,2) - t(1,1) )
     &        - ( t(2,2) - t(2,1) ) * ( t(1,3) - t(1,1) )

      pc(1) = t(1,1) + 0.5D+00 * top(1) / det
      pc(2) = t(2,1) + 0.5D+00 * top(2) / det

      return
      end
      subroutine triangle_edge_length_2d ( t, edge_length )

c*********************************************************************72
c
cc TRIANGLE_EDGE_LENGTH_2D returns edge lengths of a triangle in 2D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision EDGE_LENGTH(3), the length of the edges.
c
      implicit none

      double precision edge_length(3)
      integer i4_wrap
      integer j1
      integer j2
      double precision t(2,3)
      double precision temp

      do j1 = 1, 3
        j2 = i4_wrap ( j1 + 1, 1, 3 )
        temp = sqrt ( ( t(1,j2) - t(1,j1) )**2 
     &              + ( t(2,j2) - t(2,j1) )**2 )
        edge_length(j1) = temp
      end do

      return
      end
      subroutine triangle_incircle_2d ( t, r, pc )

c*********************************************************************72
c
cc TRIANGLE_INCIRCLE_2D computes the inscribed circle of a triangle in 2D.
c
c  Discussion:
c
c    The inscribed circle of a triangle is the largest circle that can
c    be drawn inside the triangle.  It is tangent to all three sides,
c    and the lines from its center to the vertices bisect the angles
c    made by each vertex.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2004
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
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision R, PC(2), the radius and center of the
c    inscribed circle.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision pc(2)
      double precision perimeter
      double precision r
      double precision t(2,3)
c
c  Compute the length of each side.
c
      a = sqrt ( ( t(1,2) - t(1,1) )**2 + ( t(2,2) - t(2,1) )**2 )
      b = sqrt ( ( t(1,3) - t(1,2) )**2 + ( t(2,3) - t(2,2) )**2 )
      c = sqrt ( ( t(1,1) - t(1,3) )**2 + ( t(2,1) - t(2,3) )**2 )

      perimeter = a + b + c

      if ( perimeter .eq. 0.0D+00 ) then
        pc(1:2) = t(1:2,1)
        r = 0.0D+00
        return
      end if

      pc(1:2) = (
     &    b * t(1:2,1)
     &  + c * t(1:2,2)
     &  + a * t(1:2,3) ) / perimeter

      r = 0.5D+00 * sqrt (
     &    ( - a + b + c )
     &  * ( + a - b + c ) 
     &  * ( + a + b - c ) / perimeter )

      return
      end
      function triangle_orientation_2d ( t )

c*********************************************************************72
c
cc TRIANGLE_ORIENTATION_2D determines the orientation of a triangle in 2D.
c
c  Discussion:
c
c    Three distinct non-colinear points in the plane define a circle.
c    If the points are visited in the order P1, P2, and then
c    P3, this motion defines a clockwise or counter clockwise
c    rotation along the circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, integer TRIANGLE_ORIENTATION_2D, reports if the
c    three points lie clockwise on the circle that passes through them.
c    The possible return values are:
c    0, the points are distinct, noncolinear, and lie counter clockwise
c    on their circle.
c    1, the points are distinct, noncolinear, and lie clockwise
c    on their circle.
c    2, the points are distinct and colinear.
c    3, at least two of the points are identical.
c
      implicit none

      double precision det
      integer triangle_orientation_2d
      double precision t(2,3)

      if ( ( t(1,1) .eq. t(1,2) .and. t(2,1) .eq. t(2,2) ) .or.
     &     ( t(1,2) .eq. t(1,3) .and. t(2,2) .eq. t(2,3) ) .or.
     &     ( t(1,3) .eq. t(1,1) .and. t(2,3) .eq. t(2,1) ) ) then
        triangle_orientation_2d = 3
        return
      end if

      det = ( t(1,1) - t(1,3) ) * ( t(2,2) - t(2,3) )
     &    - ( t(1,2) - t(1,3) ) * ( t(2,1) - t(2,3) )

      if ( det .eq. 0.0D+00 ) then
        triangle_orientation_2d = 2
      else if ( det .lt. 0.0D+00 ) then
        triangle_orientation_2d = 1
      else if ( 0.0D+00 .lt. det ) then
        triangle_orientation_2d = 0
      end if

      return
      end
      subroutine triangle_orthocenter_2d ( t, pc, flag )

c*********************************************************************72
c
cc TRIANGLE_ORTHOCENTER_2D computes the orthocenter of a triangle in 2D.
c
c  Discussion:
c
c    The orthocenter is defined as the intersection of the three altitudes
c    of a triangle.
c
c    An altitude of a triangle is the line through a vertex of the triangle
c    and perpendicular to the opposite side.
c
c    In geometry, the orthocenter of a triangle is often symbolized by "H".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2009
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
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision PC(2), the orthocenter of the triangle.
c
c    Output, logical FLAG, is TRUE if there was an error condition.
c
      implicit none

      logical flag
      integer ival
      double precision p23(2)
      double precision p31(2)
      double precision pc(2)
      double precision r8_huge
      double precision t(2,3)
c
c  Determine a point P23 common to the line (P2,P3) and
c  its perpendicular through P1.
c
      call line_exp_perp_2d ( t(1:2,2), t(1:2,3), t(1:2,1), p23, flag )

      if ( flag ) then
        pc(1:2) = r8_huge ( )
        return
      end if
c
c  Determine a point P31 common to the line (P3,P1) and
c  its perpendicular through P2.
c
      call line_exp_perp_2d ( t(1:2,3), t(1:2,1), t(1:2,2), p31, flag )

      if ( flag ) then
        pc(1:2) = r8_huge ( )
        return
      end if
c
c  Determine PC, the intersection of the lines (P1,P23) and (P2,P31).
c
      call lines_exp_int_2d ( t(1:2,1), p23(1:2), t(1:2,2), p31(1:2), 
     &  ival, pc )

      if ( ival .ne. 1 ) then
        flag = .true.
        pc(1:2) = r8_huge ( )
        return
      end if

      return
      end
      subroutine triangle_quality_2d ( t, quality )

c*********************************************************************72
c
cc TRIANGLE_QUALITY_2D: "quality" of a triangle in 2D.
c
c  Discussion:
c
c    The quality of a triangle is 2.0 times the ratio of the radius of
c    the inscribed circle divided by that of the circumscribed circle.
c    An equilateral triangle achieves the maximum possible quality of 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 July 2009
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
c    Input, double precision T(2,3), the triangle vertices.
c
c    Output, double precision QUALITY, the quality of the triangle.
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision quality
      double precision t(2,3)
c
c  Compute the length of each side.
c
      a = sqrt ( ( t(1,2) - t(1,1) )**2 + ( t(2,2) - t(2,1) )**2 )
      b = sqrt ( ( t(1,3) - t(1,2) )**2 + ( t(2,3) - t(2,2) )**2 )
      c = sqrt ( ( t(1,1) - t(1,3) )**2 + ( t(2,1) - t(2,3) )**2 )

      if ( a * b * c .eq. 0.0D+00 ) then
        quality = 0.0D+00
      else
        quality = ( - a + b + c ) * ( a - b + c ) 
     &    * ( a + b - c ) / ( a * b * c )
      end if

      return
      end

