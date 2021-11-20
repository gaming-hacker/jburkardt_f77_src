      function between ( xa, ya, xb, yb, xc, yc )

c*********************************************************************72
c
cc BETWEEN is TRUE if vertex C is between vertices A and B.
c
c  Discussion:
c
c    The points must be (numerically) collinear.
c
c    Given that condition, we take the greater of XA - XB and YA - YB
c    as a "scale" and check where C's value lies.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, double precision XA, YA, XB, YB, XC, YC, the coordinates of
c    the vertices.
c
c    Output, logical BETWEEN, is TRUE if C is between A and B.
c
      implicit none

      logical between
      logical collinear
      logical value
      double precision xa
      double precision xb
      double precision xc
      double precision xmax
      double precision xmin
      double precision ya
      double precision yb
      double precision yc
      double precision ymax
      double precision ymin

      if ( .not. collinear ( xa, ya, xb, yb, xc, yc ) ) then
        value = .false.
      else if ( abs ( ya - yb ) .lt. abs ( xa - xb ) ) then
        xmax = max ( xa, xb )
        xmin = min ( xa, xb )
        value = ( xmin .le. xc .and. xc .le. xmax )
      else
        ymax = max ( ya, yb )
        ymin = min ( ya, yb )
        value = ( ymin .le. yc .and. yc .le. ymax )
      end if

      between = value

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
      function collinear ( xa, ya, xb, yb, xc, yc )

c*********************************************************************72
c
cc COLLINEAR returns a measure of collinearity for three points.
c
c  Discussion:
c
c    In order to deal with collinear points whose coordinates are not
c    numerically exact, we compare the area of the largest square
c    that can be created by the line segment between two of the points
c    to (twice) the area of the triangle formed by the points.
c
c    If the points are collinear, their triangle has zero area.
c    If the points are close to collinear, then the area of this triangle
c    will be small relative to the square of the longest segment.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, double precision XA, YA, XB, YB, XC, YC, the coordinates of
c    the vertices.
c
c    Output, logical COLLINEAR, is TRUE if the points are judged
c    to be collinear.
c
      implicit none

      double precision area
      logical collinear
      double precision r8_eps
      parameter ( r8_eps = 2.220446049250313D-016 )
      double precision side_ab_sq
      double precision side_bc_sq
      double precision side_ca_sq
      double precision side_max_sq
      logical value
      double precision xa
      double precision xb
      double precision xc
      double precision ya
      double precision yb
      double precision yc

      area = 0.5D+00 * (       
     &    ( xb - xa ) * ( yc - ya )     
     &  - ( xc - xa ) * ( yb - ya ) )

      side_ab_sq = ( xa - xb ) ** 2 + ( ya - yb ) ** 2
      side_bc_sq = ( xb - xc ) ** 2 + ( yb - yc ) ** 2
      side_ca_sq = ( xc - xa ) ** 2 + ( yc - ya ) ** 2

      side_max_sq = max ( side_ab_sq, max ( side_bc_sq, side_ca_sq ) )

      if ( side_max_sq .le. r8_eps ) then
        value = .true.
      else if ( 2.0D+00 * abs ( area ) .le. r8_eps * side_max_sq ) then
        value = .true.
      else
        value = .false.
      end if

      collinear = value

      return
      end
      function diagonal ( im1, ip1, n, prev, next, x, y )

c*********************************************************************72
c
cc DIAGONAL: VERTEX(IM1) to VERTEX(IP1) is a proper internal diagonal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, integer IM1, IP1, the indices of two vertices.
c
c    Input, integer N, the number of vertices.
c
c    Input, integer PREV(N), the previous neighbor of each vertex.
c
c    Input, integer NEXT(N), the next neighbor of each vertex.
c
c    Input, double precision X(N), Y(N), the coordinates of each vertex.
c
c    Output, logical DIAGONAL, the value of the test.
c
      implicit none

      integer n

      logical diagonal
      logical diagonalie
      integer im1
      logical in_cone
      integer ip1
      integer next(n)
      integer prev(n)
      logical value1
      logical value2
      logical value3
      double precision x(n)
      double precision y(n)

      value1 = in_cone ( im1, ip1, n, prev, next, x, y )
      value2 = in_cone ( ip1, im1, n, prev, next, x, y )
      value3 = diagonalie ( im1, ip1, n, next, x, y )

      diagonal = ( value1 .and. value2 .and. value3 )

      return
      end
      function diagonalie ( im1, ip1, n, next, x, y )

c*********************************************************************72
c
cc DIAGONALIE is true if VERTEX(IM1):VERTEX(IP1) is a proper diagonal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, integer IM1, IP1, the indices of two vertices.
c
c    Input, integer N, the number of vertices.
c
c    Input, integer NEXT(N), the next neighbor of each vertex.
c
c    Input, double precision X(N), Y(N), the coordinates of each vertex.
c
c    Output, logical DIAGONALIE, the value of the test.
c
      implicit none

      integer n

      logical diagonalie
      integer first
      integer im1
      logical intersect
      integer ip1
      integer j
      integer jp1
      integer next(n)
      logical value
      logical value2
      double precision x(n)
      double precision y(n)

      first = im1
      j = first
      jp1 = next(first)

      value = .true.
c
c  For each edge VERTEX(J):VERTEX(JP1) of the polygon:
c
10    continue
c
c  Skip any edge that includes vertex IM1 or IP1.
c
        if ( j   .eq. im1 .or. 
     &       j   .eq. ip1 .or. 
     &       jp1 .eq. im1 .or. 
     &       jp1 .eq. ip1 ) then

        else

          value2 = intersect ( x(im1), y(im1), x(ip1), y(ip1), x(j), 
     &      y(j), x(jp1), y(jp1) )

          if ( value2 ) then
            value = .false.
            go to 20
          end if

        end if

        j = jp1
        jp1 = next(j)

        if ( j .eq. first ) then
          go to 20
        end if

      go to 10

20    continue

      diagonalie = value

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
     &  status = 'old',  form = 'formatted', access = 'sequential' )
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
      subroutine i4mat_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_PRINT prints an I4MAT.
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
c    30 June 2003
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
c    Input, integer A(M,N), the matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer ihi
      integer ilo
      integer jhi
      integer jlo
      character*(*) title

      ilo = 1
      ihi = m
      jlo = 1
      jhi = n

      call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

      return
      end
      subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc I4MAT_PRINT_SOME prints some of an I4MAT.
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
c    04 November 2003
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
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*(8) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i8)' ) j
        end do

        write ( *, '(''  Col '',10a8)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

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
      function in_cone ( im1, ip1, n, prev, next, x, y )

c*********************************************************************72
c
cc IN_CONE is TRUE if the diagonal VERTEX(IM1):VERTEX(IP1) is strictly internal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, integer IM1, IP1, the indices of two vertices.
c
c    Input, integer N, the number of vertices.
c
c    Input, integer PREV(N), the previous neighbor of each vertex.
c
c    Input, integer NEXT(N), the next neighbor of each vertex.
c
c    Input, double precision X(N), Y(N), the coordinates of each vertex.
c
c    Output, logical IN_CONE, the value of the test.
c
      implicit none

      integer n

      integer i
      integer im1
      integer im2
      logical in_cone
      integer ip1
      integer next(n)
      integer prev(n)
      double precision t1
      double precision t2
      double precision t3
      double precision t4
      double precision t5
      double precision triangle_area
      logical value
      double precision x(n)
      double precision y(n)

      im2 = prev(im1)
      i = next(im1)

      t1 = triangle_area ( x(im1), y(im1), x(i), y(i), x(im2), y(im2) )

      if ( 0.0D+00 .le. t1 ) then

        t2 = triangle_area ( x(im1), y(im1), x(ip1), y(ip1), x(im2), 
     &    y(im2) )
        t3 = triangle_area ( x(ip1), y(ip1), x(im1), y(im1), x(i), 
     &    y(i) )
        value = ( ( 0.0D+00 .lt. t2 ) .and. ( 0.0D+00 .lt. t3 ) )

      else

        t4 = triangle_area ( x(im1), y(im1), x(ip1), y(ip1), x(i), 
     &    y(i) )
        t5 = triangle_area ( x(ip1), y(ip1), x(im1), y(im1), x(im2), 
     &    y(im2) )
        value = .not. ( ( 0.0D+00 .le. t4 ) .and. ( 0.0D+00 .le. t5 ) )

      end if

      in_cone = value

      return
      end
      function intersect ( xa, ya, xb, yb, xc, yc, xd, yd )

c*********************************************************************72
c
cc INTERSECT is true if lines VA:VB and VC:VD intersect.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, double precision XA, YA, XB, YB, XC, YC, XD, YD, the X and Y
c    coordinates of the four vertices.
c
c    Output, logical VALUE, the value of the test.
c
      implicit none

      logical between
      logical intersect
      logical intersect_prop
      logical value
      double precision xa
      double precision xb
      double precision xc
      double precision xd
      double precision ya
      double precision yb
      double precision yc
      double precision yd

      if ( intersect_prop ( xa, ya, xb, yb, xc, yc, xc, yd ) ) then
        value = .true.
      else if ( between ( xa, ya, xb, yb, xc, yc ) ) then
        value = .true.
      else if ( between ( xa, ya, xb, yb, xd, yd ) ) then
        value = .true.
      else if ( between ( xc, yc, xd, yd, xa, ya ) ) then
        value = .true.
      else if ( between ( xc, yc, xd, yd, xb, yb ) ) then
        value = .true.
      else
        value = .false.
      end if

      intersect = value

      return
      end
      function intersect_prop ( xa, ya, xb, yb, xc, yc, xd, yd )

c*********************************************************************72
c
cc INTERSECT_PROP is TRUE if lines VA:VB and VC:VD have a proper intersection.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, double precision XA, YA, XB, YB, XC, YC, XD, YD, the X and Y
c    coordinates of the four vertices.
c
c    Output, logical INTERSECT_PROP, the result of the test.
c
      implicit none

      logical collinear
      logical intersect_prop
      logical l4_xor
      double precision t1
      double precision t2
      double precision t3
      double precision t4
      double precision triangle_area
      logical value
      logical value1
      logical value2
      logical value3
      logical value4
      double precision xa
      double precision xb
      double precision xc
      double precision xd
      double precision ya
      double precision yb
      double precision yc
      double precision yd

      if ( collinear ( xa, ya, xb, yb, xc, yc ) ) then
        value = .false.
      else if ( collinear ( xa, ya, xb, yb, xd, yd ) ) then
        value = .false.
      else if ( collinear ( xc, yc, xd, yd, xa, ya ) ) then
        value = .false.
      else if ( collinear ( xc, yc, xd, yd, xb, yb ) ) then
        value = .false.
      else

        t1 = triangle_area ( xa, ya, xb, yb, xc, yc )
        t2 = triangle_area ( xa, ya, xb, yb, xd, yd )
        t3 = triangle_area ( xc, yc, xd, yd, xa, ya )
        t4 = triangle_area ( xc, yc, xd, yd, xb, yb )

        value1 = ( 0.0D+00 .lt. t1 )
        value2 = ( 0.0D+00 .lt. t2 )
        value3 = ( 0.0D+00 .lt. t3 )
        value4 = ( 0.0D+00 .lt. t4 )

        value = ( l4_xor ( value1, value2 ) ) .and. 
     &          ( l4_xor ( value3, value4 ) )

      end if

      intersect_prop = value

      return
      end
      function l4_xor ( l1, l2 )

c*********************************************************************72
c
cc L4_XOR returns the exclusive OR of two L4's.
c
c  Discussion:
c
c    An L4 is a logical value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c   John Burkardt
c
c  Parameters:
c
c    Input, logical L1, L2, two values whose exclusive OR is needed.
c
c    Output, logical L4_XOR, the exclusive OR of L1 and L2.
c
      implicit none

      logical l1
      logical l2
      logical l4_xor
      logical value1
      logical value2

      value1 = (         l1   .and. ( .not. l2 ) )
      value2 = ( ( .not. l1 ) .and.         l2   )

      l4_xor = ( value1 .or. value2 )

      return
      end
      subroutine polygon_triangulate ( n, x, y, triangles )

c*********************************************************************72
c
cc POLYGON_TRIANGULATE determines a triangulation of a polygon.
c
c  Discussion:
c
c    There are N-3 triangles in the triangulation.
c
c    For the first N-2 triangles, the first edge listed is always an
c    internal diagonal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    Original C version by Joseph ORourke.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Joseph ORourke,
c    Computational Geometry in C,
c    Cambridge, 1998,
c    ISBN: 0521649765,
c    LC: QA448.D38.
c
c  Parameters:
c
c    Input, integer N, the number of vertices.
c
c    Input, double precision X(N), Y(N), the coordinates of each vertex.
c
c    Output, integer TRIANGLES(3,N-2), the triangles of the
c    triangulation.
c
      implicit none

      integer n

      double precision area
      logical diagonal
      logical ear(n)
      integer first
      integer i
      integer i0
      integer i1
      integer i2
      integer i3
      integer i4
      integer next(n)
      integer node
      integer node_m1
      integer prev(n)
      integer triangle_num
      integer triangles(3,n-2)
      double precision x(n)
      double precision y(n)
c
c  We must have at least 3 vertices.
c
      if ( n .lt. 3 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 3.'
        stop 1
      end if
c
c  Consecutive vertices cannot be equal.
c
      node_m1 = n
      do node = 1, n
        if ( x(node_m1) .eq. x(node) .and. 
     &       y(node_m1) .eq. y(node) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
          write ( *, '(a)' ) '  Two consecutive nodes are identical.'
          stop 1
        end if
        node_m1 = node
      end do
c
c  Area must be positive.
c
      area = 0.0D+00
      do node = 1, n - 2
        area = area + 0.5D+00 *
     &    ( ( x(node+1) - x(node) ) * ( y(node+2) - y(node) )
     &    - ( x(node+2) - x(node) ) * ( y(node+1) - y(node) ) )
      end do

      if ( area .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'POLYGON_TRIANGULATE - Fatal error!'
        write ( *, '(a)' ) '  Polygon has zero or negative area.'
        stop 1
      end if
c
c  PREV and NEXT point to the previous and next nodes.
c
      i = 1
      prev(i) = n
      next(i) = i + 1

      do i = 2, n - 1
        prev(i) = i - 1
        next(i) = i + 1
      end do

      i = n
      prev(i) = i - 1
      next(i) = 1
c
c  EAR indicates whether the node and its immediate neighbors form an ear
c  that can be sliced off immediately.
c
      do i = 1, n
        ear(i) = diagonal ( prev(i), next(i), n, prev, next, x, y )
      end do

      triangle_num = 0

      i2 = 1

10    continue

      if ( triangle_num .lt. n - 3 ) then
c
c  If I2 is an ear, gather information necessary to carry out
c  the slicing operation and subsequent "healing".
c
        if ( ear(i2) ) then

          i3 = next(i2)
          i4 = next(i3)
          i1 = prev(i2)
          i0 = prev(i1)
c
c  Make vertex I2 disappear.
c
          next(i1) = i3
          prev(i3) = i1
c
c  Update the earity of I1 and I3, because I2 disappeared.
c
          ear(i1) = diagonal ( i0, i3, n, prev, next, x, y )
          ear(i3) = diagonal ( i1, i4, n, prev, next, x, y )
c
c  Add the diagonal [I3, I1, I2] to the list.
c
          triangle_num = triangle_num + 1
          triangles(1,triangle_num) = i3
          triangles(2,triangle_num) = i1
          triangles(3,triangle_num) = i2

        end if
c
c  Try the next vertex.
c
        i2 = next(i2)

        go to 10

      end if
c
c  The last triangle is formed from the three remaining vertices.
c
      i3 = next(i2)
      i1 = prev(i2)

      triangle_num = triangle_num + 1
      triangles(1,triangle_num) = i3
      triangles(2,triangle_num) = i1
      triangles(3,triangle_num) = i2

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
      function triangle_area ( xa, ya, xb, yb, xc, yc )

c*********************************************************************72
c
cc TRIANGLE_AREA computes the signed area of a triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision XA, YA, XB, YB, XC, YC, the coordinates of
c    the vertices of the triangle, given in counterclockwise order.
c
c    Output, double precision TRIANGLE_AREA, the signed area of the triangle.
c
      implicit none

      double precision triangle_area
      double precision value
      double precision xa
      double precision xb
      double precision xc
      double precision ya
      double precision yb
      double precision yc

      value = 0.5D+00 * (
     &    ( xb - xa ) * ( yc - ya )     
     &  - ( xc - xa ) * ( yb - ya ) )

      triangle_area = value

      return
      end
