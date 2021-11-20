      program main

c*********************************************************************72
c
cc MAIN is the main program for HYPERCUBE_EXACTNESS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer arg_num
      integer degree_max
      integer dim
      integer dim_num
      integer iarg
      integer iargc
      integer ierror
      integer ios
      integer last
      integer point_num
      character * ( 255 ) quad_filename
      character * ( 255 ) quad_r_filename
      character * ( 255 ) quad_w_filename
      character * ( 255 ) quad_x_filename
      character * ( 255 ) string

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Investigate the polynomial exactness of a quadrature'
      write ( *, '(a)' ) 
     &  '  rule by integrating all monomials of a given degree'
      write ( *, '(a)' ) '  over the [0,1] hypercube.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The rule will be adjusted to the [0,1] hypercube.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the quadrature file root name:
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, quad_filename )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS:'
        write ( *, '(a)' ) 
     &    '  Enter the "root" name of the quadrature files.'

        read ( *, '(a)' ) quad_filename

      end if
c
c  Create the names of:
c    the quadrature X file;
c    the quadrature W file;
c    the quadrature R file;
c    the output "exactness" file.
c
      quad_x_filename = trim ( quad_filename ) // '_x.txt'
      quad_w_filename = trim ( quad_filename ) // '_w.txt'
      quad_r_filename = trim ( quad_filename ) // '_r.txt'
c
c  The second command line argument is the maximum degree.
c
      if ( 2 .le. arg_num ) then

        iarg = 2
        call getarg ( iarg, string )
        call s_to_i4 ( string, degree_max, ierror, last )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS:'
        write ( *, '(a)' ) '  Enter the maximum total degree to check.'

        read ( *, * ) degree_max

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS: User input:'
      write ( *, '(a)' ) '  Quadrature rule X file = "'     // 
     &  trim ( quad_x_filename ) // '".'
      write ( *, '(a)' ) '  Quadrature rule W file = "'     // 
     &  trim ( quad_w_filename ) // '".'
      write ( *, '(a)' ) '  Quadrature rule R file = "'     // 
     &  trim ( quad_r_filename ) // '".'
      write ( *, '(a,i8)' ) '  Maximum total degree to check = ',
     &  degree_max
c
c  Read the X file.
c
      call r8mat_header_read ( quad_x_filename, dim_num, point_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension = ', dim_num
      write ( *, '(a,i8)' ) '  Number of points  = ', point_num
c
c  Call subroutine so we can easily allocate space.
c
      call hypercube_exactness_sub ( dim_num, point_num, degree_max, 
     &  quad_r_filename, quad_w_filename, quad_x_filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine hypercube_exactness_sub ( dim_num, point_num, 
     &  degree_max, quad_r_filename, quad_w_filename, quad_x_filename )

c*********************************************************************72
c
cc HYPERCUBE_EXACTNESS_SUB carries out part of HYPERCUBE_EXACTNESS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 April 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      integer point_num

      integer degree
      integer degree_max
      integer dim
      integer dim_num2
      integer expon(dim_num)
      integer h
      integer i
      integer j
      integer last
      logical more
      integer point_num2
      double precision quad_error
      character * ( * ) quad_r_filename
      character * ( * ) quad_w_filename
      character * ( * ) quad_x_filename
      integer t
      double precision volume
      double precision weight(point_num)
      double precision x(dim_num,point_num)
      double precision x_range(dim_num,2)
c
c  Read the X data.
c
      call r8mat_data_read ( quad_x_filename, dim_num, point_num, x )
c
c  Read the W file.
c
      call r8mat_header_read ( quad_w_filename, dim_num2, point_num2 )

      if ( dim_num2 .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The quadrature weight file should have exactly'
        write ( *, '(a)' ) '  one value on each line.'
        stop 1
      end if

      if ( point_num2 .ne. point_num ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The quadrature weight file should have exactly'
        write ( *, '(a)' ) 
     &    '  the same number of lines as the abscissa file.'
        stop 1
      end if

      call r8mat_data_read ( quad_w_filename, 1, point_num, weight )
c
c  Read the R file.
c
      call r8mat_header_read ( quad_r_filename, dim_num2, point_num2 )

      if ( dim_num2 .ne. dim_num ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The quadrature region file should have the'
        write ( *, '(a)' ) '  same number of values on each line as the'
        write ( *, '(a)' ) '  abscissa file does.'
        stop 1
      end if

      if ( point_num2 .ne. 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HYPERCUBE_EXACTNESS - Fatal error!'
        write ( *, '(a)' ) 
     &    '  The quadrature region file should have two lines.'
        stop 1
      end if

      call r8mat_data_read ( quad_r_filename, dim_num, 2, x_range )
c
c  Rescale the weights, and translate the abscissas.
c
      volume = 1.0D+00
      do i = 1, dim_num
        volume = volume * abs ( ( x_range(i,2) - x_range(i,1) ) )
      end do

      do j = 1, point_num
        weight(j) = weight(j) / volume
      end do

      do j = 1, point_num
        do i = 1, dim_num
          x(i,j) = ( x(i,j) - x_range(i,1) ) 
     &      / ( x_range(i,2) - x_range(i,1) )
        end do
      end do
c
c  Explore the monomials.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          Error          Degree  Exponents'

      do degree = 0, degree_max

        more = .false.
        h = 0
        t = 0

        write ( *, '(a)' ) ' '

10      continue

          call comp_next ( degree, dim_num, expon, more, h, t )

          call monomial_quadrature ( dim_num, expon, point_num, weight, 
     &      x, quad_error )

          write ( *, '(2x,f24.16,3x,i2,4x,100i3)' )
     &      quad_error, degree, expon(1:dim_num)

          if ( .not. more ) then
            go to 20
          end if

        go to 10

20      continue

      end do

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
      subroutine comp_next ( n, k, a, more, h, t )

c*********************************************************************72
c
cc COMP_NEXT computes the compositions of the integer N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to N.  The compositions (1,2,1)
c    and (1,1,2) are considered to be distinct.
c
c    The routine computes one composition on each call until there are no more.
c    For instance, one composition of 6 into 3 parts is
c    3+2+1, another would be 6+0+0.
c
c    On the first call to this routine, set MORE = FALSE.  The routine
c    will compute the first element in the sequence of compositions, and
c    return it, as well as setting MORE = TRUE.  If more compositions
c    are desired, call again, and again.  Each time, the routine will
c    return with a new composition.
c
c    However, when the LAST composition in the sequence is computed 
c    and returned, the routine will reset MORE to FALSE, signaling that
c    the end of the sequence has been reached.
c
c    This routine originally used a SAVE statement to maintain the
c    variables H and T.  I have decided (based on an wasting an
c    entire morning trying to track down a problem) that it is safer
c    to pass these variables as arguments, even though the user should
c    never alter them.  This allows this routine to safely shuffle
c    between several ongoing calculations.
c
c
c    There are 28 compositions of 6 into three parts.  This routine will
c    produce those compositions in the following order:
c
c     I         A
c     -     ---------
c     1     6   0   0
c     2     5   1   0
c     3     4   2   0
c     4     3   3   0
c     5     2   4   0
c     6     1   5   0
c     7     0   6   0
c     8     5   0   1
c     9     4   1   1
c    10     3   2   1
c    11     2   3   1
c    12     1   4   1
c    13     0   5   1
c    14     4   0   2
c    15     3   1   2
c    16     2   2   2
c    17     1   3   2
c    18     0   4   2
c    19     3   0   3
c    20     2   1   3
c    21     1   2   3
c    22     0   3   3
c    23     2   0   4
c    24     1   1   4
c    25     0   2   4
c    26     1   0   5
c    27     0   1   5
c    28     0   0   6
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2008
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the integer whose compositions are desired.
c
c    Input, integer K, the number of parts in the composition.
c
c    Input/output, integer A(K), the parts of the composition.
c
c    Input/output, logical MORE, set by the user to start the computation,
c    and by the routine to terminate it.
c
c    Input/output, integer H, T, two internal parameters needed for the
c    computation.  The user should allocate space for these in the calling
c    program, include them in the calling sequence, but never alter them!
c
      implicit none

      integer k

      integer a(k)
      integer h
      integer i
      logical more
      integer n
      integer t
c
c  The first computation.
c
      if ( .not. more ) then

        t = n
        h = 0
        a(1) = n
        do i = 2, k
          a(i) = 0
        end do
c
c  The next computation.
c
      else

        if ( 1 .lt. t ) then
          h = 0
        end if

        h = h + 1
        t = a(h)
        a(h) = 0
        a(1) = t - 1
        a(h+1) = a(h+1) + 1

      end if
c
c  This is the last element of the sequence if all the
c  items are in the last slot.
c
      more = ( a(k) .ne. n )

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
      subroutine hypercube01_monomial_integral ( m, e, integral )

c*********************************************************************72
c
cc HYPERCUBE01_MONOMIAL_INTEGRAL: integral in unit hypercube in M dimensions.
c
c  Discussion:
c
c    The integration region is 
c
c      0 <= X(1:M) <= 1.
c
c    The monomial is F(X) = product ( 1 <= I <= M ) X(I)^E(I).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer E(M), the exponents.  
c    Each exponent must be nonnegative.
c
c    Output, double precision INTEGRAL, the integral.
c
      implicit none

      integer m

      integer e(m)
      integer i
      double precision integral

      do i = 1, m
        if ( e(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 
     &      'HYPERCUBE01_MONOMIAL_INTEGRAL - Fatal error!'
          write ( *, '(a)' ) '  All exponents must be nonnegative.'
          stop 1
        end if
      end do

      integral = 1.0D+00
      do i = 1, m
        integral = integral / dble ( e(i) + 1 )
      end do

      return
      end
      subroutine monomial_quadrature ( dim_num, expon, point_num, weight
     &, x,   quad_error )

c*********************************************************************72
c
cc MONOMIAL_QUADRATURE applies a quadrature rule to a monomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 August 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer EXPON(DIM_NUM), the exponents.
c
c    Input, integer POINT_NUM, the number of points in the rule.
c
c    Input, double precision WEIGHT(POINT_NUM), the quadrature weights.
c
c    Input, double precision X(DIM_NUM,POINT_NUM), the quadrature points.
c
c    Output, double precision QUAD_ERROR, the quadrature error.
c
      implicit none

      integer dim_num

      double precision exact
      integer expon(dim_num)
      integer point_num
      double precision quad
      double precision quad_error
      double precision r8vec_dot_product
      double precision scale
      double precision value(point_num)
      double precision weight(point_num)
      double precision x(dim_num,point_num)
c
c  Get the exact value of the integral of the unscaled monomial.
c
      call hypercube01_monomial_integral ( dim_num, expon, scale )
c
c  Evaluate the monomial at the quadrature points.
c
      call monomial_value ( dim_num, point_num, expon, x, value )
c
c  Compute the weighted sum and divide by the exact value.
c
      quad = r8vec_dot_product ( point_num, weight, value ) / scale
c
c  Error:
c
      exact = 1.0D+00
      quad_error = abs ( quad - exact )

      return
      end
      subroutine monomial_value ( m, n, e, x, v )

c*****************************************************************************80
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 <= i <= m ) x(i)^e(i)
c
c    The combination 0.0^0 is encountered is treated as 1.0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the point coordinates.
c
c    Output, double precision V(N), the monomial values.
c
      implicit none

      integer m
      integer n

      integer e(m)
      integer i
      integer j
      double precision v(n)
      double precision x(m,n)

      do j = 1, n
        v(j) = 1.0D+00
      end do

      do i = 1, m
        if ( 0 .ne. e(i) ) then
          do j = 1, n
            v(j) = v(j) * x(i,j) ** e(i)
          end do
        end if
      end do

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
