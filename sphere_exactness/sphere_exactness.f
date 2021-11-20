      program main

c*********************************************************************72
c
cc MAIN is the main program for SPHERE_EXACTNESS.
c
c  Discussion:
c
c    This program investigates the polynomial exactness of a quadrature
c    rule for the unit sphere.
c
c  Usage:
c
c    sphere_exactness files prefix degree_max
c
c    where
c
c    * files explains how the quadrature rule is stored:
c      'XYZW'  for file 'prefix.xyzw' containing (X,Y,Z,Weight);
c      'RTPW'  for file 'prefix.rtpw' containing (Theta, Phi, Weight) (radians);
c      'DTPW'  for file 'prefix.dtpw' containing (Theta, Phi, Weight) (degrees);
c      'XYZ+W' for file 'prefix.xyz' containing (X,Y,Z)
c              and file 'prefix.w' containing Weight;
c      'RTP+W' for file 'prefix.rtp' containing (Theta, Phi ) in radians,
c              and file 'prefix.w' containing Weight;
c      'DTP+W' for file 'prefix.dtp' containing (Theta, Phi ) in degrees,
c              and file 'prefix.w' containing Weight;
c      'XYZ1'  for file 'prefix.xyz' containing (X,Y,Z),
c              and equal weights, which do not need to be read in.
c      'RTP1'  for file 'prefix.rtp' containing (Theta, Phi ) in radians,
c              and equal weights, which do not need to be read in.
c      'DTP1'  for file 'prefix.dtp' containing (Theta, Phi ) in degrees,'
c              and equal weights, which do not need to be read in.
c    * prefix is the common file prefix;
c    * degree_max is the maximum monomial degree to check.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
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
      character * ( 255 ) filename
      character * ( 255 ) files
      integer i
      integer iarg
      integer iargc
      integer ierror
      integer ios
      integer j
      integer last
      integer point_num
      character * ( 255 ) prefix
      logical s_eqi
      character * ( 255 ) string

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_EXACTNESS'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Investigate the polynomial exactness of a quadrature'
      write ( *, '(a)' ) 
     &  '  rule for the unit sphere by integrating all monomials'
      write ( *, '(a)' ) '  of a given degree.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the file structure:
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, files )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_EXACTNESS:'
        write ( *, '(a)' ) '  Describe the files to be read:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  For coordinates and weights in one file:'
        write ( *, '(a)' ) '    XYZW     (X,Y,Z,Weight)'
        write ( *, '(a)' ) '    RTPW     (Theta, Phi, Weight) (radians)'
        write ( *, '(a)' ) '    DTPW     (Theta, Phi, Weight) (degrees)'
        write ( *, '(a)' ) 
     &    '  For coordinates in one file and weights in another:'
        write ( *, '(a)' ) '    XYZ+W    (X,Y,Z)       + Weight'
        write ( *, '(a)' ) '    RTP+W    (Theta, Phi ) + Weight'
        write ( *, '(a)' ) '    DTP+W    (Theta, Phi ) + Weight'
        write ( *, '(a)' ) 
     &    '  For coordinates in one file, and equal weights:'
        write ( *, '(a)' ) '    XYZ1     (X,Y,Z)'
        write ( *, '(a)' ) '    RTP1     (Theta, Phi ) (radians)'
        write ( *, '(a)' ) '    DTP1     (Theta, Phi ) (degrees)'

        read ( *, '(a)' ) files

      end if
c
c  Get the file prefix:
c
      if ( 2 .le. arg_num ) then

        iarg = 2
        call getarg ( iarg, prefix )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_EXACTNESS:'
        write ( *, '(a)' ) '  Enter the filename prefix.'

        read ( *, '(a)' ) prefix

      end if
c
c  The third command line argument is the maximum degree.
c
      if ( 3 .le. arg_num ) then

        iarg = 3
        call getarg ( iarg, string )
        call s_to_i4 ( string, degree_max, ierror, last )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_EXACTNESS:'
        write ( *, '(a)' ) 
     &    '  Please enter the maximum total degree to check.'

        read ( *, * ) degree_max

      end if
c
c  Summarize the input.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_EXACTNESS: User input:'
      write ( *, '(a)' ) 
     &  '  File structure = "' // trim ( files ) // '".'
      write ( *, '(a)' ) 
     &  '  Filename prefix = "' // trim ( prefix ) // '".'
      write ( *, '(a,i8)' ) '  Maximum degree = ', degree_max
c
c  Read headers needed to allocate XYZ and W arrays.
c
      if ( s_eqi ( files, 'xyzw' ) ) then

        filename = trim ( prefix ) // '.xyzw'

      else if ( s_eqi ( files, 'xyz+w' ) ) then

        filename = trim ( prefix ) // '.xyz'

      else if ( s_eqi ( files, 'xyz1' ) ) then

        filename = trim ( prefix ) // '.xyz'

      else if ( s_eqi ( files, 'rtpw' ) ) then

        filename = trim ( prefix ) // '.rtpw'

      else if ( s_eqi ( files, 'rtp+w' ) ) then

        filename = trim ( prefix ) // '.rtp'

      else if ( s_eqi ( files, 'rtp1' ) ) then

        filename = trim ( prefix ) // '.rtp'

      else if ( s_eqi ( files, 'dtpw' ) ) then

        filename = trim ( prefix ) // '.dtpw'

      else if ( s_eqi ( files, 'dtp+w' ) ) then

        filename = trim ( prefix ) // '.dtp'

      else if ( s_eqi ( files, 'dtp1' ) ) then

        filename = trim ( prefix ) // '.dtp'

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_EXACTNESS - Fatal error!'
        write ( *, '(a)' ) '  Unrecognized file structure choice!'
        stop 1

      end if

      call r8mat_header_read ( filename, dim_num, point_num )
c
c  Since FORTRAN77 can be awkward about allocating arrays, try
c  to do this by calling a subroutine.
c
      call sphere_exactness_sub ( dim_num, point_num, degree_max,
     &  files, prefix )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_EXACTNESS:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine sphere_exactness_sub ( dim_num, point_num, degree_max,
     &  files, prefix )

c*********************************************************************72
c
cc SPHERE_EXACTNESS_SUB completes the work of the main program.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
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
      double precision dtp(2,point_num)
      double precision dtpw(3,point_num)
      integer expon(3)
      character * ( 255 ) filename
      character * ( 255 ) files
      integer h
      integer i
      integer j
      integer last
      logical more
      character * ( 255 ) prefix
      double precision quad_error
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8vec_sum
      double precision rtp(2,point_num)
      double precision rtpw(3,point_num)
      logical s_eqi
      integer t
      double precision w(point_num)
      double precision w_sum
      double precision xyz(3,point_num)
      double precision xyzw(4,point_num)
c
c  Read data needed to create XYZ and W arrays.
c
      if ( s_eqi ( files, 'xyzw' ) ) then

        filename = trim ( prefix ) // '.xyzw'

        call r8mat_data_read ( filename, 4, point_num, xyzw )

        do j = 1, point_num
          do i = 1, 3
            xyz(i,j) = xyzw(i,j)
          end do
        end do

        do j = 1, point_num
          w(j) = xyzw(4,j)
        end do

      else if ( s_eqi ( files, 'xyz+w' ) ) then

        filename = trim ( prefix ) // '.xyz'

        call r8mat_data_read ( filename, 3, point_num, xyz )

        filename = trim ( prefix ) // '.w'

        call r8mat_data_read ( filename, 1, point_num, w )

      else if ( s_eqi ( files, 'xyz1' ) ) then

        filename = trim ( prefix ) // '.xyz'

        call r8mat_data_read ( filename, 3, point_num, xyz )

        do j = 1, point_num
          w(j) = 4.0D+00 * r8_pi / point_num
        end do

      else if ( s_eqi ( files, 'rtpw' ) ) then

        filename = trim ( prefix ) // '.rtpw'

        call r8mat_data_read ( filename, 3, point_num, rtpw )

        do j = 1, point_num
          xyz(1,j) = cos ( rtpw(1,j) ) * sin ( rtpw(2,j) )
          xyz(2,j) = sin ( rtpw(1,j) ) * sin ( rtpw(2,j) )
          xyz(3,j) =                     cos ( rtpw(2,j) )
        end do

        do j = 1, point_num
          w(j) = rtpw(3,j)
        end do

      else if ( s_eqi ( files, 'rtp+w' ) ) then

        filename = trim ( prefix ) // '.rtp'

        call r8mat_data_read ( filename, 2, point_num, rtp )

        filename = trim ( prefix ) // '.w'

        call r8mat_data_read ( filename, 1, point_num, w )

        do j = 1, point_num
          xyz(1,j) = cos ( rtp(1,j) ) * sin ( rtp(2,j) )
          xyz(2,j) = sin ( rtp(1,j) ) * sin ( rtp(2,j) )
          xyz(3,j) =                    cos ( rtp(2,j) )
        end do

      else if ( s_eqi ( files, 'rtp1' ) ) then

        filename = trim ( prefix ) // '.rtp'

        call r8mat_data_read ( filename, 2, point_num, rtp )

        do j = 1, point_num
          xyz(1,j) = cos ( rtp(1,j) ) * sin ( rtp(2,j) )
          xyz(2,j) = sin ( rtp(1,j) ) * sin ( rtp(2,j) )
          xyz(3,j) =                    cos ( rtp(2,j) )
        end do

        do j = 1, point_num
          w(j) = 4.0D+00 * r8_pi / point_num
        end do

      else if ( s_eqi ( files, 'dtpw' ) ) then

        filename = trim ( prefix ) // '.dtpw'

        call r8mat_data_read ( filename, 3, point_num, dtpw )

        do j = 1, point_num
          do i = 1, 2
            dtpw(i,j) = dtpw(i,j) * r8_pi / 180.0D+00
          end do
        end do

        do j = 1, point_num
          xyz(1,j) = cos ( dtpw(1,j) ) * sin ( dtpw(2,j) )
          xyz(2,j) = sin ( dtpw(1,j) ) * sin ( dtpw(2,j) )
          xyz(3,j) =                     cos ( dtpw(2,j) )
        end do

        do j = 1, point_num
          w(j) = dtpw(3,j)
        end do

      else if ( s_eqi ( files, 'dtp+w' ) ) then

        filename = trim ( prefix ) // '.dtp'

        call r8mat_data_read ( filename, 2, point_num, dtp )

        filename = trim ( prefix ) // '.w'

        call r8mat_data_read ( filename, 1, point_num, w )

        do j = 1, point_num
          do i = 1, 2
            dtp(i,j) = dtp(i,j) * r8_pi / 180.0D+00
          end do
        end do

        do j = 1, point_num
          xyz(1,j) = cos ( dtp(1,j) ) * sin ( dtp(2,j) )
          xyz(2,j) = sin ( dtp(1,j) ) * sin ( dtp(2,j) )
          xyz(3,j) =                    cos ( dtp(2,j) )
        end do

      else if ( s_eqi ( files, 'dtp1' ) ) then

        filename = trim ( prefix ) // '.dtp'

        call r8mat_data_read ( filename, 2, point_num, dtp )

        do j = 1, point_num
          do i = 1, 2
            dtp(i,j) = dtp(i,j) * r8_pi / 180.0D+00
          end do
        end do

        do j = 1, point_num
          xyz(1,j) = cos ( dtp(1,j) ) * sin ( dtp(2,j) )
          xyz(2,j) = sin ( dtp(1,j) ) * sin ( dtp(2,j) )
          xyz(3,j) =                    cos ( dtp(2,j) )
        end do

        do j = 1, point_num
          w(j) = 4.0D+00 * r8_pi / point_num
        end do

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_EXACTNESS - Fatal error!'
        write ( *, '(a)' ) '  Unrecognized file structure choice!'
        stop 1

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of points  = ', point_num
c
c  The W's should sum to 4 * PI.
c
      w_sum = r8vec_sum ( point_num, w )

      do j = 1, point_num
        w(j) = 4.0D+00 * r8_pi * w(j) / w_sum
      end do
c
c  Explore the monomials.
c
      do i = 1, 3
        expon(i) = 0
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          Error          Degree  Exponents'

      do degree = 0, degree_max

        write ( *, '(a)' ) ' '

        more = .false.
        h = 0
        t = 0

10      continue

          call comp_next ( degree, 3, expon, more, h, t )

          call sphere01_monomial_quadrature ( expon, point_num, xyz, 
     &      w, quad_error )

          write ( *, '(2x,f24.16,3x,i2,4x,10i3)' )
     &      quad_error, degree, expon(1:3)

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
      subroutine monomial_value ( m, n, e, x, v )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    F(X) = product ( 1 .le. I .le. M ) X(I)^E(I)
c
c    with the convention that 0^0 = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the evaluation points.
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
        do i = 1, m
          if ( e(i) .ne. 0.0D+00 ) then
            v(j) = v(j) * x(i,j) ** e(i)
          end if
        end do
      end do

      return
      end
      function r8_gamma ( x )

c*********************************************************************72
c
cc R8_GAMMA evaluates Gamma(X) for a real argument.
c
c  Discussion:
c
c    This routine calculates the gamma function for a real argument X.
c    Computation is based on an algorithm outlined in reference 1.
c    The program uses rational functions that approximate the gamma
c    function to at least 20 significant decimal digits.  Coefficients
c    for the approximation over the interval (1,2) are unpublished.
c    Those for the approximation for 12 <= X are from reference 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 January 2008
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody,
c    An Overview of Software Development for Special Functions,
c    in Numerical Analysis Dundee, 1975,
c    edited by GA Watson,
c    Lecture Notes in Mathematics 506,
c    Springer, 1976.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, 
c    Charles Mesztenyi, John Rice, Henry Thatcher, 
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision R8_GAMMA, the value of the function.
c
      implicit none

      double precision c(7)
      double precision eps
      double precision fact
      integer i
      integer n
      double precision p(8)
      logical parity
      double precision q(8)
      double precision r8_gamma
      double precision r8_pi
      double precision res
      double precision sqrtpi
      double precision sum
      double precision x
      double precision xbig
      double precision xden
      double precision xinf
      double precision xminin
      double precision xnum
      double precision y
      double precision y1
      double precision ysq
      double precision z
c
c  Mathematical constants
c
      data sqrtpi / 0.9189385332046727417803297D+00 /
      data r8_pi / 3.1415926535897932384626434D+00 /
c
c  Machine dependent parameters
c
      data xbig / 171.624D+00 /
      data xminin / 2.23D-308 /
      data eps / 2.22D-16 /
      data xinf /1.79D+308 /
c
c  Numerator and denominator coefficients for rational minimax
c  approximation over (1,2).
c
      data p /
     & -1.71618513886549492533811d+00,
     &  2.47656508055759199108314d+01,
     & -3.79804256470945635097577d+02,
     &  6.29331155312818442661052d+02,
     &  8.66966202790413211295064d+02,
     & -3.14512729688483675254357d+04,
     & -3.61444134186911729807069d+04,
     &  6.64561438202405440627855d+04 /

      data q /
     & -3.08402300119738975254353D+01,
     &  3.15350626979604161529144D+02,
     & -1.01515636749021914166146D+03,
     & -3.10777167157231109440444D+03,
     &  2.25381184209801510330112D+04,
     &  4.75584627752788110767815D+03,
     & -1.34659959864969306392456D+05,
     & -1.15132259675553483497211D+05 /
c
c  Coefficients for minimax approximation over (12, INF).
c
      data c /
     & -1.910444077728D-03,
     &  8.4171387781295D-04,
     & -5.952379913043012D-04,
     &  7.93650793500350248D-04,
     & -2.777777777777681622553D-03,
     &  8.333333333333333331554247D-02,
     &  5.7083835261D-03 /

      parity = .false.
      fact = 1.0D+00
      n = 0
      y = x
c
c  Argument is negative.
c
      if ( y .le. 0.0D+00 ) then

        y = - x
        y1 = aint ( y )
        res = y - y1

        if ( res .ne. 0.0D+00 ) then

          if ( y1 .ne. aint ( y1 * 0.5D+00 ) * 2.0D+00 ) then
            parity = .true.
          end if

          fact = - r8_pi / sin ( r8_pi * res )
          y = y + 1.0D+00

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Argument is positive.
c
      if ( y .lt. eps ) then
c
c  Argument < EPS.
c
        if ( xminin .le. y ) then
          res = 1.0D+00 / y
        else
          res = xinf
          r8_gamma = res
          return
        end if

      else if ( y .lt. 12.0D+00 ) then

        y1 = y
c
c  0.0 < argument < 1.0.
c
        if ( y .lt. 1.0D+00 ) then

          z = y
          y = y + 1.0D+00
c
c  1.0 < argument < 12.0.
c  Reduce argument if necessary.
c
        else

          n = int ( y ) - 1
          y = y - dble ( n )
          z = y - 1.0D+00

        end if
c
c  Evaluate approximation for 1.0 < argument < 2.0.
c
        xnum = 0.0D+00
        xden = 1.0D+00
        do i = 1, 8
          xnum = ( xnum + p(i) ) * z
          xden = xden * z + q(i)
        end do

        res = xnum / xden + 1.0D+00
c
c  Adjust result for case  0.0 < argument < 1.0.
c
        if ( y1 .lt. y ) then

          res = res / y1
c
c  Adjust result for case 2.0 < argument < 12.0.
c
        else if ( y .lt. y1 ) then

          do i = 1, n
            res = res * y
            y = y + 1.0D+00
          end do

        end if

      else
c
c  Evaluate for 12.0 <= argument.
c
        if ( y .le. xbig ) then

          ysq = y * y
          sum = c(7)
          do i = 1, 6
            sum = sum / ysq + c(i)
          end do
          sum = sum / y - y + sqrtpi
          sum = sum + ( y - 0.5D+00 ) * log ( y )
          res = exp ( sum )

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Final adjustments and return.
c
      if ( parity ) then
        res = - res
      end if

      if ( fact .ne. 1.0D+00 ) then
        res = fact / res
      end if

      r8_gamma = res

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
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

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
      subroutine sphere01_monomial_integral ( e, integral )

c*********************************************************************72
c
cc SPHERE01_MONOMIAL_INTEGRAL returns monomial integrals on the unit sphere.
c
c  Discussion:
c
c    The integration region is
c
c      X^2 + Y^2 + Z^2 = 1.
c
c    The monomial is F(X,Y,Z) = X^E(1) * Y^E(2) * Z^E(3).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2002
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Academic Press, 1984, page 263.
c
c  Parameters:
c
c    Input, integer E(3), the exponents of X, Y and Z in the
c    monomial.  Each exponent must be nonnegative.
c
c    Output, double precision INTEGRAL, the integral.
c
      implicit none

      integer e(3)
      integer i
      double precision integral
      double precision r8_gamma
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )

      do i = 1, 3
        if ( e(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SPHERE01_MONOMIAL_INTEGRAL - Fatal error!'
          write ( *, '(a)' ) '  All exponents must be nonnegative.'
          write ( *, '(a,i8)' ) '  E(1) = ', e(1)
          write ( *, '(a,i8)' ) '  E(2) = ', e(2)
          write ( *, '(a,i8)' ) '  E(3) = ', e(3)
          stop 1
        end if
      end do

      if ( e(1) .eq. 0.0D+00 .and. 
     &     e(2) .eq. 0.0D+00 .and. 
     &     e(3) .eq. 0.0D+00 ) then

        integral = 2.0D+00 * sqrt ( r8_pi ** 3 ) / r8_gamma ( 1.5D+00 )

      else if ( mod ( e(1), 2 ) .eq. 1 .or.
     &          mod ( e(2), 2 ) .eq. 1 .or.
     &          mod ( e(3), 2 ) .eq. 1 ) then

        integral = 0.0D+00

      else

        integral = 2.0D+00

        do i = 1, 3
          integral = integral * r8_gamma ( 0.5D+00 * dble ( e(i) + 1 ) )
        end do

        integral = integral / 
     &    r8_gamma ( 0.5D+00 * dble ( e(1) + e(2) + e(3) ) )

      end if

      return
      end
      subroutine sphere01_monomial_quadrature ( expon, point_num, xyz, 
     &  w, quad_error )

c*********************************************************************72
c
cc SPHERE01_MONOMIAL_QUADRATURE applies quadrature to a monomial in a sphere.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 September 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer EXPON(3), the exponents.
c
c    Input, integer POINT_NUM, the number of points in the rule.
c
c    Input, double precision XYZ(3,POINT_NUM), the quadrature points.
c
c    Input, double precision W(POINT_NUM), the quadrature weights.
c
c    Output, double precision QUAD_ERROR, the quadrature error.
c
      implicit none

      double precision exact
      integer expon(3)
      integer point_num
      double precision quad
      double precision quad_error
      double precision r8vec_dot_product
      double precision value(point_num)
      double precision w(point_num)
      double precision xyz(3,point_num)
c
c  Get the exact value of the integral.
c
      call sphere01_monomial_integral ( expon, exact )
c
c  Evaluate the monomial at the quadrature points.
c
      call monomial_value ( 3, point_num, expon, xyz, value )
c
c  Compute the weighted sum.
c
      quad = r8vec_dot_product ( point_num, w, value )

      quad_error = abs ( quad - exact )

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