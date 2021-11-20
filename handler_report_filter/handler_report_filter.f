      program main

c*********************************************************************72
c
cc MAIN is the main program for HANDLER_REPORT_FILTER.
c
c  Discussion:
c
c    HANDLER_REPORT_FILTER cleans up output of the HANDLER program.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 June 1999
c
c  Author:
c
c    John Burkardt
c
      integer IUNIT
      integer MAXHANDLE

      parameter ( IUNIT = 1 )
      parameter ( MAXHANDLE = 500 )

      integer b(MAXHANDLE)
      integer bv
      integer g(MAXHANDLE)
      integer gv
      character*100 handler_file_name
      integer i
      integer ierror
      integer indx
      integer intval
      integer isgn
      integer itemp
      integer j
      logical lbegi
      integer lenc
      integer lenchr
      integer m(MAXHANDLE)
      character*1 mode
      integer nhandle
      integer nline
      character*100 output_file_name
      integer r(MAXHANDLE)
      integer rv
      character*100 string

      call timestamp ( )
      write ( *, * ) ' '
      write ( *, * ) 'HANDLER_REPORT_FILTER'
      write ( *, * ) '  FORTRAN77 version'
      write ( *, * ) ' '
      write ( *, * ) '  This program cleans up the output of the'
      write ( *, * ) '  HANDLER program.'
      write ( *, * ) ' '
      write ( *, * ) '  A typical output line of the form'
      write ( *, * ) ' '
      write ( *, * ) '  Handle near (80,90,16)'
      write ( *, * ) ' '
      write ( *, * ) '  This data is in the order (B,G,R), and is'
      write ( *, * ) '  relative to the volume of interest or VOI.'
      write ( *, * ) ' '
      write ( *, * ) '  This routine rewrites the data as'
      write ( *, * ) '    (R+RV, G+GV, B+BV)'
      write ( *, * ) '  which is more suitable for editing with MRGray.'

      write ( *, * ) ' '
      write ( *, * ) 'Please enter the name of the HANDLER output file.'
      read ( *, '(a)' ) handler_file_name

      open ( unit = IUNIT,
     &  file = handler_file_name, 
     &  status = 'old',
     &  err = 30 )

      write ( *, * ) ' '
      write ( *, * ) 'Please enter the translation (RV,GV,BV) to be'
      write ( *, * ) 'applied to an (R,G,B) coordinate in the VOI.'

      read ( *, * ) rv, gv, bv

      mode = '?'
      nhandle = 0
      nline = 0

10    continue

      read ( IUNIT, '(a)', end = 20, err = 20 ) string

      nline = nline + 1

      if ( lbegi ( string, 'First approach is to add matter...' ) ) then

        mode = '+'

      else if ( 
     &  lbegi ( string, 'Second approach is to remove matter...' ) ) 
     &  then

        mode = '-'

      else if ( lbegi ( string, 'Handle near' ) .and. 
     &  mode .eq. '+' ) then

        nhandle = nhandle + 1

        ierror = -1

        call intnexrd ( string, intval, ierror )
        b(nhandle) = intval + bv

        call intnexrd ( string, intval, ierror )
        g(nhandle) = intval + gv

        call intnexrd ( string, intval, ierror )
        r(nhandle) = intval + rv

        if ( mode .eq. '+' ) then
          m(nhandle) = + 1
        else if ( mode .eq. '-' ) then
          m(nhandle) = - 1
        else
          m(nhandle) = 0
        end if

      else

      end if

      go to 10

20    continue

      close ( unit = IUNIT )

      write ( *, * ) 'The file contained ', nline, ' lines.'
      write ( *, * ) 'There were ', nhandle, ' handles saved.'
c
c  Now sort the data.
c
      indx = 0

25    continue

      call sort_heap_external ( nhandle, indx, i, j, isgn )

      if ( indx .lt. 0 ) then

        isgn = +1

        if ( r(i) .lt. r(j) ) then
          isgn = -1
        else if ( r(i) .eq. r(j) ) then
          if ( g(i) .lt. g(j) ) then
            isgn = -1
          else if ( g(i) .eq. g(j) ) then
            if ( b(i) .lt. b(j) ) then
              isgn = -1
            end if
          end if
        end if

        go to 25

      else if ( indx .gt. 0 ) then

        itemp = r(i)
        r(i) = r(j)
        r(j) = itemp

        itemp = g(i)
        g(i) = g(j)
        g(j) = itemp

        itemp = b(i)
        b(i) = b(j)
        b(j) = itemp

        go to 25

      end if

      write ( *, * ) ' '
      write ( *, * ) 'Data sorted by R coordinate'
c
c  Now write out the data.
c
      write ( *, * ) ' '
      write ( *, * ) 'Enter the name of the filtered output file.'
      read ( *, '(a)' ) output_file_name

      open ( unit = IUNIT,
     &  file = output_file_name, 
     &  status = 'unknown',
     &  err = 30 )

      lenc = lenchr ( output_file_name )
      write ( IUNIT, '(a)' ) output_file_name(1:lenc)
      write ( IUNIT, * ) ' '
      lenc = lenchr ( handler_file_name )
      write ( IUNIT, '(a)' ) 'Original HANDLER output file was ' // 
     &  handler_file_name(1:lenc)
      write ( IUNIT, * ) ' '
      write ( IUNIT, * ) 
     &  'Assumed VOI translation:' 
      write ( IUNIT, 
     &  '( ''( '', i3, '', '', i3, '', '', i3 '' )'' )' ) 
     &  rv, gv, bv
      write ( IUNIT, * ) ' '
      write ( IUNIT, * ) ' '
      write ( IUNIT, * ) 
     &  'Handle coordinates in original coordinate system.'
      write ( IUNIT, * ) ' '
      write ( IUNIT, * ) '  R    G    B'
      write ( IUNIT, * ) ' '
      do i = 1, nhandle
        write ( IUNIT, 
     &    '( ''( '', i3, '', '', i3, '', '', i3 '' )'' )' ) 
     &    r(i), g(i), b(i)
      end do

      close ( unit = IUNIT )

      write ( *, * ) ' '
      write ( *, * ) 'HANDLER_REPORT_FILTER:'
      write ( *, * ) '  Normal end of execution.'
      write ( *, * ) ' '
      call timestamp ( )
      stop

30    continue
      write ( *, * ) ' '
      write ( *, * ) 'HANDLER_REPORT_FILTER - Fatal error!'
      write ( *, * ) '  Could not open the file.'

      end
      subroutine capchr ( c )

c*********************************************************************72
c
cc CAPCHR capitalizes a single character.
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
c    Input/output, character*1 C, the character to capitalize.
c
      character*1 c
      integer itemp

      itemp = ichar ( c )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        c = char ( itemp - 32 )
      end if

      return
      end
      subroutine chrcti2 ( string, intval, ierror, lchar )

c*********************************************************************72
c
cc CHRCTI2 finds and reads an integer from a string.
c
c  Discussion:
c
c    CHRCTI2 is given a string which may contain one or more integers.
c    Starting at the first character position, it looks for the first
c    substring that could represent an integer.  If it finds such a string,
c    it returns the integer's value, and the position of the last character
c    read.
c
c  Example:
c
c    STRING            INTVAL      LCHAR
c
c    'Apollo 13'       13          9
c    '     1   '       1           6
c    '1A'              1           1
c    '12,34,56'        12          2
c    'A1A2A3'          1           2
c    '-1E2ABCD'        -1          2
c    '-X20ABCD'        20          4
c    '23.45'           23          2
c    ' N = 34, $'      34          7
c    'Oops!'           0           0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 September 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) STRING, the string to be read.
c    Reading will begin at position 1 and terminate at the end of the 
c    string, or when no more characters can be read to form a legal integer. 
c    Blanks, commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, integer INTVAL, the integer read from the string, or 0
c    if there was an error.
c
c    Output, integer IERROR, 0 an integer was found, 1 if no integer found.
c
c    Output, integer LCHAR, the last character of STRING that is part
c    of the integer.
c
      character*1 chrtmp
      integer i
      integer idig
      integer ierror
      integer ihave
      integer intval
      integer isgn
      integer iterm
      integer lchar
      integer nchar
      character*(*) string

      nchar = len ( string )
      ierror = 0
      i = 0
      isgn = 1
      intval = 0
      ihave = 0
      iterm = 0
c
c  Examine the next character.
c
10    continue
 
      i = i + 1

      if ( i .gt. nchar ) then

        iterm = 1

      else

        chrtmp = string(i:i)
c
c  Minus sign.
c
        if ( chrtmp .eq. '-' ) then
 
          if ( ihave .eq. 0 ) then
            ihave = 1
            isgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( chrtmp .eq. '+' ) then
  
          if ( ihave .eq. 0 ) then
            ihave = 1
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if ( 
     &    lge ( chrtmp, '0' ) .and.
     &    lle ( chrtmp, '9' ) ) then
 
          ihave = 2

          call digten ( chrtmp, idig )

          intval = 10 * intval + idig
c
c  Blank or TAB.
c
        else 

          if ( ihave .eq. 2 ) then
            iterm = 1
          else
            ihave = 0
          end if

        end if

      end if
 
      if ( iterm .ne. 1 ) then
        go to 10
      end if

      if ( ihave .eq. 2 ) then
        lchar = i - 1
        intval = isgn * intval
      else
        ierror = 0
        lchar = 0
        intval = 0
      end if
 
      return
      end
      subroutine digten ( tenval, intval )

c*********************************************************************72
c
cc DIGTEN returns the integer value of a base 10 digit.
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
c    Input, character*1 TENVAL, the decimal digit, '0' through '9'.
c
c    Output, integer INTVAL, the corresponding integer value.
c
      integer intval
      character*1 tenval

      if ( lge ( tenval, '0' ) .and. lle ( tenval, '9' ) ) then

        intval = ichar ( tenval ) - 48

      else if ( tenval .eq. ' ' ) then

        intval = 0

      else

        write ( *, * ) ' '
        write ( *, * ) 'DIGTEN - Serious errorc'
        write ( *, * ) '  Illegal decimal digit = ' // tenval
        write ( *, * ) '  ASCII number ', ichar ( tenval )
        intval = 0
        stop

      end if

      return
      end
      subroutine intnexrd ( string, intval, ierror )

c*********************************************************************72
c
cc INTNEXRD finds and reads the next integer in a string.
c
c  Discussion:
c
c    INTNEXRD can be used to extract, one at a time, the integers in 
c    a string.
c
c  Example:
c
c    Input:
c
c      STRING = 'Data set #12 extends from (5,-43) and is worth $4.56'
c      IERROR = -1
c
c    Output:
c
c      (on successive calls)
c
c      INTVAL  IERROR
c      ------  ------
c           1       0
c           2       0
c           5       0
c         -43       0
c           4       0
c          56       0
c           0       1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 May 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) STRING, the string to be examined.
c
c    Output, integer INTVAL, the next integer in the string, or 0
c    if no integer could be found.
c
c    Input/output, integer IERROR.
c
c    On the first call for a given string, set IERROR = -1.
c
c    Thereafter, the routine will return IERROR = 0 if another
c    integer was found, or 1 if no more integers were found.
c
      integer ierror
      integer intval
      integer istart
      integer lchar
      character*(*) string

      save istart

      data istart / 0 /

      if ( ierror .eq. -1 ) then
        istart = 0
      end if

      ierror = 0
      intval = 0

      if ( istart .gt. len ( string ) ) then
        ierror = 1
        return
      end if

      call chrcti2 ( string(istart:), intval, ierror, lchar )

      if ( ierror .eq. 0 .and. lchar .gt. 0 ) then
        istart = istart + lchar
      else
        ierror = 1
      end if

      return
      end
      function lbegi ( string1, string2 )

c*********************************************************************72
c
cc LBEGI compares two strings, ignoring blanks and case.
c
c  Discussion:
c
c    It returns TRUE if the two strings are equal, in this sense, up to
c    the end of the shorter one.
c
c  Example:
c
c    'Bob'          'BOB'     TRUE
c    '  B  o b '    ' bo b'   TRUE
c    'Bob'          'Bobby'   TRUE
c    'Bobo'         'Bobb'    FALSE
c    ' '            'Bob'     TRUE  (because blank matches anything)
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
c    Input, character*(*) STRING1, STRING2, the strings to be
c    compared.
c
c    Output, logical LBEGI, .TRUE. if the strings match up to
c    the end of the shorter string, ignoring case and blanks,
c    FALSE otherwise.
c
      integer i1
      integer i2
      logical lbegi
      integer len1
      integer len2
      integer lenchr
      logical leqi
      character*(*) string1
      character*(*) string2

      len1 = lenchr ( string1 )
      len2 = lenchr ( string2 )
      i1 = 0
      i2 = 0
c
c  Strings match so far.  Find next two nonblanks and compare.
c
10    continue

      i1 = i1 + 1

      if ( i1 .gt. len1 ) then
        lbegi = .true.
        return
      end if

      if ( string1(i1:i1) .eq. ' ' ) then
        go to 10
      end if

20    continue

      i2 = i2 + 1

      if ( i2 .gt. len2 ) then
        lbegi = .true.
        return
      end if

      if ( string2(i2:i2) .eq. ' ') then
        go to 20
      end if

      if ( leqi( string1(i1:i1), string2(i2:i2) ) ) then
        go to 10
      end if

      lbegi = .false.

      return
      end
      function lenchr ( string )

c*********************************************************************72
c
cc LENCHR returns the length of STRING up to the last nonblank.
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
c    Input, character*(*) STRING, the string to be measured.
c
c    Output, integer LENCHR, the location of the last nonblank
c    character in STRING.
c
      integer i
      integer lenchr
      character*(*) string

      do i = len ( string ), 1, -1

        if ( string(i:i) .ne. ' ' ) then
          lenchr = i
          return
        end if

      end do

      lenchr = 0

      return
      end
      function leqi ( strng1, strng2 )

c*********************************************************************72
c
cc LEQI is a case insensitive comparison of two strings for equality.
c
c  Example:
c
c    LEQI ( 'Anjana', 'ANJANA' ) is .TRUE.
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
c    Input, character*(*) STRNG1, STRNG2, the strings to compare.
c
c    Output, logical LEQI, the result of the comparison.
c
      integer i
      integer len1
      integer len2
      integer lenc
      logical leqi
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2

      len1 = len ( strng1 )
      len2 = len ( strng2 )
      lenc = min ( len1, len2 )

      leqi = .false.

      do i = 1, lenc

        s1 = strng1(i:i)
        s2 = strng2(i:i)
        call capchr ( s1 )
        call capchr ( s2 )

        if ( s1 .ne. s2 ) then
          return
        end if

      end do

      do i = lenc + 1, len1
        if ( strng1(i:i) .ne. ' ' ) then
          return
        end if
      end do

      do i = lenc + 1, len2
        if ( strng2(i:i) .ne. ' ' ) then
          return
        end if
      end do

      leqi = .true.

      return
      end
      subroutine sort_heap_external ( n, indx, i, j, isgn )

c*********************************************************************72
c
cc SORT_HEAP_EXTERNAL externally sorts a list of items into linear order.
c
c
c  Discussion:
c
c    The actual list of data is not passed to the routine.  Hence this
c    routine may be used to sort integers, reals, numbers, names,
c    dates, shoe sizes, and so on.  After each call, the routine asks
c    the user to compare or interchange two items, until a special
c    return value signals that the sorting is completed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 1999
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    A Nijenhuis and H Wilf,
c    Combinatorial Algorithms,
c    Academic Press, 1978, second edition,
c    ISBN 0-12-519260-6.
c
c  Parameters:
c
c    Input, integer N, the number of items to be sorted.
c
c    Input/output, integer INDX, the main communication signal.
c
c    The user must set INDX to 0 before the first call.
c    Thereafter, the user should not change the value of INDX until
c    the sorting is done.
c
c    On return, if INDX is
c
c      greater than 0,
c      * interchange items I and J;
c      * call again.
c
c      less than 0,
c      * compare items I and J;
c      * set ISGN = -1 if I < J, ISGN = +1 if I > J;
c      * call again.
c
c      equal to 0, the sorting is done.
c
c    Output, integer I, J, the indices of two items.
c    On return with INDX positive, elements I and J should be interchanged.
c    On return with INDX negative, elements I and J should be compared, and
c    the result reported in ISGN on the next call.
c
c    Input, integer ISGN, results of comparison of elements I and J.
c    (Used only when the previous call returned INDX less than 0).
c    ISGN <= 0 means I is less than or equal to J;
c    ISGN => 0 means I is greater than or equal to J.
c
      integer i
      integer indx
      integer isgn
      integer j
      integer k
      integer k1
      integer n
      integer n1

      save k
      save k1
      save n1

      data k / 0 /
      data k1 / 0 /
      data n1 / 0 /
c
c  INDX = 0: This is the first call.
c
      if ( indx .eq. 0 ) then

        n1 = n
        k = n / 2
        k1 = k
c
c  INDX < 0: The user is returning the results of a comparison.
c
      else if ( indx .lt. 0 ) then

        if ( indx .eq. -2 ) then

          if ( isgn .lt. 0 ) then
            i = i + 1
          end if

          j = k1
          k1 = i
          indx = - 1
          return

        end if

        if ( isgn .gt. 0 ) then
          indx = 2
          return
        end if

        if ( k .le. 1 ) then

          if ( n1 .eq. 1 ) then
            indx = 0
          else
            i = n1
            n1 = n1 - 1
            j = 1
            indx = 1
          end if

          return

        end if

        k = k - 1
        k1 = k
c
c  INDX > 0, the user was asked to make an interchange.
c
      else if ( indx .eq. 1 ) then

        k1 = k

      end if

10    continue

      i = 2 * k1

      if ( i .eq. n1 ) then
        j = k1
        k1 = i
        indx = - 1
        return
      else if ( i .le. n1 ) then
        j = i + 1
        indx = - 2
        return
      end if

      if ( k .gt. 1 ) then
        k = k - 1
        k1 = k
        go to 10
      end if

      if ( n1 .eq. 1 ) then
        indx = 0
      else
        i = n1
        n1 = n1 - 1
        j = 1
        indx = 1
      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
