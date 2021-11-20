      program main

c*********************************************************************72
c
cc MAIN is the main program for DSP_TO_ST.
c
c  Discussion:
c
c    DSP_TO_ST converts a DSP sparse matrix file to ST format.
c
c    Each line of the DSP file has the form I, J, A(I,J).
c    The ST file uses the same format, but ST files use 0-based
c    indexing rather than 1-based indexing.
c
c  Usage:
c
c    dsp_to_st file.dsp
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      integer arg_num
      integer i
      integer iarg
      integer iargc
      character * ( 255 ) input_filename
      integer input_unit
      integer ios
      integer j
      integer line_num
      character * ( 255 ) output_filename
      integer output_unit

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DSP_TO_ST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Read a DSP sparse matrix file,'
      write ( *, '(a)' ) 
     &  '  write the corresponding ST sparse matrix file.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  If at least one command line argument, it's the input file name.
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, input_filename )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DSP_TO_ST:'
        write ( *, '(a)' ) '  Please enter the name of the input file.'

        read ( *, '(a)' ) input_filename

      end if
c
c  Need to create the output file name from the input filename.
c
      output_filename = input_filename
      call filename_ext_swap ( output_filename, 'st' )
c
c  Read a line, write a line.
c
      call get_unit ( input_unit )
      open ( unit = input_unit, file = input_filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DSP_TO_ST - Fatal error!'
        write ( *, '(a)' ) '  Could not open the input file "' 
     &    // trim ( input_filename ) // '".'
        stop
      end if

      call get_unit ( output_unit )
      open ( unit = output_unit, file = output_filename, 
     &  status = 'replace' )

      line_num = 0

10    continue

        read ( input_unit, *, iostat = ios ) i, j, a

        if ( ios .ne. 0 ) then
          go to 20
        end if

        write ( output_unit, * ) i - 1, j - 1, a

        line_num = line_num + 1

      go to 10

20    continue

      close ( unit = input_unit )
      close ( unit = output_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DSP_TO_ST:'
      write ( *, '(a,i8)' ) '  Number of records read was ', line_num
      write ( *, '(a)' ) '  Created output ST file: "' 
     &  // trim ( output_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DSP_TO_ST:'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      function ch_index_last ( s, ch )

c*********************************************************************72
c
cc CH_INDEX_LAST is the last occurrence of a character in a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 April 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be searched.
c
c    Input, character CH, the character to be searched for.
c
c    Output, integer CH_INDEX_LAST, the location of the last 
c    occurrence of the character in the string, or -1 if it does not occur.
c
      implicit none

      character ch
      integer ch_index_last
      integer i
      character * ( * )  s
      integer s_length

      ch_index_last = -1
      s_length = len_trim ( s )

      do i = s_length, 1, -1

        if ( s(i:i) .eq. ch ) then
          ch_index_last = i
          return
        end if

      end do
 
      return
      end
      subroutine filename_ext_get ( filename, i, j )

c*********************************************************************72
c
cc FILENAME_EXT_GET determines the "extension" of a file name.
c
c  Discussion:
c
c    The "extension" of a filename is the string of characters
c    that appears after the LAST period in the name.  A file
c    with no period, or with a period as the last character
c    in the name, has a "null" extension.
c
c    Blanks are unusual in filenames.  This routine ignores all
c    trailing blanks, but will treat initial or internal blanks
c    as regular characters acceptable in a file name.
c
c  Example:
c
c    FILENAME   I  J
c
c    bob.for     4  7
c    N.B.C.D     6  7
c    Naomi.      6  6
c    Arthur     -1 -1 
c    .com        1  1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, a file name to be examined.
c
c    Output, integer I, J, the indices of the first and 
c    last characters in the file extension.  
c    If no period occurs in FILENAME, then
c      I = J = -1;
c    Otherwise,
c      I is the position of the LAST period in FILENAME, and J is the
c      position of the last nonblank character following the period.
c
      implicit none

      character * ( * ) filename
      integer i
      integer j
      integer ch_index_last

      i = ch_index_last ( filename, '.' )

      if ( i .eq. -1 ) then

        j = -1

      else

        j = len_trim ( filename )

      end if

      return
      end
      subroutine filename_ext_swap ( filename, ext )

c*********************************************************************72
c
cc FILENAME_EXT_SWAP replaces the current "extension" of a file name.
c
c  Discussion:
c
c    The "extension" of a filename is the string of characters
c    that appears after the LAST period in the name.  A file
c    with no period, or with a period as the last character
c    in the name, has a "null" extension.
c
c  Example:
c
c          Input           Output
c    ================     =========
c    FILENAME    EXT     FILENAME
c
c    bob.for      obj     bob.obj
c    bob.bob.bob  txt     bob.bob.txt
c    bob          yak     bob.yak
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character * ( * ) FILENAME, a file name.
c    On output, the extension of the file has been changed.
c
c    Input, character * ( * ) EXT, the extension to be used on the output
c    copy of FILENAME, replacing the current extension if any.
c
      implicit none

      character * ( * ) ext
      character * ( * ) filename
      integer i
      integer j
      integer len_max
      integer len_name

      len_max = len ( filename )
      len_name = len_trim ( filename )

      call filename_ext_get ( filename, i, j )

      if ( i .eq. 0 ) then

        if ( len_max .lt. len_name + 1 ) then
          return
        end if

        len_name = len_name + 1
        filename(len_name:len_name) = '.'
        i = len_name + 1

      else

        i = i + 1
        filename(i:j) = ' '

      end if

      filename(i:) = ext

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