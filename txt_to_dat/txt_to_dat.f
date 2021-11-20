      program main

c*********************************************************************72
c
cc MAIN is the main program for TXT_TO_DAT.
c
c  Discussion:
c
c    TXT2_TO_DAT takes a text file, containing one real number per line,
c    and makes a copy of that text file which is "binary", an
c    unformatted file of fixed records, containing one real number
c    per record.
c
c    I need this program because the ALPHA runs so darn fast, but
c    its binary files are unusable on the IRIS or the MAC, because,
c    presumably, of byte swapping.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 November 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character*80 input_file_name
      integer input_file_unit
      integer irec
      character*80 output_file_name
      integer output_file_unit
      real x

      write ( *, '(a)' ) ' '
      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TXT_TO_DAT'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Extract information from an ASCII text file,'
      write ( *, '(a)' ) '  and write it to a binary file, that is, an'
      write ( *, '(a)' ) '  unformatted file of fixed-length records,'
      write ( *, '(a)' ) '  sometimes called a "random access" file.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Enter the name of the text file to be read.'
      read ( *, '(a)' ) input_file_name
 
      open ( unit = input_file_unit, file = input_file_name, 
     &  status = 'old', form = 'formatted', access = 'sequential', 
     &  err = 99 )
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Enter the name of the binary data file to be created.'
      read ( *, '(a)' ) output_file_name

      call file_delete ( output_file_name )
c
c  The value of RECL varies from one operating system to the next.
c  For an unformatted file, a common convention measures RECL in bytes.
c  Then a standard integer or real number takes 4 bytes, a double precision real
c  or single precision complex value takes 8, and a double precision complex
c  value takes 16.
c
      open ( unit = 2, file = output_file_name, status = 'new',
     &  form = 'unformatted', access = 'direct', recl = 4, 
     &  err = 98 )

      irec = 0

10    continue

      read ( input_file_unit, *, end = 20 ) x
      irec = irec + 1
      write ( output_file_unit, rec = irec ) x
      go to 10

20    continue
 
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8,a)' ) 
     &  'TXT_TO_DAT read and rewrote ', irec, ' records.'
      write ( *, '(a)' ) 'The binary data file has been created.'

      close ( unit = input_file_unit )
      close ( unit = output_file_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TXT_TO_DAT:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop

99    continue
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TXT_TO_DAT - Fatal error!'
      write ( *, '(a)' ) 
     &  '  Could not open "' // trim ( input_file_name ) // '".'
      stop

98    continue
      write ( *, '(a)' ) 'TXT_TO_DAT - Fatal error!'
      write ( *, '(a)' ) 
     &  '  Could not open "' // trim ( output_file_name ) // '".'
      stop
      end
      subroutine file_delete ( file_name )

c*********************************************************************72
c
cc FILE_DELETE deletes a file if it exists.
c
c  Discussion:
c
c    You might want to call this routine to get rid of any old copy
c    of a file, before trying to open a new copy with the OPEN argument:
c
c      status = 'new'.
c
c    It's not always safe to open a file with " STATUS = 'UNKNOWN' ".
c
c    For instance, on the SGI, the most recent version of the FORTRAN
c    compiler seems to go crazy when I open an unformatted direct
c    access file this way.  It creates an enormous file (of somewhat
c    random size).  The problem goes away if I delete any old copy
c    using this routine, and then open a fresh copy with
c    " STATUS = 'NEW' ".  It's a scary world.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) FILE_NAME, the name of the file to be deleted.
c
      implicit none

      character*80 ctemp
      character*(*) file_name
      logical lfile
      integer s_len_trim
      integer unit
c
c  Does the file exist?
c
      inquire (
     &  file = file_name,
     &  exist = lfile )

      if ( .not. lfile ) then
        return
      end if
c
c  Can we get a FORTRAN unit number?
c
      call get_unit ( unit )

      if ( unit .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILE_DELETE - Error!'
        write ( *, '(a)' ) '  A free FORTRAN unit could not be found.'
        return
      end if
c
c  Can we open the file?
c  
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_DELETE:'
      write ( *, '(a)' ) '  Open "' // trim ( file_name ) // '".'

      open (
     &  unit = unit,
     &  file = file_name,
     &  status = 'old',
     &  err = 10 )
c
c  Can we close the file with "Delete" status?
c
      write ( *, '(a)' ) '  Delete "' // trim ( file_name ) // '".'

      close (
     &  unit = unit,
     &  status = 'delete',
     &  err = 20 )

      return

10    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_DELETE - Error!'
      write ( *, '(a)' ) 
     &  '  Could not open "' // trim ( file_name ) // '".'
      return

20    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_DELETE - Error!'
      write ( *, '(a)' ) 
     &  '  Could not delete "' // trim ( file_name ) // '".'

      return
      end
      subroutine get_unit ( unit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is an integer between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If UNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, UNIT is an integer between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer UNIT, the free unit number.
c
      implicit none

      integer i
      integer unit

      unit = 0

      do i = 1, 99

        if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

          open ( unit = i, err = 10 )
          close ( unit = i )

          unit = i

          return
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
