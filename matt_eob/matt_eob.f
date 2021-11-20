      program main

c***********************************************************************
c
cc MAIN is the main program for MATT_EOB.
c
c  Discussion:
c
c    Open and read the EOB file, "threats.txt", containing the initial
c    threat information.
c
c    Copy the relevant threat information into the MATT contact report
c    database.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer MAXSEQ
      parameter (MAXSEQ=1)

      integer day
      character*7 elnot
      integer eobid
      double precision ewerror
      double precision freq
      integer heading
      integer hour
      integer i
      integer icon
      integer ierror
      character*7 iff
      integer iseq
      double precision lat
      integer link
      double precision lon
      integer minute
      integer month
      character*3 smonth
      double precision nserror
      character*8 rmode
      character*8 rtype
      character*20 scan
      integer second
      integer seed
      double precision speed
      character*40 string
      logical surviv
      double precision thalt
      double precision theas
      double precision thnor
      integer year

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write(*,*)'MATT_EOB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write(*,*)'  Last modified on 15 May 1997.'
      write ( *, '(a)' ) ' '
      write(*,*)'  Read the EOB file "threats.txt"'
      write(*,*)'  Write a MATT message file "matt_message.txt".'
      write ( *, '(a)' ) ' '
      write(*,*)'  The EOB data will be used ',MAXSEQ,' times.'
      write ( *, '(a)' ) ' '
      write(*,*)'  For sequence numbers greater than one, vary'
      write(*,*)'  the original EOB data randomly.'
      write ( *, '(a)' ) ' '

      seed=1997

      call timestring ( string )
      call getdmy ( day, month, year )
      call gethms ( hour, minute, second )

      call file_delete ( 'matt_message.txt' )

      open (unit=98,file='matt_message.txt',status='new')
 
      write(98,'(a)')'#  MATT Message File created by EOBMATT.'
      write(98,'(a)' )'#  Created on ' // trim ( string ) // '.'
      write(98,'(a)')'#'
      write(98,'(a)')'#  MATT_EOB.F last modified on 01 May 1997.'
      write(98,'(a)')'#'
      write(98,'(a,i12)')'#  The random number seed is ',seed
      write(98,'(a)')'#'
      write(98,'(a)')'#'
      write(98,'(a)')'# MATT Message Format:'
      write(98,'(a)')'#'
      write(98,'(a)')'# Link                | (1-26)'
      write(98,'(a)')'# Report_Mode         | (REAL/EXERCISE)'
      write(98,'(a)')'# Report_Type         | (ELINT/NONELINT/SURVIVOR)'
      write(98,'(a)')'# ELNOT               | '//
     &  '(NONE/UNKNOWN/SEVEN/EIGHT/ZEE/A)'
      write(98,'(a)')'# IFF                 | (FRIEND/FOE/UNKNOWN)'
      write(98,'(a)')'# Scan                | '
     &  //'(SEARCH/TRACK/ACQUIRE/GUIDANCE)'
      write(98,'(a)')'# Hour                | (00-23)'
      write(98,'(a)')'# Minute              | (01-59)'
      write(98,'(a)')'# Second              | (01-59)'
      write(98,'(a)')'# Day                 | (01-31)'
      write(98,'(a)')'# Month               | (01-12)'
      write(98,'(a)')'# Year                | (1997)'
      write(98,'(a)')'# ID                  | '//
     &  'EOB code from THREATS.DAT/Survivor ID'
      write(98,'(a)')'# Sequence            | Sequence number'
      write(98,'(a)')'# Speed               | (knots)'
      write(98,'(a)')'# Heading             | (degrees)'
      write(98,'(a)')'# Altitude            | (feet)'
      write(98,'(a)')'# Frequency           | (hertz)'
      write(98,'(a)')'# EWError             | (NM)'
      write(98,'(a)')'# NSError             | (NM)'
      write(98,'(a)')'# Latitude            | (radians)'
      write(98,'(a)')'# Longitude           | (radians)'

      icon = 0

      do i = 1, MAXSEQ

        iseq = i-1

        open (unit=99,file='threats.txt',status='old',err=99)

10      continue

        call reob(elnot,eobid,ierror,scan,surviv,thalt,theas,thnor)

        if (ierror .ne. 0 ) then

          if ( i .eq. 1 ) then

            write ( *, '(a)' ) ' '
            write(*,*)'There are ',ICON,
     &        ' threat records in "threats.txt".'

          end if

        else

          icon = icon + 1

          second = second + 10

          call ymdhms_normalize ( year, month, day, hour, minute, 
     &      second )

          call interp(elnot,ewerror,freq,heading,iff,lat,link,
     &      lon,nserror,rmode,rtype,speed,surviv,thalt,theas,thnor,
     &      seed )

          if ( 1 .lt. i ) then
            call movdat ( ewerror, heading, seed, lat, lon,
     &        nserror, scan, speed, thalt )
          end if

          call wmatt(day,elnot,eobid,ewerror,freq,heading,hour,icon,
     &      iff,month,iseq,lat,link,lon,minute,nserror,rmode,rtype,
     &      scan,second,speed,thalt,year)

          go to 10

        end if
c
c  We have reached the end of the EOB file.  
c  Close it, so that we can reopen it for another pass.
c
        close (unit = 99)

      end do

      close (unit = 98)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATT_EOB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
c
c  Error on opening threat file.
c
99    continue

      write ( *, '(a)' ) ' '
      write(*,*)'MATT_EOB - Fatal error!'
      write(*,*)'  The threats file "threats.txt" could not be opened.'
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

      if ( c1_cap == c2_cap ) then
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
      function deg2rad()

c*********************************************************************72
c
cc DEG2RAD returns the factor necessary to convert degrees to radians.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
      double precision deg2rad

      deg2rad = 3.14159265D+00 / 180.0D+00

      return
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
      subroutine getdmy ( d, m, y )

c*********************************************************************72
c
cc GETDMY returns the current day, month, and year.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer D, M, Y, the current day, month, and year.
c
      implicit none

      integer d
      character * ( 8 ) date
      integer m
      character * ( 10 ) time
      integer y

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d

      return
      end
      subroutine gethms ( h, m, s )

c*********************************************************************72
c
cc GETHMS returns the current hour, minute and second.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer H, M, S, the current hour, minute and second.
c
      implicit none

      integer h
      character * ( 8 ) date
      integer m
      integer mm
      character * ( 10 ) time
      integer s

      call date_and_time ( date, time )

      read ( time, '(i2,i2,i2,1x,i3)' ) h, m, s, mm

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
c    26 October 2008
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

          open ( unit = i, err = 10, status = 'scratch' )
          close ( unit = i )

          unit = i

          return
        end if

10      continue

      end do

      return
      end
      subroutine interp(elnot,ewerror,freq,heading,iff,lat,link,
     &  lon,nserror,rmode,rtype,speed,surviv,thalt,theas,thnor,seed)

c*********************************************************************72
c
cc INTERP fills in or makes up missing data.
c
c  Discussion:
c
c    required for MATT message file that is
c    not explicitly supplied by the EOB file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision deg2rad
      character*7 elnot
      double precision ewerror
      double precision freq
      integer heading
      integer i4_uniform
      character*7 iff
      double precision lat
      double precision latr
      logical s_eqi
      integer link
      double precision lon
      double precision lonr
      double precision nserror
      double precision radius
      character*8 rmode
      character*8 rtype
      integer seed
      double precision speed
      logical surviv
      double precision thalt
      double precision theas
      double precision thnor

      radius = 20925639.76D+00
c
c  Fill in missing data.
c
      ewerror = 0.0D+00

      if ( s_eqi(elnot(1:1),'UNKNOWN') .or. 
     &     s_eqi(elnot(1:1),'NONE') ) then
        freq = 0.0D+00
      else if ( s_eqi(elnot,'SEVEN') .or. s_eqi(elnot,'EIGHT') ) then
        freq = 25.0D+09
      else if ( s_eqi(elnot,'ZEE') .or. s_eqi(elnot,'A') ) then
        freq = 30.0D+09
      else
        freq = 0.0D+00
      end if

      heading = 0

      if (surviv) then
        iff = 'FRIEND'
      else
        iff = 'FOE'
      end if

      latr =  32.7999347D+00 * deg2rad()
      lat = latr + thnor / (radius + thalt)

      link = i4_uniform ( 1, 4, seed )

      lonr = -97.1644956D+00 * deg2rad()
      lon = lonr + theas / ((radius + thalt) * cos(lat))

      nserror = 0.0D+00
      rmode = 'REAL'
      if (surviv) then
        rtype = 'SURVIVOR'
      else
        rtype = 'ELINT'
      end if
      speed=0.0D+00

      return
      end
      function i4_uniform ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM returns a scaled pseudorandom I4.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_uniform
      integer k
      double precision r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r = dble ( seed ) * 4.656612875D-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0D+00 - r ) * ( dble ( min ( a, b ) ) - 0.5D+00 )
     &  +             r   * ( dble ( max ( a, b ) ) + 0.5D+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform = value

      return
      end
      function month_days ( month, year )

c*********************************************************************72
c
cc MONTH_DAYS returns the number of days in a month.
c
c  Discussion:
c
c    The routine knows that February has 28 days, except in leap years,
c    when it has 29.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MONTH, the number of the month.
c
c    Input, integer YEAR, the year in which the month occurred.
c
c    Output, integer MONTH_LENGTH_COMMON, the number of days in the month.
c
      implicit none

      integer mdays(12)
      integer month
      integer month_days
      integer year
      logical year_is_leap

      save mdays

      data mdays / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

      month_days = mdays ( month )

      if ( month .eq. 2 .and. year_is_leap ( year ) ) then
        month_days = month_days + 1
      end if

      return
      end
      subroutine movdat(ewerror,heading,seed,lat,lon,
     &  nserror,scan,speed,thalt)

c*********************************************************************72
c
cc MOVDAT perturbs the data slightly, to make life interesting.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision ewerror
      integer heading
      integer i
      integer i4_uniform
      integer ival
      double precision lat
      logical s_eqi
      double precision lon
      double precision nserror
      double precision r4_uniform
      double precision r8_uniform
      double precision rval
      character*20 scan
      character*20 scantype(4)
      integer seed
      double precision speed
      double precision thalt

      scantype(1) = 'SEARCH'
      scantype(2) = 'TRACK'
      scantype(3) = 'ACQUIRE'
      scantype(4) = 'GUIDANCE'

      ewerror = r8_uniform ( 0.0D+00, 2.0D+00, seed )

      heading = i4_uniform ( -180, 180, seed )

      lat = r8_uniform ( 0.99D+00*lat, 1.01D+00*lat, seed )

      lon = r8_uniform ( 0.99D+00*lon, 1.01D+00*lon, seed )

      nserror = r8_uniform ( 0.0D+00, 2.0D+00, seed )

      ival = 0

      do i=1,4
        if ( s_eqi( scan, scantype(i) ) ) then
          ival = i
        end if
      end do

      rval = r8_uniform ( 0.0D+00, 1.0D+00, seed )

      if ( rval .le. 0.10D+00 .and. ival .ne. 1 ) then
        scan = scantype(ival-1)
      else if (rval .ge. 0.90D+00 .and. ival .ne. 4) then
        scan = scantype(ival+1)
      end if

      speed = r8_uniform ( -200.0D+00, 200.0D+00, seed )

      thalt = r8_uniform ( 0.8D+00*thalt, 1.2D+00*thalt, seed )

      return
      end
      function r8_uniform ( a, b, seed )

c*********************************************************************72
c
cc R8_UNIFORM returns a scaled pseudorandom R8.
c
c  Discussion:
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    06 January 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, double precision A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM, a number strictly between A and B.
c
      implicit none

      double precision a
      double precision b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      double precision r8_uniform
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if
c
c  Although SEED can be represented exactly as a 32 bit integer,
c  it generally cannot be represented exactly as a 32 bit double precision number!
c
      r8_uniform = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine reob(elnot,eobid,ierror,scan,surviv,thalt,theas,thnor)

c*********************************************************************72
c
cc REOB reads one set of EOB records from THREATS.DAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer cat
      double precision detect
      character*7 elnot
      integer eobid
      integer ierror
      integer lchar
      double precision leth
      character*80 line
      character*80 line2
      character*20 scan
      double precision senelv
      logical surviv
      character*80 symbol
      character*6 tag
      double precision thalt
      double precision theas
      double precision thnor
      integer thrid

      ierror = 0

10    continue

      read(99,'(a)',end=20)line

      if (line(1:1).eq.'#') then
        go to 10
      end if

      call s_after_ss_copy ( line, '|', line2 )
      call s_to_i4 ( line2, thrid, ierror, lchar )

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_i4 ( line2, eobid, ierror, lchar )

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_adjustl ( line2 )
      elnot = line2
 
      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_l ( line2, surviv, ierror, lchar )

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_adjustl ( line2 )
      scan = line2

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      symbol = line2

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      tag = line2

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_r8 (line2,theas,ierror,lchar)

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_r8 (line2,thnor,ierror,lchar)

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_r8 (line2,thalt,ierror,lchar)

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_r8 (line2,senelv,ierror,lchar)

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_r8 (line2,detect,ierror,lchar)

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_r8 (line2,leth,ierror,lchar)

      read(99,'(a)',end=20)line
      call s_after_ss_copy ( line, '|', line2 )
      call s_to_i4 ( line2, cat, ierror, lchar )

      return
c
c  END OF FILE.
c
20    continue

      ierror = 1
      return
      end
      subroutine s_adjustl ( s )

c*********************************************************************72
c
cc S_ADJUSTL flushes a string left.
c
c  Discussion:
c
c    Both blanks and tabs are treated as "white space".
c
c    This routine is similar to the FORTRAN90 ADJUSTL routine.
c
c  Example:
c
c    Input             Output
c
c    '     Hello'      'Hello     '
c    ' Hi therec  '    'Hi therec   '
c    'Fred  '          'Fred  '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 Jun3 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character * ( * ) S.
c    On input, S is a string of characters.
c    On output, any initial blank or tab characters have been cut.
c
      implicit none

      integer i
      integer nonb
      character * ( * ) s
      integer s_length
      character tab

      tab = char ( 9 )
c
c  Check the length of the string to the last nonblank.
c  If nonpositive, return.
c
      s_length = len_trim ( s )

      if ( s_length .le. 0 ) then
        return
      end if
c
c  Find NONB, the location of the first nonblank, nontab.
c
      nonb = 0

      do i = 1, s_length

        if ( s(i:i) .ne. ' ' .and. s(i:i) .ne. tab ) then
          nonb = i
          go to 10
        end if

      end do

10    continue

      if ( nonb .eq. 0 ) then
        s = ' '
        return
      end if
c
c  Shift the string left.
c
      if ( 1 .lt. nonb ) then
        do i = 1, s_length + 1 - nonb
          s(i:i) = s(i+nonb-1:i+nonb-1)
        end do
      end if
c
c  Blank out the end of the string.
c
      s(s_length+2-nonb:s_length) = ' '
     
      return
      end
      subroutine s_after_ss_copy ( s1, ss, s2 )

c*********************************************************************72
c
cc S_AFTER_SS_COPY copies a string after a given substring.
c
c  Discussion:
c
c    S1 and S2 can be the same object, in which case the string is
c    overwritten by a copy of itself after the substring.
c
c  Example:
c
c    Input:
c
c      S1 = 'ABCDEFGH'
c      SS = 'EF'
c
c    Output:
c
c      S2 = 'GH'.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S1, the string to be copied.
c
c    Input, character*(*) SS, the substring after which the copy begins.
c
c    Output, character*(*) S2, the copied portion of S.
c
      implicit none

      integer first
      integer last
      integer last_s2
      character*(*)  s1
      integer s1_length
      character*(*)  s2
      character*(*)  ss
c
c  Find the first occurrence of the substring.
c
      first = index ( s1, ss )
c
c  If the substring doesn't occur at all, then S2 is blank.
c
      if ( first .eq. 0 ) then
        s2 = ' '
        return
      end if
c
c  Redefine FIRST to point to the first character to copy after
c  the substring.
c
      first = first + len ( ss )
c
c  Measure the two strings.
c
      s1_length = len ( s1 )
      last_s2 = len ( s2 )
c
c  Adjust effective length of S if S2 is short.
c
      last = min ( s1_length, last_s2 + first - 1 )
c
c  Copy the string.
c
      s2(1:s1_length+1-first) = s1(first:s1_length)
c
c  Clear out the rest of the copy.
c
      s2(s1_length+2-first:last_s2) = ' '

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
      function s_first_nonblank ( s )

c*********************************************************************72
c
cc S_FIRST_NONBLANK returns the location of the first nonblank.
c
c  Discussion:
c
c    If all characters are blanks, a 0 is returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 November 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character ( len = * ) S, the string to be examined.
c
c    Output, integer S_FIRST_NONBLANK, the location of the first
c    nonblank character in the string, or 0 if all are blank.
c
      implicit none

      integer i
      character * ( * ) s
      integer s_first_nonblank
      integer s_length

      s_length = len ( s )

      do i = 1, s_length
 
        if ( s(i:i) .ne. ' ' ) then
          s_first_nonblank = i
          return
        end if
 
      end do
 
      s_first_nonblank = 0
 
      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

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
      integer s_len_trim

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, s_len_trim ( s )

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
        length = s_len_trim ( s )
      else
        ierror = 1
        length = 0
      end if

      return
      end
      subroutine s_to_l ( s, logval, ierror, length )

c*********************************************************************72
c
cc S_TO_L reads a logical value from a string.
c
c  Discussion:
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the value.
c
c    There are several ways of representing logical data that this routine
c    recognizes:
c
c      False   True
c      -----   ----
c
c      F       T
c      FALSE   TRUE
c      .FALSE. .TRUE.
c      0       1
c
c    The routine is not case sensitive.  'TRUE' may also be spelled 'true'
c    or 'True'.
c
c    The routine is not "space" sensitive.  There may be spaces before or
c    after the representation of the logical value.  But there may
c    not be spaces between the letters!  If the routine was given the
c    input "TRUE", it would read all four characters.  But the input
c    "T R U E" would cause the routine to read only the first character.
c
c    The routine doesn't care what follows the data it reads.  The
c    representation for the logical value may be followed by blanks,
c    commas, numeric values, or any kind of data.
c
c  Example:
c
c    S         LOGVAL   LENGTH  IERROR
c
c    '.TRUE.'      T       6      0
c    'TRUE'        T       4      0
c    'True'        T       4      0
c    '  TRUE'      T       6      0
c    'Trump'       T       1      0
c    'Ture'        T       1      0
c    'T'           T       1      0
c    'Talleyrand'  T       1      0
c    'Garbage'     F       0      2
c    'F'           F       1      0
c    'Furbelow'    F       1      0
c    '0'           F       1      0
c    '1'           T       1      0
c    '2'           F       0      2
c    '     1'      T       6      0
c    '17'          T       1      0
c    '1A'          T       1      0
c    '12,34,56'    T       1      0
c    '  34 7'      F       0      2
c    '-1E2ABCD'    F       0      2
c    'I am TRUE'   F       0      2
c    ' '           F       0      1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 January 2009
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
c    characters can be read to form a legal integer.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, logical LOGVAL, the logical value read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors.
c    1, input string was entirely blank.
c    2, input string did not begin with a logical value.
c
c    Output, integer LENGTH, number of characters read from
c    the string to form the logical value.
c 
      implicit none

      integer first
      integer ierror
      integer j
      integer last
      logical logval
      integer length
      integer lens
      character * ( 7 ) pat(8)
      character * ( * ) s
      logical s_eqi
      integer s_first_nonblank
      logical val(8)

      save pat
      save val

      data pat /
     &  '0      ', 
     &  '1      ', 
     &  'F      ', 
     &  'T      ', 
     &  'TRUE   ', 
     &  'FALSE  ', 
     &  '.TRUE. ', 
     &  '.FALSE.' /

      data val / 
     & .FALSE.,
     & .TRUE.,
     & .FALSE.,
     & .TRUE.,
     & .TRUE.,
     & .FALSE.,
     & .TRUE.,
     & .FALSE. /
c
c  Set the default output values.
c
      ierror = 0
      logval = .false.
      length = 0
c
c  Find the first nonblank character in the string.
c
      first = s_first_nonblank ( s )
 
      if ( first .le. 0 ) then
        ierror = 1
        return
      end if
c
c  Compare the string to the eight legal patterns, going by decreasing
c  size.
c
      lens = len_trim ( s )

      do j = 8, 1, -1
 
        last = first + len_trim ( pat(j) ) - 1
 
        if ( last .le. lens ) then

          if ( s_eqi ( pat(j), s(first:last) ) ) then
            logval = val(j)
            length = last
            return
          end if

        end if
 
      end do
c
c  The input string did not contain a logical value.
c
      ierror = 2

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
c    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
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
      integer s_len_trim

      nchar = s_len_trim ( s )

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

        if ( nchar .lt. length+1 ) then
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
      subroutine timestring ( string )

c*********************************************************************72
c
cc TIMESTRING writes the current YMDHMS date into a string.
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
c    Output, character * ( * ) STRING, contains the date information.
c    A character length of 40 should always be sufficient.
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
      character * ( * ) string
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

      write ( string, 
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
      subroutine wmatt(day,elnot,eobid,ewerror,freq,heading,hour,icon,
     &  iff,month,iseq,lat,link,lon,minute,nserror,rmode,rtype,
     &  scan,second,speed,thalt,year)

c*********************************************************************72
c
cc WMATT writes the data corresponding to a single MATT message.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer day
      character*7 elnot
      integer eobid
      double precision ewerror
      double precision freq
      integer heading
      integer hour
      integer icon
      character*7 iff
      integer month
      integer iseq
      double precision lat
      integer lchar
      integer link
      double precision lon
      integer minute
      double precision nserror
      character*8 rmode
      character*8 rtype
      character*20 scan
      integer second
      double precision speed
      double precision thalt
      integer year

      write(98,'(a)')'#'
      write(98,'(a,i6)')'# MATT Message Number ',icon
      write(98,'(a)')'#'
      write(98,'(a,i6)')'  Link                | ',link
      write(98,'(a)')'  Report_Mode         | ' //rmode
      write(98,'(a)')'  Report_Type         | ' //rtype
      lchar=len_trim (elnot)
      write(98,'(a)')'  ELNOT               | ' // elnot(1:lchar)
      write(98,'(a)')'  IFF                 | ' // iff
      lchar=len_trim(scan)
      write(98,'(a)')'  Scan                | ' // scan(1:lchar)
      write(98,'(a,i6)')'  Hour                | ',hour
      write(98,'(a,i6)')'  Minute              | ',minute
      write(98,'(a,i6)')'  Second              | ',second
      write(98,'(a,i6)')'  Day                 | ',day
      write(98,'(a,i6)')'  Month               | ',month
      write(98,'(a,i6)')'  Year                | ',year
      write(98,'(a,i6)')'  ID                  | ',eobid
      write(98,'(a,i6)')'  Sequence            | ',iseq
      write(98,'(a,g14.6)')'  Speed (knots)       | ',speed
      write(98,'(a,i6)')'  Heading (degrees)   | ',heading
      write(98,'(a,i6)')'  Altitude (FT)       | ',int(thalt)
      write(98,'(a,g14.6)')'  Frequency (Hz)      | ',freq
      write(98,'(a,g14.6)')'  EWError (NM)        | ',ewerror
      write(98,'(a,g14.6)')'  NSError (NM)        | ',nserror
      write(98,'(a,g14.6)')'  Latitude (radians)  | ',lat
      write(98,'(a,g14.6)')'  Longitude (radians) | ',lon

      return
      end
      subroutine ymdhms_normalize ( year, month, day, hour, minute,
     & second )

c*********************************************************************72
c
cc YMDHMS_NORMALIZE normalizes a date.
c
c  Discussion:
c
c    The data is presumed to have the form 
c
c      Year/Month/Day/Hour/Minute/Second
c
c    where, for instance, on input, the value of seconds might
c    be more than 59.
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, 
c    the year, month, day, hour, minutes and seconds of a date.
c
      implicit none

      integer day
      integer hour
      integer minute
      integer month
      integer month_days
      integer second
      integer year

10    continue

      if ( second .lt. 0 ) then
        second = second + 60
        minute = minute - 1
        go to 10
      end if

20    continue

      if ( 60 .le. second ) then
        second = second - 60
        minute = minute + 1
        go to 20 
      end if

30    continue

      if ( minute .lt. 0 ) then
        minute = minute + 60
        hour = hour - 1
        go to 30
      end if

40    continue

      if ( 60 .le. minute ) then
        minute = minute - 60
        hour = hour + 1
        go to 40
      end if

50    continue

      if ( hour .lt. 0 ) then
        hour = hour + 24
        day = day - 1
        go to 50
      end if

60    continue

      if ( 24 .le. hour ) then
        hour = hour - 24
        day = day + 1
        go to 60
      end if

70    continue

      if ( day .lt. 1 ) then
        month = month - 1
        day = day + month_days ( month, year )
        go to 70
      end if

80    continue

      if ( month_days ( month, year ) .lt. day ) then
        day = day - month_days ( month, year )
        month = month + 1
        go to 80
      end if

90    continue

      if ( month .lt. 1 ) then
        month = month + 12
        year = year - 1
        go to 90
      end if

100   continue

      if ( 12 .lt. month ) then
        month = month - 12
        year = year + 1
        go to 100
      end if

      return
      end
      function year_is_leap ( year )

c*********************************************************************72
c
cc YEAR_IS_LEAP returns TRUE if the year was a leap year.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer YEAR, the year to be checked.
c
c    Output, logical YEAR_IS_LEAP, TRUE if the year was a leap year,
c    FALSE otherwise.
c
        implicit none

      integer year
      logical year_is_leap

      if ( mod ( year, 400 ) == 0 ) then
        year_is_leap = .true.
      else if ( mod ( year, 100 ) == 0 ) then
        year_is_leap = .false.
      else if ( mod ( year, 4 ) == 0 ) then
        year_is_leap = .true.
      else
        year_is_leap = .false.
      end if

      return
      end
