      program main

c*********************************************************************72
c
cc MAIN is the main program for MATT_MESSAGE.
c
c  Discussion:
c
c    MATT_MESSAGE writes random threat and survivor reports to a MATT message datafile.
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
c  NREP is the number of types of reports.  
c  NREP should be 2, representing the choice of 'Threat' or 'Survivor'
c  reports.
c
      integer nrep
      parameter (nrep = 2)
c
c  NSOUR is the number of satellite sources.
c  NSOUR should be 3, representing the choices of 'TADIXSB', 'TDDS', 
c  or 'TIBS'.
c
      integer nsour
      parameter (nsour = 3)
c
c  NW is the maximum number of options we can choose from.
c  NW is set to 10, although we only need to choose from at most 3 options.
c
      integer nw
      parameter (nw = 10)

      real alt
      real ewerr
      character*40 file
      real gauss
      integer id
      integer iday
      integer ihour
      integer imess
      integer imin
      integer imonth
      integer isec
      integer iseed
      integer iunit
      integer iyear
      real lat
      real long
      real malt
      real mewerr
      real mlat
      real mlong
      real mnserr
      integer mxmess
      real nserr
      integer rchoose
      character*10 reptype
      character*10 repvals(nrep)
      real salt
      real sewerr
      real slat
      real slong
      real snserr
      character*10 source
      character*10 sourvals(nsour)
      real w(nw)
      integer wchoose
c
c  Say hello
c
      call timestamp ( )

      write(*,*)' '
      write(*,*)'MATT_MESSAGE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write(*,*)'  the MATT Message File Program,'
      write(*,*)'  last modified on 31 March 1997.'
      write(*,*)' '
      write(*,*)'  This program writes a set of messages to a file'
      write(*,*)'  which are a combination of threat and survivor'
      write(*,*)'  reports, of the sort that MATT might receive'
      write(*,*)'  from a satellite transmission.'
c
c  Initializations.
c
      call init(alt,ewerr,file,id,iday,ihour,imin,imonth,isec,
     &  iseed,iyear,lat,long,malt,mewerr,mlat,mlong,mnserr,mxmess,
     &  nrep,nserr,nsour,nw,reptype,repvals,salt,
     &  sewerr,slat,slong,snserr,source,sourvals,w)
c
c  Delete any old copies of the message file.
c
      call file_delete ( file )
c
c  Get a free output unit number.
c
      call get_unit ( iunit )

      if ( iunit .eq. 0 ) then
        write(*,*)' '
        write(*,*)'MATT_MESSAGE - Fatal error!'
        write(*,*)'  GETUNIT could not find a free FORTRAN unit.'
        stop
      end if
c
c  Open the new message file.
c
      open ( unit=iunit, file=file, status='new', err=20 )
c
c  Beginning of loop.
c
      imess = 0

10    continue

      imess = imess + 1
c
c  Decide on the type of record: Threat or Survivor
c
      w(1) = 85.0
      w(2) = 15.0
      reptype = repvals(wchoose(nrep,w,iseed))
c
c  Assemble the record by setting the report items to
c  random values.
c
      id = id + rchoose(100,iseed)
      alt = gauss(malt,salt,iseed)
      ewerr = gauss(mewerr,sewerr,iseed)
      lat = gauss(mlat,slat,iseed)
      long = gauss(mlong,slong,iseed)
      nserr = gauss(mnserr,snserr,iseed)
      source = sourvals(rchoose(nsour,iseed))

      call getdmy(iday, imonth, iyear)
      call gethms(ihour,imin,isec)
c
c  Write the record to the file.
c
      call wrtrep(alt, ewerr, id, iday, ihour, imess, imonth, 
     &  imin, isec, iunit, iyear, lat, long, nserr, reptype, source)

      if (imess .lt. mxmess ) go to 10
c
c  Close the file.
c
      close ( unit=1, err=30 )
c
c  Stop
c
      write(*,*)' '
      write(*,*)'MATT_MESSAGE: Note'
      write(*,'(1x,a,i4,a)')'  Wrote ',imess,' MATT messages.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATT_MESSAGE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )
      stop
c
20    continue
      write(*,*)' '
      write(*,*)'MATT_MESSAGE: Error!'
      write(*,*)'  Could not open the output file.'
      stop

30    continue
      write(*,*)' '
      write(*,*)'MATT_MESSAGE: Error!'
      write(*,*)'  Could not close the output file.'
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
      function gauss ( gmean, sdev, iseed )

c*********************************************************************72
c
cc GAUSS generates an approximately normal random deviate.
c
c  Discussion:
c
c    The values generated should have mean GMEAN and standard deviation SDEV.
c  
c    The algorithm used involves adding 12 numbers which are approximately 
c    uniformly distributed in [0,1].  
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
c    Input, REAL GMEAN, the mean of the distribution.
c 
c    Input, REAL SDEV, the standard deviation of the distribution.  
c    An input value of SDEV=0 means GAUSS is guaranteed to return 
c    a value of GMEAN.  Values of SDEV of larger magnitude mean that
c    GAUSS is likely to return about 68% of its values between
c    GMEAN-SDEV and GMEAN+SDEV, with other values more widely scattered.
c    Because of the approximate nature of this routine, the output
c    value GAUSS will never be less than GMEAN-6*SDEV nor more than
c    GMEAN+6*SDEV. 
c 
c    Input/output, INTEGER ISEED, used as a seed for the random 
c    number generator.  For information on ISEED, refer to the RANDOM
c    routine.
c
c    Output, REAL GAUSS, an approximately random gaussian variable.
c 
      integer n
      parameter (n=12)

      real gauss
      real gmean
      integer i
      integer iseed
      real random
      real sdev
      real x

      external random

      x = 0.0
      do i = 1, n
        x = x + random(iseed)
      end do

      gauss = gmean + ( x - 0.5*real(n) ) * sdev

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
      subroutine init(alt,ewerr,file,id,iday,ihour,imin,imonth,isec,
     &  iseed,iyear,lat,long,malt,mewerr,mlat,mlong,mnserr,mxmess,
     &  nrep,nserr,nsour,nw,reptype,repvals,salt,
     &  sewerr,slat,slong,snserr,source,sourvals,w)

c*********************************************************************72
c
cc INIT initializes the data to sensible or default or dummy values.
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
      integer nrep
      integer nsour
      integer nw

      real alt
      real ewerr
      character*40 file
      integer i
      integer id
      integer iday
      integer ihour
      integer imin
      integer imonth
      integer isec
      integer iseed
      integer iyear
      real lat
      real long
      real malt
      real mewerr
      real mlat
      real mlong
      real mnserr
      integer mxmess
      real nserr
      character*10 reptype
      character*10 repvals(nrep)
      real salt
      real sewerr
      real slat
      real slong
      real snserr
      character*10 source
      character*10 sourvals(nsour)
      real w(nw)

      call getdmy(iday,imonth,iyear)
      call gethms(ihour,imin,isec)

      alt = 0.0
      ewerr = 0.0
      file = 'matt_message.txt'
      id = 0
      iseed = 3600*ihour+60*imin+isec
      lat = 0.0
      long = 0.0
      malt = 2000.0
      mewerr = 5.0
      mlat = 45.0
      mlong = 0.0
      mnserr = 5.0
c
c  MXMESS is the number of MATT messages we will write to the file.
c
      mxmess = 20
      nserr = 0.0

      repvals(1) = 'Threat'
      repvals(2) = 'Survivor'
      reptype = repvals(1)

      salt = 100.0
      sewerr = 2.0
      slat = 0.05
      slong = 0.05
      snserr = 2.0

      sourvals(1) = 'TADIXSB'
      sourvals(2) = 'TDDS'
      sourvals(3) = 'TIBS'
      source = sourvals(1)

      do i=1,nw
        w(i) = 0.0
      enddo

      write(*,*)' '
      write(*,'(1x,a,i12)')'INIT - Random number seed is ',iseed

      return
      end
      function random(iseed)

c*********************************************************************72
c
cc RANDOM returns a random value between 0 and 1.
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
c    Input/output, INTEGER ISEED, 0 to initialize the generator on first
c    call.  Thereafter, ISEED should be equal to the output value from 
c    the previous call.
c 
c    Output, REAL RANDOM, a random number between 0.0 and 1.0.
c 
      integer icall
      integer iseed
      real random

      intrinsic float
      intrinsic mod

      save icall

      data icall / 0 /

      icall = icall + 1
      
      if ( iseed .eq. 0 ) then
        iseed = 1325
        write(*,*)' '
        write(*,*)'RANDOM - Warning!'
        write(*,*)'  Input value of ISEED was 0.'
      endif
      
      iseed = mod(3125*iseed,65536)
      
      random = float(iseed)/65536.0
      
      return
      end      
      function rchoose(n,iseed)

c*********************************************************************72
c
cc RCHOOSE returns a random integer between 1 and N.
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
c    Input, INTEGER N, the number of integers to choose from.
c
c    Input, INTEGER ISEED, the random number seed.  See RANDOM for details.
c
c    Output, INTEGER RCHOOSE, the randomly chosen integer.
c
      integer iseed
      integer n
      real random
      integer rchoose
      real test

      test = random(iseed)

      rchoose = (n * test) + 1

      if ( rchoose .ge. n ) then
        rchoose = n
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
      function wchoose ( n, w, iseed )

c*********************************************************************72
c
cc WCHOOSE returns weighted random integer.
c
c  Discussion:
c
c    The integer is selected from the range 1 to N.
c
c    An error occurs if any of the weights are negative, or all of the
c    weights are 0.
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
c    Input, INTEGER N, the number of integers to choose from.
c
c    Input, REAL W(N), the weights for the various choices.  The
c    values in W should all be nonnegative, and at least one value
c    must actually be positive.  The values do not have to add up
c    to 1; WCHOOSE will normalize them itself.
c
c    Input, INTEGER ISEED, the random number seed.  For details,
c    refer to the documentation of RANDOM.
c
c    Output, INTEGER WCHOOSE, the chosen number, between 1 and N.
c
      integer n

      integer i
      integer iseed
      real random
      real test
      real w(n)
      integer wchoose
      real wpart
      real wtot

      test = random(iseed)

      wtot = 0.0
      do i=1,n
        wtot = wtot + w(i)
        if ( w(i) .lt. 0.0 ) then
          write(*,*)' '
          write(*,*)'WCHOOSE: Fatal error!'
          write(*,*)'  Negative weight W(I)=',w(i)
          stop
        end if
      enddo

      if ( wtot .le. 0.0 ) then
        write(*,*)' '
        write(*,*)'WCHOOSE: Fatal error!'
        write(*,*)'  Nonpositive weight sum is ',wtot
        stop
      end if

      wpart = 0.0
      do i = 1, n
        wpart = wpart + w(i)
        if ( test*wtot .le. wpart ) then
          wchoose = i
          return
        endif
      enddo

      wchoose = n

      return
      end
      subroutine wrtrep(alt, ewerr, id, iday, ihour, imess, imonth, 
     &  imin, isec, iunit, iyear, lat, long, nserr, reptype, source)

c*********************************************************************72
c
cc WRTREP writes a MATT report to a file opened on FORTRAN unit IUNIT.
c
      real alt
      real ewerr
      integer id
      integer iday
      integer ihour
      integer imess
      integer imin
      integer imonth
      integer isec
      integer iunit
      integer iyear
      real lat
      real long
      real nserr
      character*10 reptype
      character*10 source

      write(iunit,'(a)') '#'
      write(iunit,'(a,i12)') '#MATT Message Number ',imess
      write(iunit,'(a)') '#'

      write(iunit,'(a22,a12)')
     &  '  Report Type       | ',reptype
      write(iunit,'(a22,a12)')
     &  '  Satellite Source  | ',source
      write(iunit,'(a22,i12)')
     &  '  ID                | ',id
      write(iunit,'(a22,g12.6)')
     &  '  Altitude          | ',alt
      write(iunit,'(a22,g12.6)')
     &  '  Latitude          | ',lat
      write(iunit,'(a22,g12.6)')
     &  '  Longitude         | ',long
      write(iunit,'(a22,g12.6)')
     &  '  North/South Error | ',nserr
      write(iunit,'(a22,g12.6)')
     &  '  East/West Error   | ',ewerr
      write(iunit,'(a22,i12)')
     &  '  Year              | ',iyear
      write(iunit,'(a22,i12)')
     &  '  Month             | ',imonth
      write(iunit,'(a22,i12)')
     &  '  Day               | ',iday
      write(iunit,'(a22,i12)')
     &  '  Hour              | ',ihour
      write(iunit,'(a22,i12)')
     &  '  Minute            | ',imin
      write(iunit,'(a22,i12)')
     &  '  Second            | ',isec

      return
      end
