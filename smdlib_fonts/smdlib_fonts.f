      program main

c*********************************************************************72
c
cc MAIN is the main program for SMDLIB_FONTS.
c
c  Discussion:
c
c    SMDLIB_FONTS generates a direct access file containing fonts
c    to be used by the SMDLIB graphics library.
c
c  Modified:
c
c    07 September 2008
c 
      integer maxtot
      parameter (maxtot = 22)   
      integer mxstrk
      parameter (mxstrk = 12000)

      integer maxrec
      parameter (maxrec = maxtot*(mxstrk+511)/512) 

      integer bwidth(95)
      integer bx(150)
      integer bxy(mxstrk)
      integer by(150)
      integer ibias(maxtot)
      integer icmax
      integer icmin
      integer ie
      integer ifill(512)
      integer indx(96)
      character*40 inpfil
      integer is
      character*80 kode
      integer luni
      parameter ( luni = 7 )
      integer luno
      parameter ( luno = 8 )
      integer numbyt
      parameter ( numbyt = 4 )
      character*40 outfil
      character sysid*6 

  100 FORMAT(//,
     . '     NOTE:',/,  
     . '    ======',/,  
     . '     This version of the "CONVERT" Program was designed',/, 
     . '     to incorporate all FONTS into one DIRECT ACCESS File.',/,  
     . '     Whenever the file is written, ALL fonts have to be',/, 
     . '     re-defined so that the first record of the file can',/,
     . '     be updated to reflect an accurate record of where each',/, 
     . '     subsequent font definition starts.',//,
     . '     <CR> to Continue',/)   
  200 FORMAT(/, 
     . '     Enter DIRECT ACCESS FILE Name...( 40 Char Max )',/)
  300 FORMAT(/, 
     . '     Enter SEQUENTIAL ASCII FONT File Name...( 40 Char Max )',/,
     . '     For Font ID = ',I2,'.',//, 
     . '       <CR> - TERMINATE ( ALL Fonts Processed )',/) 
  400 FORMAT(//,
     . '     DIRECT ACCESS Font File [',A,'] Generated.',/, 
     . '     It contains Font(s) 2 thru ',I2,'.',/) 
  500 FORMAT(//,' ERROR PROCESSING FONT FILE: [',A,']',/)   
  600 FORMAT(//,' ERROR IN WRITING DIRECT ACCESS FILE: [',A,']',/)

      call timestamp ( )
C   
C      ***** LOAD NEW FONT ***  
C   
      write(*,100)  
      read(*, '(a)' ,end=999) kode  
      write(*,200) 
      read(*, '(a)' ,end=999,err=999) outfil
C
C THE RECL PARAMETER IS SPECIFIED IN BYTES
C
      open (unit=luno,file=outfil,status='new',access='direct',  
     &  form='unformatted',recl=512*numbyt,err=999) 
c
      load = 2  
      ibias(1) = 0  
      ibias(2) = 1  
      nrec = 0  
    1 continue  
      write(*,300) load 
      read(*, '(a)' ,end=99,err=99) inpfil  
      if(inpfil(1:1) .eq. ' ') go to 90 
c   
      open(unit=luni,file=inpfil,status='old')  
      ibxy=0
C   
C  READ IN A CHARACTER FROM THE SEQUENTIAL FILE  AND CALCULATE 
C  THE WIDTH  
C   
      indx(1)=1 
      rewind luni   
   10 continue  
      read(luni, '(18i4)' ,end=30,err=99) jchar,nstrok,ileft,iright,   
     &                              (bx(i),by(i),i=1,7) 
      if(jchar.le.32.or.jchar.ge.128) go to 10  
      jchar=jchar-32
      bwidth(jchar)=iright-ileft
      nst=nstrok/2  
      if(nstrok.gt.16)  
     &  read(luni, '(18i4)' ,end=99,err=99) (bx(i),by(i),i=8,nst-1)
      nstrok=(nstrok-4)/2   
      if(nstrok.gt.0) then  
        do i=1,nstrok
          if(bx(i).eq.-999) then  
            ibxy=ibxy+1   
            bxy(ibxy)=-64 
          else
            ibxy=ibxy+2   
            bxy(ibxy-1)=bx(i)-ileft   
            bxy(ibxy)=by(i)   
          endif   
        end do
      endif 
      indx(jchar+1)=ibxy+1  
      go to 10  
   30 continue  
C   
C  CALCULATE HEIGHT FROM CAPITAL A  
C   
      is=indx(33)   
      ie=indx(34)-1 
      icmax=bxy(is+1)   
      icmin=bxy(is+1)   
      if(icmax.eq.-64) then 
        icmin=bxy(is+2) 
        icmax=bxy(is+2) 
      endif 
c      write(1,*) ' bxy(is,ie)',(bxy(i),i=is,ie)
      i=is-2
   40 continue  
      i=i+2 
      if(i.gt.ie-1) go to 50
      if(bxy(i).ne.-64) then
        if(bxy(i+1).gt.icmax) icmax=bxy(i+1)
        if(bxy(i+1).lt.icmin) icmin=bxy(i+1)
      else  
        i=i-1   
      endif 
      go to 40  
   50 continue  
      ihight=icmax-icmin
C   
C  SUBTRACT Y BIAS FROM Y VALUES IN BXY TO MAKE ZERO THE BASE   
C  OF ALL CAPITAL LETTERS   
C   
      nmchar=ibxy   
      i=-1  
   60 continue  
      i=i+2 
      if(i.gt.nmchar-1) go to 70
      if(bxy(i).ne.-64) then
        bxy(i+1)=bxy(i+1)-icmin 
      else  
        i=i-1   
      endif 
      go to 60  
   70 continue  
C
C  INITIALIZE THE PAD ARRAY
C
      do i = 1, 512
        ifill(i)=0
      end do
C   
C  WRITE OUT HEADER RECORD  
C   
      nrec = 1 + (nmchar+511)/512   
C<<<<<<<<<<EAH
C     WRITE(LUNO,REC=1,ERR=999) LOAD,(IBIAS(I),I=2,LOAD)
C     WRITE(LUNO,REC=1+IBIAS(LOAD),ERR=999) NMCHAR,IHIGHT,(INDX(I), 
C    &             I=1,96),(BWIDTH(I), I=1,95)  
      write(luno,rec=1,err=1000,iostat=ierr) load,(ibias(i),i=2,load),
     &     (ifill(i),i=load+1,512)
      write(luno,rec=1+ibias(load),err=1000,iostat=ierr) nmchar,ihight,
     &     (indx(i),i=1,96),(bwidth(i),i=1,95),(ifill(i),i=1,319)
C>>>>>>>>>>EAH
C   
C  WRITE OUT THE STROKES
C   
      jst = 1   
      do i = 1, nrec - 1  
        write(luno,rec=i+1+ibias(load),err=999) (bxy(j),j=jst,jst+511)
        jst = jst + 512   
      end do
      if (jst .le. nmchar) then 
        write(luno,rec=nrec+1+ibias(load),err=999) (bxy(j),j=jst,nmchar)
      endif 
      load = load + 1   
      if(load .le. maxtot) then 
      ibias(load) = ibias(load-1) + nrec + 1
      go to 1   
      endif 
   90 continue  
      load = load - 1   
      close (unit=luno) 
      close (unit=luni) 
      write(*,400) outfil(1:numchr(outfil)),load

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SMDLIB_FONTS'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      STOP  
C   
C  ERROR PROCESSING FONT FILE   
C   
   99 CONTINUE  
      WRITE(*,500) INPFIL(1:NUMCHR(INPFIL)) 
      STOP  
  999 CONTINUE  
      WRITE(*,600) OUTFIL(1:NUMCHR(OUTFIL)),IERR
      STOP  
1000  CONTINUE
      PRINT*,'ERROR:  WRITING DIRECT ACCESS FILE  IOSTAT=',IERR
      END   
      INTEGER FUNCTION NUMCHR(STRING)

c*********************************************************************72
c
cc NUMCHR
C
C  Added by JVB.
C
      CHARACTER CHAR*1
      CHARACTER STRING*(*)
C
      NUMCHR=1
      MAXCHR=LEN(STRING)

      DO 10 I=MAXCHR,1,-1
        NUMCHR=I
        IF(STRING(I:I).NE.' '.AND.STRING(I:I).NE.CHAR(0))RETURN
10    CONTINUE

      RETURN
      END
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
