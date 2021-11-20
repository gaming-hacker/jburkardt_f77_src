c  BUFPAK.F  19 February 1994
c
      subroutine buffls ( buff, nwordw, iflag )

c*********************************************************************72
c
cc BUFFLS "flushes out" the buffer, that is, it writes it to disk.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real BUFF(NWORDW).  BUFF contains NWORDW items to 
c    be written out.
c
c    Input, integer NWORDW, the number of items to be written.
c    NWORDW should be exactly the record length in REAL words.
c
c    Input, integer IFLAG.  Communication flag.
c     * -1, initialization.  BUFFLS is being called for the
c           first time, or for a new unit number, or a new file.
c     *  0, continuation.  Simply write BUFF out to the file.
c
      integer nwordw

      real buff(nwordw)
      integer i
      integer iflag
      integer iwform
      integer iwunit
      integer n
      integer nblok
      integer nbnext
      integer nbprev
      integer nbthis
      integer nwrec
      integer nwval
      integer nwwrd
      character*80 wformt
c
c  Remember, from one call to the next, the output unit number,
c  and the record number of the last record written.
c
      save iwform
      save iwunit
      save nwrec
      save nwval
      save nwwrd
      save wformt

      data iwform /0/
      data iwunit /1/
      data nwrec  /0/
      data nwval  /0/
      data nwwrd  /0/
      data wformt /' '/

      call imemry('get','iwform',iwform)
      call imemry('get','iwunit',iwunit)

      if(iwform.ne.0)then
        call smemry('get','wformt',wformt)
      endif
c
c  On an initialization call,
c    get the unit number,
c    write the first record as block marker 1,
c    set the number of records written as 1.
c
      if(iflag.eq.-1)then

        nwrec=1

        do i=1,nwordw
          buff(i)=0
        enddo
c
c  Set the record numbers of blocks 0, 1, and 2.
c  
        nblok=1

        nbprev=-1
        nbthis=1
        nbnext=0

        call imemry('set','nblok',nblok)
        call imemry('set','nbprev',nbprev)
        call imemry('set','nbthis',nbthis)
        call imemry('set','nbnext',nbnext)

        buff(1)=real(nbprev)
        buff(2)=real(nbthis)
        buff(3)=real(nbnext)
        buff(4)=real(nblok)

        if ( iwform .eq. 0 ) then
          write(iwunit,rec=nwrec)(buff(i),i=1,nwordw)
        else
          write(iwunit,wformt,rec=nwrec)(buff(i),i=1,nwordw)
        endif

        call imemry('set','nwrec',nwrec)

        nblok=1
        call imemry('set','nblok',nblok)
c
c  Update the record number, and write the record.
c
      elseif(iflag.eq.0)then

        call imemry('get','nwrec',nwrec)
        nwrec=nwrec+1

        if(iwform.eq.0)then
          write(iwunit,rec=nwrec)(buff(i),i=1,nwordw)
        else
          write(iwunit,wformt,rec=nwrec)(buff(i),i=1,nwordw)
        endif
c
c  Update the internal copy of the record number.
c
        n=1
        call imemry('inc','nwrec',n)
c
c  Update the first record of the file, to include
c
c    the number of blocks,
c    the number of records written,
c    the number of values written,
c    the number of words written.
c
      elseif(iflag.eq.1)then

        if(iwform.eq.0)then
          read(iwunit,rec=1)(buff(i),i=1,nwordw)
        else
          read(iwunit,wformt,rec=1)(buff(i),i=1,nwordw)
        endif

        call imemry('get','nblok',nblok)
        call imemry('get','nwrec',nwrec)
        call imemry('get','nwval',nwval)
        call imemry('get','nwwrd',nwwrd)

        buff(4)=real(nblok)
        buff(5)=real(nwrec)
        buff(6)=real(nwval)
        buff(7)=real(nwwrd)

        if(iwform.eq.0)then
          write(iwunit,rec=1)(buff(i),i=1,nwordw)
        else
          write(iwunit,wformt,rec=1)(buff(i),i=1,nwordw)
        endif

      endif

      return
      end
      subroutine bufref(buff,nwordr,iflag)

c*********************************************************************72
c
cc BUFREF refreshes the buffer, that is, it reads in a new line of data.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, REAL BUFF(NWORDR).  
c    On a typical call with IFLAG=0, BUFF will return NWORDR 
c    items read from the file.
c
c    Input, integer NWORDR, the number of items to be read.
c    NWORDR should be exactly the record length in REAL words.
c
c    Input, integer IFLAG.  Communication flag.
c    -1, initialization.  BUFREF is being called for the
c    first time, or for a new unit number, or a new file.
c    0, continuation.  Simply read BUFF from the file.
c    1, same as 0.
c
      integer mwordr
      parameter (mwordr=20)

      real buff(mwordr)
      integer i
      integer iflag
      integer irform
      integer irunit
      integer mrrec
      integer mrval
      integer mrwrd
      integer n
      integer nblok
      integer nrrec
      integer nwordr
      character*80 rformt

      intrinsic nint

      save irform
      save irunit
      save mrrec
      save mrval
      save mrwrd
      save nblok
      save nrrec
      save rformt

      data irform /0/
      data irunit /2/
      data mrrec  /0/
      data mrval  /0/
      data mrwrd  /0/
      data nblok  /0/
      data nrrec  /0/
      data rformt /' '/

      call imemry('get','irform',irform)

      if(irform.ne.0)then
        call smemry('get','rformt',rformt)
      endif
c
c  On an initialization call,
c    Get the unit number,
c    Read and exhaust the first record.
c    Pass the information in the first record to IMEMRY.
c
      if(iflag.eq.-1)then

        call imemry('get','irunit',irunit)

        nrrec=1

        if(irform.eq.0)then
          read(irunit,rec=nrrec)(buff(i),i=1,nwordr)
        else
          read(irunit,rformt,rec=nrrec)(buff(i),i=1,nwordr)
        endif

        nblok=nint(buff(4))
        mrrec=nint(buff(5))
        mrval=nint(buff(6))
        mrwrd=nint(buff(7))

        call imemry('set','nblok',nblok)
        call imemry('set','mrrec',mrrec)
        call imemry('set','mrval',mrval)
        call imemry('set','mrwrd',mrwrd)

        call imemry('set','nrrec',nrrec)
c
c  On second and following calls,
c    Update the record counter NRREC, 
c    Read in one more line from the file.
c
      else

        call imemry('get','nrrec',nrrec)
        nrrec=nrrec+1

        if(nrrec.le.0)then
          write(*,*)'BUFREF - Fatal error!'
          write(*,*)'Attempt to read record ',nrrec
          stop
        elseif(nrrec.gt.mrrec)then
          write(*,*)'BUFREF - Fatal error!'
          write(*,*)'Attempt to read record ',nrrec
          write(*,*)'Maximum legal record is ',mrrec
          stop
        endif

        if(irform.eq.0)then
          read(irunit,rec=nrrec)(buff(i),i=1,nwordr)
        else
          read(irunit,rformt,rec=nrrec)(buff(i),i=1,nwordr)
        endif
c
c  Update the internal copy of the record counter NRREC by 
c  adding N=1 to it.
c
        n=1
        call imemry('inc','nrrec',n)

      endif

      return
      end
      subroutine capchr(string)

c*********************************************************************72
c
cc CAPCHR accepts a STRING of characters and replaces any lowercase
c  letters by uppercase ones.
c
c  Compare LOWCHR which lowercases a string.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING Input/output, CHARACTER*(*) STRING, the string of
c         characters to be transformed.
c
      integer i
      integer itemp
      integer nchar
      character*(*) string

      intrinsic char
      intrinsic ichar
      intrinsic len

      nchar=len(string)

      do i=1,nchar

        itemp=ichar(string(i:i))

        if(97.le.itemp.and.itemp.le.122)then
          string(i:i)=char(itemp-32)
        endif

      enddo

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
      subroutine imemry(op,name,number)

c*********************************************************************72
c
cc IMEMRY "remembers" integer values that the user may set or
c  retrieve.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  OP     Input, CHARACTER*(*) OP, the "operation" to be carried
c         out.  Currently, OP may be one of the following:
c
c         'get' - return in NUMBER the value of NAME.
c
c         'inc' - increment the value of NAME by NUMBER.
c
c         'set' - set the value of NAME to NUMBER.
c
c  NAME   Input, CHARACTER*(*) NAME, the name of the quantity
c         to which the operation is to be applied.
c
c         The list of quantities available is:
c
c
c         IRECL  The record length of the file.
c 
c                WARNING - Right now, you should not try to
c                use the SET or INC commands to alter IRECL.
c
c         IRFORM The form of the read file.
c                0, unformatted
c                1, formatted
c
c         IRUNIT The unit number for reading.
c
c         IWFORM The form of the write file.
c                0, unformatted.
c                1, formatted.
c
c         IWUNIT The unit number for writing.
c
c         MRREC  The number of records in the READ file.
c
c         MRVAL  The number of values in the READ file.
c
c         MRWRD  The number of words in the READ file.
c
c         NBLOK  The number of block markers in the file.
c
c         NBNEXT The next block.
c
c         NBPREV The previous block.
c
c         NBTHIS The current block.
c
c         NFFAC  The factor times the number of words that
c                gives the record length of a formatted
c                record.  This would normally be the number
c                of characters used to print a single real,
c                so a format of G14.6 would mean NFFAC=14.
c
c         NRCH   The number of characters read.
c
c         NRCM   The number of complex values read.
c
c         NRDB   The number of double precision values read.
c
c         NRDR   The number of double precision/reals read.
c
c         NRIN   The number of integers read.
c
c         NRLG   The number of logical values read.
c
c         NRREC  The number of records read.
c
c         NRRL   The number of reals read.
c
c         NRVAL  The number of values read.
c
c         NRWRD  The number of words read.
c
c         NUFAC  The factor times the number of words that
c                gives the record length of an unformatted
c                record.  On computes which measure an unformatted
c                record in words, NUFAC should be 1.  On
c                the Cray, in particular, NUFAC should be 8.
c
c         NWCH   The number of characters written.
c
c         NWCM   The number of complex values written.
c
c         NWDB   The number of double precision values written.
c
c         NWDR   The number of double precision/reals written.
c
c         NWIN   The number of integers written.
c
c         NWLG   The number of logical values written.
c
c         NWORDR The number of words per record in read file.
c                NWORDR may not be greater than MWORDR, an
c                internally specified limit.
c
c         NWORDW The number of words per record in write file.
c                NWORDW may not be greater than MWORDW, an
c                internally specified limit.
c
c         NWREC  The number of records written.
c
c         NWRL   The number of reals written.
c
c         NWVAL  The number of values written.
c
c         NWWRD  The number of words written.
c
c  NUMBER Input/output, integer NUMBER, the number used in the
c         operation.
c
c         If OP is 'get', then NUMBER is an output quantity, and
c         contains the value of the named variable.
c
c         If OP is 'inc', then NUMBER is an input quantity, and
c         contains the amount by which the named variable should
c         be incremented.
c
c         If OP is 'set', then NUMBER is an input quantity, and
c         contains the value to which the named variable should
c         be set.
c
      integer mwordr
      parameter (mwordr=20)

      integer mwordw
      parameter (mwordw=20)

      integer irform
      integer irunit
      integer iwform
      integer iwunit
      integer lenn
      logical leqi
      integer mrrec
      integer mrval
      integer mrwrd
      character*(*) name
      integer nblok
      integer nbnext
      integer nbprev
      integer nbthis
      integer nffac
      integer nrch
      integer nrcm
      integer nrdb
      integer nrdr
      integer nrin
      integer nrlg
      integer nrrl
      integer nrrec
      integer nrval
      integer nrwrd
      integer nufac
      integer number
      integer nwch
      integer nwcm
      integer nwdb
      integer nwdr
      integer nwin
      integer nwlg
      integer nwordr
      integer nwordw
      integer nwrl
      integer nwrec
      integer nwval
      integer nwwrd
      character*(*) op

      intrinsic len
      external leqi

      save irform
      save irunit
      save iwform
      save iwunit
      save mrrec
      save mrval
      save mrwrd
      save nblok
      save nbnext
      save nbprev
      save nbthis
      save nffac
      save nrch
      save nrcm 
      save nrdb
      save nrdr
      save nrin
      save nrlg
      save nrrl
      save nrrec
      save nrval
      save nrwrd
      save nufac
      save nwch
      save nwcm
      save nwdb
      save nwdr
      save nwin
      save nwlg
      save nwordr
      save nwordw
      save nwrl
      save nwrec
      save nwval
      save nwwrd

      data irform /0/
      data irunit /2/
      data iwform  /0/
      data iwunit /1/
      data mrrec  /0/
      data mrval  /0/
      data mrwrd  /0/
      data nblok  /0/
      data nbnext /0/
      data nbprev /0/
      data nbthis /0/
      data nffac  /14/
      data nrch   /0/
      data nrcm   /0/
      data nrdb   /0/
      data nrdr   /0/
      data nrin   /0/
      data nrlg   /0/
      data nrrl   /0/
      data nrrec  /0/
      data nrval  /0/
      data nrwrd  /0/
      data nufac  /1/
      data nwch   /0/
      data nwcm   /0/
      data nwdb   /0/
      data nwdr   /0/
      data nwin   /0/
      data nwlg   /0/
      data nwordr /20/
      data nwordw /20/
      data nwrl   /0/
      data nwrec  /0/
      data nwval  /0/
      data nwwrd  /0/

      if(leqi(name,'irform'))then

        if(leqi(op,'set'))then

          if(number.lt.0.or.number.gt.1)then
            write(*,*)'IMEMRY - Fatal error!'
            write(*,*)'Illegal value assigned to IRFORM=',number
            write(*,*)'Legal values are 0 and 1.'
            stop
          endif

          irform=number

        elseif(leqi(op,'get'))then
          number=irform
        endif

      elseif(leqi(name,'iwform'))then

        if(leqi(op,'set'))then

          if(number.lt.0.or.number.gt.1)then
            write(*,*)'IMEMRY - Fatal error!'
            write(*,*)'Illegal value assigned to IWFORM=',number
            write(*,*)'Legal values are 0 and 1.'
            stop
          endif

          iwform=number

        elseif(leqi(op,'get'))then
          number=iwform
        endif

      elseif(leqi(name,'irunit'))then

        if(leqi(op,'set'))then
          irunit=number
        elseif(leqi(op,'get'))then
          number=irunit
        elseif(leqi(op,'inc'))then
          irunit=irunit+number
        endif

      elseif(leqi(name,'iwunit'))then

        if(leqi(op,'set'))then
          iwunit=number
        elseif(leqi(op,'get'))then
          number=iwunit
        elseif(leqi(op,'inc'))then
          iwunit=iwunit+number
        endif

      elseif(leqi(name,'mrrec'))then

        if(leqi(op,'set'))then
          mrrec=number
        elseif(leqi(op,'get'))then
          number=mrrec
        elseif(leqi(op,'inc'))then
          mrrec=mrrec+number
        endif

      elseif(leqi(name,'mrval'))then

        if(leqi(op,'set'))then
          mrval=number
        elseif(leqi(op,'get'))then
          number=mrval
        elseif(leqi(op,'inc'))then
          mrval=mrval+number
        endif

      elseif(leqi(name,'mrwrd'))then

        if(leqi(op,'set'))then
          mrwrd=number
        elseif(leqi(op,'get'))then
          number=mrwrd
        elseif(leqi(op,'inc'))then
          mrwrd=mrwrd+number
        endif

      elseif(leqi(name,'nblok'))then

        if(leqi(op,'set'))then
          nblok=number
        elseif(leqi(op,'get'))then
          number=nblok
        elseif(leqi(op,'inc'))then
          nblok=nblok+number
        endif

      elseif(leqi(name,'nbnext'))then

        if(leqi(op,'set'))then
          nbnext=number
        elseif(leqi(op,'get'))then
          number=nbnext
        elseif(leqi(op,'inc'))then
          nbnext=nbnext+number
        endif

      elseif(leqi(name,'nbprev'))then

        if(leqi(op,'set'))then
          nbprev=number
        elseif(leqi(op,'get'))then
          number=nbprev
        elseif(leqi(op,'inc'))then
          nbprev=nbprev+number
        endif

      elseif(leqi(name,'nbthis'))then

        if(leqi(op,'set'))then
          nbthis=number
        elseif(leqi(op,'get'))then
          number=nbthis
        elseif(leqi(op,'inc'))then
          nbthis=nbthis+number
        endif

      elseif(leqi(name,'nffac'))then

        if(leqi(op,'set'))then
          nffac=number
        elseif(leqi(op,'get'))then
          number=nffac
        elseif(leqi(op,'inc'))then
          nffac=nffac+number
        endif

      elseif(leqi(name,'nrch'))then

        if(leqi(op,'set'))then
          nrch=number
        elseif(leqi(op,'get'))then
          number=nrch
        elseif(leqi(op,'inc'))then
          nrch=nrch+number
          nrwrd=nrwrd+number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrcm'))then

        if(leqi(op,'set'))then
          nrcm=number
        elseif(leqi(op,'get'))then
          number=nrcm
        elseif(leqi(op,'inc'))then
          nrcm=nrcm+number
          nrwrd=nrwrd+2*number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrdb'))then

        if(leqi(op,'set'))then
          nrdb=number
        elseif(leqi(op,'get'))then
          number=nrdb
        elseif(leqi(op,'inc'))then
          nrdb=nrdb+number
          nrwrd=nrwrd+2*number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrdr'))then

        if(leqi(op,'set'))then
          nrdr=number
        elseif(leqi(op,'get'))then
          number=nrdr
        elseif(leqi(op,'inc'))then
          nrdr=nrdr+number
          nrwrd=nrwrd+number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrin'))then

        if(leqi(op,'set'))then
          nrin=number
        elseif(leqi(op,'get'))then
          number=nrin
        elseif(leqi(op,'inc'))then
          nrin=nrin+number
          nrwrd=nrwrd+number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrlg'))then

        if(leqi(op,'set'))then
          nrlg=number
        elseif(leqi(op,'get'))then
          number=nrlg
        elseif(leqi(op,'inc'))then
          nrlg=nrlg+number
          nrwrd=nrwrd+number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrrec'))then

        if(leqi(op,'set'))then
          nrrec=number
        elseif(leqi(op,'get'))then
          number=nrrec
        elseif(leqi(op,'inc'))then
          nrrec=nrrec+number
        endif

      elseif(leqi(name,'nrrl'))then

        if(leqi(op,'set'))then
          nrrl=number
        elseif(leqi(op,'get'))then
          number=nrrl
        elseif(leqi(op,'inc'))then
          nrrl=nrrl+number
          nrwrd=nrwrd+number
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrval'))then

        if(leqi(op,'set'))then
          nrval=number
        elseif(leqi(op,'get'))then
          number=nrval
        elseif(leqi(op,'inc'))then
          nrval=nrval+number
        endif

      elseif(leqi(name,'nrwrd'))then

        if(leqi(op,'set'))then
          nrwrd=number
        elseif(leqi(op,'get'))then
          number=nrwrd
        elseif(leqi(op,'inc'))then
          nrwrd=nrwrd+number
        endif

      elseif(leqi(name,'nufac'))then

        if(leqi(op,'set'))then
          nufac=number
        elseif(leqi(op,'get'))then
          number=nufac
        elseif(leqi(op,'inc'))then
          nufac=nufac+number
        endif

      elseif(leqi(name,'nwch'))then

        if(leqi(op,'set'))then
          nwch=number
        elseif(leqi(op,'get'))then
          number=nwch
        elseif(leqi(op,'inc'))then
          nwch=nwch+number
          nwwrd=nwwrd+number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwcm'))then

        if(leqi(op,'set'))then
          nwcm=number
        elseif(leqi(op,'get'))then
          number=nwcm
        elseif(leqi(op,'inc'))then
          nwcm=nwcm+number
          nwwrd=nwwrd+2*number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwdb'))then

        if(leqi(op,'set'))then
          nwdb=number
        elseif(leqi(op,'get'))then
          number=nwdb
        elseif(leqi(op,'inc'))then
          nwdb=nwdb+number
          nwwrd=nwwrd+2*number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwdr'))then

        if(leqi(op,'set'))then
          nwdr=number
        elseif(leqi(op,'get'))then
          number=nwdr
        elseif(leqi(op,'inc'))then
          nwdr=nwdr+number
          nwwrd=nwwrd+number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwin'))then

        if(leqi(op,'set'))then
          nwin=number
        elseif(leqi(op,'get'))then
          number=nwin
        elseif(leqi(op,'inc'))then
          nwin=nwin+number
          nwwrd=nwwrd+number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwlg'))then

        if(leqi(op,'set'))then
          nwlg=number
        elseif(leqi(op,'get'))then
          number=nwlg
        elseif(leqi(op,'inc'))then
          nwlg=nwlg+number
          nwwrd=nwwrd+number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwordr'))then

        if(leqi(op,'set'))then

          if(number.le.mwordr)then
            nwordr=number
          else
            write(*,*)'IMEMRY - Serious error!'
            write(*,*)'NWORDR must be no greater than ',mwordr
            write(*,*)'but you requested a value ',number
            return
          endif

        elseif(leqi(op,'get'))then

          number=nwordr

        elseif(leqi(op,'inc'))then

          if(nwordr+number.le.mwordr)then
            nwordr=nwordr+number
          else
            write(*,*)'IMEMRY - Serious error!'
            write(*,*)'NWORDR must be no greater than ',mwordr
            write(*,*)'but you requested a value ',nwordr+number
            return
          endif

        endif

      elseif(leqi(name,'nwordw'))then

        if(leqi(op,'set'))then

          if(number.le.mwordw)then
            nwordw=number
          else
            write(*,*)'IMEMRY - Serious error!'
            write(*,*)'NWORDW must be no greater than ',mwordw
            write(*,*)'but you requested a value ',number
            return
          endif

        elseif(leqi(op,'get'))then

          number=nwordw

        elseif(leqi(op,'inc'))then

          if(nwordw+number.le.mwordw)then
            nwordw=nwordw+number
          else
            write(*,*)'IMEMRY - Serious error!'
            write(*,*)'NWORDW must be no greater than ',mwordw
            write(*,*)'but you requested a value ',nwordw+number
            return
          endif

        endif

      elseif(leqi(name,'nwrec'))then

        if(leqi(op,'set'))then
          nwrec=number
        elseif(leqi(op,'get'))then
          number=nwrec
        elseif(leqi(op,'inc'))then
          nwrec=nwrec+number
        endif

      elseif(leqi(name,'nwrl'))then

        if(leqi(op,'set'))then
          nwrl=number
        elseif(leqi(op,'get'))then
          number=nwrl
        elseif(leqi(op,'inc'))then
          nwrl=nwrl+number
          nwwrd=nwwrd+number
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwval'))then

        if(leqi(op,'set'))then
          nwval=number
        elseif(leqi(op,'get'))then
          number=nwval
        elseif(leqi(op,'inc'))then
          nwval=nwval+number
        endif

      elseif(leqi(name,'nwwrd'))then

        if(leqi(op,'set'))then
          nwwrd=number
        elseif(leqi(op,'get'))then
          number=nwwrd
        elseif(leqi(op,'inc'))then
          nwwrd=nwwrd+number
        endif

      else

        lenn=len(name)

        write(*,*)'IMEMRY - Warning!'
        write(*,*)'Unrecognized name!'
        write(*,'(1x,a)')name(1:lenn)

      endif

      return
      end
      function lenchr(string)

c*********************************************************************72
c
cc LENCHR returns the length of STRING up to the last nonblank
c  character.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING Input, CHARACTER*(*) STRING, the string to be measured.
c
c  LENCHR Output, integer LENCHR, the location of the last nonblank
c         character in STRING.
c
      integer i
      integer lchar
      integer lenchr
      integer nchar
      character*(*) string

      intrinsic char
      intrinsic len

      nchar=len(string)

      do i=1,nchar
        lchar=nchar+1-i
        if(string(lchar:lchar).ne.' '.and.
     &     string(lchar:lchar).ne.char(0))go to 20
      enddo

      lchar=0

20    continue

      lenchr=lchar

      return
      end
      function leqi(strng1,strng2)

c*********************************************************************72
c
cc LEQI returns TRUE if two strings are the same, ignoring case
c  differences.
c
c  There is no FORTRAN LEQ function, but if there were, it would be
c  case sensitive.  LEQI is a case insensitive comparison of two 
c  strings for equality.  Thus, leqi('Anjana','ANJANA') is .true.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRNG1,
c  STRNG2 Input, CHARACTER*(*) STRNG1, STRNG2, the strings to 
c         compare.
c
c  LEQI   Output, LOGICAL LEQI, the result of the comparison.
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

      intrinsic len
      intrinsic min

      len1=len(strng1)
      len2=len(strng2)
      lenc=min(len1,len2)

      leqi=.false.

      do i=1,lenc
        s1=strng1(i:i)
        s2=strng2(i:i)
        call capchr(s1)
        call capchr(s2)
        if(s1.ne.s2)return
      enddo

      if(len1.gt.lenc.and.strng1(lenc+1:len1).ne.' ')return
      if(len2.gt.lenc.and.strng2(lenc+1:len2).ne.' ')return

      leqi=.true.

      return
      end
      subroutine rdblok(iblok)

c*********************************************************************72
c
cc RDBLOK advances through the file to block number IBLOK.
c
c  RDBLOK can only advance to a specific block if WRBLOK has
c  been used to write block markers in the file.
c
c  The first record of the file is always the marker for block 1.
c
c  The user is free to insert more markers elsewhere in the file
c  by calling WRBLOK.  
c
c  When WRLAST is called to close the file, a final block marker
c  is written as the last record of the file.
c
c  Thus, the number of block markers in the file will be two more 
c  than the number of calls to WRBLOK.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IBLOK  Input, integer IBLOK, the block to which we should
c         advance.
c
      integer mwordr
      parameter (mwordr=20)

      real buff(mwordr)
      integer i
      integer iblok
      integer iflag
      integer irform
      integer irunit
      integer j
      integer mrrec
      integer nbnext
      integer nbprev
      integer nbthis
      integer nrrec
      integer nwordr
      character*80 rformt
      real x

      save irform
      save irunit
      save mrrec
      save nbprev
      save nwordr
      save rformt

      data irform /0/
      data irunit /0/
      data mrrec  /0/
      data nbprev /0/
      data nwordr /20/
      data rformt /' '/

      call imemry('get','irunit',irunit)
      call imemry('get','irform',irform)
      call imemry('get','mrrec',mrrec)
      call imemry('get','nwordr',nwordr)

      if(irform.ne.0)then
        call smemry('get','rformt',rformt)
      endif

      nbthis=1
      nbnext=1

      do i=1,iblok

        nrrec=nbnext

        if(nrrec.le.0)then
          write(*,*)'RDBLOK - Serious error!'
          write(*,*)'The address of the next block is ',nrrec
          return
        elseif(nrrec.gt.mrrec)then
          write(*,*)'RDBLOK - Serious error!'
          write(*,*)'The address of the next block is ',nrrec
          write(*,*)'but the maximum allowed is ',mrrec
          return
        endif

        if(irform.eq.0)then
          read(irunit,rec=nrrec)(buff(j),j=1,nwordr)
        else
          read(irunit,rformt,rec=nrrec)(buff(j),j=1,nwordr)
        endif

        nbprev=nint(buff(1))
        nbthis=nint(buff(2))
        nbnext=nint(buff(3))

      enddo

      nrrec=nbthis
      call imemry('set','nbprev',nbprev)
      call imemry('set','nbthis',nbthis)
      call imemry('set','nbnext',nbnext)

      call imemry('set','nrrec',nrrec)
c
c  Exhaust the buffer.
c
      iflag=2
      call rdbuf(x,iflag)

      return
      end
      subroutine rdbuf(x,iflag)

c*********************************************************************72
c
cc RDBUF reads a real value X from the buffer.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  X      Output, REAL X, usually a value read from the buffer.
c
c  IFLAG  Input, integer IFLAG, communication flag.
c
c         -1, then X is ignored.  This routine and BUFREF are 
c         initialized.
c
c         0, then if neccessary the buffer is refreshed, and
c         X is read from the buffer.
c
c         1, then the buffer is refreshed by reading from the
c         file.
c
c         2, the buffer is exhausted.  That is, RDBUF is told
c         that no further data should be read from the current
c         buffer.
c
      integer mwordr
      parameter (mwordr=20)

      real buff(mwordr+1)
      integer iflag
      integer nbuff
      integer nwordr
      real x

      save buff
      save nbuff
      save nwordr

      data nbuff /-1/
      data nwordr /20/

      call imemry('get','nwordr',nwordr)
c
c  Initialize.
c
      if(iflag.eq.-1)then

        call bufref(buff,nwordr,iflag)
        nbuff=nwordr+1
c
c  If necessary, refresh the buffer, and then extract one
c  value from the buffer.
c
      elseif(iflag.eq.0)then

        if(nbuff.gt.nwordr)then

          call bufref(buff,nwordr,iflag)
          nbuff=1

        endif

        x=buff(nbuff)
        nbuff=nbuff+1
c
c  Refresh the buffer.
c
      elseif(iflag.eq.1)then

        call bufref(buff,nwordr,iflag)
        nbuff=1
c
c  Exhaust the buffer.
c
      else

        nbuff=nwordr+1

      endif

      return
      end
      subroutine rdch(chval)

c*********************************************************************72
c
cc RDCH reads a single character from the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHVAL  Output, CHARACTER*1 CHVAL, a single character from the
c         file.
c
      integer iflag
      parameter (iflag=0)

      character*1 chval
      integer j
      integer n
      real x

      intrinsic char
      intrinsic max
      intrinsic min
      intrinsic nint

      call rdbuf(x,iflag)

      j=nint(x)

      if(j.lt.0.or.j.gt.255)then
        write(*,*)'RDCH - Warning!'
        write(*,*)'Illegal ASCII index of character data!'
        write(*,*)'The ASCII index was ',j
        j=0
      endif

      chval=char(j)

      n=1
      call imemry('inc','nrch',n)

      return
      end
      subroutine rdchv(chvec,n)

c*********************************************************************72
c
cc RDCHV reads a string of N characters from a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHVEC  Output, CHARACTER*N CHVEC, a character string read
c         from the file.
c
c  N      Input, integer N, the number of characters in the string.
c
      integer iflag
      parameter (iflag=0)

      character*(*) chvec
      integer i
      integer j
      integer n
      real x

      intrinsic char
      intrinsic max
      intrinsic min
      intrinsic nint

      do i=1,n
        call rdbuf(x,iflag)
        j=nint(x)

        if(j.lt.0.or.j.gt.255)then
          write(*,*)'RDCH - Warning!'
          write(*,*)'Illegal ASCII index of character data!'
          write(*,*)'The ASCII index was ',j
          j=0
        endif

        chvec(i:i)=char(j)
      enddo

      call imemry('inc','nrch',n)

      return
      end
      subroutine rdcm(cval)

c*********************************************************************72
c
cc RDCM reads a complex value from the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CVAL   Output, COMPLEX CVAL, a complex value read from the file.
c
      integer iflag
      parameter (iflag=0)

      complex cval
      integer n
      real x1
      real x2

      intrinsic cmplx

      call rdbuf(x1,iflag)
      call rdbuf(x2,iflag)

      cval=cmplx(x1,x2)

      n=1
      call imemry('inc','nrcm',n)

      return
      end
      subroutine rdcmv(cvec,n)

c*********************************************************************72
c
cc RDCMV reads a vector of complex values from a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CVEC   Output, COMPLEX CVEC(N), the vector of complex values.
c
c  N      Input, integer N, the number of values to read.
c
      integer iflag
      parameter (iflag=0)

      integer n

      complex cvec(n)
      integer i
      real x1
      real x2

      intrinsic cmplx

      do i=1,n

        call rdbuf(x1,iflag)
        call rdbuf(x2,iflag)

        cvec(i)=cmplx(x1,x2)

      enddo

      call imemry('inc','nrcm',n)

      return
      end
      subroutine rddb(dval)

c*********************************************************************72
c
cc RDDB reads a double precision value from the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVAL   Output, DOUBLE PRECISION DVAL, a double precision value
c         read from the file.
c
      integer iflag
      parameter (iflag=0)

      double precision dval
      integer n
      real x

      intrinsic dble
      intrinsic sngl

      call rdbuf(x,iflag)
      dval=dble(x)
      call rdbuf(x,iflag)
      dval=dval+dble(x)

      n=1
      call imemry('inc','nrdb',n)

      return
      end
      subroutine rddbv(dvec,n)

c*********************************************************************72
c
cc RDDBV reads a vector of double precision values from a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVEC   Output, DOUBLE PRECISION DVEC(N), the vector of double
c         precision values.
c
c  N      Input, integer N, the number of values to read.
c
      integer iflag
      parameter (iflag=0)

      integer n

      double precision dvec(n)
      integer i
      real x

      intrinsic dble
      intrinsic sngl

      do i=1,n
        call rdbuf(x,iflag)
        dvec(i)=dble(x)
        call rdbuf(x,iflag)
        dvec(i)=dvec(i)+dble(x)
      enddo

      call imemry('inc','nrdb',n)

      return
      end
      subroutine rddr(dval)

c*********************************************************************72
c
cc RDDR reads a real value from a file, and returns it as a
c  double precision value.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVAL   Output, DOUBLE PRECISION DVAL, a double precision value,
c         which had been stored in the file as a real value.
c
      integer iflag
      parameter (iflag=0)

      double precision dval
      integer n
      real x

      intrinsic dble

      call rdbuf(x,iflag)
      dval=dble(x)

      n=1
      call imemry('inc','nrdr',n)

      return
      end
      subroutine rddrv(dvec,n)

c*********************************************************************72
c
cc RDDRV returns a vector of double precision values, which had
c  been stored in the file as real values.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVEC   Output, DOUBLE PRECISION DVEC(N), the vector of double
c         precision values that had been stored in the file as
c         real values.
c
c  N      Input, integer N, the number of values to read.
c
      integer iflag
      parameter (iflag=0)

      integer n

      double precision dvec(n)
      integer i
      real x

      intrinsic dble

      do i=1,n
        call rdbuf(x,iflag)
        dvec(i)=dble(x)
      enddo

      call imemry('inc','nrdr',n)

      return
      end
      subroutine rdfrst(filnam,ierror)

c*********************************************************************72
c
cc RDFRST should be called first, before doing any reading from the file.
c
c  RDFRST will properly initialize subroutines RDBUF and BUFREF.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  FILNAM Input, CHARACTER*(*), the name of the file containing
c         the data.
c
c  IERROR Output, integer IERROR, error flag.
c
c         0, no error.
c         1, the file could not be opened.
c
      integer iflag
      parameter (iflag=-1)

      character*(*) filnam
      integer ierror
      integer irform
      integer irunit
      integer lenrec
      integer nffac
      integer nufac
      integer nwordr
      real x

      intrinsic nint

      save irform
      save irunit
      save nffac
      save nufac
      save nwordr

      data irform /0/
      data irunit /2/
      data nffac  /14/
      data nufac  /1/
      data nwordr /20/

      call imemry('get','irunit',irunit)
      call imemry('get','nwordr',nwordr)
      call imemry('get','irform',irform)
c
c  Open the file.
c
      if(irform.eq.0)then

        call imemry('get','nufac',nufac)

        lenrec = nwordr * nufac

        open(unit=irunit,file=filnam,form='unformatted',
     &    access='direct',recl=lenrec,err=10,status='old')

      else

        call imemry('get','nffac',nffac)

        lenrec=nwordr*nffac

        open(unit=irunit,file=filnam,form='formatted',
     &    access='direct',recl=lenrec,err=10,status='old')

      endif

      ierror=0

      call rdbuf(x,iflag)

      return

10    continue
      ierror=1
      write(*,*)'RDFRST - Serious error!'
      write(*,*)'The file could not be opened!'
      return
      end
      subroutine rdin(ival)

c*********************************************************************72
c
cc RDIN reads an integer value from the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IVAL   Output, integer IVAL, the integer value stored in the 
c         file.
c
      integer iflag
      parameter (iflag=0)

      integer ival
      integer n
      real x

      intrinsic nint

      call rdbuf(x,iflag)
      ival=nint(x)

      n=1
      call imemry('inc','nrin',n)

      return
      end
      subroutine rdinv(ivec,n)

c*********************************************************************72
c
cc RDINV reads a vector of integer information stored in the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IVEC   Output, integer IVEC(N), the vector of integer 
c         information.
c
c  N      Input, integer N, the number of values to be read from
c         the file.
c
      integer iflag
      parameter (iflag=0)

      integer n

      integer i
      integer ivec(n)
      real x

      intrinsic nint

      do i=1,n
        call rdbuf(x,iflag)
        ivec(i)=nint(x)
      enddo

      call imemry('inc','nrin',n)

      return
      end
      subroutine rdlast

c*********************************************************************72
c
cc RDLAST terminates reading from the file, and closes it.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      integer irunit

      save irunit

      data irunit /2/

      call imemry('get','irunit',irunit)

      close(unit=irunit)

      return
      end
      subroutine rdnext

c*********************************************************************72
c
cc RDNEXT reads the next record from the file.  
c
c  Any items in the current buffer are discarded, and the
c  buffer is refreshed with items from the next record.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      integer iflag
      parameter (iflag=1)

      real x

      call rdbuf(x,iflag)

      return
      end
      subroutine rdlg(lval)

c*********************************************************************72
c
cc RDLG reads a logical value from the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  LVAL   Output, LOGICAL LVAL, a logical value read from the file.
c
      integer iflag
      parameter (iflag=0)

      logical lval
      integer n
      real x

      call rdbuf(x,iflag)
      lval=x.eq.1.0

      n=1
      call imemry('inc','nrlg',n)

      return
      end
      subroutine rdlgv(lvec,n)

c*********************************************************************72
c
cc RDLGV reads a vector of logical values from a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  LVEC   Output, LOGICAL LVEC(N), the vector of logical values.
c
c  N      Input, integer N, the number of values to read.
c
      integer iflag
      parameter (iflag=0)

      integer n

      integer i
      logical lvec(n)
      real x

      do i=1,n
        call rdbuf(x,iflag)
        lvec(i)=x.eq.1.0
      enddo

      call imemry('inc','nrlg',n)

      return
      end
      subroutine rdrl(rval)

c*********************************************************************72
c
cc RDRL reads a real value from the file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  RVAL   Output, REAL RVAL, the real value read from the file.
c
      integer iflag
      parameter (iflag=0)

      integer n
      real rval

      call rdbuf(rval,iflag)

      n=1
      call imemry('inc','nrrl',n)

      return
      end
      subroutine rdrlv(rvec,n)

c*********************************************************************72
c
cc RDRLV reads a vector of real data from a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  RVEC   Output, REAL RVEC(N), the vector of real data.
c
c  N      Input, integer N, the number of items to read.
c
      integer iflag
      parameter (iflag=0)

      integer n

      integer i
      real rvec(n)

      do i=1,n
        call rdbuf(rvec(i),iflag)
      enddo

      call imemry('inc','nrrl',n)

      return
      end
      subroutine smemry(op,name,string)

c*********************************************************************72
c
cc SMEMRY "remembers" string values that the user may set or 
c  retrieve.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  OP     Input, CHARACTER*(*) OP, the "operation" to be carried
c         out.  Currently, OP may be one of the following:
c
c         'get' - return in STRING the value of NAME.
c
c         'set' - set the value of NAME to STRING.
c
c  NAME   Input, CHARACTER*(*) NAME, the name of the quantity
c         to which the operation is to be applied.
c
c         The list of quantities available is:
c
c           RFILE  - The name of the file being read.
c
c           RFORMT - The format used for reading, if the file is 
c                    formatted.
c
c           WFILE  - The name of the file being written.
c
c           WFORMT - The format used for writing, if the file is 
c                    formatted.
c
c  STRING Input/output, CHARACTER*(*) STRING, the string used in 
c         the operation.
c
c         If OP is 'get', then STRING is an output quantity, and
c         contains the value of the named variable.
c
c         If OP is 'set', then STRING is an input quantity, and
c         contains the value to which the named variable should
c         be set.
c
      logical leqi
      character*(*) name
      character*(*) op
      character*80 rfile
      character*80 rformt
      character*(*) string
      character*80 wfile
      character*80 wformt

      external leqi

      save rfile
      save rformt
      save wfile
      save wformt

      data rfile  /'bufrd.dat'/
      data rformt /'(20g14.6)'/
      data wfile  /'bufwt.dat'/
      data wformt /'(20g14.6)'/

      if(leqi(name,'rfile'))then

        if(leqi(op,'set'))then

          rfile=string

        elseif(leqi(op,'get'))then
 
          string=rfile
 
        endif
 
      elseif(leqi(name,'rformt'))then

        if(leqi(op,'set'))then

          rformt=string

        elseif(leqi(op,'get'))then
 
          string=rformt
 
        endif
 
      elseif(leqi(name,'wfile'))then

        if(leqi(op,'set'))then

          wfile=string

        elseif(leqi(op,'get'))then
 
          string=wfile
 
        endif
 
      elseif(leqi(name,'wformt'))then

        if(leqi(op,'set'))then

          wformt=string

        elseif(leqi(op,'get'))then
 
          string=wformt
 
        endif

      endif
 
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
      subroutine wrblok

c*********************************************************************72
c
cc WRBLOK writes a block marker to a file.
c
c  A block marker is a record that does not contain data.
c  Instead, it contains information about the location of the
c  previous and next blocks.
c
c  This allows a user to group data into blocks, which might
c  correspond to a specific time step, or particular value of
c  a parameter.  Simply call WRBLOK to write a block marker
c  before the data (if you wish to access it quickly) or
c  immediately after it (if you wish to skip over it).
c
c  A typical set of calls might be:
c
c    CALL WRFRST  <-- automatically writes block marker 1.
c    CALL WRRLV   <-- write a bunch of real numbers.
c    CALL WRINV   <-- write some integers
c    CALL WRBLOK  <-- write block marker 2.
c    CALL WRRLV   <-- write some more real numbers.
c    CALL WRBLOK  <-- write block marker 3.
c    CALL WRRLV   <-- write some more real numbers.
c    CALL WRLAST  <-- last call for writing.
c
c  Then the user can easily return to the beginning of any
c  block of data by using the RDBLOK routine.  That is,
c  for instance, to read just the information between block
c  marker 2 and 3, the user might use the program
c
c    CALL RDFRST
c    CALL RDBLOK(2)
c    CALL RDRLV
c    CALL RDLAST
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      integer mwordw
      parameter (mwordw=20)

      real buff(mwordw)
      integer i
      integer iflag
      integer iwform
      integer iwunit
      integer n
      integer nbnext
      integer nbprev
      integer nbthis
      integer nwordw
      integer nwrec
      character*80 wformt
      real x

      intrinsic nint

      save iwform
      save iwunit
      save nbnext
      save nbprev
      save nbthis
      save nwordw
      save nwrec
      save wformt

      data iwform /0/
      data iwunit /1/
      data nbnext /0/
      data nbprev /0/
      data nbthis /0/
      data nwordw /20/
      data nwrec  /0/
      data wformt /' '/

      call imemry('get','iwunit',iwunit)
      call imemry('get','iwform',iwform)
      call imemry('get','nwordw',nwordw)

      if(iwform.ne.0)then
        call smemry('get','wformt',wformt)
      endif
c
c  Flush the buffer.
c
      x=0
      iflag=1
      call wrbuf(x,iflag)

      call imemry('get','nwrec',nwrec)
c
c  Update the pointer at the previous block to this block.
c
      call imemry('get','nbthis',nbthis)

      if(iwform.eq.0)then
        read(iwunit,rec=nbthis)(buff(i),i=1,nwordw)
      else
        read(iwunit,wformt,rec=nbthis)(buff(i),i=1,nwordw)
      endif

      nbprev=nint(buff(1))
      nbnext=nwrec+1
      buff(3)=real(nbnext)

      if(iwform.eq.0)then
        write(iwunit,rec=nbthis)(buff(i),i=1,nwordw)
      else
        write(iwunit,wformt,rec=nbthis)(buff(i),i=1,nwordw)
      endif
c
c  Set the pointers at the current block.
c
      nwrec=nwrec+1

      do i=1,mwordw
        buff(i)=0
      enddo

      nbprev=nbthis
      nbthis=nwrec
      nbnext=0

      buff(1)=nbprev
      buff(2)=nbthis
      buff(3)=nbnext

      if(iwform.eq.0)then
        write(iwunit,rec=nwrec)(buff(i),i=1,nwordw)
      else
        write(iwunit,wformt,rec=nwrec)(buff(i),i=1,nwordw)
      endif

      call imemry('set','nbprev',nbprev)
      call imemry('set','nbthis',nbthis)
      call imemry('set','nbnext',nbnext)
c
c  Update the number of block markers written.
c
      n=1
      call imemry('inc','nblok',n)
c
c  Advance the file to the next record.
c
      call imemry('set','nwrec',nwrec)

      return
      end
      subroutine wrbuf(x,iflag)

c*********************************************************************72
c
cc WRBUF "writes" a real value X to the buffer.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  X      Input, REAL X, usually a value to be written out.
c
c  IFLAG  Input, integer IFLAG, communication flag.
c
c         If IFLAG is -1, then X is ignored.  This subroutine 
c         and BUFFLS are initialized.
c
c         If IFLAG is 0, then X is added to the buffer, and,
c         if necessary, the buffer is flushed.
c
c         If IFLAG is 1, then, if neccessary, the buffer is 
c         flushed.
c
      integer mwordw
      parameter (mwordw=20)

      real buff(mwordw+1)
      integer i
      integer iflag
      integer jflag
      integer nbuff
      integer nwordw
      real x

      save buff
      save nbuff
      save nwordw

      data nbuff  /0/
      data nwordw /20/
c
c  Get the number of words per record.
c
      call imemry('get','nwordw',nwordw)
c
c  Initialization
c
      if(iflag.eq.-1)then

        nbuff=0

        do i=1,mwordw
          buff(i)=0
        enddo

        call buffls ( buff, nwordw, iflag )
c
c  Add X to buffer, flush buffer if necessary.
c
      elseif(iflag.eq.0)then

        nbuff=nbuff+1
        buff(nbuff)=x

        if(nbuff.ge.nwordw)then
          call buffls ( buff, nwordw, iflag )
          nbuff=nbuff-nwordw
        endif
c
c  Flush buffer, if necessary.
c
      elseif(iflag.eq.1)then

        if(nbuff.gt.0)then
          jflag=0
          call buffls ( buff, nwordw, jflag )
          nbuff=0
        endif

        call buffls ( buff, nwordw, iflag )

      endif

      return
      end
      subroutine wrch(chval)

c*********************************************************************72
c
cc WRCH writes a single character to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHVAL  Input, CHARACTER*1 CHVAL, the character to be written.
c
      integer iflag
      parameter (iflag=0)

      character*1 chval
      integer n
      real x

      intrinsic float
      intrinsic ichar

      x=float(ichar(chval))

      call wrbuf(x,iflag)

      n=1
      call imemry('inc','nwch',n)

      return
      end
      subroutine wrchv(chvec,n)

c*********************************************************************72
c
cc WRCHV writes a string of characters to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHVEC  Input, CHARACTER*(N) CHVEC, the string of characters to
c         be written.
c
c  N      Input, integer N, the number of characters to write.
c
      integer iflag
      parameter (iflag=0)

      character*(*) chvec
      integer i
      integer n
      real x

      intrinsic float
      intrinsic ichar

      do i=1,n
        x=float(ichar(chvec(i:i)))
        call wrbuf(x,iflag)
      enddo

      call imemry('inc','nwch',n)

      return
      end
      subroutine wrcm(cval)

c*********************************************************************72
c
cc WRCM writes a complex value to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CVAL   Input, COMPLEX CVAL, the complex value to write.
c
      integer iflag
      parameter (iflag=0)

      complex cval
      integer n
      real x

      intrinsic aimag
      intrinsic real

      x=real(cval)
      call wrbuf(x,iflag)
      x=aimag(cval)
      call wrbuf(x,iflag)

      n=1
      call imemry('inc','nwcm',n)

      return
      end
      subroutine wrcmv(cvec,n)

c*********************************************************************72
c
cc WRCMV writes a vector of complex values to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CVEC   Input, COMPLEX CVEC(N), the complex values to write.
c
c  N      Input, integer N, the number of complex values to write.
c
      integer iflag
      parameter (iflag=0)

      integer n

      complex cvec(n)
      integer i
      real x

      intrinsic aimag
      intrinsic real

      do i=1,n
        x=real(cvec(i))
        call wrbuf(x,iflag)
        x=aimag(cvec(i))
        call wrbuf(x,iflag)
      enddo

      call imemry('inc','nwcm',n)

      return
      end
      subroutine wrdb(dval)

c*********************************************************************72
c
cc WRDB writes a double precision value to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVAL   Input, DOUBLE PRECISION DVAL, the double precision value
c         to write.
c
      integer iflag
      parameter (iflag=0)

      double precision dval
      integer n
      real x

      intrinsic dble
      intrinsic sngl

      x=sngl(dval)
      call wrbuf(x,iflag)
      x=sngl(dval-dble(x))
      call wrbuf(x,iflag)

      n=1
      call imemry('inc','nwdb',n)

      return
      end
      subroutine wrdbv(dvec,n)

c*********************************************************************72
c
cc WRDBV writes a vector of double precision values to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVEC   Input, DOUBLE PRECISION DVEC(N), the double precision 
c         values to write.
c
c  N      Input, integer N, the number of values to write.
c
      integer iflag
      parameter (iflag=0)

      integer n

      double precision dvec(n)
      integer i
      real x

      intrinsic dble
      intrinsic sngl

      do i=1,n
        x=sngl(dvec(i))
        call wrbuf(x,iflag)
        x=sngl(dvec(i)-dble(x))
        call wrbuf(x,iflag)
      enddo

      call imemry('inc','nwdb',n)

      return
      end
      subroutine wrdr(dval)

c*********************************************************************72
c
cc WRDR writes a real copy of a double precision value to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVAL   Input, DOUBLE PRECISION DVAL, the double precision value,
c         whose real value is to be written to a file.
c
      integer iflag
      parameter (iflag=0)

      double precision dval
      integer n
      real x

      intrinsic dble
      intrinsic sngl

      x=sngl(dval)
      call wrbuf(x,iflag)

      n=1
      call imemry('inc','nwdr',n)

      return
      end
      subroutine wrdrv(dvec,n)

c*********************************************************************72
c
cc WRDRV writes real copies of a vector of double precision values 
c  to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  DVEC   Input, DOUBLE PRECISION DVEC(N), the vector of double
c         precision values.
c
c  N      Input, integer N, the number of values to write.
c
      integer iflag
      parameter (iflag=0)

      integer n

      double precision dvec(n)
      integer i
      real x

      intrinsic dble
      intrinsic sngl

      do i=1,n
        x=sngl(dvec(i))
        call wrbuf(x,iflag)
      enddo

      call imemry('inc','nwdr',n)

      return
      end
      subroutine wrfrst(filnam,ierror)

c*********************************************************************72
c
cc WRFRST prepares the BUFPAK routines WRBUF and BUFFLS for writing
c  to a file.  
c
c  WRFRST should be called before any of the other BUFPAK writing
c  routines.  If the current file is closed, and a new file is
c  opened for writing, then WRFRST should be called again to 
c  initialize data for the new file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  FILNAM Input, CHARACTER*(*), the name of the file to which the
c         data should be written.
c
c  IERROR Output, integer IERROR, error flag.
c
c         0, no error.
c         1, the file could not be opened.
c
      integer iflag
      parameter (iflag=-1)

      real x
      parameter (x=0.0)

      character*(*) filnam
      integer ierror
      integer iwform
      integer iwunit
      integer lenrec
      integer nffac
      integer nufac
      integer nwordw

      save iwform
      save iwunit
      save nffac
      save nufac
      save nwordw

      data iwform /0/
      data iwunit /1/
      data nffac  /14/
      data nufac  /1/
      data nwordw /20/
c
c  Get the unit number.
c
      call imemry('get','iwunit',iwunit)
c
c  Get the record length.
c
      call imemry('get','nwordw',nwordw)
c
c  Get the file format.
c
      call imemry('get','iwform',iwform)
c
c  Open the file.
c
      if(iwform.eq.0)then

        call imemry('get','nufac',nufac)

        lenrec=nufac*nwordw

        open(unit=iwunit,file=filnam,form='unformatted',
     &    access='direct',recl=lenrec,err=10,status='unknown')

      else

        call imemry('get','nffac',nffac)

        lenrec=nffac*nwordw

        open(unit=iwunit,file=filnam,form='formatted',
     &    access='direct',recl=lenrec,err=10,status='unknown')

      endif

      ierror=0

      call wrbuf(x,iflag)

      return

10    continue
      write(*,*)'WRFRST - Serious error!'
      write(*,*)'The file could not be opened!'
      ierror=1
      return

      end
      subroutine wrin(ival)

c*********************************************************************72
c
cc WRIN writes an integer to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IVAL   Input, integer IVAL, the integer to be written.
c
      integer iflag
      parameter (iflag=0)

      integer ival
      integer n
      real x

      x=float(ival)
      call wrbuf(x,iflag)

      n=1
      call imemry('inc','nwin',n)

      return
      end
      subroutine wrinv(ivec,n)

c*********************************************************************72
c
cc WRINV writes a vector of integers to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IVEC   Input, integer IVEC(N), the vector of integers.
c
c  N      Input, integer N, the number of values to write to the 
c         file.
c
      integer iflag
      parameter (iflag=0)

      integer n

      integer i
      integer ivec(n)
      real x

      do i=1,n
        x=float(ivec(i))
        call wrbuf(x,iflag)
      enddo

      call imemry('inc','nwin',n)

      return
      end
      subroutine wrlast

c*********************************************************************72
c
cc WRLAST terminates writing to the file, and closes it.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      integer iflag
      integer iwunit
      real x

      save iwunit

      data iwunit /1/
c
c  Tell WRBUF to flush the buffer.  
c
      x=0
      iflag=1

      call wrbuf(x,iflag)
c
c  Write one final block marker.
c
      call wrblok
c
c  Get the unit number, and close the file.
c
      call imemry('get','iwunit',iwunit)

      close(unit=iwunit)

      return
      end
      subroutine wrlg(lval)

c*********************************************************************72
c
cc WRLG writes a logical value to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  LVAL   Input, LOGICAL LVAL, the logical value to write.
c
      integer iflag
      parameter (iflag=0)

      logical lval
      integer n
      real x

      if(lval)then
        x=1.0
      else
        x=0.0
      endif

      call wrbuf(x,iflag)

      n=1
      call imemry('inc','nwlg',n)

      return
      end
      subroutine wrlgv(lvec,n)

c*********************************************************************72
c
cc WRLGV writes a vector of logical values to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  LVEC   Input, LOGICAL LVEC(N), the vector of logical values.
c
c  N      Input, integer N, the number of values to write.
c
      integer iflag
      parameter (iflag=0)

      integer n

      integer i
      logical lvec(n)
      real x

      do i=1,n

        if(lvec(i))then
          x=1.0
        else
          x=0.0
        endif

        call wrbuf(x,iflag)
      enddo

      call imemry('inc','nwlg',n)

      return
      end
      subroutine wrrl(rval)

c*********************************************************************72
c
cc WRRL writes a real value to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  RVAL   Input, REAL RVAL, the real value to be written.
c
      integer iflag
      parameter (iflag=0)

      integer n
      real rval

      call wrbuf(rval,iflag)

      n=1
      call imemry('inc','nwrl',n)

      return
      end
      subroutine wrrlv(rvec,n)

c*********************************************************************72
c
cc WRRLV writes a vector of real values to a file.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  RVEC   Input, REAL RVEC(N), the vector of real values.
c
c  N      Input, integer N, the number of values to write.
c
      integer iflag
      parameter (iflag=0)

      integer n

      integer i
      real rvec(n)

      do i=1,n
        call wrbuf(rvec(i),iflag)
      enddo

      call imemry('inc','nwrl',n)

      return
      end
