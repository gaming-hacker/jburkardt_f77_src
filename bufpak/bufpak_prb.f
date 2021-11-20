c  BUFPRB.F  19 February 1994
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for BUFPAK_PRB.
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BUFPAK_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Tests for BUFPAK, using buffered I/O.'
      write ( *, '(a)' ) ' '

      call test01
      call test02
      call test03

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BUFPAK_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
      integer maxvec
      parameter (maxvec=25)

      character*(maxvec) chvec
      complex cvec(maxvec)
      double precision drvec(maxvec)
      double precision dvec(maxvec)
      integer i
      integer ierror
      integer irform
      integer irunit
      integer ivec(maxvec)
      integer iwform
      integer iwunit
      integer j
      logical lvec(maxvec)
      integer mrrec
      integer mrval
      integer mrwrd
      integer nrch
      integer nrcm
      integer nrdb
      integer nrdr
      integer nrin
      integer nrlg
      integer nrrec
      integer nrrl
      integer nrwrd
      integer nrval
      integer nufac
      integer nwch
      integer nwcm
      integer nwdb
      integer nwdr
      integer nwin
      integer nwlg
      integer nwrec
      integer nwrl
      integer nwwrd
      integer nwval
      real rvec(maxvec)

      intrinsic char
      intrinsic cmplx
      intrinsic dble
      intrinsic mod
      intrinsic sqrt

      write(*,*)' '
      write(*,*)' '
      write(*,*)'TEST01'
      write(*,*)' '
      write(*,*)'Test read and write of various types of data.'
      write(*,*)'Both the READ and WRITE files are unformatted.'
      write(*,*)' '
c
c  Set up the data vectors.
c
      do i = 1, maxvec
        j = mod ( i-1, 26 )
        chvec(i:i) = char ( 65 + j )
        cvec(i) = cmplx ( i, - i )
        drvec(i) = sqrt ( dble ( i ) )
        dvec(i) = sqrt ( dble ( i ) )
        ivec(i) = i**2
        lvec(i) = mod ( i, 4 ) .eq. 0
        rvec(i) = sqrt ( real ( i ) )
      enddo
c
c  Set the unit number.
c
      iwunit = 11
      call imemry('set','iwunit',iwunit)
c
c  Specify that the file is unformatted.
c
      iwform=0
      call imemry('set','iwform',iwform)
c
c  Specify that on this computer, an unformatted record has
c  length equal to the number of words.
c
c  On a Cray, set NUFAC to 8.
c  On the Mac, set NUFAC to 4.
c
      nufac = 4
      call imemry('set','nufac',nufac)
c
c  Delete the file.
c
      call file_delete ( 'bufprb01.dat' )
c
c  Write the data to the file.
c
      call wrfrst('bufprb01.dat',ierror)

      if(ierror.ne.0)then
        write(*,*)'TEST01 - Fatal error!'
        write(*,*)'WRFRST reports a problem.'
        stop
      endif

      call wrchv(chvec,maxvec)
      call wrcmv(cvec,maxvec)
      call wrdrv(drvec,maxvec)
      call wrdbv(dvec,maxvec)
      call wrinv(ivec,maxvec)
      call wrlgv(lvec,maxvec)
      call wrrlv(rvec,maxvec)

      call wrlast
c
c  Report on the values we wrote.
c
      write(*,*)' '
      write(*,*)'Here are the values we wrote:'
      write(*,*)' '
      write(*,*)'CHVEC(1:25)='//chvec(1:25)
      write(*,*)'CVEC(1)=    ',cvec(1)
      write(*,*)'DRVEC(1)=   ',drvec(1)
      write(*,*)'DVEC(1)=    ',dvec(1)
      write(*,*)'IVEC(1)=    ',ivec(1)
      write(*,*)'LVEC(1)=    ',lvec(1)
      write(*,*)'RVEC(1)=    ',rvec(1)

      call imemry('get','nwch',nwch)
      call imemry('get','nwcm',nwcm)
      call imemry('get','nwdb',nwdb)
      call imemry('get','nwdr',nwdr)
      call imemry('get','nwin',nwin)
      call imemry('get','nwlg',nwlg)
      call imemry('get','nwrl',nwrl)

      call imemry('get','nwrec',nwrec)
      call imemry('get','nwval',nwval) 
      call imemry('get','nwwrd',nwwrd)
 
      write(*,*)' '
      write(*,*)'Wrote:'
      write(*,*)' '
      write(*,*)nwch,' characters.'
      write(*,*)nwcm,' complex values.'
      write(*,*)nwdb,' double precision values.'
      write(*,*)nwdr,' double precision values as reals.'
      write(*,*)nwin,' integers.'
      write(*,*)nwlg,' logicals.'
      write(*,*)nwrl,' reals.'
      write(*,*)' '
      write(*,*)nwrec,' records.'
      write(*,*)nwval,' values.'
      write(*,*)nwwrd,' words total.'
c
c  Clear out the data.
c
      do i=1,maxvec
        chvec(i:i)=' '
        cvec(i)=cmplx(0.0,0.0)
        drvec(i)=0
        dvec(i)=0
        ivec(i)=0
        lvec(i)=.false.
        rvec(i)=0
      enddo
c
c  Now read the information back in.
c
      write(*,*)' '
      write(*,*)'Now read the data back in!'
      write(*,*)' '
c
c  Set the unit number.
c
      irunit=12
      call imemry('set','irunit',irunit)
c
c  Specify that the file is unformatted.
c
      irform=0
      call imemry('set','irform',irform)

      call rdfrst('bufprb01.dat',ierror)

      if(ierror.ne.0)then
        write(*,*)'TEST01 - Fatal error!'
        write(*,*)'RDFRST reports a problem.'
        stop
      endif

      call imemry('get','mrrec',mrrec)
      call imemry('get','mrval',mrval)
      call imemry('get','mrwrd',mrwrd)
      write(*,*)' '
      write(*,*)'Number of records written reported as ',mrrec
      write(*,*)'Number of values written reported as  ',mrval
      write(*,*)'Number of words written reported as   ',mrwrd

      call rdchv(chvec,maxvec)
      call rdcmv(cvec,maxvec)
      call rddrv(drvec,maxvec)
      call rddbv(dvec,maxvec)
      call rdinv(ivec,maxvec)
      call rdlgv(lvec,maxvec)
      call rdrlv(rvec,maxvec)

      call rdlast
c
c  Report on the values we have read in.
c
      write(*,*)' '
      write(*,*)'Here are the values we read:'
      write(*,*)' '
      write(*,*)'CHVEC(1:25)='//chvec(1:25)
      write(*,*)'CVEC(1)=    ',cvec(1)
      write(*,*)'DRVEC(1)=   ',drvec(1)
      write(*,*)'DVEC(1)=    ',dvec(1)
      write(*,*)'IVEC(1)=    ',ivec(1)
      write(*,*)'LVEC(1)=    ',lvec(1)
      write(*,*)'RVEC(1)=    ',rvec(1)

      call imemry('get','nrch',nrch)
      call imemry('get','nrcm',nrcm)
      call imemry('get','nrdb',nrdb)
      call imemry('get','nrdr',nrdr)
      call imemry('get','nrin',nrin)
      call imemry('get','nrlg',nrlg)
      call imemry('get','nrrl',nrrl)

      call imemry('get','nrrec',nrrec)
      call imemry('get','nrval',nrval) 
      call imemry('get','nrwrd',nrwrd)

      write(*,*)' '
      write(*,*)'Read:'
      write(*,*)' '
      write(*,*)nrch,' characters.'
      write(*,*)nrcm,' complex values.'
      write(*,*)nrdb,' double precision values.'
      write(*,*)nrdr,' double precision values as reals.'
      write(*,*)nrin,' integers.'
      write(*,*)nrlg,' logicals.'
      write(*,*)nrrl,' reals.'
      write(*,*)' '
      write(*,*)nrrec,' records.'
      write(*,*)nrval,' values.'
      write(*,*)nrwrd,' words total.'

      return
      end
      subroutine test02
c
c*********************************************************************72
c
cc TEST02
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
      integer maxvec
      parameter (maxvec=25)
c
      character*(maxvec) chvec
      complex cvec(maxvec)
      double precision drvec(maxvec)
      double precision dvec(maxvec)
      integer i
      integer ierror
      integer irform
      integer irunit
      integer ivec(maxvec)
      integer iwform
      integer iwunit
      integer j
      logical lvec(maxvec)
      integer mrrec
      integer mrval
      integer mrwrd
      integer nffac
      integer nrch
      integer nrcm
      integer nrdb
      integer nrdr
      integer nrin
      integer nrlg
      integer nrrec
      integer nrrl
      integer nrwrd
      integer nrval
      integer nwch
      integer nwcm
      integer nwdb
      integer nwdr
      integer nwin
      integer nwlg
      integer nwordr
      integer nwordw
      integer nwrec
      integer nwrl
      integer nwwrd
      integer nwval
      character*80 rformt
      real rvec(maxvec)
      character*80 wformt

      intrinsic char
      intrinsic cmplx
      intrinsic dble
      intrinsic mod
      intrinsic sqrt

      write(*,*)' '
      write(*,*)' '
      write(*,*)'TEST02'
      write(*,*)' '
      write(*,*)'Test read and write of various types of data.'
      write(*,*)'Both the READ and WRITE files are formatted.'
      write(*,*)' '
c
c  Set the data values.
c
      do i=1,maxvec
        j=mod(i-1,26)
        chvec(i:i)=char(65+j)
        cvec(i)=cmplx(i,-i)
        drvec(i)=sqrt(dble(i))
        dvec(i)=sqrt(dble(i))
        ivec(i)=i**2
        lvec(i)=mod(i,4).eq.0
        rvec(i)=sqrt(real(i))
      enddo
c
c  Set the unit number.
c
      iwunit=13
      call imemry('set','iwunit',iwunit)
c
c  Specify that the file is formatted.
c
      iwform=1
      call imemry('set','iwform',iwform)
c
c  Specify that each word takes up 14 bytes.
c
      nffac=14
      call imemry('set','nffac',nffac)
c
c  Specify that there will be 10 words per record.
c
      nwordw=10
      call imemry('set','nwordw',nwordw)
c
c  Specify the format to use for writing.
c
      wformt='(10g14.6)'
      call smemry('set','wformt',wformt)
c
c  Delete the file.
c
      call file_delete ( 'bufprb02.dat' )
c
c  Write the data to the file.
c
      call wrfrst('bufprb02.dat',ierror)

      if(ierror.ne.0)then
        write(*,*)'TEST02 - Fatal error!'
        write(*,*)'WRFRST reports a problem.'
        stop
      endif

      call wrchv(chvec,maxvec)
      call wrcmv(cvec,maxvec)
      call wrdrv(drvec,maxvec)
      call wrdbv(dvec,maxvec)
      call wrinv(ivec,maxvec)
      call wrlgv(lvec,maxvec)
      call wrrlv(rvec,maxvec)

      call wrlast
c
c  Report on the values we wrote.
c
      write(*,*)' '
      write(*,*)'Here are the values we wrote:'
      write(*,*)' '
      write(*,*)'CHVEC(1:25)='//chvec(1:25)
      write(*,*)'CVEC(1)=    ',cvec(1)
      write(*,*)'DRVEC(1)=   ',drvec(1)
      write(*,*)'DVEC(1)=    ',dvec(1)
      write(*,*)'IVEC(1)=    ',ivec(1)
      write(*,*)'LVEC(1)=    ',lvec(1)
      write(*,*)'RVEC(1)=    ',rvec(1)
c
      call imemry('get','nwch',nwch)
      call imemry('get','nwcm',nwcm)
      call imemry('get','nwdb',nwdb)
      call imemry('get','nwdr',nwdr)
      call imemry('get','nwin',nwin)
      call imemry('get','nwlg',nwlg)
      call imemry('get','nwrl',nwrl)

      call imemry('get','nwrec',nwrec)
      call imemry('get','nwval',nwval) 
      call imemry('get','nwwrd',nwwrd)
 
      write(*,*)' '
      write(*,*)'Wrote:'
      write(*,*)' '
      write(*,*)nwch,' characters.'
      write(*,*)nwcm,' complex values.'
      write(*,*)nwdb,' double precision values.'
      write(*,*)nwdr,' double precision values as reals.'
      write(*,*)nwin,' integers.'
      write(*,*)nwlg,' logicals.'
      write(*,*)nwrl,' reals.'
      write(*,*)' '
      write(*,*)nwrec,' records.'
      write(*,*)nwval,' values.'
      write(*,*)nwwrd,' words total.'
c
c  Clear out the data.
c
      do i=1,maxvec
        chvec(i:i)=' '
        cvec(i)=cmplx(0.0,0.0)
        drvec(i)=0
        dvec(i)=0
        ivec(i)=0
        lvec(i)=.false.
        rvec(i)=0
      enddo
c
c  Now read the information back in.
c
      write(*,*)' '
      write(*,*)'Now read the data back in!'
      write(*,*)' '
c
c  Set the unit number.
c
      irunit=14
      call imemry('set','irunit',irunit)
c
c  Specify that the file is formatted.
c
      irform=1
      call imemry('set','irform',irform)
c
c  Specify that there will be 10 words per record.
c
      nwordr=10
      call imemry('set','nwordr',nwordr)
c
c  Specify the format to use for reading.
c
      rformt='(10g14.6)'
      call smemry('set','rformt',rformt)

      call rdfrst('bufprb02.dat',ierror)

      if(ierror.ne.0)then
        write(*,*)'TEST02 - Fatal error!'
        write(*,*)'RDFRST reports a problem.'
        stop
      endif

      call imemry('get','mrrec',mrrec)
      call imemry('get','mrval',mrval)
      call imemry('get','mrwrd',mrwrd)
      write(*,*)' '
      write(*,*)'Number of records written reported as ',mrrec
      write(*,*)'Number of values written reported as  ',mrval
      write(*,*)'Number of words written reported as   ',mrwrd

      call rdchv(chvec,maxvec)
      call rdcmv(cvec,maxvec)
      call rddrv(drvec,maxvec)
      call rddbv(dvec,maxvec)
      call rdinv(ivec,maxvec)
      call rdlgv(lvec,maxvec)
      call rdrlv(rvec,maxvec)

      call rdlast
c
c  Report on the values we have read in.
c
      write(*,*)' '
      write(*,*)'Here are the values we read:'
      write(*,*)' '
      write(*,*)'CHVEC(1:25)='//chvec(1:25)
      write(*,*)'CVEC(1)=    ',cvec(1)
      write(*,*)'DRVEC(1)=   ',drvec(1)
      write(*,*)'DVEC(1)=    ',dvec(1)
      write(*,*)'IVEC(1)=    ',ivec(1)
      write(*,*)'LVEC(1)=    ',lvec(1)
      write(*,*)'RVEC(1)=    ',rvec(1)

      call imemry('get','nrch',nrch)
      call imemry('get','nrcm',nrcm)
      call imemry('get','nrdb',nrdb)
      call imemry('get','nrdr',nrdr)
      call imemry('get','nrin',nrin)
      call imemry('get','nrlg',nrlg)
      call imemry('get','nrrl',nrrl)

      call imemry('get','nrrec',nrrec)
      call imemry('get','nrval',nrval) 
      call imemry('get','nrwrd',nrwrd)

      write(*,*)' '
      write(*,*)'Read:'
      write(*,*)' '
      write(*,*)nrch,' characters.'
      write(*,*)nrcm,' complex values.'
      write(*,*)nrdb,' double precision values.'
      write(*,*)nrdr,' double precision values as reals.'
      write(*,*)nrin,' integers.'
      write(*,*)nrlg,' logicals.'
      write(*,*)nrrl,' reals.'
      write(*,*)' '
      write(*,*)nrrec,' records.'
      write(*,*)nrval,' values.'
      write(*,*)nrwrd,' words total.'

      return
      end
      subroutine test03

c*********************************************************************72
c
cc TEST03
c
c  Modified:
c
c    18 August 2008
c
c  Author:
c
c    John Burkardt
c
      integer nblok
      parameter (nblok=4)

      integer nsize
      parameter (nsize=250)

      integer maxdat
      parameter (maxdat=nblok*nsize)

      integer i
      integer iblok
      integer ierror
      integer irform
      integer iwform
      integer iwunit
      integer j
      integer nrrec
      integer nrrl
      integer nrval
      integer nrwrd
      integer nufac
      integer nwrec
      integer nwrl
      integer nwval
      integer nwwrd
      real x(maxdat)

      write(*,*)' '
      write(*,*)'Test03'
      write(*,*)' '
      write(*,*)'Test the use of blocks to separate data into'
      write(*,*)'addressable groups.'
      write(*,*)' '
c
c  Set the data we will use.
c
      do i=1,maxdat
        x(i)=i
      enddo
c
c  Set the unit number.
c
      iwunit=15
      call imemry('set','iwunit',iwunit)
c
c  Specify unformatted files.
c
      iwform=0
      call imemry('set','iwform',iwform)
c
c  Specify that on this computer, an unformatted record has
c  length equal to the number of words.
c
c  On a Cray, set NUFAC to 8!
c  On the Mac, set NUFAC to 4.
c
      nufac=4
      call imemry('set','nufac',nufac)
c
c  Delete the file.
c
      call file_delete ( 'bufprb03.dat' )
c
c  Write the data to the file.
c
      call wrfrst('bufprb03.dat',ierror)

      if(ierror.ne.0)then
        write(*,*)'TEST03 - Fatal error!'
        write(*,*)'WRFRST reports a problem.'
        stop
      endif

      write(*,*)' '
      write(*,*)'Write the data to the file in ',nblok,' blocks'
      write(*,*)'of size ',nsize
      write(*,*)' '

      do iblok=1,nblok
        
        j=1+(iblok-1)*nsize
        call wrrlv(x(j),nsize)

        write(*,*)'Block ',iblok,' starts with X=',x(j),x(j+1)

        call wrblok
      enddo

      call wrlast

      call imemry('get','nwrl',nwrl)

      call imemry('get','nwrec',nwrec)
      call imemry('get','nwval',nwval) 
      call imemry('get','nwwrd',nwwrd)
 
      write(*,*)' '
      write(*,*)'Wrote:'
      write(*,*)' '
      write(*,*)nwrl,' reals.'
      write(*,*)' '
      write(*,*)nwrec,' records.'
      write(*,*)nwval,' values.'
      write(*,*)nwwrd,' words.'
c
c  Read the data from the file, but in reverse order!
c
      write(*,*)' '
      write(*,*)' '
      write(*,*)'Now read the data back in, but in reverse'
      write(*,*)'order.  This should be easy, using blocks.'
      write(*,*)' '
c
c  Set the unit number.
c
      irunit=16
      call imemry('set','irunit',irunit)
c
c  Specify unformatted files.
c
      irform=0
      call imemry('set','irform',irform)
c
      call rdfrst('bufprb03.dat',ierror)

      if(ierror.ne.0)then
        write(*,*)'TEST03 - Fatal error!'
        write(*,*)'RDFRST reports a problem.'
        stop
      endif

      do iblok=nblok,1,-1
        
        call rdblok(iblok)
        j=1+(iblok-1)*nsize
        call rdrlv(x(j),nsize)

        write(*,*)'Block ',iblok,' starts with X=',x(j),x(j+1)

      enddo

      call rdlast
c
      call imemry('get','nrrl',nrrl)

      call imemry('get','nrrec',nrrec)
      call imemry('get','nrval',nrval) 
      call imemry('get','nrwrd',nrwrd)
 
      write(*,*)' '
      write(*,*)'Read:'
      write(*,*)' '
      write(*,*)nrrl,' reals.'
      write(*,*)' '
      write(*,*)nrrec,' records.'
      write(*,*)nrval,' values.'
      write(*,*)nrwrd,' words total.'
      write(*,*)nrwrd,' words total.'

      return
      end
