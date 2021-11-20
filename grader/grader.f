c  grader.f  13 December 1996
c
      program main

c*********************************************************************72
c
cc GRADER is a program for maintaining class records.
c
c  Version 1.13
c
c    I am planning on dropping the computation of standard deviations,
c    since I have no interest in it at all.
c
c    I had to add WEIGHT back in, because I was giving "extra credit"
c    points that I don't want to count.  This affected routines
c      GRACOM, GRAPRN, INIT, PRINT, TEST, WRITER.
c
c    I added a "LABELS" command to print out the labels.
c
c    I changed the MODIFY command to work by test label rather than
c    test number.
c
c    Added the DELETE command.
c
c    I added the option to label each graded item, as a test,
c    homework, or other thing.
c
c    I will now read and write the grades as characters.
c
c    I now force the Possible, Total, and Average columns to the end.
c
c  Version 1.12
c
c  Version 1.11
c
c    Improved working of ROSTER routine, allowing for variable
c    number of columns of boxes.
c
c    Put program in lower case.
c
c    Used "&" for continuation.
c
c    Updated copies of CAPCHR, CHRCHP, CHRCTI, CHRCTR, CHRDB1,
c      CHRDB2, CHRINP, CHRINT, CHRREA, CHRWRT, HLPVMS, INTREA, 
c      LENCHR, LEQI, RELREA.
c
c    Standardized declarations.
c
c    Added options "$", "%", "<", "#".
c
c    Put all unit numbers under the supervision of NUMFIL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Character*9 CHDATE.
c    CHDATE contains today's date, as read from the computer.
c
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c    Real CLNUMD(MXTEST).
c    CLNUMD is the class numeric median.  CLNUMD(I) is the median 
c    grade for all students in the class with a valid grade for 
c    test I.
c
c    Real CLNUMN(MXTEST).
c    CLNUMN is the class numeric mean.  CLNUMN(I) is the mean 
c    grade for all students in the class with a valid grade for 
c    test I.
c
c    Class numeric standard deviation.  CLNUST(I) is the standard
c    deviation from the mean, for all students in the class with
c    a valid grade for test I.
c
c    Character*80 FILHLP.
c    FILHLP is the name of the help file, which is probably called
c    'grader.hlp'.  
c
c    Character*80 FILNEW.
c    FILNEW is the name of the file into which the new data is
c    stored after each command that changes grade information.
c    This file is usually called 'grader.new'.
c
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c    Integer ISTAT.
c    0, no grade file has been specified.
c    1, a grade file has been specified.
c    2, a grade file has been specified and opened.
c
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c    Two special columns are labeled '   Total' and ' Average'.
c
c    Integer LPAGE.
c    LPAGE is the number of lines per page.  The program pauses after
c    every LPAGE lines of output, requesting the user to hit RETURN.
c
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c    Integer NOSTCL(MXTEST).
c    NOSTCL(I) is the number of students with valid scores for 
c    test I.
c
c    Integer NOSTTR.
c    NOSTTR is the number of students with valid scores for term.
c
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c    However, if SCORES(I,J) is negative, then the score is not
c    to be counted.
c
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
c    Real STNUMN(MXSTUD).
c    For each student, STNUMN(I) is the student's numeric mean,
c    or averaged grade for the class.
c
c    Real TRNUMD 
c    Term numeric median
c
c    Real TRNUMN.
c    Term numeric mean
c
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      parameter (mxstud=50)
c
      integer mxtest
      parameter (mxtest=51)
c
      character*9 chdate
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      real clnumd(mxtest)
      real clnumn(mxtest)
      real clnust(mxtest)
      character*80 filhlp
      character*60 filinp
      character*60 fillpt
      character*60 filnam
      character*60 filnew
      character*60 filold
      character*60 filsav
      integer i
      integer ierror
      character*80 isay
      integer isect(mxstud)
      integer issn
      integer istat
      character*8 labtst(mxtest)
      logical leqi
      integer list(mxstud)
      character*20 names(mxstud)
      character*20 namtmp
      integer nline
      integer nostcl(mxtest)
      integer nosttr
      integer nsect
      integer nstud
      integer ntest
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      real stnumn(mxstud)
      real stunk(mxstud)
      real weight(mxtest)
c
      external leqi
c
c  Initialize the variables.
c
      call init(chscor,classn,filhlp,filinp,fillpt,filnam,filnew,
     &  filold,filsav,ierror,isect,issn,istat,labtst,
     &  mxstud,mxtest,names,nline,nostcl,nsect,nstud,ntest,scomax,
     &  scores,ssn,stnumn,stunk,weight)
c
c  Say hello.
c
      call hello(mxstud,mxtest)
c
c  Get and print today's date.
c
      chdate='Oops'
c     call getdmy(chdate)
      write(*,*)'Today is '//chdate

10    continue
      
      write(*,*)' '

      write(*,*)'Enter command, ("H" for menu, "?" for help)'
      
30    continue

      read(*,'(a)')isay
 
      if(isay.eq.'#')then
        isay=' '
        go to 30
      endif
 
      if(isay.eq.' ')then
        go to 10
      endif
 
      if(ierror.eq.2)then
        write(*,*)' '
        write(*,*)'GRADER - Oops!'
        stop
      endif

      if(ierror.ne.0)then
        go to 10
      endif
c
c  A: Anonymous test result listing
c
      if(leqi(isay,'a'))then

        call anonym(classn,isect,issn,istat,labtst,
     &    mxstud,mxtest,nline,nsect,nstud,ntest,scores,ssn)
c
c  ADD: New names added to file
c
      elseif(leqi(isay,'add'))then

        if(istat.le.0)then

          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          write(*,*)' '
          write(*,*)'Grader - Add new students to the roster.'

          call addnam(isect,issn,labtst,mxstud,mxtest,
     &      names,nsect,nstud,ntest,scores,ssn)

          if(ierror.ne.0)go to 50

          call rename(filold,filnew,ierror)

          if(ierror.ne.0)go to 50

        endif
c
c  C: Create a grade file
c
      elseif(leqi(isay,'c'))then

        if(istat.gt.1)then
          write(*,*)'Enter name of file to create'
          read(*,'(a)')filnew
        endif

        call create(classn,isect,issn,istat,
     &    mxstud,names,nsect,nstud,ntest,ssn)

        if(ierror.ne.0)go to 50
c
c  DATE: print out the date
c
      elseif(leqi(isay,'date'))then

        write(*,*)' '
        write(*,*)'Date:'
        write(*,*)'  '//chdate
c
c  DELETE name: delete a student.
c  (maybe later want to allow "DELETE TEST" as well)
c
      elseif(leqi(isay(1:6),'delete'))then

        write(*,*)' '
        write(*,*)'Grader - Delete students from the roster.'

        if(istat.le.1)then

          write(*,*)' '
          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          namtmp=isay(7:)
          call flushl(namtmp)
 
          call delete(isect,mxstud,mxtest,names,
     &      namtmp,nstud,scores,ssn)

          call rename(filold,filnew,ierror)

          if(ierror.ne.0)go to 50

        endif
c
c  F: Choose a new grade file
c
      elseif(leqi(isay,'f'))then

        call deffil(chscor,classn,filnew,filsav,ierror,
     &    isect,issn,istat,labtst,mxstud,mxtest,names,nsect,
     &    nstud,ntest,scomax,scores,ssn,weight)

        if(ierror.ne.0)go to 50
c
c  G: Grade file
c
      elseif(leqi(isay,'g'))then

        if(istat.le.1)then

          write(*,*)' '
          write(*,*)'GRADER - Error!'  
          write(*,*)'  Please define a file first with the "F" command!'

        else

          call gracom(chscor,clnumd,clnumn,clnust,labtst,list,
     &      mxstud,mxtest,nostcl,nosttr,nstud,ntest,scomax,scores,
     &      stnumn,stunk,weight)


          if(ierror.ne.0)go to 50

          call graprn(chscor,classn,clnumd,clnumn,clnust,labtst,
     &      mxstud,mxtest,names,nostcl,nstud,ntest,scomax,weight)

          if(ierror.ne.0)go to 50

        endif
c
c  H: Help
c
      elseif(leqi(isay,'h'))then

        call help(filhlp,filnew)
c
c  LABELS: Print out the labels.
c
      elseif(leqi(isay(1:3),'lab'))then

        write(*,*)' '
        write(*,*)'GRADER - Print out grade labels:'
        write(*,*)' '
        write(*,'(9a8)')(labtst(i),i=1,ntest)
c
c  M: Modify information in file
c
      elseif(leqi(isay,'m'))then

        if(istat.le.1)then

          write(*,*)' '
          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          call modify(ierror,isect,issn,labtst,mxstud,mxtest,names,
     &      nsect,nstud,ntest,scores,ssn)
 
          if(ierror.ne.0)go to 50

          call rename(filold,filnew,ierror)
          if(ierror.ne.0)go to 50

        endif
c
c  P: Print the file
c
      elseif(leqi(isay,'p'))then

        if(istat.le.0)then

          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          call print(chscor,classn,labtst,
     &      mxstud,mxtest,names,nstud,ntest,scomax,weight)

        endif
c
c  PRDIS: Print grade distribution.
c
      elseif(leqi(isay,'prdis'))then

        call pravds(mxstud,nstud,stnumn)
c
c  PRRANK: Print students by rank.
c
      elseif(leqi(isay,'prrank'))then

        call prrank(labtst,mxstud,mxtest,names,nstud,ntest,scores)
c
c  Q: Quit
c
      elseif(leqi(isay,'q'))then

        write(*,*)' '
        write(*,*)'Do you want to quit?  Enter "Y" to confirm.'
        read(*,'(a)')isay
        if(leqi(isay,'y'))stop
c
c  R: Roster
c
      elseif(leqi(isay,'r'))then

        if(istat.le.1)then

          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          call roster(classn,ierror,isect,mxstud,names,nsect,nstud)
 
          if(ierror.ne.0)go to 50

        endif
c
c  SAVE: Save a copy of the grade file.
c
      elseif(leqi(isay,'save'))then

        if(istat.le.1)then

          write(*,*)'GRADER - Error!'
          write(*,*)'  There is no current grade file to save.'

        endif

        write(*,*)'GRADER - A copy of the current grade file was saved.'
c
c  SORT: Sort file
c
      elseif(leqi(isay,'sort'))then

        if(istat.le.1)then

          write(*,*)' '
          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          call sort(isect,mxstud,mxtest,names,nsect,nstud,ntest,
     &      scores,ssn)
 
          if(ierror.ne.0)go to 50
 
          call rename(filold,filnew,ierror)
          if(ierror.ne.0)go to 50

        endif
c
c  T: Test scores to add to file
c
      elseif(leqi(isay,'t'))then

        if(istat.le.1)then

          write(*,*)'Error!  Please define a file first with the'
          write(*,*)'"F" command!'

        else

          call test(isect,labtst,mxstud,mxtest,names,nsect,nstud,
     &     ntest,scomax,scores,weight)

          if(ierror.ne.0)go to 50

          call rename(filold,filnew,ierror)

          if(ierror.ne.0)go to 50

        endif
c
c  ?: Get help from GRADER.HLP
c
      elseif(isay.eq.'?')then

        call hlpvms(filhlp)
c
c  Command not recognized
c
      else

        write(*,*)' '
        write(*,*)'GRADER did not understand your command: '//isay
        go to 10

      endif
c
c  After certain commands, write the current data to the file.
c
      if(leqi(isay,'add').or.
     &   leqi(isay,'c').or.
     &   leqi(isay,'delete').or.
     &   leqi(isay,'g').or.
     &   leqi(isay,'m').or.
     &   leqi(isay,'s').or.
     &   leqi(isay,'sort').or.
     &   leqi(isay,'t') )then

        call writer(chscor,classn,filnew,ierror,isect,issn,
     &    labtst,mxstud,mxtest,names,nsect,nstud,ntest,
     &    scomax,scores,ssn,weight)

      endif
c
c  Get the next command.
c
      go to 10
c
c  Error
c
50    continue
      write(*,*)' '
      write(*,*)'GRADER - Warning!'
      write(*,*)'  An error occurred while processing your command.'
      go to 10
      end
      subroutine addnam(isect,issn,labtst,mxstud,mxtest,
     &  names,nsect,nstud,ntest,scores,ssn)

c*********************************************************************72
c
cc ADDNAM adds one or more names to the class roster.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IERROR
c    Integer IERROR.
c    0, no error occurred.
c    nonzero, an error occurred while trying to add a name.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
      integer mxstud
      integer mxtest
c
      integer i
      character*1 isay
      integer iscore
      integer isect(mxstud)
      integer issn
      integer istud
      integer itest
      integer jsect
      character*8 labtst(ntest)
      logical leqi
      character*20 names(mxstud)
      character*20 namtmp
      integer nsect
      integer nstud
      integer ntest
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      character*20 ssntmp
      real temp
c
      external leqi
c
c  Are scores to be entered?
c
      iscore=0

      if(ntest.gt.0)then
        write(*,*)' '
        write(*,*)'Enter "Y" if scores are to be entered also.'
        read(*,'(a)')isay
        if(leqi(isay,'y'))iscore=1
      else
        iscore=0
      endif

      write(*,*)' '
      write(*,*)'ADDNAM - Note:'
      write(*,*)'  Ready to read the new names!'
c
c  Get new entry for new student: name, possibly section, possibly 
c  SSN, possibly scores.
c
      istud=nstud

10    continue

      nstud=istud
      istud=istud+1
c
c  Get name
c
      write(*,*)' '
      write(*,*)'Enter name (or RETURN if no more names)'
      read(*,'(a)')namtmp
      if(namtmp.eq.' ')return
      names(istud)=namtmp
c
c  Get section number
c
      if(nsect.gt.1)then
        write(*,*)'Enter section number'
        read(*,*)jsect
        isect(istud)=jsect
      else
        isect(istud)=1
      endif
c
c  Get SSN
c
      if(issn.ne.0)then
        write(*,*)'Enter SSN'
        read(*,'(a)')ssntmp
        if(ssntmp.eq.' ')ssntmp='000-00-0000'
        ssn(istud)=ssntmp
      else
        ssn(istud)='000-00-0000'
      endif
c
c  Enter scores
c
      if(iscore.eq.1.and.ntest.ge.1)then

        do itest=1,ntest
          write(*,*)'Enter score for '//labtst(itest)
          read(*,*)temp
          scores(istud,itest)=temp
        enddo

      else

        do i=1,ntest
          scores(istud,i)=0.0
        enddo

      endif
 
      go to 10

      end
      subroutine anonym(classn,isect,issn,istat,labtst,
     &  mxstud,mxtest,nline,nsect,nstud,ntest,scores,ssn)

c*********************************************************************72
c
cc ANONYM creates an "anonymous" grade listing.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
      integer mxstud
      integer mxtest
c
      character*50 classn
      integer i
      integer ido
      character*1 isay
      integer isect(mxstud)
      integer issn
      integer istat
      integer istud
      integer itest
      integer jsect
      character*8 labtst(mxtest)
      integer lenc
      integer lenchr
      logical leqi
      integer nline
      integer nsect
      integer nstud
      integer ntest
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
c
      external lenchr
      external leqi
c
      if(istat.le.1)then
        write(*,*)' '
        write(*,*)'ANONYM - Fatal error!'
        write(*,*)'  Please define a file first with the "F" command!'
        return
      endif

      if(issn.ne.1)then
        write(*,*)' '
        write(*,*)'ANONYM - Fatal error!'
        write(*,*)'  The anonymous listing option cannot be used '
        write(*,*)'  for this class, because SSN''s are not stored.'
        return
      endif
c
c  Determine what is wanted
c
      ido=0
      jsect=0

      if(nsect.gt.1)then

        nline=0
        write(*,*)'Enter:' 
        write(*,*)'  A for all sections, together,' 
        write(*,*)'  B for sections, separately,'
        write(*,*)'  C for a single section:'
        read(*,'(a1)',end=99,err=99)isay
 
        if(leqi(isay,'a'))then
          ido=0
          jsect=0
        elseif(leqi(isay,'b'))then
          ido=1
          jsect=0
        elseif(leqi(isay,'c'))then
          ido=2
          write(*,*)'Enter the section number:'
          read(*,*,end=99,err=99)jsect
        else
          write(*,*)' '
          write(*,*)'ANONYM - Fatal error.'
          write(*,*)'  Illegal option: '//isay
          return
        endif

      endif
c
c  Test number to be listed?
c
      itest=ntest

      if(ntest.gt.0)then
        write(*,*)'Enter the test number to list:'
        read(*,*,end=99,err=99)itest
      endif
c
c  Loop for roster
c
      write(*,*)' '
      lenc=lenchr(classn)
      write(*,'(a)')classn(1:lenc)
      write(*,*)' '
      write(*,*)'Anonymous listing of '//labtst(itest)
      write(*,*)' '

10    continue

      if(ido.eq.1)then
        jsect=jsect+1
      endif

      write(*,*)' '

      if(ido.eq.1.or.ido.eq.2)then
        write(*,*)'Section ',jsect
      endif

      write(*,*)' '

      istud=0
 
      do i=1,nstud

        if(ido.eq.0.or.isect(i).eq.jsect)then

          istud=istud+1

          if(nsect.le.1.or.ido.ne.0)then
            write(*,'(1x,i3,''.'',3x,a20,5x,f7.1)')
     &        istud,ssn(i),scores(i,itest)
          else
            write(*,'(1x,i3,''.'',i1,2x,a20,5x,f7.1)')
     &        istud,isect(i),ssn(i),scores(i,itest)
          endif

        endif

      enddo
 
      if(ido.eq.1.and.jsect.lt.nsect)go to 10

40    continue
 
      return

99    continue
      write(*,*)' '
      write(*,*)'ANONYM - Fatal error.'
      write(*,*)'  End of file, or error, on input.'
      stop

      end
      subroutine capchr(string)

c*********************************************************************72
c
cc CAPCHR capitalizes a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input/output, CHARACTER*(*) STRING.
c    STRING is the string of characters to be transformed.
c
      integer i
      integer itemp
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic ichar
      intrinsic len
c
      nchar=len(string)

      do i=1,nchar

        itemp=ichar(string(i:i))
        if(97.le.itemp.and.itemp.le.122)then
          string(i:i)=char(itemp-32)
        endif

      enddo

      return
      end
      subroutine chrcat(chrpre,chrpst,chrout)

c*********************************************************************72
c
cc CHRCAT concatenates two strings to make a third string.  
c
c  This is "roughly" the same as the following operation:
c    CHROUT=CHRPRE//CHRPST.
c  However, this routine was written to get around some of the
c  problems of using the FORTRAN "//" operator.
c
c  First of all, the "//" operator concatenates the entirety of two
c  strings, even if they contain unwanted trailing blanks.  If
c  CHRPRE was a ten character string, but only contained "dog" and
c  CHRPST was a ten character string that contained "cat", the
c  CHRPRE//CHRPST would create a twenty character string
c  "dog       cat       " whereas CHRCAT would create "dogcat".
c
c  Moreover, in the common case where you want to prefix or postfix
c  a string and put it back into the same variable, the "//"
c  operator won't work unless you restrict the range of the
c  operands.  For instance, the dog-cat example above might be
c  written CHRPRE=CHRPRE(1:3)//CHRPST(1:3).  This is tedious.
c
c  CHRCAT is designed to work for character strings or constants,
c  and to take as much work as possible off the user's shoulders.
c  You simply pass in the two strings to be concatenated, and the
c  string into which the concatenation is to be stored.  CHRCAT
c  will only handle up to a total of 255 nonblank characters.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHRPRE 
c    Input, CHARACTER*(*) CHRPRE.
c    CHRPRE is the "prefix" string.
c
c  CHRPST 
c    Input, CHARACTER*(*) CHRPST.
c    CHRPST is the "postfix" string.
c
c  CHROUT 
c    Output, CHARACTER*(*) CHROUT.
c    CHROUT is the output string made by concatenating CHRPRE and
c    CHRPST, ignoring any trailing blanks.
c
      integer maxtmp
      parameter (maxtmp=255)
c
      character*(*) chrpre
      character*(*) chrpst
      character*(*) chrout
      character*(maxtmp) chrtmp
      integer iout
      integer ipre
      integer ipst
      integer lenchr
c
      intrinsic len
      external lenchr
      intrinsic min
c
      iout=len(chrout)
      iout=min(iout,maxtmp)
c
      ipre=lenchr(chrpre)
      ipre=min(ipre,iout)
c
      ipst=lenchr(chrpst)
      ipst=min(ipst,iout-ipre)
c
      chrtmp=' '
      chrtmp(1:ipre+ipst)=chrpre(1:ipre)//chrpst(1:ipst)
      chrout=chrtmp
 
      return
      end
      subroutine chrchp(string,ilo,ihi)

c*********************************************************************72
c
cc CHRCHP chops out a portion of a string.
c
c  The routine accepts a STRING of characters and removes
c  positions ILO through IHI, pushes the end of STRING down and
c  pads with blanks.
c
c  Using quotes to denote the beginning and end of the string, then
c  calling CHRCHP with STRING='Fred is not a jerk!' and ILO=9 and
c  IHI=12 will result in the output STRING='Fred is a jerk!    '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input/output, CHARACTER*(*) STRING.
c    STRING is the character string to be transformed.
c
c  ILO    
c    Input, INTEGER ILO.
c    ILO is the location of the first character to be removed.
c
c  IHI    
c    Input, INTEGER IHI.
c    IHI is the location of the last character to be removed.
c
      character*1 chrtmp
      integer i
      integer ihi
      integer ilo
      integer inew
      integer nchar
      character*(*) string
c
      intrinsic len
c
      nchar=len(string)

      if(ilo.gt.ihi)return

      do i=ilo,nchar

        inew=i-ilo+ihi+1

        if(inew.le.nchar)then
          chrtmp=string(inew:inew)
          string(i:i)=chrtmp
        else
          string(i:i)=' '
        endif

      enddo

      return
      end
      subroutine chrctr(string,rval,ierror,lchar)

c*********************************************************************72
c
cc CHRCTR extracts a real number form a string.
c
c  The routine accepts a string of characters, and tries to extract a
c  real number from the initial part of the string.
c
c  CHRCTR will read as many characters as possible until it reaches
c  the end of the string, or encounters a character which cannot be
c  part of the real number.
c
c  Legal input is:
c
c     1 blanks,
c     2 '+' or '-' sign,
c     3 integer part,
c     4 decimal point,
c     5 fraction part,
c     6 'E' or 'e' or 'D' or 'd', exponent marker,
c     7 exponent sign,
c     8 exponent integer part,
c     9 exponent decimal point,
c    10 exponent fraction part,
c    11 blanks,
c    12 final comma,
c
c  with most quantities optional.
c
c  Example:
c
c    STRING            RVAL
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
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input, CHARACTER*(*) STRING.
c    STRING is the string containing the data to be read.  Reading 
c    will begin at position 1 and terminate at the end of the 
c    string, or when no more characters can be read to form a legal
c    real.  Blanks, commas, or other nonnumeric data will, in
c    particular, cause the conversion to halt.
c
c  RVAL   
c    Output, REAL RVAL.
c    RVAL is the real value that was read from the string.
c
c  IERROR
c    Output, INTEGER IERROR.
c    IERROR is the error flag.
c
c    0, no errors occurred.
c
c    1, 2, 6 or 7, the input number was garbled.  The
c    value of IERROR is the last type of input successfully
c    read.  For instance, 1 means initial blanks, 2 means
c    a plus or minus sign, and so on.
c
c  LCHAR  
c    Output, INTEGER LCHAR.
c    LCHAR is the number of characters read from
c    STRING to form the number, including any terminating
c    characters such as a trailing comma or blanks.
c
      character*1 chrtmp
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer lchar
      logical leqi
      integer nchar
      integer ndig
      real rbot
      real rexp
      real rtop
      real rval
      character*(*) string
c
      intrinsic len
      external leqi
      intrinsic lge
      intrinsic lle
      intrinsic real
c
      nchar=len(string)

      ierror=0
      rval=0.0
      lchar=-1
      isgn=1
      rtop=0
      rbot=1
      jsgn=1
      jtop=0
      jbot=1
      ihave=1
      iterm=0

10    continue
      lchar=lchar+1
      chrtmp=string(lchar+1:lchar+1)
c
c  Blank character.
c
      if(chrtmp.eq.' ')then
        if(ihave.eq.2.or.ihave.eq.6.or.ihave.eq.7)then
          iterm=1
        elseif(ihave.gt.1)then
          ihave=11
        endif
c
c  Comma.
c
      elseif(chrtmp.eq.',')then
        if(ihave.ne.1)then
          iterm=1
          ihave=12
          lchar=lchar+1
        endif
c
c  Minus sign.
c
      elseif(chrtmp.eq.'-')then
        if(ihave.eq.1)then
          ihave=2
          isgn=-1
        elseif(ihave.eq.6)then
          ihave=7
          jsgn=-1
        else
          iterm=1
        endif
c
c  Plus sign.
c
      elseif(chrtmp.eq.'+')then
        if(ihave.eq.1)then
          ihave=2
        elseif(ihave.eq.6)then
          ihave=7
        else
          iterm=1
        endif
c
c  Decimal point.
c
      elseif(chrtmp.eq.'.')then
        if(ihave.lt.4)then
          ihave=4
        elseif(ihave.ge.6.and.ihave.le.8)then
          ihave=9
        else
          iterm=1
        endif
c
c  Exponent marker.
c
      elseif(leqi(chrtmp,'e').or.leqi(chrtmp,'d') )then
        if(ihave.lt.6)then
          ihave=6
        else
          iterm=1
        endif
c
c  Digit.
c
      elseif(ihave.lt.11.and.
     &  lge(chrtmp,'0').and.lle(chrtmp,'9') )then
 
        if(ihave.le.2)then
          ihave=3
        elseif(ihave.eq.4)then
          ihave=5
        elseif(ihave.eq.6.or.ihave.eq.7)then
          ihave=8
        elseif(ihave.eq.9)then
          ihave=10
        endif
 
        read(chrtmp,'(i1)')ndig
 
        if(ihave.eq.3)then
          rtop=10*rtop+ndig
        elseif(ihave.eq.5)then
          rtop=10*rtop+ndig
          rbot=10*rbot
        elseif(ihave.eq.8)then
          jtop=10*jtop+ndig
        elseif(ihave.eq.10)then
          jtop=10*jtop+ndig
          jbot=10*jbot
        endif
c
c  Anything else is regarded as a terminator.
c
      else
        iterm=1
      endif
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
      if(iterm.ne.1.and.lchar+1.lt.nchar)go to 10
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LCHAR is equal to NCHAR.
c
      if(iterm.ne.1.and.lchar+1.eq.nchar)lchar=nchar
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if(ihave.eq.1.or.ihave.eq.2.or.ihave.eq.6.or.ihave.eq.7)then
        ierror=ihave
        return
      endif
c
c  Number seems OK.  Form it.
c
      if(jtop.eq.0)then
        rexp=1.0
      else
        if(jbot.eq.1)then
          rexp=10.0**(jsgn*jtop)
        else
          rexp=10.0**(real(jsgn*jtop)/real(jbot))
        endif
      endif
 
      rval=isgn*rexp*rtop/rbot
 
      return
      end
      subroutine chrdb1(string)

c*********************************************************************72
c
cc CHRDB1 removes all blanks from a string.
c
c  The routine accepts a string of characters and removes all
c  blanks and nulls, left justifying the remainder and padding with
c  blanks.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input/output, CHARACTER*(*) STRING.
c    STRING is the string to be transformed.
c
      character*1 chrtmp
      integer i
      integer j
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      nchar=len(string)

      j=0

      do i=1,nchar

        chrtmp=string(i:i)
        string(i:i)=' '

        if(chrtmp.ne.' '.and.chrtmp.ne.char(0))then
          j=j+1
          string(j:j)=chrtmp
        endif

      enddo

      return
      end
      subroutine chrdb2(string)

c*********************************************************************72
c
cc CHRDB2 replaces double blanks by one.
c
c  The routine accepts a string of characters.  It replaces all nulls
c  by blanks.  It replaces all strings of consecutive blanks by a
c  single blank, left justifying the remainder and padding with
c  blanks.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input/output, CHARACTER*(*) STRING.
c    STRING is the string to be transformed.
c
      integer i
      integer j
      integer nchar
      character*1 newchr
      character*1 oldchr
      character*(*) string
c
      intrinsic char
      intrinsic len
c
      nchar=len(string)
 
      j=0
      newchr=' '

      do i=1,nchar

        oldchr=newchr
        if(string(i:i).eq.char(0))string(i:i)=' '
        newchr=string(i:i)
        string(i:i)=' '

        if(oldchr.ne.' '.or.newchr.ne.' ')then
          j=j+1
          string(j:j)=newchr
        endif

      enddo
 
      return
      end
      subroutine colswap(chscor,clnumd,clnumn,clnust,icol,jcol,labtst,
     &  mxstud,mxtest,nostcl,nstud,scomax,scores,weight)

c*********************************************************************72
c
cc COLSWAP swaps two columns of the grade file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      integer mxstud
      integer mxtest
c
      character*8 chscor(mxstud,mxtest)
      character*8 chrtmp
      real clnumd(mxtest)
      real clnumn(mxtest)
      real clnust(mxtest)
      integer i
      integer icol
      integer jcol
      character*8 labtst(mxtest)
      integer nostcl(mxtest)
      integer nstud
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      real weight(mxtest)
c
      if(icol.eq.jcol)return

      do i=1,nstud
        call rswap(scores(i,icol),scores(i,jcol))
      enddo

      do i=1,nstud
        chrtmp=chscor(i,icol)
        chscor(i,icol)=chscor(i,jcol)
        chscor(i,jcol)=chrtmp
      enddo

      chrtmp=labtst(icol)
      labtst(icol)=labtst(jcol)
      labtst(jcol)=chrtmp

      call rswap(clnumd(icol),clnumd(jcol))
      call rswap(clnumn(icol),clnumn(jcol))
      call rswap(clnust(icol),clnust(jcol))
      call iswap(nostcl(icol),nostcl(jcol))
      call rswap(scomax(icol),scomax(jcol))
      call rswap(weight(icol),weight(jcol))

      return
      end
      subroutine compar(name,lennam,key,lenkey,match)

c*********************************************************************72
c
cc COMPAR compares a name to a key, and returns the number of matching characters.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  NAME
c    Input, CHARACTER*20 NAME.
c    NAME contains a name to be compared with KEY.
c
c  LENNAM
c    Input, integer LENNAM.
c    LENNAM is the "length" of NAME, that is, the position of the last
c    nonblank character.
c
c  KEY
c    Input, CHARACTER*20 KEY.
c    KEY is the key to be considered.
c
c  LENKEY
c    Input, INTEGER LENKEY.
c    LENKEY is the length of KEY, to the last nonblank.
c
c  MATCH
c    Output, INTEGER MATCH.
c    MATCH is the number of characters of NAME and KEY that match.
c
      integer i
      integer i1
      integer i2
      character*20 key
      integer lenkey
      integer lennam
      logical leqi
      integer match
      character*20 name
c
      external leqi
c
      match=0
      if(lenkey.gt.lennam)return

      do i=1,lennam+1-lenkey

        match=0

10      continue

        i1=i+match
        i2=1+match

        if(leqi(name(i1:i1),key(i2:i2)))then
          match=match+1
          if(match.ge.lenkey)return
          go to 10
        endif

      enddo

      return
      end
      subroutine create(classn,isect,issn,istat,mxstud,names,nsect,
     &  nstud,ntest,ssn)

c*********************************************************************72
c
cc CREATE creates a file for a class and list of students.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
      integer mxstud
c
      character*50 classn
      character*1 isay
      integer isect(mxstud)
      integer issn
      integer istat
      integer istud
      logical leqi
      character*20 names(mxstud)
      integer nsect
      integer nstud
      integer ntest
      character*20 ssn(mxstud)
c
      external leqi
c
      ntest=0
c
c  Get the class name.
c
      write(*,*)'Enter the class name.'
      read(*,'(a)')classn

10    continue
c
c  Get the number of sections.
c
      nsect=1
      write(*,*)'Enter the number of sections in class.'
      read(*,*)nsect
      
      if(nsect.le.0)then
        nsect=1
      endif
c
c  Store SSN?
c
      write(*,*)'Enter "Y" if social security numbers (SSN) are used.'
      read(*,'(a)')isay

      if(leqi(isay,'y'))then
        issn=1
      else
        issn=0
      endif
c
c  It is assumed user wants to input names to this new file
c
      write(*,*)' '
      write(*,*)'CREATE is ready to read student information.'
      write(*,*)' '
      write(*,*)'If you use the format "Last name, First name"'
      write(*,*)'GRADER will be able to alphabetize the names.'

      if(issn.eq.1)then
        write(*,*)' '
        write(*,*)'Use the format "123-45-6789" for SSN''s'
      endif
c
c  Read information for next student
c
      istud=0

      write(*,*)' '

30    continue

      nstud=istud
      istud=istud+1

      names(istud)=' '
      isect(istud)=1
      ssn(istud)='000-00-0000'

      write(*,*)'Enter name (or return to stop).'
      read(*,'(a)')names(istud)
c
c  Check for a blank name.
c
      if(names(istud).eq.' ')go to 40

      if(nsect.gt.1)then
        write(*,*)'Enter section'
        read(*,*)isect(istud)
      endif

      if(issn.ne.0)then
        write(*,*)'Enter SSN'
        read(*,'(a)')ssn(istud)
      endif

      go to 30
c
c  A blank name was entered, signaling the end of input.
c
40    continue

      write(*,*)' '
      write(*,*)'CREATE read ',nstud,' names.'

      istat=2

      return
      end
      subroutine deffil(chscor,classn,filnew,filsav,ierror,
     &  isect,issn,istat,labtst,mxstud,mxtest,names,
     &  nsect,nstud,ntest,scomax,scores,ssn,weight)

c*********************************************************************72
c
cc DEFFIL opens an existing file, or creates a new one.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      character*60 filnew
      character*60 filsav
      integer ierror
      integer isect(mxstud)
      integer issn
      integer istat
      character*8 labtst(mxtest)
      integer lenc
      integer lenchr
      character*20 names(mxstud)
      integer nsect
      integer nstud
      integer ntest
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      real weight(mxtest)
c
      external lenchr
c
      write(*,*)' '
      write(*,*)'DEFFIL:'
      write(*,*)'  Enter the name of the grade file to use.'
      read(*,'(a)')filnew

      lenc=lenchr(filnew)

      if(lenc.le.0)then
        write(*,*)' '
        write(*,*)'DEFFIL - Error!'
        write(*,*)'  Your file name was blank!'
        ierror=2
        return
      endif
c
c  Try to open file and read information
c
      call reader(chscor,classn,filnew,ierror,isect,issn,istat,
     &  labtst,mxstud,mxtest,names,nsect,nstud,ntest,
     &  scomax,scores,ssn,weight)

      if(ierror.ne.0)then
        istat=0
        ierror=0
      endif
c
c  If file exists, make a backup copy in FILSAV.
c
      if(istat.eq.2)then

        call delfil(filsav)

        call writer(chscor,classn,filsav,ierror,isect,issn,
     &    labtst,mxstud,mxtest,names,nsect,nstud,ntest,
     &    scomax,scores,ssn,weight)

        if(ierror.ne.0)return

      else
        write(*,*)' '
        write(*,*)'DEFFIL - Note:'
        write(*,*)'  This is a new file.'
      endif

      return
      end
      subroutine delete(isect,mxstud,mxtest,names,namtmp,nstud,
     &  scores,ssn)

c*********************************************************************72
c
cc DELETE deletes from the roster all students whose names match the key NAMTMP.
c
c  The user is asked to confirm each deletion before it is carried out.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
      integer mxstud
      integer mxtest
c
      character*1 isay
      integer isect(mxstud)
      integer istud
      integer jstud
      integer k
      character*20 key
      integer lenchr
      integer lenkey
      integer lennam
      logical leqi
      integer match1
      character*20 names(mxstud)
      character*20 namtmp
      integer ndel
      integer nstud
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
c
      external lenchr
      external leqi
c
      ndel=0
c
c  Get a search key for the student's name.
c
10    continue

      write(*,*)'Enter search key (return to quit)'
      read(*,'(a)')key

      call chrdb1(key)
      lenkey=lenchr(key)
      if(lenkey.le.0)return

      call capchr(key)
c
c  Search for a similar name
c
      istud=0

20    continue

      istud=istud+1

      if(istud.gt.nstud)then
        write(*,*)' '
        write(*,*)'DELETE - Note:'
        write(*,*)ndel,' students were dropped from the roster.'
        return
      endif

      namtmp=names(istud)
      call chrdb1(namtmp)
      lennam=lenchr(namtmp)
      call capchr(namtmp)
      if(lennam.lt.lenkey)go to 20

      call compar(namtmp,lennam,key,lenkey,match1)
      if(match1.ne.lenkey)go to 20
c
c  Perfect match
c
30    continue

      write(*,*)' '
      write(*,*)'Enter Y to delete all records of '//names(istud)
      read(*,'(a)')isay
    
      if(.not.leqi(isay,'y'))go to 20
c
c  Shift all students up one row, obliterating student ISTUD.
c
      ndel=ndel+1

      do jstud=istud+1,nstud

        names(jstud-1)=names(jstud)
        ssn(jstud-1)=ssn(jstud)
        isect(jstud-1)=isect(jstud)

        do k=1,mxtest
          scores(jstud-1,k)=scores(jstud,k)
        enddo

      enddo

      nstud=nstud-1

      go to 20

      end
      subroutine delfil(filnam)

c*********************************************************************72
c
cc DELFIL will delete a file of a given name.
c
c  If the file does not already exist, then DELFIL will simply return.
c
c  One reason to use DELFIL would be so that one could then use an
c  OPEN statement with the keyword " STATUS='NEW' ", guaranteeing
c  that a new copy of a file is opened.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  FILNAM 
c    Input, CHARACTER*(*) FILNAM.
c    FILNAM is the name of the file to be deleted.
c
      character*(*) filnam
      integer ierror
      integer iunit
      integer lenchr
      integer lenc
c
      external lenchr
c
      lenc=lenchr(filnam)
 
      if(lenc.le.0)then
        write(*,*)' '
        write(*,*)'DELFIL - Fatal error!'
        write(*,*)'  The name of the file is blank.'
        stop
      endif
c
c  Get a free FORTRAN unit number.
c
      call numfil(ierror,iunit,'open')

      open(unit=iunit,file=filnam,status='old',err=10)

      close(unit=iunit,status='delete',err=20)

      call numfil(ierror,iunit,'close')

      write(*,*)' '
      write(*,*)'DELFIL - Note:'
      write(*,*)'  Deleted old copy of '//filnam(1:lenc)

10    continue

      return

20    continue

      write(*,*)' '
      write(*,*)'DELFIL - Fatal error!'
      write(*,*)'  Could not delete the file '//filnam(1:lenc)
 
      stop
      end
      subroutine exheap(n,indx,i,j,isgn)

c*********************************************************************72
c
cc EXHEAP externally sorts a list of items into linear order.
c
c  The actual list is not passed to the routine.  Hence it may
c  consist of integers, reals, numbers, names, etc.  The user,
c  after each return from EXHEAP, will be asked to compare or
c  interchange two items.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  N      
c    Input, INTEGER N.
c    N is the length of the input list.
c
c  INDX   
c    Input/output, INTEGER INDX.
c    The user must set INDX to 0 before the first call to EXHEAP.
c
c    On return,
c
c     if INDX is greater than 0, the user must interchange
c     items I and J
c     and recall the routine.
c
c     If INDX is less than 0, the user is to compare items I
c     and J and return in ISGN a negative value if I is to
c     precede J, and a positive value otherwise.
c
c     If INDX is 0, the sorting is done.
c
c  I,
c  J      
c    Output, INTEGER I, J.
c    On return with INDX positive, elements I and J of the user's 
c    list should be interchanged.  On return with INDX negative, 
c    elements I and J are to be compared by the user.
c
c  ISGN   
c    Input, INTEGER ISGN. 
c    On return with INDX negative, the user should compare elements 
c    I and J of the list.  If item I is to precede item J, set ISGN 
c    negative, otherwise set ISGN postive.
c
      integer i
      integer indx
      integer isgn
      integer j
      integer l
      integer l1
      integer n
      integer n1
c
      save l
      save l1
      save n1
c
      data l /0/
      data l1 /0/
      data n1 /0/
c
      if(indx.lt.0)then

        if(indx.ne.-1)then
          if(isgn.lt.0)i=i+1
          j=l1
          l1=i
          indx=-1
          return
        endif

        if(isgn.le.0)go to 20
        indx=2
        return

      elseif(indx.eq.1)then

        l1=l

      elseif(indx.ne.2)then

        n1=n
        l=n/2
        l1=l

      endif

10    continue

      i=l1+l1

      if(i.eq.n1)then
        j=l1
        l1=i
        indx=-1
        return
      elseif(i.le.n1)then
        j=i+1
        indx=-2
        return
      endif

20    continue

      if(l.gt.1)then
        l=l-1
        l1=l
        go to 10
      endif

      if(n1.eq.1)then
        indx=0
        return
      endif

      i=n1
      n1=n1-1
      j=1
      indx=1

      return
      end
      subroutine flushl(string)

c*********************************************************************72
c
cc FLUSHL flushes a string left.  
c
c  For instance:
c
c    Input             Output
c
c    'Hello     '      'Hello     '
c    ' Hi there!  '    'Hi there!   '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING Input/output, CHARACTER*(*) STRING.
c
c         On input, STRING is a string of characters.
c
c         On output, any initial blank characters in STRING
c         have been cut, and pasted onto the back.
c
      integer i
      integer lchar
      integer lenchr
      integer nonb
      character*(*) string
c
      intrinsic len
      external lenchr
c
c  Check the full length of the string.
c
      lchar=len(string)
c
c  Find the occurrence of the first nonblank.
c
      do i=1,lchar
        if(string(i:i).ne.' ')then
          nonb=i
          string(1:lchar+1-nonb)=string(nonb:lchar)
          string(lchar+2-nonb:lchar)=' '
          return
        endif
      enddo

      return
      end
      subroutine flushr(string)

c*********************************************************************72
c
cc FLUSHR flushes a string right.  
c
c  For instance:
c
c    Input             Output
c
c    'Hello     '      '     Hello'
c    ' Hi there!  '    '   Hi there!'
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input/output, CHARACTER*(*) STRING.
c
c    On input, STRING is a string of characters.
c
c    On output, any trailing blank characters in STRING
c    have been cut, and pasted back onto the front.
c
      integer i
      integer lchar
      integer lenchr
      integer nonb
      character*(*) string
c
      intrinsic len
      external lenchr
c
c  Check the full length of the string.
c
      lchar=len(string)
c
c  Find the occurrence of the last nonblank.
c
      nonb=lenchr(string)
c
c  Shift the string right.
c
      do i=lchar,lchar+1-nonb,-1
        string(i:i)=string(i-lchar+nonb:i-lchar+nonb)
      enddo
c
c  Blank out the beginning of the string.
c
      do i=1,lchar-nonb
        string(i:i)=' '
      enddo

      return
      end
      subroutine gracom(chscor,clnumd,clnumn,clnust,labtst,list,
     &  mxstud,mxtest,nostcl,nosttr,nstud,ntest,scomax,scores,
     &  stnumn,stunk,weight)

c*********************************************************************72
c
cc GRACOM computes the grade data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLNUMD 
c    Real CLNUMD(MXTEST).
c    CLNUMD is the class numeric median.  CLNUMD(I) is the median 
c    grade for all students in the class with a valid grade for 
c    test I.
c
c  CLNUMN 
c    Real CLNUMN(MXTEST).
c    CLNUMN is the class numeric mean.  CLNUMN(I) is the mean 
c    grade for all students in the class with a valid grade for 
c    test I.
c
c  CLNUST 
c    Class numeric standard deviation.  CLNUST(I) is the standard
c    deviation from the mean, for all students in the class with
c    a valid grade for test I.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NOSTCL 
c    Integer NOSTCL(MXTEST).
c    NOSTCL(I) is the number of students with valid scores for 
c    test I.
c
c  NOSTTR 
c    Integer NOSTTR.
c    NOSTTR is the number of students with valid scores for the term.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c    However, if SCORES(I,J) is negative, then the score is not
c    to be counted.
c
c  STNUMN 
c    Real STNUMN(MXSTUD).
c    For each student, STNUMN(I) is the student's numeric mean,
c    or averaged grade for the class.
c
c  TRNUMD Term numeric median
c
c  TRNUMN Term numeric mean
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      real bot
      character*8 chrtmp
      character*8 chscor(mxstud,mxtest)
      real clnumd(mxtest)
      real clnumn(mxtest)
      real clnust(mxtest)
      integer i
      integer indx
      integer isgn
      integer istud
      integer itemp
      integer itest
      integer j
      integer jave
      integer jposs
      integer jtotal
      character*8 labtst(mxtest)
      integer lenchr
      logical leqi
      integer list(mxstud)
      integer lmedn
      integer nlist
      integer nonumn
      integer nostcl(mxtest)
      integer nosttr
      integer nstud
      integer ntest
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      real scotot
      real stnumn(mxstud)
      real stumax
      real stunk(mxstud)
      real stutot
      real stuval
      real temp
      real trnumd
      real trnumn
      real weight(mxtest)
c
      external lenchr
      external leqi
      intrinsic mod
c
c  Initialize some items.
c
      do istud=1,mxstud
        list(istud)=istud
      enddo
c
c  For student ISTUD, compute the numeric mean, STNUMN(ISTUD).
c
      do istud=1,nstud

        stumax=0.0
        stuval=0.0

        do itest=1,ntest

          if((labtst(itest).ne.' Average').and.
     &       (labtst(itest).ne.'   Extra').and.
     &       (labtst(itest).ne.'Possible').and.
     &       (labtst(itest).ne.'   Total'))then
c
c  If the student is liable for test ITEST,
c    add the maximum number of points possible to STUMAX,
c    and the actual achieved points to STUVAL.
c
            if(scores(istud,itest).ge.0.0)then
              stumax=stumax+weight(itest)*scomax(itest)
              stuval=stuval+weight(itest)*scores(istud,itest)
            endif
 
          endif

        enddo
c
c  The student's numeric mean is the amount achieved divided by 
c  what was possible, times 100.
c
        if(stumax.ne.0.0)then
          stnumn(istud)=100.0*stuval/stumax
        else
          stnumn(istud)=0.0
        endif

      enddo
c
c  For each test, compute
c    CLNUMN(ITEST), the class numeric mean, and
c    NOSTCL(ITEST), the number of students liable for the test.
c
      do itest=1,ntest

        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
 
          clnumn(itest)=0.0
          nostcl(itest)=0.0

          do istud=1,nstud
c
c  If student ISTUD is liable for test ITEST, 
c    add him to the count,
c    add his score to the numeric mean calculation.
c
            if(scores(istud,itest).ge.0.0)then

              nostcl(itest)=nostcl(itest)+1
              clnumn(itest)=clnumn(itest)+scores(istud,itest)
  
            endif

          enddo

          if(nostcl(itest).le.0)then
            clnumn(itest)=0.0
          else
            clnumn(itest)=clnumn(itest)/real(nostcl(itest))
          endif

        endif

      enddo
c
c  Compute NONUMN, the number of valid scores for the whole term, 
c  and TRNUMN, the term mean.
c
      nonumn=0
      trnumn=0.0

      do itest=1,ntest

        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
 
          do istud=1,nstud

            if(scores(istud,itest).ge.0.0)then

              if(scomax(itest).ne.0.0)then
               temp=100.0*scores(istud,itest)/scomax(itest)
              else
                temp=0.0
              endif

              nonumn=nonumn+1
              trnumn=trnumn+temp

            endif

          enddo

        endif

      enddo

      if(nonumn.gt.0)then
        trnumn=trnumn/real(nonumn)
      else
        trnumn=0.0
      endif
c
c  Start the computation of CLNUST, the class standard deviations.
c
      do itest=1,ntest

        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
 
          clnust(itest)=0.0

          do istud=1,nstud

            if(scores(istud,itest).ge.0.0)then
              if(scomax(itest).ne.0.0)then
                clnust(itest)=clnust(itest)+100*
     &            scores(istud,itest)**2/scomax(itest)
              endif
            endif

          enddo

        endif

      enddo
c
c  Compute CLNUMD, the class medians.
c
      do itest=1,ntest

        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
 
          nlist=0
          clnumd(itest)=0.0

          do istud=1,nstud

            if(scores(istud,itest).ge.0.0)then
              nlist=nlist+1
              stunk(nlist)=scores(istud,itest)
            endif

          enddo

          if(nlist.gt.0)then

            i=0
            indx=0
            isgn=0
            j=0

10          continue

            call exheap(nlist,indx,i,j,isgn)

            if(indx.lt.0)then

              if(stunk(j).gt.stunk(i))then
                isgn=1
              else
                isgn=-1
              endif

              go to 10

            elseif(indx.gt.0)then
              temp=stunk(i)
              stunk(i)=stunk(j)
              stunk(j)=temp
              go to 10
            endif

            if(mod(nostcl(itest),2).eq.1)then
              lmedn=(nostcl(itest)+1)/2
              clnumd(itest)=stunk(lmedn)
            else
              lmedn=nostcl(itest)/2
              clnumd(itest)=0.5*(stunk(lmedn)+stunk(lmedn+1))
            endif

          endif

        endif

      enddo
c
c  Sort student means for test averages.
c
      do i=1,nstud
        stunk(i)=stnumn(i)
      enddo

      i=0
      indx=0
      isgn=0
      j=0

20    continue

      call exheap(nstud,indx,i,j,isgn)

      if(indx.lt.0)then
        isgn=-1
        if(stunk(j).gt.stunk(i))isgn=1
        go to 20
      elseif(indx.gt.0)then
        temp=stunk(i)
        stunk(i)=stunk(j)
        stunk(j)=temp
        itemp=list(i)
        list(i)=list(j)
        list(j)=itemp
        go to 20
      endif

      if(mod(nstud,2).eq.1)then
        lmedn=(nstud+1)/2
        trnumd=stunk(lmedn)
      else
        lmedn=nstud/2
        trnumd=0.5*(stunk(lmedn)+stunk(lmedn+1))
      endif

      do itest=1,ntest

        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
 
          if(nostcl(itest).le.1)then
            clnust(itest)=0.0
          else
            bot=real(nostcl(itest)-1)
            temp=(clnust(itest)-nostcl(itest)*clnumn(itest)**2) / bot
            if(temp.ge.0.0)then
              clnust(itest)=sqrt(temp)
            else
              write(*,*)' '
              write(*,*)'GRACOM - Fatal error!'
              write(*,*)'  Standard deviation calculation fails.'
              write(*,*)'  Test number ITEST=',itest
              write(*,*)'  Test label '//labtst(itest)
              write(*,*)'    Number of students NOSTCL(ITEST)=',
     &          nostcl(itest)
              write(*,*)'    Sum of squares of scores = ',clnust(itest)
              write(*,*)'    NOSTCL * Average**2 = ',
     &          nostcl(itest)*clnumn(itest)**2
              clnust(itest)=0.0
c             stop
            endif
          endif
        endif

      enddo
c
c  Compute NOSTTR, the total number of valid scores.
c
      nosttr=0
      do itest=1,ntest
        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
 
          if(scomax(itest).ne.0.0)then
            do istud=1,nstud
              if(scores(istud,itest).ge.0.0)then
                nosttr=nosttr+1
              endif
            enddo
          endif
        endif
      enddo

c     endif
c
c  If there is not already a "Total" column, add it to the end.
c  Then write the total data in the total column.
c
      do itest=1,ntest
        if(labtst(itest).eq.'   Total')then
          jtotal=itest
          go to 98
        endif
      enddo

      jtotal=ntest+1
      ntest=ntest+1
      labtst(jtotal)='   Total'

98    continue
      
      scotot=0.0
      do itest=1,ntest
        if((labtst(itest).ne.' Average').and.
     &     (labtst(itest).ne.'   Extra').and.
     &     (labtst(itest).ne.'Possible').and.
     &     (labtst(itest).ne.'   Total'))then
          scotot=scotot+weight(itest)*scomax(itest)
        endif
      enddo

      scomax(jtotal)=scotot
      clnumn(jtotal)=0.0
      clnumd(jtotal)=0.0
      clnust(jtotal)=0.0
      nostcl(jtotal)=nosttr
c
c  Now, for each student, get the total points.
c
      do istud=1,nstud
        stutot=0.0
        do itest=1,ntest
          if((labtst(itest).ne.' Average').and.
     &       (labtst(itest).ne.'   Extra').and.
     &       (labtst(itest).ne.'Possible').and.
     &       (labtst(itest).ne.'   Total'))then
            if(scores(istud,itest).ge.0.0)then
              stutot=stutot+weight(itest)*scores(istud,itest)
            endif
          endif
        enddo

        scores(istud,jtotal)=stutot

        if(scores(istud,jtotal).ge.0.0)then
          write(chrtmp,'(f8.1)')scores(istud,jtotal)
        else
          chrtmp=' Excused'
        endif

        chscor(istud,jtotal)=chrtmp

      enddo

c
c  If there is not already an "Average" column, add it.
c  Then write the average data in the average column.
c
      do itest=1,ntest
        if(labtst(itest).eq.' Average')then
          jave=itest
          go to 99
        endif
      enddo

      jave=ntest+1
      ntest=ntest+1
      labtst(jave)=' Average'

99    continue
      
      scomax(jave)=100.0
      clnumn(jave)=trnumn
      clnumd(jave)=trnumd
      clnust(jave)=0.0
      nostcl(jave)=nosttr

      do istud=1,nstud
        scores(istud,jave)=stnumn(istud)
        if(scores(istud,jave).ge.0.0)then
          write(chrtmp,'(f8.1)')scores(istud,jave)
        else
          chrtmp=' Excused'
        endif
        chscor(istud,jave)=chrtmp
      enddo

c
c  If there is not already a "Possible" column, add it.
c  Then write the maximum points possible for each student.
c
      do itest=1,ntest
        if(labtst(itest).eq.'Possible')then
          jposs=itest
          go to 199
        endif
      enddo

      jposs=ntest+1
      ntest=ntest+1
      labtst(jposs)='Possible'

199   continue

      temp=0.0
      do j=1,ntest
        if(labtst(j).ne.' Average'.and.
     &     labtst(j).ne.'   Extra'.and.
     &     labtst(j).ne.'   Total'.and.
     &     labtst(j).ne.'Possible')then
          temp=temp+scomax(j)
        endif
      enddo
      scomax(jposs)=temp

      clnumn(jposs)=0
      clnumd(jposs)=0
      clnust(jposs)=0
      nostcl(jposs)=nstud

      do istud=1,nstud
        temp=0.0
        do j=1,ntest
          if(labtst(j).ne.' Average'.and.
     &       labtst(j).ne.'   Total'.and.
     &       labtst(j).ne.'Possible')then
            if(scores(istud,j).ge.0.0)then
              temp=temp+scomax(j)
            endif
          endif
        enddo
        scores(istud,jposs)=temp
      enddo
c
c  Now, just for neatness, make sure that the last three columns are
c  Total, Possible, Average.
c
c  Take care of the common problem that when you put, say, JTOTAL
c  in the right place, you displace column JPOSS.
c
      call colswap(chscor,clnumd,clnumn,clnust,jtotal,ntest-2,labtst,
     &  mxstud,mxtest,nostcl,nstud,scomax,scores,weight)

      if(jposs.eq.ntest-2)then
        jposs=jtotal
      elseif(jave.eq.ntest-2)then
        jave=jtotal
      endif

      jtotal=ntest-2

      call colswap(chscor,clnumd,clnumn,clnust,jposs,ntest-1,labtst,
     &  mxstud,mxtest,nostcl,nstud,scomax,scores,weight)

      if(jave.eq.ntest-1)then
        jave=jposs
      endif

      jposs=ntest-1

      call colswap(chscor,clnumd,clnumn,clnust,jave,ntest,labtst,
     &  mxstud,mxtest,nostcl,nstud,scomax,scores,weight)

      jave=ntest

      return
      end
      subroutine graprn(chscor,classn,clnumd,clnumn,clnust,labtst,
     &  mxstud,mxtest,names,nostcl,nstud,ntest,scomax,weight)

c*********************************************************************72
c
cc GRAPRN prints out the data graded by GRACOM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  CLNUMD 
c    Real CLNUMD(MXTEST).
c    CLNUMD is the class numeric median.  CLNUMD(I) is the median 
c    grade for all students in the class with a valid grade for 
c    test I.
c
c  CLNUMN 
c    Real CLNUMN(MXTEST).
c    CLNUMN is the class numeric mean.  CLNUMN(I) is the mean 
c    grade for all students in the class with a valid grade for 
c    test I.
c
c  CLNUST 
c    Class numeric standard deviation.  CLNUST(I) is the standard
c    deviation from the mean, for all students in the class with
c    a valid grade for test I.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NOSTCL 
c    Integer NOSTCL(MXTEST).
c    NOSTCL(I) is the number of students with valid scores for 
c    test I.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      real clnumd(mxtest)
      real clnumn(mxtest)
      real clnust(mxtest)
      integer i
      integer j
      character*8 labtst(mxtest)
      integer lenc
      integer lenchr
      character*20 names(mxstud)
      integer nostcl(mxtest)
      integer nstud
      integer ntest
      real scomax(mxtest)
      real weight(mxtest)
c
c  Write header
c
      write(*,*)' '
      lenc=lenchr(classn)
      write(*,*)classn(1:lenc)
      write(*,*)' '
      write(*,*)'There are ',nstud,' students.'
      write(*,*)'There are ',ntest,' scores recorded.'
      write(*,*)' '
      write(*,*)' '
      write(*,'(9a8)')(labtst(j),j=1,ntest)
      write(*,*)' '
      write(*,'(a)')'Test maximum:'
      write(*,'(9f8.1)')(scomax(j),j=1,ntest)
      write(*,*)' '
      write(*,'(a)')'Test weight:  '
      write(*,'(9f8.1)')(weight(j),j=1,ntest)
      write(*,*)' '
      write(*,*)'Means:'
      write(*,'(9f8.1)')(clnumn(j),j=1,ntest)
      write(*,*)'Medians:'
      write(*,'(9f8.1)')(clnumd(j),j=1,ntest)
      write(*,*)'Standard deviations:'
      write(*,'(9f8.1)')(clnust(j),j=1,ntest)
      write(*,*)'Number of students:'
      write(*,'(9i8)')(nostcl(j),j=1,ntest)

      do i=1,nstud
        write(*,*)' '
        write(*,'(a20)')names(i)
        write(*,'(9a8)')(chscor(i,j),j=1,ntest)
      enddo

      return
      end
      subroutine hello(mxstud,mxtest)

c*********************************************************************72
c
cc HELLO prints out a brief introductory message.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
      integer mxstud
      integer mxtest
c
c
      write(*,*)' '
      write(*,*)'GRADER'
      write(*,*)'  Version 1.13'
      write(*,*)'  11 December 1996'
      write(*,*)' '
      write(*,*)'GRADER can handle up to ',mxstud,' students'
      write(*,*)'and ',mxtest-1,' tests.'
      write(*,*)' '
 
      return
      end
      subroutine help(filhlp,filnew)

c*********************************************************************72
c
cc HELP prints out a list of the available commands.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  FILHLP
c    Character*80 FILHLP.
c    FILHLP is the name of the help file, which is probably called
c    'grader.hlp'.  
c
      character*80 filhlp
      character*60 filnew
c
      write(*,*)' '
      write(*,*)'A       Anonymous test result listing by SSN.'
      write(*,*)'Add     Add names to class roster.'
      write(*,*)'C       Create a new grade file named.'
      write(*,*)'Date    print today''s date.'
      write(*,*)'Delete  delete names from class roster.'
      write(*,*)'F       handle another file'
      write(*,*)'G       grade '//filnew
      write(*,*)'H       Help  (print this list)'
      write(*,*)'LABELS  Print out the grade labels.'
      write(*,*)'M       Modify a student name or scores.'
      write(*,*)'P       Print out '//filnew
      write(*,*)'PRDIS   Print out score distribution.'
      write(*,*)'PRRANK  Print students by rank.'
      write(*,*)'Q       Quit'
      write(*,*)'R       Make a class roster.'
      write(*,*)'SAVE    Save a copy of the current file.'
      write(*,*)'SORT    Sort '//filnew
      write(*,*)'T       Enter scores for a single test.'
      write(*,*)'#       Make a comment.'
      write(*,*)'?       Extensive help from '//filhlp

      return
      end
      subroutine hlpvms(filhlp)

c*********************************************************************72
c
cc HLPVMS is an interactive help system.  
c
c  To be used, the GRADER help file must be available.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  FILHLP
c    Character*80 FILHLP.
c    FILHLP is the name of the help file, which is probably called
c    'grader.hlp'.  
c
      integer maxtop
      parameter (maxtop=40)
c
      character*75 choice
      character*75 ctemp
      character*75 ctemp2
      character*80 filhlp
      integer i
      integer ierror
      integer iline
      character*75 inline
      integer itop
      integer iunit
      integer jerror
      character*1 lab
      integer lenc
      integer lenchr
      logical leqi
      integer level
      character*75 levelc(maxtop)
      integer levelm(10)
      integer levelo
      integer levelt(maxtop)
      integer move
      integer ntop
      integer num
c
      external lenchr
      external leqi
      intrinsic lge
      intrinsic lle
c
      ierror=0

      call numfil(ierror,iunit,'open')
c
c  Open help file.
c
c  For a "private" VMS copy of GRADER, or any copy on IBM,
c  Macintosh, or UNIX, use this statement:
c
      open(unit=iunit,file=filhlp,status='old',err=100)

      levelo=0
      level=1
      iline=1
c
c  Move to beginning of current topic by reading move lines from
c  the top of the file.  Record this position, corresponding to
c  the current LEVEL, in LEVELM, in case we later want to back up.
c
c  Print out the heading line of this topic.
c
10    continue

      jerror=0
      move=iline
      levelm(level)=iline

      do i=1,move-1
        read(iunit,'(1x)',end=110,err=110)
      enddo

      write(*,*)' '
      read(iunit,'(a1,a75)',end=110,err=110)lab,inline
      write(*,'(a)')inline
c
c  If 'going down' or redisplaying, (as opposed to backing up),
c  display the information available under the current topic.
c
c  We stop printing when we hit a numeric label.
c
c  If this label is less than or equal to the current level,
c  there are no subtopics.
c
c  Otherwise, we now move ahead to print out the list of subtopics
c  available for this topic.
c
      if(level.ge.levelo)then
        ntop=-1
30      continue
        read(iunit,'(a1,a75)',end=50)lab,inline
        move=move+1

        if(lge(lab,'0').and.lle(lab,'9'))then
          read(lab,'(i1)')num
          if(num.le.level)go to 50
          ntop=0
          go to 40
        endif

        write(*,'(a)')inline
        go to 30
      else
        ntop=0
        inline=' '
        lab=' '
      endif
c
c  Locate each subtopic by examining column 1, searching for
c  integer labels.
c
c  Assuming we are at level level, we are searching for labels
c  equal to level+1.  As we encounter each such label, we want to
c  store the rest of the line as a subtopic.
c
c  We ignore labels greater than level+1 because these are
c  sub-subtopics, and we cease our search when we reach a label
c  less than or equal to level.
c
40    continue

      if(lge(lab,'0').and.lle(lab,'9'))then

        read(lab,'(i1)')num
        if(num.le.level)go to 50

        if(num.eq.level+1)then

          ntop=ntop+1

          if(ntop.eq.1)then
            write(*,*)' '
            write(*,*)'Help is available on:'
            write(*,*)' '
          endif

          write(*,'(a)')inline

          levelt(ntop)=move
          levelc(ntop)=inline

        endif

      endif

      read(iunit,'(a1,a75)',end=50,err=50)lab,inline
      move=move+1
      go to 40

50    continue
c
c  Display subtopics.
c
      write(*,*)' '
      write(*,*)'RETURN to back up, ? to redisplay.'
c
c  Prompt for user choice of new topic, exit, or back up.
c
60    continue
      ierror=0

      if(ntop.gt.0)then
        write(*,*)'Enter topic you want help on, or RETURN or ?.'
      else
        write(*,*)'RETURN or ?.'
      endif

      read(*,'(a)')choice

      if(ierror.ne.0)then
        ierror=0
        close(unit=iunit)
        call numfil(ierror,iunit,'close')
        return
      endif

      call chrdb2(choice)
      lenc=lenchr(choice)
      if(lenc.le.0)choice='!'
      ctemp=choice
c
c  Two errors in a row, OK, but three suggests that something is
c  wrong.
c
      if(ierror.ne.0)then
        jerror=jerror+1
        if(jerror.le.4)go to 60
        write(*,*)' '
        write(*,*)'Too many input errors in a row!'
      endif
c
c  Consider ending this help session.
c
      if((ctemp.eq.'!'.and.level.eq.1).or.jerror.gt.4)then
        close(unit=iunit)
        call numfil(ierror,iunit,'close')
        return
      endif
c
c  User wants to back up to a supertopic.  We must rewind.
c
      rewind iunit

      levelo=level
      if(ctemp.eq.'!')then
        level=level-1
        iline=levelm(level)
c
c  Redisplay current topic.
c
      elseif(ctemp.eq.'?')then
        go to 10
c
c  User wants to go down to a subtopic.
c
      else

        do i=1,ntop
          ctemp2=levelc(i)
          call chrdb2(ctemp2)
          itop=i
          if(leqi(ctemp(1:lenc),ctemp2(1:lenc)))go to 90
        enddo

        write(*,*)'Sorry, no help available on '//choice

        jerror=jerror+1
        go to 60
90      continue
        level=level+1
        iline=levelt(itop)
      endif

      go to 10
c
c  Error opening help file.
c
100   continue
      ierror=1

      write(*,*)'Error!  Could not open the help file '//filhlp

      call numfil(ierror,iunit,'close')

      return
c
c  Error reading help file.
c
110   continue
      ierror=1

      write(*,*)' '
      write(*,*)'HLPVMS - Error!'
      write(*,*)'  End of input or error while reading '//
     &  filhlp

      close(unit=iunit)
      call numfil(ierror,iunit,'close')

      return
      end
      subroutine init(chscor,classn,filhlp,filinp,fillpt,filnam,filnew,
     &  filold,filsav,ierror,isect,issn,istat,labtst,
     &  mxstud,mxtest,names,nline,nostcl,nsect,nstud,ntest,scomax,
     &  scores,ssn,stnumn,stunk,weight)

c*********************************************************************72
c
cc INIT sets the initial values for certain variables.
c
c  FILNEW is the name of the previous version.
c  FILOLD is the name of the backup copy.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  FILHLP
c    Character*80 FILHLP.
c    FILHLP is the name of the help file, which is probably called
c    'grader.hlp'.  
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NOSTCL 
c    Integer NOSTCL(MXTEST).
c    NOSTCL(I) is the number of students with valid scores for 
c    test I.
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      character*80 filhlp
      character*60 filinp
      character*60 fillpt
      character*60 filnam
      character*60 filnew
      character*60 filold
      character*60 filsav
      integer i
      integer ierror
      integer isect(mxstud)
      integer issn
      integer istat
      integer j
      character*8 labtst(mxtest)
      character*20 names(mxstud)
      integer nline
      integer nostcl(mxtest)
      integer nsect
      integer nstud
      integer ntest
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      real stnumn(mxstud)
      real stunk(mxstud)
      real weight(mxtest)
c
      do i=1,mxstud
        do j=1,mxtest
          chscor(i,j)=' '
        enddo
      enddo

      classn='Elementary Uda Doodling'
      filhlp='grader.hlp'
      filinp='grader.inp'
      fillpt='grader.lpt'
      filnam='grader.nam'
      filnew='grader.new'
      filold='grader.old'
      filsav='grader.sav'
      ierror=0

      do i=1,mxstud
        isect(i)=1
      enddo

      issn=0
      istat=0

      do i=1,mxtest
        labtst(i)=' '
      enddo

      do i=1,mxstud
        names(i)=' '
      enddo

      nline=0

      do i=1,mxtest
        nostcl(i)=0
      enddo

      nsect=1
      nstud=0
      ntest=0

      do i=1,mxtest
        scomax(i)=100.0
      enddo

      do i=1,mxstud
        do j=1,mxtest
          scores(i,j)=0.0
        enddo
      enddo

      do i=1,mxstud
        ssn(i)='000-00-0000'
      enddo

      do i=1,mxstud
        stnumn(i)=0.0
      enddo

      do i=1,mxstud
        stunk(i)=0.0
      enddo

      do i=1,mxtest
        weight(i)=0.0
      enddo

      return
      end
      subroutine iswap(ia,ib)

c*********************************************************************72
c
cc ISWAP swaps two integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      integer ia
      integer ib
      integer ic
c
      ic=ia
      ia=ib
      ib=ic

      return
      end
      function lenchr(string)

c*********************************************************************72
c
cc LENCHR returns the length of STRING up to the last nonblank character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input, CHARACTER*(*) STRING.
c    STRING is the string to be measured.
c
c  LENCHR 
c    Output, INTEGER LENCHR.
c    LENCHR is the location of the last nonblank character in STRING.
c
      integer i
      integer lchar
      integer lenchr
      integer nchar
      character*(*) string
c
      intrinsic char
      intrinsic len
c
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
cc LEQI compares two strings for equality, disregarding case.
c
c  Thus, LEQI('Anjana','ANJANA') is .TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRNG1,
c  STRNG2 
c    Input, CHARACTER*(*) STRNG1, STRNG2.
c    STRNG1 and STRNG2 are the strings to compare.
c
c  LEQI   
c    Output, LOGICAL LEQI, the result of the comparison.
c
      integer i
      integer len1
      integer len2
      integer lenchr
      logical leqi
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2
c
      external lenchr
c
      len1=lenchr(strng1)
      len2=lenchr(strng2)

      if(len1.ne.len2)then
        leqi=.false.
        return
      endif

      leqi=.false.
      do i=1,len1
        s1=strng1(i:i)
        s2=strng2(i:i)
        call capchr(s1)
        call capchr(s2)
        if(s1.ne.s2)return
      enddo

      leqi=.true.

      return
      end
      function malpha(string)

c*********************************************************************72
c
cc MALPHA is TRUE if a string is purely alphabetic.
c
c  The routine returns .TRUE. if STRING contains only alphabetic
c  characters, (A through Z and a through z), and .FALSE.
c  otherwise.
c
c  Note that MALPHA MUST be declared LOGICAL in your calling
c  program.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  STRING 
c    Input, CHARACTER*(*) STRING.
c    STRING is the string to be checked.
c
c  MALPHA 
c    Output, LOGICAL MALPHA.
c    MALPHA is .TRUE. if STRING contains only
c    alphabetic characters, .FALSE. otherwise.
c
      integer i
      integer itemp
      integer lenc
      logical malpha
      character*(*) string
c
      intrinsic ichar
      intrinsic len
      intrinsic lgt
      intrinsic llt
c
      lenc=len(string)

      malpha=.false.

      do i=1,lenc

        itemp=ichar(string(i:i))
        if(.not.((itemp.ge.65.and.itemp.le.90).or.
     &           (itemp.ge.97.and.itemp.le.122)))return

      enddo

      malpha=.true.

      return
      end
      subroutine modify(ierror,isect,issn,labtst,mxstud,mxtest,names,
     &  nsect,nstud,ntest,scores,ssn)

c*********************************************************************72
c
cc MODIFY can change any item related to a given student.
c
c  The routine allows the user to change the name, SSN, section number, or
c  a test score of a particular student.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IERROR
c    Integer IERROR.
c    IERROR is an error flag, which is 0 if no error occurred during
c    the modifications, and 1 otherwise.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c    However, if SCORES(I,J) is negative, then the score is not
c    to be counted.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
      integer mxstud
      integer mxtest
c
      integer i
      integer ierror
      integer ihi
      integer ilo
      character*8 isay
      integer isect(mxstud)
      integer issn
      integer istud
      integer itemp
      integer itest
      integer j
      character*20 key
      character*8 labtst(mxtest)
      integer lenchr
      integer lenkey
      integer lennam
      logical leqi
      integer match1
      character*20 names(mxstud)
      character*20 namtmp
      integer nsect
      integer nstud
      integer ntest
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      character*20 ssntmp
      real temp
c
      external lenchr
      external leqi
c
c  Get a search key for the student's name.
c
10    continue

      write(*,*)'Enter search key (return to quit)'
      read(*,'(a)')key
      call chrdb1(key)
      lenkey=lenchr(key)
      if(lenkey.le.0)return

      call capchr(key)
c
c  Search for a similar name
c
      istud=0

20    continue

      istud=istud+1

      if(istud.gt.nstud)then
        write(*,*)' '
        write(*,*)'End of search.'
        return
      endif

      namtmp=names(istud)
      call chrdb1(namtmp)
      lennam=lenchr(namtmp)
      call capchr(namtmp)

      if(lennam.lt.lenkey)go to 20

      call compar(namtmp,lennam,key,lenkey,match1)

      if(match1.ne.lenkey)go to 20
c
c  Perfect match
c
30    continue

      write(*,'(1x,i3,''.'',i1,2x,a20)')
     &  istud,isect(istud),names(istud)

      if(issn.ne.0)then
        write(*,'(8x,a20)')ssn(istud)
      endif

      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        write(*,'(5(1x,a8))')(labtst(i),i=ilo,ihi)
        write(*,'(5(1x,f8.1))')(scores(istud,i),i=ilo,ihi)
      enddo

      write(*,*)
     &  'Enter label of test, or N=name #=SSN S=Sec D=delete Q=quit'
      read(*,'(a)')isay

      if(ierror.ne.0)return
c
c  D: User wants record deleted.
c
      if(leqi(isay,'d'))then

        write(*,*)' '
        write(*,*)'MODIFY - Note.'
        write(*,*)'  Deleting student '//names(istud)

        do i=istud,nstud-1
          names(i)=names(i+1)
          isect(i)=isect(i+1)
          ssn(i)=ssn(i+1)

          do j=1,ntest
            scores(i,j)=scores(i+1,j)
          enddo

        enddo
 
        nstud=nstud-1
c
c  N: Change student name.
c
      elseif(leqi(isay,'n'))then

        write(*,*)'Enter correct name:'
        read(*,'(a)')namtmp
        names(istud)=namtmp
c
c  #: Change SSN.
c
      elseif(isay.eq.'#')then

        write(*,*)'Enter correct SSN'
        read(*,'(a)')ssntmp
        ssn(istud)=ssntmp
c
c  S: Change section.
c
      elseif(leqi(isay,'S'))then

        write(*,*)'Enter correct section'
        read(*,*)itemp
        if(itemp.gt.0.and.itemp.le.nsect)then
          isect(istud)=itemp
        else
          write(*,*)' '
          write(*,*)'MODIFY - Error!'
          write(*,*)'  The section number must be at least 1'
          write(*,*)'  and no more than NSECT=',nsect
        endif
c
c  Q: Quit
c
      elseif(leqi(isay,'Q'))then

        return
c
c  Test label?
c
      else

        itest=0

        call flushr(isay)

        do i=1,ntest

          if(leqi(isay,labtst(i)))then
            itest=i
          endif
        enddo

        if(itest.eq.0)then
          write(*,*)' '
          write(*,*)'MODIFY - Error.'
          write(*,*)'  Modify did not understand the command '//isay
          go to 30
        endif

        write(*,*)'Enter score for '//labtst(itest)
        read(*,*)temp
        scores(istud,itest)=temp

        write(*,'(1x,i3,''.'',i1,2x,a20)')
     &    istud,isect(istud),names(istud)

        if(issn.ne.0)then
          write(*,'(8x,a20)')ssn(istud)
        endif

        do ilo=1,ntest,5
          ihi=min(ntest,ilo+4)
          write(*,'(5(1x,a8))')(labtst(i),i=ilo,ihi)
          write(*,'(5(1x,f8.1))')(scores(istud,i),i=ilo,ihi)
        enddo

        return

      endif
 
      go to 30

      end
      subroutine numfil(ierror,iunit,op)

c*********************************************************************72
c
cc NUMFIL keeps track of FORTRAN file unit numbers.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  IERROR 
c    Output, INTEGER IERROR.
c    IERROR is an error flag.
c
c    IERROR is zero if no error occurred.
c
c    If OP was 'OPEN', then
c
c      IERROR=1 means there was no free unit number found.
c      This is a highly unlikely error.
c
c    If OP was 'CLOSE', then
c
c      IERROR=2 means that the given unit was not recorded as
c      being open.
c
c    If OP was 'QUERY', then
c
c      IERROR=3 means that the given unit number is outside 
c      the legal range.
c
c    IERROR=4 means that OP was neither 'OPEN' nor 'CLOSE'
c    nor 'QUERY'.
c
c  IUNIT  
c    Input/output, INTEGER IERROR.
c    IUNIT is a FORTRAN unit number.
c
c    If OP was 'OPEN', then IUNIT is an output quantity,
c    and is a FORTRAN unit number that is free, and may be
c    assigned to a file about to be opened.
c
c    If OP is 'CLOSE', then IUNIT is an input quantity,
c    and is a FORTRAN unit number of a file about to be closed.
c    NUMFIL will note that unit IUNIT is now free.
c
c    If OP is 'QUERY', then IUNIT is an input and output 
c    quantity.  
c
c      On input, IUNIT is a FORTRAN unit number, about which 
c      the user would like to know whether it is open or 
c      closed.
c
c      On output, the value of IUNIT is 1 if the file is open,
c      and 0 if the file is closed.
c
c  OP     
c    Input, CHARACTER*(*) OP.
c
c    'OPEN' means that the user would like a unit number that
c    is currently free, so that a file may be opened with that
c    associated number.
c
c    'CLOSE' means that the user would like to report that the
c    file associated with a given unit number is being closed,
c    so that NUMFIL may update its records.
c
c    'QUERY' means that the user wants to know if a particular
c    unit is being used.
c
      integer i
      integer icall
      integer ierror
      integer iunit
      integer junit(99)
      logical leqi
      character*(*) op
c
      external leqi
c
      save icall
c
      data icall /0/
c
      ierror=0
c
c  On the first call, initialize the unit number records.
c
      if(icall.eq.0)then

        icall=1

        do i=1,99
          junit(i)=0
        enddo
c
c  We assume that units 5 and 6 are reserved by the operating 
c  system.
c  
        junit(5)=1
        junit(6)=1

      endif
c
c  If the user wants to OPEN a file, then search for a free
c  unit number.  If none found, return error.
c
      if(leqi(op,'open'))then

        do i=1,99

          if(junit(i).eq.0)then
            junit(i)=1
            iunit=i
            return
          endif

        enddo

        ierror=1
        iunit=0
c
c  If the user is CLOSING a file, then "retire" the number.
c
      elseif(leqi(op,'close'))then

        if(iunit.ge.1.and.iunit.le.99)then

          if(junit(iunit).eq.0)then
            ierror=1
          else
            junit(iunit)=0
          endif

        endif
c
c  QUERY about whether the given file is open.
c
      elseif(leqi(op,'query'))then

        if(iunit.le.0.or.iunit.gt.99)then
          ierror=3
        else
          iunit=junit(iunit)
        endif
c
c  Unrecognized operation.
c
      else
 
        ierror=4
      endif
          
      return
      end
      subroutine pravds(mxstud,nstud,stnumn)

c*********************************************************************72
c
cc PRAVDS prints out the distribution of total scores.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  STNUMN 
c    Real STNUMN(MXSTUD).
c    For each student, STNUMN(I) is the student's numeric mean,
c    or averaged grade for the class.
c
      integer nbin
      parameter (nbin=10)
c
      integer mxstud
c
      real bottom
      real hi
      integer i
      integer ik
      integer indx
      integer ip(nbin)
      integer nstud
      real stmax
      real stmin
      real stnumn(mxstud)
c
c  Set the IP vector.
c
      stmin=0.0
      stmax=100.0

      do i=1,nbin
        ip(i)=0
      enddo
 
      do i=1,nstud

        if(stnumn(i).gt.0.0)then

          indx = 1+nbin*(stnumn(i)-stmin)/(stmax-stmin)

          if(indx.lt.1)indx=1
          if(indx.gt.nbin)indx=nbin

          indx=nbin+1-indx

          ip(indx)=ip(indx)+1

        endif

      enddo
c
      write(*,*)' '
      write(*,*)'PRAVDS:'
      write(*,*)'  Print the average distribution:'
      write(*,*)' '
      write(*,*)'------ Range ------ ##   Graph '
      write(*,*)' '

      do i=1,nbin

        hi=((nbin-i+1)*stmax+(i-1)*stmin)/nbin
        bottom=((nbin-i)*stmax+i*stmin)/nbin

        write(*,'(f8.1,'' to '',f8.1,i3,3x,60a1)')bottom,hi,ip(i),
     &    ('*',ik=1,ip(i))

      enddo

      return
      end
      subroutine print(chscor,classn,labtst,mxstud,mxtest,names,nstud,
     &  ntest,scomax,weight)

c*********************************************************************72
c
cc PRINT displays a copy of the current grade file to the screen.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      integer i
      integer j
      character*8 labtst(mxtest)
      integer lenc
      integer lenchr
      character*20 names(mxstud)
      integer nstud
      integer ntest
      real scomax(mxtest)
      real weight(mxtest)
c
      write(*,*)' '
      lenc=lenchr(classn)
      write(*,*)classn(1:lenc)
      write(*,*)' '
      write(*,*)'There are ',nstud,' students.'
      write(*,*)'There are ',ntest,' scores recorded.'
      write(*,*)' '
      write(*,*)' '
      write(*,'(9a8)')(labtst(j),j=1,ntest)
      write(*,*)' '
      write(*,'(a)')'Maximum:  '
      write(*,'(9f8.1)')(scomax(j),j=1,ntest)
      write(*,'(a)')'Weight:   '
      write(*,'(9f8.1)')(weight(j),j=1,ntest)
      write(*,*)' '
 
      do i=1,nstud
        write(*,*)' '
        write(*,'(a20)')names(i)
        write(*,'(9a8)')(chscor(i,j),j=1,ntest)
      enddo
 
      write(*,*)' '

      return
      end
      subroutine prrank(labtst,mxstud,mxtest,names,nstud,ntest,scores)

c*********************************************************************72
c
cc PRRANK prints out the students in order of their average.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c    Two special columns are labeled '   Total' and ' Average'.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c    However, if SCORES(I,J) is negative, then the score is not
c    to be counted.
c
      integer mxstud
      integer mxtest
c
      integer i
      integer indx
      integer isgn
      integer itemp
      integer j
      integer jave
      integer jpos
      integer jtot
      character*8 labtst(mxtest)
c
c  FORTRAN 90 array.
c
c     integer list(mxstud)
      integer list(1000)
      integer m
      integer mm
      character*20 names(mxstud)
      integer nstud
      integer ntest
      real scores(mxstud,mxtest)
c
c  FORTRAN 90 array.
c
c     real stunk(mxstud)
      real stunk(1000)
      real temp
c
c  Figure out which column is averages, and which column is totals.
c
      jave=0
      jtot=0
      do j=1,ntest
        if(labtst(j).eq.' Average')then
          jave=j
        endif
        if(labtst(j).eq.'Possible')then
          jpos=j
        endif
        if(labtst(j).eq.'   Total')then
          jtot=j
        endif
      enddo

      if(jave.eq.0.or.jtot.eq.0)then
        write(*,*)' '
        write(*,*)'PRRANK - Fatal error.'
        write(*,*)'  Cannot find Average or Total columns.'
        write(*,*)'  Perhaps you need to grade first!'
        return
      endif
c
c  Sort student means for test averages.
c
      do i=1,nstud
        list(i)=i
        stunk(i)=scores(i,jave)
      enddo

      i=0
      indx=0
      isgn=0
      j=0

10    continue

      call exheap(nstud,indx,i,j,isgn)

      if(indx.lt.0)then
        isgn=-1
        if(stunk(j).gt.stunk(i))isgn=1
        go to 10
      elseif(indx.gt.0)then
        temp=stunk(i)
        stunk(i)=stunk(j)
        stunk(j)=temp
        itemp=list(i)
        list(i)=list(j)
        list(j)=itemp
        go to 10
      endif
c
c  Print out the names, totals, and averages, in order.
c
      write(*,*)' '
      write(*,*)'PRRANK:'
      write(*,*)'  Print the students sorted by term average:'
      write(*,*)' '
      write(*,*)'Rank  Name                      '//
     &  'Total    Possible  Average'
      write(*,*)' '

      do m=1,nstud
        mm=list(m)
        write(*,'(i5,2x,a20,2x,f8.1,2x,f8.1,2x,f8.1)')m,names(mm),
     &    scores(mm,jtot),scores(mm,jpos),scores(mm,jave)
      enddo

      return
      end
      subroutine reader(chscor,classn,filnew,ierror,isect,issn,istat,
     &  labtst,mxstud,mxtest,names,nsect,nstud,ntest,scomax,scores,
     &  ssn,weight)

c*********************************************************************72
c
cc READER reads in the data from a grade file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      character*60 filnew
      integer i
      integer ierror
      integer ihi
      integer ilo
      integer isect(mxstud)
      integer issn
      integer istat
      character*40 item
      integer iunit
      integer j
      integer jhi
      integer jlo
      character*8 labtst(mxtest)
      integer lchar
      integer lenchr
      integer mstud
      character*20 names(mxstud)
      integer nsect
      integer nstud
      integer ntest
      real rval
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      real weight(mxtest)
c
c  Get a free unit number.
c
      call numfil(ierror,iunit,'open')

      if(ierror.ne.0)then
        write(*,*)' '
        write(*,*)'READER - Serious error.'
        write(*,*)'  Could not find a free unit number.'
        return
      endif
c
c  Open file FILNEW on unit 1
c
      open(unit=iunit,file=filnew,status='old',err=50)
      rewind iunit
c
c  Read:
c    Class title
c    Number of students
c    Number of sections
c    Number of tests
c    Test labels
c    Test maximums
c    Test weights
c
      item='Class name'
      read(iunit,'(a50)',end=50,err=50)classn
      item='Number of students'
      read(iunit,*,end=50,err=50)nstud
      item='Number of sections'
      read(iunit,*,end=50,err=50)nsect
      item='SSN switch'
      read(iunit,*,end=50,err=50)issn
      item='Number of tests.'
      read(iunit,*,end=50,err=50)ntest

      item='Test labels'
      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        read(iunit,'(5(a8,1x))',end=50,err=50)(labtst(i),i=ilo,ihi)
      enddo

      item='Maximum scores'
      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        read(iunit,*,end=50,err=50)(scomax(i),i=ilo,ihi)
      enddo

      item='Test weights'
      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        read(iunit,*,end=50,err=50)(weight(i),i=ilo,ihi)
      enddo
c
c  For each student, read
c    Section and name,
c    SSN
c    Scores.
c
      i=1

      write(item,'(''Data for student '',i4)')i
20    continue

      read(iunit,'(i2,1x,a20)',end=40,err=50)isect(i),names(i)
      read(iunit,'(a20)',end=40,err=50)ssn(i)
 
      do jlo=1,ntest,5
        jhi=min(ntest,jlo+4)
        read(iunit,'(5(a8,1x))',end=40,err=50)(chscor(i,j),j=jlo,jhi)
      enddo
 
      i=i+1
      go to 20
c
c  (Expected) end-of-file.
c
40    continue

      mstud=i-1
c
c  Close the file and release unit number.
c
      close(unit=iunit)
      call numfil(ierror,iunit,'close')

      if(ierror.ne.0)then
        write(*,*)' '
        write(*,*)'READER - Warning!'
        write(*,*)'  NUMFIL returned an error when releasing the unit.'
      endif
c
c  Compare the reported number of students with actual.
c
      istat=2

      if(nstud.ne.mstud)then
        write(*,*)' '
        write(*,*)'READER - Warning!'
        write(*,*)'  The file claims to record ',nstud,' students'
        write(*,*)'  but seems actually to have ',mstud
        write(*,*)'  This file should be checked!'
        nstud=mstud
      endif
c
c  Convert the character scores CHSCOR to numeric SCORES, where possible.
c  If CHSCOR does not represent a number, set SCORES to -1.
c
      do i=1,nstud
        do j=1,ntest

          call chrctr(chscor(i,j),rval,ierror,lchar)

          if(ierror.eq.0.and.lchar.gt.0)then
            scores(i,j)=rval
          else
            scores(i,j)=-1.0
          endif

        enddo
      enddo

      lchar=lenchr(filnew)

      write(*,*)' '
      write(*,*)'READER - Note:'
      write(*,*)'  Was able to read the grade file '//filnew(1:lchar)

      return
c
c  End-of-file on reading class name
c
50    continue

      write(*,*)' '
      write(*,*)'READER - Fatal error!'
      write(*,*)'  Error while reading '//item

      istat=0
      ierror=2

      close(unit=iunit)
      call numfil(ierror,iunit,'close')

      return
      end
      subroutine rename(fcopy,forig,ierror)

c*********************************************************************72
c
cc RENAME copies file FORIG into FCOPY, and deletes FORIG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  FCOPY
c    Input, CHARACTER*60 FCOPY.
c    FCOPY is the name of the copy file to be made.
c
c  FORIG
c    Input, CHARACTER*60 FORIG.
c    FORIG is the name of the original file to be copied.
c
c  IERROR
c    Output, INTEGER IERROR.
c    0, no error occurred.
c    1, FCOPY could not be opened.
c    2, FORIG could not be opened.
c
      character*60 fcopy
      character*60 forig
      integer ierror
      integer iunit1
      integer iunit2
      character*100 line
c
      ierror=0
c
c  Copy old file into new file
c
      call delfil(fcopy)

      call numfil(ierror,iunit1,'open')
      call numfil(ierror,iunit2,'open')

      open(unit=iunit1,file=forig,status='old',err=40)
      open(unit=iunit2,file=fcopy,status='new',err=30)
c
c  Read a line from FORIG, and write it to FCOPY.
c
10    continue
      read(iunit1,'(a100)',end=20,err=20)line
      write(iunit2,'(a100)')line
      go to 10
c
c  Finished copying.
c
20    continue

      close(unit=iunit2)
      call numfil(ierror,iunit2,'close')

      close(unit=iunit1,status='delete')
      call numfil(ierror,iunit1,'close')

      return
c
c  Could not open FCOPY.
c
30    continue

      ierror=1

      close(unit=iunit1)

      call numfil(ierror,iunit1,'close')

      write(*,*)' '
      write(*,*)'RENAME - Warning!'
      write(*,*)'  Could not create the backup file '//fcopy

      return
c
c  Could not open FORIG.
c
40    continue

      ierror=2

      write(*,*)' '
      write(*,*)'RENAME - Fatal error!'
      write(*,*)'  Could not open the original file '//forig

      return
      end
      subroutine roster(classn,ierror,isect,mxstud,names,nsect,nstud)

c*********************************************************************72
c
cc ROSTER makes a class roster, suitable for recording attendance.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  IERROR
c    Output, INTEGER IERROR.
c    IERROR is 0 if no problem occurred, but nonzero if
c    some error occurred, such as a problem with user input.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
      integer mxstud
c
      character*50 classn
      integer i
      integer ibox
      integer ido
      integer ierror
      character*1 isay
      integer isect(mxstud)
      integer istud
      integer j
      integer jbox
      integer jsect
      integer lenc
      integer lenchr
      integer lens
      logical leqi
      character*80 line1
      character*80 line2
      character*80 line3
      character*80 line4
      character*20 names(mxstud)
      integer nsect
      integer nstud
c
      external lenchr
      external leqi
c
c  Return immediately if there aren't any students.
c
      if(nstud.le.0)then
        write(*,*)' '
        write(*,*)'ROSTER - Error!'
        write(*,*)'  There are no students in the file,'
        write(*,*)'  so no roster can be created!'
        return
      endif
c
c  Get the number of rows of boxes.
c
      write(*,*)'Enter number of rows of boxes (0 for none)'
      read(*,*)ibox
      if(ibox.lt.0)return
c
c  Get the number of columns of boxes.
c  (You're going to have to check that this doesn't get too long!)
c
99    continue

      if(ibox.gt.0)then
        write(*,*)'Enter number of columns of boxes'
        read(*,*)jbox
        if(jbox.lt.0)return

        if(28+4*jbox.gt.80)then
          write(*,*)' '
          write(*,*)'ROSTER - Error!'
          write(*,*)'  The number of columns is too large!'
          write(*,*)'  Try a value no greater than 13.'
          go to 99
        endif

      endif

      ido=0
      jsect=0

      if(nsect.gt.1)then

        write(*,*)
     &    'Enter A=class roster, B=by sections, C=single section'
        read(*,'(a)')isay
        lens=lenchr(isay)

        if(lens.le.0)then
          ierror=2
          return
        endif

        if(leqi(isay,'a'))then
          ido=0
          jsect=0
        elseif(leqi(isay,'b'))then
          ido=1
          jsect=0
        elseif(leqi(isay,'c'))then
          ido=2
          write(*,*)'Enter section number'
          read(*,*)jsect
        else
          ierror=1
          return
        endif

      endif
c
c  Set up characters to print.
c
      if(ibox.gt.0)then

        line1='                           |'
        line2='---------------------------+'
        line3='|'
        line4='                           +'

        do i=1,jbox
          call chrcat(line1,'   |',line1)
          call chrcat(line2,'---+',line2)
          call chrcat(line3,'   |',line3)
          call chrcat(line4,'---+',line4)
        enddo

      else
        line1=' '
        line2=' '
        line3=' '
        line4=' '
      endif
c
c  Loop for roster
c
10    continue
      if(ido.eq.1)jsect=jsect+1
      write(*,*)' '
      lenc=lenchr(classn)
      write(*,'(a)')classn(1:lenc)
20    continue
      write(*,*)' '

      if(ido.eq.1.or.ido.eq.2)then
        write(*,*)'Section ',jsect
      endif

      write(*,*)' '

      if(ibox.gt.0)then
        write(*,'(27x,35(1x,i2,1x))')(i,i=1,jbox)
        write(*,*)' '
      endif
 
      istud=0
 
      do i=1,nstud

        if(ido.eq.0.or.isect(i).eq.jsect)then

          istud=istud+1
          if(ibox.gt.0)then
            lenc=lenchr(line2)
            write(*,'(a)')line2(1:lenc)
          endif

          if(nsect.le.1.or.ido.ne.0)then
            write(*,'(i3,''.'',3x,a20,a41)')istud,names(i),line3
          else
            write(*,'(i3,''.'',i1,2x,a20,a41)')
     &        istud,isect(i),names(i),line3
          endif

          do j=1,ibox-1
            lenc=lenchr(line4)
            write(*,'(a)')line4(1:lenc)
            lenc=lenchr(line1)
            write(*,'(a)')line1(1:lenc)
          enddo

        endif

      enddo
 
      if(ibox.gt.0)then
        lenc=lenchr(line2)
        write(*,'(a)')line2(1:lenc)
      endif
 
      if(ido.eq.1.and.jsect.lt.nsect)go to 10
 
      return
      end
      subroutine rswap(a,b)

c*********************************************************************72
c
cc RSWAP swaps two real values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      real a
      real b
      real c
c
      c=a
      a=b
      b=c

      return
      end
      subroutine sort(isect,mxstud,mxtest,names,nsect,nstud,
     &  ntest,scores,ssn)

c*********************************************************************72
c
cc SORT sorts the student records by name or test score.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c    However, if SCORES(I,J) is negative, then the score is not
c    to be counted.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
      integer mxstud
      integer mxtest
c
      character*20 ctemp
      integer i
      integer ido
      integer idosec
      integer indx
      character*1 isay
      integer isect(mxstud)
      integer isgn
      integer itemp
      integer j
      integer k
      logical leqi
      character*20 names(mxstud)
      integer nsect
      integer nstud
      integer ntest
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      character*20 ssntmp
      real temp
c
      external leqi
      intrinsic lle
c
      if(nstud.le.1)then
        write(*,*)' '
        write(*,*)'SORT - Note:'
        write(*,*)'  There is no more than 1 student,'
        write(*,*)'  so no sorting is necessary.'
        return
      endif
c
c  By name or by score?
c
      write(*,*)'Enter 0 to sort by name, >0 sort by test number.'
      read(*,*)ido
      if(ido.lt.0)ido=0

      if(ido.gt.ntest)ido=ntest
c
c  By section or whole class?
c
      idosec=0

      if(nsect.gt.1)then
        write(*,*)'Enter "Y" to subsort by section.'
        read(*,'(a)')isay
        if(leqi(isay,'y'))idosec=1
      endif
c
c  External heap sort
c
      i=0
      j=0
      isgn=0
      indx=0

10    continue

      call exheap(nstud,indx,i,j,isgn)
c
c  INDX<0, Compare items I and J.  If I is before J, set ISGN negative.
c
      if(indx.lt.0)then

        if(idosec.eq.1)then

          if(isect(j).lt.isect(i))then
            isgn=1
          elseif(isect(j).gt.isect(i))then
            isgn=-1
          else
            isgn=1

            if(ido.gt.0)then
              if(scores(i,ido).gt.scores(j,ido))isgn=-1
            else
              if(lle(names(i),names(j)))isgn=-1
            endif

          endif

        elseif(ido.eq.0)then
          isgn=1
          if(lle(names(i),names(j)))isgn=-1
        else
          isgn=1
          if(scores(i,ido).gt.scores(j,ido))isgn=-1
        endif

        go to 10
c
c  INDX>0, Interchange items I and J
c
      elseif(indx.gt.0)then

        ctemp=names(i)
        names(i)=names(j)
        names(j)=ctemp
        ssntmp=ssn(i)
        ssn(i)=ssn(j)
        ssn(j)=ssntmp
        itemp=isect(i)
        isect(i)=isect(j)
        isect(j)=itemp

        do k=1,mxtest
          temp=scores(i,k)
          scores(i,k)=scores(j,k)
          scores(j,k)=temp
        enddo

        go to 10
      endif

      write(*,*)' '
      write(*,*)'Sorting completed.'

      return
      end
      subroutine test(isect,labtst,mxstud,mxtest,names,nsect,nstud,
     &  ntest,scomax,scores,weight)

c*********************************************************************72
c
cc TEST records the grades of all the students for a new test.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chrtmp
      integer i
      integer isect(mxstud)
      integer istud
      integer itest
      character*8 labtst(mxtest)
      integer lenchr
      logical leqi
      character*20 names(mxstud)
      integer nsect
      integer nstud
      integer ntest
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      real temp
      real weight(mxtest)
c
      external lenchr
      external leqi
c
c  Get the test number
c
      itest=ntest+1

      if(ntest+1.gt.mxtest)then
        write(*,*)' '
        write(*,*)'TEST - Fatal error!'
        write(*,*)'  Limit of ',mxtest,' tests reached.'
        write(*,*)'  No more tests can be added!'
        return
      endif

      ntest=ntest+1
 
      do i=1,nstud
        scores(i,ntest)=0.0
      enddo
c
c  Get LABTST(ITEST), the test label.
c
      labtst(itest)=' '
      write(*,*)' '
      write(*,*)'Enter a title for this assigment (HW, Lab, Test...)'
      read(*,'(a)',end=20,err=20)chrtmp
 
      call flushr(chrtmp)
 
      labtst(itest)=chrtmp
c
c  Get SCOMAX(ITEST), the test maximum.
c
      scomax(itest)=100.0
      write(*,*)' '
      write(*,*)'Enter the maximum score for '//labtst(itest)
      read(*,*,end=20,err=20)temp

      if(temp.gt.0.0)then
        scomax(itest)=temp
      endif 
c
c  Get WEIGHT(ITEST), the test weight.
c
      weight(itest)=1.0
      write(*,*)' '
      write(*,*)'Enter the weight for '//labtst(itest)
      read(*,*,end=20,err=20)temp

      if(temp.ge.0.0)then
        weight(itest)=temp
      endif 
c
c  For each student, present record
c
      do istud=1,nstud

10      continue

        if(nsect.gt.1)then
          write(*,'(1x,i3,''.'',i1,2x,a20)')
     &      istud,isect(istud),names(istud)
        else
          write(*,'(1x,i3,''.'',3x,a20)')istud,names(istud)
        endif

        read(*,*,end=20,err=10)temp

        scores(istud,itest)=temp

      enddo

      return
c
c  Errors on read.
c
20    continue
      write(*,*)' '
      write(*,*)'TEST - Error!'
      write(*,*)'  Returning to main program.'
      return
      end
      subroutine writer(chscor,classn,filnew,ierror,isect,issn,labtst,
     &  mxstud,mxtest,names,nsect,nstud,ntest,scomax,scores,ssn,weight)

c*********************************************************************72
c
cc WRITER writes out a copy of the student information.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 January 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  CHSCOR
c    Character*8 CHSCOR(MXSTUD,MXTEST).
c    CHSCOR contains a character representation of the student's 
c    numerical score, or non-numerical codes like '*'.  A non-numerical
c    code means that the score will be skipped during grading.
c
c  CLASSN
c    Character*50 CLASSN.
c    CLASSN is a name or title for the class.
c
c  FILNEW
c    Character*80 FILNEW.
c    FILNEW is the name of the file into which the new data is
c    stored after each command that changes grade information.
c    This file is usually called 'grader.new'.
c
c  IERROR.
c    Integer IERROR.
c    IERROR is 0 if no error occurred.
c    IERROR is nonzero if the file could not be created.
c
c  ISECT
c    Integer ISECT(MXSTUD).
c    For each student I, ISECT(I) is the student's section.
c
c  ISSN
c    Integer ISSN.
c    ISSN is 0 if social security numbers are not stored, 
c    and 1 if they are.
c
c  LABTST
c    Character*8 LABTST(MXTEST).
c    For each test I, LABTST is a short descriptive label for the test.
c
c  MXSTUD
c    Integer MXSTUD.
c    MXSTUD is the maximum number of students the program can handle.
c
c  MXTEST
c    Integer MXTEST.
c    MXTEST is the maximum number of tests the program can handle.
c
c  NAMES
c    Character*20 NAMES(MXSTUD).
c    The name of student I is NAMES(I).
c
c  NSECT
c    Integer NSECT.
c    NSECT is the number of sections of the class.
c
c  NSTUD
c    Integer NSTUD.
c    NSTUD is the number of students enrolled in the class.
c
c  NTEST
c    Integer NTEST.
c    NTEST is the number of graded assignments given.
c
c  SCOMAX
c    Real SCOMAX(MXTEST).
c    SCOMAX(I) is the maximum possible score for test I.
c
c  SCORES
c    Real SCORES(MXSTUD,MXTEST)
c    SCORES(I,J) is the score that student I got on test J.
c
c  SSN
c    Character*20 SSN(MXSTUD).
c    SSN(I) is the social security number of student I.
c
c  WEIGHT
c    Real WEIGHT(MXTEST).
c    WEIGHT(I) is the weight assigned to test I.
c
      integer mxstud
      integer mxtest
c
      character*8 chrtmp
      character*8 chscor(mxstud,mxtest)
      character*50 classn
      character*60 filnew
      integer i
      integer ierror
      integer ihi
      integer ilo
      integer isect(mxstud)
      integer issn
      integer iunit
      integer j
      integer jhi
      integer jlo
      character*8 labtst(mxtest)
      integer lenc
      integer lenchr
      character*20 names(mxstud)
      integer nsect
      integer nstud
      integer ntest
      real scomax(mxtest)
      real scores(mxstud,mxtest)
      character*20 ssn(mxstud)
      real weight(mxtest)
c
c  Convert numeric scores to characters.
c
      do i=1,mxstud
        do j=1,mxtest
          if(scores(i,j).ge.0.0)then
            write(chrtmp,'(f8.1)')scores(i,j)
            chscor(i,j)=chrtmp
          endif
        enddo
      enddo
c
c  Get a free unit number,
c  delete the old version of the file, if any,
c  open the new version.
c
      call numfil(ierror,iunit,'open')

      call delfil(filnew)

      open(unit=iunit,file=filnew,status='new',err=40)
c
c  Write:
c    Class title;
c    Number of students;
c    Number of sections;
c    Number of tests;
c    Test labels;
c    Maximum test scores;
c    Test weights;
c
      write(iunit,'(a50)')classn
      write(iunit,*)nstud,' students'
      write(iunit,*)nsect,' sections'
      write(iunit,*)issn,', 0=SSN not stored, 1=SSN stored.'
      write(iunit,*)ntest,' tests recorded'

      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        write(iunit,'(5(a8,'',''))')(labtst(i),i=ilo,ihi)
      enddo

      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        write(iunit,'(5(f8.1,'',''))')(scomax(i),i=ilo,ihi)
      enddo

      do ilo=1,ntest,5
        ihi=min(ntest,ilo+4)
        write(iunit,'(5(f8.1,'',''))')(weight(i),i=ilo,ihi)
      enddo
c
c  Write each record.
c
      do i=1,nstud
        write(iunit,'(i2,1x,a20)')isect(i),names(i)
        write(iunit,'(a20)')ssn(i)
        do jlo=1,ntest,5
          jhi=min(ntest,jlo+4)
          write(iunit,'(5(a8,'',''))')(chscor(i,j),j=jlo,jhi)
        enddo
      enddo
c
c  Close the file.
c
      close(unit=iunit)
      call numfil(ierror,iunit,'close')

      lenc=lenchr(filnew)

      write(*,*)' '
      write(*,*)'WRITER - Note:'
      write(*,*)'  The current data has been saved in '//filnew(1:lenc)

      return
c
c  Error opening file
c
40    continue

      ierror=1

      write(*,*)' '
      write(*,*)'WRITER - Warning!'
      write(*,*)'  The attempt to write the data to a file failed.'

      call numfil(ierror,iunit,'close')

      return
      end
