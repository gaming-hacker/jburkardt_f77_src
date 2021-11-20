      program main

c*********************************************************************72
c
cc MAIN is the main program for HELP.
c
c  Discussion:
c
c    HELP is a program that navigates a VMS-style help file, allowing
c    the user to select the next topic, or to backup.
c
c    This program was last worked on on 18 July 1996.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
      character*60 filhlp
      integer iounit(4)
      integer lenchr
      logical leqi
      character*80 line
      integer lpage
      integer nline
      character*100 output
      character*80 prompt

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HELP'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  A VMS-style help facility.'
c
c  Initializations.
c
      filhlp='matman.hlp'
      iounit(1)=0
      iounit(2)=0
      iounit(3)=-1
      iounit(4)=-1
      line=' '
      lpage=24
      call setpag(lpage)
      nline=0
c
c  Say hello.
c
      call hello(iounit,output)
c
c  Get next command from user.
c
10    continue

      write(*,*)' '
      write(*,*)'Enter command (H for help)'

      nline=0
      read(*,'(a)')line
      nline=lenchr(line)

      if(leqi(line(1:5),'file='))then
         filhlp=line(6:)
      elseif(leqi(line,'h'))then
        write(*,*)' '
        write(*,*)'FILE=       Name the help file to be read.'
        write(*,*)'H           Print this list of commands.'
        write(*,*)'HELP        Get help on current topic.'
        write(*,*)'HELP topic  Get help from file "topic.hlp".'
        write(*,*)'QUIT        stop.'
      elseif(leqi(line,'help'))then
        call hlpvms(filhlp,iounit,line,nline,output,prompt)
      elseif(leqi(line(1:5),'help '))then
        filhlp=line(6:)
        call hlpvms(filhlp,iounit,line,nline,output,prompt)
      elseif(leqi(line(1:1),'q'))then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HELP'
        write ( *, '(a)' ) '  Normal end of execution.'
        write ( *, '(a)' ) ' '
        call timestamp ( )
        stop
      endif

      go to 10
      end
      subroutine addlin ( )

c*********************************************************************72
c
cc ADDLIN is called whenever a new line is printed.  
c
c  Discussion:
c
c    It simply updates an internal count of the number of lines printed since
c    the last pause.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      integer nline

      nline=0
      call indata('get','nline',nline)
      nline=nline+1
      call indata('set','nline',nline)

      return
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
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, CHARACTER*(*) STRING, the string of characters to be transformed.
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
      subroutine chrchp(string,ilo,ihi)

c*********************************************************************72
c
cc CHRCHP chops a substring from a string.
c
c  Discussion:
c
c    The routine accepts a STRING of characters and removes 
c    positions ILO through IHI, pushes the end of STRING down and 
c    pads with blanks.  
c
c    Using quotes to denote the beginning and end of the string, then 
c    calling CHRCHP with STRING='Fred is not a jerk!' and ILO=9 and 
c    IHI=12 will result in the output STRING='Fred is a jerk!    '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, CHARACTER*(*) STRING, the character string
c    to be transformed.
c
c    Input, INTEGER ILO, the location of the first character 
c    to be removed.
c
c    Input, INTEGER IHI, the location of the last character 
c    to be removed.
c
      character*1 chrtmp
      integer i
      integer ihi
      integer ilo
      integer inew
      integer nchar
      character*(*) string

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
      subroutine chrdb2(string)

c*********************************************************************72
c
cc CHRDB2 replaces strings of blanks by one blank.
c
c  Discussion:
c
c    The routine accepts a string of characters.  It replaces all nulls 
c    by blanks.  It replaces all strings of consecutive blanks by a 
c    single blank, left justifying the remainder and padding with 
c    blanks.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, CHARACTER*(*) STRING, the string to be transformed.
c
      integer i
      integer j
      integer nchar
      character*1 newchr
      character*1 null
      character*1 oldchr
      character*(*) string

      intrinsic char
      intrinsic len

      nchar=len(string)

      j=0
      null=char(0)
      newchr=' '

      do i=1,nchar

        oldchr=newchr
        if(string(i:i).eq.null)string(i:i)=' '
        newchr=string(i:i)
        string(i:i)=' '

        if(oldchr.ne.' '.or.
     &     newchr.ne.' ')then
          j=j+1
          string(j:j)=newchr
        endif

      enddo

      return
      end
      subroutine chrinp(ierror,iounit,line,nline,output,prompt)

c*********************************************************************72
c
cc CHRINP manages buffered input from the  user.
c
c  Discussion:
c
c    The routine checks to see whether there is any more information in
c    the buffer array LINE.  If so, it simply updates the prompt
c    and returns.  Otherwise, it prints the prompt string out,
c    reads the input from the user, and reprints the prompt and
c    the user input on those I/O units where it is appropriate.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, INTEGER IERROR.
c    If IERROR is nonzero on input, CHRINP stops.  It is the
c    calling routine's responsibility to make sure IERROR is
c    zero on input.  This is because CHRINP signals problems
c    to the calling routine using IERROR.  If the routine
c    does not take the trouble to reset IERROR, then it
c    is likely not to have addressed the problem itself.
c    These problems can include things like end of input,
c    so a failure to act can be catastrophic.
c    On output,
c    0, if no errors were detected,
c    1, if there was an error in the read,
c    2, if there was an end-of-file in the read.
c
c    Input, INTEGER IOUNIT(4).
c    IOUNIT(1) is the FORTRAN input unit.
c    IOUNIT(2) is the standard output unit, while IOUNIT(3) and
c    IOUNIT(4), if nonzero, are auxilliary output units.
c
c    Input/output, CHARACTER*80 LINE.
c    On input, LINE may contain information that the calling
c    program can use, or LINE may be empty.
c    On output, LINE is unchanged if it contained information
c    on input.  But if the input LINE was empty, then the
c    output LINE contains whatever information the user typed.
c
c    Input/output, INTEGER NLINE.
c    On input, if NLINE is zero, CHRINP assumes LINE is
c    empty, and asks the user for more information.
c    If NLINE is greater than zero, than NLINE and LINE
c    are left unchanged.
c    On output, NLINE is reset to the length of LINE.
c
c    Workspace, CHARACTER*100 OUTPUT.
c
c    Input/output, CHARACTER*80 PROMPT.
c    On input, the prompt string to be printed.
c    On output, PROMPT has been blanked out, up to the first comma.
c
      character*6 chrint
      integer i
      integer icomma
      integer ierror
      integer iosave
      integer iounit(4)
      integer lchar
      integer lenchr
      character*80 line
      integer nline
      character*100 output
      character*80 prompt

      external chrint
      intrinsic index
      external lenchr
c
c  Catch nasty errors in calling routines.
c
      if(ierror.ne.0)then
        output='Error!'
        call chrwrt(iounit,output)
        output='Nonzero input value of IERROR='//chrint(ierror)
        call chrdb2(output)
        call chrwrt(iounit,output)
        stop
      endif

10    continue
c
c  If there is nothing in the LINE buffer, then:
c    "turn off" the automatic echo for units between 30 and 39,
c    print the prompt line,
c    "turn on" the automatic echo for units between 30 and 39,
c    read the input line,
c    remove double blanks, 
c    set NLINE to the length of the LINE,
c    don't print a copy of the input on units between 40 and 49.
c
      if(nline.le.0)then

        do i=2,4
          if(iounit(i).ge.30.and.
     &       iounit(i).le.39)iounit(i)=-iounit(i)
        enddo

        lchar=lenchr(prompt)
        if(lchar.gt.0)then
          output='Enter '//prompt(1:lchar)
          call chrwrt(iounit,output)
        endif

        do i=2,4
          if(iounit(i).le.-30.and.
     &       iounit(i).ge.-39)iounit(i)=-iounit(i)
        enddo

        if(iounit(1).le.0)then
          read(*,'(a80)',end=50,err=40)line
        else
          read(iounit(1),'(a80)',end=50,err=40)line
        endif
        call chrdb2(line)
c
c  Don't echo input to IOUNIT(2).
c
        if(iounit(1).lt.40.or.iounit(1).gt.49)then
          iosave=iounit(2)
          if(iounit(1).le.0)iounit(2)=-1
          output=line
          call chrwrt(iounit,output)
          iounit(2)=iosave
        endif

      endif
c
c  Reset NLINE.
c
      nline=lenchr(line)
c
c  If the user typed something in, reset the line position to 0.
c
      if(iounit(1).eq.0)call setlin(0)
c
c  If item was read, remove item from PROMPT list.
c
      if(nline.gt.0)then
        icomma=index(prompt,',')
        if(icomma.gt.0.and.
     &    icomma.lt.80.and.
     &    prompt(icomma+1:icomma+1).eq.' ')icomma=icomma+1
        call chrchp(prompt,1,icomma)
      endif

      return
c
c  Error in input.
c
40    continue
      ierror=1
      output='Error in input format.'
      call chrwrt(iounit,output)
      output='Input line follows:'
      call chrwrt(iounit,output)
      output=line
      call chrwrt(iounit,output)

      if(iounit(1).le.0)then
        nline=0
        go to 10
      endif

      return
c
c  End of input.
c
c  If we are reading from a file, then set IERROR=2 and return.
c  But if we are reading from the user, something is seriously
c  wrong, and we must stop.
c
50    continue
      ierror=2
      nline=0
      output='End of input!'
      call chrwrt(iounit,output)

      if(iounit(1).eq.0)then
        output='The program is being stopped now!'
        call chrwrt(iounit,output)
      else
        close(unit=iounit(1))
        iounit(1)=0
        output='Closing current input file!'
        call chrwrt(iounit,output)
      endif

      return
      end
      function chrint(intval)

c*********************************************************************72
c
cc CHRINT returns a character representation of an integer.
c
c  Discussion:
c
c    The function accepts an integer and returns in CHRINT the 6-character
c    representation of the integer, right justified, or '******' if 
c    the integer is too large or negative to fit in six positions.  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, INTEGER INTVAL, an integer variable to be converted.
c
c    Output (through function value), CHARACTER*6 CHRINT, a 6 
c    character representation of the integer, right justified. 
c    Thus, if INTVAL=1, CHRINT='     1'.  CHRINT must be 
c    declared "CHARACTER CHRINT*6" in the calling program.
c
      character*6 chrint
      character*6 chrtmp
      integer intval

      if(intval.gt.999999)then
        chrtmp='******'
      elseif(intval.lt.-99999)then
        chrtmp='-*****'
      else
        write(chrtmp,'(i6)')intval
      endif
      chrint=chrtmp

      return
      end
      subroutine chrrea(string,line,nline,prompt,iounit,ierror,iterm)

c*********************************************************************72
c
cc CHRREA reads data from a line.
c
c  Discussion:
c
c    The routine accepts LINE, which is assumed to contain NLINE user 
c    input characters, where NLINE may be less than 1, and a PROMPT 
c    line.
c
c    If NLINE is less than 1, the PROMPT is printed and user input 
c    read from IOUNIT(1) into LINE, and NLINE updated.
c
c    In either case, enough characters are read from LINE to fill
c    STRING and the positions read are removed, and NLINE updated.
c
c    PROMPT is also updated.  On satisfactory input of STRING, 
c    everything in PROMPT up to and including the first comma is 
c    removed.
c
c    IOUNIT is assumed to have the following properties, which
c    also apply to routines CHRWRT, CHRINP, RELREA, RELWRT, INTREA 
c    and RATREA:
c
c    IOUNIT(1) represents the input unit.  0 is taken to be the user
c    and we READ(*,format) the input.
c
c    IOUNIT(2) is taken to be a standard output unit.  Input is never
c    echoed to IOUNIT(2), but may be to other units.
c
c    Later units:  If their values is between 30 and 39, user input 
c    is copied to them, but no output.
c    If between 40 and 49, output is copied to them, but no input.
c    If the unit number is negative, no input is read, nor output 
c    written.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, CHARACTER*(*) STRING.
c    The user's response to the PROMPT, as read from LINE.
c
c    Input/output, CHARACTER*80 LINE.
c    A buffer containing the user's input.
c
c    Input/output, INTEGER NLINE.
c    The number of characters of information in LINE.
c
c    Input/output, CHARACTER*80 PROMPT.
c    On input, a prompt string that will be printed if NLINE is
c    not positive.  
c    On output, if STRING has been read, then PROMPT is cleared out
c    up to, and including, the first comma.
c
c    Input, INTEGER IOUNIT(4).
c    IOUNIT(1) is the FORTRAN input unit.
c    IOUNIT(2) is the standard output unit, while IOUNIT(3) and
c    IOUNIT(4), if nonzero, are auxilliary output units.
c
c    Output, integer IERROR, error flag.
c    0, No error occurred.
c    1, Format error during read.
c    2, End of file during read.
c
c    Input, integer ITERM, terminator option.
c    0, No check for terminators.
c    1, Blank, slash, comma, semicolon, equals, greater or
c       lesser signs terminate input.
c    2, Nonalphabetic terminates input
c    3, Nonalphanumeric terminates input
c    4, Blank, slash, comma, semicolon, equals, greater or
c       lesser signs or nonalphabetic characters terminate input.
c            
      character*1 chrtmp
      integer i
      integer ierror
      integer iounit(4)
      integer iterm
      integer lchar
      integer lenchr
      logical let
      character*80 line
      integer nchar
      integer nline
      character*1 null
      logical num
      character*100 output
      character*80 prompt
      character*(*) string

      intrinsic char
      intrinsic len
      external lenchr
      intrinsic lge
      intrinsic lle

      ierror=0
      null=char(0)
      string=' '
      call chrinp(ierror,iounit,line,nline,output,prompt)
      if(ierror.ne.0)return
c
c  Remove double blanks.
c
      if(iterm.eq.2.or.iterm.eq.3)then
        call chrdb2(line)
      endif
c
c  Null input acceptable for character input only.
c
      if(nline.le.0)return
      lchar=0
      nchar=len(string)

      do i=1,nchar

        if(lchar.ne.0)go to 10

        chrtmp=line(i:i)

        if(iterm.eq.1)then

          if(chrtmp.eq.' '.or.
     &       chrtmp.eq.null.or.
     &       chrtmp.eq.'/'.or.
     &       chrtmp.eq.','.or.
     &       chrtmp.eq.';'.or.
     &       chrtmp.eq.'=')lchar=i

        elseif(iterm.eq.2)then

          let=(lge(chrtmp,'a').and.lle(chrtmp,'z')).or.
     &        (lge(chrtmp,'A').and.lle(chrtmp,'Z'))
          if(.not.let)lchar=i

        elseif(iterm.eq.3)then

          let=(lge(chrtmp,'a').and.lle(chrtmp,'z')).or.
     &        (lge(chrtmp,'A').and.lle(chrtmp,'Z'))
          num=lge(chrtmp,'0').and.lle(chrtmp,'9')
          if((.not.let).and.(.not.num))lchar=i

        elseif(iterm.eq.4)then

          let=(lge(chrtmp,'a').and.lle(chrtmp,'z')).or.
     &        (lge(chrtmp,'A').and.lle(chrtmp,'Z'))
          if(.not.let)lchar=i

          if(chrtmp.eq.' '.or.
     &       chrtmp.eq.null.or.
     &       chrtmp.eq.'/'.or.
     &       chrtmp.eq.','.or.
     &       chrtmp.eq.';'.or.
     &       chrtmp.eq.'<'.or.
     &       chrtmp.eq.'>'.or.
     &       chrtmp.eq.'=')lchar=i

        endif

        if(lchar.eq.0)string(i:i)=chrtmp

      enddo

10    continue
c
c  Chop out the character positions that have been used.
c
      if(lchar.eq.0)lchar=nchar
      call chrchp(line,1,lchar)
c
c  Force the string to be flush left by removing leading blanks.
c
      call flushl(line)
c
c  Update the line length.
c
      nline=lenchr(line)

      return
      end
      function chrrel(rval)

c*********************************************************************72
c
cc CHRREL creates a character string representation of a real number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, REAL RVAL, a number.
c
c    Output (through function value), CHARACTER*14 CHRREL,
c    a right-justified character variable containing the
c    representation of RVAL, using a G14.7 format.
c
      character*14 chrrel
      character*14 chrtmp
      real rval
c
c  We can't seem to write directly into CHRREL because of compiler
c  quibbles.
c
      write(chrtmp,'(g14.7)')rval
      chrrel=chrtmp

      return
      end
      subroutine chrwrt(iounit,string)

c*********************************************************************72
c
cc CHRWRT writes a character STRING of characters to one or more output units.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, INTEGER IOUNIT(4).
c    IOUNIT(1) is the FORTRAN input unit.
c    IOUNIT(2) is the standard output unit, while IOUNIT(3) and
c    IOUNIT(4), if nonzero, are auxilliary output units.
c
c    Input, CHARACTER*(*) STRING, the string to be printed.
c
      character*1 cc
      integer i
      integer iounit(4)
      integer k
      integer lchar
      integer lenchr
      integer npage
      character*(*) string

      external lenchr
      external npage
c
c  If output is to the user, rather than to a file, then
c  see if we need to pause for a new page.
c
      if(iounit(2).eq.0.and.npage().gt.0)then
        write(6,*)'(more)'
        read(5,'(a1)',end=10,err=10)
10      continue
      endif

      lchar=lenchr(string)
      if(lchar.le.0)lchar=1

      do i=2,4
        if(iounit(i).eq.0)then
c
c  Use the following line for UNIX machines, and the IBM PC:
c
c         write(6,'(80a1)')(string(k:k),k=1,lchar)
c
c  Use the following lines for Macintosh and VAX/VMS systems:
c
          cc=' '
          write(6,'(a1,80a1)')cc,(string(k:k),k=1,lchar)

        elseif(iounit(i).gt.0)then
          write(iounit(i),'(80a1)')(string(k:k),k=1,lchar)
        endif

      enddo
c
c  Update the line count.
c
      call addlin

      return
      end
      subroutine flushl(string)

c*********************************************************************72
c
cc FLUSHL flushes a string left.  
c
c  Example:
c
c    Input             Output
c
c    '     Hello'      'Hello     '
c    ' Hi there!  '    'Hi there!   '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, CHARACTER*(*) STRING.
c    On input, STRING is a string of characters.
c    On output, any initial blank characters in STRING
c    have been cut, and pasted back onto the end.
c
      integer i
      integer lchar
      integer lenchr
      integer nonb
      character*1 null
      character*(*) string

      intrinsic char
      external lenchr
c
c  Check the length of the string to the last nonblank.
c  If nonpositive, return.
c
      lchar=lenchr(string)
      if(lchar.le.0)return
      null=char(0)
c
c  Find the occurrence of the first nonblank.
c
      do i=1,lchar

        nonb=i
        if(string(i:i).ne.' '.and.
     &     string(i:i).ne.null)go to 10

      enddo

      return

10    continue
c
c  Shift the string left.
c
      do i=1,lchar+1-nonb
        string(i:i)=string(i+nonb-1:i+nonb-1)
      enddo
c
c  Blank out the end of the string.
c
      do i=lchar-nonb+2,lchar
        string(i:i)=' '
      enddo

      return
      end
      subroutine hello(iounit,output)

c*********************************************************************72
c
cc HELLO prints out an initial message to the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, INTEGER IOUNIT(4).
c    IOUNIT(1) is the FORTRAN input unit.
c    IOUNIT(2) is the standard output unit, while IOUNIT(3) and
c    IOUNIT(4), if nonzero, are auxilliary output units.
c
c    Workspace, CHARACTER*80 OUTPUT.
c
      integer iounit(4)
      character*80 output

      output=' '
      call chrwrt(iounit,output)
      output='HELP'
      call chrwrt(iounit,output)
      output='Version 1.05'
      call chrwrt(iounit,output)
      output='Last modified on 18 July 1996'
      call chrwrt(iounit,output)
      output=' '
      call chrwrt(iounit,output)
 
      return
      end
      subroutine hlpvms(filhlp,iounit,line,nline,output,prompt)

c*********************************************************************72
c
cc HLPVMS navigates through a given VMS-style help file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, CHARACTER*60 FILHLP.
c    The name of the help file.
c
c    Input, INTEGER IOUNIT(4).
c    IOUNIT(1) is the FORTRAN input unit.
c    IOUNIT(2) is the standard output unit, while IOUNIT(3) and
c    IOUNIT(4), if nonzero, are auxilliary output units.
c
c    Workspace, CHARACTER*80 LINE.
c    Used to hold the user's input.  
c
c    Input/output, INTEGER NLINE.
c    Keeps track of the number of useful characters in LINE.
c
c    Workspace, CHARACTER*100 OUTPUT.
c
c    Workspace, CHARACTER*80 PROMPT.
c
      integer maxtop
      parameter (maxtop=100)

      character*75 choice
      character*75 ctemp
      character*75 ctemp2
      character*60 filhlp
      integer i
      integer ierror
      integer iline
      character*75 inline
      integer iounit(4)
      integer iterm
      integer itop
      integer jerror
      character*1 lab
      integer lchar
      integer lenc
      integer lenchr
      logical leqi
      integer level
      character*75 levelc(maxtop)
      integer levelm(10)
      integer levelo
      integer levelt(maxtop)
      integer lhunit
      character*80 line
      integer move
      integer nline
      integer ntop
      integer num
      character*100 output
      character*80 prompt

      external lenchr
      external leqi
      intrinsic lge
      intrinsic lle

      ierror=0
      lhunit=55
      call setlin(0)
c
c  Open help file
c
c  These lines work for a "private" copy of MATMAN on VAX/VMS,
c  UNIX, IBM PC or Macintosh
c
      open(unit=lhunit,file=filhlp,status='old',err=100)
c
c  These lines work for a "shared" copy of MATMAN on a VAX/VMS 
c  system:
c
c     open(unit=lhunit,file=filhlp,status='old',err=100,
c    &  shared,readonly)
c
      levelo=0
      level=1
      iline=1
c
c  Move to beginning of current topic by reading MOVE lines from 
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
        read(lhunit,'(1x)',end=110,err=110)
      enddo

      output=' '
      call chrwrt(iounit,output)
      read(lhunit,'(a1,a75)',end=110,err=110)lab,inline
      output=inline
      call chrwrt(iounit,output)
c
c  If 'going down' or redisplaying, (as opposed to backing up), 
c  display information available under the current topic. 
c
c  We stop printing when we hit a numeric label.  
c
c  If this label is less than or equal to current level, there are 
c  no subtopics.  
c
c  Otherwise, we now move ahead to print out the list of subtopics 
c  available for this topic.
c
      if(level.ge.levelo)then
        ntop=-1

30      continue

        read(lhunit,'(a1,a75)',end=50)lab,inline
        move=move+1

        if(lge(lab,'0').and.lle(lab,'9'))then
          read(lab,'(i1)')num
          if(num.le.level)go to 50
          ntop=0
          go to 40
        endif

        output=inline
        call chrwrt(iounit,output)
        go to 30
      else
        ntop=0
        inline=' '
        lab=' '
      endif
c
c  Locate each subtopic by examining column 1, searching for 
c  integer label.
c
c  Assuming we are at level LEVEL, we are searching for labels 
c  equal to LEVEL+1.  As we encounter each such label, we want to 
c  store the rest of the line as a subtopic.  We ignore labels 
c  greater than LEVEL+1 because these are sub-subtopics, and we 
c  cease our search when we reach a label less than or equal to 
c  LEVEL.
c
40    continue

      if(lge(lab,'0').and.lle(lab,'9'))then
        read(lab,'(i1)')num
        if(num.le.level)go to 50
 
        if(num.eq.level+1)then
 
          if(ntop.ge.maxtop)then
            write(*,*)' '
            write(*,*)'HELP - Warning!'
            write(*,*)'  Maximum number of topics reached!'
          else
            ntop=ntop+1
 
            if(ntop.eq.1)then
              output=' '
              call chrwrt(iounit,output)
              output='Help is available on:'
              call chrwrt(iounit,output)
              output=' '
              call chrwrt(iounit,output)
            endif

            output=inline
            call chrwrt(iounit,output)
            levelt(ntop)=move
            levelc(ntop)=inline
          endif
 
        endif
      endif
      read(lhunit,'(a1,a75)',end=50,err=50)lab,inline
      move=move+1
      go to 40

50    continue
c
c  Display subtopics.
c
      output=' '
      call chrwrt(iounit,output)
      output='Return to back up, ? to redisplay.'
      call chrwrt(iounit,output)
c
c  Prompt for user choice of new topic, exit, or back up.
c
60    continue

      ierror=0
      nline=0

      if(ntop.gt.0)then
        prompt='topic you want help on, or RETURN or ?.'
      else
        prompt='RETURN or ?.'
      endif

      iterm=0
      call chrrea(choice,line,nline,prompt,iounit,ierror,iterm)
      if(ierror.ne.0)then
        ierror=0
        close(unit=lhunit)
        return
      endif

      call setlin(0)
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
        output='Too many input errors in a row!'
        call chrwrt(iounit,output)
      endif
c
c  Consider ending this help session.
c
      if((ctemp.eq.'!'.and.level.eq.1).or.jerror.gt.4)then
        close(unit=lhunit)
        return
      endif
c
c  User wants to back up to a supertopic.  We must rewind.
c
      rewind lhunit
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

        lchar=lenchr(choice)
        output='Sorry, no help available on "'//choice(1:lchar)//'".'
        call chrdb2(output)
        call chrwrt(iounit,output)
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
      lchar=lenchr(filhlp)
      output='Could not open the help file "'//filhlp(1:lchar)//'".'
      call chrdb2(output)
      call chrwrt(iounit,output)
      return
c
c  Error reading help file.
c
110   continue
      ierror=1
      lchar=lenchr(filhlp)
      output='Unexpected error while reading "'//filhlp(1:lchar)//'".'
      call chrdb2(output)
      call chrwrt(iounit,output)
      close(unit=lhunit)

      return
      end
      function igcf(i,j)

c*********************************************************************72
c
cc IGCF finds the greatest common factor of I and J.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, INTEGER I, J, two numbers whose greatest
c    common factor is desired.
c
c    Output, INTEGER IGCF, the greatest common factor of I and J.
c    Note that if J is negative, IGCF will also be negative.
c    This is because it is likely that the caller is forming
c    the fraction I/J, and so any minus sign should be 
c    factored out of J.
c    If I and J are both zero, IGCF is returned as 1.
c    If I is zero and J is not, IGCF is returned as J,
c    and vice versa.
c    If I and J have no common factor, IGCF is returned as 1.
c    Otherwise, using the Euclidean algorithm, IGCF is the
c    largest common factor of I and J.
c
      integer i
      integer igcf
      integer ip
      integer iq
      integer ir
      integer j

      intrinsic abs
      intrinsic max
      intrinsic min

      igcf=1
c
c  If both I and J are zero, IGCF is 1.
c
      if(i.eq.0.and.j.eq.0)then
        igcf=1
        return
      endif
c
c  If just one of I and J is zero, IGCF is the other one.
c
      if(i.eq.0)then
        igcf=j
        return
      elseif(j.eq.0)then
        igcf=abs(i)
        return
      endif
c
c  Set IP to the larger of I and J, IQ to the smaller.
c  This way, we can alter IP and IQ as we go.
c
      ip=max(abs(i),abs(j))
      iq=min(abs(i),abs(j))
c
c  Carry out the Euclidean algorithm.
c
10    continue
      ir=mod(ip,iq)

      if(ir.ne.0)then
        ip=iq
        iq=ir
        go to 10
      endif
c
c  Take the sign of J into account.
c
      iq=abs(iq)

      if(j.lt.0)iq=-iq

      igcf=iq

      return
      end
      subroutine indata(op,var,ival)

c*********************************************************************72
c
cc INDATA works like a sort of COMMON block.  
c
c  Discussion:
c
c    It stores or returns
c    the values of certain variables.  Thus, it allows routines
c    to "communicate" without having to have data passed up and 
c    down the calling tree in argument lists.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, CHARACTER*(*) OP, describes the operation to be done.
c    'SET' means set a value.
c    'GET' means get a value.
c
c    Input, CHARACTER*(*) VAR, the name of the variable to be set
c    or gotten.
c    VAR may have the value 'NLINE' or 'LPAGE'.
c
c    Input/output, INTEGER IVAL.
c    If OP is 'SET', then the variable named in VAR is set to the
c    value IVAL.
c    If OP is 'GET', then the value of IVAL is set to the value of
c    the variable named in VAR.
c
      integer ival
      logical leqi
      integer lpage
      integer nline
      character*(*) op
      character*(*) var

      external leqi

      save lpage
      save nline

      data lpage /24/
      data nline /0/

      if(leqi(op,'set').and.leqi(var,'nline'))then
        nline=ival
      elseif(leqi(op,'set').and.leqi(var,'lpage'))then
        lpage=ival
      elseif(leqi(op,'get').and.leqi(var,'nline'))then
        ival=nline
      elseif(leqi(op,'get').and.leqi(var,'lpage'))then
        ival=lpage
      endif

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
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, CHARACTER*(*) STRING, the string to be measured.
c
c    Output, INTEGER LENCHR, the location of the last nonblank character in STRING.
c
      integer i
      integer lchar
      integer lenchr
      integer nchar
      character*1 null
      character*(*) string

      intrinsic char
      intrinsic len

      nchar=len(string)
      null=char(0)

      do i=1,nchar

        lchar=nchar+1-i

        if(string(lchar:lchar).ne.' '.and.
     &     string(lchar:lchar).ne.null)then
          lenchr=lchar
          return
        endif

      enddo

      lenchr=0

      return
      end
      function leqi(strng1,strng2)

c*********************************************************************72
c
cc LEQI is a case insensitive comparison of two strings for equality. 
c
c  Example:
c
c    LEQI('Anjana','ANJANA') is .TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, CHARACTER*(*) STRNG1, STRNG2, the strings to compare.
c
c    Output, LOGICAL LEQI, the result of the comparison.
c
      integer i
      integer len1
      integer len2
      integer lenc
      logical leqi
      character*1 null
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2

      intrinsic char
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

      null=char(0)

      if(len1.gt.lenc.and.
     &  strng1(lenc+1:len1).ne.' '.and.
     &  strng1(lenc+1:len1).ne.null)return

      if(len2.gt.lenc.and.
     &  strng2(lenc+1:len2).ne.' '.and.
     &  strng2(lenc+1:len2).ne.null)return

      leqi=.true.

      return
      end
      function npage ( )

c*********************************************************************72
c
cc NPAGE determines whether it's time to pause before printing more.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, INTEGER NPAGE.
c    The current number of pages completed, defined as the
c    number of lines printed, divided by the number of pages per 
c    line.
c
      integer lpage
      integer nline
      integer npage

      lpage=0
      nline=0
c
c  Get the page length.
c
      call indata('get','lpage',lpage)

      if(lpage.le.0)then
        npage=0
        return
      endif
c
c  Get the current line number.
c
      call indata('get','nline',nline)

      npage=nline/lpage
      nline=nline-npage*lpage
      call setlin(nline)

      return
      end
      subroutine setlin(nline)

c*********************************************************************72
c
cc SETLIN sets the current line number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, INTEGER NLINE, the current line number.
c
      integer nline

      call indata('set','nline',nline)

      return
      end
      subroutine setpag(lpage)

c*********************************************************************72
c
cc SETPAG sets the number of lines per page.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, INTEGER LPAGE, the desired number of lines per page.
c
      integer lpage

      call indata('set','lpage',lpage)

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
