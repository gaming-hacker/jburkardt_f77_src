c  RK124.F  Version 1.14  24 October 1990
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for RK124
c
c  Version 1.14
c
c  VSCALE added to allow user to control size of arrows.
c
c  Version 1.13
c
c  CHRREA no longer forcibly capitalizes its output, so file names
c  are not capitalized.
c
c  Version 1.12
c
c  Changed to flow field arrows of uniform size.
c
c  At the expense of a "patch", the flow field command no longer kills
c  the initial condition data.
c
c  The program was failing to accept the input "T, Y1" for the graph
c  command.  CHRREA was stripping the "," as a terminator for T, but then
c  accepting the blank as the terminator for the next (null) input!
c  This has been patched, not fixed.  CHRREA should be rewritten!
c
c  Version 1.11
c
c  Major rewrites to plotting utilities.  Trying to get clipping working.
c
c  Graph of more than one quantity did not work, fixed now.
c
c  Display X and Y axis if in the plot.
c
c  Graphics routines were restarting and shutting down graphics each time.
c    This causes some graphics files to lose all but the latest image.
c    Now RK124 starts and shuts down graphics once.
c
c  Questions:
c
c  Can you have Y'=SIN(T)/T graph the flow field properly at T=0?  No.
c
c  Could you redesign to allow one to overlay F and G output?
c  Can autonomous equations use F command for vectors dy2/dt / dy1/dt?
c  I can't add that in time for this release!
c
c  Version 1.10
c
c  05 September 1990, Flow fields were not properly scaled.
c  Test case: y1'=3*t*t, plot from 0 < t < 4, 0 < y < 1, slope
c  at right hand points should be almost vertical.  Fixed now.
c
c  04 September 1990, Serious problems in SETUP were corrected.
c
c  28 August 1990, SAMPLE now prints out a message after setting up
c  the sample problem.  Also, if FLOW is called, it now sets NTAB
c  to 0, and TYPE checks this, and no longer prints out information
c  that should have been discarded.
c
c  27 August 1990, changed SUBROUTINE STEP so that, if exact solution
c  is known, it is properly evaluated at initial T.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      parameter (MAXMET=3)
      parameter (MAXEQN=4)
      parameter (MAXRHS=2*MAXEQN)
      parameter (MAXVAR=3*MAXEQN+1)
      parameter (MAXFIX=80)
      parameter (MAXPLT=10)
      parameter (MAXRPN=MAXVAR*MAXFIX)
      parameter (MAXTAB=201)

      character CARRAY*80
      character COM*1
      character FILHLP*30
      character FILNAM*30
      DIMENSION IHAVE(MAXVAR)
      character INFIX*(MAXFIX)
      DIMENSION IOUNIT(4)
      DIMENSION IPLOT(2,MAXPLT)
      character IRHS(MAXRHS)*(MAXFIX)
      DIMENSION IRPN(MAXRPN)
      DIMENSION ITEMP(MAXVAR)
      character LINE*80
      character LINE1*80
      character METNAM(MAXMET)*60
      character NAMES(MAXVAR)*3
      character NAMVAR*10
      character OUTPUT*120
      character PROMPT*80
      DIMENSION TABLE(MAXTAB,MAXVAR)
      DIMENSION TABLE2(MAXVAR)

      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     &                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY
      COMMON /CHRCOM/ NPAGE
c
c  Initialization
c
      call INIT(FILHLP,IERROR,IOUNIT,MAXTAB,MAXVAR,METNAM,NAMES,
     &  NPAGE,ROVER,TABLE,VSCALE)
c
c  Initialize compiler
c
      call COMRPN('I',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,IOUNIT)
      call COMRPN('A',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',VALUE,IOUNIT)
      DO I=2,8,2
        call COMRPN('A',IERROR,IFRM,INFIX,IRPN,
     &  MAXRPN,NAMES(I),VALUE,IOUNIT)
      end do
c
c  Set initial right hand side and solution
c
      call SAMPLE(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NEQN,
     *NSTEPS,NTAB,OUTPUT,TABLE,TSTOP)

      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='RK124 - Version 1.14  24 October 1990'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Demonstrate the numerical solution of'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='first order ordinary differential equations.'
      call CHRWRT(OUTPUT,0,IOUNIT)

      ICOM=0
      XPLT1=0.0
      XPLT2=1.0
      YPLT1=0.0
      YPLT2=1.0
      call ANYPLT(ICOM)
c
c  Next command?
c
30    CONTINUE

      IERROR=0
      NLINE=0
      PROMPT='command (H for help)'
      call CHRREA(LINE1,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)
      IF(IERROR.NE.0)GO TO 30
      call CAPCHR(LINE1)
c
c  Check first for assignment statements.
c
      IF(INDEX(LINE1,'=').GT.1)THEN
        II=INDEX(LINE1,'=')
        NAMVAR=LINE1(1:II-1)
        call s_chop ( LINE1, 1, II )
        NLINE=0
        LINE=LINE1
        call CHRLEN(LINE,NLINE)
        call SETVAL(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     &  LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     &  NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP,
     &  VSCALE)
        GO TO 30
      ELSE
        LINE=LINE1
        call CHRLEN(LINE,NLINE)
        call CHRREA(COM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)
      end if
c
c  B=Set up new problem
c
      IF(COM.EQ.'B')THEN
        call SETUP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     &  ISET,LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     &  NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP,
     &  VSCALE)
c
c  C=Continue integration
c
      else if ( COM.EQ.'C')THEN
        call STEP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRPN,
     &  MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NAMES,NEQN,
     &  NSTEPS,NTAB,OUTPUT,ROVER,TABLE,TSTOP)
c
c  D=Disk file
c
      else if ( COM.EQ.'D')THEN
        call DISK(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
c
c  F=Flow field
c
      else if ( COM.EQ.'F')THEN

        DO I=1,MAXVAR
          TABLE2(I)=TABLE(1,I)
        end do

        call FLOW(ICOMP,IERROR,INFIX,IOUNIT,IRPN,LINE,MAXRPN,
     &  MAXTAB,MAXVAR,NAMES,NEQN,NLINE,NTAB,OUTPUT,PROMPT,TABLE,VSCALE)

        DO I=1,MAXVAR
          TABLE(1,I)=TABLE2(I)
        end do
c
c  G=Graphics
c
      else if ( COM.EQ.'G')THEN
        call GRAFF(IERROR,IHAVE,IOUNIT,IPLOT,LINE,MAXPLT,MAXTAB,
     &  MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)
c
c  H=Help
c
      else if ( COM.EQ.'H')THEN
        call HELPER(IOUNIT,OUTPUT)
c
c  N=Note
c
      else if ( COM.EQ.'N')THEN
        call NOTE(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
c
c  O=input plot file
c
      else if ( COM.EQ.'O')THEN
        call INDUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     &  MAXVAR,NLINE,NTAB,PROMPT,TABLE)
c
c  P=Dump plot file
c
      else if ( COM.EQ.'P')THEN
        call DUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     &  MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)
c
c  Q=Quit
c
      else if ( COM.EQ.'Q')THEN
        call QUIT(FILNAM,IERROR,IOUNIT,NLINE,LINE,OUTPUT,PROMPT)
c
c  R=Read input from file
c
      else if ( COM.EQ.'R')THEN
        call READER(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
c
c  S=Sample problem
c
      else if ( COM.EQ.'S')THEN
        call SAMPLE(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     &  MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NEQN,
     &  NSTEPS,NTAB,OUTPUT,TABLE,TSTOP)
c
c  T=Type parameters
c
      else if ( COM.EQ.'T')THEN
        call TYPE(IOUNIT,IHAVE,IPRINT,IRHS,MAXEQN,
     &  MAXTAB,MAXVAR,METHOD,METNAM,NEQN,NSTEPS,NTAB,OUTPUT,
     &  ROVER,TABLE,TSTOP,VSCALE)
c
c  V=What variables can I set?
c
      else if ( COM.EQ.'V')THEN
        call SHOSET(IOUNIT,OUTPUT)
c
c  W=Write environment and problem to file
c
      else if ( COM.EQ.'W')THEN
        call WRITER(FILNAM,IERROR,IOUNIT,IPRINT,IRHS,LINE,
     &  MAXEQN,MAXTAB,MAXVAR,METHOD,NEQN,NLINE,
     &  NSTEPS,OUTPUT,PROMPT,TABLE,TSTOP)
c
c  ? - Extensive help from RK124.HLP
c
      else if ( COM.EQ.'?')THEN
        call GETHLP(FILHLP,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
c
c  Unrecognized command
c
      ELSE
        IERROR=1
        OUTPUT='Unknown command ignored.'
        call CHRWRT(OUTPUT,0,IOUNIT)
      end if

      IF(IERROR.NE.0.AND.IOUNIT(1).NE.0)THEN
        OUTPUT='An error has occurred in RK124 while using input'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='from a file.  That file will be closed, and input'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='will only be accepted from the user!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        CLOSE(UNIT=IOUNIT(1))
        IOUNIT(1)=0
        NPAGE=0
      end if

      GO TO 30
      END
      SUBROUTINE CAPCHR(STRING)

c*********************************************************************72
c
cc CAPCHR capitalizes the characters in a string.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHAR*1
      LOGICAL   LGE
      LOGICAL   LLE
      character STRING*(*)

      NCOPY=LEN(STRING)

      DO I=1,NCOPY
        IF(LGE(STRING(I:I),'a').AND.LLE(STRING(I:I),'z'))THEN
          ITEMP=ICHAR(STRING(I:I))+ICHAR('A')-ICHAR('a')
          STRING(I:I)=CHAR(ITEMP)
          end if
      end do

      RETURN
      END
      SUBROUTINE CHRCTI(STRING,NCHAR,INTVAL,IERROR,LCHAR)

c*********************************************************************72
c
cc CHRCTI reads an integer from a string.
c
c  CHRCTI accepts a STRING of NCHAR characters and reads an integer
c  from STRING into INTVAL.  The STRING must begin with an integer
c  but that may be followed by other information.
c  CHRCTI will read as many characters as possible until it reaches
c  the end of the STRING, or encounters a character which cannot be
c  part of the number.
c
c  Legal input is
c  blanks // initial + or - sign // integer part // blanks,// final comma
c  with most quantities optional.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRTMP*1
      LOGICAL   LGE
      LOGICAL   LLE
      character STRING*(*)

      NCOPY=NCHAR
      IF(NCOPY.LE.0)NCOPY=LEN(STRING)

      IERROR=0
      INTVAL=0
      LCHAR=-1
      ISGN=1
      ITOP=0
      IHAVE=1
      ITERM=0

10    CONTINUE

      LCHAR=LCHAR+1
      CHRTMP=STRING(LCHAR+1:LCHAR+1)
      IF(CHRTMP.EQ.' ')THEN
        IF(IHAVE.EQ.2)THEN
          ITERM=1
        else if ( IHAVE.EQ.3)THEN
          IHAVE=11
          end if
      else if ( CHRTMP.EQ.',')THEN
        IF(IHAVE.NE.1)THEN
          ITERM=1
          IHAVE=12
          LCHAR=LCHAR+1
          end if
      else if ( CHRTMP.EQ.'-')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
          ISGN=-1
        ELSE
          ITERM=1
        end if
      else if ( CHRTMP.EQ.'+')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
        ELSE
          ITERM=1
        end if
      else if ( LGE(CHRTMP,'0').AND.LLE(CHRTMP,'9').AND.IHAVE.LT.11)THEN
        IHAVE=3
        READ(CHRTMP,'(I1)')NDIG
        ITOP=10*ITOP+NDIG
      ELSE
        ITERM=1
      end if

      IF(ITERM.NE.1.AND.LCHAR+1.LT.NCOPY)GO TO 10
      IF(ITERM.NE.1.AND.LCHAR+1.EQ.NCOPY)LCHAR=NCOPY
c
c  Number seems to have terminated.  Have we got a legal number?
c
      IF(IHAVE.EQ.1.OR.IHAVE.EQ.2)THEN
        IERROR=IHAVE
        WRITE(*,*)'CHRCTI - IERROR=',IERROR
        WRITE(*,*)'CHRCTI - Illegal or nonnumeric input:'
        WRITE(*,'(1X,A)')STRING
        RETURN
      end if
c
c  Number seems OK.  Form it.
c
      INTVAL=ISGN*ITOP
      RETURN
      END
      SUBROUTINE CHRCTR(STRING,NCHAR,RVAL,IERROR,LCHAR)

c*********************************************************************72
c
cc CHRCTR reads a real number from a string.
c
c  CHRCTR accepts a STRING of NCHAR characters and reads a real
c  number from STRING into RVAL.  The STRING must begin with a real
c  number, but that may be followed by other information.
c  CHRCTR will read as many characters as possible until it reaches
c  the end of the STRING, or encounters a character which cannot be
c  part of the real number.
c
c  Legal input is
c  blanks, initial sign, integer part, decimal, fraction part,
c  E, exponent sign, exponent integer part, exponent decimal, exponent
c  fraction part, blanks, final comma, with most quantities optional.
c
c  Examples: 15, 15.0, -14E-7, E2, -12.73E-98.23, etc.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRTMP*1
      LOGICAL   LGE
      LOGICAL   LLE
      character STRING*(*)

      NCOPY=NCHAR
      IF(NCOPY.LE.0)NCOPY=LEN(STRING)

      IERROR=0
      RVAL=0.0
      LCHAR=-1
      ISGN=1
      ITOP=0
      IBOT=1
      JSGN=1
      JTOP=0
      JBOT=1
      IHAVE=1
      ITERM=0

10    CONTINUE

      LCHAR=LCHAR+1
      CHRTMP=STRING(LCHAR+1:LCHAR+1)
      IF(CHRTMP.EQ.' ')THEN
        IF(IHAVE.EQ.2.OR.IHAVE.EQ.6.OR.IHAVE.EQ.7)THEN
          ITERM=1
        else if ( IHAVE.GT.1)THEN
          IHAVE=11
          end if
      else if ( CHRTMP.EQ.',')THEN
        IF(IHAVE.NE.1)THEN
          ITERM=1
          IHAVE=12
          LCHAR=LCHAR+1
          end if
      else if ( CHRTMP.EQ.'-')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
          ISGN=-1
        else if ( IHAVE.EQ.6)THEN
          IHAVE=7
          JSGN=-1
        ELSE
          ITERM=1
          end if
      else if ( CHRTMP.EQ.'+')THEN
        IF(IHAVE.EQ.1)THEN
          IHAVE=2
        else if ( IHAVE.EQ.6)THEN
          IHAVE=7
        ELSE
          ITERM=1
          end if
      else if ( CHRTMP.EQ.'.')THEN
        IF(IHAVE.LT.4)THEN
          IHAVE=4
        else if ( IHAVE.GE.6.AND.IHAVE.LE.8)THEN
          IHAVE=9
        ELSE
          ITERM=1
          end if
      else if ( CHRTMP.EQ.'E'.OR.CHRTMP.EQ.'e')THEN
        IF(IHAVE.LT.6)THEN
          IHAVE=6
        ELSE
          ITERM=1
          end if
      else if ( LGE(CHRTMP,'0').AND.LLE(CHRTMP,'9').AND.IHAVE.LT.11)THEN
        IF(IHAVE.LE.2)THEN
          IHAVE=3
        else if ( IHAVE.EQ.4)THEN
          IHAVE=5
        else if ( IHAVE.EQ.6.OR.IHAVE.EQ.7)THEN
          IHAVE=8
        else if ( IHAVE.EQ.9)THEN
          IHAVE=10
          end if
        READ(CHRTMP,'(I1)')NDIG
        IF(IHAVE.EQ.3)THEN
          ITOP=10*ITOP+NDIG
        else if ( IHAVE.EQ.5)THEN
          ITOP=10*ITOP+NDIG
          IBOT=10*IBOT
        else if ( IHAVE.EQ.8)THEN
          JTOP=10*JTOP+NDIG
        else if ( IHAVE.EQ.10)THEN
          JTOP=10*JTOP+NDIG
          JBOT=10*JBOT
          end if
      ELSE
        ITERM=1
        end if
      IF(ITERM.NE.1.AND.LCHAR+1.LT.NCOPY)GO TO 10
      IF(ITERM.NE.1.AND.LCHAR+1.EQ.NCOPY)LCHAR=NCOPY
c
c  Number seems to have terminated.  Have we got a legal number?
c
      IF(IHAVE.EQ.1.OR.IHAVE.EQ.2.OR.IHAVE.EQ.6.OR.IHAVE.EQ.7)THEN
        IERROR=IHAVE
        WRITE(*,*)'CHRCTR - Illegal or nonnumeric input:'
        call CHRLEN(STRING,LLEN)
        WRITE(*,'(1X,A)')STRING(1:LLEN)
        RETURN
        end if
c
c  Number seems OK.  Form it.
c
      IF(JTOP.EQ.0)THEN
        REXP=1.0
      ELSE
        IF(JBOT.EQ.1)THEN
          REXP=10.0**(JSGN*JTOP)
        ELSE
          REXP=10.0**(REAL(JSGN*JTOP)/REAL(JBOT))
          end if
        end if
      RVAL=REXP*REAL(ISGN*ITOP)/REAL(IBOT)
      RETURN
      END
      SUBROUTINE CHRDB1(STRING)

c*********************************************************************72
c
cc CHRDB1 removes all blanks from a string.
c
c  CHRDB1 accepts a STRING of characters and removes all blanks
c  and nulls, left justifying the remainder and padding with blanks.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHAR*1
      character CHRTMP*1
      character STRING*(*)

      NCOPY=LEN(STRING)

      J=0
      DO I=1,NCOPY
        CHRTMP=STRING(I:I)
        STRING(I:I)=' '
        IF(CHRTMP.NE.' '.AND.CHRTMP.NE.CHAR(0))THEN
          J=J+1
          STRING(J:J)=CHRTMP
          end if
      end do

      RETURN
      END
      SUBROUTINE CHRDB2(STRING)

c*********************************************************************72
c
cc CHRDB2 replaces consecutive blanks by a single blank.
c
c  CHRDB2 accepts a STRING of characters.  It replaces all nulls
c  by blanks.  It replaces all strings of consecutive blanks by a single
c  blank, left justifying the remainder and padding with blanks.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHAR*1
      character STRING*(*)
      character NEWCHR*1
      character OLDCHR*1

      NCOPY=LEN(STRING)

      J=0
      NEWCHR=' '

      DO I=1,NCOPY
        OLDCHR=NEWCHR
        IF(STRING(I:I).EQ.CHAR(0))STRING(I:I)=' '
        NEWCHR=STRING(I:I)
        STRING(I:I)=' '
        IF(OLDCHR.NE.' '.OR.NEWCHR.NE.' ')THEN
          J=J+1
          STRING(J:J)=NEWCHR
          end if
      end do

      RETURN
      END
      SUBROUTINE CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc CHRINP processes input strings.
c
c  CHRINP is used by CHRREA, INTREA and RELREA
c  to print the PROMPT and read the LINE if necessary.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
c  IERROR - If IERROR is nonzero on input, CHRINP halts the program.
c           On output, IERROR=0 if no errors were detected,
c           IERROR=1 if there was an error in the read,
c           IERROR=2 if there was an end-of-file in the read.
c
      character CHRINT*6
      DIMENSION IOUNIT(*)
      character LINE*80
      character OUTPUT*90
      character PROMPT*80
      COMMON /CHRCOM/ NPAGE

      IF(IERROR.NE.0)THEN
        OUTPUT='CHRINP - Fatal nonzero value of IERROR='//CHRINT(IERROR)
        call CHRWRT(OUTPUT,1,IOUNIT)
        STOP
      end if

10    CONTINUE
c
c  If necessary, print prompt before reading input.
c  Turn off echo on units between 30 and 39.
c
      IF(NLINE.EQ.0)THEN
        LINE=' '
      ELSE
        call CHRLEN(LINE,NLINE)
      end if

      IF(NLINE.LE.0)THEN

        DO I=2,4
          IF(IOUNIT(I).GE.30.AND.IOUNIT(I).LE.39)IOUNIT(I)=-IOUNIT(I)
        end do

        call CHRLEN(PROMPT,LCHAR)

        IF(LCHAR.GT.0)THEN
          WRITE(OUTPUT,'(''Enter '',80A1)')(PROMPT(I:I),I=1,LCHAR)
          call CHRWRT(OUTPUT,0,IOUNIT)
        end if

        DO I=2,4
          IF(IOUNIT(I).LE.-30.AND.IOUNIT(I).GE.-39)IOUNIT(I)=-IOUNIT(I)
        end do

        IF(IOUNIT(1).LE.0)THEN
          READ(*,'(A80)',END=70,ERR=60)LINE
        ELSE
          READ(IOUNIT(1),'(A80)',END=70,ERR=60)LINE
        end if

        call CHRDB2(LINE)
        call CHRLEN(LINE,NLINE)
c
c  Don't echo input to IOUNIT(2)
c
        IF(IOUNIT(1).LT.40.OR.IOUNIT(1).GT.49)THEN
          IOSAVE=IOUNIT(2)
          IF(IOUNIT(1).LE.0)IOUNIT(2)=-1
          call CHRWRT(LINE,0,IOUNIT)
          IOUNIT(2)=IOSAVE
        end if

      end if

      IF(IOUNIT(1).EQ.0)NPAGE=0
c
c  If item was read, remove item from PROMPT list
c
      IF(NLINE.GT.0)THEN
        ICOMMA=INDEX(PROMPT,',')
        IF(ICOMMA.GT.0.AND.ICOMMA.LT.80.AND.
     &  PROMPT(ICOMMA+1:ICOMMA+1).EQ.' ')ICOMMA=ICOMMA+1
        call s_chop ( PROMPT, 1, ICOMMA )
      end if

      RETURN

70    CONTINUE

      IERROR=2
      IF(IOUNIT(1).GT.0)THEN
        CLOSE(UNIT=IOUNIT(1))
        IOUNIT(1)=0
        NPAGE=0
        OUTPUT='CHRINP - End of input from file.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='         Now seeking input from user.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=0
        GO TO 10
      end if

      OUTPUT='CHRINP - End of user input.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='       - Fatal error.  Stopping!'
      call CHRWRT(OUTPUT,0,IOUNIT)
      STOP

60    CONTINUE

      IERROR=1
      OUTPUT='CHRINP - Error in input format.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='CHRINP - Input line follows:'
      call CHRWRT(OUTPUT,0,IOUNIT)
      call CHRWRT(LINE,0,IOUNIT)
      IF(IOUNIT(1).LE.0)THEN
        NLINE=0
        GO TO 10
        end if
      RETURN
      END
      FUNCTION CHRINT(INTVAL)

c*********************************************************************72
c
cc CHRINT writes an integer to a string.
c
c  CHRINT accepts an integer and returns in CHRINT the 6-character
c  representation of the integer, right justified, or '******' if the
c  integer is too large or negative to fit in six positions.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRINT*6
      character CHRTMP*6

      IF(INTVAL.GT.999999)THEN
        CHRTMP='******'
      else if ( INTVAL.LT.-99999)THEN
        CHRTMP='-*****'
      ELSE
        WRITE(CHRTMP,'(I6)')INTVAL
      end if

      CHRINT=CHRTMP
      RETURN
      END
      SUBROUTINE CHRITC(STRING,NCHAR,INTVAL)

c*********************************************************************72
c
cc CHRITC writes an integer to a string.
c
c  CHRITC accepts an integer in INTVAL and stores it in a STRING of
c  NCHAR characters.  The last digit of the integer is stored in the
c  last character of the STRING.  Any left-over entries in STRING
c  are filled with blanks.  If the integer is too large to be
c  written into STRING, STRING is filled with '*' characters.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHAR*1
      character STRING*(*)

      NCOPY=NCHAR
      IF(NCOPY.LE.0)NCOPY=LEN(STRING)

      IVAL=IABS(INTVAL)
      IOFF=ICHAR('0')
      I=NCOPY+1

10    CONTINUE

      I=I-1
      IDIG=MOD(IVAL,10)
      STRING(I:I)=CHAR(IDIG+IOFF)
      IVAL=IVAL/10

      IF(IVAL.NE.0)THEN
        IF(INTVAL.GE.0.AND.I.GT.1)GO TO 10
        IF(INTVAL.LT.0.AND.I.GT.2)GO TO 10
        DO J=1,NCOPY
          STRING(J:J)='*'
        end do
        RETURN
      end if

      IF(INTVAL.LT.0)THEN
        I=I-1
        STRING(I:I)='-'
      end if

      DO J=1,I-1
        STRING(J:J)=' '
      end do

      RETURN
      END
      SUBROUTINE CHRLEN(STRING,LCHAR)

c*********************************************************************72
c
cc CHRLEN returns the length of a string.
c
c  CHRLEN accepts a STRING of characters and returns LCHAR,
c  the length of the string up to the last nonblank, nonnull.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHAR*1
      character STRING*(*)

      NCOPY=LEN(STRING)

      DO I=1,NCOPY
        LCHAR=NCOPY+1-I
        IF(STRING(LCHAR:LCHAR).NE.' '.AND.
     *     STRING(LCHAR:LCHAR).NE.CHAR(0))RETURN
      end do

      LCHAR=0
      RETURN
      END
      SUBROUTINE CHRREA(STRING,LINE,NLINE,PROMPT,IOUNIT,IERROR,ITERM,
     *ICAP)

c*********************************************************************72
c
cc CHRREA reads a real number from a string of input.
c
c  CHRREA accepts LINE, which is assumed to contain NLINE user input
c  characters, where NLINE may be less than 1, and a PROMPT line.
c  If NLINE is less than 1, the PROMPT is printed and user input read
c  from IOUNIT(1) into LINE, and NLINE updated.
c  In either case, up to NCHAR characters are read from LINE into
c  the character STRING and the positions read are removed,
c  and NLINE updated.
c
c  PROMPT is also updated.  On satisfactory input of STRING, everything
c  in PROMPT up to and including the first comma is removed.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
c  IOUNIT(1) represents the input unit.  0 is taken to be the user
c  and we READ(*,format) the input.
c  IOUNIT(2) is taken to be a standard output unit.  Input is never
c  echoed to IOUNIT(2), but may be to other units.
c  Later units:  If their values is between 30 and 39, user input is
c  copied to them, but no output.
c  If between 40 and 49, output is copied to them, but no input.
c  If the unit number is negative, no input is read, nor output written.
c
c  IERROR - 0, No error occurred.
c           1, Format error during read.
c           2, End of file during read.
c
c  ITERM  - 0, No check for terminators.
c           1, Blank, slash, comma, and equal terminate input.
c
      character CHRTMP
      DIMENSION IOUNIT(*)
      character LINE*80
      character OUTPUT*90
      character PROMPT*80
      character STRING*(*)

      NCHAR=LEN(STRING)
      STRING=' '
      call CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      IF(IERROR.NE.0)RETURN
c
c  Remove double blanks, and capitalize LINE
c
c     IF(ITERM.EQ.2.OR.ITERM.EQ.3)CALL CHRDB2(LINE)
      IF(ICAP.EQ.1)CALL CAPCHR(LINE)
c
c  Null input acceptable for character input only
c
      IF(NLINE.LE.0)RETURN
c
c  Take one character at a time from LINE and put into STRING.
c
      LCHAR=0
      DO I = 1, NCHAR
        IF ( LCHAR .NE. 0 ) then
          CHRTMP=LINE(I:I)
          IF(ITERM.EQ.1)THEN
            IF(CHRTMP.EQ.' '.OR.CHRTMP.EQ.'/'.OR.
     &         CHRTMP.EQ.','.OR.CHRTMP.EQ.'=')LCHAR=I
          end if
          IF(LCHAR.EQ.0)STRING(I:I)=CHRTMP
        end if
      end do

      IF(LCHAR.EQ.0)LCHAR=NCHAR
      call s_chop ( LINE, 1, LCHAR )
      call CHRLEN(LINE,NLINE)
      RETURN
      END
      character*14 FUNCTION CHRREL(RVAL)

c*********************************************************************72
c
cc CHRREL writes a real number to a string.
c
c  CHRREL accepts a real number in RVAL and returns in CHRREL a
c  14-character representation, right justified, of that number.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRTMP*14
c
c  We can't seem to write directly into CHRREL because of compiler
c  quibbles.
c
      WRITE(CHRTMP,'(G14.6)')RVAL
      CHRREL=CHRTMP
      RETURN
      END
      SUBROUTINE CHRWRT(STRING,ICOMP,IOUNIT)

c*********************************************************************72
c
cc CHRWRT outputs a character string.
c
c  CHRWRT writes a character STRING of at most NCHAR characters to one
c  or more output units.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c  ICOMP  - Input flag.
c           0, no action.
c           1, then double blanks are removed from the STRING before
c              printing.
c           2, paging is turned off.
c           3, initialization, with number of lines per page set to 22
c
c  IOUNIT - vector of length 4, containing in positions 2
c           through 4 either a negative value, or 0 for the user output
c           unit *, or the value of a Fortran logical output unit.
c
      character ISAY*1
      DIMENSION IOUNIT(*)
      character STRING*(*)
      SAVE IPAGE, ICALL
      COMMON /CHRCOM/ NPAGE
      DATA ICALL /0/

      IF(ICOMP.EQ.2)THEN
        IPAGE=-1
        ICALL=1
        RETURN
      else if ( ICOMP.EQ.3)THEN
        ICALL=1
        NPAGE=0
        IPAGE=22
        RETURN
      end if

      IF(ICALL.EQ.0)THEN
        ICALL=1
        NPAGE=0
        IPAGE=22
      end if
c
c  If first call, with I/O attached to user, see if paging desired
c
      IF(IPAGE.EQ.0.AND.IOUNIT(1).EQ.0.AND.IOUNIT(2).EQ.0)THEN
        WRITE(*,*)'Enter lines per page, 0 for no paging.'
        READ(*,*,END=20,ERR=20)IPAGE
        IF(IPAGE.EQ.0)IPAGE=-1
20      CONTINUE
      end if

      IF(IPAGE.GT.0.AND.
     &   NPAGE.GE.IPAGE.AND.
     &   IOUNIT(2).EQ.0.AND.
     &   IOUNIT(1).LE.0)THEN
        NPAGE=0
        WRITE(*,*)'Press RETURN for more'
        READ(*,'(A1)',END=25,ERR=25)ISAY
        IF(ISAY.EQ.'Q'.OR.ISAY.EQ.'q')IPAGE=0
25      CONTINUE
      end if

      IF(ICOMP.EQ.1)CALL CHRDB2(STRING)
      call CHRLEN(STRING,LCHAR)
      IF(LCHAR.LE.0)LCHAR=1

      DO I=2,4
        IF(IOUNIT(I).EQ.0)THEN
          WRITE(*,'(1X,A)')STRING(1:LCHAR)
        else if ( IOUNIT(I).GT.0)THEN
          WRITE(IOUNIT(I),'(1X,A)')STRING(1:LCHAR)
          end if
      end do

      NPAGE=NPAGE+1
      RETURN
      END
      SUBROUTINE COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,
     & Y,YP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)

c*********************************************************************72
c
cc COMPUT evaluates the right hand side function of the ODE.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character COM*1
      character INFIX*(*)
      DIMENSION IOUNIT(*)
      DIMENSION IRPN(MAXRPN)
      character NAMES(MAXVAR)*3
      character NAMVAR*10
      character OUTPUT*(*)
      DIMENSION Y(NEQN)
      DIMENSION YP(NEQN)

      IERROR=0
      call COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',TVAL,IOUNIT)

      DO I=1,NEQN
        NAMVAR=NAMES(2*I)
        call COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,Y(I),
     &  IOUNIT)
      end do
c
c  Evaluate formulas
c
      DO I=1,NEQN
        IFRM=I
        call COMRPN('E',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,YP(I),
     &  IOUNIT)
        IF(ABS(YP(I)).GT.ROVER)THEN
          IERROR=101
          OUTPUT='Overflow in value of derivative!'
          call CHRWRT(OUTPUT,0,IOUNIT)
          RETURN
        end if
      end do

      RETURN
      END
      SUBROUTINE COMRPN(COM,IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVR,VALUE,
     &  IOUNIT)

c*********************************************************************72
c
cc COMRPN interprets symbolic formulas.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      parameter (MAXSYM=75)
      parameter (MAXVAL=MAXSYM)

      character CHRINT*6
      character COM*1
      character INFIX*(*)
      DIMENSION INTSYM(MAXSYM)
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION IRPN(MAXRPN)
      DIMENSION ISTACK(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      character NAMVR*(*)
      character NAMVAR*10
      character OUTPUT*100
      character SYM*10
      character SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)
      SAVE EPS,IFREE,ISEED,LENFIX,NINT,NSYM,NSYMS
      SAVE INTSYM,IOPSYM,IPRSYM,IPVAL,ISTACK,LENSYM
      SAVE SYMBOL,VALSYM
      SAVE IFINIS,INDX1,INEG
      DATA IOPSYM
     * / 1, 1, 1, 1, 1,
     *   1, 2, 1, 1, 1,
     *   1, 0, 1, 1, 1,
     *   1, 1, 1, 1, 1,
     *   1, 1, 2, 2, 1,
     *   1, 1, 1, 0, 2,
     *   1, 1, 1, 1, 1,
     *   1, 1, 1, 1, 1,
     *   1, 1, 2, 2, 2,
     *   2, 2, 2,-1,-1,
     *   2,-1,-1, 1, 2,
     *   20*0/
      DATA IPRSYM
     * / 9, 9, 9, 9, 9,
     *   9, 9, 9, 9, 9,
     *   9,10, 9, 9, 9,
     *   9, 9, 9, 9, 9,
     *   9, 9, 9, 9, 5,
     *   9, 9, 9,10, 9,
     *   9, 9, 9, 9, 9,
     *   9, 9, 9, 9, 9,
     *   9, 9, 8, 8, 5,
     *   5, 7, 6, 1, 2,
     *   3, 0, 9, 9, 9,
     *   20*0/
      DATA SYMBOL
     * /'ABS',   'ACOS',  'ALOG',  'ALOG10','ASIN',
     *  'ATAN',  'ATAN2', 'COS',   'COSH',  'DET',
     *  'DIAG',  'EPS',   'EVAL',  'EVEC',  'EXP',
     *  'ID',    'INT',   'INV',   'IVAL',  'LN',
     *  'LOG',   'LOG10', 'MAX',   'MIN',   'NEG',
     *  'NORM0', 'NORM1', 'NORM2', 'PI',    'POLY',
     *  'RAN',   'RVAL',  'SIN',   'SINE',  'SINH',
     *  'SQRT',  'STEP',  'TAN',   'TANH',  'TRACE',
     *  'TRANS', 'ZERO',  '**',    '^',     '+',
     *  '-',     '/',     '*',     '(',     ')',
     *  '=',     '$',     ',',     '!',     'INDEX1',
     *  20*' '/

      NAMVAR=NAMVR
      IMPLIC=0
      MAXFIX=LEN(INFIX)
      MAXFRM=(MAXRPN/MAXFIX)
      call CAPCHR(NAMVAR)
      call CAPCHR(INFIX)
      call CHRDB1(NAMVAR)
      IERROR=0
      IF(COM.EQ.'E'.OR.COM.EQ.'F'.OR.COM.EQ.'G')THEN
        IF(IFRM.LE.0.OR.IFRM.GT.MAXFRM)THEN
          OUTPUT='COMRPN - Illegal formula index='//CHRINT(IFRM)
          call CHRWRT(OUTPUT,1,IOUNIT)
          IERROR=1
          RETURN
          end if
        end if
c
c  COM=I initialize
c  Note that the assignment of NSYMP below must be updated
c  if new symbols are added.
c
      IF(COM.EQ.'I')THEN
        EPS=1.0

10      CONTINUE

        EPS=0.5*EPS
        IF(1.0+EPS.GT.1.0)GO TO 10

        EPS=2.0*EPS
        ISEED=0
        LENFIX=0
        NINT=0
        NSYMP=56
        NSYM=NSYMP
        NSYMS=NSYMP
        IFREE=NSYMP+1

        DO I=1,NSYMP
          call CHRLEN(SYMBOL(I),LCHAR)
          LENSYM(I)=LCHAR
          IPVAL(I)=I
          VALSYM(I)=0.0
          IF(SYMBOL(I).EQ.'NEG')INEG=I
          IF(SYMBOL(I).EQ.'$')IFINIS=I
          IF(SYMBOL(I).EQ.'INDEX1')INDX1=I
          IF(SYMBOL(I).EQ.'PI')VALSYM(I)=4.0*ATAN2(1.0,1.0)
          IF(SYMBOL(I).EQ.'EPS')VALSYM(I)=EPS
        end do

        DO I=1,MAXRPN
          IRPN(I)=0
        end do

        INFIX=' '
c
c  COM=A, Add variable
c
      else if ( COM.EQ.'A')THEN
        call SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     1  MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *  VALSYM,IOUNIT,OUTPUT,IFREE)
c
c  COM=V, Set variable value
c  COM=R, Get variable value
c
      else if ( COM.EQ.'R'.OR.COM.EQ.'V')THEN
        call SYMVAL(COM,IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     1  MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,VALSYM,VALUE,
     *  IOUNIT,OUTPUT,IFREE)
c
c  COM=F or G, compile formula.
c
      else if ( COM.EQ.'F'.OR.COM.EQ.'G')THEN
        NUVAR=1
        IF(COM.EQ.'G')NUVAR=0
c
c  Compress and measure formula
c
        call CHRDB1(INFIX)
        call CHRLEN(INFIX,LENFIX)
        IF(LENFIX.LE.0)THEN
          IERROR=1
          OUTPUT='COMRPN - Formula has zero length.'
          call CHRWRT(OUTPUT,1,IOUNIT)
          RETURN
          end if
c
c  Parenthesis check
c
        ISUM=0
        DO I=1,LENFIX
          IF(INFIX(I:I).EQ.'(')ISUM=ISUM+1
          IF(INFIX(I:I).EQ.')')ISUM=ISUM-1
          IF(ISUM.LT.0)IERROR=1
        end do

        IF(ISUM.NE.0)IERROR=1

        DO I=2,LENFIX
          IF(INFIX(I-1:I-1).EQ.')'.AND.INFIX(I:I).EQ.'(')IERROR=1
          IF(INFIX(I-1:I-1).EQ.'('.AND.INFIX(I:I).EQ.')')IERROR=1
        end do

        IF(IERROR.NE.0)THEN
          OUTPUT='COMRPN - Unbalanced or illegal parentheses.'
          call CHRWRT(OUTPUT,0,IOUNIT)
          RETURN
        end if
c
c  Convert to string of integers
c
        call SYMCHK(IERROR,IFINIS,IFREE,IMPLIC,INDX1,INEG,INFIX,
     *  INTSYM,IOPSYM,IOUNIT,IPRSYM,IPVAL,LENFIX,LENSYM,
     *  MAXSYM,MAXVAL,NINT,NSYM,NSYMP,NUVAR,OUTPUT,SYMBOL,
     *  VALSYM)

        IF(IERROR.NE.0)THEN
          OUTPUT='COMRPN - Could not compile formula.'
          call CHRWRT(OUTPUT,0,IOUNIT)
          IF(IMPLIC.NE.0)GO TO 160
          RETURN
        end if
c
c  Convert to RPN
c
        IMIN=(IFRM-1)*80+1
        call RPNSET(IERROR,INTSYM,IOPSYM,IPRSYM,IRPN(IMIN),ISTACK,
     1  MAXFIX,MAXSYM,NINT,NRPN,SYMBOL,IOUNIT,OUTPUT,IFINIS)
        IF(IERROR.NE.0)THEN
          OUTPUT='COMRPN - Could not compile formula.'
          call CHRWRT(OUTPUT,0,IOUNIT)
          IF(IMPLIC.NE.0)GO TO 160
          RETURN
          end if
        INFIX=' '
c
c  Check that operators and arguments are balanced
c
        IHI=IMIN+NRPN-1
        call RPNCHK(IERROR,IHI,ILO,IOPSYM,IRPN,MAXRPN,MAXSYM)
        IF(IERROR.NE.0.OR.ILO.NE.IMIN)THEN
          OUTPUT='COMRPN - Illegal formula'
          call CHRWRT(OUTPUT,1,IOUNIT)
          IERROR=1
          IF(IMPLIC.NE.0)GO TO 160
          RETURN
          end if
c
c  For implicit definition via equality, evaluate formula to
c  get dimensions
c
        IF(IMPLIC.NE.0)GO TO 150
      else if ( COM.EQ.'E')THEN
        GO TO 150
      ELSE
        OUTPUT='COMRPN - Unknown command = '//COM
        call CHRWRT(OUTPUT,1,IOUNIT)
        IERROR=1
        end if
      RETURN
c
c  COM=E, Evaluate formula
c
150   CONTINUE

      IMIN=(IFRM-1)*80+1
      NRPN=80
      IFREESV=IFREE
      call RPNVAL(IERROR,IOPSYM,IPRSYM,IPVAL,IRPN(IMIN),ISTACK,
     *LENSYM,MAXRPN,MAXSYM,MAXVAL,IFREE,NRPN,NSYM,NSYMS,SYMBOL,VALSYM,
     *VALUE,IOUNIT,OUTPUT)
      IFREE=IFREESV
      IF(IERROR.NE.0)THEN
        VALUE=0.0
        OUTPUT='COMRPN - Evaluation error.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        IF(IMPLIC.NE.0)GO TO 160
        end if
      RETURN
c
c  If problems, cancel implicit declaration
c
160   CONTINUE

      OUTPUT='COMRPN - Cancelling implicit variable '
      call CHRWRT(OUTPUT,1,IOUNIT)
      SYMBOL(IMPLIC)='VOID'
      LENSYM(IMPLIC)=4
      IF(IMPLIC.EQ.NSYM)NSYM=NSYM-1
      RETURN
      END
      SUBROUTINE DELFIL(FILNAM,IOUNIT,OUTPUT)

c*********************************************************************72
c
cc DELFIL deletes a file.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character FILNAM*(*)
      DIMENSION IOUNIT(*)
      character OUTPUT*(*)

      OPEN(UNIT=18,FILE=FILNAM,STATUS='OLD',ERR=10)
      OUTPUT='RK124 is deleting the old copy of '//FILNAM
      call CHRWRT(OUTPUT,0,IOUNIT)
      CLOSE(UNIT=18,STATUS='DELETE')

10    CONTINUE

      RETURN
      END
      SUBROUTINE DISK(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc DISK opens or closes the transcript file.
c
c  If disk file is opened, close it.
c  If disk file is not open, delete any old copy, and open a new one.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character FILNAM*(*)
      DIMENSION IOUNIT(*)
      character LINE*(*)
      character OUTPUT*(*)
      character PROMPT*(*)

      IF(IOUNIT(3).GT.0)THEN
        OUTPUT='RK124 is closing the disk transcript file.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        CLOSE(UNIT=IOUNIT(3))
        IOUNIT(3)=-1
      ELSE
        PROMPT='transcript filename'
        call CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,0)
        IF(IERROR.NE.0)RETURN
        IOUNIT(3)=21
        call DELFIL(FILNAM,IOUNIT,OUTPUT)
        OPEN(UNIT=IOUNIT(3),FILE=FILNAM,STATUS='NEW',ERR=10)
        OUTPUT='RK124 is opening the file '//FILNAM
        call CHRWRT(OUTPUT,0,IOUNIT)
      end if

      RETURN
c
c  File not found
c
10    CONTINUE

      OUTPUT='File could not be opened.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE DUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     *MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)

c*********************************************************************72
c
cc DUMP writes the problem information to a dump file.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character FILNAM*(*)
      DIMENSION IHAVE(MAXVAR)
      DIMENSION IOUNIT(*)
      DIMENSION ITEMP(MAXVAR)
      character LINE*(*)
      character NAMES(MAXVAR)*3
      character OUTPUT*(*)
      character PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

      IF(NTAB.LE.1)THEN
        IERROR=1
        OUTPUT='There is no data to write to a file!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if

      PROMPT='filename to store saved data in.'
      call CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,0)
      IF(IERROR.NE.0)RETURN
      call DELFIL(FILNAM,IOUNIT,OUTPUT)

      NDO=0

      DO I=1,MAXVAR
        IF(IHAVE(I).NE.0)THEN
          NDO=NDO+1
          ITEMP(NDO)=I
        end if
      end do

      OPEN(UNIT=17,FILE=FILNAM,STATUS='NEW',ERR=30)
      WRITE(17,*)'RK124 created this file.'
      WRITE(17,'('' Variables:'',13(1X,A3))')
     *(NAMES(ITEMP(I)),I=1,NDO)
      WRITE(17,'(1X,15I4)')NTAB,(IHAVE(I),I=1,MAXVAR)
      WRITE(17,*)'Table:'
      DO I=1,NTAB
        WRITE(17,'(1X,6G12.4)')(TABLE(I,ITEMP(J)),J=1,NDO)
      end do
      WRITE(17,*)'End.'
      CLOSE(UNIT=17)

      OUTPUT='Data stored in file.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN

30    CONTINUE

      IERROR=2
      RETURN
      END
      SUBROUTINE FLOW(ICOMP,IERROR,INFIX,IOUNIT,IRPN,LINE,MAXRPN,
     *MAXTAB,MAXVAR,NAMES,NEQN,NLINE,NTAB,OUTPUT,PROMPT,TABLE,VSCALE)

c*********************************************************************72
c
cc FLOW ???
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
c  TABLE  Output, REAL TABLE(NY,NX), contains the direction field values.
c         TABLE(I,J) contains the scaled value of DYDT at Y(I), X(J).
c         Here Y(I)=((NY-I)*YMIN + (I-1)*YMAX) / REAL (NY-1)
c         and  X(J)=((NX-J)*XMIN + (J-1)*XMAX) / REAL (NX-1)
c         The scaling done divides each entry of TABLE by DY/(DX*DMAX)
c         where DY = (YMAX-YMIN)/REAL(NY-1), DX=(XMAX-XMIN)/REAL(NX-1)
c         and DMAX is the maximum absolute value of the entries in DIR.
c         This guarantees that the flow field vector (DX, TABLE(I,J)*DX)
c         will fit reasonably well inside a box of width DT, height DY,
c         centered at Y(I), X(J).
c
c  ICOMP  Input, INTEGER ICOMP, the component of the solution whose
c         flow field is to be mapped.
c
c  NX,
c  NY     Input, INTEGER NX, NY, the number of points in the X and Y directions.
c
c  XMAX,
c  XMIN,
c  YMAX,
c  YMIN   Input, REAL XMAX, XMIN, YMAX, YMIN, the maximum X, minimum X,
c         maximum Y and minimum Y.
c
      character CARRAY*80
      character CHRINT*6
      character COM*1
      character INFIX*(*)
      DIMENSION IOUNIT(*)
      DIMENSION IRPN(MAXRPN)
      character LINE*(*)
      character NAMES(MAXVAR)*3
      character NAMVAR*10
      character OUTPUT*(*)
      character PROMPT*(*)
      DIMENSION TABLE(*)

      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     *                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2
      COMMON /ANYCHR/ CARRAY

      IERROR=0
      PROMPT='number of T points, minimum T, maximum T.'
      call INTREA(NX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(NX.LT.1)THEN
        IERROR=1
        OUTPUT='Number of T points must be at least 1!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if
      call RELREA(XMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      call RELREA(XMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(IERROR.NE.0)RETURN
      TEMP1=MIN(XMIN,XMAX)
      TEMP2=MAX(XMIN,XMAX)
      XMIN=TEMP1
      XMAX=TEMP2

      IF(XMIN.EQ.XMAX)THEN
        XMIN=XMIN-0.5
        XMAX=XMAX+0.5
      end if

      PROMPT='number of Y points, minimum Y, maximum Y.'
      call INTREA(NY,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(NY.LT.1)THEN
        IERROR=1
        OUTPUT='Number of Y points must be at least 1!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if
      call RELREA(YMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      call RELREA(YMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      IF(IERROR.NE.0)RETURN
      TEMP1=MIN(YMIN,YMAX)
      TEMP2=MAX(YMIN,YMAX)
      YMIN=TEMP1
      YMAX=TEMP2
      IF(YMIN.EQ.YMAX)THEN
        YMIN=YMIN-0.5
        YMAX=YMAX+0.5
        end if

      IF(NX*NY.GT.MAXVAR*MAXTAB)THEN
        OUTPUT='Sorry, you have asked for too many points.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='The product NX*NY must be less than '//
     *  CHRINT(MAXVAR*MAXTAB)
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='but your value is '//CHRINT(NX*NY)
        call CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=1
        RETURN
      end if

      IF(NEQN.GT.1)THEN
        PROMPT='Y component number.'
        call INTREA(ICOMP,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        IF(IERROR.NE.0)RETURN
      ELSE
        ICOMP=1
      end if
c
c  Note that all solution data beyond the initial condition is about to
c  be overwritten:
c
      IF(NTAB.GT.0)NTAB=1

      DMAX=1.0
      IF(NX.GT.1)DX=(XMAX-XMIN)/REAL(NX-1)
      IF(NY.GT.1)DY=(YMAX-YMIN)/REAL(NY-1)
c
c  Set a value of X, and tell COMRPN what that value is.
c
      DO J=1,NX

        IF(NX.GT.1)THEN
          X=((NX-J)*XMIN + (J-1)*XMAX) / REAL (NX-1)
        ELSE
          X=0.5*(XMIN+XMAX)
        end if

        call COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',X,IOUNIT)
c
c  Set a value of Y, and tell COMRPN what that value is.
c  Then ask COMRPN to evaluate DYDT(T,Y).
c
        DO I=1,NY

          IF(NY.GT.1)THEN
            Y=((NY-I)*YMIN + (I-1)*YMAX) / REAL (NY-1)
          ELSE
            Y=0.5*(YMIN+YMAX)
          end if

          NAMVAR=NAMES(2*ICOMP)
          call COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,Y,IOUNIT)
          call COMRPN('E',IERROR,ICOMP,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,
     *    IOUNIT)
          TABLE((J-1)*NY+I)=VALUE
c         DMAX=MAX(DMAX,ABS(VALUE))
        end do
      end do
c
c  Scale direction vector so that if you multiply it by DX
c  it's guaranteed to be no greater than DY.
c
c     SCALE=0.5*DY/DMAX
c     IF(SCALE.GT.0.5*DX)SCALE=0.5*DX
c     DELT=SCALE
c     DO J=1,NX*NY
c       TABLE(J)=SCALE*TABLE(J)
c     end do
c
c  Do the drawing.
c
      XPLT1=0.00
      XPLT2=1.00
      YPLT1=0.00
      YPLT2=1.00
      IPLT1=1
      ICOM=2
      call ANYPLT(ICOM)

      XPLT1=XMIN-DX
      XPLT2=XMAX-XMIN+2.0*DX
      YPLT1=YMIN-DY
      YPLT2=YMAX-YMIN+2.0*DY
      ICOM=3
      call ANYPLT(ICOM)
c
c  Draw X and Y axis if they appear
c
      IF(XMIN.LE.0.0.AND.0.0.LE.XMAX)THEN
        XPLT1=0.0
        YPLT1=YMIN
        ICOM=4
        call ANYPLT(ICOM)
        XPLT1=0.0
        YPLT1=YMAX
        ICOM=5
        call ANYPLT(ICOM)
      end if

      IF(YMIN.LE.0.0.AND.0.0.LE.YMAX)THEN
        XPLT1=XMIN
        YPLT1=0.0
        ICOM=4
        call ANYPLT(ICOM)
        XPLT1=XMAX
        YPLT1=0.0
        ICOM=5
        call ANYPLT(ICOM)
      end if

      CARRAY='.'
c     VSCALE=0.75*MIN((XMAX-XMIN)/REAL(NX-1), (YMAX-YMIN)/REAL(NY-1))
      ALEN=VSCALE*MIN(1.0/REAL(NX),1.0/REAL(NY))

      DO I=1,NY
        DO J=1,NX
          X1=XMIN+(J-1)*(XMAX-XMIN)/REAL(NX-1)
          Y1=YMIN+(I-1)*(YMAX-YMIN)/REAL(NY-1)
          XPLT1=X1
          YPLT1=Y1
          ICOM=11
          call ANYPLT(ICOM)
          ICOM=4
          call ANYPLT(ICOM)

          TEMPX=1.0
          TEMPY=TABLE((J-1)*NY+I)
          ANGLE=ATAN(TEMPY)
c         TEMPD=SQRT(TEMPY**2+TEMPX**2)
c         TEMPX=VSCALE*TEMPX/TEMPD
c         TEMPY=VSCALE*TEMPY/TEMPD
c
c         X2=X1+TEMPX
c         Y2=Y1+TEMPY
          X2=X1+(XMAX-XMIN)*ALEN*COS(ANGLE)
          Y2=Y1+(YMAX-YMIN)*ALEN*SIN(ANGLE)
          XPLT1=X2
          YPLT1=Y2
          ICOM=5
          call ANYPLT(ICOM)
c
c         UU=X2-X1
c         VV=Y2-Y1
c         TNORM=SQRT((UU)**2+(VV)**2)
c         IF(TNORM.GT.0.0)THEN
c           THET=1.57-ATAN(2.0)
c           THET=0.5*THET
c           ALPH=ATAN2(VV,UU)
c           DEL=SQRT(5.0)*TNORM/3.0
c           U1=X1+DEL*COS(ALPH-THET)
c           V1=Y1+DEL*SIN(ALPH-THET)
c           U2=X1+DEL*COS(ALPH+THET)
c           V2=Y1+DEL*SIN(ALPH+THET)
c           XPLT1=U1
c           YPLT1=V1
c           ICOM=4
c           call ANYPLT(ICOM)
c           XPLT1=X2
c           YPLT1=Y2
c           ICOM=5
c           call ANYPLT(ICOM)
c           XPLT1=U2
c           YPLT1=V2
c           ICOM=5
c           call ANYPLT(ICOM)
c           end if
        end do
      end do

      ICOM=9
      call ANYPLT(ICOM)
      RETURN
      END
      SUBROUTINE FUNVAL(IARG1,IARG2,IERROR,IOPSYM,IPRSYM,IPVAL,
     *ITEMP,LENSYM,MAXSYM,MAXVAL,NSYMS,SYM,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IPSET,IFREE)

c*********************************************************************72
c
cc FUNVAL is used by COMRPN to evaluate specific arithmetic functions.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CTEMP*3
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      character OUTPUT*100
      character SYM*10
      character SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)

      IPSET=0
      IERROR=0
      INDEX1=IPVAL(IARG1)
      INDEX2=0
      IF(IARG2.NE.0)INDEX2=IPVAL(IARG2)

      IF(NSYMS.GE.MAXSYM)THEN
        IERROR=1
        OUTPUT='FUNVAL - Not enough storage.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if

      NSYMS=NSYMS+1
      IOPSYM(NSYMS)=0
      IPRSYM(NSYMS)=10
      LENSYM(NSYMS)=6
      ITEMP=ITEMP+1
      call CHRITC(CTEMP,3,ITEMP)
      IF(ITEMP.LE.99)CTEMP(1:1)='0'
      IF(ITEMP.LE.9)CTEMP(2:2)='0'
      SYMBOL(NSYMS)='STK000'
      SYMBOL(NSYMS)(4:6)=CTEMP
      IPVAL(NSYMS)=IFREE
      INDEX4=IPVAL(NSYMS)

        ARG1=VALSYM(IPVAL(IARG1))
        ARG2=0.0
        IF(IARG2.NE.0)ARG2=VALSYM(IPVAL(IARG2))
        IF(SYM.EQ.'+')THEN
          VALSYM(INDEX4)=ARG1+ARG2
          RETURN
        else if ( SYM.EQ.'-')THEN
          VALSYM(INDEX4)=ARG1-ARG2
          RETURN
        else if ( SYM.EQ.'/')THEN
          IF(ARG2.EQ.0.0)THEN
            IERROR=1
            OUTPUT='FUNVAL - Attempt to divide by 0.'
            call CHRWRT(OUTPUT,0,IOUNIT)
          ELSE
            VALSYM(INDEX4)=ARG1/ARG2
            end if
          RETURN
        else if ( SYM.EQ.'*')THEN
          VALSYM(INDEX4)=ARG1*ARG2
          RETURN
        else if ( SYM.EQ.'**'.OR.SYM.EQ.'^')THEN
          IF(ARG1.EQ.0.0.AND.ARG2.EQ.0.0)THEN
            OUTPUT='FUNVAL - Attempt to compute 0**0'
            call CHRWRT(OUTPUT,0,IOUNIT)
            IERROR=1
            RETURN
            end if
          IF(ARG1.LT.0.0.AND.REAL(INT(ARG2)).NE.ARG2)THEN
            WRITE(OUTPUT,1040)ARG1,ARG2
            call CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
            end if
          SARG=1.0
          VARG=ARG1
          IF(ARG1.LT.0.0)THEN
            VARG=ABS(ARG1)
            IARG=INT(ARG2)
            IF(2*(IARG/2).NE.IARG)SARG=-1.0
            end if
          VALSYM(INDEX4)=SARG*VARG**ARG2
          RETURN
        else if ( SYM.EQ.'=')THEN
          ARG1=ARG2
          VALSYM(INDEX1)=ARG2
          VALSYM(INDEX4)=ARG1
          RETURN
        else if ( SYM.EQ.'ABS')THEN
          VALSYM(INDEX4)=ABS(ARG1)
          RETURN
        else if ( SYM.EQ.'ACOS')THEN
          IF(ARG1.LT.-1.0.OR.ARG1.GT.1.0)THEN
            WRITE(OUTPUT,1000)SYM,ARG1
            call CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
            end if
          VALSYM(INDEX4)=ACOS(ARG1)
          RETURN
        else if ( SYM.EQ.'ALOG'.OR.SYM.EQ.'LN'.OR.SYM.EQ.'LOG')THEN
          IF(ARG1.LE.0.0)THEN
            IERROR=1
            WRITE(OUTPUT,1000)SYM,ARG1
            call CHRWRT(OUTPUT,1,IOUNIT)
          ELSE
            VALSYM(INDEX4)=ALOG(ARG1)
            end if
          RETURN
        else if ( SYM.EQ.'ALOG10'.OR.SYM.EQ.'LOG10')THEN
          IF(ARG1.LE.0.0)THEN
            WRITE(OUTPUT,1000)SYM,ARG1
            call CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
            end if
          VALSYM(INDEX4)=ALOG10(ARG1)
          RETURN
        else if ( SYM.EQ.'ASIN')THEN
          IF(ARG1.LT.-1.0.OR.ARG1.GT.1.0)THEN
            WRITE(OUTPUT,1000)SYM,ARG1
            call CHRWRT(OUTPUT,1,IOUNIT)
            IERROR=1
            RETURN
            end if
          VALSYM(INDEX4)=ASIN(ARG1)
          RETURN
        else if ( SYM.EQ.'ATAN')THEN
          VALSYM(INDEX4)=ATAN(ARG1)
          RETURN
        else if ( SYM.EQ.'ATAN2')THEN
          IF(ARG1.EQ.0.0.AND.ARG2.EQ.0.0)ARG2=1.0
          VALSYM(INDEX4)=ATAN2(ARG1,ARG2)
          RETURN
        else if ( SYM.EQ.'COS')THEN
          VALSYM(INDEX4)=COS(ARG1)
          RETURN
        else if ( SYM.EQ.'COSH')THEN
          VALSYM(INDEX4)=COSH(ARG1)
          RETURN
        else if ( SYM.EQ.'EXP')THEN
          VALSYM(INDEX4)=EXP(ARG1)
          RETURN
        else if ( SYM.EQ.'INT')THEN
          VALSYM(INDEX4)=ANINT(ARG1)
          RETURN
        else if ( SYM.EQ.'MAX')THEN
          VALSYM(INDEX4)=ARG1
          IF(VALSYM(INDEX4).LT.ARG2)VALSYM(INDEX4)=ARG2
          RETURN
        else if ( SYM.EQ.'MIN')THEN
          VALSYM(INDEX4)=ARG1
          IF(VALSYM(INDEX4).GT.ARG2)VALSYM(INDEX4)=ARG2
          RETURN
        else if ( SYM.EQ.'NEG')THEN
          VALSYM(INDEX4)=-VALSYM(INDEX1)
          RETURN
        else if ( SYM.EQ.'SIN'.OR.SYM.EQ.'SINE')THEN
          VALSYM(INDEX4)=SIN(ARG1)
          RETURN
        else if ( SYM.EQ.'SINH')THEN
          VALSYM(INDEX4)=SINH(ARG1)
          RETURN
        else if ( SYM.EQ.'SQRT')THEN
          IF(ARG1.LT.0.0)THEN
            IERROR=1
            WRITE(OUTPUT,1000)SYM,ARG1
            call CHRWRT(OUTPUT,1,IOUNIT)
          ELSE
            VALSYM(INDEX4)=SQRT(ARG1)
            end if
          RETURN
        else if ( SYM.EQ.'STEP')THEN
          IF(ARG1.LT.0.0)VALSYM(INDEX4)=0.0
          IF(ARG1.GE.0.0)VALSYM(INDEX4)=1.0
          RETURN
        else if ( SYM.EQ.'TAN')THEN
          VALSYM(INDEX4)=TAN(ARG1)
          RETURN
        else if ( SYM.EQ.'TANH')THEN
          VALSYM(INDEX4)=TANH(ARG1)
          RETURN
        else if ( SYM.EQ.'!')THEN
          IF(ARG1.LT.0.0.OR.ARG1.GT.25.0)THEN
            OUTPUT='FUNVAL - Argument out of range for factorial.'
            call CHRWRT(OUTPUT,0,IOUNIT)
            IERROR=1
            RETURN
            end if
          ARGTMP=ARG1
          FACT=1.0

5         CONTINUE

          IF(ARGTMP.GT.1.0)THEN
            FACT=FACT*ARGTMP
            ARGTMP=ARGTMP-1.0
            GO TO 5
          end if

          VALSYM(INDEX4)=FACT
          RETURN

        end if

      IERROR=1
      OUTPUT='FUNVAL - Unknown operation: '//SYM
      call CHRWRT(OUTPUT,1,IOUNIT)
      RETURN
1000  FORMAT('FUNVAL - Illegal ',A10,' of ',G14.6)
1040  FORMAT('FUNVAL - Illegal exponentiation ',G14.6,' **',G14.6)
      END
      SUBROUTINE GETHLP(FILHLP,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc GETHLP presents help information to the user from a file.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      parameter (MAXTOP=40)
      character CHOICE*75
      character CTEMP*75
      character CTEMP2*75
      character FILHLP*30
      character INLINE*75
      DIMENSION IOUNIT(*)
      character LAB*1
      character LEVELC(MAXTOP)*75
      DIMENSION LEVELM(10)
      DIMENSION LEVELT(MAXTOP)
      character LINE*80
      character OUTPUT*100
      character PROMPT*80
      COMMON /CHRCOM/ NPAGE
      IERROR=0
      LHUNIT=55
c
c  Open help file
c
      OPEN(UNIT=LHUNIT,FILE=FILHLP,STATUS='OLD',ERR=100)
c    *SHARED,READONLY)
      LEVELO=0
      LEVEL=1
      ILINE=1
c
c  Move to beginning of current topic by reading MOVE lines from the
c  top of the file.  Record this position, corresponding to the current
c  LEVEL, in LEVELM, in case we later want to back up.  Print out the
c  heading line of this topic.
c
10    CONTINUE

      JERROR=0
      MOVE=ILINE
      LEVELM(LEVEL)=ILINE
      DO I=1,MOVE-1
        READ(LHUNIT,'(1X)',END=110,ERR=110)
      end do
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      READ(LHUNIT,'(2A)',END=110,ERR=110)LAB,INLINE
      call CHRWRT(INLINE,0,IOUNIT)
c
c  If 'going down' or redisplaying, (as opposed to backing up), display the
c  information available under the current topic.  We stop printing
c  when we hit a numeric label.  If this label is less than or equal to the
c  current level, there are no subtopics.  Otherwise, we now move ahead
c  to print out the list of subtopics available for this topic.
c
      IF(LEVEL.GE.LEVELO)THEN
        NTOP=-1

30      CONTINUE

        READ(LHUNIT,'(2A)',END=50)LAB,INLINE
        MOVE=MOVE+1
        IF(LGE(LAB,'0').AND.LLE(LAB,'9'))THEN
          READ(LAB,'(I1)')NUM
          IF(NUM.LE.LEVEL)GO TO 50
          NTOP=0
          GO TO 40
          end if
        call CHRWRT(INLINE,0,IOUNIT)
        GO TO 30
      ELSE
        NTOP=0
        INLINE=' '
        LAB=' '
      end if
c
c  Locate each subtopic by examining column 1, searching for integer labels.
c
c  Assuming we are at level LEVEL, we are searching for labels equal
c  to LEVEL+1.  As we encounter each such label, we want to store the
c  rest of the line as a subtopic.  We ignore labels greater than LEVEL+1
c  because these are sub-subtopics, and we cease our search when we
c  reach a label less than or equal to LEVEL.
c
40    CONTINUE

      IF(LGE(LAB,'0').AND.LLE(LAB,'9'))THEN
        READ(LAB,'(I1)')NUM
        IF(NUM.LE.LEVEL)GO TO 50
        IF(NUM.EQ.LEVEL+1)THEN
          NTOP=NTOP+1
          IF(NTOP.EQ.1)THEN
            OUTPUT=' '
            call CHRWRT(OUTPUT,0,IOUNIT)
            OUTPUT='Help is available on:'
            call CHRWRT(OUTPUT,0,IOUNIT)
            OUTPUT=' '
            call CHRWRT(OUTPUT,0,IOUNIT)
            end if
          WRITE(OUTPUT,'(1X,A75)')INLINE
          call CHRWRT(OUTPUT,0,IOUNIT)
          LEVELT(NTOP)=MOVE
          LEVELC(NTOP)=INLINE
          end if
        end if
      READ(LHUNIT,'(2A)',END=50,ERR=50)LAB,INLINE
      MOVE=MOVE+1
      GO TO 40

50    CONTINUE
c
c  Display subtopics
c
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='RETURN to back up, ? to redisplay, CTRL-Z to quit.'
      call CHRWRT(OUTPUT,0,IOUNIT)
c
c  Prompt for user choice of new topic, exit, or back up
c
60    CONTINUE

      IERROR=0
      NLINE=0

      IF(NTOP.GT.0)THEN
        PROMPT='topic you want help on, or RETURN or ? or CTRL-Z.'
      ELSE
        PROMPT='RETURN or ? or CTRL-Z.'
      end if

      call CHRREA(CHOICE,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)

      IF(IERROR.NE.0)THEN
        IERROR=0
        CLOSE(UNIT=LHUNIT)
        RETURN
      end if

      NPAGE=0
      call CHRDB2(CHOICE)
      call CHRLEN(CHOICE,LENC)
      IF(LENC.LE.0)CHOICE='!'
      CTEMP=CHOICE
      call CAPCHR(CTEMP)
c
c  Two errors in a row, OK, but three suggests that something is wrong.
c
      IF(IERROR.NE.0)THEN
        JERROR=JERROR+1
        IF(JERROR.LE.4)GO TO 60
        OUTPUT='Too many input errors in a row!'
        call CHRWRT(OUTPUT,0,IOUNIT)
      end if
c
c  Consider ending this help session
c
      IF((CTEMP.EQ.'!'.AND.LEVEL.EQ.1).OR.
     *   JERROR.GT.4)THEN
        CLOSE(UNIT=LHUNIT)
        RETURN
      end if
c
c  User wants to back up to a supertopic.  We must rewind.
c
      REWIND LHUNIT
      LEVELO=LEVEL
      IF(CTEMP.EQ.'!')THEN
        LEVEL=LEVEL-1
        ILINE=LEVELM(LEVEL)
c
c  Redisplay current topic
c
      else if ( CTEMP.EQ.'?')THEN
        GO TO 10
c
c  User wants to go down to a subtopic.
c
      ELSE
        DO I=1,NTOP
          CTEMP2=LEVELC(I)
          call CHRDB2(CTEMP2)
          call CAPCHR(CTEMP2)
          ITOP=I
          DO J=1,LENC
            IF(CTEMP(J:J).NE.CTEMP2(J:J))GO TO 80
          end do
          GO TO 90
80        CONTINUE
        end do

        OUTPUT='Sorry, no help available on '//CHOICE
        call CHRWRT(OUTPUT,0,IOUNIT)
        JERROR=JERROR+1
        GO TO 60
90      CONTINUE
        LEVEL=LEVEL+1
        ILINE=LEVELT(ITOP)
        end if
      GO TO 10
c
c  Error opening help file
c
100   CONTINUE

      IERROR=1
      OUTPUT='Could not open the help file '//FILHLP
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
c
c  Error reading help file
c
110   CONTINUE

      IERROR=1
      OUTPUT='Unexpected end or error while reading '//FILHLP
      call CHRWRT(OUTPUT,0,IOUNIT)
      CLOSE(UNIT=LHUNIT)
      RETURN
c
c  End of input from user
c
120   CONTINUE

      CLOSE(UNIT=LHUNIT)
      RETURN
      END
      SUBROUTINE GRAFF(IERROR,IHAVE,IOUNIT,IPLOT,LINE,MAXPLT,MAXTAB,
     *MAXVAR,NAMES,NLINE,NTAB,OUTPUT,PROMPT,TABLE)

c*********************************************************************72
c
cc GRAFF ???
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRREL*14
      character IGRAFX*3
      character IGRAFY*3
      DIMENSION IHAVE(*)
      DIMENSION IOUNIT(*)
      DIMENSION IPLOT(2,MAXPLT)
      character ISAY*1
      character LINE*(*)
      character NAMES(MAXVAR)*3
      character OUTPUT*(*)
      character PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

      COMMON /ANYCOM/ IPLT1,IPLT2,IXPLT1,IXPLT2,IYPLT1,
     *                IYPLT2,MARRAY,XPLT1,XPLT2,YPLT1,YPLT2

      IF(NTAB.LE.1)THEN
        OUTPUT='No data to plot!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if

      NPLOT=0

10    CONTINUE

      NLINE=0
      PROMPT='horizontal variable (or RETURN), vertical variable'
      call CHRREA(IGRAFX,LINE,NLINE,PROMPT,IOUNIT,IERROR,1,1)
      IF(IERROR.NE.0)RETURN
      call CHRLEN(IGRAFX,LCHAR)
      IF(LCHAR.LE.0)GO TO 30
      call CAPCHR(IGRAFX)
      call CHRDB1(LINE)
      call CHRREA(IGRAFY,LINE,NLINE,PROMPT,IOUNIT,IERROR,1,1)
      IF(IERROR.NE.0)RETURN
      call CAPCHR(IGRAFY)

      IF(IGRAFX.EQ.'Y')IGRAFX='Y1'
      IF(IGRAFY.EQ.'Y')IGRAFY='Y1'
      IND1=0
      IND2=0

      DO I=1,MAXVAR
        IF(IHAVE(I).EQ.1)THEN
          IF(IGRAFX.EQ.NAMES(I))IND1=I
          IF(IGRAFY.EQ.NAMES(I))IND2=I
        end if
      end do

      IF(IND1.EQ.0)THEN
        OUTPUT='I do not recognize the variable '//IGRAFX
        call CHRWRT(OUTPUT,0,IOUNIT)
        GO TO 10
      else if ( IND2.EQ.0)THEN
        OUTPUT='I do not recognize the variable '//IGRAFY
        call CHRWRT(OUTPUT,0,IOUNIT)
        GO TO 10
        end if
      IF(IGRAFX.EQ.IGRAFY)THEN
        OUTPUT='You cannot graph '//IGRAFX//' against itself!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        GO TO 10
        end if
      NPLOT=NPLOT+1
      IPLOT(1,NPLOT)=IND1
      IPLOT(2,NPLOT)=IND2
      NLINE=0
      IF(NPLOT.LT.MAXPLT)GO TO 10

30    CONTINUE

      IF(NPLOT.LE.0)RETURN
      IND1=IPLOT(1,1)
      IND2=IPLOT(2,1)
      XMIN=TABLE(1,IND1)
      XMAX=XMIN
      YMIN=TABLE(1,IND2)
      YMAX=YMIN

      DO I=1,NPLOT
        IND1=IPLOT(1,I)
        IND2=IPLOT(2,I)
        DO J=1,NTAB
          XMAX=MAX(XMAX,TABLE(J,IND1))
          XMIN=MIN(XMIN,TABLE(J,IND1))
          YMAX=MAX(YMAX,TABLE(J,IND2))
          YMIN=MIN(YMIN,TABLE(J,IND2))
        end do
      end do

      PROMPT='Y to alter range of plot'
      call CHRREA(ISAY,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)
      IF(ISAY.EQ.'Y')THEN
        OUTPUT='Horizontal range is '//CHRREL(XMIN)//' to'
     *  //CHRREL(XMAX)
        call CHRWRT(OUTPUT,0,IOUNIT)
        PROMPT='new minimum, new maximum'
        call RELREA(XMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        call RELREA(XMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        OUTPUT='Vertical range is '//CHRREL(YMIN)//' to'
     *  //CHRREL(YMAX)
        call CHRWRT(OUTPUT,0,IOUNIT)
        PROMPT='new minimum, new maximum'
        call RELREA(YMIN,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        call RELREA(YMAX,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        end if

      XDEL=XMAX-XMIN
      XMIN=XMIN-0.05*XDEL
      XMAX=XMAX+0.05*XDEL
      YDEL=YMAX-YMIN
      YMIN=YMIN-0.05*YDEL
      YMAX=YMAX+0.05*YDEL

      XPLT1=0.00
      XPLT2=1.00
      YPLT1=0.00
      YPLT2=1.00
      IPLT1=1
      ICOM=2
      call ANYPLT(ICOM)

      XPLT1=XMIN
      XPLT2=XMAX-XMIN
      YPLT1=YMIN
      YPLT2=YMAX-YMIN
      ICOM=3
      call ANYPLT(ICOM)
c
c  Plot X and Y axis if they appear on the graph.
c
      IF(XMIN.LE.0.0.AND.0.0.LE.XMAX)THEN
        XPLT1=0.0
        YPLT1=YMIN
        ICOM=4
        call ANYPLT(ICOM)
        XPLT1=0.0
        YPLT1=YMAX
        ICOM=5
        call ANYPLT(ICOM)
      end if

      IF(YMIN.LE.0.0.AND.0.0.LE.YMAX)THEN
        XPLT1=XMIN
        YPLT1=0.0
        ICOM=4
        call ANYPLT(ICOM)
        XPLT1=XMAX
        YPLT1=0.0
        ICOM=5
        call ANYPLT(ICOM)
      end if

      DO I=1,NPLOT
        ICOM=4
        IND1=IPLOT(1,I)
        IND2=IPLOT(2,I)
        DO J=1,NTAB
          XPLT1=TABLE(J,IND1)
          YPLT1=TABLE(J,IND2)
          call ANYPLT(ICOM)
          ICOM=5
        end do
      end do

      ICOM=9
      call ANYPLT(ICOM)
      RETURN
      END
      SUBROUTINE HELPER(IOUNIT,OUTPUT)

c*********************************************************************72
c
cc HELPER prints a list of commands.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IOUNIT(*)
      character OUTPUT*(*)

      OUTPUT='B - set up new problem'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='C - integrate current problem '
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='D - Open/close transcript file.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='F - Graph the flow field T versus Y''.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='G - Graph solution components.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='H - Help (print this list).'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='O - Read solution data from file.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='P - Write solution data to file.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Q - Quit.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='R - Read commands from file'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='S - Set up sample problem'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='T - Type out parameters'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='V - What parameters can I set?'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='W - Write problem data to file.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='? - Display extensive help'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='variable = value  Assign value to variable.'
      call CHRWRT(OUTPUT,0,IOUNIT)

      RETURN
      END
      SUBROUTINE INDUMP(FILNAM,IERROR,IHAVE,IOUNIT,ITEMP,LINE,MAXTAB,
     *MAXVAR,NLINE,NTAB,PROMPT,TABLE)

c*********************************************************************72
c
cc INDUMP reads a dump file saved from a previous run.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character FILNAM*(*)
      DIMENSION IHAVE(MAXVAR)
      DIMENSION IOUNIT(*)
      DIMENSION ITEMP(MAXVAR)
      character LINE*(*)
      character PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

      PROMPT='filename to read saved data from.'
      call CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,0)
      IF(IERROR.NE.0)RETURN
      OPEN(UNIT=17,FILE=FILNAM,STATUS='OLD',ERR=30)
      READ(17,'(1X)',END=40)
      READ(17,'(1X)',END=40)
      READ(17,'(1X,15I4)',END=40)NTAB,(IHAVE(I),I=1,MAXVAR)
      NDO=0
      DO I=1,MAXVAR
        IF(IHAVE(I).NE.0)THEN
          NDO=NDO+1
          ITEMP(NDO)=I
        end if
      end do

      READ(17,'(1X)',END=40)
      DO I=1,NTAB
        READ(17,'(1X,6G12.4)',END=40)(TABLE(I,ITEMP(J)),J=1,NDO)
      end do
      CLOSE(UNIT=17)
      WRITE(*,*)'Data successfully read in.'
      RETURN

30    CONTINUE

      WRITE(*,*)'INDUMP - Could not open dump file!'
      RETURN

40    CONTINUE

      WRITE(*,*)'INDUMP - Information was missing from the file!'
      CLOSE(UNIT=17)
      RETURN
      END
      SUBROUTINE INIT(FILHLP,IERROR,IOUNIT,MAXTAB,MAXVAR,METNAM,NAMES,
     *NPAGE,ROVER,TABLE,VSCALE)

c*********************************************************************72
c
cc INIT initializes data.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character FILHLP*(*)
      DIMENSION IOUNIT(*)
      character METNAM(*)*(*)
      character NAMES(*)*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

      FILHLP='rk124.hlp'
      IERROR=0
      IOUNIT(1)=0
      IOUNIT(2)=0
      IOUNIT(3)=-1
      IOUNIT(4)=-1
      METNAM(1)='Euler method'
      METNAM(2)='Runge-Kutta method of order 2'
      METNAM(3)='Runge Kutta method of order 4'
      NAMES(1)='T'
      NAMES(2)='Y1'
      NAMES(3)='Y1'''
      NAMES(4)='Y2'
      NAMES(5)='Y2'''
      NAMES(6)='Y3'
      NAMES(7)='Y3'''
      NAMES(8)='Y4'
      NAMES(9)='Y4'''
      NAMES(10)='X1'
      NAMES(11)='X2'
      NAMES(12)='X3'
      NAMES(13)='X4'
      NPAGE=0
      ROVER=1.0E+10
      DO I=1,MAXTAB
        DO J=1,MAXVAR
          TABLE(I,J)=0.0
        end do
      end do
      VSCALE=0.9
      RETURN
      END
      SUBROUTINE INTREA(INTVAL,LINE,NLINE,PROMPT,IOUNIT,IERROR)

c*********************************************************************72
c
cc INTREA reads an integer value from the user.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IOUNIT(*)
      character LINE*80
      character OUTPUT*90
      character PROMPT*80

      INTVAL=0

10    CONTINUE

      call CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      IF(IERROR.NE.0)RETURN
      IF(NLINE.LE.0)GO TO 10
      call CHRCTI(LINE,NLINE,INTVAL,IERROR,LCHAR)
      call s_chop ( LINE, 1, LCHAR )
      call CHRLEN(LINE,NLINE)
      RETURN
      END
      SUBROUTINE NOTE(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc NOTE allows the user to annotate the output file.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IOUNIT(*)
      character LINE*(*)
      character OUTPUT*(*)
      character PROMPT*(*)

      OUTPUT='**************************************************'
      call CHRWRT(OUTPUT,0,IOUNIT)
      PROMPT='comments, followed by period in column 1.'

10    CONTINUE

      NLINE=0
      call CHRREA(OUTPUT,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)
      PROMPT=' '
      IF(IERROR.NE.0)RETURN
      IF(OUTPUT(1:1).NE.'.')GO TO 10
      OUTPUT='**************************************************'
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE QUIT(FILNAM,IERROR,IOUNIT,NLINE,LINE,OUTPUT,
     *PROMPT)

c*********************************************************************72
c
cc QUIT terminates execution of the program.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character COM*1
      character FILNAM*(*)
      DIMENSION IOUNIT(*)
      character LINE*(*)
      character OUTPUT*(*)
      character PROMPT*(*)

      NLINE=0
      PROMPT='Y to confirm you want to stop'
      call CHRREA(COM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)
      IF(IERROR.NE.0)RETURN
      call CAPCHR(COM)
      IF(COM.NE.'Y')RETURN
c
c  Shut down
c
      OUTPUT='RK124 is stopping now.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      IF(IOUNIT(3).NE.(-1))
     *CALL DISK(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      ICOM=1
      call ANYPLT(ICOM)
      STOP
      END
      SUBROUTINE READER(FILNAM,IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)

c*********************************************************************72
c
cc READER handles cases where the user wants input to come from a file.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character FILNAM*(*)
      INTEGER   IERROR
      INTEGER   IOUNIT(*)
      character LINE*(*)
      INTEGER   NLINE
      character OUTPUT*(*)
      character PROMPT*(*)

      PROMPT='input filename.'
      call CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,0)
      IF(IERROR.NE.0)RETURN
      IOUNIT(1)=31
      OPEN(UNIT=IOUNIT(1),FILE=FILNAM,STATUS='OLD',ERR=10)
      OUTPUT='Input will come from '//FILNAM
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
c
c  File not found
c
10    CONTINUE

      OUTPUT='File could not be opened.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE RELREA(RVAL,LINE,NLINE,PROMPT,IOUNIT,IERROR)

c*********************************************************************72
c
cc RELREA reads a real value from the user.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IOUNIT(*)
      character LINE*80
      character OUTPUT*90
      character PROMPT*80
      RVAL=0.0

10    CONTINUE

      call CHRINP(IERROR,IOUNIT,LINE,NLINE,OUTPUT,PROMPT)
      IF(IERROR.NE.0)RETURN
      IF(NLINE.LE.0)GO TO 10
      call CHRCTR(LINE,NLINE,TOP,IERROR,LCHAR)

      IF(LINE(LCHAR+1:LCHAR+1).NE.'/')THEN
        RVAL=TOP
      ELSE
        LCHAR=LCHAR+1
        call s_chop ( LINE, 1, LCHAR )
        call CHRCTR(LINE,NLINE,BOT,IERROR,LCHAR)
        IF(BOT.EQ.0.0)BOT=1.0
        RVAL=TOP/BOT
      end if

      call s_chop ( LINE, 1, LCHAR )
      call CHRLEN(LINE,NLINE)

      RETURN
      END
      SUBROUTINE RPNCHK(IERROR,IHI,ILO,IOPSYM,IRPN,MAXRPN,MAXSYM)

c*********************************************************************72
c
cc RPNCHK examines an RPN formula, looking for a complete RPN expression.
c
c  Discussion:
c
c    The routine starts at location IHI, and finds the position ILO
c    such that IRPN(ILO)...IRPN(IHI) represents a single argument.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
c    Output, integer IERROR, 0, no error; 1 an error.
c
c    Input, integer IHI, the location in IRPN where the search begins.
c
c    Output, integer ILO, the location in IRPN such that IRPN(ILO) through
c    IRPN(IHI) represents a single argument, or IHI+1 if no such location
c    was found.
c
c    Input, integer IOPSYM(MAXSYM), the number of operands per symbol.
c    Constants have 0, unaries have 1, and so on.
c
c    Workspace, integer IRPN(MAXRPN), used to store the compiled
c    versions of user formulas.
c
c    Input, integer MAXRPN, specifies the length of IRPN.
c
c    Input, integer MAXSYM, the maximum number of symbols allowed.
c
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IRPN(MAXRPN)
      ISUM=0
      IERROR=0
      ILO=IHI+1

10    CONTINUE

      ILO=ILO-1
      IF(ILO.LE.0)THEN
        IERROR=1
        RETURN
      end if

      IF(IOPSYM(IRPN(ILO)).LT.0.AND.ISUM.EQ.0)GO TO 10
      ISUM=ISUM+1-IOPSYM(IRPN(ILO))
      IF(ISUM.NE.1)GO TO 10
      RETURN
      END
      SUBROUTINE RPNSET(IERROR,INTSYM,IOPSYM,IPRSYM,IRPN,ISTACK,
     *MAXFIX,MAXSYM,NINT,NRPN,SYMBOL,IOUNIT,OUTPUT,IFINIS)

c*********************************************************************72
c
cc RPNSET converts the infix formula into an RPN formula.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION INTSYM(MAXSYM)
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IRPN(MAXFIX)
      DIMENSION ISTACK(MAXSYM)
      character OUTPUT*100
      character SYM*10
      character SYM2*10
      character SYMBOL(MAXSYM)*10

      NRPN=0
      IERROR=0
      DO I=1,MAXFIX
        IRPN(I)=0
      end do
c
c  Null formula
c
      IF(NINT.LE.0)THEN
        OUTPUT='RPNSET - Null formula?'
        call CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=1
        RETURN
        end if
      IREAD=0
      ISTAK=0

20    CONTINUE

      IREAD=IREAD+1
      IF(IREAD.GT.NINT)THEN
        IERROR=1
        OUTPUT='RPNSET - Formula makes no sense.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if
      ISYM=INTSYM(IREAD)
      IF(ISYM.EQ.0)GO TO 20
      SYM=SYMBOL(ISYM)
      IF(SYM.EQ.',')GO TO 20
      IF(SYM.EQ.'$')GO TO 60
      IF(SYM.EQ.'(')THEN
        ISTAK=ISTAK+1
        ISTACK(ISTAK)=ISYM
        GO TO 20
      end if
c
c  Variable or constant goes immediately into IRPN
c
      IF(IOPSYM(ISYM).EQ.0)THEN
        NRPN=NRPN+1
        IRPN(NRPN)=ISYM
        GO TO 20
      end if

40    CONTINUE

      IF(ISTAK.LE.0)THEN
        ISTAK=ISTAK+1
        ISTACK(ISTAK)=ISYM
        GO TO 20
      end if

      JSYM=ISTACK(ISTAK)
      IF(IPRSYM(JSYM).GE.IPRSYM(ISYM))THEN
        NRPN=NRPN+1
        IRPN(NRPN)=ISTACK(ISTAK)
        ISTAK=ISTAK-1
        GO TO 40
        end if
      JSYM=ISTACK(ISTAK)
      SYM2=SYMBOL(JSYM)
      IF(SYM.EQ.')'.AND.SYM2.EQ.'(')THEN
        ISTAK=ISTAK-1
        GO TO 20
        end if
      ISTAK=ISTAK+1
      ISTACK(ISTAK)=ISYM
      GO TO 20
c
c  Done, pop stack
c
60    CONTINUE

      IF(ISTAK.LE.0)THEN
        NRPN=NRPN+1
        IRPN(NRPN)=IFINIS
        IF(IREAD.LT.NINT)THEN
          OUTPUT='RPNSET - Some of formula left over'
          call CHRWRT(OUTPUT,0,IOUNIT)
          IERROR=1
          end if
        RETURN
        end if
      NRPN=NRPN+1
      IRPN(NRPN)=ISTACK(ISTAK)
      ISTAK=ISTAK-1
      GO TO 60
      END
      SUBROUTINE RPNVAL(IERROR,IOPSYM,IPRSYM,IPVAL,IRPN,ISTACK,
     *LENSYM,MAXRPN,MAXSYM,MAXVAL,IFREE,NRPN,NSYM,NSYMS,SYMBOL,VALSYM,
     *VALUE,IOUNIT,OUTPUT)

c*********************************************************************72
c
cc RPNVAL evaluates the symbolic functions in an RPN formula.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CTEMP*3
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION IRPN(MAXRPN)
      DIMENSION ISTACK(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      character OUTPUT*100
      character SYM*10
      character SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)

      IREAD=0
      ISTAK=0
      VALUE=0.0
      IF(NRPN.LE.1)RETURN
      NSYMS=NSYM
      ITEMP=0

10    CONTINUE

      IREAD=IREAD+1
      IF(IREAD.GT.NRPN)THEN
        IERROR=1
        OUTPUT='RPNVAL - Read past end of formula.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if

      ISYM=IRPN(IREAD)
      SYM=' '
      IF(ISYM.GT.0)SYM=SYMBOL(ISYM)

      IF(SYM.EQ.'$'.OR.IREAD.GT.NRPN)THEN
        INDEX4=IPVAL(ISYMO)
        VALUE=VALSYM(INDEX4)
        RETURN
      end if
c
c  Constants and variables go into stack
c
      IF(IOPSYM(ISYM).EQ.0)THEN
        IF(ISYM.LE.NSYM)THEN
          ISTAK=ISTAK+1
          ISTACK(ISTAK)=ISYM
          ISYMO=ISYM
        ELSE
          IF(NSYMS.GE.MAXSYM)THEN
            OUTPUT='RPNVAL - Not enough storage.'
            call CHRWRT(OUTPUT,0,IOUNIT)
            IERROR=1
            RETURN
            end if
          NSYMS=NSYMS+1
          ISYMO=NSYMS
          ISTAK=ISTAK+1
          ISTACK(ISTAK)=NSYMS
          IPRSYM(NSYMS)=10
          IOPSYM(NSYMS)=0
          LENSYM(NSYMS)=6
          ITEMP=ITEMP+1
          call CHRITC(CTEMP,3,ITEMP)
          IF(ITEMP.LE.99)CTEMP(1:1)='0'
          IF(ITEMP.LE.9)CTEMP(2:2)='0'
          SYMBOL(NSYMS)='STK000'
          SYMBOL(NSYMS)(4:6)=CTEMP
          IPVAL(NSYMS)=IFREE
          IFREE=IFREE+1
          INDEX1=IPVAL(ISYM)
          INDEX4=IPVAL(NSYMS)
          VALSYM(INDEX4)=VALSYM(INDEX1)
          end if
        GO TO 10
        end if
c
c  Pull off arguments
c
      IARG1=ISTACK(ISTAK)
      IARG2=0

      IF(IOPSYM(ISYM).EQ.2)THEN
        IARG2=ISTACK(ISTAK)
        ISTAK=ISTAK-1
        IARG1=ISTACK(ISTAK)
      end if

      SYM=SYMBOL(ISYM)
      call FUNVAL(IARG1,IARG2,IERROR,IOPSYM,IPRSYM,IPVAL,
     *ITEMP,LENSYM,MAXSYM,MAXVAL,NSYMS,SYM,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IPSET,IFREE)
      ISYMO=NSYMS

      IF(IERROR.NE.0)THEN
        OUTPUT='RPNVAL - Evaluation abandoned.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        VALUE=0.0
        RETURN
      end if

      IF(IPSET.EQ.0)THEN
        IPVAL(NSYMS)=IFREE
        IFREE=IFREE+1
      ELSE
        IPVAL(NSYMS)=IPSET
        IPSET=0
      end if

      ISTACK(ISTAK)=NSYMS
      GO TO 10
      END
      subroutine s_chop ( s, ilo, ihi )

c*********************************************************************72
c
cc S_CHOP "chops out" a portion of a string, and closes up the hole.
c
c  Example:
c
c    S = 'Fred is not a jerk!' 
c
c    call s_chop ( S, 9, 12 ) 
c
c    S = 'Fred is a jerk!    '
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:  
c
c    06 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character * ( * ) S, the string to be transformed.
c
c    Input, integer ILO, IHI, the locations of the first and last
c    characters to be removed.  
c
      implicit none

      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      character * ( * )  s
      integer s_length

      s_length = len ( s )

      ilo2 = max ( ilo, 1 )
      ihi2 = min ( ihi, s_length )

      if ( ihi2 .lt. ilo2 ) then
        return
      end if

      s(ilo2:s_length+ilo2-ihi2-1) = s(ihi2+1:s_length)
      s(s_length+ilo2-ihi2:s_length) = ' '

      return
      end
      SUBROUTINE SAMPLE(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NEQN,
     *NSTEPS,NTAB,OUTPUT,TABLE,TSTOP)

c*********************************************************************72
c
cc SAMPLE sets up a sample problem.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character COM*1
      DIMENSION IHAVE(MAXVAR)
      character INFIX*(*)
      character IRHS(*)*(*)
      DIMENSION IRPN(MAXRPN)
      character NAMVAR*10
      DIMENSION TABLE(MAXTAB,MAXVAR)
      character OUTPUT*(*)

      NEQN=1
      DO I=1,MAXVAR
        IHAVE(I)=0
      end do
      IHAVE(1)=1
      IHAVE(2)=1
      IHAVE(3)=1
      INFIX='COS(T)'
      IRHS(1)=INFIX
      IFRM=1
      call COMRPN('G',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,IOUNIT)
      IF(IERROR.NE.0)RETURN
      IHAVE(1+2*MAXEQN+1)=1
      INFIX='SIN(T)'
      IRHS(MAXEQN+1)=INFIX
      IFRM=MAXEQN+1
      call COMRPN('G',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,IOUNIT)
      IF(IERROR.NE.0)RETURN
      IPRINT=5
      TABLE(1,1)=0.0
      TSTOP=4.0*ATAN(1.0)
      METHOD=3
      NSTEPS=25
      NTAB=1
      TABLE(NTAB,2)=0.0
      TABLE(NTAB,1+2*MAXEQN+1)=TABLE(NTAB,2)
      OUTPUT='The sample problem has now been set up.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE SETVAL(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP,
     *VSCALE)

c*********************************************************************72
c
cc SETVAL
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRINT*6
      DIMENSION IHAVE(*)
      character INFIX*(*)
      DIMENSION IOUNIT(*)
      character IRHS(*)*(*)
      DIMENSION IRPN(*)
      character LINE*(*)
      LOGICAL   LGE
      LOGICAL   LLT
      character METNAM(MAXMET)*60
      character NAMVAR*(*)
      character OUTPUT*(*)
      character PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

201   CONTINUE
c
c  Read value
c
      PROMPT=NAMVAR
      IF(NAMVAR.EQ.'X1'.OR.NAMVAR.EQ.'Y1'''.OR.
     *   NAMVAR.EQ.'X2'.OR.NAMVAR.EQ.'Y2'''.OR.
     *   NAMVAR.EQ.'X3'.OR.NAMVAR.EQ.'Y3'''.OR.
     *   NAMVAR.EQ.'X4'.OR.NAMVAR.EQ.'Y4''')THEN
        call CHRREA(INFIX,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,1)
      else if ( LLT(NAMVAR,'I').OR.LGE(NAMVAR,'O'))THEN
        call RELREA(TEMP,LINE,NLINE,PROMPT,IOUNIT,IERROR)
      ELSE
        call INTREA(ITEMP,LINE,NLINE,PROMPT,IOUNIT,IERROR)
        end if
      IF(IERROR.NE.0)RETURN
c
c  Assign value to proper variable
c
      IF(NAMVAR.EQ.'IPRINT')THEN
        IPRINT=ITEMP
      else if ( NAMVAR.EQ.'METHOD')THEN
        IF(ITEMP.GE.1.AND.ITEMP.LE.MAXMET)THEN
          METHOD=ITEMP
          OUTPUT='Method='//METNAM(ITEMP)
          call CHRWRT(OUTPUT,0,IOUNIT)
        ELSE
          OUTPUT='The following methods are available'
          call CHRWRT(OUTPUT,0,IOUNIT)
          DO I=1,MAXMET
            OUTPUT=CHRINT(I)//'  '//METNAM(I)
            call CHRWRT(OUTPUT,0,IOUNIT)
          end do
          GO TO 201
          end if
      else if ( NAMVAR.EQ.'NEQN')THEN
        IF(ITEMP.GE.1.AND.ITEMP.LE.MAXEQN)THEN
          NEQN=ITEMP
        ELSE
          OUTPUT='NEQN must be between 1 and '//CHRINT(MAXEQN)
          call CHRDB2(OUTPUT)
          call CHRWRT(OUTPUT,0,IOUNIT)
          end if
      else if ( NAMVAR.EQ.'NSTEPS')THEN
        NSTEPS=ITEMP
      else if ( NAMVAR.EQ.'ROVER')THEN
        ROVER=TEMP
      else if ( NAMVAR.EQ.'T')THEN
        NTAB=1
        TABLE(1,1)=TEMP
        IHAVE(1)=1
      else if ( NAMVAR.EQ.'TINIT')THEN
        NTAB=1
        TABLE(1,1)=TEMP
        IHAVE(1)=1
      else if ( NAMVAR.EQ.'TSTOP')THEN
        TSTOP=TEMP
      else if ( NAMVAR.EQ.'VSCALE')THEN
        VSCALE=TEMP
      else if ( NAMVAR.EQ.'Y1')THEN
        NTAB=1
        TABLE(1,2)=TEMP
        IHAVE(2)=1
      else if ( NAMVAR.EQ.'Y2')THEN
        NTAB=1
        TABLE(1,4)=TEMP
        IHAVE(4)=1
      else if ( NAMVAR.EQ.'Y3')THEN
        NTAB=1
        TABLE(1,6)=TEMP
        IHAVE(6)=1
      else if ( NAMVAR.EQ.'Y4')THEN
        NTAB=1
        TABLE(1,8)=TEMP
        IHAVE(8)=1
      else if ( NAMVAR.EQ.'Y1'''.OR.NAMVAR.EQ.'X1'.OR.
     *       NAMVAR.EQ.'Y2'''.OR.NAMVAR.EQ.'X2'.OR.
     *       NAMVAR.EQ.'Y3'''.OR.NAMVAR.EQ.'X3'.OR.
     *       NAMVAR.EQ.'Y4'''.OR.NAMVAR.EQ.'X4')THEN
        READ(NAMVAR,'(1X,I1)')IEQN
        IF(NAMVAR(1:1).EQ.'Y')THEN
          IFRM=IEQN
        ELSE
          IFRM=MAXEQN+IEQN
          IHAVE(2*IEQN+1)=1
          end if
        IRHS(IFRM)=INFIX
        IF(INFIX.NE.' ')THEN
          call COMRPN('G',IERROR,IFRM,INFIX,IRPN,MAXRPN,NAMVAR,VALUE,
     *    IOUNIT)
          end if
        IF(IERROR.NE.0)THEN
          OUTPUT='Formula was not acceptable.'
          call CHRWRT(OUTPUT,0,IOUNIT)
          ITAG=0
        else if ( IRHS(IFRM).EQ.' ')THEN
          OUTPUT='This variable will now be undefined.'
          call CHRWRT(OUTPUT,0,IOUNIT)
          ITAG=0
        ELSE
          ITAG=1
          end if
        IF(NAMVAR(1:1).EQ.'Y')THEN
          IHAVE(2*IEQN)=ITAG
          IHAVE(2*IEQN+1)=ITAG
          IFRM=IEQN
        ELSE
          IHAVE(1+2*MAXEQN+IEQN)=ITAG
          IFRM=MAXEQN+IEQN
          end if
      ELSE
        OUTPUT='RK124 did not recognize the variable you were'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='trying to set.  Use the "V" command for a list.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        end if
      RETURN
      END
      SUBROUTINE SETUP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *ISET,LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP,
     *VSCALE)

c*********************************************************************72
c
cc SETUP sets up a problem.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IHAVE(*)
      character INFIX*(*)
      DIMENSION IOUNIT(*)
      character IRHS(*)*(*)
      DIMENSION IRPN(*)
      character LINE*(*)
      DIMENSION METHOD(*)
      character METNAM(*)*(*)
      character NAMVAR*(*)
      character OUTPUT*(*)
      character PROMPT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

      ISET=0

10    CONTINUE

      IF(IERROR.NE.0)THEN
        OUTPUT='Error occurred while setting up problem.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if

      ISET=ISET+1
      IF(ISET.EQ.1)THEN
        OUTPUT='NEQN is the number of equations, between 1 and 4.'
        NAMVAR='NEQN'
      else if ( ISET.EQ.2)THEN
        OUTPUT='TINIT is the initial value of the independent variable.'
        NAMVAR='TINIT'
      else if ( ISET.EQ.3)THEN
        OUTPUT='Y1 is the initial value of the first ODE component.'
        NAMVAR='Y1'
        IF(NEQN.EQ.1)ISET=6
      else if ( ISET.EQ.4)THEN
        OUTPUT='The initial value of Y2 is needed.'
        NAMVAR='Y2'
        IF(NEQN.EQ.2)ISET=6
      else if ( ISET.EQ.5)THEN
        OUTPUT='The initial value of Y3 is needed.'
        NAMVAR='Y3'
        IF(NEQN.EQ.3)ISET=6
      else if ( ISET.EQ.6)THEN
        OUTPUT='The initial value of Y4 is needed.'
        NAMVAR='Y4'
      else if ( ISET.EQ.7)THEN
        OUTPUT='We need to enter the right hand sides of the form'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Y1''=F(T,Y1) (or perhaps a more complicated form).'
        NAMVAR='Y1'''
      else if ( ISET.EQ.8)THEN
        IF(NEQN.LE.1)GO TO 10
        OUTPUT='We need the formula for Y2'''
        NAMVAR='Y2'''
      else if ( ISET.EQ.9)THEN
        IF(NEQN.LE.2)GO TO 10
        OUTPUT='We need the formula for Y3'''
        NAMVAR='Y3'''
      else if ( ISET.EQ.10)THEN
        IF(NEQN.LE.3)GO TO 10
        OUTPUT='We need the formula for Y4'''
        NAMVAR='Y4'''
      else if ( ISET.EQ.11)THEN
        OUTPUT='If a formula for Y1 is known, enter it now, or a blank.'
        NAMVAR='X1'
      else if ( ISET.EQ.12)THEN
        IF(NEQN.LE.1)GO TO 10
        OUTPUT='We can accept the formula for Y2 now, or a blank.'
        NAMVAR='X2'
      else if ( ISET.EQ.13)THEN
        IF(NEQN.LE.2)GO TO 10
        OUTPUT='We can accept the formula for Y3 now, or a blank.'
        NAMVAR='X3'
      else if ( ISET.EQ.14)THEN
        IF(NEQN.LE.3)GO TO 10
        OUTPUT='We can accept the formula for Y4 now, or a blank.'
        NAMVAR='X4'
      else if ( ISET.EQ.15)THEN
        OUTPUT='The number of the ODE method is needed. (1-3)'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='0=Help, 1=Euler, 2=RK2, 3=RK4'
        NAMVAR='METHOD'
      else if ( ISET.EQ.16)THEN
        OUTPUT='Please specify the number of steps to take (1-200)'
        NAMVAR='NSTEPS'
      else if ( ISET.EQ.17)THEN
        OUTPUT='Please specify the value of T at which to stop.'
        NAMVAR='TSTOP'
      ELSE
        OUTPUT='Problem has been set up'
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Use the C command to integrate.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if
      call CHRWRT(OUTPUT,0,IOUNIT)
      call CHRLEN(LINE,NLINE)
      call SETVAL(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRHS,IRPN,
     *LINE,MAXEQN,MAXMET,MAXRPN,MAXTAB,MAXVAR,METHOD,METNAM,
     *NAMVAR,NEQN,NLINE,NSTEPS,NTAB,OUTPUT,PROMPT,ROVER,TABLE,TSTOP,
     *VSCALE)
      GO TO 10
      END
      SUBROUTINE SHOSET(IOUNIT,OUTPUT)

c*********************************************************************72
c
cc SHOSET prints out the current settings.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IOUNIT(*)
      character OUTPUT*(*)

      OUTPUT='Here are the parameters you can set via commands like'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='  "IPRINT=5" or TINIT=4.5 or "Y1''=SIN(T)"'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='IPRINT - printout frequency'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='METHOD - integration method'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NEQN   - number of equations'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NSTEPS - number of steps to take'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='ROVER  - overflow threshhold'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TINIT  - initial time'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TSTOP  - stopping time'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='VSCALE - flow field vector scale factor (default=0.9)'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Y1     - initial condition (also Y2, Y3, or Y4)'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Y1''    - formula for derivative of Y1.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='X1     - exact formula for Y1, if known.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
      SUBROUTINE STEP(IERROR,IHAVE,INFIX,IOUNIT,IPRINT,IRPN,
     *MAXEQN,MAXRPN,MAXTAB,MAXVAR,METHOD,NAMES,NEQN,
     *NSTEPS,NTAB,OUTPUT,ROVER,TABLE,TSTOP)

c*********************************************************************72
c
cc STEP ?
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      parameter (MXEQN=4)
      character COM*1
      DIMENSION IHAVE(MAXVAR)
      character INFIX*(*)
      DIMENSION IOUNIT(*)
      DIMENSION IRPN(MAXRPN)
      character NAMES(MAXVAR)*3
      character NAMVAR*10
      character OUTPUT*120
      DIMENSION TABLE(MAXTAB,MAXVAR)
      DIMENSION YK1(MXEQN)
      DIMENSION YK2(MXEQN)
      DIMENSION YK3(MXEQN)
      DIMENSION YK4(MXEQN)
      DIMENSION YK5(MXEQN)
      DIMENSION YK6(MXEQN)
      DIMENSION YTEMP(MXEQN)
      DIMENSION YPTEMP(MXEQN)

      NTAB=1

      IF(NEQN.LE.0)THEN
        IERROR=1
        OUTPUT='The ODE has not been set up yet!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if

      IF(ABS(TABLE(1,1)-TSTOP).LT.0.00001*ABS(TSTOP))THEN
        IERROR=1
        OUTPUT='Please set TSTOP so we go somewhere!'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
        end if

      TVAL=TABLE(1,1)
      DO I=1,NEQN
        YTEMP(I)=TABLE(1,2*I)
      end do

      call COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     *YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
      IF(IERROR.NE.0)RETURN
      DO I=1,NEQN
        TABLE(1,2*I+1)=YPTEMP(I)
      end do

      call COMRPN('V',IERROR,IFRM,INFIX,IRPN,MAXRPN,'T',TVAL,IOUNIT)
      IF(IERROR.NE.0)RETURN

      DO I=1,NEQN
        IVAR=1+2*MAXEQN+I
        IF ( IHAVE(IVAR) .eq. 1 ) then
          IFRM=MAXEQN+I
          call COMRPN('E',IERROR,IFRM,INFIX,IRPN,
     &    MAXRPN,NAMVAR,VALUE,IOUNIT)
          IF(IERROR.NE.0)RETURN
          TABLE(1,IVAR)=VALUE
        end if
      end do

      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      IF(NEQN.EQ.1.AND.IHAVE(1+2*MAXEQN+1).EQ.1)THEN
        OUTPUT='Step      T              Y1             X1'
      else if ( NEQN.EQ.1)THEN
        OUTPUT='Step      T              Y1             Y1'''
      ELSE
        WRITE(OUTPUT,1270)(' ',J,J=1,NEQN)
      end if
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      INDHI=2*NEQN
      ITEMP=NTAB-1
      IF(NEQN.EQ.1.AND.IHAVE(1+2*MAXEQN+1).EQ.1)THEN
        WRITE(OUTPUT,1280)ITEMP,TABLE(1,1),TABLE(1,2),
     &  TABLE(1,1+2*MAXEQN+1)
      else if ( NEQN.EQ.1)THEN
        WRITE(OUTPUT,1280)ITEMP,(TABLE(1,I),I=1,3)
      ELSE
        WRITE(OUTPUT,1280)ITEMP,TABLE(1,1),(TABLE(1,IND),IND=2,INDHI,2)
      end if
      call CHRWRT(OUTPUT,0,IOUNIT)
      TBEGIN=TABLE(1,1)
      DELT=(TSTOP-TBEGIN)/FLOAT(NSTEPS)

      DO ISTEP=1,NSTEPS
c
c  Update time = position where solution is sought
c
        TOLD=TABLE(NTAB,1)
        TIME=((NSTEPS-ISTEP)*TBEGIN+ISTEP*TSTOP)/REAL(NSTEPS)
        TABLE(NTAB+1,1)=TIME

        GO TO (180,230,260)METHOD
c
c  METHOD=1, Euler method
c
        if ( method .eq. 1 ) then

          DO I=1,NEQN
            TEMP1=TABLE(NTAB,2*I+1)
            TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP1
          end do
c
c  METHOD=2, Runge Kutta method order 2
c
        else if ( method .eq. 2 ) then

          TVAL=TIME
          DO I=1,NEQN
            YTEMP(I)=TABLE(NTAB,2*I)+DELT*TABLE(NTAB,2*I+1)
          end do
          call COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     &    YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
          IF(IERROR.NE.0)RETURN
          DO I=1,NEQN
            TEMP1=0.5*(TABLE(NTAB,2*I+1)+YPTEMP(I))
            TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP1
          end do
c
c  METHOD=3, Runge-Kutta of order 4
c
        else if ( method .eq. 3 ) then

          DO I=1,NEQN
            YK1(I)=TABLE(NTAB,2*I+1)
          end do
          TVAL=TOLD+0.50*DELT
          DO I=1,NEQN
            YTEMP(I)=TABLE(NTAB,2*I)+0.5*DELT*YK1(I)
          end do
          call COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     &    YK2,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
          IF(IERROR.NE.0)RETURN
          DO I=1,NEQN
            YTEMP(I)=TABLE(NTAB,2*I)+0.5*DELT*YK2(I)
          end do
          call COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     &    YK3,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
          IF(IERROR.NE.0)RETURN
          TVAL=TOLD+DELT
          DO I=1,NEQN
            YTEMP(I)=TABLE(NTAB,2*I)+DELT*YK3(I)
          end do
          call COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TVAL,YTEMP,
     &    YK4,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
          IF(IERROR.NE.0)RETURN
          DO I=1,NEQN
            TEMP=(YK1(I)+2.0*YK2(I)+2.0*YK3(I)+YK4(I))/6.0
            TABLE(NTAB+1,2*I)=TABLE(NTAB,2*I)+DELT*TEMP
          end do

        end if
c
c  All join here
c
        DO I=1,NEQN
          YTEMP(I)=TABLE(NTAB+1,2*I)
          IF(ABS(YTEMP(I)).GT.ROVER)THEN
            IERROR=100
            OUTPUT='Overflow in value of solution!'
            call CHRWRT(OUTPUT,0,IOUNIT)
            RETURN
          end if
        end do
        call COMPUT(INFIX,IRPN,MAXRPN,NAMES,NEQN,TIME,YTEMP,
     &  YPTEMP,IERROR,IOUNIT,MAXVAR,OUTPUT,ROVER)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          TABLE(NTAB+1,1+2*I)=YPTEMP(I)
        end do
c
c  Evaluate exact formulas
c
        VALUE=TABLE(NTAB+1,1)
        call COMRPN('V',IERROR,IFRM,INFIX,IRPN,
     &  MAXRPN,'T',VALUE,IOUNIT)
        IF(IERROR.NE.0)RETURN
        DO I=1,NEQN
          IVAR=1+2*MAXEQN+I
          IF ( IHAVE(IVAR) .eq. 1 ) then
            IFRM=MAXEQN+I
            call COMRPN('E',IERROR,IFRM,INFIX,IRPN,
     &      MAXRPN,NAMVAR,VALUE,IOUNIT)
            IF(IERROR.NE.0)RETURN
            TABLE(NTAB+1,IVAR)=VALUE
          end if
        end do

        NTAB=NTAB+1

        IF(IPRINT.GT.0)THEN
          IF(MOD(ISTEP,IPRINT).EQ.0)THEN
            INDHI=2*NEQN
            ITEMP=NTAB-1
            IF(NEQN.EQ.1.AND.IHAVE(1+2*MAXEQN+1).EQ.1)THEN
              WRITE(OUTPUT,1280)ITEMP,TABLE(NTAB,1),TABLE(NTAB,2),
     &        TABLE(NTAB,1+2*MAXEQN+1)
            else if ( NEQN.EQ.1)THEN
              WRITE(OUTPUT,1280)ITEMP,(TABLE(NTAB,IND),IND=1,3)
            ELSE
              WRITE(OUTPUT,1280)
     &        ITEMP,TABLE(NTAB,1),(TABLE(NTAB,IND),IND=2,INDHI,2)
            end if
            call CHRWRT(OUTPUT,0,IOUNIT)
          end if
        end if

      end do

      RETURN
 1270 FORMAT('Step      T ',12X,8(A1,'Y',I1,12X))
 1280 FORMAT(I4,9G16.8)
      END
      SUBROUTINE SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)

c*********************************************************************72
c
cc SYMADD adds a symbol name to the list of symbolic names.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      character NAMVAR*10
      character OUTPUT*100
      character SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)

      IERROR=0
      call CHRLEN(NAMVAR,LENNAM)
      IF(LENNAM.LE.0)THEN
        OUTPUT='SYMADD - Null name?'
        call CHRWRT(OUTPUT,0,IOUNIT)
        IERROR=1
        RETURN
      end if
c
c  Check for name already in use
c
      DO I=1,NSYMP
        IF(NAMVAR.EQ.SYMBOL(I))THEN
          OUTPUT='SYMADD - Name '//NAMVAR//' reserved.'
          call CHRWRT(OUTPUT,1,IOUNIT)
          IERROR=1
          RETURN
        end if
      end do
c
c  Is there room for another symbol?
c
      IF(NSYM.GE.MAXSYM)THEN
        OUTPUT='SYMADD - No room to add '//NAMVAR
        call CHRWRT(OUTPUT,1,IOUNIT)
        IERROR=1
        RETURN
      end if

      IF(IFREE.GT.MAXVAL)THEN
        IERROR=1
        OUTPUT='SYMADD - No room to add '//NAMVAR
        call CHRWRT(OUTPUT,1,IOUNIT)
        RETURN
      end if
c
c  Insert symbol.
c
      NSYM=NSYM+1
      SYMBOL(NSYM)=NAMVAR
      LENSYM(NSYM)=LENNAM
      IOPSYM(NSYM)=0
      IPRSYM(NSYM)=10
      IPVAL(NSYM)=IFREE
      IFREE=IFREE+1
      INDX=IPVAL(NSYM)
      VALSYM(INDX)=0.0
      RETURN
      END
      SUBROUTINE SYMCHK(IERROR,IFINIS,IFREE,IMPLIC,INDX1,INEG,
     * INFIX,INTSYM,IOPSYM,IOUNIT,IPRSYM,IPVAL,LENFIX,LENSYM,
     * MAXSYM,MAXVAL,NINT,NSYM,NSYMP,NUVAR,
     * OUTPUT,SYMBOL,VALSYM)

c*********************************************************************72
c
cc SYMCHK ???
c
c  Input is INFIX.  Output is INTSYM, containing integers standing for
c  the recognized variable names, or constants used in the formula.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character INFIX*(*)
      DIMENSION INTSYM(MAXSYM)
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      character NAMVAR*10
      LOGICAL   NOTLET
      LOGICAL   NOTNUM
      character OUTPUT*100
      character SYM*1
      character SYM1*10
      character SYM2*10
      character SYM3*10
      character SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)
      IMPLIC=0
      IERROR=0
      NINT=0
      MATCH=0
      IPOS=1
c
c  Consider possibility of implicit declaration of new variable
c  via assignment statement.
c
      SYM='='
      LOC1=INDEX(INFIX,SYM)
      IF(LOC1.LE.1)GO TO 30
      SYM='('
      LOC2=INDEX(INFIX,SYM)
      LOC=LOC1
      IF(LOC2.GT.0)LOC=MIN(LOC1,LOC2)
      IF(LOC.LE.1.OR.LOC.GT.11)GO TO 30
      LOC=LOC-1
      NAMVAR=' '
      NAMVAR(1:LOC)=INFIX(1:LOC)
      DO I=1,NSYM
        IF(NAMVAR.EQ.SYMBOL(I))GO TO 30
      end do
      IPOS=LOC+1
      call SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      NINT=NINT+1
      INTSYM(NINT)=NSYM
      MATCH=NSYM
      IMPLIC=NSYM
      OUTPUT='SYMCHK - Implicit definition of '//NAMVAR
      call CHRWRT(OUTPUT,1,IOUNIT)
c
c  Process next chunk of formula
c
30    CONTINUE

      IF(IPOS.GT.LENFIX)THEN
        NINT=NINT+1
        INTSYM(NINT)=IFINIS
        RETURN
      end if

      MATCHO=MATCH
      MATCH=0
      LENMAT=0

      DO I=1,NSYM
        LENS=LENSYM(I)
        IF(LENS.LE.LENMAT)GO TO 50
        IF((IPOS-1+LENS).GT.LENFIX)GO TO 50
        DO J=1,LENS
          IF(INFIX(IPOS-1+J:IPOS-1+J).NE.SYMBOL(I)(J:J))GO TO 50
        end do
        LENMAT=LENS
        MATCH=I
50      CONTINUE
      end do

      IF(MATCH.EQ.0)GO TO 110
c
c  Watch out for new, implicit symbol whose initial part matches old symbol
c
      IF(IOPSYM(MATCH).NE.0)GO TO 70
      IF(IPOS+LENMAT-1.GE.LENFIX)GO TO 70
      INEXT=IPOS+LENMAT
      DO I=43,52
        IF(INFIX(INEXT:INEXT).EQ.SYMBOL(I))GO TO 70
      end do
      GO TO 140
c
c  We match an old symbol
c
70    CONTINUE

      SYM1=SYMBOL(MATCH)
      SYM2=' '
      IF(MATCHO.NE.0)SYM2=SYMBOL(MATCHO)
c
c  Check for unary minus or plus
c
      IF(SYM1.EQ.'-')THEN
        IF(SYM2.EQ.'**'.OR.SYM2.EQ.','.OR.
     *     SYM2.EQ.'('.OR.SYM2.EQ.'='.OR.IPOS.EQ.1)THEN
          SYM=INFIX(IPOS+1:IPOS+1)
          IF(LGE(SYM,'0').AND.LLE(SYM,'9'))GO TO 110
          IF(SYM.EQ.'.')GO TO 110
          end if
        end if
      IF(SYM1.EQ.'-')THEN
        IF((IPOS.EQ.1).OR.(SYM2.EQ.'(').OR.
     *     (SYM2.EQ.',').OR.
     *     (SYM2.EQ.'='))THEN
          NINT=NINT+1
          INTSYM(NINT)=INEG
          IPOS=IPOS+1
          GO TO 30
          end if
        end if
      IF(SYM1.EQ.'+')THEN
        IF((IPOS.EQ.1).OR.(SYM2.EQ.'(').OR.
     *     (SYM2.EQ.'='))THEN
          IPOS=IPOS+1
          GO TO 30
          end if
        end if
c
c  Check for matrix/vector index or two place operator
c
      IF(SYM1.EQ.')')THEN
        ICOM=0
        ILPR=0
        IRPR=1

        DO IBACK=1,NINT

          I=NINT+1-IBACK
          IF(INTSYM(I).LE.0)GO TO 90
          SYM3=SYMBOL(INTSYM(I))
          IF(SYM3.EQ.','.AND.IRPR-ILPR.EQ.1)THEN
            IF(ICOM.NE.0)THEN
              IERROR=1
              OUTPUT='SYMCHK - Too many commas.'
              call CHRWRT(OUTPUT,0,IOUNIT)
              RETURN
              end if
            ICOM=I
          else if ( SYM3.EQ.'(')THEN
            ILPR=ILPR+1
          else if ( SYM3.EQ.')')THEN
            IRPR=IRPR+1
            end if
          IF(ILPR.EQ.IRPR)THEN
            IF(I.LE.1)GO TO 100
            IF(INTSYM(I-1).LE.0)GO TO 100
            SYM3=SYMBOL(INTSYM(I-1))
            IF(IOPSYM(INTSYM(I-1)).NE.0.AND.SYM3.NE.'MAX'
     *      .AND.SYM3.NE.'MIN'.AND.SYM3.NE.'ATAN2'
     *      .AND.SYM3.NE.'POLY')GO TO 100
c
c  Copy ) into INTSYM
c
            NINT=NINT+1
            INTSYM(NINT)=MATCH
c
c  ) should be followed by INDX1 if vector/matrix reference
c
            IF(ICOM.EQ.0)THEN
              MATCH=INDX1
            ELSE
c
c  Insert )( to divide arguments
c
              INTSYM(ICOM)=MATCH
              DO JBACK=1,NINT-ICOM
                J=NINT+1-JBACK
                INTSYM(J+1)=INTSYM(J)
              end do
              NINT=NINT+1
              INTSYM(ICOM+1)=MATCH-1
              IF(IOPSYM(INTSYM(I-1)).NE.0)THEN
                IPOS=IPOS+1
                GO TO 30
                end if
              end if
            IPOS=IPOS+1
            NINT=NINT+1
            INTSYM(NINT)=MATCH
            GO TO 30
            end if
90        CONTINUE

        end do

      end if

100   CONTINUE

      LENS=LENSYM(MATCH)
      NINT=NINT+1
      INTSYM(NINT)=MATCH
      IPOS=IPOS+LENS
      GO TO 30
c
c  Check for constant
c
110   CONTINUE

      NWORDS=LENFIX+1-IPOS
      call CHRCTR(INFIX(IPOS:),NWORDS,RVAL,IERROR,LCHAR)
      IF(LCHAR.LE.0)GO TO 140

      IF(IERROR.NE.0)THEN
        OUTPUT='SYMCHK - Illegal real number.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if

      DO I=NSYMP+1,NSYM
        IF(IPRSYM(I).NE.10)GO TO 120
        IF(SYMBOL(I)(1:1).NE.'"')GO TO 120
        IPOINT=IPVAL(I)
        IF(VALSYM(IPOINT).NE.RVAL)GO TO 120
        MATCH=I
        GO TO 130
120     CONTINUE
      end do

      IF(REAL(INT(RVAL)).EQ.RVAL)THEN
        WRITE(NAMVAR,1040)INT(RVAL)
      else if ( 0.0001.LE.ABS(RVAL).AND.ABS(RVAL).LE.1)THEN
        WRITE(NAMVAR,1050)RVAL
      ELSE
        WRITE(NAMVAR,1060)RVAL
      end if

      call CHRDB1(NAMVAR)
      call SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      IF(IERROR.NE.0)RETURN
      IPOINT=IPVAL(NSYM)
      VALSYM(IPOINT)=RVAL
      MATCH=NSYM

130   CONTINUE
c
c  Advance LCHAR characters, except that CHRCTR will read in a
c  comma as the end of the number, and we have to back up in
c  such a case.
c
      IF(INFIX(IPOS+LCHAR-1:IPOS+LCHAR-1).NE.',')THEN
        IPOS=IPOS+LCHAR
      ELSE
        IPOS=IPOS+LCHAR-1
        end if
      NINT=NINT+1
      INTSYM(NINT)=MATCH
      GO TO 30
c
c  Consider implicit variable declaration on right hand side.
c
140   CONTINUE

      IF(NUVAR.EQ.0)THEN
        IERROR=1
        OUTPUT='SYMCHK - Undeclared variable.'
        call CHRWRT(OUTPUT,0,IOUNIT)
        RETURN
      end if

      NAMVAR=' '
      DO I=1,10
        SYM=INFIX(IPOS-1+I:IPOS-1+I)
        NOTLET=LLT(SYM,'A').OR.LGT(SYM,'Z')
        NOTNUM=LLT(SYM,'0').OR.LGT(SYM,'0')
        IF(NOTLET.AND.NOTNUM)GO TO 160
        NAMVAR(I:I)=SYM
      end do

160   CONTINUE

      call CHRLEN(NAMVAR,LCHAR)
      IF(LCHAR.LE.0)GO TO 170
      IPOS=IPOS+LCHAR
      call SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      NINT=NINT+1
      INTSYM(NINT)=NSYM
      MATCH=NSYM
      OUTPUT='SYMCHK - Undeclared variable '//NAMVAR//' assumed scalar.'
      call CHRWRT(OUTPUT,1,IOUNIT)
      GO TO 30

170   CONTINUE

      IERROR=1
      WRITE(OUTPUT,1010)IPOS
      call CHRWRT(OUTPUT,1,IOUNIT)
      WRITE(OUTPUT,'(1X,A)')INFIX(1:LENFIX)
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
1010  FORMAT('SYMCHK - The string at location',I6,' is unreadable')
1040  FORMAT('"',I9)
1050  FORMAT('"',F9.6)
1060  FORMAT('"',G9.2)
      END
      SUBROUTINE SYMVAL(COM,IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,VALSYM,
     *VALUE,IOUNIT,OUTPUT,IFREE)

c*********************************************************************72
c
cc SYMVAL sets, evaluates, or deletes a variable.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character COM*1
      DIMENSION IOPSYM(MAXSYM)
      DIMENSION IOUNIT(*)
      DIMENSION IPRSYM(MAXSYM)
      DIMENSION IPVAL(MAXSYM)
      DIMENSION LENSYM(MAXSYM)
      character NAMVAR*10
      character OUTPUT*100
      character SYMBOL(MAXSYM)*10
      DIMENSION VALSYM(MAXVAL)

      IERROR=0
      call CHRLEN(NAMVAR,LENNAM)
      IF(LENNAM.LE.0)THEN
        IERROR=1
        OUTPUT='SYMVAL - Cannot understand variable '//NAMVAR
        call CHRWRT(OUTPUT,1,IOUNIT)
        RETURN
      end if
c
c  Search for match
c
      DO I=1,NSYM
        MATCH=I
        IF(NAMVAR.EQ.SYMBOL(I))GO TO 20
      end do

      IF(COM.EQ.'R')THEN
        OUTPUT='SYMVAL - Could not find '//NAMVAR
        call CHRWRT(OUTPUT,0,IOUNIT)
        IF(COM.EQ.'R')IERROR=1
        end if
c
c  Add name to list
c
      call SYMADD(IERROR,IOPSYM,IPRSYM,IPVAL,LENSYM,
     *MAXSYM,MAXVAL,NAMVAR,NSYM,NSYMP,SYMBOL,
     *VALSYM,IOUNIT,OUTPUT,IFREE)
      IF(IERROR.NE.0)RETURN
      MATCH=NSYM
      GO TO 30

20    CONTINUE

      IF(IOPSYM(MATCH).NE.0)IERROR=1
      IF(IERROR.NE.0)THEN
        OUTPUT='SYMVAL - Variable may not be altered.'
        call CHRWRT(OUTPUT,1,IOUNIT)
        RETURN
        end if

30    CONTINUE

      INDX=IPVAL(MATCH)
      IF(COM.EQ.'V')THEN
        VALSYM(INDX)=VALUE
      ELSE
        VALUE=VALSYM(INDX)
        end if
      INDX=INDX+1
      RETURN
      END
      SUBROUTINE TYPE(IOUNIT,IHAVE,IPRINT,IRHS,MAXEQN,
     *MAXTAB,MAXVAR,METHOD,METNAM,NEQN,NSTEPS,NTAB,OUTPUT,
     *ROVER,TABLE,TSTOP,VSCALE)

c*********************************************************************72
c
cc TYPE ???
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRINT*6
      character CHRREL*14
      DIMENSION IHAVE(MAXVAR)
      DIMENSION IOUNIT(*)
      character IRHS(*)*(*)
      character METNAM(*)*60
      character OUTPUT*(*)
      DIMENSION TABLE(MAXTAB,MAXVAR)

      IF(NEQN.EQ.1)THEN
        OUTPUT='There is one equation:'
      else if ( NEQN.GT.1)THEN
        OUTPUT='There are NEQN='//CHRINT(NEQN)//' equations:'
      end if

      call CHRWRT(OUTPUT,1,IOUNIT)
      DO I=1,NEQN
        WRITE(OUTPUT,1090)I,IRHS(I)
        call CHRWRT(OUTPUT,1,IOUNIT)
      end do

      DO I=1,NEQN
        IVAR=1+2*MAXEQN+I
        IF(IHAVE(IVAR).NE.0)THEN
          WRITE(OUTPUT,1091)I,IRHS(I+MAXEQN)
          call CHRWRT(OUTPUT,1,IOUNIT)
        end if
      end do

      IF(IHAVE(1).NE.0)THEN
        OUTPUT=' '
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Initial condition defined at TINIT='//CHRREL(TABLE(1,1))
        call CHRWRT(OUTPUT,1,IOUNIT)
        DO I=1,NEQN
          WRITE(OUTPUT,1000)I,TABLE(1,2*I)
          call CHRWRT(OUTPUT,1,IOUNIT)
        end do
      ELSE
        OUTPUT='Initial conditions have not been specified.'
        call CHRWRT(OUTPUT,0,IOUNIT)
      end if

      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='The integration method is METHOD='//CHRINT(METHOD)
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT=METNAM(METHOD)
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='taking NSTEPS='//CHRINT(NSTEPS)//' steps'
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='increasing T to TSTOP='//CHRREL(TSTOP)
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      IF(IHAVE(1).NE.0.AND.NTAB.GT.1)THEN
        OUTPUT='Solution has been computed from'
        call CHRWRT(OUTPUT,1,IOUNIT)
        OUTPUT='T='//CHRREL(TABLE(1,1))//' to '//CHRREL(TABLE(NTAB,1))
        call CHRWRT(OUTPUT,1,IOUNIT)
      ELSE
        OUTPUT='Solution has not been computed.'
        call CHRWRT(OUTPUT,0,IOUNIT)
      end if
      OUTPUT=' '
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='Writing output every IPRINT='//CHRINT(IPRINT)//' steps.'
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='There are '//CHRINT(NTAB)//' plot points saved.'
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='At most '//CHRINT(MAXTAB)//' plot points are allowed.'
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='Overflow threshhold is ROVER='//CHRREL(ROVER)
      call CHRWRT(OUTPUT,1,IOUNIT)
      OUTPUT='Flow field vector scale factor VSCALE='//CHRREL(VSCALE)
      call CHRWRT(OUTPUT,1,IOUNIT)
      RETURN
1000  FORMAT('Initial Y',I1,'=',G14.6)
1090  FORMAT('Y',I1,'''=',A80)
1091  FORMAT('Exact value X',I1,'=',A80)
      END
      SUBROUTINE WRITER(FILNAM,IERROR,IOUNIT,IPRINT,IRHS,LINE,
     *MAXEQN,MAXTAB,MAXVAR,METHOD,NEQN,NLINE,
     *NSTEPS,OUTPUT,PROMPT,TABLE,TSTOP)

c*********************************************************************72
c
cc WRITER writes the ODE problem definition to a file.
c
c  Modified:
c
c    01 November 2008
c
c  Author:
c
c    John Burkardt
c
      character CHRINT*6
      character CHRREL*14
      character FILNAM*(*)
      DIMENSION IOUNIT(*)
      character IRHS(*)*(*)
      character LINE*80
      character OUTPUT*120
      character PROMPT*80
      DIMENSION TABLE(MAXTAB,MAXVAR)

      PROMPT='output filename.'
      call CHRREA(FILNAM,LINE,NLINE,PROMPT,IOUNIT,IERROR,0,0)
      IOSAVE=IOUNIT(2)
      IOUNIT(2)=41
      OPEN(UNIT=41,FILE=FILNAM,STATUS='NEW',ERR=20)
      OUTPUT='NOTE'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='This file was created by RK124, and defines an ODE.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='.'
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NEQN='//CHRINT(NEQN)
      call CHRDB1(OUTPUT)
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TINIT='//CHRREL(TABLE(1,1))
      call CHRDB1(OUTPUT)
      call CHRWRT(OUTPUT,0,IOUNIT)
      DO I=1,NEQN
        OUTPUT='Y'//CHRINT(I)//'='//CHRREL(TABLE(1,2*I))
        call CHRDB1(OUTPUT)
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='Y1''='//IRHS(I)
        call CHRWRT(OUTPUT,0,IOUNIT)
        OUTPUT='X1='//IRHS(MAXEQN+I)
        call CHRWRT(OUTPUT,0,IOUNIT)
      end do
      OUTPUT='METHOD='//CHRINT(METHOD)
      call CHRDB2(OUTPUT)
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='IPRINT='//CHRINT(IPRINT)
      call CHRDB1(OUTPUT)
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='NSTEPS='//CHRINT(NSTEPS)
      call CHRDB1(OUTPUT)
      call CHRWRT(OUTPUT,0,IOUNIT)
      OUTPUT='TSTOP='//CHRREL(TSTOP)
      call CHRDB1(OUTPUT)
      call CHRWRT(OUTPUT,0,IOUNIT)
      CLOSE(UNIT=41)
      IOUNIT(2)=IOSAVE
      OUTPUT='RK124 has written the file '//FILNAM
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN

20    CONTINUE

      IERROR=1
      OUTPUT='WRITER - Error trying to open file!'
      call CHRWRT(OUTPUT,0,IOUNIT)
      RETURN
      END
